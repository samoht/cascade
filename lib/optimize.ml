(** CSS optimization implementation *)

open Declaration
open Stylesheet
module String_set = Set.Make (String)

(** {1 Edge Model} *)

type edge = {
  summary : Selector_summary.t;
  property : string;
  important : bool;
}

let selectors_of_rule_selector (sel : Selector.t) =
  match Selector.as_list sel with Some xs -> xs | None -> [ sel ]

let edges_of_decl summary = function
  | Declaration _ as d ->
      Some
        {
          summary;
          property = Declaration.property_name d;
          important = Declaration.is_important d;
        }
  | _ -> None

let edges_of_rule (rule : Stylesheet.rule) : edge list =
  let summaries =
    selectors_of_rule_selector rule.selector
    |> List.map Selector_summary.of_selector
  in
  List.concat_map
    (fun summary -> List.filter_map (edges_of_decl summary) rule.declarations)
    summaries

(** {1 Declaration Optimization} *)

let duplicate_buggy_properties decls =
  (* Check if webkit-text-decoration:inherit is already duplicated *)
  let webkit_text_decoration_inherit_count =
    List.fold_left
      (fun count decl ->
        match decl with
        | Declaration { property = Webkit_text_decoration; value = Inherit; _ }
          ->
            count + 1
        | _ -> count)
      0 decls
  in

  List.concat_map
    (fun decl ->
      match decl with
      | Declaration { property = Webkit_text_decoration; value = Inherit; _ } ->
          if webkit_text_decoration_inherit_count >= 3 then [ decl ]
            (* Already tripled *)
          else [ decl; decl; decl ] (* Triplicate only when inherit *)
      | Declaration { property = Transform; _ } ->
          (* Do not duplicate transform to -webkit-transform. Tailwind v4 does
             not emit vendor-prefixed transform here, and tests expect a single
             canonical property. *)
          [ decl ]
      | _ -> [ decl ])
    decls

let is_intentionally_duplicated prop_name =
  prop_name = "content" || prop_name = "outline"
  || (String.length prop_name > 12 && String.sub prop_name 0 12 = "-webkit-mask")

let shorthand_longhands = function
  | "margin" -> [ "margin-top"; "margin-right"; "margin-bottom"; "margin-left" ]
  | "padding" ->
      [ "padding-top"; "padding-right"; "padding-bottom"; "padding-left" ]
  | "background" ->
      [
        "background-attachment";
        "background-blend-mode";
        "background-clip";
        "background-color";
        "background-image";
        "background-origin";
        "background-position";
        "background-repeat";
        "background-size";
      ]
  | _ -> []

let all_resets_property name =
  not
    (name = "direction" || name = "unicode-bidi"
    || (String.length name >= 2 && String.sub name 0 2 = "--"))

let all_preserved_reorder_property name =
  name = "direction" || name = "unicode-bidi"

let declaration_covers covering covered =
  covering = covered
  || (covering = "all" && all_resets_property covered)
  || List.mem covered (shorthand_longhands covering)

(* Detect a value that begins with a CSS vendor-prefix (-webkit-, -moz-, -ms-,
   -o-). Used to preserve legacy fallback patterns like
   [display:-webkit-box;display:flex]: the spec value cascades over the prefixed
   value in modern browsers, but old browsers only understand the prefixed
   spelling, so dropping the earlier declaration removes a real browser-compat
   fallback. *)
let value_is_vendor_prefixed decl =
  let s = string_of_value ~minify:true decl in
  let len = String.length s in
  let starts_with prefix =
    String.length prefix <= len
    && String.sub s 0 (String.length prefix) = prefix
  in
  starts_with "-webkit-" || starts_with "-moz-" || starts_with "-ms-"
  || starts_with "-o-"

(* CSS Box 4 7.1: a 1/2/3/4-value box shorthand expands to four explicit sides;
   recompose by emitting all four and letting the printer's
   [collapse_box_shorthand] pick the shortest spelling. *)
let expand_box vs =
  match vs with
  | [ a ] -> Some (a, a, a, a)
  | [ a; b ] -> Some (a, b, a, b)
  | [ a; b; c ] -> Some (a, b, c, b)
  | [ a; b; c; d ] -> Some (a, b, c, d)
  | _ -> None

type sides = Values.length * Values.length * Values.length * Values.length

(* A runtime-subst leaf may resolve to a 1-to-4-value sequence, so a corner
   longhand can't be guaranteed to shadow it - bail out of the merge then. *)
let sides_have_runtime_subst ((top, right, bottom, left) : sides) =
  Values.length_has_runtime_subst top
  || Values.length_has_runtime_subst right
  || Values.length_has_runtime_subst bottom
  || Values.length_has_runtime_subst left

(* Try to absorb a corner-longhand declaration into a margin 4-tuple. Pattern
   matching is inlined here so the GADT existential value type ([length]) stays
   inside the typed branch. Returns the updated tuple, or [None] if [d] is not
   an absorbable margin longhand at the matching importance. *)
let absorb_margin_corner ~important ((top, right, bottom, left) : sides) d :
    sides option =
  match d with
  | Declaration { property = Margin_top; value = v; important = i }
    when i = important ->
      Some (v, right, bottom, left)
  | Declaration { property = Margin_right; value = v; important = i }
    when i = important ->
      Some (top, v, bottom, left)
  | Declaration { property = Margin_bottom; value = v; important = i }
    when i = important ->
      Some (top, right, v, left)
  | Declaration { property = Margin_left; value = v; important = i }
    when i = important ->
      Some (top, right, bottom, v)
  | _ -> None

let absorb_padding_corner ~important ((top, right, bottom, left) : sides) d :
    sides option =
  match d with
  | Declaration { property = Padding_top; value = v; important = i }
    when i = important ->
      Some (v, right, bottom, left)
  | Declaration { property = Padding_right; value = v; important = i }
    when i = important ->
      Some (top, v, bottom, left)
  | Declaration { property = Padding_bottom; value = v; important = i }
    when i = important ->
      Some (top, right, v, left)
  | Declaration { property = Padding_left; value = v; important = i }
    when i = important ->
      Some (top, right, bottom, v)
  | _ -> None

let is_margin_shorthand = function
  | Declaration { property = Margin; _ } -> true
  | _ -> false

let is_padding_shorthand = function
  | Declaration { property = Padding; _ } -> true
  | _ -> false

(* Walk forward through [rest], absorbing every matching corner longhand until
   we hit another instance of the same shorthand (which would override the
   merged result anyway). Returns the updated 4-tuple and [rest] with absorbed
   declarations removed. *)
let absorb_box_longhands ~absorb ~is_same_shorthand sides rest =
  let rec loop sides acc = function
    | [] -> (sides, List.rev acc)
    | (i, d) :: rest when is_same_shorthand d ->
        (sides, List.rev_append acc ((i, d) :: rest))
    | (i, d) :: rest -> (
        match absorb sides d with
        | Some sides' -> loop sides' acc rest
        | None -> loop sides ((i, d) :: acc) rest)
  in
  loop sides [] rest

let box_shorthand_had_prior_longhand source idx shorthand =
  let shorthand_prop = property_name shorthand in
  let shorthand_important = is_important shorthand in
  let rec loop i = function
    | [] -> false
    | d :: rest ->
        i < idx
        &&
        let prop = property_name d in
        List.mem prop (shorthand_longhands shorthand_prop)
        && (shorthand_important || not (is_important d))
        || loop (i + 1) rest
  in
  loop 0 source

(* Fold subsequent margin/padding corner longhands into the preceding box
   shorthand. Tailwind / Lightning-CSS / cssnano all do this; the dead-code
   suite asserts it for [margin: 10px; margin-top: 20px] -> [margin: 20px 10px
   10px]. *)
(* Commit the merge only when every side ends up concrete; otherwise restore
   the original shorthand and leave its longhand tail in place. *)
let try_merge_box_shorthand ~property ~vs ~important ~absorb ~is_same_shorthand
    rest =
  match expand_box vs with
  | None -> (Declaration { property; value = vs; important }, rest)
  | Some sides -> (
      let ((top, right, bottom, left) as absorbed), rest' =
        absorb_box_longhands ~absorb ~is_same_shorthand sides rest
      in
      match sides_have_runtime_subst absorbed with
      | true -> (Declaration { property; value = vs; important }, rest)
      | false ->
          ( Declaration
              { property; value = [ top; right; bottom; left ]; important },
            rest' ))

(* CSS Overflow 3 §3.1: [overflow] is the [overflow-x overflow-y] shorthand.
   When the two longhands appear together with matching importance and neither
   side is later shadowed within the same block, fold them into [overflow] -
   single value when the two axes match, two values otherwise. *)
let combined_overflow v_x v_y : Properties.overflow =
  if v_x = v_y then v_x else Overflow_pair (v_x, v_y)

let try_take_overflow_y ~important rest =
  let rec loop acc :
      (int * declaration) list ->
      (Properties.overflow * (int * declaration) list) option = function
    | [] -> None
    | (_, Declaration { property = Overflow_y; value = v_y; important = i' })
      :: rest
      when i' = important ->
        Some (v_y, List.rev_append acc rest)
    | (_, Declaration { property = Overflow | Overflow_x | Overflow_y; _ }) :: _
      ->
        None
    | other :: rest -> loop (other :: acc) rest
  in
  loop [] rest

let try_take_overflow_x ~important rest =
  let rec loop acc :
      (int * declaration) list ->
      (Properties.overflow * (int * declaration) list) option = function
    | [] -> None
    | (_, Declaration { property = Overflow_x; value = v_x; important = i' })
      :: rest
      when i' = important ->
        Some (v_x, List.rev_append acc rest)
    | (_, Declaration { property = Overflow | Overflow_x | Overflow_y; _ }) :: _
      ->
        None
    | other :: rest -> loop (other :: acc) rest
  in
  loop [] rest

let merge_overflow_longhands decls =
  let rec go acc = function
    | [] -> List.rev acc
    | (idx, Declaration { property = Overflow_x; value = v_x; important })
      :: rest -> (
        match try_take_overflow_y ~important rest with
        | None ->
            go
              (( idx,
                 Declaration { property = Overflow_x; value = v_x; important }
               )
              :: acc)
              rest
        | Some (v_y, rest') ->
            let merged =
              Declaration
                {
                  property = Overflow;
                  value = combined_overflow v_x v_y;
                  important;
                }
            in
            go ((idx, merged) :: acc) rest')
    | (idx, Declaration { property = Overflow_y; value = v_y; important })
      :: rest -> (
        match try_take_overflow_x ~important rest with
        | None ->
            go
              (( idx,
                 Declaration { property = Overflow_y; value = v_y; important }
               )
              :: acc)
              rest
        | Some (v_x, rest') ->
            let merged =
              Declaration
                {
                  property = Overflow;
                  value = combined_overflow v_x v_y;
                  important;
                }
            in
            go ((idx, merged) :: acc) rest')
    | d :: rest -> go (d :: acc) rest
  in
  go [] decls

(* Compose 4 contiguous box-side longhands ([margin-top / -right / -bottom /
   -left], or the [padding-] equivalents) into a single shorthand. Runs before
   [merge_box_shorthand_longhands] so the absorption pass can pick up any
   remaining stragglers. Conservative: requires all four sides present in the
   next four positions (any order), matching importance, and no
   runtime-substitution leaves on any side. *)
type box_side = Top | Right | Bottom | Left

let extract_margin_side :
    declaration -> (box_side * Values.length * bool) option = function
  | Declaration { property = Margin_top; value; important } ->
      Some (Top, value, important)
  | Declaration { property = Margin_right; value; important } ->
      Some (Right, value, important)
  | Declaration { property = Margin_bottom; value; important } ->
      Some (Bottom, value, important)
  | Declaration { property = Margin_left; value; important } ->
      Some (Left, value, important)
  | _ -> None

let extract_padding_side :
    declaration -> (box_side * Values.length * bool) option = function
  | Declaration { property = Padding_top; value; important } ->
      Some (Top, value, important)
  | Declaration { property = Padding_right; value; important } ->
      Some (Right, value, important)
  | Declaration { property = Padding_bottom; value; important } ->
      Some (Bottom, value, important)
  | Declaration { property = Padding_left; value; important } ->
      Some (Left, value, important)
  | _ -> None

(* CSS Position 3 §3.1: [inset] is the [top right bottom left] shorthand. The
   longhand values are wrapped in a [length list] for grammar reasons but carry
   exactly one length per side. *)
let extract_inset_side : declaration -> (box_side * Values.length * bool) option
    = function
  | Declaration { property = Top; value = [ v ]; important } ->
      Some (Top, v, important)
  | Declaration { property = Right; value = [ v ]; important } ->
      Some (Right, v, important)
  | Declaration { property = Bottom; value = [ v ]; important } ->
      Some (Bottom, v, important)
  | Declaration { property = Left; value = [ v ]; important } ->
      Some (Left, v, important)
  | _ -> None

let extract_border_radius_corner :
    declaration -> (box_side * Values.length * bool) option = function
  | Declaration { property = Border_top_left_radius; value; important } ->
      Some (Top, value, important)
  | Declaration { property = Border_top_right_radius; value; important } ->
      Some (Right, value, important)
  | Declaration { property = Border_bottom_right_radius; value; important } ->
      Some (Bottom, value, important)
  | Declaration { property = Border_bottom_left_radius; value; important } ->
      Some (Left, value, important)
  | _ -> None

let try_compose_box ~extract ~build = function
  | (idx, d1) :: (_, d2) :: (_, d3) :: (_, d4) :: rest -> (
      match (extract d1, extract d2, extract d3, extract d4) with
      | ( Some (s1, v1, imp1),
          Some (s2, v2, imp2),
          Some (s3, v3, imp3),
          Some (s4, v4, imp4) )
        when imp1 = imp2 && imp2 = imp3 && imp3 = imp4 ->
          let sides = [ (s1, v1); (s2, v2); (s3, v3); (s4, v4) ] in
          let distinct =
            List.length (List.sort_uniq compare (List.map fst sides)) = 4
          in
          let no_runtime =
            List.for_all
              (fun (_, v) -> not (Values.length_has_runtime_subst v))
              sides
          in
          if distinct && no_runtime then
            let find s = List.assoc s sides in
            let merged =
              build ~important:imp1 ~top:(find Top) ~right:(find Right)
                ~bottom:(find Bottom) ~left:(find Left)
            in
            Some ((idx, merged), rest)
          else None
      | _ -> None)
  | _ -> None

let compose_box_shorthands decls =
  let build_margin ~important ~top ~right ~bottom ~left =
    Declaration
      { property = Margin; value = [ top; right; bottom; left ]; important }
  in
  let build_padding ~important ~top ~right ~bottom ~left =
    Declaration
      { property = Padding; value = [ top; right; bottom; left ]; important }
  in
  let build_border_radius ~important ~top ~right ~bottom ~left =
    let lp v : Values.length_percentage = Length v in
    Declaration
      {
        property = Border_radius;
        value =
          Radius
            {
              horizontal = [ lp top; lp right; lp bottom; lp left ];
              vertical = None;
            };
        important;
      }
  in
  let build_inset ~important ~top ~right ~bottom ~left =
    Declaration
      { property = Inset; value = [ top; right; bottom; left ]; important }
  in
  let composers =
    [
      try_compose_box ~extract:extract_margin_side ~build:build_margin;
      try_compose_box ~extract:extract_padding_side ~build:build_padding;
      try_compose_box ~extract:extract_inset_side ~build:build_inset;
      try_compose_box ~extract:extract_border_radius_corner
        ~build:build_border_radius;
    ]
  in
  let try_any decls = List.find_map (fun f -> f decls) composers in
  let rec go acc decls =
    match (decls, try_any decls) with
    | [], _ -> List.rev acc
    | _, Some (merged, rest) -> go (merged :: acc) rest
    | hd :: rest, None -> go (hd :: acc) rest
  in
  go [] decls

(* Compose 2-longhand shorthands ([gap] from [row-gap] / [column-gap],
   [place-items] from [align-items] / [justify-items], etc) when both longhands
   appear contiguously with matching importance. *)
type pair_side = Row | Column

let extract_gap_side : declaration -> (pair_side * Values.length * bool) option
    = function
  | Declaration { property = Row_gap; value; important } ->
      Some (Row, value, important)
  | Declaration { property = Column_gap; value; important } ->
      Some (Column, value, important)
  | _ -> None

let try_compose_gap = function
  | (idx, d1) :: (_, d2) :: rest -> (
      match (extract_gap_side d1, extract_gap_side d2) with
      | Some (s1, v1, imp1), Some (s2, v2, imp2)
        when imp1 = imp2 && s1 <> s2
             && (not (Values.length_has_runtime_subst v1))
             && not (Values.length_has_runtime_subst v2) ->
          let pair = [ (s1, v1); (s2, v2) ] in
          let find s = List.assoc s pair in
          let merged =
            Declaration
              {
                property = Gap;
                value =
                  Lengths
                    {
                      row_gap = Some (find Row);
                      column_gap = Some (find Column);
                    };
                important = imp1;
              }
          in
          Some ((idx, merged), rest)
      | _ -> None)
  | _ -> None

(* Compose [<base>-inline] / [<base>-block] from the matching [-start] / [-end]
   longhands. Both longhands carry exactly one length value (wrapped in a
   1-element list for [inset-*] grammar reasons). The result is a [length list]
   payload: [v] when both sides match, [v_start; v_end] otherwise. *)
type axis_side = Side_start | Side_end

let try_compose_axis_pair ~extract ~build = function
  | (idx, d1) :: (_, d2) :: rest -> (
      match (extract d1, extract d2) with
      | Some (s1, v1, imp1), Some (s2, v2, imp2)
        when imp1 = imp2 && s1 <> s2
             && (not (Values.length_has_runtime_subst v1))
             && not (Values.length_has_runtime_subst v2) ->
          let pair = [ (s1, v1); (s2, v2) ] in
          let v_start = List.assoc Side_start pair in
          let v_end = List.assoc Side_end pair in
          let value =
            if v_start = v_end then [ v_start ] else [ v_start; v_end ]
          in
          Some ((idx, build ~important:imp1 ~value), rest)
      | _ -> None)
  | _ -> None

let extract_margin_inline_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Margin_inline_start; value; important } ->
      Some (Side_start, value, important)
  | Declaration { property = Margin_inline_end; value; important } ->
      Some (Side_end, value, important)
  | _ -> None

let extract_margin_block_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Margin_block_start; value; important } ->
      Some (Side_start, value, important)
  | Declaration { property = Margin_block_end; value; important } ->
      Some (Side_end, value, important)
  | _ -> None

let extract_padding_inline_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Padding_inline_start; value; important } ->
      Some (Side_start, value, important)
  | Declaration { property = Padding_inline_end; value; important } ->
      Some (Side_end, value, important)
  | _ -> None

let extract_padding_block_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Padding_block_start; value; important } ->
      Some (Side_start, value, important)
  | Declaration { property = Padding_block_end; value; important } ->
      Some (Side_end, value, important)
  | _ -> None

let extract_inset_inline_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Inset_inline_start; value = [ v ]; important } ->
      Some (Side_start, v, important)
  | Declaration { property = Inset_inline_end; value = [ v ]; important } ->
      Some (Side_end, v, important)
  | _ -> None

let extract_inset_block_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Inset_block_start; value = [ v ]; important } ->
      Some (Side_start, v, important)
  | Declaration { property = Inset_block_end; value = [ v ]; important } ->
      Some (Side_end, v, important)
  | _ -> None

(* CSS Align 3 §6.1: [place-items] / [place-content] / [place-self] are the
   [<align> <justify>] shorthands. When the two longhands appear contiguously
   with matching importance, fold them; the per-property printer then collapses
   matching pairs to a single value. *)
let try_compose_place_items = function
  | (idx, Declaration { property = Align_items; value = a; important = i1 })
    :: (_, Declaration { property = Justify_items; value = j; important = i2 })
    :: rest
    when i1 = i2 ->
      let merged =
        Declaration
          {
            property = Place_items;
            value = (Align_justify (a, j) : Properties.place_items);
            important = i1;
          }
      in
      Some ((idx, merged), rest)
  | (idx, Declaration { property = Justify_items; value = j; important = i1 })
    :: (_, Declaration { property = Align_items; value = a; important = i2 })
    :: rest
    when i1 = i2 ->
      let merged =
        Declaration
          {
            property = Place_items;
            value = (Align_justify (a, j) : Properties.place_items);
            important = i1;
          }
      in
      Some ((idx, merged), rest)
  | _ -> None

let try_compose_place_content = function
  | (idx, Declaration { property = Align_content; value = a; important = i1 })
    :: (_, Declaration { property = Justify_content; value = j; important = i2 })
    :: rest
    when i1 = i2 ->
      let merged =
        Declaration
          {
            property = Place_content;
            value = (Align_justify (a, j) : Properties.place_content);
            important = i1;
          }
      in
      Some ((idx, merged), rest)
  | (idx, Declaration { property = Justify_content; value = j; important = i1 })
    :: (_, Declaration { property = Align_content; value = a; important = i2 })
    :: rest
    when i1 = i2 ->
      let merged =
        Declaration
          {
            property = Place_content;
            value = (Align_justify (a, j) : Properties.place_content);
            important = i1;
          }
      in
      Some ((idx, merged), rest)
  | _ -> None

let try_compose_place_self = function
  | (idx, Declaration { property = Align_self; value = a; important = i1 })
    :: (_, Declaration { property = Justify_self; value = j; important = i2 })
    :: rest
    when i1 = i2 ->
      let merged =
        Declaration { property = Place_self; value = (a, j); important = i1 }
      in
      Some ((idx, merged), rest)
  | (idx, Declaration { property = Justify_self; value = j; important = i1 })
    :: (_, Declaration { property = Align_self; value = a; important = i2 })
    :: rest
    when i1 = i2 ->
      let merged =
        Declaration { property = Place_self; value = (a, j); important = i1 }
      in
      Some ((idx, merged), rest)
  | _ -> None

let compose_pair_shorthands decls =
  let axis property extract decls =
    let build ~important ~value = Declaration { property; value; important } in
    try_compose_axis_pair ~extract ~build decls
  in
  let composers =
    [
      try_compose_gap;
      axis Margin_inline extract_margin_inline_side;
      axis Margin_block extract_margin_block_side;
      axis Padding_inline extract_padding_inline_side;
      axis Padding_block extract_padding_block_side;
      axis Inset_inline extract_inset_inline_side;
      axis Inset_block extract_inset_block_side;
      try_compose_place_items;
      try_compose_place_content;
      try_compose_place_self;
    ]
  in
  let try_any decls = List.find_map (fun f -> f decls) composers in
  let rec go acc decls =
    match (decls, try_any decls) with
    | [], _ -> List.rev acc
    | _, Some (merged, rest) -> go (merged :: acc) rest
    | hd :: rest, None -> go (hd :: acc) rest
  in
  go [] decls

(* Compose [outline-width / -style / -color] into the [outline] shorthand when
   all three longhands appear contiguously with matching importance. *)
type outline_part = Width | Style | Color

let outline_part_of : declaration -> outline_part option = function
  | Declaration { property = Outline_width; _ } -> Some Width
  | Declaration { property = Outline_style; _ } -> Some Style
  | Declaration { property = Outline_color; _ } -> Some Color
  | _ -> None

let outline_width_value : declaration -> Values.length option = function
  | Declaration { property = Outline_width; value; _ } -> Some value
  | _ -> None

let outline_style_value : declaration -> Properties.outline_style option =
  function
  | Declaration { property = Outline_style; value; _ } -> Some value
  | _ -> None

let outline_color_value : declaration -> Values.color option = function
  | Declaration { property = Outline_color; value; _ } -> Some value
  | _ -> None

let try_compose_outline = function
  | (idx, d1) :: (_, d2) :: (_, d3) :: rest -> (
      match (outline_part_of d1, outline_part_of d2, outline_part_of d3) with
      | Some p1, Some p2, Some p3
        when is_important d1 = is_important d2
             && is_important d2 = is_important d3
             && List.length (List.sort_uniq compare [ p1; p2; p3 ]) = 3 ->
          let triple = [ d1; d2; d3 ] in
          let width = List.find_map outline_width_value triple in
          let style = List.find_map outline_style_value triple in
          let color = List.find_map outline_color_value triple in
          let no_runtime =
            match width with
            | Some w -> not (Values.length_has_runtime_subst w)
            | None -> true
          in
          if no_runtime then
            let merged =
              Declaration
                {
                  property = Outline;
                  value = Shorthand { width; style; color };
                  important = is_important d1;
                }
            in
            Some ((idx, merged), rest)
          else None
      | _ -> None)
  | _ -> None

let compose_outline_shorthand decls =
  let rec go acc decls =
    match (decls, try_compose_outline decls) with
    | [], _ -> List.rev acc
    | _, Some (merged, rest) -> go (merged :: acc) rest
    | hd :: rest, None -> go (hd :: acc) rest
  in
  go [] decls

(* CSS Fonts 4 sec. 2.7: [font] shorthand reads [<style>? <weight>?
   <size>[/<line-height>]? <family>+] Cascade stores [font] as a string, so
   composition renders each longhand via its pretty-printer and stitches the
   shorthand together. Default-valued components ([normal] style, [400] weight,
   [normal] line-height) drop. Requires both font-size and font-family. *)
type font_kind = FStyle | FWeight | FSize | FLine_height | FFamily

let font_kind_of : declaration -> font_kind option = function
  | Declaration { property = Font_style; _ } -> Some FStyle
  | Declaration { property = Font_weight; _ } -> Some FWeight
  | Declaration { property = Font_size; _ } -> Some FSize
  | Declaration { property = Line_height; _ } -> Some FLine_height
  | Declaration { property = Font_family; _ } -> Some FFamily
  | _ -> None

let minified_value d = Declaration.string_of_value ~minify:true d
let drop_if_default ~default v = if v = default then None else Some v

let render_font_shorthand parts =
  let find k =
    List.find_map
      (fun (kind, decl) -> if kind = k then Some decl else None)
      parts
  in
  let style =
    Option.bind (find FStyle) (fun d ->
        drop_if_default ~default:"normal" (minified_value d))
  in
  let weight =
    Option.bind (find FWeight) (fun d ->
        drop_if_default ~default:"400" (minified_value d))
  in
  let line_height =
    Option.bind (find FLine_height) (fun d ->
        drop_if_default ~default:"normal" (minified_value d))
  in
  match (find FSize, find FFamily) with
  | Some size_d, Some family_d ->
      let size = minified_value size_d in
      let family = minified_value family_d in
      let size_lh =
        match line_height with
        | Some lh -> String.concat "" [ size; "/"; lh ]
        | None -> size
      in
      let leading =
        [ style; weight ] |> List.filter_map (fun x -> x) |> String.concat " "
      in
      let body =
        if leading = "" then String.concat " " [ size_lh; family ]
        else String.concat " " [ leading; size_lh; family ]
      in
      Some body
  | _ -> None

let try_compose_font = function
  | (idx, d1) :: (_, d2) :: (_, d3) :: (_, d4) :: (_, d5) :: rest -> (
      match
        ( font_kind_of d1,
          font_kind_of d2,
          font_kind_of d3,
          font_kind_of d4,
          font_kind_of d5 )
      with
      | Some k1, Some k2, Some k3, Some k4, Some k5
        when is_important d1 = is_important d2
             && is_important d2 = is_important d3
             && is_important d3 = is_important d4
             && is_important d4 = is_important d5
             && List.length (List.sort_uniq compare [ k1; k2; k3; k4; k5 ]) = 5
        -> (
          let parts = [ (k1, d1); (k2, d2); (k3, d3); (k4, d4); (k5, d5) ] in
          match render_font_shorthand parts with
          | Some s ->
              let merged =
                Declaration
                  { property = Font; value = s; important = is_important d1 }
              in
              Some ((idx, merged), rest)
          | None -> None)
      | _ -> None)
  | _ -> None

let compose_font_shorthand decls =
  let rec go acc decls =
    match (decls, try_compose_font decls) with
    | [], _ -> List.rev acc
    | _, Some (merged, rest) -> go (merged :: acc) rest
    | hd :: rest, None -> go (hd :: acc) rest
  in
  go [] decls

let merge_box_shorthand_longhands source decls =
  let rec go acc = function
    | [] -> List.rev acc
    | (idx, (Declaration { property = Margin; value = vs; important } as d))
      :: rest
      when not (box_shorthand_had_prior_longhand source idx d) ->
        let merged, rest =
          try_merge_box_shorthand ~property:Margin ~vs ~important
            ~absorb:(absorb_margin_corner ~important)
            ~is_same_shorthand:is_margin_shorthand rest
        in
        go ((idx, merged) :: acc) rest
    | (idx, (Declaration { property = Padding; value = vs; important } as d))
      :: rest
      when not (box_shorthand_had_prior_longhand source idx d) ->
        let merged, rest =
          try_merge_box_shorthand ~property:Padding ~vs ~important
            ~absorb:(absorb_padding_corner ~important)
            ~is_same_shorthand:is_padding_shorthand rest
        in
        go ((idx, merged) :: acc) rest
    | d :: rest -> go (d :: acc) rest
  in
  go [] decls

let property_covered_by_important kept prop_name =
  List.exists
    (fun (_, decl) ->
      (not (is_intentionally_duplicated (property_name decl)))
      && is_important decl
      && declaration_covers (property_name decl) prop_name)
    kept

let same_minified_value new_decl existing =
  string_of_value ~minify:true new_decl = string_of_value ~minify:true existing

let legacy_vendor_fallback new_decl existing =
  (* Different-value duplicates are kept when one value is vendor-prefixed: the
     cascade may pick whichever the browser understands. *)
  property_name new_decl = property_name existing
  && (not (same_minified_value new_decl existing))
  && (value_is_vendor_prefixed existing || value_is_vendor_prefixed new_decl)

(* The earlier declaration is a real cascade fallback when the later one uses
   CSS Color 4 / 5 syntax that older browsers drop. *)
let legacy_color_fallback new_decl existing =
  property_name new_decl = property_name existing
  && (not (same_minified_value new_decl existing))
  && Declaration.value_uses_color_4 new_decl
  && not (Declaration.value_uses_color_4 existing)

(* Same shape: the later value uses a runtime substitution ([var()] / [env()] /
   [attr()]) and the earlier doesn't, so the earlier is a static fallback for
   browsers that can't resolve the substitution at parse time. *)
let legacy_runtime_subst_fallback new_decl existing =
  property_name new_decl = property_name existing
  && (not (same_minified_value new_decl existing))
  && Declaration.value_uses_runtime_subst new_decl
  && not (Declaration.value_uses_runtime_subst existing)

let same_property_value_declaration new_decl existing =
  property_name new_decl = property_name existing
  && same_minified_value new_decl existing
  && (is_important new_decl || not (is_important existing))

let covered_by_new_declaration new_decl existing =
  let new_prop = property_name new_decl in
  let existing_prop = property_name existing in
  (not (is_intentionally_duplicated existing_prop))
  && declaration_covers new_prop existing_prop
  && (is_important new_decl || not (is_important existing))
  && (not (legacy_vendor_fallback new_decl existing))
  && (not (legacy_color_fallback new_decl existing))
  && not (legacy_runtime_subst_fallback new_decl existing)

let append_all_declaration idx decl kept =
  let before, after =
    List.partition
      (fun (_, old) -> not (all_preserved_reorder_property (property_name old)))
      kept
  in
  before @ [ (idx, decl) ] @ after

let deduplicate_step kept (idx, decl) =
  let prop_name = property_name decl in
  if is_intentionally_duplicated prop_name then
    let kept =
      List.filter
        (fun (_, old) -> not (same_property_value_declaration decl old))
        kept
    in
    kept @ [ (idx, decl) ]
  else if
    (not (is_important decl)) && property_covered_by_important kept prop_name
  then kept
  else
    let kept =
      List.filter
        (fun (_, old) -> not (covered_by_new_declaration decl old))
        kept
    in
    if prop_name = "all" then append_all_declaration idx decl kept
    else kept @ [ (idx, decl) ]

let deduplicate_declarations_with ?(merge_box = true) props =
  let indexed_props = List.mapi (fun i decl -> (i, decl)) props in
  let kept = List.fold_left deduplicate_step [] indexed_props in
  let kept =
    let kept = if merge_box then compose_box_shorthands kept else kept in
    let kept = if merge_box then compose_pair_shorthands kept else kept in
    let kept = if merge_box then compose_outline_shorthand kept else kept in
    let kept = if merge_box then compose_font_shorthand kept else kept in
    let kept =
      if merge_box then merge_box_shorthand_longhands props kept else kept
    in
    let kept = if merge_box then merge_overflow_longhands kept else kept in
    List.map (fun (_, decl) -> decl) kept
  in
  duplicate_buggy_properties kept

let deduplicate_declarations props = deduplicate_declarations_with props
let sort_commuting_declarations decls = decls

let color_custom_property_names stylesheet =
  let var_name_of_custom_property name =
    if String.length name >= 2 && name.[0] = '-' && name.[1] = '-' then
      String.sub name 2 (String.length name - 2)
    else name
  in
  let declaration names = function
    | Declaration
        {
          property = Custom_property name;
          value = Custom_value { value = Typed { kind = Color; _ }; _ };
          _;
        } ->
        String_set.add (var_name_of_custom_property name) names
    | _ -> names
  in
  let declarations names decls = List.fold_left declaration names decls in
  let rec statement names = function
    | Rule rule ->
        let names = declarations names rule.declarations in
        List.fold_left statement names rule.nested
    | Declarations decls -> declarations names decls
    | Layer (_, block)
    | Media (_, block)
    | Container (_, _, block)
    | Supports (_, block)
    | Moz_document (_, block)
    | When (_, block)
    | Else (_, block)
    | Starting_style block
    | Origin (_, block)
    | Scope (_, _, block) ->
        List.fold_left statement names block
    | Page (_, decls) | Position_try (_, decls) | Supports_condition (_, decls)
      ->
        declarations names decls
    | Page_with_margins (_, descs, margins) ->
        let names = declarations names descs in
        List.fold_left
          (fun names margin -> declarations names margin.margin_descriptors)
          names margins
    | _ -> names
  in
  List.fold_left statement String_set.empty stylesheet

let color_fallback_of_length_fallback :
    Values.length Values.fallback -> Values.color Values.fallback = function
  | Values.None -> Values.None
  | Values.Empty -> Values.Empty
  | Values.Empty2 -> Values.Empty2
  | Values.Syntax_fallback components -> Values.Syntax_fallback components
  | Values.Var_fallback name -> Values.Var_fallback name
  | Values.Fallback value ->
      Values.Syntax_fallback
        (Cursor.remaining
           (Cursor.of_string (Pp.to_string ~minify:true Values.pp_length value)))

let color_var_of_length_var (var : Values.length Values.var) :
    Values.color Values.var =
  {
    name = var.name;
    fallback = color_fallback_of_length_fallback var.fallback;
    default = None;
    layer = var.layer;
    meta = var.meta;
  }

let rec normalize_shadow_color_vars color_vars (value : Properties.shadow) :
    Properties.shadow =
  match value with
  | Shadow
      ({ blur = Some (Values.Var var); spread = None; color = None; _ } as
       shadow)
    when String_set.mem var.name color_vars ->
      Shadow
        {
          shadow with
          blur = Some Zero;
          color = Some (Values.Var (color_var_of_length_var var));
        }
  | List shadows ->
      List (List.map (normalize_shadow_color_vars color_vars) shadows)
  | shadow -> shadow

let normalize_shadow_color_var_declaration color_vars = function
  | Declaration ({ property = Box_shadow; value; _ } as decl) ->
      Declaration
        { decl with value = normalize_shadow_color_vars color_vars value }
  | Declaration
      ({
         property = Custom_property _;
         value =
           Custom_value
             { value = Typed { kind = Shadow; value = shadow }; layer; meta };
         _;
       } as decl) ->
      Declaration
        {
          decl with
          value =
            Properties.Custom_value
              {
                value =
                  Properties.Typed
                    {
                      kind = Shadow;
                      value = normalize_shadow_color_vars color_vars shadow;
                    };
                layer;
                meta;
              };
        }
  | decl -> decl

let normalize_shadow_color_var_slots stylesheet =
  let color_vars = color_custom_property_names stylesheet in
  if String_set.is_empty color_vars then stylesheet
  else
    let declarations =
      List.map (normalize_shadow_color_var_declaration color_vars)
    in
    let rec statement = function
      | Rule rule ->
          Rule
            {
              rule with
              declarations = declarations rule.declarations;
              nested = List.map statement rule.nested;
            }
      | Declarations decls -> Declarations (declarations decls)
      | Layer (name, block) -> Layer (name, List.map statement block)
      | Media (query, block) -> Media (query, List.map statement block)
      | Container (name, query, block) ->
          Container (name, query, List.map statement block)
      | Supports (query, block) -> Supports (query, List.map statement block)
      | Moz_document (query, block) ->
          Moz_document (query, List.map statement block)
      | When (query, block) -> When (query, List.map statement block)
      | Else (query, block) -> Else (query, List.map statement block)
      | Starting_style block -> Starting_style (List.map statement block)
      | Origin (origin, block) -> Origin (origin, List.map statement block)
      | Scope (start, end_, block) ->
          Scope (start, end_, List.map statement block)
      | Page (selector, decls) -> Page (selector, declarations decls)
      | Page_with_margins (selector, descs, margins) ->
          Page_with_margins
            ( selector,
              declarations descs,
              List.map
                (fun margin ->
                  {
                    margin with
                    margin_descriptors = declarations margin.margin_descriptors;
                  })
                margins )
      | Position_try (name, decls) -> Position_try (name, declarations decls)
      | Supports_condition (name, decls) ->
          Supports_condition (name, declarations decls)
      | other -> other
    in
    List.map statement stylesheet

(** {1 Rule Optimization} *)

(* Extract the pseudo-element from a selector (::before, ::after, etc.). Returns
   None if no pseudo-element is present. Used to prevent combining selectors
   with different pseudo-elements which would change semantics. *)
let rec extract_pseudo_element : Selector.t -> Selector.t option = function
  | Before f -> Some (Before f)
  | After f -> Some (After f)
  | First_letter f -> Some (First_letter f)
  | First_line f -> Some (First_line f)
  | Marker -> Some Marker
  | Placeholder -> Some Placeholder
  | Selection -> Some Selection
  | File_selector_button -> Some File_selector_button
  | Backdrop -> Some Backdrop
  | Details_content -> Some Details_content
  | Compound sels ->
      (* For compound selectors, look for pseudo-element at the end *)
      List.fold_left
        (fun acc sel ->
          match extract_pseudo_element sel with
          | Some _ as pe -> pe
          | None -> acc)
        None sels
  | Combined (_, _, right) | Relative (_, right) ->
      (* Pseudo-element is always at the end of a combined selector *)
      extract_pseudo_element right
  | _ -> None

(* Check if a selector contains vendor-specific pseudo-elements. These should
   not be grouped because if one selector in a group is invalid in a browser,
   the entire rule fails. Keeping them separate ensures maximum
   compatibility. *)
let rec contains_vendor_pseudo_element : Selector.t -> bool = function
  | File_selector_button -> true
  | Webkit_scrollbar | Webkit_search_cancel_button | Webkit_search_decoration
  | Webkit_datetime_edit_fields_wrapper | Webkit_date_and_time_value
  | Webkit_datetime_edit | Webkit_datetime_edit_year_field
  | Webkit_datetime_edit_month_field | Webkit_datetime_edit_day_field
  | Webkit_datetime_edit_hour_field | Webkit_datetime_edit_minute_field
  | Webkit_datetime_edit_second_field | Webkit_datetime_edit_millisecond_field
  | Webkit_datetime_edit_meridiem_field | Webkit_inner_spin_button
  | Webkit_outer_spin_button | Webkit_details_marker ->
      true
  | Compound sels -> List.exists contains_vendor_pseudo_element sels
  | Combined (left, _, right) ->
      contains_vendor_pseudo_element left
      || contains_vendor_pseudo_element right
  | Relative (_, right) -> contains_vendor_pseudo_element right
  | List sels -> List.exists contains_vendor_pseudo_element sels
  | Not sels -> List.exists contains_vendor_pseudo_element sels
  | Is sels -> List.exists contains_vendor_pseudo_element sels
  | Where sels -> List.exists contains_vendor_pseudo_element sels
  | Has sels -> List.exists contains_vendor_pseudo_element sels
  | _ -> false

let single_rule_without_nested (rule : rule) : rule =
  {
    rule with
    declarations =
      deduplicate_declarations_with ~merge_box:false rule.declarations
      |> sort_commuting_declarations;
  }

let finalize_rule_without_nested (rule : rule) : rule =
  {
    rule with
    declarations =
      deduplicate_declarations rule.declarations |> sort_commuting_declarations;
  }

let rules_have_same_selector (prev : Stylesheet.rule) (rule : Stylesheet.rule) =
  prev.selector = rule.selector
  && not (contains_vendor_pseudo_element rule.selector)

let merge_two_adjacent_rules (prev : Stylesheet.rule) (rule : Stylesheet.rule) :
    Stylesheet.rule =
  (* Same selector and adjacent source position: merge into one block, then
     re-run declaration optimization over the combined source-ordered
     declarations. *)
  {
    selector = prev.selector;
    declarations = prev.declarations @ rule.declarations;
    nested = prev.nested @ rule.nested;
    merge_key = prev.merge_key;
  }

let merge_rules (rules : Stylesheet.rule list) : Stylesheet.rule list =
  (* Only merge truly adjacent rules with the same selector to preserve cascade
     order. This is safe because we don't reorder rules - we only combine
     immediately adjacent rules with identical selectors, which maintains
     cascade semantics.

     However, we don't merge vendor-specific pseudo-elements to match Tailwind's
     behavior and ensure browser compatibility. *)
  let step acc prev rule rest =
    if rules_have_same_selector prev rule then
      let merged = merge_two_adjacent_rules prev rule in
      (acc, Some merged, rest)
    else (prev :: acc, Some rule, rest)
  in
  let rec merge_adjacent (acc : Stylesheet.rule list)
      (prev_rule : Stylesheet.rule option) :
      Stylesheet.rule list -> Stylesheet.rule list = function
    | [] -> List.rev (match prev_rule with Some r -> r :: acc | None -> acc)
    | (rule : Stylesheet.rule) :: rest -> (
        match prev_rule with
        | None -> merge_adjacent acc (Some rule) rest
        | Some prev ->
            let acc', prev', rest' = step acc prev rule rest in
            merge_adjacent acc' prev' rest')
  in
  merge_adjacent [] None rules

(** Check if a selector uses a descendant combinator with a pseudo-element (e.g.
    [.marker:flex ::marker]). These must not be combined with direct
    pseudo-element selectors (e.g. [.marker:flex::marker]) because they target
    different elements. *)
let rec has_descendant_pseudo_element : Selector.t -> bool = function
  | Combined (_, Descendant, right) -> extract_pseudo_element right <> None
  | Compound sels -> List.exists has_descendant_pseudo_element sels
  | List sels -> List.exists has_descendant_pseudo_element sels
  | _ -> false

(* Check if a selector has a descendant combinator that would make combining
   unsafe. Descendant combinators where the ancestor is :where() are safe
   because :where() has zero specificity and is always valid in selector lists
   (e.g., :where(.group) .in-[.group]:flex). *)
let rec has_descendant_combinator : Selector.t -> bool = function
  | Combined (Where _, Descendant, _) -> false
  | Combined (_, Descendant, _) -> true
  | Compound sels -> List.exists has_descendant_combinator sels
  | List sels -> List.exists has_descendant_combinator sels
  | _ -> false

let should_not_combine selector =
  (* Already a list selector - don't combine *)
  Selector.is_compound_list selector
  (* Check if selector contains vendor-specific pseudo-elements These should not
     be grouped because: - If one selector in a group is invalid in a browser,
     the entire rule fails - Keeping them separate ensures maximum browser
     compatibility *)
  || contains_vendor_pseudo_element selector
  (* Descendant pseudo-element selectors (.x ::marker) must not be combined with
     direct ones (.x::marker) — they target different elements *)
  || has_descendant_pseudo_element selector
  ||
  (* Descendant combinator selectors (e.g., .prose :where(ol[type=i
     s]):not(...)) should not be combined — Tailwind keeps them separate even
     with identical declarations, to match tailwindcss output exactly. *)
  has_descendant_combinator selector

(* Count the modifier depth (number of ':' separators) in a class name. E.g.,
   "group-focus:flex" has depth 1, "group-focus:group-hover:flex" has depth 2.
   Only selectors with the same modifier depth should be combined. *)

(** Check if two selectors can be combined. This is only called when the
    declarations are already verified to be identical. Following Tailwind v4's
    behavior:
    - Don't combine if the selectors have different pseudo-elements (::before vs
      ::after) since they target different generated content
    - Don't combine if one has modifiers and one doesn't (e.g., .bg-blue-500 and
      .aria-selected:bg-blue-500) to preserve ordering semantics
    - Otherwise, can combine since both rules set the same values *)
let modifier_depth class_name =
  let len = String.length class_name in
  let rec loop i depth bracket_depth =
    if i >= len then depth
    else
      match class_name.[i] with
      | '[' -> loop (i + 1) depth (bracket_depth + 1)
      | ']' -> loop (i + 1) depth (max 0 (bracket_depth - 1))
      | '\\' when i + 1 < len && class_name.[i + 1] = ':' ->
          (* Skip escaped colon \: — not a modifier separator *)
          loop (i + 2) depth bracket_depth
      | ':' when bracket_depth = 0 -> loop (i + 1) (depth + 1) bracket_depth
      | _ -> loop (i + 1) depth bracket_depth
  in
  loop 0 0 0

let has_escaped_colon class_name =
  let len = String.length class_name in
  let rec check i =
    if i >= len - 1 then false
    else if class_name.[i] = '\\' && class_name.[i + 1] = ':' then true
    else check (i + 1)
  in
  check 0

let can_combine_selectors sel1 sel2 =
  (* Check if pseudo-elements match (both None, or both the same) *)
  let pe1 = extract_pseudo_element sel1 in
  let pe2 = extract_pseudo_element sel2 in
  if pe1 <> pe2 then false
  else
    (* Tailwind v4 combines consecutive rules with identical declarations. For
       variant-prefixed classes (e.g., group-X:flex, peer-X:flex), we require
       the same modifier depth to avoid combining different nesting levels.
       Simple class selectors always combine. *)
    match (Selector.first_class sel1, Selector.first_class sel2) with
    | Some c1, Some c2 ->
        (* Don't combine if one has escaped colon (variant-prefixed like
           peer-checked\:font-semibold) and the other doesn't *)
        if has_escaped_colon c1 <> has_escaped_colon c2 then false
        else
          let d1 = modifier_depth c1 in
          let d2 = modifier_depth c2 in
          (d1 > 0 && d2 > 0) || d1 = d2
    | _ -> false

(* Check if a selector contains a :not() pseudo-class at the top level *)
let rec has_not_pseudo = function
  | Selector.Not _ -> true
  | Selector.Compound sels -> List.exists has_not_pseudo sels
  | _ -> false

(* Check if a selector contains a :has() pseudo-class at the top level *)
let rec has_has_pseudo = function
  | Selector.Has _ -> true
  | Selector.Compound sels -> List.exists has_has_pseudo sels
  | _ -> false

(* Sort selectors for merging: not-* first (sub-sorted by group/peer/plain),
   group-* second, peer-* third, ancestor-context fourth, has-* last. Uses
   structured selector analysis. *)
let base_sort_key sel =
  if has_not_pseudo sel then
    (* Sub-classify not-* selectors by group/peer/plain *)
    if Selector.has_group_marker sel then -3
    else if Selector.has_peer_marker sel then -2
    else -1
  else if has_has_pseudo sel then
    (* Only give :has() a distinct sort key when combined with group/peer
       markers, so group-has-* sorts after group-* and peer-has-* after peer-*.
       Plain has-* selectors sort among regular selectors. *)
    if Selector.has_group_marker sel || Selector.has_peer_marker sel then 3
    else 2
  else if Selector.has_group_marker sel then 0
  else if Selector.has_peer_marker sel then 1
  else 2

let is_ancestor_context = function
  | Selector.Combined (Selector.Where _, Selector.Descendant, _) -> true
  | _ -> false

let selector_sort_key sel =
  let base = if is_ancestor_context sel then 2 else base_sort_key sel in
  let depth =
    match Selector.first_class sel with
    | Some cls -> modifier_depth cls
    | None -> 0
  in
  (* Sort nth variants by selector AST type: nth-child < nth-last-child <
     nth-of-type < nth-last-of-type, matching Tailwind v4 ordering where "last"
     follows its non-last counterpart *)
  let nth_order =
    let rec find_nth = function
      | Selector.Nth_last_of_type _ -> 3
      | Selector.Nth_of_type _ -> 2
      | Selector.Nth_last_child _ -> 1
      | Selector.Compound sels ->
          List.fold_left (fun acc s -> max acc (find_nth s)) 0 sels
      | _ -> 0
    in
    find_nth sel
  in
  (base, nth_order, depth)

let class_has_bracket cls =
  (* Check if the class name contains a bracket [, indicating an arbitrary
     variant like group-has-[:checked] vs a named variant like
     group-has-checked. Named variants sort before bracket variants. Note: class
     names are stored unescaped in the AST. *)
  String.contains cls '['

let class_base_and_slash cls =
  (* Extract the base variant name and whether there's a /name suffix. E.g.,
     "group-has-checked/parent-name:flex" -> ("group-has-checked", true)
     "group-has-checked:flex" -> ("group-has-checked", false)
     "group-has-[:checked]:flex" -> ("group-has-[:checked]", false) This allows
     grouping variants by their base name, with plain before /name variants
     within each group. *)
  let len = String.length cls in
  (* Find / or trailing :utility, whichever comes first (outside brackets) *)
  let rec find_end i depth =
    if i >= len then (cls, false)
    else
      match cls.[i] with
      | '[' -> find_end (i + 1) (depth + 1)
      | ']' -> find_end (i + 1) (max 0 (depth - 1))
      | '/' when depth = 0 -> (String.sub cls 0 i, true)
      | ':' when depth = 0 -> (String.sub cls 0 i, false)
      | _ -> find_end (i + 1) depth
  in
  find_end 0 0

(** Extract variant family prefix (e.g., "group-has-" from
    "group-has-[:checked]:flex" or "group-has-checked:flex"). Used to group
    named and bracket variants of the same family. *)
let variant_family cls =
  let bracket_pos = String.index_opt cls '[' in
  let colon_pos =
    (* Find first : outside brackets *)
    let len = String.length cls in
    let rec find i depth =
      if i >= len then None
      else
        match cls.[i] with
        | '[' -> find (i + 1) (depth + 1)
        | ']' -> find (i + 1) (max 0 (depth - 1))
        | ':' when depth = 0 -> Some i
        | _ -> find (i + 1) depth
    in
    find 0 0
  in
  match (bracket_pos, colon_pos) with
  | Some bi, _ -> String.sub cls 0 bi (* prefix before bracket *)
  | _, Some ci -> String.sub cls 0 ci (* prefix before colon *)
  | _ -> cls

let bracket_content_type cls =
  match String.index_opt cls '[' with
  | Some i when i + 1 < String.length cls && cls.[i + 1] = ':' -> 0
  | _ -> 1

let normalize_attr_base b =
  let starts_with s p =
    String.length s >= String.length p && String.sub s 0 (String.length p) = p
  in
  if
    starts_with b "aria-[" || starts_with b "data-["
    || starts_with b "group-aria-["
    || starts_with b "group-data-["
    || starts_with b "peer-aria-["
    || starts_with b "peer-data-["
  then String.map (fun c -> if c = '_' then ' ' else c) b
  else b

(** Compare two bracket selectors within the same variant family. Sorts by
    bracket content type (pseudo-class before combinator), then by normalized
    base name, then by slash presence, then by index. *)
let compare_bracket_selectors c1 c2 i1 i2 =
  let bt_cmp =
    Int.compare (bracket_content_type c1) (bracket_content_type c2)
  in
  if bt_cmp <> 0 then bt_cmp
  else
    let base1, slash1 = class_base_and_slash c1 in
    let base2, slash2 = class_base_and_slash c2 in
    let base_cmp =
      String.compare (normalize_attr_base base1) (normalize_attr_base base2)
    in
    if base_cmp <> 0 then base_cmp
    else
      let slash_cmp = Bool.compare slash1 slash2 in
      if slash_cmp <> 0 then slash_cmp else Int.compare i1 i2

type selector_kind = Named | Bracket

(** Compare two selectors within the same variant family. Named variants sort
    before bracket variants; within each group, bracket variants use
    [compare_bracket_selectors] and non-bracket variants preserve original
    insertion order. *)
let compare_same_family_selectors kind1 kind2 c1 c2 i1 i2 =
  match (kind1, kind2) with
  | Named, Bracket -> -1
  | Bracket, Named -> 1
  | Bracket, Bracket -> compare_bracket_selectors c1 c2 i1 i2
  | Named, Named -> Int.compare i1 i2

let compare_selectors_for_merge (sel1, i1) (sel2, i2) =
  let k1 = selector_sort_key sel1 and k2 = selector_sort_key sel2 in
  let c = compare k1 k2 in
  if c <> 0 then c
  else
    let cls1 = Selector.first_class sel1 in
    let cls2 = Selector.first_class sel2 in
    let c1 = match cls1 with Some c -> c | None -> "" in
    let c2 = match cls2 with Some c -> c | None -> "" in
    let kind_of b = if b then Bracket else Named in
    let k1 = kind_of (class_has_bracket c1) in
    let k2 = kind_of (class_has_bracket c2) in
    let fam1 = variant_family c1 in
    let fam2 = variant_family c2 in
    if fam1 = fam2 then compare_same_family_selectors k1 k2 c1 c2 i1 i2
    else if k1 = Bracket && k2 = Bracket then
      (* Different families, both bracket: compare by class name so that e.g.
         hover:peer-[&_p] sorts before peer-[&_p]:hover *)
      let cls_cmp = String.compare c1 c2 in
      if cls_cmp <> 0 then cls_cmp else Int.compare i1 i2
    else
      (* Different families, at least one non-bracket: preserve original
         insertion order to respect variant cascade ordering *)
      Int.compare i1 i2

(* Convert group of selectors to a rule *)
let group_to_rule :
    (Selector.t * declaration list * string option) list ->
    Stylesheet.rule option = function
  | [ (sel, decls, _) ] ->
      Some
        { selector = sel; declarations = decls; nested = []; merge_key = None }
  | [] -> None
  | group ->
      let selector_list = List.rev group |> List.map (fun (s, _, _) -> s) in
      (* Always sort: group-* first, peer-* second, base last with stable index
         tiebreaker. This matches Tailwind's selector list ordering. *)
      let sorted_selectors =
        let indexed = List.mapi (fun i s -> (s, i)) selector_list in
        List.sort compare_selectors_for_merge indexed |> List.map fst
      in
      let _, decls, _ = List.hd group in
      (* Create a List selector from all the selectors *)
      let combined_selector =
        if List.length sorted_selectors = 1 then List.hd sorted_selectors
        else Selector.list sorted_selectors
      in
      Some
        {
          selector = combined_selector;
          declarations = decls;
          nested = [];
          merge_key = None;
        }

(* Flush current group to accumulator *)
let flush_group acc group =
  match group_to_rule group with Some rule -> rule :: acc | None -> acc

(* Don't combine selectors when one uses :is(:where()) (group/peer) and the
   other uses a newer pseudo-class directly. In a selector list, if the newer
   pseudo-class is unsupported, the entire rule is dropped — but the
   :is(:where()) variant would have survived on its own due to forgiving
   selector parsing. *)
let newer_pseudo_class_compatible sel1 sel2 =
  let sel1_complex = Selector.has_is_where_pattern sel1 in
  let sel2_complex = Selector.has_is_where_pattern sel2 in
  if sel1_complex <> sel2_complex then
    let plain_sel = if sel1_complex then sel2 else sel1 in
    not (Selector.has_newer_pseudo_class plain_sel)
  else true

(* Lightning CSS does not merge rules when values contain the 'none' keyword in
   color functions like oklab(). Check if a CSS string contains this pattern. *)
let has_oklab_none s =
  (* Search for "oklab(" then check if "none" appears before the closing ")" *)
  let len = String.length s in
  let rec find_oklab i =
    if i > len - 6 then false
    else if
      s.[i] = 'o'
      && s.[i + 1] = 'k'
      && s.[i + 2] = 'l'
      && s.[i + 3] = 'a'
      && s.[i + 4] = 'b'
      && s.[i + 5] = '('
    then check_none (i + 6)
    else find_oklab (i + 1)
  and check_none i =
    if i > len - 4 then false
    else if s.[i] = ')' then find_oklab (i + 1)
    else if
      s.[i] = 'n'
      && s.[i + 1] = 'o'
      && s.[i + 2] = 'n'
      && s.[i + 3] = 'e'
      && (i + 4 >= len || s.[i + 4] = ' ' || s.[i + 4] = ')' || s.[i + 4] = '/')
    then true
    else check_none (i + 1)
  in
  find_oklab 0

let declarations_css_equal d1 d2 =
  (d1 = d2
  ||
  let pp_decls ctx ds = List.iter (Declaration.pp_declaration ctx) ds in
  let s1 = Pp.to_string ~minify:true pp_decls d1 in
  let s2 = Pp.to_string ~minify:true pp_decls d2 in
  s1 = s2)
  &&
  let pp_decls ctx ds = List.iter (Declaration.pp_declaration ctx) ds in
  let s = Pp.to_string ~minify:true pp_decls d1 in
  not (has_oklab_none s)

let can_combine_rules (prev : Stylesheet.rule) (rule : Stylesheet.rule) =
  declarations_css_equal prev.declarations rule.declarations
  && newer_pseudo_class_compatible prev.selector rule.selector
  &&
  match (prev.merge_key, rule.merge_key) with
  | Some k1, Some k2 when k1 = k2 ->
      (* When both have the same merge_key, allow combining unless they have
         incompatible pseudo-elements. Pseudo-elements in the same "tier" can
         combine (e.g. ::placeholder + ::backdrop), but pseudo-elements from
         different tiers cannot (e.g. ::backdrop + ::details-content). *)
      let pe1 = extract_pseudo_element prev.selector in
      let pe2 = extract_pseudo_element rule.selector in
      let pseudo_tier = function
        | None -> 0
        | Some (Selector.Before _) | Some (After _) -> 1
        | Some (First_letter _) | Some (First_line _) -> 2
        | Some Placeholder | Some Backdrop -> 3
        | Some Details_content -> 4
        | Some Marker -> 5
        | Some Selection -> 6
        | Some File_selector_button -> 7
        | Some _ -> 8
      in
      pseudo_tier pe1 = pseudo_tier pe2
  | _ ->
      (* Different or missing merge_keys: fall through to selector compatibility
         check. Declarations are already verified identical at call site, so two
         utilities with the same CSS content (e.g. shadow and shadow-sm in v4)
         can combine when their selectors are compatible. *)
      can_combine_selectors prev.selector rule.selector

let rule_cannot_combine (rule : Stylesheet.rule) =
  (* Don't combine rules with nested statements or rules whose selectors are
     structurally incompatible with combining (e.g., vendor pseudo-elements,
     descendant pseudo-elements). Always check should_not_combine regardless of
     merge_key — merge_key controls whether identical-declaration rules CAN
     combine, but structural selector constraints still apply. *)
  rule.Stylesheet_intf.nested <> [] || should_not_combine rule.selector

let group_member_of_rule (rule : Stylesheet.rule) =
  (rule.selector, rule.declarations, rule.merge_key)

let prev_rule_of_group_head (prev_sel, prev_decls, prev_merge_key) =
  {
    Stylesheet_intf.selector = prev_sel;
    declarations = prev_decls;
    nested = [];
    merge_key = prev_merge_key;
  }

let combine_identical_rules (rules : Stylesheet.rule list) :
    Stylesheet.rule list =
  (* Cross-rule combining is sound when an intervening rule's subject is
     definitely disjoint from every member of the merge group: no element can
     match both, so moving the intervening rule past the merged group can't
     change the cascade for any element. We carry a [delayed] list of such
     rules; on flush they are emitted after the merged group rule, preserving
     their relative order. The invariant is: every rule in [delayed] has a
     subject summary that does not [may_overlap] any group member's subject. *)
  let summary_of_rule (rule : Stylesheet.rule) =
    Selector_summary.of_selector rule.Stylesheet_intf.selector
  in
  let disjoint_from summaries candidate =
    List.for_all
      (fun s -> not (Selector_summary.may_overlap s candidate))
      summaries
  in
  let flush acc current_group delayed =
    let group_members = List.map fst current_group in
    let acc = flush_group acc group_members in
    List.fold_left (fun acc (rule, _) -> rule :: acc) acc delayed
  in
  let rec combine_consecutive acc current_group delayed = function
    | [] -> List.rev (flush acc current_group delayed)
    | (rule : Stylesheet.rule) :: rest ->
        if rule_cannot_combine rule then
          let acc = rule :: flush acc current_group delayed in
          combine_consecutive acc [] [] rest
        else extend_delay_or_restart acc current_group delayed rule rest
  and extend_delay_or_restart acc current_group delayed rule rest =
    let rule_summary = summary_of_rule rule in
    let push_to_group () =
      let member = (group_member_of_rule rule, rule_summary) in
      combine_consecutive acc (member :: current_group) delayed rest
    in
    match current_group with
    | [] -> push_to_group ()
    | (head_member, _) :: _ ->
        let prev_rule = prev_rule_of_group_head head_member in
        let delayed_summaries = List.map snd delayed in
        if
          can_combine_rules prev_rule rule
          && disjoint_from delayed_summaries rule_summary
        then push_to_group ()
        else
          let group_summaries = List.map snd current_group in
          if disjoint_from group_summaries rule_summary then
            combine_consecutive acc current_group
              (delayed @ [ (rule, rule_summary) ])
              rest
          else
            let acc = flush acc current_group delayed in
            let member = (group_member_of_rule rule, rule_summary) in
            combine_consecutive acc [ member ] [] rest
  in
  combine_consecutive [] [] [] rules

(** {1 Statement Optimization} *)

(* Merge consecutive media queries with the same condition. This only merges
   immediately adjacent media queries to preserve cascade order. When blocks are
   merged, we recursively call merge_consecutive_media on the combined content
   to merge any inner consecutive media queries. *)
(* Forward declaration to allow merge_consecutive_media to call statements *)
let statements_ref : (statement list -> statement list) ref =
  ref (fun stmts -> stmts)

let flatten_rule_ref : (rule -> statement list) ref =
  ref (fun _ -> assert false)

(* Shared predicates for media block optimization *)
let rec should_consolidate cond =
  match cond with
  | Media.Min_width _ | Media.Min_width_rem _ | Media.Max_width _
  | Media.Min_width_length _ | Media.Not_min_width_length _
  | Media.Prefers_reduced_motion _ | Media.Prefers_color_scheme _ ->
      true
  | Media.Negated inner -> should_consolidate inner
  | _ -> false

let is_responsive_media = function
  | Media (cond, _) -> (
      match cond with
      | Media.Min_width _ | Media.Min_width_rem _ | Media.Max_width _
      | Media.Min_width_length _ | Media.Not_min_width_length _ ->
          true
      | Media.Negated inner -> (
          match inner with
          | Media.Min_width _ | Media.Min_width_rem _ | Media.Max_width _
          | Media.Min_width_length _ | Media.Not_min_width_length _ ->
              true
          | _ -> false)
      | _ -> false)
  | _ -> false

let has_nested_preference_media block =
  List.exists
    (function
      | Media (cond, _) -> (
          match cond with
          | Prefers_contrast _ | Prefers_reduced_motion _
          | Prefers_color_scheme _ ->
              true
          | _ -> false)
      | _ -> false)
    block

let is_container_block block =
  List.exists
    (function
      | Rule { selector; _ } -> Selector.to_string selector = ".container"
      | _ -> false)
    block

let is_preference_media_cond = function
  | Media.Prefers_color_scheme _ | Media.Prefers_reduced_motion _
  | Media.Prefers_contrast _ ->
      true
  | _ -> false

let record_consolidated_media ~media_map ~last_pos ~first_responsive_pos
    ~has_responsive i stmt cond block =
  let key = Media.to_string cond in
  Hashtbl.replace last_pos key i;
  let existing =
    try Hashtbl.find media_map key with Not_found -> (cond, [])
  in
  let _, blocks = existing in
  Hashtbl.replace media_map key (cond, blocks @ [ block ]);
  if is_responsive_media stmt then (
    has_responsive := true;
    if !first_responsive_pos = None then first_responsive_pos := Some i)

let collect_media_step ~media_map ~last_pos ~first_responsive_pos
    ~has_responsive ~has_preference_media i stmt =
  match stmt with
  | Media (cond, block)
    when should_consolidate cond
         && (not (has_nested_preference_media block))
         && not (is_container_block block) ->
      record_consolidated_media ~media_map ~last_pos ~first_responsive_pos
        ~has_responsive i stmt cond block
  | Media (cond, _) ->
      if is_preference_media_cond cond then has_preference_media := true
  | _ when is_responsive_media stmt ->
      has_responsive := true;
      if !first_responsive_pos = None then first_responsive_pos := Some i
  | _ -> ()

let collect_media_data stmts =
  let media_map = Hashtbl.create 16 in
  let last_pos = Hashtbl.create 16 in
  let first_responsive_pos = ref None in
  let has_responsive = ref false in
  let has_preference_media = ref false in
  List.iteri
    (collect_media_step ~media_map ~last_pos ~first_responsive_pos
       ~has_responsive ~has_preference_media)
    stmts;
  ( media_map,
    last_pos,
    !first_responsive_pos,
    !has_responsive,
    !has_preference_media )

let compute_hover_insert_pos stmts ~first_responsive_pos ~has_responsive
    ~has_preference_media =
  let regular_stmt_count =
    List.fold_left
      (fun acc stmt -> match stmt with Rule _ -> acc + 1 | _ -> acc)
      0 stmts
  in
  let is_top_level =
    has_responsive || has_preference_media || regular_stmt_count > 10
  in
  let hover_insert_pos =
    match (first_responsive_pos, is_top_level) with
    | Some pos, _ -> pos
    | None, true -> List.length stmts
    | None, false -> -1
  in
  (hover_insert_pos, is_top_level)

(* Group all media blocks with the same condition together, for specific media
   types (Hover, Min_width, Max_width, Prefers_reduced_motion). This allows
   @media (hover:hover) blocks to be consolidated into a single block matching
   Tailwind's behavior.

   For @media (hover:hover), the consolidated block is placed after all Regular
   utilities but before responsive media queries (@media (min-width:...)). For
   responsive media, the consolidated block is placed at the last occurrence. *)
(* Flush pending hover/motion blocks at the insertion point. Returns the
   updated accumulator with pending blocks prepended (in reverse order). *)
let emit_pending_hover ~hover_insert_pos ~pending_hover_blocks
    ~pending_motion_blocks i acc =
  if i = hover_insert_pos && List.length !pending_hover_blocks > 0 then (
    let all_pending = !pending_hover_blocks @ !pending_motion_blocks in
    let hover_acc = List.rev_append all_pending acc in
    pending_hover_blocks := [];
    pending_motion_blocks := [];
    hover_acc)
  else acc

(* Route a consolidated media block: either defer it to a pending list for later
   repositioning, or emit it directly at the current position. *)
let route_consolidated ~should_reposition_hover ~is_top_level
    ~pending_hover_blocks ~pending_motion_blocks:_ consolidated (cond : Media.t)
    acc =
  match cond with
  | Media.Hover Media.Hover when should_reposition_hover ->
      (* For hover at top-level, add to pending list for repositioning. Append
         to maintain order (first occurrence stays first). *)
      pending_hover_blocks := !pending_hover_blocks @ [ consolidated ];
      acc
  | Media.Prefers_reduced_motion _ when is_top_level ->
      (* Motion blocks are positioned correctly by variant_order sorting. Emit
         directly at their sorted position. *)
      consolidated :: acc
  | _ ->
      (* For responsive media, or hover in nested context, emit at last
         position *)
      consolidated :: acc

let try_consolidate_media ~optimize_merged_block ~media_map ~last_pos
    ~emitted_media ~should_reposition_hover ~is_top_level ~pending_hover_blocks
    ~pending_motion_blocks i stmt acc =
  match stmt with
  | Media (cond, block)
    when should_consolidate cond
         && (not (has_nested_preference_media block))
         && not (is_container_block block) ->
      let key = Media.to_string cond in
      if Hashtbl.mem last_pos key then
        let is_last_pos = Hashtbl.find last_pos key = i in
        if is_last_pos && not (Hashtbl.mem emitted_media key) then (
          Hashtbl.add emitted_media key true;
          let _, all_blocks = Hashtbl.find media_map key in
          let merged = List.concat all_blocks in
          let consolidated = Media (cond, optimize_merged_block merged) in
          route_consolidated ~should_reposition_hover ~is_top_level
            ~pending_hover_blocks ~pending_motion_blocks consolidated cond acc)
        else acc
      else stmt :: acc
  | _ -> stmt :: acc

let _consolidate_media_blocks (stmts : statement list) : statement list =
  let optimize_merged_block block = !statements_ref block in
  let ( media_map,
        last_pos,
        first_responsive_pos,
        has_responsive,
        has_preference_media ) =
    collect_media_data stmts
  in
  let hover_insert_pos, is_top_level =
    compute_hover_insert_pos stmts ~first_responsive_pos ~has_responsive
      ~has_preference_media
  in
  let emitted_media = Hashtbl.create 16 in
  let pending_hover_blocks = ref [] in
  let pending_motion_blocks = ref [] in
  let should_reposition_hover = hover_insert_pos >= 0 in

  let rec filter_with_index i acc = function
    | [] -> List.rev_append acc (!pending_hover_blocks @ !pending_motion_blocks)
    | stmt :: rest ->
        let acc_with_hover =
          emit_pending_hover ~hover_insert_pos ~pending_hover_blocks
            ~pending_motion_blocks i acc
        in
        let new_acc =
          try_consolidate_media ~optimize_merged_block ~media_map ~last_pos
            ~emitted_media ~should_reposition_hover ~is_top_level
            ~pending_hover_blocks ~pending_motion_blocks i stmt acc_with_hover
        in
        filter_with_index (i + 1) new_acc rest
  in
  filter_with_index 0 [] stmts

(* CSS Cascade 6.4: consecutive named [@layer] blocks with the same name are
   spec-equivalent to a single block. Merge them when no rule with a conflicting
   condition appears between. Anonymous layers stay distinct because each
   [@layer { ... }] without a name creates a new layer. *)
let merge_consecutive_layers (stmts : statement list) : statement list =
  let optimize_merged_block block = !statements_ref block in
  let rec merge result prev = function
    | [] -> (
        match prev with
        | Some (Some name, block) ->
            result @ [ Layer (Some name, optimize_merged_block block) ]
        | Some (None, block) -> result @ [ Layer (None, block) ]
        | None -> result)
    | Layer (Some name, block) :: rest -> (
        match prev with
        | Some (Some prev_name, prev_block) when String.equal prev_name name ->
            merge result (Some (Some name, prev_block @ block)) rest
        | Some (Some prev_name, prev_block) ->
            merge
              (result
              @ [ Layer (Some prev_name, optimize_merged_block prev_block) ])
              (Some (Some name, block))
              rest
        | Some (None, prev_block) ->
            merge
              (result @ [ Layer (None, prev_block) ])
              (Some (Some name, block))
              rest
        | None -> merge result (Some (Some name, block)) rest)
    | (Layer (None, _) as anon) :: rest -> (
        match prev with
        | Some (Some prev_name, prev_block) ->
            merge
              (result
              @ [ Layer (Some prev_name, optimize_merged_block prev_block) ])
              None (anon :: rest)
        | Some (None, prev_block) ->
            merge (result @ [ Layer (None, prev_block) ]) None (anon :: rest)
        | None -> merge (result @ [ anon ]) None rest)
    | stmt :: rest -> (
        match prev with
        | Some (Some name, block) ->
            merge
              (result @ [ Layer (Some name, optimize_merged_block block); stmt ])
              None rest
        | Some (None, block) ->
            merge (result @ [ Layer (None, block); stmt ]) None rest
        | None -> merge (result @ [ stmt ]) None rest)
  in
  merge [] None stmts

let merge_consecutive_media (stmts : statement list) : statement list =
  let optimize_merged_block block =
    (* When we merge media blocks, the resulting block may have consecutive
       rules with identical declarations that should be combined. We need to
       re-run the full optimization pipeline on the merged content. *)
    !statements_ref block
  in
  let rec merge result prev_media = function
    | [] -> (
        match prev_media with
        | Some (cond, block) ->
            (* Emit pending media block with re-optimized content *)
            result @ [ Media (cond, optimize_merged_block block) ]
        | None -> result)
    | Media (cond, block) :: rest -> (
        match prev_media with
        | Some (prev_cond, prev_block)
          when Media.equal prev_cond cond
               (* Don't merge if either block contains nested preference media.
                  This keeps stacked preference modifiers separate while
                  allowing other nested media (like hover inside dark) to be
                  merged. *)
               && not
                    (has_nested_preference_media prev_block
                    || has_nested_preference_media block) ->
            (* Same condition and compatible structure - merge the blocks *)
            let merged_block = prev_block @ block in
            merge result (Some (cond, merged_block)) rest
        | Some (prev_cond, prev_block) ->
            (* Different condition - emit previous (with optimized content),
               store new one *)
            merge
              (result @ [ Media (prev_cond, optimize_merged_block prev_block) ])
              (Some (cond, block))
              rest
        | None ->
            (* First media query - store it *)
            merge result (Some (cond, block)) rest)
    | stmt :: rest -> (
        (* Non-media statement - flush any pending media query *)
        match prev_media with
        | Some (cond, block) ->
            (* Emit media query (with optimized content) then the statement *)
            merge
              (result @ [ Media (cond, optimize_merged_block block); stmt ])
              None rest
        | None -> merge (result @ [ stmt ]) None rest)
  in
  merge [] None stmts

(* CSS Conditional Rules 5: adjacent same-condition [@supports] / [@container]
   blocks may be merged because the cascade evaluates them identically. Mirror
   the [@media] approach. *)
let merge_consecutive_supports (stmts : statement list) : statement list =
  let optimize_merged_block block = !statements_ref block in
  let rec merge result prev = function
    | [] -> (
        match prev with
        | Some (cond, block) ->
            result @ [ Supports (cond, optimize_merged_block block) ]
        | None -> result)
    | Supports (cond, block) :: rest -> (
        match prev with
        | Some (prev_cond, prev_block) when Supports.equal prev_cond cond ->
            merge result (Some (cond, prev_block @ block)) rest
        | Some (prev_cond, prev_block) ->
            merge
              (result
              @ [ Supports (prev_cond, optimize_merged_block prev_block) ])
              (Some (cond, block))
              rest
        | None -> merge result (Some (cond, block)) rest)
    | stmt :: rest -> (
        match prev with
        | Some (cond, block) ->
            merge
              (result @ [ Supports (cond, optimize_merged_block block); stmt ])
              None rest
        | None -> merge (result @ [ stmt ]) None rest)
  in
  merge [] None stmts

let merge_consecutive_containers (stmts : statement list) : statement list =
  let optimize_merged_block block = !statements_ref block in
  let compare_condition a b =
    match (a, b) with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some a, Some b -> Container.compare a b
  in
  let rec merge result prev = function
    | [] -> (
        match prev with
        | Some (name, cond, block) ->
            result @ [ Container (name, cond, optimize_merged_block block) ]
        | None -> result)
    | Container (name, cond, block) :: rest -> (
        match prev with
        | Some (prev_name, prev_cond, prev_block)
          when prev_name = name && compare_condition prev_cond cond = 0 ->
            merge result (Some (name, cond, prev_block @ block)) rest
        | Some (prev_name, prev_cond, prev_block) ->
            merge
              (result
              @ [
                  Container
                    (prev_name, prev_cond, optimize_merged_block prev_block);
                ])
              (Some (name, cond, block))
              rest
        | None -> merge result (Some (name, cond, block)) rest)
    | stmt :: rest -> (
        match prev with
        | Some (name, cond, block) ->
            merge
              (result
              @ [ Container (name, cond, optimize_merged_block block); stmt ])
              None rest
        | None -> merge (result @ [ stmt ]) None rest)
  in
  merge [] None stmts

(* Check if a layer block contains only empty rules or no statements *)
let is_layer_empty (block : statement list) : bool =
  List.for_all
    (function Rule { declarations = []; _ } -> true | _ -> false)
    block
  || block = []

(* Collect consecutive empty named layers and merge them into a Layer_decl *)
let rec collect_empty_layer_names names remaining =
  match remaining with
  | Layer (Some layer_name, layer_block) :: rest when is_layer_empty layer_block
    ->
      collect_empty_layer_names (layer_name :: names) rest
  | Layer_decl existing_names :: rest ->
      (* Merge with existing layer declaration *)
      (List.rev names @ existing_names, rest)
  | _ -> (List.rev names, remaining)

(* Merge consecutive Layer_decl statements *)
let merge_layer_declarations (stmts : statement list) : statement list =
  let rec merge acc = function
    | [] -> List.rev acc
    | Layer_decl names1 :: Layer_decl names2 :: rest ->
        (* Merge consecutive layer declarations *)
        merge acc (Layer_decl (names1 @ names2) :: rest)
    | stmt :: rest -> merge (stmt :: acc) rest
  in
  merge [] stmts

let add_new_layer_names seen names =
  let seen, added_rev =
    List.fold_left
      (fun (seen, added_rev) name ->
        if List.exists (String.equal name) seen then (seen, added_rev)
        else (name :: seen, name :: added_rev))
      (seen, []) names
  in
  (seen, List.rev added_rev)

let layer_names_of_statement = function
  | Layer_decl names -> names
  | Layer (Some name, _) -> [ name ]
  | _ -> []

let following_layer_introduction_order seen stmts =
  let rec loop seen introduced_rev added_by_layer_decl = function
    | [] | Import _ :: _ -> (List.rev introduced_rev, added_by_layer_decl)
    | stmt :: rest ->
        let names = layer_names_of_statement stmt in
        let seen, added = add_new_layer_names seen names in
        let added_by_layer_decl =
          added_by_layer_decl
          || match stmt with Layer_decl _ -> added <> [] | _ -> false
        in
        loop seen
          (List.rev_append added introduced_rev)
          added_by_layer_decl rest
  in
  loop seen [] false stmts

let list_has_prefix prefix list =
  let rec loop = function
    | [], _ -> true
    | _ :: _, [] -> false
    | x :: xs, y :: ys -> String.equal x y && loop (xs, ys)
  in
  loop (prefix, list)

let layer_decl_forward_redundant seen names rest =
  let _, introduced_by_decl = add_new_layer_names seen names in
  let introduced_by_rest, added_by_layer_decl =
    following_layer_introduction_order seen rest
  in
  added_by_layer_decl && list_has_prefix introduced_by_decl introduced_by_rest

let layer_decl_backward_redundant seen names =
  List.for_all (fun name -> List.exists (String.equal name) seen) names

(* Top-level CSS Cascade 6.4 cleanup. A layer statement is removable when it
   only repeats layer order already introduced in the current import-separated
   segment, or when the immediately following top-level layer blocks/statements
   introduce the same new names in the same order. Nested conditional layer
   declarations are deliberately left alone because their participation depends
   on the condition at evaluation time. *)
let drop_redundant_layer_decls stmts =
  let rec loop seen acc = function
    | [] -> List.rev acc
    | (Import _ as stmt) :: rest -> loop [] (stmt :: acc) rest
    | (Layer_decl names as stmt) :: rest ->
        if
          layer_decl_backward_redundant seen names
          || layer_decl_forward_redundant seen names rest
        then loop seen acc rest
        else
          let seen, _ = add_new_layer_names seen names in
          loop seen (stmt :: acc) rest
    | stmt :: rest ->
        let seen, _ =
          add_new_layer_names seen (layer_names_of_statement stmt)
        in
        loop seen (stmt :: acc) rest
  in
  loop [] [] stmts

(* Main statement processing function with layer optimization *)
(* CSS Cascade 6.1: a rule with no declarations and no nested rules
   contributes nothing to the cascade. Drop it under [~optimize:true]
   (Lightning CSS / cssnano convention). [@media] / [@supports] /
   [@container] / [@scope] / [@starting-style] blocks with an empty body
   are likewise no-ops and removed. Empty named [@layer] blocks survive
   as a [Layer_decl] (the layer name still contributes to the layer order
   per CSS Cascade L6 6.4). *)
let drop_empty_rules stmts =
  List.filter
    (function
      | Rule { declarations = []; nested = []; _ } -> false
      | Rule { selector; _ } when Selector.matches_nothing selector -> false
      | Media (_, []) -> false
      | Supports (_, []) -> false
      | Container (_, _, []) -> false
      | Scope (_, _, []) -> false
      | Starting_style [] -> false
      | Page (_, []) -> false
      | Page_with_margins (_, [], []) -> false
      | _ -> true)
    stmts

(* CSS Cascade 5 §6.6.3: a [@layer <name>;] declaration form is prelude-friendly
   and may interleave with [@charset] / [@import] / [@namespace], so a
   [Layer_decl] before [@import] / [@namespace] must not flip [seen_body] -
   otherwise the following [@import] gets dropped as misplaced. *)
let drop_misplaced_imports stmts =
  let seen_body = ref false in
  let seen_import_or_namespace = ref false in
  List.filter
    (fun stmt ->
      match stmt with
      | (Charset _ | Import _ | Namespace _) when not !seen_body ->
          (match stmt with
          | Import _ | Namespace _ -> seen_import_or_namespace := true
          | _ -> ());
          true
      | Import _ -> false
      | Charset _ | Namespace _ -> true
      | Layer_decl _ when not !seen_import_or_namespace -> true
      | _ ->
          seen_body := true;
          true)
    stmts

(* CSS Cascade 6.4: consecutive same-name [@layer] blocks merge only at the
   level the user wrote them at. Two [@layer foo] siblings inside the same
   [@layer { ... }] anonymous parent stay distinct because re-ordering them
   would change the layer-declaration shape - the fuzz boundary invariant
   catches this. The top-level entry point applies the layer merge once; nested
   invocations from [process_statements] skip it. *)
let rec statements (stmts : statement list) : statement list =
  process_statements [] stmts
  |> merge_consecutive_media |> merge_consecutive_supports
  |> merge_consecutive_containers |> merge_layer_declarations
  |> drop_misplaced_imports |> drop_empty_rules

and process_statements (acc : statement list) (remaining : statement list) :
    statement list =
  match remaining with
  | [] -> List.rev acc
  | Rule r :: rest ->
      (* Collect consecutive Rule items *)
      let rec collect_rules (rules_acc : rule list) :
          statement list -> rule list * statement list = function
        | Rule r :: rest -> collect_rules (r :: rules_acc) rest
        | rest -> (List.rev rules_acc, rest)
      in
      let plain_rules, rest = collect_rules [ r ] rest in
      let rec statement_has_scope = function
        | Scope _ -> true
        | Rule rule -> List.exists statement_has_scope rule.nested
        | Media (_, block)
        | Container (_, _, block)
        | Supports (_, block)
        | Layer (_, block)
        | Origin (_, block)
        | Starting_style block
        | When (_, block)
        | Else (_, block) ->
            List.exists statement_has_scope block
        | _ -> false
      in
      let rule_has_scope_nested rule =
        List.exists statement_has_scope rule.nested
      in
      let flattened_scope_rules =
        List.concat_map
          (fun rule ->
            if rule_has_scope_nested rule then !flatten_rule_ref rule
            else [ Rule rule ])
          plain_rules
      in
      if
        List.exists
          (function Rule _ -> false | _ -> true)
          flattened_scope_rules
      then
        let optimized = statements flattened_scope_rules in
        process_statements (List.rev_append optimized acc) rest
      else
        (* Optimize this batch of consecutive rules, including their nested
           statements *)
        let rules =
          List.map
            (function Rule rule -> rule | _ -> assert false)
            flattened_scope_rules
        in
        let optimized = rules_aux rules in
        let as_statements = List.map (fun r -> Rule r) optimized in
        process_statements (List.rev_append as_statements acc) rest
  | Media (cond, block) :: rest ->
      (* Just optimize the block and pass through - grouping happens later *)
      let optimized_block = statements block in
      let optimized = Media (cond, optimized_block) in
      process_statements (optimized :: acc) rest
  | Container (name, cond, block) :: rest ->
      (* Recursively optimize container query content *)
      let optimized = Container (name, cond, statements block) in
      process_statements (optimized :: acc) rest
  | Supports (cond, block) :: rest ->
      (* Recursively optimize supports block content *)
      let optimized = Supports (cond, statements block) in
      process_statements (optimized :: acc) rest
  | Scope (start, end_, block) :: rest ->
      let optimized = Scope (start, end_, statements block) in
      process_statements (optimized :: acc) rest
  | Origin (origin, block) :: rest ->
      let optimized = Origin (origin, statements block) in
      process_statements (optimized :: acc) rest
  | Layer (name, block) :: rest ->
      let optimized_block = statements block in
      if is_layer_empty optimized_block then
        (* Handle empty layer optimization *)
        match name with
        | Some layer_name ->
            let all_names, remaining =
              collect_empty_layer_names [ layer_name ] rest
            in
            let layer_decl = Layer_decl all_names in
            process_statements (layer_decl :: acc) remaining
        | None ->
            (* Anonymous empty layer - just remove it *)
            process_statements acc rest
      else
        let optimized = Layer (name, optimized_block) in
        process_statements (optimized :: acc) rest
  | hd :: rest ->
      (* Other statement types - keep as-is *)
      process_statements (hd :: acc) rest

and rules_aux (rules : rule list) : rule list =
  (* First optimize each rule's nested statements recursively *)
  let with_optimized_nested =
    List.map (fun rule -> { rule with nested = statements rule.nested }) rules
  in
  (* Apply standard rule optimizations. Adjacent same-selector rules merge:
     [.x{a}] [.x{b}] -> [.x{a;b}], which is safe because cascade order within
     the merged block matches the source order of the originals.
     [combine_identical_rules] then groups same-declaration rules under a
     selector list ([.a, .b, .c{...}]). *)
  List.map single_rule_without_nested with_optimized_nested
  |> merge_rules
  |> List.map finalize_rule_without_nested
  |> combine_identical_rules

(* CSS Syntax 3 sec. 2.2: [@charset] is an encoding-declaration byte pattern
   recognised before tokenization, not a stylesheet at-rule after parsing. The
   cascade parser has already consumed the encoding metadata and the serialiser
   emits UTF-8, so [@charset "UTF-8"] is purely redundant. Drop any UTF-8
   charset and keep at most the first non-UTF-8 one. *)
let normalize_charset stmts =
  let is_utf8 encoding =
    String.equal (String.lowercase_ascii encoding) "utf-8"
  in
  let kept_one = ref false in
  List.filter
    (fun stmt ->
      match stmt with
      | Charset enc when is_utf8 enc -> false
      | Charset _ when !kept_one -> false
      | Charset _ ->
          kept_one := true;
          true
      | _ -> true)
    stmts

let statements_top_level (stmts : statement list) : statement list =
  statements stmts |> normalize_charset |> merge_consecutive_layers
  |> drop_redundant_layer_decls

let single_rule (rule : rule) : rule =
  {
    rule with
    declarations = deduplicate_declarations rule.declarations;
    nested = statements rule.nested;
  }

let rules (rules : rule list) : rule list = rules_aux rules

(* Initialize the forward reference for merge_consecutive_media *)
let () = statements_ref := statements

(** {1 Nesting Flattening} *)

let contains_nesting sel =
  Selector.any (function Selector.Nesting -> true | _ -> false) sel

let substitute_nesting ~parent sel =
  Selector.map (function Selector.Nesting -> parent | s -> s) sel

let combine_with_parent (parent : Selector.t) (child : Selector.t) : Selector.t
    =
  if contains_nesting child then substitute_nesting ~parent child
  else Selector.Combined (parent, Selector.Descendant, child)

let scope_selector_in_context (parent : Selector.t) s =
  try
    let selector = Selector.of_string s in
    let selector =
      if contains_nesting selector then substitute_nesting ~parent selector
      else selector
    in
    Selector.to_string ~minify:true selector
  with Cursor.Parse_error _ | Invalid_argument _ -> s

let rec flatten_rule ?(parent : Selector.t option) (rule : rule) :
    statement list =
  let selector =
    match parent with
    | None -> rule.selector
    | Some p -> combine_with_parent p rule.selector
  in
  let direct =
    if rule.declarations = [] then []
    else
      [
        Rule
          {
            selector;
            declarations = rule.declarations;
            nested = [];
            merge_key = rule.merge_key;
          };
      ]
  in
  let nested_flat =
    List.concat_map (flatten_in_rule_context selector) rule.nested
  in
  direct @ nested_flat

and flatten_in_rule_context (parent : Selector.t) : statement -> statement list
    = function
  | Rule child -> flatten_rule ~parent child
  | Declarations decls ->
      [
        Rule
          {
            selector = parent;
            declarations = decls;
            nested = [];
            merge_key = None;
          };
      ]
  | Media (cond, block) ->
      [ Media (cond, List.concat_map (flatten_in_rule_context parent) block) ]
  | Container (name, cond, block) ->
      [
        Container
          (name, cond, List.concat_map (flatten_in_rule_context parent) block);
      ]
  | Supports (cond, block) ->
      [
        Supports (cond, List.concat_map (flatten_in_rule_context parent) block);
      ]
  | Layer (name, block) ->
      [ Layer (name, List.concat_map (flatten_in_rule_context parent) block) ]
  | Origin (origin, block) ->
      [
        Origin (origin, List.concat_map (flatten_in_rule_context parent) block);
      ]
  | Starting_style block ->
      [
        Starting_style (List.concat_map (flatten_in_rule_context parent) block);
      ]
  | When (cond, block) ->
      [ When (cond, List.concat_map (flatten_in_rule_context parent) block) ]
  | Else (cond, block) ->
      [ Else (cond, List.concat_map (flatten_in_rule_context parent) block) ]
  | Scope (s, e, block) ->
      [
        Scope
          ( Option.map (scope_selector_in_context parent) s,
            Option.map (scope_selector_in_context parent) e,
            List.concat_map (flatten_in_rule_context parent) block );
      ]
  | other -> [ other ]

let rec flatten_top_statement : statement -> statement list = function
  | Rule rule -> flatten_rule rule
  | Media (cond, block) -> [ Media (cond, flatten_block block) ]
  | Container (name, cond, block) ->
      [ Container (name, cond, flatten_block block) ]
  | Supports (cond, block) -> [ Supports (cond, flatten_block block) ]
  | Layer (name, block) -> [ Layer (name, flatten_block block) ]
  | Origin (origin, block) -> [ Origin (origin, flatten_block block) ]
  | Starting_style block -> [ Starting_style (flatten_block block) ]
  | When (cond, block) -> [ When (cond, flatten_block block) ]
  | Else (cond, block) -> [ Else (cond, flatten_block block) ]
  | Scope (s, e, block) -> [ Scope (s, e, flatten_block block) ]
  | other -> [ other ]

and flatten_block (block : statement list) : statement list =
  List.concat_map flatten_top_statement block

let flatten_nesting (stylesheet : t) : t = flatten_block stylesheet
let () = flatten_rule_ref := flatten_rule

(** {1 Stylesheet Optimization} *)

let apply_property_duplication (stylesheet : t) : t =
  (* Apply only property duplication without other optimizations *)
  let rec apply_to_statements stmts =
    List.map
      (function
        | Rule rule ->
            Rule
              {
                rule with
                declarations = duplicate_buggy_properties rule.declarations;
              }
        | Media (cond, inner_stmts) ->
            Media (cond, apply_to_statements inner_stmts)
        | Layer (name, inner_stmts) ->
            Layer (name, apply_to_statements inner_stmts)
        | Container (name, cond, inner_stmts) ->
            Container (name, cond, apply_to_statements inner_stmts)
        | Supports (cond, inner_stmts) ->
            Supports (cond, apply_to_statements inner_stmts)
        | Origin (origin, inner_stmts) ->
            Origin (origin, apply_to_statements inner_stmts)
        | other -> other)
      stmts
  in
  apply_to_statements stylesheet |> normalize_shadow_color_var_slots

(** [drop_invalid] walks every declaration list in the stylesheet (rules, bare
    nesting blocks, [@page] / [@font-palette-values] / [@view-transition] /
    [@position-try]) and removes declarations whose typed value contains an
    [Invalid] arm. *)
let drop_invalid (stylesheet : t) : t =
  let filter_decls = List.filter (fun d -> not (Declaration.is_invalid d)) in
  let rec statement = function
    | Rule rule ->
        Rule
          {
            rule with
            declarations = filter_decls rule.declarations;
            nested = List.map statement rule.nested;
          }
    | Declarations decls -> Declarations (filter_decls decls)
    | Layer (name, block) -> Layer (name, List.map statement block)
    | Media (m, block) -> Media (m, List.map statement block)
    | Container (n, c, block) -> Container (n, c, List.map statement block)
    | Supports (s, block) -> Supports (s, List.map statement block)
    | Moz_document (c, block) -> Moz_document (c, List.map statement block)
    | When (c, block) -> When (c, List.map statement block)
    | Else (c, block) -> Else (c, List.map statement block)
    | Starting_style block -> Starting_style (List.map statement block)
    | Origin (o, block) -> Origin (o, List.map statement block)
    | Scope (a, b, block) -> Scope (a, b, List.map statement block)
    | Page (sel, decls) -> Page (sel, filter_decls decls)
    | Page_with_margins (sel, descs, margins) ->
        Page_with_margins
          ( sel,
            filter_decls descs,
            List.map
              (fun (m : page_margin_rule) ->
                {
                  m with
                  margin_descriptors = filter_decls m.margin_descriptors;
                })
              margins )
    | Position_try (name, decls) -> Position_try (name, filter_decls decls)
    | Supports_condition (name, decls) ->
        Supports_condition (name, filter_decls decls)
    | other -> other
  in
  List.map statement stylesheet

(** [drop_unknown_at_rules] removes [Unknown_at_rule] statements at every block
    depth. Used in [--minify] alongside [drop_invalid] so the typed warnings
    emitted at parse time materialise as a dropped rule, matching CSS Syntax 3
    §5.4.1 (unknown at-rules are discarded). *)
let drop_unknown_at_rules (stylesheet : t) : t =
  let rec statement = function
    | Unknown_at_rule _ -> None
    | Rule rule ->
        Some (Rule { rule with nested = List.filter_map statement rule.nested })
    | Layer (name, block) ->
        Some (Layer (name, List.filter_map statement block))
    | Media (m, block) -> Some (Media (m, List.filter_map statement block))
    | Container (n, c, block) ->
        Some (Container (n, c, List.filter_map statement block))
    | Supports (s, block) ->
        Some (Supports (s, List.filter_map statement block))
    | Moz_document (c, block) ->
        Some (Moz_document (c, List.filter_map statement block))
    | When (c, block) -> Some (When (c, List.filter_map statement block))
    | Else (c, block) -> Some (Else (c, List.filter_map statement block))
    | Starting_style block ->
        Some (Starting_style (List.filter_map statement block))
    | Origin (o, block) -> Some (Origin (o, List.filter_map statement block))
    | Scope (a, b, block) ->
        Some (Scope (a, b, List.filter_map statement block))
    | other -> Some other
  in
  List.filter_map statement stylesheet

let stylesheet ?(flatten_nesting = false) (stylesheet : t) : t =
  let stylesheet =
    if flatten_nesting then flatten_block stylesheet else stylesheet
  in
  (* [drop_invalid] and [drop_unknown_at_rules] run before the main optimisation
     passes so the empty rules they leave behind get picked up by
     [drop_empty_rules]. *)
  stylesheet |> drop_invalid |> drop_unknown_at_rules
  |> normalize_shadow_color_var_slots |> statements_top_level
