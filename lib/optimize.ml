(** CSS optimization implementation *)

open Declaration
open Stylesheet
module String_set = Set.Make (String)

(** {1 Edge Model} *)

type scope = [ `Fragment | `Stylesheet ]

(* Threaded by the entry points ([stylesheet], [rules], [single_rule],
   [deduplicate_declarations]); shorthand composers read it for the
   fragment-vs-stylesheet scope decision. Default fragment. *)
let current_scope : scope ref = ref `Fragment

let with_scope (w : scope) f =
  let saved = !current_scope in
  current_scope := w;
  Fun.protect ~finally:(fun () -> current_scope := saved) f

type packed_property = Packed : 'a Properties.property -> packed_property

type edge = {
  summary : Selector_summary.t;
  property : packed_property;
  important : bool;
}

let selectors_of_rule_selector (sel : Selector.t) =
  match Selector.as_list sel with Some xs -> xs | None -> [ sel ]

let edges_of_decl summary = function
  | Declaration { property; _ } as d ->
      Some
        {
          summary;
          property = Packed property;
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

let duplicate_buggy_properties decls = decls

(* Properties whose typed cascade keeps duplicates verbatim: [content] and
   [outline] use the duplicate sequence for fallback patterns, and the
   vendor-prefixed [-webkit-mask-*] longhands keep parallel prefixed/unprefixed
   spellings for old-Safari compatibility. *)
let is_intentionally_duplicated_typed : type a. a Properties.property -> bool =
  function
  | Content -> true
  | Outline -> true
  | Webkit_mask_image -> true
  | Webkit_mask_composite -> true
  | Webkit_mask_source_type -> true
  | Webkit_mask_size -> true
  | Webkit_mask_position -> true
  | Webkit_mask_repeat -> true
  | Webkit_mask_clip -> true
  | Webkit_mask_origin -> true
  | _ -> false

let is_intentionally_duplicated decl =
  match decl with
  | Theme_guarded { decl; _ } -> (
      match decl with
      | Declaration { property; _ } ->
          is_intentionally_duplicated_typed property
      | _ -> false)
  | Declaration { property; _ } -> is_intentionally_duplicated_typed property

(* Typed shorthand -> longhand coverage relation. Each match arm spells out a
   reachable [(shorthand, longhand)] pair, including transitive cases ([border]
   covers [border-top-width] both directly and through [border-width] /
   [border-top]). Properties not listed have no shorthand relation; they
   self-cover by exact identity, never reset others. Custom and unknown
   properties are handled by [declaration_covers] separately. *)
let shorthand_covers_longhand : type a b.
    a Properties.property -> b Properties.property -> bool =
 fun sh lh ->
  match (sh, lh) with
  | Margin, Margin_top -> true
  | Margin, Margin_right -> true
  | Margin, Margin_bottom -> true
  | Margin, Margin_left -> true
  | Padding, Padding_top -> true
  | Padding, Padding_right -> true
  | Padding, Padding_bottom -> true
  | Padding, Padding_left -> true
  | Inset, Top -> true
  | Inset, Right -> true
  | Inset, Bottom -> true
  | Inset, Left -> true
  | Background, Background_attachment -> true
  | Background, Background_blend_mode -> true
  | Background, Background_clip -> true
  | Background, Background_color -> true
  | Background, Background_image -> true
  | Background, Background_origin -> true
  | Background, Background_position -> true
  | Background, Background_repeat -> true
  | Background, Background_size -> true
  (* CSS Logical 1: physical-axis pairs. *)
  | Margin_inline, Margin_inline_start -> true
  | Margin_inline, Margin_inline_end -> true
  | Margin_block, Margin_block_start -> true
  | Margin_block, Margin_block_end -> true
  | Padding_inline, Padding_inline_start -> true
  | Padding_inline, Padding_inline_end -> true
  | Padding_block, Padding_block_start -> true
  | Padding_block, Padding_block_end -> true
  | Inset_inline, Inset_inline_start -> true
  | Inset_inline, Inset_inline_end -> true
  | Inset_block, Inset_block_start -> true
  | Inset_block, Inset_block_end -> true
  (* CSS Backgrounds 3 sec. 4: [border] resets every per-side longhand and the
     per-axis [width / style / color] groupings; the transitive closure is
     listed explicitly so the match is one-shot. *)
  | Border, Border_width -> true
  | Border, Border_style -> true
  | Border, Border_color -> true
  | Border, Border_top -> true
  | Border, Border_right -> true
  | Border, Border_bottom -> true
  | Border, Border_left -> true
  | Border, Border_top_width -> true
  | Border, Border_right_width -> true
  | Border, Border_bottom_width -> true
  | Border, Border_left_width -> true
  | Border, Border_top_style -> true
  | Border, Border_right_style -> true
  | Border, Border_bottom_style -> true
  | Border, Border_left_style -> true
  | Border, Border_top_color -> true
  | Border, Border_right_color -> true
  | Border, Border_bottom_color -> true
  | Border, Border_left_color -> true
  | Border_width, Border_top_width -> true
  | Border_width, Border_right_width -> true
  | Border_width, Border_bottom_width -> true
  | Border_width, Border_left_width -> true
  | Border_style, Border_top_style -> true
  | Border_style, Border_right_style -> true
  | Border_style, Border_bottom_style -> true
  | Border_style, Border_left_style -> true
  | Border_color, Border_top_color -> true
  | Border_color, Border_right_color -> true
  | Border_color, Border_bottom_color -> true
  | Border_color, Border_left_color -> true
  | Border_top, Border_top_width -> true
  | Border_top, Border_top_style -> true
  | Border_top, Border_top_color -> true
  | Border_right, Border_right_width -> true
  | Border_right, Border_right_style -> true
  | Border_right, Border_right_color -> true
  | Border_bottom, Border_bottom_width -> true
  | Border_bottom, Border_bottom_style -> true
  | Border_bottom, Border_bottom_color -> true
  | Border_left, Border_left_width -> true
  | Border_left, Border_left_style -> true
  | Border_left, Border_left_color -> true
  | Border, Border_image -> true
  (* CSS Masking 1 sec. 6.1: [mask] resets every mask layer longhand and
     [mask-border]. *)
  | Mask, Mask_image -> true
  | Mask, Mask_repeat -> true
  | Mask, Mask_size -> true
  | Mask, Mask_position -> true
  | Mask, Mask_origin -> true
  | Mask, Mask_clip -> true
  | Mask, Mask_mode -> true
  | Mask, Mask_composite -> true
  | Mask, Mask_border -> true
  (* CSS Fonts 4 sec. 2.7: [font] resets [font-style / -weight / -stretch /
     -size / line-height / -family]. Cascade doesn't model [font-variant-css21]
     separately; other [font-variant-*] longhands ([numeric], [ligatures], ...)
     survive the shorthand. *)
  | Font, Font_style -> true
  | Font, Font_weight -> true
  | Font, Font_stretch -> true
  | Font, Font_size -> true
  | Font, Line_height -> true
  | Font, Font_family -> true
  | _ -> false

(* CSS Cascade 5 sec. 7.2: [all] resets every property except [direction],
   [unicode-bidi], and custom properties. [Unknown_property _] is reset by [all]
   (an unrecognised non-custom property is still a CSS property). *)
let is_excluded_from_all_reset : type a. a Properties.property -> bool =
  function
  | Direction -> true
  | Unicode_bidi -> true
  | Custom_property _ -> true
  | _ -> false

let rec unwrap_theme_guard = function
  | Theme_guarded { decl; _ } -> unwrap_theme_guard decl
  | d -> d

(* CSS Cascade 5 sec. 7.2: [direction] and [unicode-bidi] keep their relative
   position after an [all] declaration; partition uses this to anchor them. *)
let is_all_preserved_reorder : type a. a Properties.property -> bool = function
  | Direction -> true
  | Unicode_bidi -> true
  | _ -> false

let all_preserved_reorder_declaration decl =
  match unwrap_theme_guard decl with
  | Declaration { property; _ } -> is_all_preserved_reorder property
  | _ -> false

(* Coverage relation between two declarations. Custom and unknown properties
   have only generic behavior: cover themselves by exact name, no shorthand
   coverage, and (for custom) exempt from the [all] reset. *)
let declaration_covers covering covered =
  match (unwrap_theme_guard covering, unwrap_theme_guard covered) with
  | Declaration { property = All; _ }, Declaration { property = covered_p; _ }
    ->
      not (is_excluded_from_all_reset covered_p)
  | ( Declaration { property = Custom_property a; _ },
      Declaration { property = Custom_property b; _ } ) ->
      String.equal a b
  | Declaration { property = Custom_property _; _ }, _ -> false
  | _, Declaration { property = Custom_property _; _ } -> false
  | ( Declaration { property = Unknown_property a; _ },
      Declaration { property = Unknown_property b; _ } ) ->
      String.equal a b
  | Declaration { property = Unknown_property _; _ }, _ -> false
  | _, Declaration { property = Unknown_property _; _ } -> false
  | ( Declaration { property = covering_p; _ },
      Declaration { property = covered_p; _ } ) ->
      String.equal (property_name covering) (property_name covered)
      || shorthand_covers_longhand covering_p covered_p
  | _ -> false

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

(* [source] carries the deduplicated declarations with their original indices;
   the prior-longhand check fires only on a longhand that survived dedup as a
   real cascade fallback (legacy color, vendor prefix, runtime substitution
   shape), not on one that was already eliminated as shadowed by the current
   shorthand. *)
let box_shorthand_had_prior_longhand source idx shorthand =
  match unwrap_theme_guard shorthand with
  | Theme_guarded _ -> false
  | Declaration
      { property = shorthand_prop; important = shorthand_important; _ } ->
      List.exists
        (fun (i, d) ->
          i < idx
          && (shorthand_important || not (is_important d))
          &&
          match unwrap_theme_guard d with
          | Declaration { property = lh_prop; _ } ->
              shorthand_covers_longhand shorthand_prop lh_prop
          | _ -> false)
        source

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
   <size>[/<line-height>]? <family>+]. Cascade stores [font] as a string, so
   composition renders each longhand via its pretty-printer and stitches the
   shorthand together. Default-valued components ([normal] style, [400] weight,
   [normal] line-height) drop on emit. Requires both font-size and
   font-family. *)
let is_font_longhand : declaration -> bool = function
  | Declaration { property = Font_style; _ } -> true
  | Declaration { property = Font_weight; _ } -> true
  | Declaration { property = Font_size; _ } -> true
  | Declaration { property = Line_height; _ } -> true
  | Declaration { property = Font_family; _ } -> true
  | _ -> false

(* Each helper returns [Some <typed value>] when the declaration is the relevant
   longhand; [None] otherwise. The pretty-printer drops default components on
   emit, so the composer doesn't normalise here. *)
let font_style_of : declaration -> Properties.font_style option = function
  | Declaration { property = Font_style; value; _ } -> Some value
  | _ -> Option.None

let font_weight_of : declaration -> Properties.font_weight option = function
  | Declaration { property = Font_weight; value; _ } -> Some value
  | _ -> Option.None

let line_height_of : declaration -> Properties.line_height option = function
  | Declaration { property = Line_height; value; _ } -> Some value
  | _ -> Option.None

let font_size_of : declaration -> Properties.font_size option = function
  | Declaration { property = Font_size; value; _ } -> Some value
  | _ -> Option.None

let font_family_of : declaration -> Properties.font_family option = function
  | Declaration { property = Font_family; value; _ } -> Some value
  | _ -> Option.None

let take_first_n n xs =
  let rec aux acc n = function
    | rest when n = 0 -> Some (List.rev acc, rest)
    | [] -> None
    | x :: rest -> aux (x :: acc) (n - 1) rest
  in
  aux [] n xs

let same_importance = function
  | [] -> true
  | first :: rest ->
      let important = is_important first in
      List.for_all (fun d -> is_important d = important) rest

let render_font_shorthand decls : Properties.font option =
  let pick f = List.find_map f decls in
  match (pick font_size_of, pick font_family_of) with
  | Some size, Some family ->
      Some
        (Shorthand
           {
             style = pick font_style_of;
             variant = Option.None;
             weight = pick font_weight_of;
             stretch = Option.None;
             size;
             line_height = pick line_height_of;
             family;
           })
  | _ -> Option.None

let try_compose_font indexed_decls =
  match take_first_n 5 indexed_decls with
  | None -> None
  | Some (five, rest) -> (
      let raw_decls = List.map snd five in
      if
        (not (List.for_all is_font_longhand raw_decls))
        || not (same_importance raw_decls)
      then None
      else
        match render_font_shorthand raw_decls with
        | Some font_value ->
            let idx = fst (List.hd five) in
            let merged =
              Declaration
                {
                  property = Font;
                  value = font_value;
                  important = is_important (List.hd raw_decls);
                }
            in
            Some ((idx, merged), rest)
        | None -> None)

let compose_font_shorthand decls =
  let rec go acc decls =
    match (decls, try_compose_font decls) with
    | [], _ -> List.rev acc
    | _, Some (merged, rest) -> go (merged :: acc) rest
    | hd :: rest, None -> go (hd :: acc) rest
  in
  go [] decls

(* CSS Lists 3 sec. 1.2: [list-style: <position> <image> <type>] in any order,
   any subset of components. Cascade stores [List_style] as a string. Drop
   defaults ([outside] / [none] / [disc]) on emit; if all three are defaulted,
   leave a single [outside] - never an empty value. *)
let is_list_style_longhand : declaration -> bool = function
  | Declaration { property = List_style_type; _ } -> true
  | Declaration { property = List_style_position; _ } -> true
  | Declaration { property = List_style_image; _ } -> true
  | _ -> false

(* Extract each list-style longhand's typed value, [None] when the declaration
   isn't the relevant longhand; the pretty-printer drops default components on
   emit. *)
let list_style_type_of : declaration -> Properties.list_style_type option =
  function
  | Declaration { property = List_style_type; value; _ } -> Some value
  | _ -> Option.None

let list_style_position_of :
    declaration -> Properties.list_style_position option = function
  | Declaration { property = List_style_position; value; _ } -> Some value
  | _ -> Option.None

let list_style_image_of : declaration -> Properties.list_style_image option =
  function
  | Declaration { property = List_style_image; value; _ } -> Some value
  | _ -> Option.None

let render_list_style decls : Properties.list_style =
  let pick f = List.find_map f decls in
  Shorthand
    {
      type_ = pick list_style_type_of;
      position = pick list_style_position_of;
      image = pick list_style_image_of;
    }

let try_compose_list_style = function
  | (idx, d1) :: (_, d2) :: (_, d3) :: rest
    when is_list_style_longhand d1 && is_list_style_longhand d2
         && is_list_style_longhand d3
         && is_important d1 = is_important d2
         && is_important d2 = is_important d3 ->
      let merged =
        Declaration
          {
            property = List_style;
            value = render_list_style [ d1; d2; d3 ];
            important = is_important d1;
          }
      in
      Some ((idx, merged), rest)
  | _ -> None

let compose_list_style_shorthand decls =
  let rec go acc decls =
    match (decls, try_compose_list_style decls) with
    | [], _ -> List.rev acc
    | _, Some (merged, rest) -> go (merged :: acc) rest
    | hd :: rest, None -> go (hd :: acc) rest
  in
  go [] decls

(* CSS Flexbox 1 sec. 7.2: [flex] shorthand is grow / shrink / basis. Cascade
   types [Flex] as [Full of grow * shrink * basis]; the composition extracts the
   three typed longhands and builds the constructor. *)
type flex_kind = FGrow | FShrink | FBasis

let flex_kind_of : declaration -> flex_kind option = function
  | Declaration { property = Flex_grow; _ } -> Some FGrow
  | Declaration { property = Flex_shrink; _ } -> Some FShrink
  | Declaration { property = Flex_basis; _ } -> Some FBasis
  | _ -> None

let flex_grow_of : declaration -> float option = function
  | Declaration { property = Flex_grow; value = Number f; _ } -> Some f
  | _ -> None

let flex_shrink_of : declaration -> float option = function
  | Declaration { property = Flex_shrink; value = Number f; _ } -> Some f
  | _ -> None

let flex_basis_of : declaration -> Properties.flex_basis option = function
  | Declaration { property = Flex_basis; value; _ } -> Some value
  | _ -> None

let try_compose_flex = function
  | (idx, d1) :: (_, d2) :: (_, d3) :: rest -> (
      match (flex_kind_of d1, flex_kind_of d2, flex_kind_of d3) with
      | Some k1, Some k2, Some k3
        when is_important d1 = is_important d2
             && is_important d2 = is_important d3
             && List.length (List.sort_uniq compare [ k1; k2; k3 ]) = 3 -> (
          let triple = [ d1; d2; d3 ] in
          let grow = List.find_map flex_grow_of triple in
          let shrink = List.find_map flex_shrink_of triple in
          let basis = List.find_map flex_basis_of triple in
          match (grow, shrink, basis) with
          | Some g, Some s, Some b ->
              let merged =
                Declaration
                  {
                    property = Flex;
                    value = Full (g, s, b);
                    important = is_important d1;
                  }
              in
              Some ((idx, merged), rest)
          | _ -> None)
      | _ -> None)
  | _ -> None

let compose_flex_shorthand decls =
  let rec go acc decls =
    match (decls, try_compose_flex decls) with
    | [], _ -> List.rev acc
    | _, Some (merged, rest) -> go (merged :: acc) rest
    | hd :: rest, None -> go (hd :: acc) rest
  in
  go [] decls

(* CSS Text Decoration 4 sec. 2: [text-decoration] shorthand carries line list,
   style, color, and optional thickness. The composition extracts the three
   required typed longhands; the pretty-printer drops default-valued style and
   color when emitting the shorthand. *)
type td_kind = Line | Style | Color

let td_kind_of : declaration -> td_kind option = function
  | Declaration { property = Text_decoration_line; _ } -> Some Line
  | Declaration { property = Text_decoration_style; _ } -> Some Style
  | Declaration { property = Text_decoration_color; _ } -> Some Color
  | _ -> None

let td_line_of : declaration -> Properties.text_decoration_line list option =
  function
  | Declaration { property = Text_decoration_line; value; _ } -> Some value
  | _ -> None

let td_style_of : declaration -> Properties.text_decoration_style option =
  function
  | Declaration { property = Text_decoration_style; value; _ } -> Some value
  | _ -> None

let td_color_of : declaration -> Values.color option = function
  | Declaration { property = Text_decoration_color; value; _ } -> Some value
  | _ -> None

let try_compose_text_decoration = function
  | (idx, d1) :: (_, d2) :: (_, d3) :: rest -> (
      match (td_kind_of d1, td_kind_of d2, td_kind_of d3) with
      | Some k1, Some k2, Some k3
        when is_important d1 = is_important d2
             && is_important d2 = is_important d3
             && List.length (List.sort_uniq compare [ k1; k2; k3 ]) = 3 -> (
          let triple = [ d1; d2; d3 ] in
          let lines = List.find_map td_line_of triple in
          let style = List.find_map td_style_of triple in
          let color = List.find_map td_color_of triple in
          match (lines, style, color) with
          | Some lines, Some _, Some _ ->
              let merged =
                Declaration
                  {
                    property = Text_decoration;
                    value = Shorthand { lines; style; color; thickness = None };
                    important = is_important d1;
                  }
              in
              Some ((idx, merged), rest)
          | _ -> None)
      | _ -> None)
  | _ -> None

let compose_text_decoration_shorthand decls =
  let rec go acc decls =
    match (decls, try_compose_text_decoration decls) with
    | [], _ -> List.rev acc
    | _, Some (merged, rest) -> go (merged :: acc) rest
    | hd :: rest, None -> go (hd :: acc) rest
  in
  go [] decls

(* CSS Backgrounds 3 sec. 4.1: [border] is the shorthand for [border-{top,
   right,bottom,left}-{width,style,color}]. Cascade composes when all 12
   longhands appear in a contiguous run with matching importance, every width /
   style / color is uniform across the four sides, and no runtime-substitution
   value would change the resolved shape. *)
let border_width_of :
    declaration -> (box_side * Properties.border_width * bool) option = function
  | Declaration { property = Border_top_width; value; important } ->
      Some (Top, value, important)
  | Declaration { property = Border_right_width; value; important } ->
      Some (Right, value, important)
  | Declaration { property = Border_bottom_width; value; important } ->
      Some (Bottom, value, important)
  | Declaration { property = Border_left_width; value; important } ->
      Some (Left, value, important)
  | _ -> None

let border_style_of :
    declaration -> (box_side * Properties.border_style * bool) option = function
  | Declaration { property = Border_top_style; value; important } ->
      Some (Top, value, important)
  | Declaration { property = Border_right_style; value; important } ->
      Some (Right, value, important)
  | Declaration { property = Border_bottom_style; value; important } ->
      Some (Bottom, value, important)
  | Declaration { property = Border_left_style; value; important } ->
      Some (Left, value, important)
  | _ -> None

let border_color_of : declaration -> (box_side * Values.color * bool) option =
  function
  | Declaration { property = Border_top_color; value; important } ->
      Some (Top, value, important)
  | Declaration { property = Border_right_color; value; important } ->
      Some (Right, value, important)
  | Declaration { property = Border_bottom_color; value; important } ->
      Some (Bottom, value, important)
  | Declaration { property = Border_left_color; value; important } ->
      Some (Left, value, important)
  | _ -> None

let all_box_sides_present xs =
  List.length xs = 4
  &&
  let sides = List.map (fun (s, _, _) -> s) xs in
  List.sort_uniq compare sides = [ Top; Right; Bottom; Left ]

let uniform_side_value = function
  | [] -> false
  | (_, value, _) :: rest -> List.for_all (fun (_, v, _) -> v = value) rest

let border_parts_of raw_decls =
  let widths = List.filter_map border_width_of raw_decls in
  let styles = List.filter_map border_style_of raw_decls in
  let colors = List.filter_map border_color_of raw_decls in
  if
    all_box_sides_present widths
    && all_box_sides_present styles
    && all_box_sides_present colors
    && uniform_side_value widths && uniform_side_value styles
    && uniform_side_value colors
  then Some (widths, styles, colors)
  else None

let declaration_of_border_parts ~important widths styles colors =
  let _, width, _ = List.hd widths in
  let _, style, _ = List.hd styles in
  let _, color, _ = List.hd colors in
  Declaration
    {
      property = Border;
      value =
        Shorthand { width = Some width; style = Some style; color = Some color };
      important;
    }

let try_compose_border indexed_decls =
  match take_first_n 12 indexed_decls with
  | None -> None
  | Some twelve -> (
      let twelve, rest = twelve in
      let raw_decls = List.map snd twelve in
      if not (same_importance raw_decls) then None
      else
        match border_parts_of raw_decls with
        | None -> None
        | Some (widths, styles, colors) ->
            let merged =
              declaration_of_border_parts
                ~important:(is_important (List.hd raw_decls))
                widths styles colors
            in
            let idx = fst (List.hd twelve) in
            Some ((idx, merged), rest))

let compose_border_shorthand decls =
  let rec go acc decls =
    match (decls, try_compose_border decls) with
    | [], _ -> List.rev acc
    | _, Some (merged, rest) -> go (merged :: acc) rest
    | hd :: rest, None -> go (hd :: acc) rest
  in
  go [] decls

(* Compose the [border] shorthand from the three whole-border longhands
   [border-width] / [border-style] / [border-color] when they appear as a
   contiguous run (any order, single-valued, same importance). The [border]
   shorthand also resets [border-image] to its initial, so only compose when no
   [border-image] declaration is present in the rule - otherwise the synthesised
   [border] would clobber it (the reset/reorder case is handled separately). *)
let try_compose_border_whole indexed_decls =
  match take_first_n 3 indexed_decls with
  | None -> None
  | Some (three, rest) -> (
      let raw = List.map snd three in
      if not (same_importance raw) then None
      else
        let width : Properties.border_width option ref = ref None in
        let style : Properties.border_style option ref = ref None in
        let color : Values.color option ref = ref None in
        List.iter
          (function
            | Declaration { property = Border_width; value = [ w ]; _ } ->
                width := Some w
            | Declaration { property = Border_style; value = s; _ } ->
                style := Some s
            | Declaration { property = Border_color; value = [ c ]; _ } ->
                color := Some c
            | _ -> ())
          raw;
        match (!width, !style, !color) with
        | Some width, Some style, Some color ->
            let merged =
              Declaration
                {
                  property = Border;
                  value =
                    Shorthand
                      {
                        width = Some width;
                        style = Some style;
                        color = Some color;
                      };
                  important = is_important (List.hd raw);
                }
            in
            Some ((fst (List.hd three), merged), rest)
        | _ -> None)

(* [border-image*] and [border-width/style/color] are independent properties, so
   a border-image declaration (or longhand run) that immediately precedes the
   whole-border longhands can move after them without changing any property's
   cascade. Doing so lets [compose_border_whole_shorthand] synthesise [border]
   in place: the synthesised [border] resets border-image, but the now-trailing
   border-image declaration overrides that reset back. Only swap when the
   following run carries the full width/style/color trio, so the move happens
   exactly where it enables composition. *)
let reorder_border_image_before_border decls =
  let prop d = property_name (snd d) in
  let is_border_image d = String.starts_with ~prefix:"border-image" (prop d) in
  let is_border_longhand d =
    match prop d with
    | "border-width" | "border-style" | "border-color" -> true
    | _ -> false
  in
  let rec span pred acc = function
    | d :: rest when pred d -> span pred (d :: acc) rest
    | rest -> (List.rev acc, rest)
  in
  let rec go acc = function
    | [] -> List.rev acc
    | d :: _ as l when is_border_image d ->
        let img_block, rest1 = span is_border_image [] l in
        let long_block, rest2 = span is_border_longhand [] rest1 in
        let has p = List.exists (fun d -> String.equal (prop d) p) long_block in
        if has "border-width" && has "border-style" && has "border-color" then
          go (List.rev_append (long_block @ img_block) acc) rest2
        else go (List.rev_append img_block acc) rest1
    | d :: rest -> go (d :: acc) rest
  in
  go [] decls

let compose_border_whole_shorthand decls =
  (* [border] resets [border-image] to its initial, so the synthesised shorthand
     is only safe when it ends up before every [border-image] declaration.
     Compose in place while no [border-image] has been seen earlier in the rule;
     once one is, leave the longhands alone (the reorder-before-border-image
     case is handled separately). *)
  let rec go ~seen_border_image acc decls =
    match try_compose_border_whole decls with
    | Some (merged, rest) when not seen_border_image ->
        go ~seen_border_image (merged :: acc) rest
    | _ -> (
        match decls with
        | [] -> List.rev acc
        | ((_, d) as hd) :: rest ->
            let seen_border_image =
              seen_border_image
              || String.starts_with ~prefix:"border-image" (property_name d)
            in
            go ~seen_border_image (hd :: acc) rest)
  in
  go ~seen_border_image:false [] decls

(* CSS Backgrounds 3: the [border] shorthand resets [border-image] to its
   initial. A [border-image*] declaration is therefore dead when a later
   [border] shorthand of at least equal importance resets it and no later
   [border-image*] re-establishes it. Intra-block source order is fixed
   regardless of any surrounding CSS, so the drop is safe in every scope. *)
let drop_border_image_shadowed_by_border kept =
  let is_bimg (_, d) =
    String.starts_with ~prefix:"border-image" (property_name d)
  in
  let is_border (_, d) =
    match d with Declaration { property = Border; _ } -> true | _ -> false
  in
  let arr = Array.of_list kept in
  let n = Array.length arr in
  let rec any_bimg_from k =
    k < n && (is_bimg arr.(k) || any_bimg_from (k + 1))
  in
  let dead i =
    let img_important = is_important (snd arr.(i)) in
    let rec scan j =
      if j >= n then false
      else if
        is_border arr.(j) && (is_important (snd arr.(j)) || not img_important)
      then if any_bimg_from (j + 1) then scan (j + 1) else true
      else scan (j + 1)
    in
    scan (i + 1)
  in
  List.filteri (fun i item -> not (is_bimg item && dead i)) kept

(* CSS Backgrounds 3 sec. 3.10: [background] is the shorthand for the eight
   per-layer longhands. Cascade composes when a contiguous run of bg-* longhands
   covers a single layer: every longhand carries a single-layer value, no entry
   uses a CSS-wide keyword or [var()], and all share the same importance. *)
let background_image_singleton :
    Properties.background_image list -> Properties.background_image option =
  function
  | [ img ] -> (
      match img with
      | Inherit | Initial | Unset | Revert | Revert_layer | Var _ | List _ ->
          None
      | _ -> Some img)
  | _ -> None

let background_position_singleton :
    Properties.background_position -> Properties.position_value option =
  function
  | [ pos ] -> Some pos
  | _ -> None

let bg_color_part : declaration -> Values.color option = function
  | Declaration { property = Background_color; value; _ } -> (
      match value with Inherit | Initial | Unset -> None | v -> Some v)
  | _ -> None

let bg_image_part : declaration -> Properties.background_image option = function
  | Declaration { property = Background_image; value; _ } ->
      background_image_singleton value
  | _ -> None

let bg_repeat_part : declaration -> Properties.background_repeat option =
  function
  | Declaration { property = Background_repeat; value; _ } -> (
      match value with Inherit | Initial | Unset -> None | v -> Some v)
  | _ -> None

let bg_position_part : declaration -> Properties.position_value option =
  function
  | Declaration { property = Background_position; value; _ } ->
      background_position_singleton value
  | _ -> None

let bg_size_part : declaration -> Properties.background_size option = function
  | Declaration { property = Background_size; value; _ } -> (
      match value with
      | Inherit | Initial | Unset | Revert | Revert_layer | Var _ -> None
      | v -> Some v)
  | _ -> None

let bg_attachment_part : declaration -> Properties.background_attachment option
    = function
  | Declaration { property = Background_attachment; value; _ } -> (
      match value with Inherit | Initial | Unset -> None | v -> Some v)
  | _ -> None

let bg_origin_part : declaration -> Properties.background_box option = function
  | Declaration { property = Background_origin; value; _ } -> (
      match value with Inherit | Initial | Unset -> None | v -> Some v)
  | _ -> None

let bg_clip_part : declaration -> Properties.background_box option = function
  | Declaration { property = Background_clip; value; _ } -> (
      match value with Inherit | Initial | Unset -> None | v -> Some v)
  | _ -> None

type bg_updater =
  declaration ->
  (Properties.background_shorthand -> Properties.background_shorthand) option

let lift_part :
    'a.
    (declaration -> 'a option) ->
    (Properties.background_shorthand -> 'a -> Properties.background_shorthand) ->
    bg_updater =
 fun extract set d ->
  match extract d with Some v -> Some (fun s -> set s v) | None -> None

let bg_updaters : bg_updater list =
  [
    lift_part bg_color_part (fun s v -> { s with color = Some v });
    lift_part bg_image_part (fun s v -> { s with image = Some v });
    lift_part bg_repeat_part (fun s v -> { s with repeat = Some v });
    lift_part bg_position_part (fun s v -> { s with position = Some v });
    lift_part bg_size_part (fun s v -> { s with size = Some v });
    lift_part bg_attachment_part (fun s v -> { s with attachment = Some v });
    lift_part bg_origin_part (fun s v -> { s with origin = Some v });
    lift_part bg_clip_part (fun s v -> { s with clip = Some v });
  ]

let background_part_of (d : declaration) =
  List.find_map (fun f -> f d) bg_updaters

let take_contiguous_background indexed_decls =
  let rec aux acc = function
    | (i, d) :: rest -> (
        match background_part_of d with
        | Some f -> aux ((d, f) :: acc) rest
        | None -> (List.rev acc, (i, d) :: rest))
    | [] -> (List.rev acc, [])
  in
  match indexed_decls with
  | [] -> (None, [])
  | (idx, _) :: _ -> (
      let parts, rest = aux [] indexed_decls in
      match parts with
      | [] -> (None, indexed_decls)
      | _ -> (Some (idx, parts), rest))

let empty_bg_shorthand : Properties.background_shorthand =
  {
    color = None;
    image = None;
    position = None;
    size = None;
    repeat = None;
    attachment = None;
    clip = None;
    origin = None;
  }

(* Default open-world policy: the synthesized [background] shorthand resets
   every absent longhand to its initial, which would shadow a prior cascade
   write the optimizer cannot see (earlier <link>, earlier <style>, bundler
   concatenation, layer outside the file). Cascade composes only when the local
   run is reset-closed -- every reset field has a declaration in the run, so the
   shorthand cannot disturb prior writes. *)
let bg_longhand_property_name :
    Properties.background_shorthand -> string -> bool =
 fun s name ->
  match name with
  | "color" -> s.color <> None
  | "image" -> s.image <> None
  | "position" -> s.position <> None
  | "size" -> s.size <> None
  | "repeat" -> s.repeat <> None
  | "attachment" -> s.attachment <> None
  | "origin" -> s.origin <> None
  | "clip" -> s.clip <> None
  | _ -> false

let background_run_is_reset_closed layer =
  List.for_all
    (bg_longhand_property_name layer)
    [
      "color";
      "image";
      "position";
      "size";
      "repeat";
      "attachment";
      "origin";
      "clip";
    ]

let try_compose_background indexed_decls =
  let idx_opt, rest = take_contiguous_background indexed_decls in
  match idx_opt with
  | None -> None
  | Some (idx, parts) ->
      let raw_decls = List.map fst parts in
      if List.length raw_decls < 2 then None
      else if not (same_importance raw_decls) then None
      else
        let layer =
          List.fold_left (fun acc (_, f) -> f acc) empty_bg_shorthand parts
        in
        (* [`Fragment] requires the run to cover every reset field; in
           [`Stylesheet] the caller asserts no prior author CSS exists that the
           shorthand could shadow. *)
        let permit =
          match !current_scope with
          | `Stylesheet -> true
          | `Fragment -> background_run_is_reset_closed layer
        in
        if not permit then None
        else
          let merged =
            Declaration
              {
                property = Background;
                value = [ (Shorthand layer : Properties.background) ];
                important = is_important (List.hd raw_decls);
              }
          in
          Some ((idx, merged), rest)

let compose_background_shorthand decls =
  let rec go acc decls =
    match (decls, try_compose_background decls) with
    | [], _ -> List.rev acc
    | _, Some (merged, rest) -> go (merged :: acc) rest
    | hd :: rest, None -> go (hd :: acc) rest
  in
  go [] decls

(* CSS Masking 1 sec. 6.1: [mask] is the layer shorthand for [mask-image] /
   [mask-position] / [mask-size] / [mask-repeat] / [mask-origin] / [mask-clip] /
   [mask-mode] / [mask-composite] (analogous to [background]). Compose a
   contiguous run that carries a [mask-image]; like [border], [mask] resets
   [mask-border] to its initial, so only compose while no [mask-border] precedes
   (the reorder / dead-drop cases are handled separately). Closed-world
   ([`Stylesheet]) only, since the shorthand resets the layer fields the run
   leaves unset. *)
let mask_image_part : declaration -> Properties.background_image option =
  function
  | Declaration { property = Mask_image; value; _ } -> (
      match value with Inherit | Initial | Unset -> None | v -> Some v)
  | _ -> None

let mask_repeat_part : declaration -> Properties.background_repeat option =
  function
  | Declaration { property = Mask_repeat; value; _ } -> (
      match value with Inherit | Initial | Unset -> None | v -> Some v)
  | _ -> None

let mask_size_part : declaration -> Properties.background_size option = function
  | Declaration { property = Mask_size; value; _ } -> (
      match value with
      | Inherit | Initial | Unset | Revert | Revert_layer | Var _ -> None
      | v -> Some v)
  | _ -> None

let mask_position_part : declaration -> Properties.position_value option =
  function
  | Declaration { property = Mask_position; value; _ } ->
      background_position_singleton value
  | _ -> None

let mask_origin_part : declaration -> Properties.mask_box option = function
  | Declaration { property = Mask_origin; value; _ } -> (
      match value with Inherit | Initial | Unset -> None | v -> Some v)
  | _ -> None

let mask_clip_part : declaration -> Properties.mask_box option = function
  | Declaration { property = Mask_clip; value; _ } -> (
      match value with Inherit | Initial | Unset -> None | v -> Some v)
  | _ -> None

let mask_mode_part : declaration -> Properties.mask_mode option = function
  | Declaration { property = Mask_mode; value; _ } -> (
      match value with Inherit | Initial | Unset -> None | v -> Some v)
  | _ -> None

let mask_composite_part : declaration -> Properties.mask_composite option =
  function
  | Declaration { property = Mask_composite; value; _ } -> (
      match value with Inherit | Initial | Unset -> None | v -> Some v)
  | _ -> None

type mask_updater =
  declaration -> (Properties.mask_layer -> Properties.mask_layer) option

let lift_mask :
    'a.
    (declaration -> 'a option) ->
    (Properties.mask_layer -> 'a -> Properties.mask_layer) ->
    mask_updater =
 fun extract set d ->
  match extract d with Some v -> Some (fun s -> set s v) | None -> None

let mask_updaters : mask_updater list =
  [
    lift_mask mask_image_part (fun s v -> { s with image = Some v });
    lift_mask mask_repeat_part (fun s v -> { s with repeat = Some v });
    lift_mask mask_size_part (fun s v -> { s with size = Some v });
    lift_mask mask_position_part (fun s v -> { s with position = Some v });
    lift_mask mask_origin_part (fun s v -> { s with origin = Some v });
    lift_mask mask_clip_part (fun s v -> { s with clip = Some v });
    lift_mask mask_mode_part (fun s v -> { s with mode = Some v });
    lift_mask mask_composite_part (fun s v -> { s with composite = Some v });
  ]

let mask_part_of (d : declaration) = List.find_map (fun f -> f d) mask_updaters

let empty_mask_layer : Properties.mask_layer =
  {
    image = None;
    position = None;
    size = None;
    repeat = None;
    origin = None;
    clip = None;
    mode = None;
    composite = None;
  }

let take_contiguous_mask indexed_decls =
  let rec aux acc = function
    | (i, d) :: rest -> (
        match mask_part_of d with
        | Some f -> aux ((d, f) :: acc) rest
        | None -> (List.rev acc, (i, d) :: rest))
    | [] -> (List.rev acc, [])
  in
  match indexed_decls with
  | [] -> (None, [])
  | (idx, _) :: _ -> (
      let parts, rest = aux [] indexed_decls in
      match parts with
      | [] -> (None, indexed_decls)
      | _ -> (Some (idx, parts), rest))

let try_compose_mask indexed_decls =
  let idx_opt, rest = take_contiguous_mask indexed_decls in
  match idx_opt with
  | None -> None
  | Some (idx, parts) ->
      let raw_decls = List.map fst parts in
      if List.length raw_decls < 2 then None
      else if not (same_importance raw_decls) then None
      else if !current_scope <> `Stylesheet then None
      else
        let layer =
          List.fold_left (fun acc (_, f) -> f acc) empty_mask_layer parts
        in
        (* The layer shorthand can only express a [<size>] after a
           [<position>/]; a size with no position would serialise without the
           required slot and fail to reparse, so decline that combination. *)
        if layer.image = None then None
        else if layer.size <> None && layer.position = None then None
        else
          let merged =
            Declaration
              {
                property = Mask;
                value = (Layer layer : Properties.mask);
                important = is_important (List.hd raw_decls);
              }
          in
          Some ((idx, merged), rest)

let compose_mask_shorthand decls =
  let rec go ~seen_mask_border acc decls =
    match try_compose_mask decls with
    | Some (merged, rest) when not seen_mask_border ->
        go ~seen_mask_border (merged :: acc) rest
    | _ -> (
        match decls with
        | [] -> List.rev acc
        | ((_, d) as hd) :: rest ->
            let seen_mask_border =
              seen_mask_border
              ||
              match d with
              | Declaration { property = Mask_border; _ } -> true
              | _ -> false
            in
            go ~seen_mask_border (hd :: acc) rest)
  in
  go ~seen_mask_border:false [] decls

(* CSS Transitions 1 sec. 2.1: [transition] composes from
   [transition-{property,duration,timing-function,delay}]. Compose when a
   contiguous run covers a single layer (each longhand carries a one-entry
   list), the importance matches, no CSS-wide keyword leaks in, and
   [transition-property] is present (it has no default, so the shorthand needs
   it). *)
let transition_property_singleton :
    Properties.transition_property ->
    Properties.transition_property_value option = function
  | [ p ] -> ( match p with Inherit | Initial | Unset -> None | _ -> Some p)
  | _ -> None

let duration_singleton : Values.duration -> Values.duration option = function
  | Durations _ -> None
  | Inherit | Initial | Unset | Revert | Revert_layer -> None
  | d -> Some d

let timing_singleton :
    Properties.timing_function -> Properties.timing_function option = function
  | Timing_functions _ -> None
  | Inherit | Initial | Unset | Revert | Revert_layer -> None
  | t -> Some t

let tr_property_part :
    declaration -> Properties.transition_property_value option = function
  | Declaration { property = Transition_property; value; _ } ->
      transition_property_singleton value
  | _ -> None

let tr_duration_part : declaration -> Values.duration option = function
  | Declaration { property = Transition_duration; value; _ } ->
      duration_singleton value
  | _ -> None

let tr_timing_part : declaration -> Properties.timing_function option = function
  | Declaration { property = Transition_timing_function; value; _ } ->
      timing_singleton value
  | _ -> None

let tr_delay_part : declaration -> Values.duration option = function
  | Declaration { property = Transition_delay; value; _ } ->
      duration_singleton value
  | _ -> None

type tr_updater =
  declaration ->
  (Properties.transition_shorthand -> Properties.transition_shorthand) option

let lift_tr_part :
    'a.
    (declaration -> 'a option) ->
    (Properties.transition_shorthand -> 'a -> Properties.transition_shorthand) ->
    tr_updater =
 fun extract set d ->
  match extract d with Some v -> Some (fun s -> set s v) | None -> None

let tr_updaters : tr_updater list =
  [
    lift_tr_part tr_property_part (fun s v -> { s with property = v });
    lift_tr_part tr_duration_part (fun s v -> { s with duration = Some v });
    lift_tr_part tr_timing_part (fun s v -> { s with timing_function = Some v });
    lift_tr_part tr_delay_part (fun s v -> { s with delay = Some v });
  ]

let transition_part_of (d : declaration) =
  List.find_map (fun f -> f d) tr_updaters

let empty_tr_shorthand : Properties.transition_shorthand =
  {
    property = (All : Properties.transition_property_value);
    duration = None;
    timing_function = None;
    delay = None;
    behavior = None;
  }

let take_contiguous_transition indexed_decls =
  let rec aux acc = function
    | (i, d) :: rest -> (
        match transition_part_of d with
        | Some f -> aux ((d, f) :: acc) rest
        | None -> (List.rev acc, (i, d) :: rest))
    | [] -> (List.rev acc, [])
  in
  match indexed_decls with
  | [] -> (None, [])
  | (idx, _) :: _ -> (
      let parts, rest = aux [] indexed_decls in
      match parts with
      | [] -> (None, indexed_decls)
      | _ -> (Some (idx, parts), rest))

let has_transition_property_decl raw_decls =
  List.exists
    (fun d ->
      match d with
      | Declaration { property = Transition_property; _ } -> true
      | _ -> false)
    raw_decls

let try_compose_transition indexed_decls =
  let idx_opt, rest = take_contiguous_transition indexed_decls in
  match idx_opt with
  | None -> None
  | Some (idx, parts) ->
      let raw_decls = List.map fst parts in
      if List.length raw_decls < 2 then None
      else if not (same_importance raw_decls) then None
      else if not (has_transition_property_decl raw_decls) then None
      else
        let layer =
          List.fold_left (fun acc (_, f) -> f acc) empty_tr_shorthand parts
        in
        let merged =
          Declaration
            {
              property = Transition;
              value = [ (Shorthand layer : Properties.transition) ];
              important = is_important (List.hd raw_decls);
            }
        in
        Some ((idx, merged), rest)

let compose_transition_shorthand decls =
  let rec go acc decls =
    match (decls, try_compose_transition decls) with
    | [], _ -> List.rev acc
    | _, Some (merged, rest) -> go (merged :: acc) rest
    | hd :: rest, None -> go (hd :: acc) rest
  in
  go [] decls

(* CSS Animations 1 sec. 3.1: [animation] composes from the per-layer animation
   longhands. Compose when a contiguous run sticks to a single layer (no
   multi-value list constructor leaks in), no CSS-wide keyword appears, and the
   importance matches. *)
let animation_name_singleton :
    Properties.animation_name -> Properties.animation_name option = function
  | Names _ -> None
  | Inherit | Initial | Unset | Revert | Revert_layer -> None
  | n -> Some n

let animation_direction_singleton :
    Properties.animation_direction -> Properties.animation_direction option =
  function
  | Directions _ -> None
  | Inherit | Initial | Unset | Revert | Revert_layer -> None
  | d -> Some d

let animation_fill_mode_singleton :
    Properties.animation_fill_mode -> Properties.animation_fill_mode option =
  function
  | Fill_modes _ -> None
  | Inherit | Initial | Unset | Revert | Revert_layer -> None
  | f -> Some f

let animation_iteration_singleton :
    Properties.animation_iteration_count ->
    Properties.animation_iteration_count option = function
  | Counts _ -> None
  | Inherit | Initial | Unset | Revert | Revert_layer -> None
  | c -> Some c

let animation_play_state_singleton :
    Properties.animation_play_state -> Properties.animation_play_state option =
  function
  | States _ -> None
  | Inherit | Initial | Unset | Revert | Revert_layer -> None
  | p -> Some p

let an_name_part : declaration -> Properties.animation_name option = function
  | Declaration { property = Animation_name; value; _ } ->
      animation_name_singleton value
  | _ -> None

let an_duration_part : declaration -> Values.duration option = function
  | Declaration { property = Animation_duration; value; _ } ->
      duration_singleton value
  | _ -> None

let an_timing_part : declaration -> Properties.timing_function option = function
  | Declaration { property = Animation_timing_function; value; _ } ->
      timing_singleton value
  | _ -> None

let an_delay_part : declaration -> Values.duration option = function
  | Declaration { property = Animation_delay; value; _ } ->
      duration_singleton value
  | _ -> None

let an_iteration_part :
    declaration -> Properties.animation_iteration_count option = function
  | Declaration { property = Animation_iteration_count; value; _ } ->
      animation_iteration_singleton value
  | _ -> None

let an_direction_part : declaration -> Properties.animation_direction option =
  function
  | Declaration { property = Animation_direction; value; _ } ->
      animation_direction_singleton value
  | _ -> None

let an_fill_mode_part : declaration -> Properties.animation_fill_mode option =
  function
  | Declaration { property = Animation_fill_mode; value; _ } ->
      animation_fill_mode_singleton value
  | _ -> None

let an_play_state_part : declaration -> Properties.animation_play_state option =
  function
  | Declaration { property = Animation_play_state; value; _ } ->
      animation_play_state_singleton value
  | _ -> None

type an_updater =
  declaration ->
  (Properties.animation_shorthand -> Properties.animation_shorthand) option

let lift_an_part :
    'a.
    (declaration -> 'a option) ->
    (Properties.animation_shorthand -> 'a -> Properties.animation_shorthand) ->
    an_updater =
 fun extract set d ->
  match extract d with Some v -> Some (fun s -> set s v) | None -> None

let an_updaters : an_updater list =
  [
    lift_an_part an_name_part (fun s v -> { s with name = Some v });
    lift_an_part an_duration_part (fun s v -> { s with duration = Some v });
    lift_an_part an_timing_part (fun s v -> { s with timing_function = Some v });
    lift_an_part an_delay_part (fun s v -> { s with delay = Some v });
    lift_an_part an_iteration_part (fun s v ->
        { s with iteration_count = Some v });
    lift_an_part an_direction_part (fun s v -> { s with direction = Some v });
    lift_an_part an_fill_mode_part (fun s v -> { s with fill_mode = Some v });
    lift_an_part an_play_state_part (fun s v -> { s with play_state = Some v });
  ]

let animation_part_of (d : declaration) =
  List.find_map (fun f -> f d) an_updaters

let empty_an_shorthand : Properties.animation_shorthand =
  {
    name = None;
    duration = None;
    timing_function = None;
    delay = None;
    iteration_count = None;
    direction = None;
    fill_mode = None;
    play_state = None;
    timeline = None;
  }

let take_contiguous_animation indexed_decls =
  let rec aux acc = function
    | (i, d) :: rest -> (
        match animation_part_of d with
        | Some f -> aux ((d, f) :: acc) rest
        | None -> (List.rev acc, (i, d) :: rest))
    | [] -> (List.rev acc, [])
  in
  match indexed_decls with
  | [] -> (None, [])
  | (idx, _) :: _ -> (
      let parts, rest = aux [] indexed_decls in
      match parts with
      | [] -> (None, indexed_decls)
      | _ -> (Some (idx, parts), rest))

let try_compose_animation indexed_decls =
  let idx_opt, rest = take_contiguous_animation indexed_decls in
  match idx_opt with
  | None -> None
  | Some (idx, parts) ->
      let raw_decls = List.map fst parts in
      if List.length raw_decls < 2 then None
      else if not (same_importance raw_decls) then None
      else
        let layer =
          List.fold_left (fun acc (_, f) -> f acc) empty_an_shorthand parts
        in
        let merged =
          Declaration
            {
              property = Animation;
              value = [ (Shorthand layer : Properties.animation) ];
              important = is_important (List.hd raw_decls);
            }
        in
        Some ((idx, merged), rest)

let compose_animation_shorthand decls =
  let rec go acc decls =
    match (decls, try_compose_animation decls) with
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

let property_covered_by_important kept decl =
  List.exists
    (fun (_, existing) ->
      (not (is_intentionally_duplicated existing))
      && is_important existing
      && declaration_covers existing decl)
    kept

let same_property d1 d2 = String.equal (property_name d1) (property_name d2)

let same_minified_value new_decl existing =
  string_of_value ~minify:true new_decl = string_of_value ~minify:true existing

let legacy_vendor_fallback new_decl existing =
  (* Different-value duplicates are kept when one value is vendor-prefixed: the
     cascade may pick whichever the browser understands. *)
  same_property new_decl existing
  && (not (same_minified_value new_decl existing))
  && (value_is_vendor_prefixed existing || value_is_vendor_prefixed new_decl)

(* The earlier declaration is a real cascade fallback when the later one uses
   CSS Color 4 / 5 syntax that older browsers drop. *)
let legacy_color_fallback new_decl existing =
  same_property new_decl existing
  && (not (same_minified_value new_decl existing))
  && Declaration.value_uses_color_4 new_decl
  && not (Declaration.value_uses_color_4 existing)

(* Same shape: the later value uses a runtime substitution ([var()] / [env()] /
   [attr()]) and the earlier doesn't, so the earlier is a static fallback for
   browsers that can't resolve the substitution at parse time. *)
let legacy_runtime_subst_fallback new_decl existing =
  same_property new_decl existing
  && (not (same_minified_value new_decl existing))
  && Declaration.value_uses_runtime_subst new_decl
  && not (Declaration.value_uses_runtime_subst existing)

let same_property_value_declaration new_decl existing =
  same_property new_decl existing
  && same_minified_value new_decl existing
  && (is_important new_decl || not (is_important existing))

let covered_by_new_declaration new_decl existing =
  (not (is_intentionally_duplicated existing))
  && declaration_covers new_decl existing
  && (is_important new_decl || not (is_important existing))
  && (not (legacy_vendor_fallback new_decl existing))
  && (not (legacy_color_fallback new_decl existing))
  && not (legacy_runtime_subst_fallback new_decl existing)

let append_all_declaration idx decl kept =
  let before, after =
    List.partition
      (fun (_, old) -> not (all_preserved_reorder_declaration old))
      kept
  in
  before @ [ (idx, decl) ] @ after

let is_all_declaration = function
  | Declaration { property = All; _ } -> true
  | Theme_guarded { decl; _ } -> (
      match unwrap_theme_guard decl with
      | Declaration { property = All; _ } -> true
      | _ -> false)
  | _ -> false

let deduplicate_step kept (idx, decl) =
  if is_intentionally_duplicated decl then
    let kept =
      List.filter
        (fun (_, old) -> not (same_property_value_declaration decl old))
        kept
    in
    kept @ [ (idx, decl) ]
  else if (not (is_important decl)) && property_covered_by_important kept decl
  then kept
  else
    let kept =
      List.filter
        (fun (_, old) -> not (covered_by_new_declaration decl old))
        kept
    in
    if is_all_declaration decl then append_all_declaration idx decl kept
    else kept @ [ (idx, decl) ]

(* For each typed vendor-prefixed property, the typed unprefixed sibling. [None]
   for vendor-only properties ([-webkit-line-clamp],
   [-webkit-tap-highlight-color], ...). Drop-vendor-when-standard-present only
   fires when this returns [Some _] - keeps [text-decoration] off the list
   because dropping the WebKit copy regresses documented inheritance quirks. *)
let unprefixed_property_name : type a. a Properties.property -> string option =
  function
  | Webkit_transform -> Some "transform"
  | Webkit_transition -> Some "transition"
  | Webkit_filter -> Some "filter"
  | Webkit_backdrop_filter -> Some "backdrop-filter"
  | Webkit_user_select -> Some "user-select"
  | Webkit_appearance -> Some "appearance"
  | Webkit_text_size_adjust -> Some "text-size-adjust"
  | Webkit_hyphens -> Some "hyphens"
  | Moz_user_select -> Some "user-select"
  | O_transition -> Some "transition"
  | _ -> None

let unprefixed_property_of_decl decl =
  match unwrap_theme_guard decl with
  | Declaration { property; _ } -> unprefixed_property_name property
  | _ -> None

(* Drop a vendor-prefixed declaration when its unprefixed sibling appears in the
   same rule with the same minified value and importance. The unprefixed form
   supersedes in modern browsers, so the vendor copy is dead under the
   recent-browser policy. *)
let drop_vendor_aliases (kept : (int * declaration) list) :
    (int * declaration) list =
  let has_unprefixed_twin (_, decl) =
    match unprefixed_property_of_decl decl with
    | None -> false
    | Some unprefixed ->
        List.exists
          (fun (_, other) ->
            String.equal (property_name other) unprefixed
            && same_minified_value decl other
            && is_important decl = is_important other)
          kept
  in
  List.filter (fun item -> not (has_unprefixed_twin item)) kept

let compose_shorthands kept =
  kept |> compose_box_shorthands |> compose_pair_shorthands
  |> compose_outline_shorthand |> compose_font_shorthand
  |> compose_list_style_shorthand |> compose_flex_shorthand
  |> compose_text_decoration_shorthand |> compose_border_shorthand
  |> reorder_border_image_before_border |> compose_border_whole_shorthand
  |> drop_border_image_shadowed_by_border |> compose_background_shorthand
  |> compose_mask_shorthand |> compose_transition_shorthand
  |> compose_animation_shorthand
  |> fun kept ->
  merge_box_shorthand_longhands kept kept |> merge_overflow_longhands

let deduplicate_declarations_with ?(merge_box = true) props =
  let indexed_props = List.mapi (fun i decl -> (i, decl)) props in
  let kept = List.fold_left deduplicate_step [] indexed_props in
  let kept =
    let kept = if merge_box then compose_shorthands kept else kept in
    let kept = drop_vendor_aliases kept in
    List.map (fun (_, decl) -> decl) kept
  in
  duplicate_buggy_properties kept

let deduplicate_declarations ?scope props =
  let run () = deduplicate_declarations_with props in
  match scope with Some s -> with_scope s run | None -> run ()

let sort_commuting_declarations decls = decls

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

(* CSS Multicol 2 sec. 6.1: [column-width] + [column-count] in the same rule
   collapse to the [columns] shorthand. [columns] resets exactly those two
   longhands, so the rewrite preserves every other property's cascade and needs
   no closed-world assumption. Cascade models the longhands as unknown
   properties, so their values are re-parsed from text here. *)
let parse_column_width s : [ `Auto | `Width of Values.length ] option =
  if String.trim s = "auto" then Some `Auto
  else
    let t = Cursor.of_string s in
    match Values.read_length ~with_keywords:false t with
    | w ->
        Cursor.ws t;
        if Cursor.is_done t then Some (`Width w) else None
    | exception Cursor.Parse_error _ -> None

let parse_column_count s : [ `Auto | `Count of int ] option =
  if String.trim s = "auto" then Some `Auto
  else
    let t = Cursor.of_string s in
    match Cursor.int t with
    | n ->
        Cursor.ws t;
        if n > 0 && Cursor.is_done t then Some (`Count n) else None
    | exception Cursor.Parse_error _ -> None

let columns_value_of_longhands width count : Properties.columns_value =
  match (width, count) with
  | `Auto, `Auto -> (Auto : Properties.columns_value)
  | `Auto, `Count n -> Auto_count n
  | `Width w, `Auto -> Width w
  | `Width w, `Count n -> Both (w, n)

(* Emit [shorthand] where the first of the [name_a] / [name_b] longhands
   appeared and drop both. Used by shorthand synthesis whose shorthand resets
   exactly those two longhands, so the position move is cascade-safe. *)
let replace_longhand_pair ~name_a ~name_b ~shorthand decls =
  let is_pair d =
    String.equal (property_name d) name_a
    || String.equal (property_name d) name_b
  in
  let placed = ref false in
  List.filter_map
    (fun d ->
      if is_pair d then
        if !placed then None
        else (
          placed := true;
          Some shorthand)
      else Some d)
    decls

(* Synthesise a shorthand from the unique [name_a] + [name_b] longhands when
   both are present with matching importance and [build] accepts their value
   strings. *)
let synthesize_pair ~name_a ~name_b ~build decls =
  let named name d = String.equal (property_name d) name in
  let count name = List.length (List.filter (named name) decls) in
  match
    ( List.find_opt (named name_a) decls,
      count name_a,
      List.find_opt (named name_b) decls,
      count name_b )
  with
  | Some da, 1, Some db, 1 when is_important da = is_important db -> (
      match build ~important:(is_important da) da db with
      | Some shorthand -> replace_longhand_pair ~name_a ~name_b ~shorthand decls
      | None -> decls)
  | _ -> decls

let synthesize_columns decls =
  synthesize_pair ~name_a:"column-width" ~name_b:"column-count"
    ~build:(fun ~important wd cd ->
      match
        ( parse_column_width (string_of_value wd),
          parse_column_count (string_of_value cd) )
      with
      | Some width, Some count ->
          Some
            (Declaration.v ~important Properties.Columns
               (columns_value_of_longhands width count))
      | _ -> None)
    decls

(* CSS Anchor Positioning 1: [position-try] is [<'position-try-order'> ||
   <'position-try-fallbacks'>]. [position-try-order: normal] is the initial
   value and folds away. Cascade has no typed [position-try] shorthand, so the
   synthesised value is re-parsed into the (round-tripping) unknown property. *)
let synthesize_position_try decls =
  synthesize_pair ~name_a:"position-try-order" ~name_b:"position-try-fallbacks"
    ~build:(fun ~important od fd ->
      let order = string_of_value ~minify:true od in
      let fallbacks = string_of_value ~minify:true fd in
      let body =
        if String.equal order "normal" then fallbacks
        else String.concat " " [ order; fallbacks ]
      in
      match
        Declaration.of_string (String.concat "" [ "position-try:"; body ])
      with
      | shorthand ->
          Some
            (if important then Declaration.important shorthand else shorthand)
      | exception _ -> None)
    decls

let finalize_rule_without_nested (rule : rule) : rule =
  {
    rule with
    declarations =
      deduplicate_declarations rule.declarations
      |> synthesize_columns |> synthesize_position_try
      |> sort_commuting_declarations;
  }

(* Compare selectors as sets when both are comma lists: [h1, h2] and [h2, h1]
   target the same elements and should merge. Normalise by sorting the list
   entries via their minified spelling. *)
let canonical_selector_key (sel : Selector.t) : string =
  match Selector.as_list sel with
  | Some xs ->
      xs
      |> List.map (Selector.to_string ~minify:true)
      |> List.sort String.compare |> String.concat ","
  | None -> Selector.to_string ~minify:true sel

let rules_have_same_selector (prev : Stylesheet.rule) (rule : Stylesheet.rule) =
  String.equal
    (canonical_selector_key prev.selector)
    (canonical_selector_key rule.selector)
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

let should_not_combine selector =
  (* Already a list selector: nothing to combine. *)
  Selector.is_compound_list selector
  (* Vendor-prefixed pseudo-elements ([::-webkit-...]) are kept on their own
     rule: if any selector in a list is invalid in a browser, the whole list
     fails parsing, so grouping them with vendor-specific ones would break
     cross-browser compatibility. *)
  || contains_vendor_pseudo_element selector
  (* [.x ::marker] (descendant pseudo-element) must not be combined with
     [.x::marker] (direct pseudo-element): they target different elements. *)
  || has_descendant_pseudo_element selector

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
let rule_of_group :
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
  match rule_of_group group with Some rule -> rule :: acc | None -> acc

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

let summary_of_rule (rule : Stylesheet.rule) =
  Selector_summary.of_selector rule.Stylesheet_intf.selector

let property_set decls = List.map (fun d -> Declaration.property_name d) decls

let writes_any_of props decls =
  List.exists (fun d -> List.mem (Declaration.property_name d) props) decls

let delayed_blocks_group ~group_props ~rule_summary delayed =
  List.exists
    (fun (dr, ds) ->
      Selector_summary.may_overlap ds rule_summary
      && writes_any_of group_props dr.Stylesheet_intf.declarations)
    delayed

let rule_perturbs_group ~group_props ~rule_summary current_group rule =
  let overlaps_group =
    List.exists
      (fun (_, s) -> Selector_summary.may_overlap s rule_summary)
      current_group
  in
  overlaps_group && writes_any_of group_props rule.Stylesheet_intf.declarations

let flush_combined_group acc current_group delayed =
  let group_members = List.map fst current_group in
  let acc = flush_group acc group_members in
  List.fold_left (fun acc (rule, _) -> rule :: acc) acc delayed

let can_join_group ~group_props ~rule_summary delayed prev_rule rule =
  can_combine_rules prev_rule rule
  && not (delayed_blocks_group ~group_props ~rule_summary delayed)

let group_member rule rule_summary = (group_member_of_rule rule, rule_summary)

(* Adjacent rules with identical declarations that combine_identical_rules
   couldn't fold into its current group (because the group's anchor was a
   different declaration block) end up next to each other in the output. A
   simple post-pass folds those into a single selector-list rule. cleancss /
   csso both rely on this peephole to collapse e.g. [.a{top:0}.b{top:0}] when
   the previous rule wrote something unrelated. *)
let combine_adjacent_identical_decls (rules : Stylesheet.rule list) :
    Stylesheet.rule list =
  let rec walk acc = function
    | [] -> List.rev acc
    | r1 :: r2 :: rest
      when (not (rule_cannot_combine r1))
           && (not (rule_cannot_combine r2))
           && can_combine_rules r1 r2 ->
        let group_member r = (group_member_of_rule r, ()) in
        let members = [ group_member r2; group_member r1 ] in
        let combined =
          match rule_of_group (List.map fst members) with
          | Some r -> r
          | None -> r1
        in
        walk acc (combined :: rest)
    | r :: rest -> walk (r :: acc) rest
  in
  walk [] rules

let combine_identical_rules (rules : Stylesheet.rule list) :
    Stylesheet.rule list =
  (* Delayed rules can slide around the merge group when they do not write any
     property the group writes. That keeps the cascade observable unchanged even
     when selector subjects may overlap. *)
  let rec combine_consecutive acc current_group delayed = function
    | [] -> List.rev (flush_combined_group acc current_group delayed)
    | (rule : Stylesheet.rule) :: rest ->
        if rule_cannot_combine rule then
          let acc = rule :: flush_combined_group acc current_group delayed in
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
        let group_props = property_set prev_rule.declarations in
        if can_join_group ~group_props ~rule_summary delayed prev_rule rule then
          push_to_group ()
        else
          let rule_perturbs_group =
            rule_perturbs_group ~group_props ~rule_summary current_group rule
          in
          if not rule_perturbs_group then
            combine_consecutive acc current_group
              (delayed @ [ (rule, rule_summary) ])
              rest
          else
            let acc = flush_combined_group acc current_group delayed in
            let member = group_member rule rule_summary in
            combine_consecutive acc [ member ] [] rest
  in
  combine_consecutive [] [] [] rules |> combine_adjacent_identical_decls

(* CSS Cascade L5: when a run of adjacent rules shares two or more identical
   declarations, hoist them into a single grouped rule whose selector is the
   union of the originals, and keep the remaining (rule-specific) declarations
   in per-selector follow-up rules. The transformation preserves cascade order
   for every element, regardless of whether it matches one or several of the
   original selectors:

   S1 { X; A } S1, S2 { X } S2 { X; B } becomes S1 { A } S2 { B }

   Only adjacent rules are eligible; an intervening rule's matched elements
   could otherwise pick up declarations they didn't see in the source. Nested
   rules and unparsed merge keys disable factoring as a precaution.

   Byte budget: hoisting saves [(N - 1) * common_size] but costs
   [sum(selector_size_i) + N] extra characters for the new rule headers and
   commas. Only commit when the savings are positive. *)

let decls_pp_string ds =
  let pp ctx ds = List.iter (Declaration.pp_declaration ctx) ds in
  Pp.to_string ~minify:true pp ds

let rule_factor_eligible (r : Stylesheet.rule) =
  r.nested = [] && r.merge_key = None
  && (not (should_not_combine r.selector))
  && not (List.exists is_all_declaration r.declarations)

let decl_property d = Declaration.property_name d

let merge_selector_list = function
  | [ s ] -> s
  | sels ->
      let flatten = function Selector.List xs -> xs | s -> [ s ] in
      Selector.List (List.concat_map flatten sels)

(* Byte size of the printed rule [<selector>{<d1>;<d2>;...;<dn>}].
   [decls_pp_string] concatenates declarations without separators - the [;]
   between them is added by the rule printer at emission time, so we add [n - 1]
   back here. *)
let rule_pp_size (r : Stylesheet.rule) =
  let sel = Pp.to_string ~minify:true Selector.pp r.Stylesheet_intf.selector in
  let decls = decls_pp_string r.Stylesheet_intf.declarations in
  let separators =
    match r.declarations with [] | [ _ ] -> 0 | _ :: rest -> List.length rest
  in
  String.length sel + 2 + String.length decls + separators

(* [factorable_default decl_for] returns the first rule's declaration for a
   property iff every rule declares the same property. Default-pick is
   considered separately because it may need a leftover override even when the
   value matches (see [keep_default_at]). *)
let factorable_default rules prop =
  let decl_for r =
    List.find_opt
      (fun d -> decl_property d = prop)
      r.Stylesheet_intf.declarations
  in
  match rules with
  | [] -> None
  | first :: _ -> (
      match decl_for first with
      | None -> None
      | Some _ as fst ->
          if List.for_all (fun r -> decl_for r <> None) rules then fst else None
      )

(* For a rule [R_i] whose value for [prop] equals the default, we must still
   emit it in the leftover when an EARLIER rule [R_j] with overlapping selector
   declares a different value - otherwise [R_j]'s leftover would override the
   shared default for elements matching both. *)
let earlier_overrides_overlap ~summaries ~default decls i =
  let r_i_summary = Array.get summaries i in
  let default_prop = decl_property default in
  let rec loop j =
    if j >= i then false
    else
      let d_j =
        List.find_opt
          (fun d -> decl_property d = default_prop)
          (Array.get decls j)
      in
      match d_j with
      | Some d
        when d <> default
             && Selector_summary.may_overlap (Array.get summaries j) r_i_summary
        ->
          true
      | _ -> loop (j + 1)
  in
  loop 0

let common_factorable_decls rules first =
  List.filter_map
    (fun d -> factorable_default rules (decl_property d))
    first.Stylesheet_intf.declarations

let keep_factor_leftover ~summaries ~decls ~default_decl ~i decl =
  decl <> default_decl
  || earlier_overrides_overlap ~summaries ~default:default_decl decls i

let leftover_for_factor_rule ~common ~summaries ~decls i (r : Stylesheet.rule) =
  List.filter
    (fun d ->
      let prop = decl_property d in
      match List.find_opt (fun d' -> decl_property d' = prop) common with
      | None -> true
      | Some default ->
          keep_factor_leftover ~summaries ~decls ~default_decl:default ~i d)
    r.declarations

let grouped_factor_rule first rules_arr common =
  let sels =
    Array.to_list (Array.map (fun r -> r.Stylesheet_intf.selector) rules_arr)
  in
  { first with selector = merge_selector_list sels; declarations = common }

let factor_leftovers ~common ~summaries ~decls rules_arr =
  let acc = ref [] in
  for i = Array.length rules_arr - 1 downto 0 do
    let r = rules_arr.(i) in
    let l = leftover_for_factor_rule ~common ~summaries ~decls i r in
    if l <> [] then acc := { r with declarations = l } :: !acc
  done;
  !acc

let rules_pp_size rules =
  List.fold_left (fun acc r -> acc + rule_pp_size r) 0 rules

let factorise_group (rules : Stylesheet.rule list) : Stylesheet.rule list =
  match rules with
  | [] | [ _ ] -> rules
  | first :: _ ->
      let rules_arr = Array.of_list rules in
      let summaries =
        Array.map
          (fun r -> Selector_summary.of_selector r.Stylesheet_intf.selector)
          rules_arr
      in
      let decls =
        Array.map (fun r -> r.Stylesheet_intf.declarations) rules_arr
      in
      let common = common_factorable_decls rules first in
      if common = [] then rules
      else
        let grouped = grouped_factor_rule first rules_arr common in
        let leftovers = factor_leftovers ~common ~summaries ~decls rules_arr in
        let before_size = rules_pp_size rules in
        let after_size = rule_pp_size grouped + rules_pp_size leftovers in
        if after_size <= before_size then grouped :: leftovers else rules

let factor_common_declarations (rules : Stylesheet.rule list) :
    Stylesheet.rule list =
  let rec group acc current = function
    | [] -> List.rev_append (factorise_group (List.rev current)) acc
    | (r : Stylesheet.rule) :: rest ->
        if not (rule_factor_eligible r) then
          let acc = List.rev_append (factorise_group (List.rev current)) acc in
          group (r :: acc) [] rest
        else
          let shares_a_property () =
            match current with
            | [] -> r.declarations <> []
            | _ ->
                List.exists
                  (fun d ->
                    let prop = decl_property d in
                    List.for_all
                      (fun r' ->
                        List.exists
                          (fun d' -> decl_property d' = prop)
                          r'.Stylesheet_intf.declarations)
                      current)
                  r.declarations
          in
          if shares_a_property () then group acc (r :: current) rest
          else
            let acc =
              List.rev_append (factorise_group (List.rev current)) acc
            in
            group acc [ r ] rest
  in
  List.rev (group [] [] rules)

(** {1 Statement Optimization} *)

(* Merge consecutive media queries with the same condition. This only merges
   immediately adjacent media queries to preserve cascade order. When blocks are
   merged, we recursively call merge_consecutive_media on the combined content
   to merge any inner consecutive media queries. *)
(* Shared predicates for media block optimization. Width bounds reach these
   helpers either as [min-]/[max-] plains or, after [lower_for_minify], as the
   range forms on the [width] feature; both classify identically through
   [Media.kind]. *)
let media_feature_is name (f : Media.feature) =
  match f with
  | Media.Plain (n, _) | Media.Boolean n -> n = name
  | Media.Range (n, _, _) | Media.Range_rev (_, _, n) -> n = name
  | Media.Interval (_, _, n, _, _) -> n = name

let rec condition_has_feature name (c : Media.condition) =
  match c with
  | Media.Feature f -> media_feature_is name f
  | Media.Not c -> condition_has_feature name c
  | Media.And (a, b) | Media.Or (a, b) ->
      condition_has_feature name a || condition_has_feature name b

(* A query mentions feature [name] either directly or as the trailing condition
   of a media type ([not all and (min-width: ...)]). *)
let rec query_has_feature name (q : Media.t) =
  match q with
  | Media.Cond c -> condition_has_feature name c
  | Media.Type { trailing = Some c; _ } -> condition_has_feature name c
  | Media.Type _ -> false
  | Media.List qs -> List.exists (query_has_feature name) qs

let has_nested_preference_media block =
  List.exists
    (function
      | Media (cond, _) ->
          query_has_feature Media.Prefers_contrast cond
          || query_has_feature Media.Prefers_reduced_motion cond
          || query_has_feature Media.Prefers_color_scheme cond
      | _ -> false)
    block

(* CSS Cascade 6.4: consecutive named [@layer] blocks with the same name are
   spec-equivalent to a single block. Merge them when no rule with a conflicting
   condition appears between. Anonymous layers stay distinct because each
   [@layer { ... }] without a name creates a new layer. *)
let merge_consecutive_layers ~optimize_merged_block (stmts : statement list) :
    statement list =
  (* [acc] holds output in REVERSE order so we cons (O(1)) rather than append
     (O(n)); reverse once at the end. *)
  let rec merge acc prev = function
    | [] -> (
        match prev with
        | Some (Some name, block) ->
            List.rev (Layer (Some name, optimize_merged_block block) :: acc)
        | Some (None, block) -> List.rev (Layer (None, block) :: acc)
        | None -> List.rev acc)
    | Layer (Some name, block) :: rest -> (
        match prev with
        | Some (Some prev_name, prev_block) when String.equal prev_name name ->
            merge acc (Some (Some name, prev_block @ block)) rest
        | Some (Some prev_name, prev_block) ->
            merge
              (Layer (Some prev_name, optimize_merged_block prev_block) :: acc)
              (Some (Some name, block))
              rest
        | Some (None, prev_block) ->
            merge
              (Layer (None, prev_block) :: acc)
              (Some (Some name, block))
              rest
        | None -> merge acc (Some (Some name, block)) rest)
    | (Layer (None, _) as anon) :: rest -> (
        match prev with
        | Some (Some prev_name, prev_block) ->
            merge
              (Layer (Some prev_name, optimize_merged_block prev_block) :: acc)
              None (anon :: rest)
        | Some (None, prev_block) ->
            merge (Layer (None, prev_block) :: acc) None (anon :: rest)
        | None -> merge (anon :: acc) None rest)
    | stmt :: rest -> (
        match prev with
        | Some (Some name, block) ->
            merge
              (stmt :: Layer (Some name, optimize_merged_block block) :: acc)
              None rest
        | Some (None, block) ->
            merge (stmt :: Layer (None, block) :: acc) None rest
        | None -> merge (stmt :: acc) None rest)
  in
  merge [] None stmts

let merge_consecutive_media ~optimize_merged_block (stmts : statement list) :
    statement list =
  (* When we merge media blocks, the resulting block may have consecutive rules
     with identical declarations that should be combined. Re-run the statement
     optimizer on the merged content. *)
  (* [acc] holds output in REVERSE order; reverse once at the end. *)
  let rec merge acc prev_media = function
    | [] -> (
        match prev_media with
        | Some (cond, block) ->
            List.rev (Media (cond, optimize_merged_block block) :: acc)
        | None -> List.rev acc)
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
            let merged_block = prev_block @ block in
            merge acc (Some (cond, merged_block)) rest
        | Some (prev_cond, prev_block) ->
            merge
              (Media (prev_cond, optimize_merged_block prev_block) :: acc)
              (Some (cond, block))
              rest
        | None -> merge acc (Some (cond, block)) rest)
    | stmt :: rest -> (
        match prev_media with
        | Some (cond, block) ->
            merge
              (stmt :: Media (cond, optimize_merged_block block) :: acc)
              None rest
        | None -> merge (stmt :: acc) None rest)
  in
  merge [] None stmts

(* CSS Conditional Rules 5: adjacent same-condition [@supports] / [@container]
   blocks may be merged because the cascade evaluates them identically. Mirror
   the [@media] approach. *)
let merge_consecutive_supports ~optimize_merged_block (stmts : statement list) :
    statement list =
  (* [acc] holds output in REVERSE order; reverse once at the end. *)
  let rec merge acc prev = function
    | [] -> (
        match prev with
        | Some (cond, block) ->
            List.rev (Supports (cond, optimize_merged_block block) :: acc)
        | None -> List.rev acc)
    | Supports (cond, block) :: rest -> (
        match prev with
        | Some (prev_cond, prev_block) when Supports.equal prev_cond cond ->
            merge acc (Some (cond, prev_block @ block)) rest
        | Some (prev_cond, prev_block) ->
            merge
              (Supports (prev_cond, optimize_merged_block prev_block) :: acc)
              (Some (cond, block))
              rest
        | None -> merge acc (Some (cond, block)) rest)
    | stmt :: rest -> (
        match prev with
        | Some (cond, block) ->
            merge
              (stmt :: Supports (cond, optimize_merged_block block) :: acc)
              None rest
        | None -> merge (stmt :: acc) None rest)
  in
  merge [] None stmts

let merge_consecutive_containers ~optimize_merged_block (stmts : statement list)
    : statement list =
  let compare_condition a b =
    match (a, b) with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some a, Some b -> Container.compare a b
  in
  (* [acc] holds output in REVERSE order; reverse once at the end. *)
  let rec merge acc prev = function
    | [] -> (
        match prev with
        | Some (name, cond, block) ->
            List.rev (Container (name, cond, optimize_merged_block block) :: acc)
        | None -> List.rev acc)
    | Container (name, cond, block) :: rest -> (
        match prev with
        | Some (prev_name, prev_cond, prev_block)
          when prev_name = name && compare_condition prev_cond cond = 0 ->
            merge acc (Some (name, cond, prev_block @ block)) rest
        | Some (prev_name, prev_cond, prev_block) ->
            merge
              (Container (prev_name, prev_cond, optimize_merged_block prev_block)
              :: acc)
              (Some (name, cond, block))
              rest
        | None -> merge acc (Some (name, cond, block)) rest)
    | stmt :: rest -> (
        match prev with
        | Some (name, cond, block) ->
            merge
              (stmt
              :: Container (name, cond, optimize_merged_block block)
              :: acc)
              None rest
        | None -> merge (stmt :: acc) None rest)
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
  (* CSS Cascade 5 sec. 6.6.2: [@layer A, B;] declares layers in source order;
     re-declaring a layer name is a no-op for cascade order. Merge consecutive
     [@layer ...;] statements and dedupe repeated names so [@layer u; @layer u]
     emits one [@layer u]. *)
  let dedup_preserving_order names =
    let seen = Hashtbl.create (List.length names) in
    List.filter
      (fun name ->
        if Hashtbl.mem seen name then false
        else begin
          Hashtbl.add seen name ();
          true
        end)
      names
  in
  let rec merge acc = function
    | [] -> List.rev acc
    | Layer_decl names1 :: Layer_decl names2 :: rest ->
        merge acc (Layer_decl (dedup_preserving_order (names1 @ names2)) :: rest)
    | Layer_decl names :: rest ->
        merge (Layer_decl (dedup_preserving_order names) :: acc) rest
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

(* CSS Cascade 5 sec 6.4.2: same-name [@layer] blocks in the same enclosing
   context accumulate into one layer. Merge them at every level (top-level and
   nested inside @media / @supports / @container) but only when the first
   occurrence is non-empty - leading empty blocks fold into Layer_decl
   downstream. *)
let merge_named_layers_by_name (stmts : statement list) : statement list =
  let is_empty_block = function [] -> true | _ -> false in
  let content : (string, statement list) Hashtbl.t = Hashtbl.create 8 in
  let first_nonempty : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  List.iter
    (fun stmt ->
      match stmt with
      | Layer (Some name, block) when not (is_empty_block block) ->
          let prev =
            Hashtbl.find_opt content name |> Option.value ~default:[]
          in
          Hashtbl.replace content name (prev @ block);
          if not (Hashtbl.mem first_nonempty name) then
            Hashtbl.add first_nonempty name ()
      | _ -> ())
    stmts;
  let emitted = Hashtbl.create 8 in
  List.filter_map
    (fun stmt ->
      match stmt with
      | Layer (Some name, block) when not (is_empty_block block) ->
          if Hashtbl.mem emitted name then None
          else begin
            Hashtbl.add emitted name ();
            Some (Layer (Some name, Hashtbl.find content name))
          end
      | _ -> Some stmt)
    stmts

(* Require structural selector equality (not just set-equality) so that [h1, h2]
   and [h2, h1] go through [merge_rules] instead - that pass keeps the earlier
   rule's selector spelling, which is the form authors are more likely to want
   preserved. *)
let rule_shadows ~earlier ~later =
  earlier.Stylesheet_intf.selector = later.Stylesheet_intf.selector
  && List.for_all
       (fun ed ->
         List.exists
           (fun ld ->
             same_property ed ld
             && (Declaration.is_important ld
                || not (Declaration.is_important ed)))
           later.Stylesheet_intf.declarations)
       earlier.Stylesheet_intf.declarations

(* Drop an earlier rule when a later rule with the same canonical selector
   writes every one of its property names at the same or stronger importance.
   The later same-property write shadows the earlier value regardless of
   intervening rules. *)
let drop_shadowed_rules (rules : rule list) : rule list =
  let indexed = List.mapi (fun i r -> (i, r)) rules in
  List.filter_map
    (fun (i, rule) ->
      let shadowed =
        List.exists
          (fun (j, later) -> j > i && rule_shadows ~earlier:rule ~later)
          indexed
      in
      if shadowed then None else Some rule)
    indexed

(* Finer-grained sibling of [drop_shadowed_rules]: keep the rule but drop the
   individual declarations whose property is rewritten by a later rule for every
   selector this rule targets. The later same-selector write masks the earlier
   value for every matching element regardless of intervening rules' specificity
   or [!important] state (cleancss and csso both rely on this to collapse
   repeated declaration blocks).

   For list selectors ([.x, .y { width: 100px }]), the declaration is dead only
   when EACH selector in the list is shadowed by some later rule whose selector
   list contains that selector at same-or-stronger importance. We index later
   rules per individual selector key so the per-selector check is linear in the
   size of the list.

   Empty rules left behind are pruned downstream by [drop_empty_rules] on the
   statement-list pass. *)
let drop_shadowed_declarations (rules : rule list) : rule list =
  let indexed = List.mapi (fun i r -> (i, r)) rules in
  let single_keys r =
    match Selector.as_list r.Stylesheet_intf.selector with
    | Some xs -> List.map canonical_selector_key xs
    | None -> [ canonical_selector_key r.Stylesheet_intf.selector ]
  in
  (* For each individual-selector key, accumulate (rule_index, declarations) of
     every later rule whose selector list contains a member with that key. *)
  let later_by_key : (string, (int * Declaration.t list) list) Hashtbl.t =
    Hashtbl.create 16
  in
  List.iter
    (fun (i, r) ->
      List.iter
        (fun key ->
          let prev =
            Hashtbl.find_opt later_by_key key |> Option.value ~default:[]
          in
          Hashtbl.replace later_by_key key
            ((i, r.Stylesheet_intf.declarations) :: prev))
        (single_keys r))
    indexed;
  List.map
    (fun (i, rule) ->
      let keys = single_keys rule in
      let property_shadowed_for_key prop_decl key =
        let writes =
          Hashtbl.find_opt later_by_key key |> Option.value ~default:[]
        in
        List.exists
          (fun (j, decls) ->
            j > i
            && List.exists
                 (fun ld ->
                   same_property prop_decl ld
                   && (Declaration.is_important ld
                      || not (Declaration.is_important prop_decl)))
                 decls)
          writes
      in
      let shadowed_by_later decl =
        (not (is_intentionally_duplicated decl))
        && List.for_all (property_shadowed_for_key decl) keys
      in
      let kept =
        List.filter (fun d -> not (shadowed_by_later d)) rule.declarations
      in
      { rule with declarations = kept })
    indexed

let contains_nesting sel =
  Selector.any (function Selector.Nesting -> true | _ -> false) sel

let substitute_nesting ~parent sel =
  Selector.map (function Selector.Nesting -> parent | s -> s) sel

let combine_with_parent (parent : Selector.t) (child : Selector.t) : Selector.t
    =
  match child with
  | Selector.Relative (comb, right) ->
      (* A nested rule that starts with a combinator ([> .bar]) is parsed as a
         relative selector; the combinator is taken relative to the parent, so
         [parent { > .bar }] flattens to [parent > .bar]. *)
      Selector.Combined (parent, comb, substitute_nesting ~parent right)
  | _ when contains_nesting child -> substitute_nesting ~parent child
  | _ -> Selector.Combined (parent, Selector.Descendant, child)

let is_selector_list = function Selector.List _ -> true | _ -> false

(* CSS Nesting 1: a rule with no declarations and exactly one nested rule is a
   pure wrapper. Merge the child up by substituting the parent for the child's
   [&] (or appending it as a descendant): this drops the wrapper braces without
   duplicating the parent selector, so it always shortens. Restricted to a
   non-[List] parent because [&] expands to [:is(<parent>)], whose specificity
   equals the parent only when the parent is a single complex selector, so
   textual substitution is specificity-preserving only there. The child's nested
   statements are assumed already optimised by the caller. *)
let rec merge_lone_nested_rule (rule : rule) : rule =
  match (rule.declarations, rule.nested) with
  | [], [ Rule child ] when not (is_selector_list rule.selector) ->
      merge_lone_nested_rule
        {
          child with
          selector = combine_with_parent rule.selector child.selector;
        }
  | _ -> rule

(* CSS Nesting synthesis (inverse of flattening). [strip_prefix parent child] is
   the relative nested selector [rel] with [combine_with_parent parent rel =
   child], when [child] is an exact local extension of [parent]: a trailing
   combinator step ([parent > x] -> [> x] / a bare descendant) or a compound
   suffix on the rightmost compound ([a] -> [a:hover] -> [&:hover]). [None] for
   anything that is not a plain prefix extension, so [:is] / [:where] / list
   distribution never sneak in. *)
let rec strip_prefix (parent : Selector.t) (child : Selector.t) :
    Selector.t option =
  match (parent, child) with
  | _, Selector.Combined (cp, comb, crest) when cp = parent ->
      Some
        (if comb = Selector.Descendant then crest
         else Selector.Relative (comb, crest))
  | Selector.Combined (pp, pcomb, prest), Selector.Combined (cp, ccomb, crest)
    when pcomb = ccomb && pp = cp ->
      strip_prefix prest crest
  | _, Selector.Compound cps -> (
      let pps =
        match parent with Selector.Compound l -> l | single -> [ single ]
      in
      let rec drop p c =
        match (p, c) with
        | [], rest -> Some rest
        | ph :: pt, ch :: ct when ph = ch -> drop pt ct
        | _ -> None
      in
      match drop pps cps with
      | Some (_ :: _ as suffix) ->
          Some (Selector.Compound (Selector.Nesting :: suffix))
      | _ -> None)
  | _ -> None

let extends a b = strip_prefix a b <> None

(* The class / id / element / attribute simple selectors that identify which
   elements a selector can match. Collected by traversing with [Selector.any]
   under an always-[false] predicate (visits the whole tree). *)
let identifying_components sel =
  let acc = ref [] in
  ignore
    (Selector.any
       (fun s ->
         (match s with
         | Selector.Class _ | Selector.Id _
         | Selector.Element (_, _)
         | Selector.Attribute _ ->
             acc := s :: !acc
         | _ -> ());
         false)
       sel);
  !acc

(* Two selectors "compete" when they could match a common element - a sound
   (conservative) approximation is that they share an identifying component.
   Synthesis only fires for a chain that is isolated from every other rule in
   the run under this relation, which blocks the fan case ([.card:hover] +
   [.card .title] both off [.card]), the cascade competitor case ([.item.active]
   beside a later [.active]), and the boundary-duplicate case ([.card] hoisted
   out of [@supports] beside another [.card]). *)
let selectors_compete a b =
  let ca = identifying_components a in
  List.exists (fun t -> List.mem t (identifying_components b)) ca

(* Fold a chain [r0; r1; ...; rk] (each [r(i+1)] extends [ri]'s selector) into a
   single nested rule rooted at [r0], substituting the running parent for each
   child's [&]. *)
let rec nest_chain (root : rule) : rule list -> rule = function
  | [] -> root
  | child :: rest -> (
      match strip_prefix root.selector child.selector with
      | Some rel ->
          (* Recurse with [child]'s absolute selector so [rest] is stripped
             against the right parent, then relativise [child]'s own
             selector. *)
          let nested_child = nest_chain child rest in
          let nested_child = { nested_child with selector = rel } in
          { root with nested = root.nested @ [ Rule nested_child ] }
      | None -> root)

let synthesize_shortens (before : rule list) (after : rule) : bool =
  let len rules =
    String.length
      (Stylesheet.to_string ~minify:true (List.map (fun r -> Rule r) rules))
  in
  len [ after ] < len before

(* CSS Nesting synthesis under [feedback_nesting_synthesis_safety]: split the
   run into maximal chains where each rule extends its immediate predecessor,
   and fold a chain into nested form only when (1) the parent is a single
   (non-[List]) selector so [&] preserves specificity, (2) the chain is isolated
   - no rule outside it [selectors_compete]s with any member (adjacency / no
   competitor / no boundary-duplicate), and (3) the result is strictly shorter.
   Chains that fail any guard stay flat. *)
let synthesize_nesting_rules (rules : rule list) : rule list =
  let arr = Array.of_list rules in
  let n = Array.length arr in
  (* Maximal chains as [start, length] over consecutive-extends. *)
  let chains = ref [] in
  let i = ref 0 in
  while !i < n do
    let start = !i in
    incr i;
    while !i < n && extends arr.(!i - 1).selector arr.(!i).selector do
      incr i
    done;
    chains := (start, !i - start) :: !chains
  done;
  let chains = List.rev !chains in
  let isolated start len =
    let members = Array.sub arr start len in
    let related_outside =
      Array.to_list arr
      |> List.mapi (fun idx r -> (idx, r))
      |> List.exists (fun (idx, r) ->
          (idx < start || idx >= start + len)
          && Array.exists
               (fun m -> selectors_compete m.selector r.selector)
               members)
    in
    not related_outside
  in
  List.concat_map
    (fun (start, len) ->
      let members = Array.to_list (Array.sub arr start len) in
      match members with
      | root :: (_ :: _ as rest)
        when (not (is_selector_list root.selector)) && isolated start len ->
          let nested = nest_chain root rest in
          if synthesize_shortens members nested then [ nested ] else members
      | _ -> members)
    chains

(* Apply [synthesize_nesting_rules] to each maximal run of consecutive [Rule]
   statements. Runs at the statement-list level (not inside [rules_aux]) so that
   rules unwrapped from a baseline [@supports] are contiguous with their
   siblings here: the chain isolation check then sees the duplicate-selector
   competitor and declines to synthesize across the former boundary. *)
let synthesize_nesting_statements (stmts : statement list) : statement list =
  let rec span_rules acc = function
    | Rule r :: rest -> span_rules (r :: acc) rest
    | rest -> (List.rev acc, rest)
  in
  let rec go acc = function
    | [] -> List.rev acc
    | Rule _ :: _ as l ->
        let run, rest = span_rules [] l in
        let synthesized =
          synthesize_nesting_rules run |> List.map (fun r -> Rule r)
        in
        go (List.rev_append synthesized acc) rest
    | s :: rest -> go (s :: acc) rest
  in
  go [] stmts

let rec statements ~enforce_spec (stmts : statement list) : statement list =
  let optimize_merged_block = statements ~enforce_spec in
  merge_named_layers_by_name stmts
  |> process_statements ~enforce_spec []
  |> synthesize_nesting_statements
  |> merge_consecutive_media ~optimize_merged_block
  |> merge_consecutive_supports ~optimize_merged_block
  |> merge_consecutive_containers ~optimize_merged_block
  |> merge_layer_declarations |> drop_misplaced_imports |> drop_empty_rules

and process_statements ~enforce_spec (acc : statement list)
    (remaining : statement list) : statement list =
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
      (* @scope is a valid nested group rule per CSS Nesting; preserve it as-is
         during normal optimization. Flattening is reserved for the explicit
         [flatten_nesting] transform. *)
      let optimized = rules_aux ~enforce_spec plain_rules in
      let as_statements = List.map (fun r -> Rule r) optimized in
      process_statements ~enforce_spec (List.rev_append as_statements acc) rest
  | Media (cond, block) :: rest ->
      (* Just optimize the block and pass through - grouping happens later.
         Outside [enforce_spec], apply the target-fact grammar upgrades
         ([min-]/[max-] to range syntax, lower+upper bound to interval) that the
         pretty-printer no longer performs. *)
      let cond = if enforce_spec then cond else Media.lower_for_minify cond in
      let optimized_block = statements ~enforce_spec block in
      let optimized = Media (cond, optimized_block) in
      process_statements ~enforce_spec (optimized :: acc) rest
  | Container (name, cond, block) :: rest ->
      (* Recursively optimize container query content *)
      let cond =
        if enforce_spec then cond
        else Option.map Container.lower_for_minify cond
      in
      let optimized = Container (name, cond, statements ~enforce_spec block) in
      process_statements ~enforce_spec (optimized :: acc) rest
  | Supports (cond, block) :: rest -> (
      (* Recursively optimize supports block content *)
      let optimized_block = statements ~enforce_spec block in
      let baseline =
        if enforce_spec then `Cond cond else Supports.simplify_baseline cond
      in
      match baseline with
      (* Condition is a known-true baseline fact: unwrap the block into the
         current statement stream so its rules merge with their siblings. *)
      | `True -> process_statements ~enforce_spec acc (optimized_block @ rest)
      (* Condition can never hold: the guarded block is dead. *)
      | `False -> process_statements ~enforce_spec acc rest
      | `Cond cond' ->
          process_statements ~enforce_spec
            (Supports (cond', optimized_block) :: acc)
            rest)
  | Scope (start, end_, block) :: rest ->
      let optimized = Scope (start, end_, statements ~enforce_spec block) in
      process_statements ~enforce_spec (optimized :: acc) rest
  | Origin (origin, block) :: rest ->
      let optimized = Origin (origin, statements ~enforce_spec block) in
      process_statements ~enforce_spec (optimized :: acc) rest
  | Layer (name, block) :: rest ->
      let optimized_block = statements ~enforce_spec block in
      if is_layer_empty optimized_block then
        (* Handle empty layer optimization *)
        match name with
        | Some layer_name ->
            let all_names, remaining =
              collect_empty_layer_names [ layer_name ] rest
            in
            let layer_decl = Layer_decl all_names in
            process_statements ~enforce_spec (layer_decl :: acc) remaining
        | None ->
            (* Anonymous empty layer - just remove it *)
            process_statements ~enforce_spec acc rest
      else
        let optimized = Layer (name, optimized_block) in
        process_statements ~enforce_spec (optimized :: acc) rest
  | Import import :: rest ->
      (* A [supports()] condition on [@import] is a feature query: when it is a
         known-true baseline fact, default minify drops just the condition (the
         import always applies). [--enforce-spec] keeps it. *)
      let import =
        match import.supports with
        | Some cond
          when (not enforce_spec) && Supports.simplify_baseline cond = `True ->
            { import with supports = None }
        | _ -> import
      in
      process_statements ~enforce_spec (Import import :: acc) rest
  | hd :: rest ->
      (* Other statement types - keep as-is *)
      process_statements ~enforce_spec (hd :: acc) rest

and rules_aux ~enforce_spec (rules : rule list) : rule list =
  (* First optimize each rule's nested statements recursively. Nested rule
     selectors are implicitly relative to the parent [&], so drop a redundant
     leading [& <combinator>] (CSS Nesting 1 sec. 2). *)
  let drop_nesting_prefix (stmt : statement) : statement =
    match stmt with
    | Rule nr ->
        Rule
          {
            nr with
            selector = Selector.drop_redundant_nesting_prefix nr.selector;
          }
    | other -> other
  in
  let with_optimized_nested =
    List.map
      (fun rule ->
        let rule =
          {
            rule with
            nested =
              statements ~enforce_spec rule.nested
              |> List.map drop_nesting_prefix;
          }
        in
        merge_lone_nested_rule rule)
      rules
  in
  (* Apply standard rule optimizations. Adjacent same-selector rules merge:
     [.x{a}] [.x{b}] -> [.x{a;b}], which is safe because cascade order within
     the merged block matches the source order of the originals.
     [combine_identical_rules] then groups same-declaration rules under a
     selector list ([.a, .b, .c{...}]). *)
  List.map single_rule_without_nested with_optimized_nested
  |> drop_shadowed_declarations |> drop_shadowed_rules |> merge_rules
  |> List.map finalize_rule_without_nested
  |> combine_identical_rules |> factor_common_declarations

(* CSS Animations 2 sec. 4.1: [@keyframes name] re-declaration overrides the
   earlier definition in source order. Drop earlier same-name keyframes; the
   later one wins. Vendor-prefixed [-webkit-] / [-moz-] variants are separate
   namespaces, so they are not dedup'd against the unprefixed form. *)
let drop_shadowed_keyframes (stmts : statement list) : statement list =
  let exists_later kind name tail =
    List.exists
      (fun stmt ->
        match (kind, stmt) with
        | `Plain, Keyframes (n, _) -> n = name
        | `Webkit, Webkit_keyframes (n, _) -> n = name
        | `Moz, Moz_keyframes (n, _) -> n = name
        | _ -> false)
      tail
  in
  let rec walk acc = function
    | [] -> List.rev acc
    | (Keyframes (name, _) as kf) :: rest ->
        if exists_later `Plain name rest then walk acc rest
        else walk (kf :: acc) rest
    | (Webkit_keyframes (name, _) as kf) :: rest ->
        if exists_later `Webkit name rest then walk acc rest
        else walk (kf :: acc) rest
    | (Moz_keyframes (name, _) as kf) :: rest ->
        if exists_later `Moz name rest then walk acc rest
        else walk (kf :: acc) rest
    | stmt :: rest -> walk (stmt :: acc) rest
  in
  walk [] stmts

(* CSS Cascade 5 sec. 6.4.2: when a named layer is declared multiple times the
   rules from all occurrences accumulate into the layer. Merge same-name blocks
   at the position of the FIRST NON-EMPTY occurrence so the merged content stays
   where the author placed the layer's first real declaration. Leading empty
   blocks ([@layer name {}]) stay in place so the [empty-named-layer ->
   Layer_decl] normalisation in [process_statements] still folds them into the
   order-only declaration. *)
let statements_top_level ~enforce_spec (stmts : statement list) : statement list
    =
  let optimize_merged_block = statements ~enforce_spec in
  statements ~enforce_spec stmts
  |> merge_consecutive_layers ~optimize_merged_block
  |> drop_redundant_layer_decls |> drop_shadowed_keyframes

let single_rule ?scope (rule : rule) : rule =
  let run () =
    {
      rule with
      declarations = deduplicate_declarations rule.declarations;
      nested = statements ~enforce_spec:false rule.nested;
    }
  in
  match scope with Some s -> with_scope s run | None -> run ()

let rules ?scope (rules : rule list) : rule list =
  let run () = rules_aux ~enforce_spec:false rules in
  match scope with Some s -> with_scope s run | None -> run ()

(** {1 Nesting Flattening} *)

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
  apply_to_statements stylesheet

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

(* CSS Properties and Values API 1 sec. 2: an [@property --name { syntax: ... }]
   declaration registers [name] with a typed CSS syntax, lifting later [--name:
   ...] uses out of the unregistered opaque-token-stream rule. Apply
   registrations in source order so a [@property] only affects uses that follow
   it; later registrations of the same name overwrite, matching how the browser
   registry resolves duplicate declarations. *)
let promote_registered_custom_properties (stmts : statement list) :
    statement list =
  let registry : (string, Variables.any_syntax) Hashtbl.t = Hashtbl.create 8 in
  let try_promote_with (type a) (syntax : a Variables.syntax) components =
    match syntax with
    | Variables.Color -> Properties.try_read_custom_color components
    | Variables.Length -> Properties.try_read_custom_length components
    | Variables.Length_percentage ->
        Properties.try_read_custom_length_percentage components
    | Variables.Number -> Properties.try_read_custom_number components
    | Variables.Percentage -> Properties.try_read_custom_percentage components
    | _ -> None
  in
  let promote_decl (decl : Declaration.declaration) : Declaration.declaration =
    match decl with
    | Declaration
        {
          property = Custom_property name;
          value = Custom_value { value = Tokens components; layer; meta };
          important;
        } -> (
        match Hashtbl.find_opt registry name with
        | None -> decl
        | Some (Variables.Syntax syntax) -> (
            match try_promote_with syntax components with
            | None -> decl
            | Some typed ->
                Declaration
                  {
                    property = Custom_property name;
                    value = Custom_value { value = typed; layer; meta };
                    important;
                  }))
    | _ -> decl
  in
  let rec walk_stmt (stmt : statement) : statement =
    match stmt with
    | Property pr ->
        Hashtbl.replace registry pr.name (Variables.Syntax pr.syntax);
        stmt
    | Rule r ->
        Rule
          {
            r with
            declarations = List.map promote_decl r.declarations;
            nested = List.map walk_stmt r.nested;
          }
    | Declarations decls -> Declarations (List.map promote_decl decls)
    | Media (c, b) -> Media (c, List.map walk_stmt b)
    | Container (n, c, b) -> Container (n, c, List.map walk_stmt b)
    | Supports (c, b) -> Supports (c, List.map walk_stmt b)
    | Layer (n, b) -> Layer (n, List.map walk_stmt b)
    | Origin (o, b) -> Origin (o, List.map walk_stmt b)
    | Scope (s, e, b) -> Scope (s, e, List.map walk_stmt b)
    | Starting_style b -> Starting_style (List.map walk_stmt b)
    | Moz_document (c, b) -> Moz_document (c, List.map walk_stmt b)
    | When (c, b) -> When (c, List.map walk_stmt b)
    | Else (c, b) -> Else (c, List.map walk_stmt b)
    | _ -> stmt
  in
  List.map walk_stmt stmts

(* Under closed-stylesheet scope the optimiser knows every [@position-try
   --name] rule defined in the sheet. A [position-try-fallbacks: --x, --y] entry
   whose name has no matching [@position-try] rule cannot match at runtime, so
   prune unknown [Name] arms. Keep the [Flip_*] tactics and any [Var]
   indirection untouched. When every arm gets pruned the whole declaration drops
   (the property becomes equivalent to its initial). *)
let prune_position_try_fallbacks (stylesheet : t) : t =
  match !current_scope with
  | `Fragment -> stylesheet
  | `Stylesheet ->
      let known : (string, unit) Hashtbl.t = Hashtbl.create 8 in
      let rec collect (stmt : statement) =
        match stmt with
        | Position_try (name, _) -> Hashtbl.replace known name ()
        | Rule rule -> List.iter collect rule.nested
        | Layer (_, b)
        | Media (_, b)
        | Container (_, _, b)
        | Supports (_, b)
        | Moz_document (_, b)
        | When (_, b)
        | Else (_, b)
        | Starting_style b
        | Origin (_, b)
        | Scope (_, _, b) ->
            List.iter collect b
        | _ -> ()
      in
      List.iter collect stylesheet;
      let rec prune_decl (decl : Declaration.declaration) :
          Declaration.declaration option =
        match decl with
        | Declaration
            {
              property = Position_try_fallbacks;
              value = (Fallbacks items : Properties.position_try_fallbacks);
              important;
            } -> (
            let kept =
              List.filter
                (function
                  | (Properties.Name s : Properties.position_try_fallback) ->
                      Hashtbl.mem known s
                  | _ -> true)
                items
            in
            match kept with
            | [] -> None
            | _ ->
                Some
                  (Declaration
                     {
                       property = Position_try_fallbacks;
                       value = Fallbacks kept;
                       important;
                     }))
        | Theme_guarded { var_name; decl } -> (
            match prune_decl decl with
            | None -> None
            | Some decl -> Some (Theme_guarded { var_name; decl }))
        | other -> Some other
      in
      let prune_decls = List.filter_map prune_decl in
      let rec walk (stmt : statement) : statement =
        match stmt with
        | Rule rule ->
            Rule
              {
                rule with
                declarations = prune_decls rule.declarations;
                nested = List.map walk rule.nested;
              }
        | Declarations decls -> Declarations (prune_decls decls)
        | Layer (n, b) -> Layer (n, List.map walk b)
        | Media (m, b) -> Media (m, List.map walk b)
        | Container (n, c, b) -> Container (n, c, List.map walk b)
        | Supports (c, b) -> Supports (c, List.map walk b)
        | Moz_document (c, b) -> Moz_document (c, List.map walk b)
        | When (c, b) -> When (c, List.map walk b)
        | Else (c, b) -> Else (c, List.map walk b)
        | Starting_style b -> Starting_style (List.map walk b)
        | Origin (o, b) -> Origin (o, List.map walk b)
        | Scope (s, e, b) -> Scope (s, e, List.map walk b)
        | Page (sel, decls) -> Page (sel, prune_decls decls)
        | Position_try (n, decls) -> Position_try (n, prune_decls decls)
        | Supports_condition (n, decls) ->
            Supports_condition (n, prune_decls decls)
        | other -> other
      in
      List.map walk stylesheet

let stylesheet ?scope ?(flatten_nesting = false) ?(enforce_spec = false)
    (stylesheet : t) : t =
  let run () =
    let stylesheet =
      if flatten_nesting then flatten_block stylesheet else stylesheet
    in
    (* [drop_invalid] and [drop_unknown_at_rules] run before the main
       optimisation passes so the empty rules they leave behind get picked up by
       [drop_empty_rules]. *)
    stylesheet |> promote_registered_custom_properties
    |> prune_position_try_fallbacks |> drop_invalid |> drop_unknown_at_rules
    |> statements_top_level ~enforce_spec
  in
  match scope with Some s -> with_scope s run | None -> run ()
