open Declaration
open Common
open Shorthand_common
open Shorthand_coverage

let registered = Ctx.registered

(* CSS Box 4 7.1: a 1/2/3/4-value box shorthand expands to four explicit sides.
   Authored shorthands stay as authored when optimise has no longhand to absorb;
   generated shorthands store the shortest arity so pretty optimise output does
   not expand [padding:0] into four sides. *)
let expand_box vs =
  match vs with
  | [ a ] -> Some (a, a, a, a)
  | [ a; b ] -> Some (a, b, a, b)
  | [ a; b; c ] -> Some (a, b, c, b)
  | [ a; b; c; d ] -> Some (a, b, c, d)
  | _ -> None

let collapse_box_by same = function
  | [ a; b; c; d ] when same a b && same b c && same c d -> [ a ]
  | [ a; b; c; d ] when same a c && same b d -> [ a; b ]
  | [ a; b; c; d ] when same b d -> [ a; b; c ]
  | [ a; b; c ] when same a b && same b c -> [ a ]
  | [ a; b; c ] when same a c -> [ a; b ]
  | [ a; b ] when same a b -> [ a ]
  | vs -> vs

(* Both sides are the same [length] type, so structural equality is the
   minified-equality test once lengths are canonical - no need to render and
   compare text. *)
let same_minified_length = Values.equal_length
let collapse_box_lengths vs = collapse_box_by same_minified_length vs

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
  | Declaration { property = Margin_top; value = v; important = i; _ }
    when i = important ->
      Some (v, right, bottom, left)
  | Declaration { property = Margin_right; value = v; important = i; _ }
    when i = important ->
      Some (top, v, bottom, left)
  | Declaration { property = Margin_bottom; value = v; important = i; _ }
    when i = important ->
      Some (top, right, v, left)
  | Declaration { property = Margin_left; value = v; important = i; _ }
    when i = important ->
      Some (top, right, bottom, v)
  | _ -> None

let absorb_padding_corner ~important ((top, right, bottom, left) : sides) d :
    sides option =
  match d with
  | Declaration { property = Padding_top; value = v; important = i; _ }
    when i = important ->
      Some (v, right, bottom, left)
  | Declaration { property = Padding_right; value = v; important = i; _ }
    when i = important ->
      Some (top, v, bottom, left)
  | Declaration { property = Padding_bottom; value = v; important = i; _ }
    when i = important ->
      Some (top, right, v, left)
  | Declaration { property = Padding_left; value = v; important = i; _ }
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
  (* Keep [rest] physically when nothing is absorbed: the caller treats an
     unchanged [rest] as "no merge" via [==], so a rebuilt-but-equal spine would
     force a needless rebuild of the shorthand. *)
  let rec loop sides acc absorbed = function
    | [] -> if absorbed then (sides, List.rev acc) else (sides, rest)
    | (i, d) :: tail when is_same_shorthand d ->
        if absorbed then (sides, List.rev_append acc ((i, d) :: tail))
        else (sides, rest)
    | (i, d) :: tail -> (
        match absorb sides d with
        | Some sides' -> loop sides' acc true tail
        | None -> loop sides ((i, d) :: acc) absorbed tail)
  in
  loop sides [] false rest

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
              covers_longhand shorthand_prop lh_prop
          | _ -> false)
        source

(* Fold subsequent margin/padding corner longhands into the preceding box
   shorthand. Tailwind / Lightning-CSS / cssnano all do this; the dead-code
   suite asserts it for [margin: 10px; margin-top: 20px] -> [margin: 20px 10px
   10px]. *)
(* Commit the merge only when every side ends up concrete; otherwise restore
   the original shorthand and leave its longhand tail in place. *)
let try_merge_box_shorthand ~original ~property ~vs ~important ~absorb
    ~is_same_shorthand rest =
  match expand_box vs with
  | None -> (original, rest)
  | Some sides -> (
      let ((top, right, bottom, left) as absorbed), rest' =
        absorb_box_longhands ~absorb ~is_same_shorthand sides rest
      in
      match sides_have_runtime_subst absorbed with
      | true -> (original, rest)
      | false ->
          if rest' == rest then (original, rest)
          else
            let value =
              preserve_list vs
                (collapse_box_lengths [ top; right; bottom; left ])
            in
            let merged = Declaration.v ~important property value in
            (* Absorbing a side into [margin: inherit] leaves the other three
               holding the keyword beside a concrete one. *)
            if Declaration.value_has_css_wide_mix merged then (original, rest)
            else (merged, rest'))

(* CSS Overflow 3 sec. 3.1: [overflow] is the [overflow-x overflow-y] shorthand.
   When the two longhands appear together with matching importance and neither
   side is later shadowed within the same block, fold them into [overflow] -
   single value when the two axes match, two values otherwise. *)
let combined_overflow v_x v_y : Properties.overflow =
  if Properties.equal_overflow v_x v_y then v_x else Overflow_pair (v_x, v_y)

let try_take_overflow_y ~important rest =
  let rec loop acc :
      (int * declaration) list ->
      (Properties.overflow * (int * declaration) list) option = function
    | [] -> None
    | (_, Declaration { property = Overflow_y; value = v_y; important = i'; _ })
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
    | (_, Declaration { property = Overflow_x; value = v_x; important = i'; _ })
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
    | ((idx, Declaration { property = Overflow_x; value = v_x; important; _ })
       as item)
      :: rest -> (
        match try_take_overflow_y ~important rest with
        | None -> go (item :: acc) rest
        | Some (v_y, rest') ->
            let merged =
              Declaration.v ~important Overflow (combined_overflow v_x v_y)
            in
            if Declaration.value_has_css_wide_mix merged then
              go (item :: acc) rest
            else go ((idx, merged) :: acc) rest')
    | ((idx, Declaration { property = Overflow_y; value = v_y; important; _ })
       as item)
      :: rest -> (
        match try_take_overflow_x ~important rest with
        | None -> go (item :: acc) rest
        | Some (v_x, rest') ->
            let merged =
              Declaration.v ~important Overflow (combined_overflow v_x v_y)
            in
            if Declaration.value_has_css_wide_mix merged then
              go (item :: acc) rest
            else go ((idx, merged) :: acc) rest')
    | d :: rest -> go (d :: acc) rest
  in
  preserve_list decls (go [] decls)

(* Compose 4 contiguous box-side longhands ([margin-*] or [padding-*]) into one
   shorthand. Runs before [merge_box_shorthand_longhands] so the absorption pass
   picks up stragglers. Requires all four sides in the next four positions (any
   order), matching importance, and no runtime-substitution leaves. *)
type box_side = Top | Right | Bottom | Left

let extract_margin_side :
    declaration -> (box_side * Values.length * bool) option = function
  | Declaration { property = Margin_top; value; important; _ } ->
      Some (Top, value, important)
  | Declaration { property = Margin_right; value; important; _ } ->
      Some (Right, value, important)
  | Declaration { property = Margin_bottom; value; important; _ } ->
      Some (Bottom, value, important)
  | Declaration { property = Margin_left; value; important; _ } ->
      Some (Left, value, important)
  | _ -> None

let extract_padding_side :
    declaration -> (box_side * Values.length * bool) option = function
  | Declaration { property = Padding_top; value; important; _ } ->
      Some (Top, value, important)
  | Declaration { property = Padding_right; value; important; _ } ->
      Some (Right, value, important)
  | Declaration { property = Padding_bottom; value; important; _ } ->
      Some (Bottom, value, important)
  | Declaration { property = Padding_left; value; important; _ } ->
      Some (Left, value, important)
  | _ -> None

(* CSS Position 3 sec. 3.1: [inset] is the [top right bottom left] shorthand.
   The longhand values are wrapped in a [length list] for grammar reasons but
   carry exactly one length per side. *)
let extract_inset_side : declaration -> (box_side * Values.length * bool) option
    = function
  | Declaration { property = Top; value = [ v ]; important; _ } ->
      Some (Top, v, important)
  | Declaration { property = Right; value = [ v ]; important; _ } ->
      Some (Right, v, important)
  | Declaration { property = Bottom; value = [ v ]; important; _ } ->
      Some (Bottom, v, important)
  | Declaration { property = Left; value = [ v ]; important; _ } ->
      Some (Left, v, important)
  | _ -> None

(* [border-radius] gives the horizontal radii before the [/] and the vertical
   ones after it, so a corner naming both axes has no slot in the box the four
   corners compose. Only a single-valued corner takes part. *)
let extract_border_radius_corner :
    declaration -> (box_side * Values.length * bool) option = function
  | Declaration
      { property = Border_top_left_radius; value = [ v ]; important; _ } ->
      Some (Top, v, important)
  | Declaration
      { property = Border_top_right_radius; value = [ v ]; important; _ } ->
      Some (Right, v, important)
  | Declaration
      { property = Border_bottom_right_radius; value = [ v ]; important; _ } ->
      Some (Bottom, v, important)
  | Declaration
      { property = Border_bottom_left_radius; value = [ v ]; important; _ } ->
      Some (Left, v, important)
  | _ -> None

(* An elliptical corner names a horizontal radius and a vertical one. CSS
   Backgrounds 3 (ED) sec. 4.1 writes the four horizontals, then [/], then the
   four verticals, so the four corners compose into one box per axis. *)
let extract_border_radius_ellipse :
    declaration -> (box_side * (Values.length * Values.length) * bool) option =
  function
  | Declaration
      { property = Border_top_left_radius; value = [ h; v ]; important; _ } ->
      Some (Top, (h, v), important)
  | Declaration
      { property = Border_top_right_radius; value = [ h; v ]; important; _ } ->
      Some (Right, (h, v), important)
  | Declaration
      { property = Border_bottom_right_radius; value = [ h; v ]; important; _ }
    ->
      Some (Bottom, (h, v), important)
  | Declaration
      { property = Border_bottom_left_radius; value = [ h; v ]; important; _ }
    ->
      Some (Left, (h, v), important)
  | _ -> None

(* CSS Scroll Snap 1 sec. 4.2 and 5.1: [scroll-padding] sets the four snapport
   insets and [scroll-margin] the four snap area outsets, each assigning its
   sides exactly as [padding] and [margin] do. *)
let extract_scroll_margin_side :
    declaration -> (box_side * Values.length * bool) option = function
  | Declaration { property = Scroll_margin_top; value; important; _ } ->
      Some (Top, value, important)
  | Declaration { property = Scroll_margin_right; value; important; _ } ->
      Some (Right, value, important)
  | Declaration { property = Scroll_margin_bottom; value; important; _ } ->
      Some (Bottom, value, important)
  | Declaration { property = Scroll_margin_left; value; important; _ } ->
      Some (Left, value, important)
  | _ -> None

let extract_scroll_padding_side :
    declaration -> (box_side * Values.length * bool) option = function
  | Declaration { property = Scroll_padding_top; value; important; _ } ->
      Some (Top, value, important)
  | Declaration { property = Scroll_padding_right; value; important; _ } ->
      Some (Right, value, important)
  | Declaration { property = Scroll_padding_bottom; value; important; _ } ->
      Some (Bottom, value, important)
  | Declaration { property = Scroll_padding_left; value; important; _ } ->
      Some (Left, value, important)
  | _ -> None

(* CSS Backgrounds 3 sec. 3.1 to 3.3: [border-color], [border-style] and
   [border-width] each set exactly the four matching side longhands. *)
let border_width_of :
    declaration -> (box_side * Properties.border_width * bool) option = function
  | Declaration { property = Border_top_width; value; important; _ } ->
      Some (Top, value, important)
  | Declaration { property = Border_right_width; value; important; _ } ->
      Some (Right, value, important)
  | Declaration { property = Border_bottom_width; value; important; _ } ->
      Some (Bottom, value, important)
  | Declaration { property = Border_left_width; value; important; _ } ->
      Some (Left, value, important)
  | _ -> None

let border_style_of :
    declaration -> (box_side * Properties.border_style * bool) option = function
  | Declaration { property = Border_top_style; value; important; _ } ->
      Some (Top, value, important)
  | Declaration { property = Border_right_style; value; important; _ } ->
      Some (Right, value, important)
  | Declaration { property = Border_bottom_style; value; important; _ } ->
      Some (Bottom, value, important)
  | Declaration { property = Border_left_style; value; important; _ } ->
      Some (Left, value, important)
  | _ -> None

let border_color_of : declaration -> (box_side * Values.color * bool) option =
  function
  | Declaration { property = Border_top_color; value; important; _ } ->
      Some (Top, value, important)
  | Declaration { property = Border_right_color; value; important; _ } ->
      Some (Right, value, important)
  | Declaration { property = Border_bottom_color; value; important; _ } ->
      Some (Bottom, value, important)
  | Declaration { property = Border_left_color; value; important; _ } ->
      Some (Left, value, important)
  | _ -> None

let build_margin_box ~important ~top ~right ~bottom ~left =
  Some
    (Declaration.v ~important Margin
       (collapse_box_lengths [ top; right; bottom; left ]))

let build_padding_box ~important ~top ~right ~bottom ~left =
  Some
    (Declaration.v ~important Padding
       (collapse_box_lengths [ top; right; bottom; left ]))

let build_inset_box ~important ~top ~right ~bottom ~left =
  Some
    (Declaration.v ~important Inset
       (collapse_box_lengths [ top; right; bottom; left ]))

let build_border_radius_box ~important ~top ~right ~bottom ~left =
  let lp v : Values.length_percentage = Length v in
  let horizontal =
    List.map lp (collapse_box_lengths [ top; right; bottom; left ])
  in
  Some
    (Declaration.v ~important Border_radius
       (Radius { horizontal; vertical = None }))

let build_border_radius_ellipse ~important ~top ~right ~bottom ~left =
  let lp v : Values.length_percentage = Length v in
  let axis f =
    List.map lp (collapse_box_lengths [ f top; f right; f bottom; f left ])
  in
  Some
    (Declaration.v ~important Border_radius
       (Radius { horizontal = axis fst; vertical = Some (axis snd) }))

let build_scroll_margin_box ~important ~top ~right ~bottom ~left =
  Some
    (Declaration.v ~important Scroll_margin
       (collapse_box_lengths [ top; right; bottom; left ]))

let build_scroll_padding_box ~important ~top ~right ~bottom ~left =
  Some
    (Declaration.v ~important Scroll_padding
       (collapse_box_lengths [ top; right; bottom; left ]))

(* Canonical values of one type, so the typed structural equality is the
   minified test, as it is for [same_minified_length]. *)
let same_border_width = Properties.equal_border_width
let same_border_style = Properties.equal_border_style

let build_border_width_box ~important ~top ~right ~bottom ~left =
  Some
    (Declaration.v ~important Border_width
       (collapse_box_by same_border_width [ top; right; bottom; left ]))

let build_border_style_box ~important ~top ~right ~bottom ~left =
  Some
    (Declaration.v ~important Border_style
       (collapse_box_by same_border_style [ top; right; bottom; left ]))

let build_border_color_box ~important ~top ~right ~bottom ~left =
  Some
    (Declaration.v ~important Border_color
       (collapse_box_by Values.equal_color [ top; right; bottom; left ]))

(* Whether a side value may move into the positional shorthand. A [var()] leaf
   substitutes an arbitrary token sequence, which in the shorthand would shift
   the sides that follow it; a registration pins the leaf to one value. *)
let foldable_length ~ctx (v : Values.length) =
  match v with
  | Var vr -> registered ctx vr.name
  | _ -> not (Values.length_has_runtime_subst v)

let foldable_length_strict (v : Values.length) =
  not (Values.length_has_runtime_subst v)

let foldable_border_width ~ctx (w : Properties.border_width) =
  match w with Var v -> registered ctx v.name | _ -> true

let foldable_border_style ~ctx (s : Properties.border_style) =
  match s with Var v -> registered ctx v.name | _ -> true

let foldable_border_color ~ctx (c : Values.color) =
  match c with Var v -> registered ctx v.name | _ -> true

(* Index-based: positions i..i+3 form a same-importance 4-side box. One call
   covers one family, and most positions start none of them, so the first
   declaration is tested before the other three are read. *)
let try_compose_box_at idx ~foldable ~extract ~build i =
  let n = Rule_index.length idx in
  if i + 3 >= n then None
  else if
    Rule_index.is_absorbed idx i
    || Rule_index.is_absorbed idx (i + 1)
    || Rule_index.is_absorbed idx (i + 2)
    || Rule_index.is_absorbed idx (i + 3)
  then None
  else if Option.is_none (extract (Rule_index.decl_at idx i)) then None
  else
    let d1 = Rule_index.decl_at idx i in
    let d2 = Rule_index.decl_at idx (i + 1) in
    let d3 = Rule_index.decl_at idx (i + 2) in
    let d4 = Rule_index.decl_at idx (i + 3) in
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
        let no_runtime = List.for_all (fun (_, v) -> foldable v) sides in
        if distinct && no_runtime then
          let find s = List.assoc s sides in
          build ~important:imp1 ~top:(find Top) ~right:(find Right)
            ~bottom:(find Bottom) ~left:(find Left)
        else None
    | _ -> None

(* Mixed-importance 4-side absorption: emit a non-important shorthand and then
   re-state each important side immediately after. The cascade picks the
   !important longhand over the shorthand regardless of order. *)
let box_split_emit ~build entries =
  let find s =
    let _, v, _, _ = List.find (fun (x, _, _, _) -> x = s) entries in
    v
  in
  let important_decls =
    List.filter_map (fun (_, _, imp, d) -> if imp then Some d else None) entries
  in
  Option.map
    (fun shorthand -> shorthand :: important_decls)
    (build ~important:false ~top:(find Top) ~right:(find Right)
       ~bottom:(find Bottom) ~left:(find Left))

let try_compose_box_split_at idx ~foldable ~extract ~build i =
  let n = Rule_index.length idx in
  if i + 3 >= n then None
  else if
    Rule_index.is_absorbed idx i
    || Rule_index.is_absorbed idx (i + 1)
    || Rule_index.is_absorbed idx (i + 2)
    || Rule_index.is_absorbed idx (i + 3)
  then None
  else if Option.is_none (extract (Rule_index.decl_at idx i)) then None
  else
    let d1 = Rule_index.decl_at idx i in
    let d2 = Rule_index.decl_at idx (i + 1) in
    let d3 = Rule_index.decl_at idx (i + 2) in
    let d4 = Rule_index.decl_at idx (i + 3) in
    match (extract d1, extract d2, extract d3, extract d4) with
    | ( Some (s1, v1, imp1),
        Some (s2, v2, imp2),
        Some (s3, v3, imp3),
        Some (s4, v4, imp4) ) ->
        let entries =
          [
            (s1, v1, imp1, d1);
            (s2, v2, imp2, d2);
            (s3, v3, imp3, d3);
            (s4, v4, imp4, d4);
          ]
        in
        let distinct =
          List.length (List.sort_uniq compare [ s1; s2; s3; s4 ]) = 4
        in
        let no_runtime =
          List.for_all (fun (_, v, _, _) -> foldable v) entries
        in
        let n_imp =
          List.length (List.filter (fun (_, _, imp, _) -> imp) entries)
        in
        if distinct && no_runtime && n_imp >= 1 && n_imp <= 2 then
          box_split_emit ~build entries
        else None
    | _ -> None

type box_outcome =
  | Single of declaration (* All 4 sides same importance: one shorthand. *)
  | Split of declaration list
(* Mixed: shorthand + re-stated important sides. *)

(* Every 4-side family, each paired with the guard its value type needs. The
   same-importance forms come first: a mixed-importance split re-states the
   important sides, so it is the fallback. *)
(* CSS Grid 2 sec. 8.4: [grid-area] names four lines, in the order row-start,
   column-start, row-end, column-end. They are four distinct slots of one
   contiguous run, which is what the box walk tests, so the four [box_side] tags
   stand for the four lines in that order. A substituted line can stand for a
   whole [<start> / <end>], so only a resolved one takes part. *)
let foldable_grid_line : Properties.grid_line -> bool = function
  | Var _ -> false
  | _ -> true

let extract_grid_area_side :
    declaration -> (box_side * Properties.grid_line * bool) option = function
  | Declaration { property = Grid_row_start; value; important; _ } ->
      Some (Top, value, important)
  | Declaration { property = Grid_column_start; value; important; _ } ->
      Some (Right, value, important)
  | Declaration { property = Grid_row_end; value; important; _ } ->
      Some (Bottom, value, important)
  | Declaration { property = Grid_column_end; value; important; _ } ->
      Some (Left, value, important)
  | _ -> None

let build_grid_area ~important ~top ~right ~bottom ~left =
  Some
    (Declaration.v ~important Grid_area
       (Lines
          {
            row_start = top;
            column_start = right;
            row_end = bottom;
            column_end = left;
          }
         : Properties.grid_area))

let box_composers ~ctx idx =
  let try_same foldable extract build i =
    Option.map
      (fun sh -> Single sh)
      (try_compose_box_at idx ~foldable ~extract ~build i)
  in
  let try_split foldable extract build i =
    Option.map
      (fun ds -> Split ds)
      (try_compose_box_split_at idx ~foldable ~extract ~build i)
  in
  let len = foldable_length ~ctx in
  let strict = foldable_length_strict in
  let width = foldable_border_width ~ctx in
  let style = foldable_border_style ~ctx in
  let color = foldable_border_color ~ctx in
  [
    try_same len extract_margin_side build_margin_box;
    try_same len extract_padding_side build_padding_box;
    try_same len extract_inset_side build_inset_box;
    try_same len extract_border_radius_corner build_border_radius_box;
    try_same
      (fun (h, v) -> len h && len v)
      extract_border_radius_ellipse build_border_radius_ellipse;
    try_same len extract_scroll_margin_side build_scroll_margin_box;
    try_same len extract_scroll_padding_side build_scroll_padding_box;
    try_same width border_width_of build_border_width_box;
    try_same style border_style_of build_border_style_box;
    try_same color border_color_of build_border_color_box;
    try_same foldable_grid_line extract_grid_area_side build_grid_area;
    try_split strict extract_margin_side build_margin_box;
    try_split strict extract_padding_side build_padding_box;
    try_split strict extract_inset_side build_inset_box;
    try_split strict extract_border_radius_corner build_border_radius_box;
    try_split strict extract_scroll_margin_side build_scroll_margin_box;
    try_split strict extract_scroll_padding_side build_scroll_padding_box;
    try_split width border_width_of build_border_width_box;
    try_split style border_style_of build_border_style_box;
    try_split color border_color_of build_border_color_box;
  ]

let compose_box_via_index ~ctx idx =
  let n = Rule_index.length idx in
  let composers = box_composers ~ctx idx in
  let try_one i =
    let rec loop = function
      | [] -> None
      | f :: rest -> ( match f i with Some _ as r -> r | None -> loop rest)
    in
    loop composers
  in
  let i = ref 0 in
  while !i < n do
    if Rule_index.is_absorbed idx !i then incr i
    else
      match try_one !i with
      | Some (Single shorthand) ->
          if
            Rule_index.absorb idx ~at:!i
              ~absorbed:[ !i; !i + 1; !i + 2; !i + 3 ]
              ~shorthand
          then i := !i + 4
          else incr i
      | Some (Split decls) ->
          if
            Rule_index.splice idx ~at:!i
              ~absorbed:[ !i; !i + 1; !i + 2; !i + 3 ]
              ~new_decls:decls
          then i := !i + 4
          else incr i
      | None -> incr i
  done

let merge_box_shorthand_longhands source decls =
  (* [try_merge_box_shorthand] returns the original declaration when it absorbs
     nothing, so keep the original tuple then ([==]) rather than re-pairing it
     with its index - the head stays physically shared on a no-op. *)
  let rec go acc = function
    | [] -> List.rev acc
    | ((idx, (Declaration { property = Margin; value = vs; important; _ } as d))
       as item)
      :: rest
      when not (box_shorthand_had_prior_longhand source idx d) ->
        let merged, rest =
          try_merge_box_shorthand ~original:d ~property:Margin ~vs ~important
            ~absorb:(absorb_margin_corner ~important)
            ~is_same_shorthand:is_margin_shorthand rest
        in
        let head = if merged == d then item else (idx, merged) in
        go (head :: acc) rest
    | ((idx, (Declaration { property = Padding; value = vs; important; _ } as d))
       as item)
      :: rest
      when not (box_shorthand_had_prior_longhand source idx d) ->
        let merged, rest =
          try_merge_box_shorthand ~original:d ~property:Padding ~vs ~important
            ~absorb:(absorb_padding_corner ~important)
            ~is_same_shorthand:is_padding_shorthand rest
        in
        let head = if merged == d then item else (idx, merged) in
        go (head :: acc) rest
    | d :: rest -> go (d :: acc) rest
  in
  preserve_list decls (go [] decls)
