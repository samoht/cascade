(** Declaration deduplication and shorthand composition. *)

open Declaration
open Common

let preserve_list = List.preserve
let scope = Ctx.scope
let registered = Ctx.registered
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
let covers_longhand : type a b.
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
  (* CSS Logical 1 sec. 4.2: [border-block] / [border-inline] reset their two
     flow-relative middle-tier longhands. *)
  | Border_block, Border_block_start -> true
  | Border_block, Border_block_end -> true
  | Border_inline, Border_inline_start -> true
  | Border_inline, Border_inline_end -> true
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
  (* CSS Fonts 4 sec. 2.7: the [font] shorthand resets [font-style / -weight /
     -stretch / -size / line-height / -family] to the given values and every
     other [font]-subproperty to its initial - including the [font-variant-*]
     longhands, [font-variation-settings], [font-feature-settings],
     [font-size-adjust], [font-kerning], and [font-optical-sizing]. *)
  | Font, Font_style -> true
  | Font, Font_weight -> true
  | Font, Font_stretch -> true
  | Font, Font_size -> true
  | Font, Line_height -> true
  | Font, Font_family -> true
  | Font, Font_variant_ligatures -> true
  | Font, Caps -> true
  | Font, Numeric -> true
  | Font, Font_variant_position -> true
  | Font, East_asian -> true
  | Font, Font_variant_emoji -> true
  | Font, Font_variation_settings -> true
  | Font, Font_feature_settings -> true
  | Font, Font_size_adjust -> true
  | Font, Font_kerning -> true
  | Font, Font_optical_sizing -> true
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
   cover themselves by exact name and have no typed shorthand coverage; custom
   properties are exempt from the [all] reset. *)
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
      Declaration.same_property covering covered
      || covers_longhand covering_p covered_p
  | _ -> false

let display_value_is_vendor : Properties.display -> bool = function
  | Webkit_flex | Webkit_inline_flex | Ms_flexbox | Webkit_box | Moz_box
  | Moz_inline_box ->
      true
  | _ -> false

let background_image_is_vendor : Properties.background_image -> bool = function
  | Webkit_linear_gradient _ | Webkit_repeating_linear_gradient _
  | Webkit_radial_gradient _ | Webkit_repeating_radial_gradient _
  | Moz_linear_gradient _ | Moz_repeating_linear_gradient _
  | Moz_radial_gradient _ | Moz_repeating_radial_gradient _
  | O_linear_gradient _ | O_repeating_linear_gradient _ | O_radial_gradient _
  | O_repeating_radial_gradient _ | Webkit_image_set _ | Webkit_gradient _ ->
      true
  | _ -> false

(* A value whose rendering begins with a CSS vendor prefix (-webkit-, -moz-,
   -ms-, -o-). Only [display] keywords and [background-image] gradients render
   that way, so a structural match on those value types is exhaustive - no
   rendering needed. Used to preserve legacy fallbacks like
   [display:-webkit-box;display:flex]: the spec value cascades over the prefixed
   value in modern browsers, but old browsers only understand the prefixed
   spelling, so dropping the earlier declaration removes a real browser-compat
   fallback. *)
let rec value_is_vendor_prefixed decl =
  match decl with
  | Theme_guarded { decl; _ } -> value_is_vendor_prefixed decl
  | Declaration { property = Display; value; _ } ->
      display_value_is_vendor value
  | Declaration { property = Background_image; value; _ } ->
      List.exists background_image_is_vendor value
  | Declaration { property = Webkit_mask_image; value; _ } ->
      background_image_is_vendor value
  | Declaration { property = Mask_image; value; _ } ->
      background_image_is_vendor value
  | Declaration { property = Border_image_source; value; _ } ->
      background_image_is_vendor value
  | Declaration _ -> false

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
let same_minified_length (a : Values.length) (b : Values.length) = a = b
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
            (Declaration.v ~important property value, rest'))

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
    | ((idx, Declaration { property = Overflow_x; value = v_x; important }) as
       item)
      :: rest -> (
        match try_take_overflow_y ~important rest with
        | None -> go (item :: acc) rest
        | Some (v_y, rest') ->
            let merged =
              Declaration.v ~important Overflow (combined_overflow v_x v_y)
            in
            go ((idx, merged) :: acc) rest')
    | ((idx, Declaration { property = Overflow_y; value = v_y; important }) as
       item)
      :: rest -> (
        match try_take_overflow_x ~important rest with
        | None -> go (item :: acc) rest
        | Some (v_x, rest') ->
            let merged =
              Declaration.v ~important Overflow (combined_overflow v_x v_y)
            in
            go ((idx, merged) :: acc) rest')
    | d :: rest -> go (d :: acc) rest
  in
  preserve_list decls (go [] decls)

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

let try_compose_box ~ctx ~extract ~build = function
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
          (* A side with runtime substitution can only fold into the box
             shorthand when it is a registered ([@property]) [var()]: the
             shorthand makes the four sides share fate, so an unregistered var
             that goes invalid at computed-value time would poison all four
             rather than just its own side. *)
          let subst_safe (_, v) =
            match (v : Values.length) with
            | Var vr -> registered ctx vr.name
            | _ -> not (Values.length_has_runtime_subst v)
          in
          let no_runtime = List.for_all subst_safe sides in
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

(* When the four box longhands are all present but importance is mixed, emit a
   non-important shorthand carrying every side's value and re-state the
   [!important] side(s) after it. An [!important] longhand wins over the
   shorthand for its side regardless of order, while the non-important sides
   take the shorthand's value, so the cascade is preserved. Worthwhile only when
   at most two sides are important (otherwise the longhand count is not
   reduced). *)
let try_compose_box_important_split ~extract ~build = function
  | (idx1, d1) :: (idx2, d2) :: (idx3, d3) :: (idx4, d4) :: rest -> (
      match (extract d1, extract d2, extract d3, extract d4) with
      | ( Some (s1, v1, i1),
          Some (s2, v2, i2),
          Some (s3, v3, i3),
          Some (s4, v4, i4) ) ->
          let sides =
            [
              (s1, v1, i1, (idx1, d1));
              (s2, v2, i2, (idx2, d2));
              (s3, v3, i3, (idx3, d3));
              (s4, v4, i4, (idx4, d4));
            ]
          in
          let distinct =
            List.length (List.sort_uniq compare [ s1; s2; s3; s4 ]) = 4
          in
          let no_runtime =
            List.for_all
              (fun (_, v, _, _) -> not (Values.length_has_runtime_subst v))
              sides
          in
          let important_pairs =
            List.filter_map
              (fun (_, _, i, p) -> if i then Some p else None)
              sides
          in
          let n_imp = List.length important_pairs in
          if distinct && no_runtime && n_imp >= 1 && n_imp <= 2 then
            let find s =
              let _, v, _, _ = List.find (fun (x, _, _, _) -> x = s) sides in
              v
            in
            let shorthand =
              build ~important:false ~top:(find Top) ~right:(find Right)
                ~bottom:(find Bottom) ~left:(find Left)
            in
            Some ((idx1, shorthand), important_pairs @ rest)
          else None
      | _ -> None)
  | _ -> None

let build_margin_box ~important ~top ~right ~bottom ~left =
  Declaration.v ~important Margin
    (collapse_box_lengths [ top; right; bottom; left ])

let build_padding_box ~important ~top ~right ~bottom ~left =
  Declaration.v ~important Padding
    (collapse_box_lengths [ top; right; bottom; left ])

let build_inset_box ~important ~top ~right ~bottom ~left =
  Declaration.v ~important Inset
    (collapse_box_lengths [ top; right; bottom; left ])

let build_border_radius_box ~important ~top ~right ~bottom ~left =
  let lp v : Values.length_percentage = Length v in
  let horizontal =
    List.map lp (collapse_box_lengths [ top; right; bottom; left ])
  in
  Declaration.v ~important Border_radius
    (Radius { horizontal; vertical = None })

let compose_box_shorthands ~ctx decls =
  let composers =
    [
      try_compose_box ~ctx ~extract:extract_margin_side ~build:build_margin_box;
      try_compose_box ~ctx ~extract:extract_padding_side
        ~build:build_padding_box;
      try_compose_box ~ctx ~extract:extract_inset_side ~build:build_inset_box;
      try_compose_box ~ctx ~extract:extract_border_radius_corner
        ~build:build_border_radius_box;
      try_compose_box_important_split ~extract:extract_margin_side
        ~build:build_margin_box;
      try_compose_box_important_split ~extract:extract_padding_side
        ~build:build_padding_box;
      try_compose_box_important_split ~extract:extract_inset_side
        ~build:build_inset_box;
      try_compose_box_important_split ~extract:extract_border_radius_corner
        ~build:build_border_radius_box;
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
            Declaration.v ~important:imp1 Gap
              (Lengths
                 { row_gap = Some (find Row); column_gap = Some (find Column) })
          in
          Some ((idx, merged), rest)
      | _ -> None)
  | _ -> None

(* Compose [<base>-inline] / [<base>-block] from the matching [-start] / [-end]
   longhands. Both longhands carry exactly one length value (wrapped in a
   1-element list for [inset-*] grammar reasons). The result is a [length list]
   payload: [v] when both sides match, [v_start; v_end] otherwise. *)
type axis_side = Start | End

let try_compose_axis_pair ~extract ~build = function
  | (idx, d1) :: (_, d2) :: rest -> (
      match (extract d1, extract d2) with
      | Some (s1, v1, imp1), Some (s2, v2, imp2)
        when imp1 = imp2 && s1 <> s2
             && (not (Values.length_has_runtime_subst v1))
             && not (Values.length_has_runtime_subst v2) ->
          let pair = [ (s1, v1); (s2, v2) ] in
          let v_start = List.assoc Start pair in
          let v_end = List.assoc End pair in
          let value =
            if v_start = v_end then [ v_start ] else [ v_start; v_end ]
          in
          Some ((idx, build ~important:imp1 ~value), rest)
      | _ -> None)
  | _ -> None

let extract_margin_inline_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Margin_inline_start; value; important } ->
      Some (Start, value, important)
  | Declaration { property = Margin_inline_end; value; important } ->
      Some (End, value, important)
  | _ -> None

let extract_margin_block_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Margin_block_start; value; important } ->
      Some (Start, value, important)
  | Declaration { property = Margin_block_end; value; important } ->
      Some (End, value, important)
  | _ -> None

let extract_padding_inline_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Padding_inline_start; value; important } ->
      Some (Start, value, important)
  | Declaration { property = Padding_inline_end; value; important } ->
      Some (End, value, important)
  | _ -> None

let extract_padding_block_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Padding_block_start; value; important } ->
      Some (Start, value, important)
  | Declaration { property = Padding_block_end; value; important } ->
      Some (End, value, important)
  | _ -> None

let extract_inset_inline_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Inset_inline_start; value = [ v ]; important } ->
      Some (Start, v, important)
  | Declaration { property = Inset_inline_end; value = [ v ]; important } ->
      Some (End, v, important)
  | _ -> None

let extract_inset_block_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Inset_block_start; value = [ v ]; important } ->
      Some (Start, v, important)
  | Declaration { property = Inset_block_end; value = [ v ]; important } ->
      Some (End, v, important)
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
        Declaration.v ~important:i1 Place_items
          (Align_justify (a, j) : Properties.place_items)
      in
      Some ((idx, merged), rest)
  | (idx, Declaration { property = Justify_items; value = j; important = i1 })
    :: (_, Declaration { property = Align_items; value = a; important = i2 })
    :: rest
    when i1 = i2 ->
      let merged =
        Declaration.v ~important:i1 Place_items
          (Align_justify (a, j) : Properties.place_items)
      in
      Some ((idx, merged), rest)
  | _ -> None

let try_compose_place_content = function
  | (idx, Declaration { property = Align_content; value = a; important = i1 })
    :: (_, Declaration { property = Justify_content; value = j; important = i2 })
    :: rest
    when i1 = i2 ->
      let merged =
        Declaration.v ~important:i1 Place_content
          (Align_justify (a, j) : Properties.place_content)
      in
      Some ((idx, merged), rest)
  | (idx, Declaration { property = Justify_content; value = j; important = i1 })
    :: (_, Declaration { property = Align_content; value = a; important = i2 })
    :: rest
    when i1 = i2 ->
      let merged =
        Declaration.v ~important:i1 Place_content
          (Align_justify (a, j) : Properties.place_content)
      in
      Some ((idx, merged), rest)
  | _ -> None

let try_compose_place_self = function
  | (idx, Declaration { property = Align_self; value = a; important = i1 })
    :: (_, Declaration { property = Justify_self; value = j; important = i2 })
    :: rest
    when i1 = i2 ->
      let merged = Declaration.v ~important:i1 Place_self (a, j) in
      Some ((idx, merged), rest)
  | (idx, Declaration { property = Justify_self; value = j; important = i1 })
    :: (_, Declaration { property = Align_self; value = a; important = i2 })
    :: rest
    when i1 = i2 ->
      let merged = Declaration.v ~important:i1 Place_self (a, j) in
      Some ((idx, merged), rest)
  | _ -> None

let compose_pair_shorthands decls =
  let axis property extract decls =
    let build ~important ~value = Declaration.v ~important property value in
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
              Declaration.v ~important:(is_important d1) Outline
                (Shorthand { width; style; color })
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

let try_compose_font (indexed_decls : (int * Declaration.declaration) list) :
    ((int * Declaration.declaration) * (int * Declaration.declaration) list)
    option =
  match take_first_n 5 indexed_decls with
  | None -> Option.None
  | Some (five, rest) -> (
      let raw_decls = List.map snd five in
      if
        (not (List.for_all is_font_longhand raw_decls))
        || not (same_importance raw_decls)
      then Option.None
      else
        match render_font_shorthand raw_decls with
        | Some font_value ->
            let idx = fst (List.hd five) in
            let merged =
              Declaration.v
                ~important:(is_important (List.hd raw_decls))
                Font font_value
            in
            Some ((idx, merged), rest)
        | None -> Option.None)

let compose_font_shorthand decls =
  let rec go acc decls =
    match (decls, try_compose_font decls) with
    | [], _ -> List.rev acc
    | _, Some (merged, rest) -> go (merged :: acc) rest
    | hd :: rest, None -> go (hd :: acc) rest
  in
  go [] decls

(* The [font] shorthand resets the [font-variant-*] / [font-variation-settings]
   / [font-feature-settings] / [font-size-adjust] / [font-kerning] /
   [font-optical-sizing] subproperties to their initials. When such a reset
   declaration precedes a run of [font] longhands that [compose_font_shorthand]
   will fold (the run carries the mandatory [font-size] and [font-family]), move
   it after the run so the synthesised [font] does not clobber it - the same
   pattern as [reorder_border_image_before_border]. *)
let reorder_font_resets_before_font decls =
  let prop d = property_name (snd d) in
  let is_font_reset d =
    match prop d with
    | "font-variant-ligatures" | "font-variant-caps" | "font-variant-numeric"
    | "font-variant-position" | "font-variant-east-asian" | "font-variant-emoji"
    | "font-variation-settings" | "font-feature-settings" | "font-size-adjust"
    | "font-kerning" | "font-optical-sizing" ->
        true
    | _ -> false
  in
  let is_font_longhand d =
    match prop d with
    | "font-style" | "font-weight" | "font-stretch" | "font-size"
    | "line-height" | "font-family" ->
        true
    | _ -> false
  in
  let rec span pred acc = function
    | d :: rest when pred d -> span pred (d :: acc) rest
    | rest -> (List.rev acc, rest)
  in
  let rec go acc = function
    | [] -> List.rev acc
    | d :: _ as l when is_font_reset d ->
        let reset_block, rest1 = span is_font_reset [] l in
        let long_block, rest2 = span is_font_longhand [] rest1 in
        let has p = List.exists (fun d -> String.equal (prop d) p) long_block in
        if has "font-size" && has "font-family" then
          go (List.rev_append (long_block @ reset_block) acc) rest2
        else go (List.rev_append reset_block acc) rest1
    | d :: rest -> go (d :: acc) rest
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
        Declaration.v ~important:(is_important d1) List_style
          (render_list_style [ d1; d2; d3 ])
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

let flex_grow_of : declaration -> Properties.flex_factor option = function
  | Declaration { property = Flex_grow; value = (Number _ | Var _) as v; _ } ->
      Some v
  | _ -> None

let flex_shrink_of : declaration -> Properties.flex_factor option = function
  | Declaration { property = Flex_shrink; value = (Number _ | Var _) as v; _ }
    ->
      Some v
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
                Declaration.v ~important:(is_important d1) Flex (Full (g, s, b))
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
                Declaration.v ~important:(is_important d1) Text_decoration
                  (Shorthand { lines; style; color; thickness = None })
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
  Declaration.v ~important Border
    (Shorthand { width = Some width; style = Some style; color = Some color })

(* [border] / [border-<edge>] disambiguate width/style/color by type, so a
   [var()] (or other runtime substitution) in a longhand cannot be safely folded
   into the shorthand: the substituted tokens might re-assign to a different
   component, and one bad substitution invalidates the whole shorthand rather
   than the single longhand. Positional same-type shorthands (padding, ...) are
   exempt. *)
let has_runtime_substitution d = Variables.vars_of_declarations [ d ] <> []

let try_compose_border (indexed_decls : (int * Declaration.declaration) list) :
    ((int * Declaration.declaration) * (int * Declaration.declaration) list)
    option =
  match take_first_n 12 indexed_decls with
  | None -> Option.None
  | Some twelve -> (
      let twelve, rest = twelve in
      let raw_decls = List.map snd twelve in
      if not (same_importance raw_decls) then Option.None
      else if List.exists has_runtime_substitution raw_decls then Option.None
      else
        match border_parts_of raw_decls with
        | None -> Option.None
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
let try_compose_border_whole ~ctx indexed_decls =
  match take_first_n 3 indexed_decls with
  | None -> Option.None
  | Some (three, rest) -> (
      let raw = List.map snd three in
      if not (same_importance raw) then Option.None
      else
        let width : Properties.border_width option ref = ref Option.None in
        let style : Properties.border_style option ref = ref Option.None in
        let color : Values.color option ref = ref Option.None in
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
        (* CSS Variables 1 sec. 3: a [var()] invalid at computed-value time
           poisons its whole declaration. Folding [var()] longhands into the
           shorthand widens that blast radius from one longhand to the whole
           [border], so it is only safe when the referenced custom property is
           registered ([@property] with an initial-value) and therefore never
           invalid at computed-value time. *)
        let foldable_width (w : Properties.border_width) =
          match w with Var v -> registered ctx v.name | _ -> true
        in
        let foldable_style (s : Properties.border_style) =
          match s with Var v -> registered ctx v.name | _ -> true
        in
        let foldable_color (c : Values.color) =
          match c with Var v -> registered ctx v.name | _ -> true
        in
        match (!width, !style, !color) with
        | Some width, Some style, Some color
          when foldable_width width && foldable_style style
               && foldable_color color ->
            let merged =
              Declaration.v
                ~important:(is_important (List.hd raw))
                Border
                (Shorthand
                   {
                     width = Some width;
                     style = Some style;
                     color = Some color;
                   })
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
let is_border_image_decl = function
  | Declaration { property = Border_image; _ }
  | Declaration { property = Border_image_source; _ }
  | Declaration { property = Border_image_slice; _ }
  | Declaration { property = Border_image_width; _ }
  | Declaration { property = Border_image_outset; _ }
  | Declaration { property = Border_image_repeat; _ } ->
      true
  | _ -> false

let is_border_image_longhand d = is_border_image_decl (snd d)

let is_border_width_decl d =
  match snd d with
  | Declaration { property = Border_width; _ } -> true
  | _ -> false

let is_border_style_decl d =
  match snd d with
  | Declaration { property = Border_style; _ } -> true
  | _ -> false

let is_border_color_decl d =
  match snd d with
  | Declaration { property = Border_color; _ } -> true
  | _ -> false

let reorder_border_image_before_border decls =
  let is_border_longhand d =
    is_border_width_decl d || is_border_style_decl d || is_border_color_decl d
  in
  let rec span pred acc = function
    | d :: rest when pred d -> span pred (d :: acc) rest
    | rest -> (List.rev acc, rest)
  in
  let rec go acc = function
    | [] -> List.rev acc
    | d :: _ as l when is_border_image_longhand d ->
        let img_block, rest1 = span is_border_image_longhand [] l in
        let long_block, rest2 = span is_border_longhand [] rest1 in
        if
          List.exists is_border_width_decl long_block
          && List.exists is_border_style_decl long_block
          && List.exists is_border_color_decl long_block
        then go (List.rev_append (long_block @ img_block) acc) rest2
        else go (List.rev_append img_block acc) rest1
    | d :: rest -> go (d :: acc) rest
  in
  go [] decls

let compose_border_whole_shorthand ~ctx decls =
  (* [border] resets [border-image] to its initial, so the synthesised shorthand
     is only safe when it ends up before every [border-image] declaration.
     Compose in place while no [border-image] has been seen earlier in the rule;
     once one is, leave the longhands alone (the reorder-before-border-image
     case is handled separately). *)
  let rec go ~seen_border_image acc decls =
    match try_compose_border_whole ~ctx decls with
    | Some (merged, rest) when not seen_border_image ->
        go ~seen_border_image (merged :: acc) rest
    | _ -> (
        match decls with
        | [] -> List.rev acc
        | ((_, d) as hd) :: rest ->
            let seen_border_image =
              seen_border_image || is_border_image_decl d
            in
            go ~seen_border_image (hd :: acc) rest)
  in
  go ~seen_border_image:false [] decls

(* CSS Backgrounds 3: the [border] shorthand resets [border-image] to its
   initial. A [border-image*] declaration is therefore dead when a later
   [border] shorthand of at least equal importance resets it and no later
   [border-image*] re-establishes it. Intra-block source order is fixed
   regardless of any surrounding CSS, so the drop is safe in every scope. *)
let drop_bimg_shadowed_by_border kept =
  let is_bimg (_, d) = is_border_image_decl d in
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

(* CSS Backgrounds 3 sec. 6.1: compose [border-image] from a contiguous run of
   its longhands ([source] / [slice] [/ width [/ outset]] / [repeat]). The
   longhands are unknown properties, so the shorthand value is rebuilt from
   their text and re-parsed. The shorthand resets any longhand the run omits, so
   this is closed-world ([`Stylesheet]) only. *)
let is_border_image_longhand_decl = function
  | Declaration { property = Border_image_source; _ }
  | Declaration { property = Border_image_slice; _ }
  | Declaration { property = Border_image_width; _ }
  | Declaration { property = Border_image_outset; _ }
  | Declaration { property = Border_image_repeat; _ } ->
      true
  | _ -> false

let span_border_image_run =
  let is_bi d = is_border_image_longhand_decl (snd d) in
  let rec span acc = function
    | d :: rest when is_bi d -> span (d :: acc) rest
    | rest -> (List.rev acc, rest)
  in
  span []

let border_image_run_can_compose run ~foldable ~slice ~width ~outset =
  let need_slice =
    (Option.is_some width || Option.is_some outset) && Option.is_none slice
  in
  List.length run >= 2
  && same_importance (List.map snd run)
  && foldable && not need_slice

let border_image_shorthand run ~source ~slice ~width ~outset ~repeat =
  Declaration.v
    ~important:(is_important (snd (List.hd run)))
    Border_image
    { source; slice; width; outset; repeat; mode = None }

let record_border_image_longhand
    ~(source : Properties.background_image option ref)
    ~(slice : Properties.border_image_slice option ref)
    ~(width : Properties.border_image_width_item list option ref)
    ~(outset : Properties.border_image_outset_item list option ref)
    ~(repeat : Properties.border_image_repeat_keyword list option ref) ~foldable
    ((_, d) : int * declaration) =
  match d with
  | Declaration { property = Border_image_source; value; _ } ->
      source := Some value
  | Declaration { property = Border_image_slice; value; _ } ->
      slice := Some value
  | Declaration { property = Border_image_width; value = Widths l; _ } ->
      width := Some l
  | Declaration { property = Border_image_width; _ } -> foldable := false
  | Declaration { property = Border_image_outset; value = Outsets l; _ } ->
      outset := Some l
  | Declaration { property = Border_image_outset; _ } -> foldable := false
  | Declaration { property = Border_image_repeat; value = Repeats l; _ } ->
      repeat := Some l
  | Declaration { property = Border_image_repeat; _ } -> foldable := false
  | _ -> ()

let compose_border_image_run (run : (int * Declaration.declaration) list) :
    (int * Declaration.declaration) option =
  let source : Properties.background_image option ref = ref Option.None in
  let slice : Properties.border_image_slice option ref = ref Option.None in
  let width : Properties.border_image_width_item list option ref =
    ref Option.None
  in
  let outset : Properties.border_image_outset_item list option ref =
    ref Option.None
  in
  let repeat : Properties.border_image_repeat_keyword list option ref =
    ref Option.None
  in
  (* A CSS-wide keyword or [var()] in any longhand cannot be folded into the
     [border_image] record, which holds plain component values. *)
  let foldable = ref true in
  List.iter
    (record_border_image_longhand ~source ~slice ~width ~outset ~repeat
       ~foldable)
    run;
  if
    not
      (border_image_run_can_compose run ~foldable:!foldable ~slice:!slice
         ~width:!width ~outset:!outset)
  then Option.None
  else
    let merged =
      border_image_shorthand run ~source:!source ~slice:!slice ~width:!width
        ~outset:!outset ~repeat:!repeat
    in
    Some (fst (List.hd run), merged)

let compose_border_image_shorthand ~ctx decls =
  if scope ctx <> `Stylesheet then decls
  else
    let is_bi d = is_border_image_longhand_decl (snd d) in
    let rec go acc = function
      | [] -> List.rev acc
      | d :: _ as l when is_bi d -> (
          let run, rest = span_border_image_run l in
          match compose_border_image_run run with
          | Some merged -> go (merged :: acc) rest
          | None -> go (List.rev_append run acc) rest)
      | d :: rest -> go (d :: acc) rest
    in
    go [] decls

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

type bg_part =
  Properties.background_shorthand -> Properties.background_shorthand

type bg_updater = declaration -> bg_part option

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

let take_contiguous_background
    (indexed_decls : (int * Declaration.declaration) list) :
    (int * (Declaration.declaration * bg_part) list) option
    * (int * Declaration.declaration) list =
  let rec aux acc = function
    | (i, d) :: rest -> (
        match background_part_of d with
        | Some f -> aux ((d, f) :: acc) rest
        | None -> (List.rev acc, (i, d) :: rest))
    | [] -> (List.rev acc, [])
  in
  match indexed_decls with
  | [] -> (Option.None, [])
  | (idx, _) :: _ -> (
      let parts, rest = aux [] indexed_decls in
      match parts with
      | [] -> (Option.None, indexed_decls)
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

let try_compose_background ~ctx
    (indexed_decls : (int * Declaration.declaration) list) :
    ((int * Declaration.declaration) * (int * Declaration.declaration) list)
    option =
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
          match scope ctx with
          | `Stylesheet -> true
          | `Fragment -> background_run_is_reset_closed layer
        in
        if not permit then Option.None
        else
          let merged =
            Declaration.v
              ~important:(is_important (List.hd raw_decls))
              Background
              [ (Shorthand layer : Properties.background) ]
          in
          Some ((idx, merged), rest)

let compose_background_shorthand ~ctx decls =
  let rec go acc decls =
    match (decls, try_compose_background ~ctx decls) with
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

type mask_part = Properties.mask_layer -> Properties.mask_layer
type mask_updater = declaration -> mask_part option

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

let take_contiguous_mask (indexed_decls : (int * Declaration.declaration) list)
    :
    (int * (Declaration.declaration * mask_part) list) option
    * (int * Declaration.declaration) list =
  let rec aux acc = function
    | (i, d) :: rest -> (
        match mask_part_of d with
        | Some f -> aux ((d, f) :: acc) rest
        | None -> (List.rev acc, (i, d) :: rest))
    | [] -> (List.rev acc, [])
  in
  match indexed_decls with
  | [] -> (Option.None, [])
  | (idx, _) :: _ -> (
      let parts, rest = aux [] indexed_decls in
      match parts with
      | [] -> (Option.None, indexed_decls)
      | _ -> (Some (idx, parts), rest))

let try_compose_mask ~ctx (indexed_decls : (int * Declaration.declaration) list)
    :
    ((int * Declaration.declaration) * (int * Declaration.declaration) list)
    option =
  let idx_opt, rest = take_contiguous_mask indexed_decls in
  match idx_opt with
  | None -> Option.None
  | Some (idx, parts) ->
      let raw_decls = List.map fst parts in
      if List.length raw_decls < 2 then Option.None
      else if not (same_importance raw_decls) then Option.None
      else if scope ctx <> `Stylesheet then Option.None
      else
        let layer =
          List.fold_left (fun acc (_, f) -> f acc) empty_mask_layer parts
        in
        if layer.image = Option.None then Option.None
        else
          let merged =
            Declaration.v
              ~important:(is_important (List.hd raw_decls))
              Mask
              (Layer layer : Properties.mask)
          in
          Some ((idx, merged), rest)

let is_mask_border_decl d =
  match snd d with
  | Declaration { property = Mask_border; _ } -> true
  | _ -> false

let is_mask_image_decl d =
  match snd d with
  | Declaration { property = Mask_image; _ } -> true
  | _ -> false

let is_mask_layer_longhand d =
  match snd d with
  | Declaration { property = Mask_image; _ }
  | Declaration { property = Mask_repeat; _ }
  | Declaration { property = Mask_size; _ }
  | Declaration { property = Mask_position; _ }
  | Declaration { property = Mask_origin; _ }
  | Declaration { property = Mask_clip; _ }
  | Declaration { property = Mask_mode; _ }
  | Declaration { property = Mask_composite; _ } ->
      true
  | _ -> false

(* The [mask] shorthand resets [mask-border] (CSS Masking 1 sec. 6.1), so a
   [mask-border] that precedes a run of mask layer longhands can move after them
   without changing any property's cascade: the synthesised [mask] resets
   [mask-border], but the now-trailing [mask-border] overrides that reset back.
   Only swap when the following run carries a [mask-image] (what the composer
   needs), so the move happens exactly where it enables composition. Mirror of
   [reorder_border_image_before_border]. *)
let reorder_mask_border_before_mask decls =
  let rec span pred acc = function
    | d :: rest when pred d -> span pred (d :: acc) rest
    | rest -> (List.rev acc, rest)
  in
  let rec go acc = function
    | [] -> List.rev acc
    | d :: _ as l when is_mask_border_decl d ->
        let border_block, rest1 = span is_mask_border_decl [] l in
        let long_block, rest2 = span is_mask_layer_longhand [] rest1 in
        if List.exists is_mask_image_decl long_block then
          go (List.rev_append (long_block @ border_block) acc) rest2
        else go (List.rev_append border_block acc) rest1
    | d :: rest -> go (d :: acc) rest
  in
  go [] decls

let compose_mask_shorthand ~ctx decls =
  let rec go ~seen_mask_border acc decls =
    match try_compose_mask ~ctx decls with
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

type tr_part =
  Properties.transition_shorthand -> Properties.transition_shorthand

type tr_updater = declaration -> tr_part option

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

let take_contiguous_transition
    (indexed_decls : (int * Declaration.declaration) list) :
    (int * (Declaration.declaration * tr_part) list) option
    * (int * Declaration.declaration) list =
  let rec aux acc = function
    | (i, d) :: rest -> (
        match transition_part_of d with
        | Some f -> aux ((d, f) :: acc) rest
        | None -> (List.rev acc, (i, d) :: rest))
    | [] -> (List.rev acc, [])
  in
  match indexed_decls with
  | [] -> (Option.None, [])
  | (idx, _) :: _ -> (
      let parts, rest = aux [] indexed_decls in
      match parts with
      | [] -> (Option.None, indexed_decls)
      | _ -> (Some (idx, parts), rest))

let has_transition_property_decl raw_decls =
  List.exists
    (fun d ->
      match d with
      | Declaration { property = Transition_property; _ } -> true
      | _ -> false)
    raw_decls

let try_compose_transition
    (indexed_decls : (int * Declaration.declaration) list) :
    ((int * Declaration.declaration) * (int * Declaration.declaration) list)
    option =
  let idx_opt, rest = take_contiguous_transition indexed_decls in
  match idx_opt with
  | None -> Option.None
  | Some (idx, parts) ->
      let raw_decls = List.map fst parts in
      if List.length raw_decls < 2 then Option.None
      else if not (same_importance raw_decls) then Option.None
      else if not (has_transition_property_decl raw_decls) then Option.None
      else
        let layer =
          List.fold_left (fun acc (_, f) -> f acc) empty_tr_shorthand parts
        in
        let merged =
          Declaration.v
            ~important:(is_important (List.hd raw_decls))
            Transition
            [ (Shorthand layer : Properties.transition) ]
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

type an_part = Properties.animation_shorthand -> Properties.animation_shorthand
type an_updater = declaration -> an_part option

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

let take_contiguous_animation
    (indexed_decls : (int * Declaration.declaration) list) :
    (int * (Declaration.declaration * an_part) list) option
    * (int * Declaration.declaration) list =
  let rec aux acc = function
    | (i, d) :: rest -> (
        match animation_part_of d with
        | Some f -> aux ((d, f) :: acc) rest
        | None -> (List.rev acc, (i, d) :: rest))
    | [] -> (List.rev acc, [])
  in
  match indexed_decls with
  | [] -> (Option.None, [])
  | (idx, _) :: _ -> (
      let parts, rest = aux [] indexed_decls in
      match parts with
      | [] -> (Option.None, indexed_decls)
      | _ -> (Some (idx, parts), rest))

let try_compose_animation (indexed_decls : (int * Declaration.declaration) list)
    :
    ((int * Declaration.declaration) * (int * Declaration.declaration) list)
    option =
  let idx_opt, rest = take_contiguous_animation indexed_decls in
  match idx_opt with
  | None -> Option.None
  | Some (idx, parts) ->
      let raw_decls = List.map fst parts in
      if List.length raw_decls < 2 then Option.None
      else if not (same_importance raw_decls) then Option.None
      else
        let layer =
          List.fold_left (fun acc (_, f) -> f acc) empty_an_shorthand parts
        in
        let merged =
          Declaration.v
            ~important:(is_important (List.hd raw_decls))
            Animation
            [ (Shorthand layer : Properties.animation) ]
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
  (* [try_merge_box_shorthand] returns the original declaration when it absorbs
     nothing, so keep the original tuple then ([==]) rather than re-pairing it
     with its index - the head stays physically shared on a no-op. *)
  let rec go acc = function
    | [] -> List.rev acc
    | ((idx, (Declaration { property = Margin; value = vs; important } as d)) as
       item)
      :: rest
      when not (box_shorthand_had_prior_longhand source idx d) ->
        let merged, rest =
          try_merge_box_shorthand ~original:d ~property:Margin ~vs ~important
            ~absorb:(absorb_margin_corner ~important)
            ~is_same_shorthand:is_margin_shorthand rest
        in
        let head = if merged == d then item else (idx, merged) in
        go (head :: acc) rest
    | ((idx, (Declaration { property = Padding; value = vs; important } as d))
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

let property_covered_by_important kept decl =
  List.exists
    (fun (_, existing) ->
      (not (is_intentionally_duplicated existing))
      && is_important existing
      && declaration_covers existing decl)
    kept

let same_property = Declaration.same_property

let rec without_importance = function
  | Declaration r -> Declaration { r with important = false }
  | Theme_guarded t -> Theme_guarded { t with decl = without_importance t.decl }

(* Value equality ignoring importance. Every caller establishes [same_property]
   first, so the two declarations share a value type and this is a structural
   value comparison: on canonical ASTs it matches minified-text equality without
   rendering. *)
let same_value a b = without_importance a = without_importance b

(* Two declarations minify to the same text exactly when their canonical ASTs
   are equal (property, value, and importance). After the optimizer's
   canonicalisation passes the AST is canonical, so structural equality is the
   minified-equality test - and far cheaper than rendering both to strings. A
   pp-equal-but-structurally-different pair would be a canonicalisation bug, not
   something to paper over by comparing rendered text.

   [(==)] short-circuits the same-heap-object case. The cached
   [Declaration.hash] (computed once at construction by [Declaration.v]) short-
   circuits the (much more common) different-value case in one field-load + int
   compare without walking the AST; we only fall through to structural equality
   on a hash collision. *)
let same_minified_declaration (a : declaration) (b : declaration) =
  a == b || (Declaration.hash a = Declaration.hash b && a = b)

let legacy_vendor_fallback new_decl existing =
  (* Different-value duplicates are kept when one value is vendor-prefixed: the
     cascade may pick whichever the browser understands. *)
  same_property new_decl existing
  && (not (same_value new_decl existing))
  && (value_is_vendor_prefixed existing || value_is_vendor_prefixed new_decl)

(* The earlier declaration is a real cascade fallback when the later one uses
   CSS Color 4 / 5 syntax that older browsers drop. *)
let legacy_color_fallback new_decl existing =
  same_property new_decl existing
  && (not (same_value new_decl existing))
  && Declaration.value_uses_color_4 new_decl
  && not (Declaration.value_uses_color_4 existing)

(* Same shape: the later value uses a runtime substitution ([var()] / [env()] /
   [attr()]) and the earlier doesn't, so the earlier is a static fallback for
   browsers that can't resolve the substitution at parse time. *)
let legacy_runtime_subst_fallback new_decl existing =
  same_property new_decl existing
  && (not (same_value new_decl existing))
  && Declaration.value_uses_runtime_subst new_decl
  && not (Declaration.value_uses_runtime_subst existing)

let same_property_value_declaration new_decl existing =
  same_property new_decl existing
  && same_value new_decl existing
  && (is_important new_decl || not (is_important existing))

let covered_by_new_declaration new_decl existing =
  (not (is_intentionally_duplicated existing))
  && declaration_covers new_decl existing
  && (is_important new_decl || not (is_important existing))
  && (not (legacy_vendor_fallback new_decl existing))
  && (not (legacy_color_fallback new_decl existing))
  && not (legacy_runtime_subst_fallback new_decl existing)

let filter_preserve = List.filter_preserve

let add_all_declaration_rev idx decl kept =
  let after, before =
    List.partition (fun (_, old) -> all_preserved_reorder_declaration old) kept
  in
  after @ ((idx, decl) :: before)

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
      filter_preserve
        (fun (_, old) -> not (same_property_value_declaration decl old))
        kept
    in
    (idx, decl) :: kept
  else if (not (is_important decl)) && property_covered_by_important kept decl
  then kept
  else
    let kept =
      filter_preserve
        (fun (_, old) -> not (covered_by_new_declaration decl old))
        kept
    in
    if is_all_declaration decl then add_all_declaration_rev idx decl kept
    else (idx, decl) :: kept

(* [vendor_alias_redundant vendor twin] is [true] when [vendor] is a
   vendor-prefixed declaration made redundant by its unprefixed [twin] carrying
   the same value and importance. Each pair is matched on both property
   constructors so the two values share a type and compare with a typed (=) - no
   rendering. [-webkit-appearance] is intentionally absent: its value type
   [webkit_appearance] is a superset of [appearance] (extra non-standard values
   like [listbox]/[checkbox]/[radio]), so there is no typed equality between the
   two. [text-decoration] is also absent because dropping the WebKit copy
   regresses documented inheritance quirks. *)
let vendor_alias_redundant vendor twin =
  match (vendor, twin) with
  | ( Declaration { property = Webkit_transform; value = v1; important = i1 },
      Declaration { property = Transform; value = v2; important = i2 } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Webkit_transition; value = v1; important = i1 },
      Declaration { property = Transition; value = v2; important = i2 } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = O_transition; value = v1; important = i1 },
      Declaration { property = Transition; value = v2; important = i2 } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Webkit_filter; value = v1; important = i1 },
      Declaration { property = Filter; value = v2; important = i2 } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_backdrop_filter; value = v1; important = i1 },
      Declaration { property = Backdrop_filter; value = v2; important = i2 } )
    ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Webkit_user_select; value = v1; important = i1 },
      Declaration { property = User_select; value = v2; important = i2 } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Moz_user_select; value = v1; important = i1 },
      Declaration { property = User_select; value = v2; important = i2 } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Webkit_hyphens; value = v1; important = i1 },
      Declaration { property = Hyphens; value = v2; important = i2 } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_text_size_adjust; value = v1; important = i1 },
      Declaration { property = Text_size_adjust; value = v2; important = i2 } )
    ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_print_color_adjust; value = v1; important = i1 },
      Declaration { property = Print_color_adjust; value = v2; important = i2 }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | _ -> false

(* Drop a vendor-prefixed declaration when its unprefixed sibling appears in the
   same rule with the same value and importance. The unprefixed form supersedes
   in modern browsers, so the vendor copy is dead under the recent-browser
   policy. *)
let drop_vendor_aliases (kept : (int * declaration) list) :
    (int * declaration) list =
  let has_unprefixed_twin (_, decl) =
    List.exists (fun (_, other) -> vendor_alias_redundant decl other) kept
  in
  filter_preserve (fun item -> not (has_unprefixed_twin item)) kept

let compose_shorthands ~ctx kept =
  kept
  |> compose_box_shorthands ~ctx
  |> compose_pair_shorthands |> compose_outline_shorthand
  |> reorder_font_resets_before_font |> compose_font_shorthand
  |> compose_list_style_shorthand |> compose_flex_shorthand
  |> compose_text_decoration_shorthand |> compose_border_shorthand
  |> reorder_border_image_before_border
  |> compose_border_whole_shorthand ~ctx
  |> drop_bimg_shadowed_by_border
  |> compose_border_image_shorthand ~ctx
  |> compose_background_shorthand ~ctx
  |> reorder_mask_border_before_mask
  |> compose_mask_shorthand ~ctx
  |> compose_transition_shorthand |> compose_animation_shorthand
  |> fun kept ->
  merge_box_shorthand_longhands kept kept |> merge_overflow_longhands

let deduplicate_declarations_with ~ctx ?(merge_box = true) props =
  let indexed_props = List.mapi (fun i decl -> (i, decl)) props in
  let kept = List.rev (List.fold_left deduplicate_step [] indexed_props) in
  let kept =
    let kept = if merge_box then compose_shorthands ~ctx kept else kept in
    let kept = drop_vendor_aliases kept in
    List.map (fun (_, decl) -> decl) kept
  in
  let result = duplicate_buggy_properties kept in
  (* Each pipeline step keeps untouched declarations, so [preserve_list]
     restores the input list when every declaration is physically unchanged -
     callers detect a no-op by identity (the factoring fixpoint relies on
     it). *)
  preserve_list props result

let deduplicate_declarations ?scope props =
  deduplicate_declarations_with ~ctx:(Ctx.of_scope scope) props
