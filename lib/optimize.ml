(** CSS optimization implementation *)

open Declaration
open Stylesheet
module String_set = Set.Make (String)

let src = Logs.Src.create "cascade.optimize" ~doc:"Cascade CSS optimizer"

module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Edge Model} *)

type scope = [ `Fragment | `Stylesheet ]

(* Optimisation context threaded by the entry points ([stylesheet], [rules],
   [single_rule], [deduplicate_declarations]) down to the shorthand composers.
   [scope] drives the fragment-vs-stylesheet decisions; [registered] reports
   whether a custom property is registered with an [@property] initial-value, so
   folding its [var()] into a shorthand cannot widen an invalid-at-
   computed-value failure. Default: fragment, nothing registered. *)
type ctx = { scope : scope; registered : string -> bool; lossless : bool }

let fragment_ctx =
  { scope = `Fragment; registered = (fun _ -> false); lossless = false }

let ctx_of_scope ?(lossless = false) = function
  | Some s -> { fragment_ctx with scope = s; lossless }
  | None -> { fragment_ctx with lossless }

let list_map_preserve f xs =
  let rec loop changed acc = function
    | [] -> if changed then List.rev acc else xs
    | x :: rest ->
        let y = f x in
        loop (changed || not (y == x)) (y :: acc) rest
  in
  loop false [] xs

(* [List.concat_map] that returns the input list itself when every element maps
   to a singleton holding that same element (a per-element no-op). *)
let concat_map_preserve f xs =
  let changed = ref false in
  let ys =
    List.concat_map
      (fun x ->
        match f x with
        | [ y ] when y == x -> [ x ]
        | r ->
            changed := true;
            r)
      xs
  in
  if !changed then ys else xs

let list_filter_preserve f xs =
  let rec loop changed acc = function
    | [] -> if changed then List.rev acc else xs
    | x :: rest ->
        if f x then loop changed (x :: acc) rest else loop true acc rest
  in
  loop false [] xs

let list_filter_map_preserve f xs =
  let rec loop changed acc = function
    | [] -> if changed then List.rev acc else xs
    | x :: rest -> (
        match f x with
        | Some y -> loop (changed || not (y == x)) (y :: acc) rest
        | None -> loop true acc rest)
  in
  loop false [] xs

let option_map_preserve (f : 'a -> 'a) (value : 'a option) : 'a option =
  match value with
  | None -> (None : 'a option)
  | Some x ->
      let y = f x in
      if y == x then value else Some y

let rec list_same xs ys =
  match (xs, ys) with
  | [], [] -> true
  | x :: xs, y :: ys -> x == y && list_same xs ys
  | _ -> false

let preserve_list before after =
  if list_same before after then before else after

let rule_with_declarations (rule : rule) declarations =
  if declarations == rule.declarations then rule else { rule with declarations }

let rule_with_nested (rule : rule) nested =
  if nested == rule.nested then rule else { rule with nested }

let rule_with_declarations_and_nested (rule : rule) declarations nested =
  if declarations == rule.declarations && nested == rule.nested then rule
  else { rule with declarations; nested }

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

(* CSS Logical 1 sec. 4.2: [border-block] / [border-inline] reset their two
   flow-relative longhands. Cascade does not model those longhands as typed
   properties (they parse as [Unknown_property]), so the coverage is matched by
   name. *)
let logical_shorthand_covers_name covering covered =
  match covering with
  | "border-block" ->
      String.equal covered "border-block-start"
      || String.equal covered "border-block-end"
  | "border-inline" ->
      String.equal covered "border-inline-start"
      || String.equal covered "border-inline-end"
  | _ -> false

(* Coverage relation between two declarations. Custom and unknown properties
   have only generic behavior: cover themselves by exact name and no typed
   shorthand coverage (logical border shorthands match their longhands by name),
   and (for custom) exempt from the [all] reset. *)
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
      String.equal a b || logical_shorthand_covers_name a b
  | Declaration { property = Unknown_property a; _ }, _ ->
      logical_shorthand_covers_name a (property_name covered)
  | _, Declaration { property = Unknown_property b; _ } ->
      logical_shorthand_covers_name (property_name covering) b
  | ( Declaration { property = covering_p; _ },
      Declaration { property = covered_p; _ } ) ->
      Declaration.same_property covering covered
      || shorthand_covers_longhand covering_p covered_p
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
              shorthand_covers_longhand shorthand_prop lh_prop
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
            | Var vr -> ctx.registered vr.name
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
          match w with Var v -> ctx.registered v.name | _ -> true
        in
        let foldable_style (s : Properties.border_style) =
          match s with Var v -> ctx.registered v.name | _ -> true
        in
        let foldable_color (c : Values.color) =
          match c with Var v -> ctx.registered v.name | _ -> true
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
  if ctx.scope <> `Stylesheet then decls
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
          match ctx.scope with
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
      else if ctx.scope <> `Stylesheet then Option.None
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
  List.filter (fun item -> not (has_unprefixed_twin item)) kept

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
  let kept = List.fold_left deduplicate_step [] indexed_props in
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
  deduplicate_declarations_with ~ctx:(ctx_of_scope scope) props

(* A [:host] / [:root] block is the conventional design-token surface (Tailwind,
   most design systems): each [--name] is declared at most once and the order of
   distinct names has no cascade effect. Sorting them alphabetically gives a
   single canonical AST regardless of source order or which downstream generator
   emitted the rule (Tailwind's (p, s)-ordered source vs tw's
   Var.binding-constructed declarations vs a hand-written stylesheet all map to
   the same sequence). The sort is stable so two declarations with the same name
   keep their relative order ("later wins" preserved), and non-custom
   declarations stay in their original positions so any interleaving with
   regular declarations is unchanged. *)
let rec selector_targets_host_or_root : Selector.t -> bool = function
  | Selector.Root | Selector.Host _ -> true
  | Selector.List xs -> List.for_all selector_targets_host_or_root xs
  | Selector.Where xs | Selector.Is xs ->
      List.for_all selector_targets_host_or_root xs
  | _ -> false

let custom_property_name = function
  | Declaration { property = Custom_property name; _ } -> Some name
  | _ -> None

let sort_custom_property_declarations_stable decls =
  let customs = List.filter (fun d -> custom_property_name d <> None) decls in
  let sorted =
    List.stable_sort
      (fun d1 d2 ->
        match (custom_property_name d1, custom_property_name d2) with
        | Some n1, Some n2 -> String.compare n1 n2
        | _ -> 0)
      customs
  in
  let next = ref sorted in
  let result =
    List.map
      (fun d ->
        if custom_property_name d <> None then
          match !next with
          | x :: rest ->
              next := rest;
              x
          | [] -> d
        else d)
      decls
  in
  if List.equal ( == ) result decls then decls else result

let sort_commuting_declarations ?selector decls =
  match selector with
  | Some sel when selector_targets_host_or_root sel ->
      sort_custom_property_declarations_stable decls
  | _ -> decls

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

let single_rule_without_nested ~ctx (rule : rule) : rule =
  let declarations =
    list_map_preserve
      (Declaration.normalize ~lossless:ctx.lossless)
      rule.declarations
    |> deduplicate_declarations_with ~ctx ~merge_box:false
    |> sort_commuting_declarations ~selector:rule.selector
    |> preserve_list rule.declarations
  in
  rule_with_declarations rule declarations

(* CSS Multicol 2 sec. 6.1: [column-width] + [column-count] in the same rule
   collapse to the [columns] shorthand. [columns] resets exactly those two
   longhands, so the rewrite preserves every other property's cascade and needs
   no closed-world assumption. Cascade models the longhands as unknown
   properties, so their values are re-parsed from text here. *)
let columns_value_of_longhands width count : Properties.columns_value =
  match (width, count) with
  | `Auto, `Auto -> (Auto : Properties.columns_value)
  | `Auto, `Count n -> Auto_count n
  | `Width w, `Auto -> Width w
  | `Width w, `Count n -> Both (w, n)

(* CSS Multicol 2 sec. 6.1: [column-width] + [column-count] collapse to the
   [columns] shorthand, which resets exactly those two longhands. Compose the
   unique pair of matching importance when both carry a plain (non-[var()],
   non-CSS-wide) value, emitting the shorthand where the first appeared. *)
let synthesize_columns decls =
  let width_of d : (Properties.column_width * bool) option =
    match d with
    | Declaration { property = Column_width; value; important } ->
        Some (value, important)
    | _ -> None
  in
  let count_of d : (Properties.column_count * bool) option =
    match d with
    | Declaration { property = Column_count; value; important } ->
        Some (value, important)
    | _ -> None
  in
  let uniq f =
    match List.filter_map f decls with [ x ] -> Some x | _ -> None
  in
  match (uniq width_of, uniq count_of) with
  | Some (w, wi), Some (c, ci) when wi = ci -> (
      let plain_width :
          Properties.column_width -> [ `Auto | `Width of Values.length ] option
          = function
        | Auto -> Some `Auto
        | Width l -> Some (`Width l)
        | _ -> None
      in
      let plain_count :
          Properties.column_count -> [ `Auto | `Count of int ] option = function
        | Auto -> Some `Auto
        | Count n -> Some (`Count n)
        | _ -> None
      in
      match (plain_width w, plain_count c) with
      | Some w, Some c ->
          let shorthand =
            Declaration.v ~important:wi Properties.Columns
              (columns_value_of_longhands w c)
          in
          let placed = ref false in
          List.filter_map
            (fun d ->
              match d with
              | Declaration { property = Column_width; _ }
              | Declaration { property = Column_count; _ } ->
                  if !placed then None
                  else (
                    placed := true;
                    Some shorthand)
              | _ -> Some d)
            decls
      | _ -> decls)
  | _ -> decls

(* CSS Anchor Positioning 1: [position-try] is [<'position-try-order'> ||
   <'position-try-fallbacks'>]. [position-try-order: normal] is the initial
   value and folds away. Cascade has no typed [position-try] shorthand, so the
   synthesised value is re-parsed into the (round-tripping) unknown property. *)
let synthesize_position_try decls =
  let order_of d : (Properties.position_try_order * bool) option =
    match d with
    | Declaration { property = Position_try_order; value; important } ->
        Some (value, important)
    | _ -> None
  in
  let fallbacks_of d : (Properties.position_try_fallbacks * bool) option =
    match d with
    | Declaration { property = Position_try_fallbacks; value; important } ->
        Some (value, important)
    | _ -> None
  in
  let uniq f =
    match List.filter_map f decls with [ x ] -> Some x | _ -> None
  in
  match (uniq order_of, uniq fallbacks_of) with
  | Some (order, oi), Some (fallbacks, fi) when oi = fi -> (
      (* Compose only plain component values; a CSS-wide keyword or [var()] in
         one longhand cannot share a shorthand with a real value in the
         other. *)
      let plain_order : Properties.position_try_order -> bool = function
        | Normal | Most_width | Most_height | Most_block_size | Most_inline_size
          ->
            true
        | _ -> false
      in
      let plain_fallbacks : Properties.position_try_fallbacks -> bool = function
        | None | Fallbacks _ -> true
        | _ -> false
      in
      match (plain_order order, plain_fallbacks fallbacks) with
      | true, true ->
          let shorthand =
            Declaration.v ~important:oi Properties.Position_try
              (Try (order, fallbacks))
          in
          let placed = ref false in
          List.filter_map
            (fun d ->
              match d with
              | Declaration { property = Position_try_order; _ }
              | Declaration { property = Position_try_fallbacks; _ } ->
                  if !placed then None
                  else (
                    placed := true;
                    Some shorthand)
              | _ -> Some d)
            decls
      | _ -> decls)
  | _ -> decls

let finalize_rule_without_nested ?(canonicalize_selector = true) ~ctx
    (rule : rule) : rule =
  let declarations =
    deduplicate_declarations_with ~ctx rule.declarations
    |> synthesize_columns |> synthesize_position_try
    |> sort_commuting_declarations ~selector:rule.selector
    |> preserve_list rule.declarations
  in
  (* Selectors merged during factoring are fresh comma lists, so re-canonicalise
     before emission; unchanged selectors keep their identity for the
     fixpoint. *)
  let rule =
    if canonicalize_selector then
      let canon = Selector.canonicalize rule.selector in
      if canon == rule.selector then rule else { rule with selector = canon }
    else rule
  in
  rule_with_declarations rule declarations

(* Compare selectors as sets when both are comma lists: [h1, h2] and [h2, h1]
   target the same elements and should merge. Sort the list entries by stdlib
   structural order so the key is order-insensitive without serialising to a
   string. Selectors are canonicalised on entry (see [Selector.canonicalize]),
   so structural equality of the AST matches equality of the minified text. *)
let canonical_selector_key (sel : Selector.t) : Selector.t list =
  match Selector.as_list sel with
  | Some xs -> List.sort compare xs
  | None -> [ sel ]

let rules_have_same_selector (prev : Stylesheet.rule) (rule : Stylesheet.rule) =
  canonical_selector_key prev.selector = canonical_selector_key rule.selector
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
  preserve_list rules (merge_adjacent [] None rules)

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
    let rec find i depth : int option =
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

(* Two rules can be combined when their declaration blocks are structurally
   identical. Merging adjacent identical blocks into a selector list is always
   spec-equivalent, so - unlike Lightning CSS - cascade does so even when a
   value contains an oklab() with a [none] channel (a smaller, valid merge
   Lightning leaves on the table). *)
let rec declaration_lists_equal d1 d2 =
  match (d1, d2) with
  | [], [] -> true
  | a :: rest_a, b :: rest_b ->
      same_minified_declaration a b && declaration_lists_equal rest_a rest_b
  | _ -> false

let declarations_css_equal d1 d2 = d1 == d2 || declaration_lists_equal d1 d2

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
      let pseudo_tier : Selector.t option -> int = function
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

let delayed_blocks_group ~group_props ~rule_summary
    (delayed : (Stylesheet.rule * Selector_summary.t) list) =
  List.exists
    (fun ((dr : Stylesheet.rule), ds) ->
      Selector_summary.may_overlap ds rule_summary
      && writes_any_of group_props dr.Stylesheet_intf.declarations)
    delayed

let rule_perturbs_group ~group_props ~rule_summary
    (current_group :
      (Stylesheet.rule
      * (Selector.t * Declaration.declaration list * string option)
      * Selector_summary.t)
      list) (rule : Stylesheet.rule) =
  let overlaps_group =
    List.exists
      (fun (_, _, s) -> Selector_summary.may_overlap s rule_summary)
      current_group
  in
  overlaps_group && writes_any_of group_props rule.Stylesheet_intf.declarations

let flush_combined_group acc current_group delayed =
  let acc =
    match current_group with
    | [] -> acc
    | [ (rule, _, _) ] -> rule :: acc
    | _ ->
        let group_members =
          List.map (fun (_, member, _) -> member) current_group
        in
        flush_group acc group_members
  in
  List.fold_left (fun acc (rule, _) -> rule :: acc) acc delayed

let can_join_group ~group_props ~rule_summary
    (delayed : (Stylesheet.rule * Selector_summary.t) list) prev_rule rule =
  can_combine_rules prev_rule rule
  && not (delayed_blocks_group ~group_props ~rule_summary delayed)

let group_member rule rule_summary =
  (rule, group_member_of_rule rule, rule_summary)

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
  preserve_list rules (walk [] rules)

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
      let member = group_member rule rule_summary in
      combine_consecutive acc (member :: current_group) delayed rest
    in
    match current_group with
    | [] -> push_to_group ()
    | (_, head_member, _) :: _ ->
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
  combine_consecutive [] [] [] rules
  |> preserve_list rules |> combine_adjacent_identical_decls

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

let decls_pp_size ds =
  let pp ctx ds = List.iter (Declaration.pp_declaration ctx) ds in
  Pp.size ~minify:true pp ds

let rule_factor_boundary (r : Stylesheet.rule) =
  r.nested <> [] || r.merge_key <> None
  || contains_vendor_pseudo_element r.selector

let rule_factor_eligible (r : Stylesheet.rule) =
  (not (rule_factor_boundary r))
  && not (List.exists is_all_declaration r.declarations)

let decl_property d = Declaration.property_key d

let merge_selector_list = function
  | [ s ] -> s
  | sels ->
      let flatten = function Selector.List xs -> xs | s -> [ s ] in
      Selector.List (List.concat_map flatten sels)

(* Byte size of the printed rule [<selector>{<d1>;<d2>;...;<dn>}].
   [decls_pp_size] measures declarations without their separators - the [;]
   between them is added by the rule printer at emission time, so we add [n - 1]
   back here. *)
let rule_pp_size (r : Stylesheet.rule) =
  let sel = Pp.size ~minify:true Selector.pp r.Stylesheet_intf.selector in
  let decls = decls_pp_size r.Stylesheet_intf.declarations in
  let separators =
    match r.declarations with [] | [ _ ] -> 0 | _ :: rest -> List.length rest
  in
  sel + 2 + decls + separators

let rules_pp_size rules =
  List.fold_left (fun acc r -> acc + rule_pp_size r) 0 rules

let decl_list_size decl_pp_size decl_count =
  decl_pp_size + max 0 (decl_count - 1)

let rule_size_from_parts selector_size decl_pp_size decl_count =
  selector_size + 2 + decl_list_size decl_pp_size decl_count

(* 63-bit Bloom filter over [Declaration.hash] values, indexed by two
   independent hash projections (low and high bytes of the structural hash). For
   K~5 declarations per rule the false-positive rate is well under 1%; a true
   negative is constant-time and lets us skip the actual [List.exists /
   same_minified_declaration] check entirely. *)
let bloom_mask (h : int) = (1 lsl (h land 63)) lor (1 lsl ((h lsr 8) land 63))
let bloom_add b h = b lor bloom_mask h

let bloom_might_contain b h =
  let m = bloom_mask h in
  b land m = m

module Prop_set = Set.Make (struct
  type t = Declaration.prop_key

  let compare = Stdlib.compare
end)

module Prop_map = Map.Make (struct
  type t = Declaration.prop_key

  let compare = Stdlib.compare
end)

module Prop_key_tbl = Hashtbl.Make (struct
  type t = Declaration.prop_key

  let equal = ( = )
  let hash = Hashtbl.hash
end)

let prop_ids : int Prop_key_tbl.t = Prop_key_tbl.create 512
let next_prop_id = ref 0

let prop_id prop =
  match Prop_key_tbl.find_opt prop_ids prop with
  | Some id -> id
  | None ->
      let id = !next_prop_id in
      incr next_prop_id;
      Prop_key_tbl.add prop_ids prop id;
      id

let prop_ids_of_set props =
  let ids = Prop_set.to_seq props |> Array.of_seq |> Array.map prop_id in
  Array.sort compare ids;
  ids

let prop_ids_of_decls decls =
  decls |> List.map (fun d -> prop_id (decl_property d)) |> Array.of_list

let prop_ids_empty ids = Array.length ids = 0

let prop_ids_mem id ids =
  let rec search lo hi =
    if lo > hi then false
    else
      let mid = (lo + hi) lsr 1 in
      let value = ids.(mid) in
      if id = value then true
      else if id < value then search lo (mid - 1)
      else search (mid + 1) hi
  in
  search 0 (Array.length ids - 1)

let prop_ids_disjoint a b =
  let rec loop i j =
    if i >= Array.length a || j >= Array.length b then true
    else
      let x = a.(i) and y = b.(j) in
      if x = y then false else if x < y then loop (i + 1) j else loop i (j + 1)
  in
  loop 0 0

let prop_ids_subset a b =
  let rec loop i j =
    if i >= Array.length a then true
    else if j >= Array.length b then false
    else
      let x = a.(i) and y = b.(j) in
      if x = y then loop (i + 1) (j + 1)
      else if x < y then false
      else loop i (j + 1)
  in
  loop 0 0

let prop_ids_inter a b =
  let acc = ref [] in
  let rec loop i j =
    if i < Array.length a && j < Array.length b then
      let x = a.(i) and y = b.(j) in
      if x = y then (
        acc := x :: !acc;
        loop (i + 1) (j + 1))
      else if x < y then loop (i + 1) j
      else loop i (j + 1)
  in
  loop 0 0;
  Array.of_list (List.rev !acc)

type factor_rule_summary = {
  factor_rule : Stylesheet.rule;
  factor_size : int;  (** cached [rule_pp_size factor_rule] *)
  factor_selector_size : int;
  factor_decl_sizes : int list;
  factor_decl_pp_size : int;
  factor_decl_count : int;
  factor_prop_set : Prop_set.t;
  factor_prop_ids : int array;
  factor_decl_prop_ids : int array;
  factor_decl_map : declaration Prop_map.t;
      (** First declaration for each property, matching [List.find_opt] over
          [factor_rule.declarations]. Duplicate-property fallback semantics are
          preserved by keeping the earliest declaration in source order. *)
  factor_decl_size_map : int Prop_map.t;
      (** Minified byte size of the declaration stored in [factor_decl_map],
          indexed separately so interval scoring can stay on cached integers. *)
  decl_bloom : int;
      (** Bloom filter over [Declaration.hash] for every declaration in
          [factor_rule]. A definite-absence answer ([bloom_might_contain]
          returns [false]) skips the full [List.exists] /
          [same_minified_declaration] walk in cross-rule overlap checks; a
          possible-presence answer falls through to the actual structural check,
          so collisions are correctness-preserving. *)
  selector_summary : Selector_summary.t Lazy.t;
}

(* Memoise summaries by physical identity. Rules are persistent records, so the
   same rule heap object is inspected by many overlapping factoring windows in a
   single [factor_rules_to_fixpoint] iteration; [rule_pp_size] does two
   [Pp.size] traversals each call, which made the inner factoring loops
   quadratic in the window size. We key on the boxed [Obj.repr] of the rule:
   [Hashtbl] uses our [equal = (==)], so misses on identity, never false hits.
   The hash function is fixed-depth (stdlib default depth, capped) so it is
   bounded regardless of the rule's structure. *)
module Rule_id_tbl = Hashtbl.Make (struct
  type t = Stylesheet.rule

  let equal = ( == )

  (* O(1) bucket hash combining the cached [Declaration.hash] of the first two
     declarations -- both are field loads on the structural fingerprint stored
     at declaration construction. With [equal = (==)], a bucket collision falls
     through to a physical pointer scan, so we never walk the rule structure on
     lookup. *)
  let hash (r : t) =
    match r.Stylesheet_intf.declarations with
    | [] -> 0
    | [ d ] -> Declaration.hash d
    | d1 :: d2 :: _ -> Declaration.hash d1 lxor (Declaration.hash d2 lsl 1)
end)

(* Per-pass and global counters for the --profile CLI flag. Reset at each
   [Optimize.stylesheet] entry; bumped from the hot loops below. Kept up here so
   [summarize_factor_rule] and [factor_anchor_score] can record into them. *)
type pass_stat = {
  mutable time : float;
  mutable calls : int;
  mutable changes : int;
  mutable rules_in : int;
  mutable rules_out : int;
}

let pass_times : (string, pass_stat) Hashtbl.t = Hashtbl.create 16
let collect_profile_stats = ref false
let set_profile enabled = collect_profile_stats := enabled
let current_factor_savings = ref 0

let record_factor_saving saving =
  if saving > 0 then current_factor_savings := !current_factor_savings + saving

type iteration_stat = {
  fixpoint : int;
  iteration : int;
  local_iteration : int;
  before_rules : int;
  after_rules : int;
  before_bytes : int;
  after_bytes : int;
  bytes_saved : int;
  active_passes : int;
  changed_passes : int;
  elapsed : float;
}

let iteration_stats_rev = ref []
let iteration_stats () = !iteration_stats_rev

let pass_stat name =
  match Hashtbl.find_opt pass_times name with
  | Some s -> s
  | None ->
      let s =
        { time = 0.0; calls = 0; changes = 0; rules_in = 0; rules_out = 0 }
      in
      Hashtbl.add pass_times name s;
      s

type counters = {
  mutable iterations : int;
  mutable factor_fixpoints_run : int;
  mutable marginal_stops : int;
  mutable summary_hits : int;
  mutable summary_misses : int;
  mutable factor_fixpoints_skipped : int;
  mutable factor_preflight_gain : int;
  mutable factor_bytes_saved : int;
  mutable anchors_scored : int;
  mutable anchors_prefiltered : int;
  mutable factorings_applied : int;
  mutable interval_candidates : int;
  mutable interval_pruned : int;
  mutable interval_scored : int;
  mutable interval_selected : int;
}

let counters =
  {
    iterations = 0;
    factor_fixpoints_run = 0;
    marginal_stops = 0;
    summary_hits = 0;
    summary_misses = 0;
    factor_fixpoints_skipped = 0;
    factor_preflight_gain = 0;
    factor_bytes_saved = 0;
    anchors_scored = 0;
    anchors_prefiltered = 0;
    factorings_applied = 0;
    interval_candidates = 0;
    interval_pruned = 0;
    interval_scored = 0;
    interval_selected = 0;
  }

let reset_counters () =
  Hashtbl.reset pass_times;
  iteration_stats_rev := [];
  Prop_key_tbl.reset prop_ids;
  next_prop_id := 0;
  counters.iterations <- 0;
  counters.factor_fixpoints_run <- 0;
  counters.marginal_stops <- 0;
  counters.summary_hits <- 0;
  counters.summary_misses <- 0;
  counters.factor_fixpoints_skipped <- 0;
  counters.factor_preflight_gain <- 0;
  counters.factor_bytes_saved <- 0;
  counters.anchors_scored <- 0;
  counters.anchors_prefiltered <- 0;
  counters.factorings_applied <- 0;
  counters.interval_candidates <- 0;
  counters.interval_pruned <- 0;
  counters.interval_scored <- 0;
  counters.interval_selected <- 0

let summary_memo : factor_rule_summary Rule_id_tbl.t = Rule_id_tbl.create 4096
let clear_summary_memo () = Rule_id_tbl.reset summary_memo

let summarize_factor_rule factor_rule =
  match Rule_id_tbl.find_opt summary_memo factor_rule with
  | Some s ->
      counters.summary_hits <- counters.summary_hits + 1;
      s
  | None ->
      counters.summary_misses <- counters.summary_misses + 1;
      let decls = factor_rule.declarations in
      let decl_sizes =
        List.map (Pp.size ~minify:true Declaration.pp_declaration) decls
      in
      let factor_prop_set, factor_decl_map, factor_decl_size_map =
        let rec loop set decl_map size_map decls sizes =
          match (decls, sizes) with
          | [], [] -> (set, decl_map, size_map)
          | decl :: decls, size :: sizes ->
              let prop = decl_property decl in
              let decl_map, size_map =
                if Prop_map.mem prop decl_map then (decl_map, size_map)
                else
                  ( Prop_map.add prop decl decl_map,
                    Prop_map.add prop size size_map )
              in
              loop (Prop_set.add prop set) decl_map size_map decls sizes
          | _ -> assert false
        in
        loop Prop_set.empty Prop_map.empty Prop_map.empty decls decl_sizes
      in
      let s =
        {
          factor_rule;
          factor_size = rule_pp_size factor_rule;
          factor_selector_size =
            Pp.size ~minify:true Selector.pp
              factor_rule.Stylesheet_intf.selector;
          factor_decl_sizes = decl_sizes;
          factor_decl_pp_size = List.fold_left ( + ) 0 decl_sizes;
          factor_decl_count = List.length decls;
          factor_prop_set;
          factor_prop_ids = prop_ids_of_set factor_prop_set;
          factor_decl_prop_ids = prop_ids_of_decls decls;
          factor_decl_map;
          factor_decl_size_map;
          decl_bloom =
            List.fold_left (fun b d -> bloom_add b (Declaration.hash d)) 0 decls;
          selector_summary =
            lazy
              (Selector_summary.of_selector factor_rule.Stylesheet_intf.selector);
        }
      in
      Rule_id_tbl.add summary_memo factor_rule s;
      s

let factor_rule_declares_all summary props =
  List.for_all (fun prop -> Prop_set.mem prop summary.factor_prop_set) props

let factor_rule_declares_prop_ids summary props =
  prop_ids_subset props summary.factor_prop_ids

let summary_decl_for_prop summary prop =
  Prop_map.find_opt prop summary.factor_decl_map

let summary_decl_size_for_prop summary prop =
  Prop_map.find_opt prop summary.factor_decl_size_map

let summary_contains_declaration summary decl =
  bloom_might_contain summary.decl_bloom (Declaration.hash decl)
  && List.exists
       (fun candidate -> same_minified_declaration decl candidate)
       summary.factor_rule.Stylesheet_intf.declarations

(* Hoisting [common] into a shared rule pays off for a member only when its
   selector entry ([|selector| + 1]) is cheaper than the bytes it would
   otherwise duplicate ([decls_pp_size common] plus one separator per
   declaration). A member fully consumed by [common] (empty leftover) always
   joins, since dropping it would spawn a whole separate rule. [leftovers] is
   aligned with [rules_arr] ([None] = empty leftover). Returns [None] when fewer
   than two members survive; otherwise the grouped rule (members only) and a
   leftover array where each pruned member keeps its full declarations inline.
   Pruning is cascade-safe: a pruned member's declarations stay in place, so the
   group is a subset of the run already proven safe to factor. *)
let cost_aware_factor_group (first : Stylesheet.rule)
    (rules_arr : Stylesheet.rule array)
    (factor_summaries : factor_rule_summary array)
    (common : Declaration.declaration list)
    (leftovers : Stylesheet.rule option array) :
    (Stylesheet.rule * Stylesheet.rule option array) option =
  let common_inline_cost = decls_pp_size common + List.length common in
  let member =
    Array.mapi
      (fun i summary ->
        match leftovers.(i) with
        | None -> true
        | Some _ ->
            (* Pruning only saves bytes when the member carries [common]
               verbatim: dropping it then removes exactly [common] from its
               rule. A member that overrides a [common] property (default-value
               factoring) keeps a differing value inline whether grouped or not,
               so the cost model does not apply - leave it in the group as the
               scan selected it. *)
            let exact =
              List.for_all
                (fun cd -> summary_contains_declaration summary cd)
                common
            in
            (not exact)
            || summary.factor_selector_size + 1 <= common_inline_cost)
      factor_summaries
  in
  let member_count =
    Array.fold_left
      (fun count keep -> if keep then count + 1 else count)
      0 member
  in
  if member_count < 2 then Option.None
  else
    let sels =
      Array.to_list rules_arr
      |> List.mapi (fun i r -> (i, r))
      |> List.filter (fun (i, _) -> member.(i))
      |> List.map (fun (_, (r : Stylesheet.rule)) -> r.Stylesheet_intf.selector)
    in
    let grouped =
      { first with selector = merge_selector_list sels; declarations = common }
    in
    let leftovers =
      Array.mapi
        (fun i lo -> if member.(i) then lo else Some rules_arr.(i))
        leftovers
    in
    Some (grouped, leftovers)

let first_decl_map decls =
  List.fold_left
    (fun map decl ->
      let prop = decl_property decl in
      if Prop_map.mem prop map then map else Prop_map.add prop decl map)
    Prop_map.empty decls

let common_prop_set_of_summaries = function
  | [] -> Prop_set.empty
  | first :: rest ->
      List.fold_left
        (fun props summary -> Prop_set.inter props summary.factor_prop_set)
        first.factor_prop_set rest

let common_props_of_array summaries =
  let len = Array.length summaries in
  if len = 0 then Prop_set.empty
  else
    let props = ref summaries.(0).factor_prop_set in
    for i = 1 to len - 1 do
      props := Prop_set.inter !props summaries.(i).factor_prop_set
    done;
    !props

let common_prop_ids_of_array summaries =
  let len = Array.length summaries in
  if len = 0 then [||]
  else
    let props = ref summaries.(0).factor_prop_ids in
    for i = 1 to len - 1 do
      props := prop_ids_inter !props summaries.(i).factor_prop_ids
    done;
    !props

let common_decls_from_props common_props first_summary =
  List.filter_map
    (fun d ->
      let prop = decl_property d in
      if Prop_set.mem prop common_props then
        summary_decl_for_prop first_summary prop
      else None)
    first_summary.factor_rule.Stylesheet_intf.declarations

let common_decls_from_ids common_ids first_summary =
  let rec loop i acc = function
    | [] -> List.rev acc
    | d :: rest ->
        let prop = decl_property d in
        let acc =
          if prop_ids_mem first_summary.factor_decl_prop_ids.(i) common_ids then
            match summary_decl_for_prop first_summary prop with
            | Some decl -> decl :: acc
            | None -> acc
          else acc
        in
        loop (i + 1) acc rest
  in
  loop 0 [] first_summary.factor_rule.Stylesheet_intf.declarations

let common_decls_from_summaries summaries first =
  let first_summary =
    match summaries with
    | first_summary :: _ -> first_summary
    | [] -> summarize_factor_rule first
  in
  common_decls_from_props (common_prop_set_of_summaries summaries) first_summary

let common_decl_entries_by_ids common_ids first_summary =
  let rec loop i acc = function
    | [] -> List.rev acc
    | d :: rest ->
        let prop = decl_property d in
        let acc =
          if prop_ids_mem first_summary.factor_decl_prop_ids.(i) common_ids then
            match
              ( summary_decl_for_prop first_summary prop,
                summary_decl_size_for_prop first_summary prop )
            with
            | Some decl, Some size -> (decl, size) :: acc
            | _ -> acc
          else acc
        in
        loop (i + 1) acc rest
  in
  loop 0 [] first_summary.factor_rule.Stylesheet_intf.declarations

let common_decls_size_by_ids common_ids first_summary =
  let entries = common_decl_entries_by_ids common_ids first_summary in
  let decls, size =
    List.fold_right
      (fun (decl, decl_size) (decls, size) -> (decl :: decls, size + decl_size))
      entries ([], 0)
  in
  (decls, size, List.length entries)

let minimum_leftover_size_by_ids common_ids summary =
  let rec loop i kept_count kept_size = function
    | [] -> (kept_count, kept_size)
    | size :: sizes ->
        if prop_ids_mem summary.factor_decl_prop_ids.(i) common_ids then
          loop (i + 1) kept_count kept_size sizes
        else loop (i + 1) (kept_count + 1) (kept_size + size) sizes
  in
  let kept_count, kept_size = loop 0 0 0 summary.factor_decl_sizes in
  if kept_count = 0 then 0
  else rule_size_from_parts summary.factor_selector_size kept_size kept_count

let common_factorable_decls rules first =
  common_decls_from_summaries (List.map summarize_factor_rule rules) first

(* For a rule [R_i] whose value for [prop] equals the default, we must still
   emit it in the leftover when an EARLIER rule [R_j] with overlapping selector
   declares a different value - otherwise [R_j]'s leftover would override the
   shared default for elements matching both. *)
let earlier_overrides_overlap ?(start = 0) ~selector_summaries ~factor_summaries
    ~default i =
  let r_i_summary = Array.get selector_summaries i in
  let default_prop = decl_property default in
  let rec loop j =
    if j >= i then false
    else
      match
        summary_decl_for_prop (Array.get factor_summaries j) default_prop
      with
      | Some d
        when (not (same_minified_declaration d default))
             && Selector_summary.may_overlap
                  (Array.get selector_summaries j)
                  r_i_summary ->
          true
      | _ -> loop (j + 1)
  in
  loop start

let keep_factor_leftover ?(start = 0) ~selector_summaries ~factor_summaries
    ~default_decl ~i decl =
  (not (same_minified_declaration decl default_decl))
  || earlier_overrides_overlap ~start ~selector_summaries ~factor_summaries
       ~default:default_decl i

let leftover_for_factor_rule ?(start = 0) ~common_by_prop ~selector_summaries
    ~factor_summaries i (r : Stylesheet.rule) =
  List.filter
    (fun d ->
      let prop = decl_property d in
      match Prop_map.find_opt prop common_by_prop with
      | None -> true
      | Some default ->
          keep_factor_leftover ~start ~selector_summaries ~factor_summaries
            ~default_decl:default ~i d)
    r.declarations

let factor_leftover_option ?(start = 0) ~common_by_prop ~selector_summaries
    ~factor_summaries i (r : Stylesheet.rule) : Stylesheet.rule option =
  let l =
    leftover_for_factor_rule ~start ~common_by_prop ~selector_summaries
      ~factor_summaries i r
  in
  if l = [] then Option.None else Some { r with declarations = l }

let factor_leftover_options ~common ~selector_summaries ~factor_summaries
    rules_arr =
  let common_by_prop = first_decl_map common in
  Array.mapi
    (fun i r ->
      factor_leftover_option ~common_by_prop ~selector_summaries
        ~factor_summaries i r)
    rules_arr
  |> Array.to_list

let factor_leftover_size ?(start = 0) ~common_by_prop ~selector_summaries
    ~factor_summaries i summary : int option =
  let rec loop kept_count kept_decl_size decls sizes =
    match (decls, sizes) with
    | [], [] -> (kept_count, kept_decl_size)
    | decl :: decls, size :: sizes ->
        let keep =
          let prop = decl_property decl in
          match Prop_map.find_opt prop common_by_prop with
          | None -> true
          | Some default ->
              keep_factor_leftover ~start ~selector_summaries ~factor_summaries
                ~default_decl:default ~i decl
        in
        if keep then loop (kept_count + 1) (kept_decl_size + size) decls sizes
        else loop kept_count kept_decl_size decls sizes
    | _ -> assert false
  in
  let kept_count, kept_decl_size =
    loop 0 0 summary.factor_rule.declarations summary.factor_decl_sizes
  in
  if kept_count = 0 then Option.None
  else
    Some
      (rule_size_from_parts summary.factor_selector_size kept_decl_size
         kept_count)

let factor_safe =
  Factor_safe.v ~same_minified_declaration ~declaration_covers
    ~contains_vendor_pseudo_element ~rule_factor_boundary ~decl_property

let safe_summary summary =
  Factor_safe.summary summary.factor_rule ~selectors:summary.selector_summary

module Factor_interval = struct
  type score = { common : declaration list; member : bool array; saving : int }

  type payload = {
    decls : declaration list;
    decl_size : int;
    decl_count : int;
    prop_ids : int array;
    by_prop : declaration Prop_map.t;
    inline_cost : int;
  }

  let common factor_summaries start common_props : payload option =
    if prop_ids_empty common_props then Option.None
    else
      let common_decls, common_decl_size, common_decl_count =
        common_decls_size_by_ids common_props factor_summaries.(start)
      in
      if common_decls = [] then Option.None
      else
        Some
          {
            decls = common_decls;
            decl_size = common_decl_size;
            decl_count = common_decl_count;
            prop_ids = common_props;
            by_prop = first_decl_map common_decls;
            inline_cost = common_decl_size + common_decl_count;
          }

  let keep_member payload summary (leftover_size : int option) =
    match leftover_size with
    | None -> true
    | Some _ ->
        let exact =
          List.for_all
            (fun cd -> summary_contains_declaration summary cd)
            payload.decls
        in
        (not exact) || summary.factor_selector_size + 1 <= payload.inline_cost

  let seed_contains seed offset =
    match seed with Option.None -> true | Option.Some seed -> seed.(offset)

  let score_members ?seed payload factor_summaries selector_summaries start len
      =
    let member = Array.make len false in
    let leftover_sizes : int option array = Array.make len Option.None in
    let member_count = ref 0 in
    for offset = 0 to len - 1 do
      if seed_contains seed offset then begin
        let i = start + offset in
        let summary = factor_summaries.(i) in
        let leftover_size =
          factor_leftover_size ~start ~common_by_prop:payload.by_prop
            ~selector_summaries ~factor_summaries i summary
        in
        leftover_sizes.(offset) <- leftover_size;
        if keep_member payload summary leftover_size then (
          member.(offset) <- true;
          incr member_count)
      end
    done;
    (member, leftover_sizes, !member_count)

  let summary_rule_size summary =
    rule_size_from_parts summary.factor_selector_size
      summary.factor_decl_pp_size summary.factor_decl_count

  let grouped_size payload factor_summaries start member member_count =
    let selector_size = ref 0 in
    for offset = 0 to Array.length member - 1 do
      if member.(offset) then
        selector_size :=
          !selector_size
          + factor_summaries.(start + offset).factor_selector_size
    done;
    rule_size_from_parts
      (!selector_size + max 0 (member_count - 1))
      payload.decl_size payload.decl_count

  let size payload factor_summaries start member
      (leftover_sizes : int option array) member_count =
    let before_size = ref 0 in
    let after_size =
      ref (grouped_size payload factor_summaries start member member_count)
    in
    for offset = 0 to Array.length member - 1 do
      let summary = factor_summaries.(start + offset) in
      before_size := !before_size + summary_rule_size summary;
      after_size :=
        !after_size
        +
        if member.(offset) then
          match leftover_sizes.(offset) with None -> 0 | Some size -> size
        else summary_rule_size summary
    done;
    !before_size - !after_size

  let optimistic_saving_upper_bound payload factor_summaries start len =
    let fixed = decl_list_size payload.decl_size payload.decl_count + 1 in
    let top1 = ref min_int and top2 = ref min_int in
    let positive_sum = ref 0 and positive_count = ref 0 in
    for offset = 0 to len - 1 do
      let summary = factor_summaries.(start + offset) in
      let min_leftover =
        minimum_leftover_size_by_ids payload.prop_ids summary
      in
      let adjusted =
        summary.factor_size - min_leftover - summary.factor_selector_size - 1
      in
      if adjusted > !top1 then (
        top2 := !top1;
        top1 := adjusted)
      else if adjusted > !top2 then top2 := adjusted;
      if adjusted > 0 then (
        positive_sum := !positive_sum + adjusted;
        incr positive_count)
    done;
    let best = if !positive_count >= 2 then !positive_sum else !top1 + !top2 in
    best - fixed

  let exact_score ?(allow_zero = false) ?seed payload factor_summaries
      selector_summaries start len : score option =
    counters.interval_scored <- counters.interval_scored + 1;
    let member, leftover_sizes, member_count =
      score_members ?seed payload factor_summaries selector_summaries start len
    in
    if member_count < 2 then None
    else
      let saving =
        size payload factor_summaries start member leftover_sizes member_count
      in
      if saving > 0 || (allow_zero && saving = 0) then
        Some { common = payload.decls; member; saving }
      else None

  let score ?(allow_zero = false) factor_summaries selector_summaries start stop
      common_props : score option =
    let len = stop - start + 1 in
    match common factor_summaries start common_props with
    | None -> None
    | Some _ when len < 2 -> None
    | Some payload ->
        let upper_bound =
          optimistic_saving_upper_bound payload factor_summaries start len
        in
        if upper_bound < 0 || ((not allow_zero) && upper_bound = 0) then (
          counters.interval_pruned <- counters.interval_pruned + 1;
          None)
        else
          exact_score ~allow_zero payload factor_summaries selector_summaries
            start len

  let build_scored (rules_arr : Stylesheet.rule array) factor_summaries
      selector_summaries start score =
    let first = rules_arr.(start) in
    let sels =
      let acc : Selector.t list ref = ref [] in
      for offset = Array.length score.member - 1 downto 0 do
        if score.member.(offset) then
          acc := rules_arr.(start + offset).Stylesheet_intf.selector :: !acc
      done;
      !acc
    in
    let grouped =
      {
        first with
        selector = merge_selector_list sels;
        declarations = score.common;
      }
    in
    let common_by_prop = first_decl_map score.common in
    let leftover_rev = ref [] in
    for offset = Array.length score.member - 1 downto 0 do
      let i = start + offset in
      let leftover =
        if score.member.(offset) then
          factor_leftover_option ~start ~common_by_prop ~selector_summaries
            ~factor_summaries i rules_arr.(i)
        else Some rules_arr.(i)
      in
      match leftover with
      | None -> ()
      | Some r -> leftover_rev := r :: !leftover_rev
    done;
    (grouped :: !leftover_rev, score.saving)

  let build ?(allow_zero = false) rules_arr factor_summaries selector_summaries
      start stop common_props : (Stylesheet.rule list * int) option =
    score ~allow_zero factor_summaries selector_summaries start stop
      common_props
    |> Option.map
         (build_scored rules_arr factor_summaries selector_summaries start)

  let candidate ?(allow_zero = false) rules_arr factor_summaries common_props :
      (Stylesheet.rule list * int) option =
    let len = Array.length rules_arr in
    if len < 2 || prop_ids_empty common_props then Option.None
    else
      let selector_summaries =
        Array.map (fun s -> Lazy.force s.selector_summary) factor_summaries
      in
      build ~allow_zero rules_arr factor_summaries selector_summaries 0
        (len - 1) common_props

  let greedy rules_arr factor_summaries =
    match
      candidate ~allow_zero:true rules_arr factor_summaries
        (common_prop_ids_of_array factor_summaries)
    with
    | Some (replacement, saving) -> (replacement, saving)
    | None -> (Array.to_list rules_arr, 0)

  type candidate = { score : score }

  let factor_common_interval_lookahead = 24
  let factor_common_indexed_occurrence_window = 24
  let factor_common_indexed_max_span = 128

  let add_candidate schedule factor_summaries selector_summaries start stop
      common_props =
    if not (prop_ids_empty common_props) then (
      counters.interval_candidates <- counters.interval_candidates + 1;
      match
        score factor_summaries selector_summaries start stop common_props
      with
      | None -> ()
      | Some score ->
          Weighted_interval.add schedule ~start ~stop ~weight:score.saving
            { score })

  let seeded_cascade_safe factor_summaries start seed common =
    let len = Array.length seed in
    let skipped_rev = ref [] in
    let safe = ref true in
    let offset = ref 0 in
    while !safe && !offset < len do
      let summary = factor_summaries.(start + !offset) in
      if seed.(!offset) then
        if
          List.exists
            (fun skipped ->
              Factor_safe.blocks_factor factor_safe common
                (safe_summary summary) (safe_summary skipped))
            !skipped_rev
        then safe := false
        else ()
      else if
        not
          (Factor_safe.can_cross factor_safe (Some common) summary.factor_rule)
      then safe := false
      else skipped_rev := summary :: !skipped_rev;
      incr offset
    done;
    !safe

  let seeded_score factor_summaries selector_summaries start stop seed
      common_props =
    let len = stop - start + 1 in
    if
      len < 2
      || Array.length seed <> len
      || (not seed.(0))
      || not seed.(len - 1)
    then Option.None
    else
      match common factor_summaries start common_props with
      | None -> None
      | Some payload ->
          if seeded_cascade_safe factor_summaries start seed payload.decls then
            exact_score ~seed payload factor_summaries selector_summaries start
              len
          else None

  let add_seeded_candidate schedule factor_summaries selector_summaries ~start
      ~stop ~seed common_props =
    if not (prop_ids_empty common_props) then (
      counters.interval_candidates <- counters.interval_candidates + 1;
      match
        seeded_score factor_summaries selector_summaries start stop seed
          common_props
      with
      | None -> ()
      | Some score ->
          Weighted_interval.add schedule ~start ~stop ~weight:score.saving
            { score })

  type decl_bucket = { decl : declaration; mutable rows_rev : int list }

  let add_decl_occurrence buckets row decl =
    let hash = Declaration.hash decl in
    let bucket_list =
      Hashtbl.find_opt buckets hash |> Option.value ~default:[]
    in
    let rec add acc = function
      | [] ->
          Hashtbl.replace buckets hash
            (List.rev ({ decl; rows_rev = [ row ] } :: acc))
      | bucket :: rest ->
          if same_minified_declaration bucket.decl decl then begin
            bucket.rows_rev <- row :: bucket.rows_rev;
            Hashtbl.replace buckets hash (List.rev_append acc (bucket :: rest))
          end
          else add (bucket :: acc) rest
    in
    add [] bucket_list

  let exact_decl_index factor_summaries =
    let buckets = Hashtbl.create 1024 in
    Array.iteri
      (fun row summary ->
        if rule_factor_eligible summary.factor_rule then begin
          let seen = Hashtbl.create 8 in
          List.iter
            (fun decl ->
              let hash = Declaration.hash decl in
              if not (Hashtbl.mem seen hash) then begin
                Hashtbl.add seen hash ();
                add_decl_occurrence buckets row decl
              end)
            summary.factor_rule.declarations
        end)
      factor_summaries;
    buckets

  let sorted_unique_rows rows =
    let rows = Array.of_list rows in
    Array.sort compare rows;
    let len = Array.length rows in
    if len <= 1 then rows
    else
      let acc = ref [ rows.(0) ] in
      for i = 1 to len - 1 do
        if rows.(i) <> rows.(i - 1) then acc := rows.(i) :: !acc
      done;
      Array.of_list (List.rev !acc)

  let add_indexed_occurrence_slice schedule factor_summaries selector_summaries
      rows first last =
    let start = rows.(first) in
    let stop = rows.(last) in
    let len = stop - start + 1 in
    let count = last - first + 1 in
    if len > count then begin
      let seed = Array.make len false in
      let common_props = ref factor_summaries.(start).factor_prop_ids in
      for i = first to last do
        let row = rows.(i) in
        seed.(row - start) <- true;
        if i <> first then
          common_props :=
            prop_ids_inter !common_props factor_summaries.(row).factor_prop_ids
      done;
      add_seeded_candidate schedule factor_summaries selector_summaries ~start
        ~stop ~seed !common_props
    end

  let add_indexed_candidates schedule factor_summaries selector_summaries =
    let buckets = exact_decl_index factor_summaries in
    Hashtbl.iter
      (fun _ bucket_list ->
        List.iter
          (fun bucket ->
            let rows = sorted_unique_rows bucket.rows_rev in
            let row_count = Array.length rows in
            if row_count >= 2 then
              for first = 0 to row_count - 2 do
                let last_limit =
                  min (row_count - 1)
                    (first + factor_common_indexed_occurrence_window - 1)
                in
                let last = ref (first + 1) in
                while
                  !last <= last_limit
                  && rows.(!last) - rows.(first)
                     <= factor_common_indexed_max_span
                do
                  add_indexed_occurrence_slice schedule factor_summaries
                    selector_summaries rows first !last;
                  incr last
                done
              done)
          bucket_list)
      buckets

  let index factor_summaries selector_summaries =
    let len = Array.length factor_summaries in
    let schedule = Weighted_interval.v ~length:len in
    for start = 0 to len - 2 do
      let common_props = ref factor_summaries.(start).factor_prop_ids in
      let stop = ref (start + 1) in
      let last = min (len - 1) (start + factor_common_interval_lookahead - 1) in
      while !stop <= last && not (prop_ids_empty !common_props) do
        common_props :=
          prop_ids_inter !common_props factor_summaries.(!stop).factor_prop_ids;
        add_candidate schedule factor_summaries selector_summaries start !stop
          !common_props;
        incr stop
      done
    done;
    add_indexed_candidates schedule factor_summaries selector_summaries;
    schedule

  let rewrite rules_arr factor_summaries : (Stylesheet.rule list * int) option =
    let len = Array.length rules_arr in
    if len < 3 then Option.None
    else
      let selector_summaries =
        Array.map (fun s -> Lazy.force s.selector_summary) factor_summaries
      in
      let schedule = index factor_summaries selector_summaries in
      let saving, selected = Weighted_interval.select schedule in
      counters.interval_selected <-
        counters.interval_selected + List.length selected;
      if saving <= 0 || selected = [] then Option.None
      else
        let rec emit i selected acc : (Stylesheet.rule list * int) option =
          if i >= len then Some (List.rev acc, saving)
          else
            match selected with
            | interval :: rest when interval.Weighted_interval.start = i ->
                let replacement, _ =
                  build_scored rules_arr factor_summaries selector_summaries
                    interval.start interval.value.score
                in
                emit (interval.stop + 1) rest (List.rev_append replacement acc)
            | _ -> emit (i + 1) selected (rules_arr.(i) :: acc)
        in
        emit 0 selected []
end

let factorise_group (rules : Stylesheet.rule list) : Stylesheet.rule list =
  match rules with
  | [] | [ _ ] -> rules
  | _ -> (
      let rules_arr = Array.of_list rules in
      let factor_summaries = Array.map summarize_factor_rule rules_arr in
      let greedy_rules, greedy_saving =
        Factor_interval.greedy rules_arr factor_summaries
      in
      match Factor_interval.rewrite rules_arr factor_summaries with
      | Some (dp_rules, dp_saving) when dp_saving > greedy_saving ->
          record_factor_saving dp_saving;
          dp_rules
      | _ ->
          record_factor_saving greedy_saving;
          greedy_rules)

let declarations_overlap common decls =
  Factor_safe.overlap factor_safe common decls

let rule_selector_may_overlap_summary = Factor_safe.selector_overlap
let rule_specificity_ties_on_overlap = Factor_safe.specificity_ties

let skipped_rule_blocks_factor =
 fun common target skipped ->
  Factor_safe.blocks_factor factor_safe common (safe_summary target)
    (safe_summary skipped)

let skipped_blocks_factor_tie common target skipped =
  Factor_safe.blocks_tie factor_safe common (safe_summary target)
    (safe_summary skipped)

let boundary_stops_scan common_opt candidate =
  not (Factor_safe.can_cross factor_safe common_opt candidate)

let rule_gap_merge_eligible (rule : Stylesheet.rule) =
  rule.nested = [] && rule.merge_key = None
  && not (contains_vendor_pseudo_element rule.selector)

(* [merged] is the combined rule a same-selector merge would produce. An
   intervening rule blocks it when it overlaps the selector, writes one of the
   merged rule's properties, and ties on specificity - then source order decides
   that property and the merge would change it. A strictly higher- or lower-
   specificity competitor is decided by specificity (not order), and one writing
   only other properties does not conflict; both are safe to cross. *)
let skipped_blocks_same_selector_merge (merged : Stylesheet.rule)
    (skipped : Stylesheet.rule) =
  rule_selector_may_overlap_summary skipped
    (Selector_summary.of_selector merged.Stylesheet_intf.selector)
  && declarations_overlap merged.declarations skipped.declarations
  && rule_specificity_ties_on_overlap merged skipped

let merge_same_selector_gaps (rules : Stylesheet.rule list) :
    Stylesheet.rule list =
  let items =
    List.map
      (fun (r : Stylesheet.rule) -> (r, canonical_selector_key r.selector))
      rules
  in
  let rec scan_for_match anchor anchor_key skipped_rev fuel
      (items : (Stylesheet.rule * Selector.t list) list) :
      (Stylesheet.rule list * (Stylesheet.rule * Selector.t list) list) option =
    match items with
    | [] -> Option.None
    | _ when fuel <= 0 -> Option.None
    | (candidate, candidate_key) :: tail ->
        if not (rule_gap_merge_eligible candidate) then Option.None
        else if anchor_key = candidate_key then
          let skipped = List.rev skipped_rev in
          let merged = merge_two_adjacent_rules anchor candidate in
          (* Check the merged rule (anchor + candidate declarations) against the
             intervening rules: a tie on any property the merged rule writes
             makes the merge observable. *)
          if List.exists (skipped_blocks_same_selector_merge merged) skipped
          then Option.None
          else
            let before = anchor :: (skipped @ [ candidate ]) in
            let after = merged :: skipped in
            if rules_pp_size after < rules_pp_size before then
              Some (merged :: skipped, tail)
            else Option.None
        else
          scan_for_match anchor anchor_key (candidate :: skipped_rev) (fuel - 1)
            tail
  in
  let rec walk acc = function
    | [] -> List.rev acc
    | (rule, key) :: rest -> (
        if not (rule_gap_merge_eligible rule) then walk (rule :: acc) rest
        else
          match scan_for_match rule key [] 128 rest with
          | None -> walk (rule :: acc) rest
          | Some (replacement, tail) ->
              walk (List.rev_append replacement acc) tail)
  in
  preserve_list rules (walk [] items)

let filter_some xs = List.filter_map (fun x -> x) xs

let split_at n xs =
  let rec loop n acc xs =
    if n <= 0 then (List.rev acc, xs)
    else
      match xs with
      | [] -> (List.rev acc, [])
      | x :: xs -> loop (n - 1) (x :: acc) xs
  in
  loop n [] xs

let factor_rules_with_skips factor_rules skipped : Stylesheet.rule list option =
  match factor_rules with
  | [] | [ _ ] -> Option.None
  | first :: _ -> (
      let rules_arr = Array.of_list factor_rules in
      let factor_summaries = Array.map summarize_factor_rule rules_arr in
      let selector_summaries =
        Array.map (fun s -> Lazy.force s.selector_summary) factor_summaries
      in
      let common =
        common_decls_from_props
          (common_props_of_array factor_summaries)
          factor_summaries.(0)
      in
      if common = [] then Option.None
      else
        let leftovers =
          factor_leftover_options ~common ~selector_summaries ~factor_summaries
            rules_arr
          |> Array.of_list
        in
        match
          cost_aware_factor_group first rules_arr factor_summaries common
            leftovers
        with
        | None -> Option.None
        | Some (grouped, leftovers) ->
            let leftover_options = Array.to_list leftovers in
            let current_count = List.length factor_rules - 1 in
            let current_leftovers, target_leftovers =
              split_at current_count leftover_options
            in
            let after =
              (grouped :: filter_some current_leftovers)
              @ skipped
              @ filter_some target_leftovers
            in
            let before = factor_rules @ skipped in
            let before_size = rules_pp_size before in
            let after_size = rules_pp_size after in
            if after_size < before_size then begin
              record_factor_saving (before_size - after_size);
              Some after
            end
            else Option.None)

type gap_entry = Factor of factor_rule_summary | Skip of factor_rule_summary

let size_of_gap_entry = function Factor s | Skip s -> s.factor_size

(* Size of [first :: entry rules] from the cached per-summary sizes, so a scan
   that re-evaluates a growing prefix does not re-render each rule every
   step. *)
let gap_before_size first entries =
  List.fold_left
    (fun acc e -> acc + size_of_gap_entry e)
    (rule_pp_size first) entries

let factor_rules_of_gap first entries =
  first
  :: List.filter_map
       (function Factor s -> Some s.factor_rule | Skip _ -> None)
       entries

let factor_summaries_of_gap first_summary entries =
  first_summary
  :: List.filter_map (function Factor s -> Some s | Skip _ -> None) entries

let factor_gap_rewrite first entries : (Stylesheet.rule list * int) option =
  let factor_rules = factor_rules_of_gap first entries in
  match factor_rules with
  | [] | [ _ ] -> Option.None
  | _ -> (
      let rules_arr = Array.of_list factor_rules in
      let factor_summaries = Array.map summarize_factor_rule rules_arr in
      let selector_summaries =
        Array.map (fun s -> Lazy.force s.selector_summary) factor_summaries
      in
      let common =
        common_decls_from_props
          (common_props_of_array factor_summaries)
          factor_summaries.(0)
      in
      if common = [] then Option.None
      else
        let leftovers =
          factor_leftover_options ~common ~selector_summaries ~factor_summaries
            rules_arr
          |> Array.of_list
        in
        match
          cost_aware_factor_group first rules_arr factor_summaries common
            leftovers
        with
        | None -> Option.None
        | Some (grouped, leftovers) ->
            let next_factor = ref 1 in
            let entry_after = function
              | Skip s -> Some s.factor_rule
              | Factor _ ->
                  let leftover = leftovers.(!next_factor) in
                  incr next_factor;
                  leftover
            in
            let after =
              grouped
              :: (filter_some [ leftovers.(0) ]
                 @ List.filter_map entry_after entries)
            in
            let before_size = gap_before_size first entries in
            let after_size = rules_pp_size after in
            if after_size < before_size then
              Some (after, before_size - after_size)
            else Option.None)

let common_equal_decls rules (first : Stylesheet.rule) =
  let summaries = List.map summarize_factor_rule rules in
  List.filter
    (fun decl ->
      List.for_all (fun s -> summary_contains_declaration s decl) summaries)
    first.Stylesheet_intf.declarations

(* [common] is the declaration list the scan has already proven present in each
   factored rule and safe across skipped rules. Reusing it avoids recomputing
   the same intersection for every improving equal-anchor prefix. *)
let factor_gap_equal_rewrite ~common first_summary entries :
    (Stylesheet.rule list * int) option =
  let first = first_summary.factor_rule in
  let factor_summaries = factor_summaries_of_gap first_summary entries in
  match factor_summaries with
  | [] | [ _ ] -> Option.None
  | _ -> (
      let factor_summaries = Array.of_list factor_summaries in
      let rules_arr = Array.map (fun s -> s.factor_rule) factor_summaries in
      let selector_summaries =
        Array.map (fun s -> Lazy.force s.selector_summary) factor_summaries
      in
      if common = [] then Option.None
      else
        let leftovers =
          factor_leftover_options ~common ~selector_summaries ~factor_summaries
            rules_arr
          |> Array.of_list
        in
        match
          cost_aware_factor_group first rules_arr factor_summaries common
            leftovers
        with
        | None -> Option.None
        | Some (grouped, leftovers) ->
            let next_factor = ref 1 in
            let entry_after = function
              | Skip s -> Some s.factor_rule
              | Factor _ ->
                  let leftover = leftovers.(!next_factor) in
                  incr next_factor;
                  leftover
            in
            let after =
              grouped
              :: (filter_some [ leftovers.(0) ]
                 @ List.filter_map entry_after entries)
            in
            let before_size = gap_before_size first entries in
            let after_size = rules_pp_size after in
            if after_size < before_size then
              Some (after, before_size - after_size)
            else Option.None)

let factor_anchor_common first candidate
    (common : Declaration.declaration list option) =
  match common with
  | None -> common_factorable_decls [ first; candidate.factor_rule ] first
  | Some common ->
      if factor_rule_declares_all candidate (List.map decl_property common) then
        common
      else []

let better_factor_gap
    (best :
      (Stylesheet.rule list
      * (Stylesheet.rule * factor_rule_summary) list
      * int)
      option) replacement tail savings =
  match best with
  | None -> Some (replacement, tail, savings)
  | Some (_, _, best_savings) when savings > best_savings ->
      Some (replacement, tail, savings)
  | Some _ -> best

let equal_factor_lookahead = 32

let single_anchor_blocked common candidate_summary entries_rev =
  common = []
  || List.exists
       (fun entry ->
         match entry with
         | Factor _ -> false
         | Skip skipped ->
             skipped_rule_blocks_factor common candidate_summary skipped)
       entries_rev

let update_single_anchor_best first entries_rev tail best candidate_summary =
  let entries = List.rev (Factor candidate_summary :: entries_rev) in
  match factor_gap_rewrite first entries with
  | Some (replacement, savings) ->
      better_factor_gap best replacement tail savings
  | None -> best

let try_single_anchor_indexed prefix first first_summary rest_summaries =
  (* [rest_summaries] is threaded as the tail in [better_factor_gap] so callers
     with a precomputed summary index can continue without rebuilding it. *)
  let first_bloom = first_summary.decl_bloom in
  let rec scan entries_rev (common : Declaration.declaration list option)
      (best :
        (Stylesheet.rule list
        * (Stylesheet.rule * factor_rule_summary) list
        * int)
        option) fuel = function
    | [] -> best
    | _ when fuel <= 0 -> best
    | (candidate, candidate_summary) :: tail ->
        if rule_factor_boundary candidate then best
        else if not (rule_factor_eligible candidate) then
          scan
            (Skip candidate_summary :: entries_rev)
            common best (fuel - 1) tail
        else if candidate_summary.decl_bloom land first_bloom = 0 then
          (* Bloom prefilter: candidate's declarations share no hash with the
             anchor's, so [factor_anchor_common] would yield []. Skip the check
             entirely. *)
          scan
            (Skip candidate_summary :: entries_rev)
            common best (fuel - 1) tail
        else
          let candidate_common =
            factor_anchor_common first candidate_summary common
          in
          if
            single_anchor_blocked candidate_common candidate_summary entries_rev
          then
            scan
              (Skip candidate_summary :: entries_rev)
              common best (fuel - 1) tail
          else
            let best =
              update_single_anchor_best first entries_rev tail best
                candidate_summary
            in
            scan
              (Factor candidate_summary :: entries_rev)
              (Some candidate_common) best (fuel - 1) tail
  in
  match
    scan [] Option.None Option.None equal_factor_lookahead rest_summaries
  with
  | None -> Option.None
  | Some (replacement, tail, _) -> Some (prefix @ replacement, tail)

let equal_anchor_common first candidate
    (common : Declaration.declaration list option) =
  match common with
  | None -> common_equal_decls [ first; candidate.factor_rule ] first
  | Some common ->
      (* Cached Bloom filter on the candidate's summary lets us drop any [decl]
         from [common] whose hash isn't a possible member in O(1) -- a single
         bit-AND -- instead of walking the candidate's declarations;
         declarations whose hash hits the bloom still need the structural
         [same_minified_declaration] check to disambiguate filter collisions,
         which are correctness-preserving. *)
      List.filter (summary_contains_declaration candidate) common

(* Returns [Some (replacement, tail, savings)] - the [savings] is the strict
   output-size reduction [factor_gap_equal_rewrite] already computed, surfaced
   so the best-first scheduler can prioritise without re-rendering. *)
let rec scan_equal_anchor ~first_bloom first_summary entries_rev common best
    fuel = function
  | [] -> best
  | _ when fuel <= 0 -> best
  | (candidate, candidate_summary) :: tail ->
      let first = first_summary.factor_rule in
      if boundary_stops_scan common candidate then best
      else if not (rule_factor_eligible candidate) then
        scan_equal_anchor ~first_bloom first_summary
          (Skip candidate_summary :: entries_rev)
          common best (fuel - 1) tail
      else if candidate_summary.decl_bloom land first_bloom = 0 then
        (* Bloom prefilter: no declaration of the candidate shares a
           [Declaration.hash] with any declaration of the anchor, so the common
           subset is necessarily empty. Skip the [equal_anchor_common] /
           [blocks] checks and treat the candidate as a skipped rule. *)
        scan_equal_anchor ~first_bloom first_summary
          (Skip candidate_summary :: entries_rev)
          common best (fuel - 1) tail
      else
        let candidate_common =
          equal_anchor_common first candidate_summary common
        in
        let blocks =
          candidate_common = []
          || List.exists
               (fun entry ->
                 match entry with
                 | Factor _ -> false
                 | Skip skipped ->
                     skipped_blocks_factor_tie candidate_common
                       candidate_summary skipped)
               entries_rev
        in
        if blocks then
          scan_equal_anchor ~first_bloom first_summary
            (Skip candidate_summary :: entries_rev)
            common best (fuel - 1) tail
        else
          let entries = List.rev (Factor candidate_summary :: entries_rev) in
          let best =
            match
              factor_gap_equal_rewrite ~common:candidate_common first_summary
                entries
            with
            | Some (replacement, savings) ->
                better_factor_gap best replacement tail savings
            | None -> best
          in
          scan_equal_anchor ~first_bloom first_summary
            (Factor candidate_summary :: entries_rev)
            (Some candidate_common) best (fuel - 1) tail

let seed_bloom : Declaration.declaration list option -> int option = function
  | None -> Option.None
  | Some decls ->
      Some
        (List.fold_left
           (fun bloom decl -> bloom_add bloom (Declaration.hash decl))
           0 decls)

let try_factor_equal_anchor ~shared_decl (first : Stylesheet.rule) first_summary
    rest_summaries =
  (* The auto-narrowing chain ([None]) commits to the first sharer's common and
     handles multi-property blocks, but it loses a single-property group when an
     earlier rule shares a different anchor declaration (the running common
     narrows away from the property the later cluster shares). Seeding the scan
     with each shared individual anchor declaration recovers those groups; the
     best factoring across all seeds wins. Non-shared declarations cannot be
     factored, and a seed-specific Bloom avoids scanning candidates that cannot
     contain the seeded declaration. *)
  let seeds : Declaration.declaration list option list =
    let shared_decls = List.filter shared_decl first.declarations in
    Option.None :: List.map (fun d -> Some [ d ]) shared_decls
  in
  let first_bloom = first_summary.decl_bloom in
  List.fold_left
    (fun best seed ->
      let first_bloom = Option.value (seed_bloom seed) ~default:first_bloom in
      match
        scan_equal_anchor ~first_bloom first_summary [] seed Option.None
          equal_factor_lookahead rest_summaries
      with
      | None -> best
      | Some (replacement, tail, savings) ->
          better_factor_gap best replacement tail savings)
    Option.None seeds

let suffix_prop_ids summaries =
  let rec loop acc = function
    | [] -> acc
    | summary :: rest ->
        let props =
          match acc with
          | [] -> summary.factor_prop_ids
          | next_props :: _ -> prop_ids_inter summary.factor_prop_ids next_props
        in
        loop (props :: acc) rest
  in
  loop [] (List.rev summaries)

let try_group_suffix_against_rest ~prefix_rev ~current ~common_props
    ~current_common rest :
    (Stylesheet.rule list * (Stylesheet.rule * factor_rule_summary) list) option
    =
  let rec scan skipped_rev fuel :
      (Stylesheet.rule * factor_rule_summary) list ->
      (Stylesheet.rule list * (Stylesheet.rule * factor_rule_summary) list)
      option = function
    | _ when fuel <= 0 -> None
    | (candidate, candidate_summary) :: tail ->
        if rule_factor_boundary candidate then None
        else if not (rule_factor_eligible candidate) then
          scan (candidate_summary :: skipped_rev) (fuel - 1) tail
        else if
          factor_rule_declares_prop_ids candidate_summary common_props
          && not
               (List.exists
                  (skipped_rule_blocks_factor current_common candidate_summary)
                  skipped_rev)
        then
          let skipped = List.rev_map (fun s -> s.factor_rule) skipped_rev in
          let factor_rules = List.map fst current @ [ candidate ] in
          match factor_rules_with_skips factor_rules skipped with
          | Some replacement -> Some (List.rev prefix_rev @ replacement, tail)
          | None -> None
        else scan (candidate_summary :: skipped_rev) (fuel - 1) tail
    | [] -> None
  in
  if current_common = [] then None else scan [] 128 rest

let try_group_indexed_lookahead current rest :
    (Stylesheet.rule list * (Stylesheet.rule * factor_rule_summary) list) option
    =
  let current = List.rev current in
  let current_summaries = List.map snd current in
  let current_suffix_props = suffix_prop_ids current_summaries in
  let rec try_suffix prefix_rev current summaries suffix_props :
      (Stylesheet.rule list * (Stylesheet.rule * factor_rule_summary) list)
      option =
    match current with
    | [] -> Option.None
    | [ (first, first_summary) ] ->
        try_single_anchor_indexed (List.rev prefix_rev) first first_summary rest
    | (first, _) :: tail_current -> (
        match (summaries, suffix_props) with
        | first_summary :: tail_summaries, common_props :: tail_suffix_props
          -> (
            let current_common =
              common_decls_from_ids common_props first_summary
            in
            match
              try_group_suffix_against_rest ~prefix_rev ~current ~common_props
                ~current_common rest
            with
            | Some _ as result -> result
            | None ->
                try_suffix (first :: prefix_rev) tail_current tail_summaries
                  tail_suffix_props)
        | _ -> Option.None)
  in
  try_suffix [] current current_summaries current_suffix_props

let try_extend_factored_rule anchor rest :
    (Stylesheet.rule list * (Stylesheet.rule * factor_rule_summary) list) option
    =
  let common = anchor.factor_rule.declarations in
  let common_props = List.map decl_property common in
  let rec scan skipped_rev fuel :
      (Stylesheet.rule * factor_rule_summary) list ->
      (Stylesheet.rule list * (Stylesheet.rule * factor_rule_summary) list)
      option = function
    | [] -> Option.None
    | _ when fuel <= 0 -> Option.None
    | (candidate, candidate_summary) :: tail ->
        if rule_factor_boundary candidate then Option.None
        else if not (rule_factor_eligible candidate) then
          scan (candidate_summary :: skipped_rev) (fuel - 1) tail
        else if
          factor_rule_declares_all candidate_summary common_props
          && not
               (List.exists
                  (skipped_rule_blocks_factor common candidate_summary)
                  skipped_rev)
        then
          let skipped = List.rev_map (fun s -> s.factor_rule) skipped_rev in
          let factor_rules =
            [ anchor.factor_rule; candidate_summary.factor_rule ]
          in
          match factor_rules_with_skips factor_rules skipped with
          | None -> Option.None
          | Some replacement -> Some (replacement, tail)
        else if
          skipped_rule_blocks_factor common candidate_summary candidate_summary
        then Option.None
        else scan (candidate_summary :: skipped_rev) (fuel - 1) tail
  in
  if common = [] then Option.None else scan [] 128 rest

let extend_factored_declarations (rules : Stylesheet.rule list) :
    Stylesheet.rule list =
  let items = List.map (fun r -> (r, summarize_factor_rule r)) rules in
  let rec walk acc = function
    | [] -> List.rev acc
    | (r, summary) :: rest -> (
        if not (rule_factor_eligible r) then walk (r :: acc) rest
        else
          match try_extend_factored_rule summary rest with
          | None -> walk (r :: acc) rest
          | Some (replacement, tail) ->
              walk (List.rev_append replacement acc) tail)
  in
  preserve_list rules (walk [] items)

let rule_identical_extend_eligible (r : Stylesheet.rule) =
  rule_factor_eligible r

let can_extend_identical_rule ~anchor_summary ~candidate_summary anchor
    candidate =
  let anchor : Stylesheet.rule = anchor in
  let candidate : Stylesheet.rule = candidate in
  (* Bloom prefilter: two rules with different declaration-hash sets cannot have
     structurally equal declaration lists. Avoids the full
     [declarations_css_equal] walk on the common rejected case. *)
  anchor_summary.factor_decl_count = candidate_summary.factor_decl_count
  && anchor_summary.decl_bloom = candidate_summary.decl_bloom
  && declarations_css_equal anchor.declarations candidate.declarations
  && extract_pseudo_element anchor.selector
     = extract_pseudo_element candidate.selector
  && newer_pseudo_class_compatible anchor.selector candidate.selector

let try_extend_identical_rule ~ctx anchor_summary rest =
  let anchor = anchor_summary.factor_rule in
  let common = anchor_summary.factor_rule.declarations in
  let skipped_rule_blocks =
    match ctx.scope with
    | `Stylesheet -> skipped_blocks_factor_tie
    | `Fragment -> skipped_rule_blocks_factor
  in
  let rec scan skipped_rev fuel = function
    | [] -> (None : _ option)
    | _ when fuel <= 0 -> (None : _ option)
    | (candidate, candidate_summary) :: tail ->
        if not (rule_identical_extend_eligible candidate) then (None : _ option)
        else if
          can_extend_identical_rule ~anchor_summary ~candidate_summary anchor
            candidate
          && not
               (List.exists
                  (skipped_rule_blocks common candidate_summary)
                  skipped_rev)
        then
          let skipped = List.rev_map (fun s -> s.factor_rule) skipped_rev in
          let grouped =
            {
              anchor with
              selector =
                merge_selector_list
                  [ anchor.selector; candidate_summary.factor_rule.selector ];
            }
          in
          let before =
            (anchor :: skipped) @ [ candidate_summary.factor_rule ]
          in
          let after = grouped :: skipped in
          let before_size = rules_pp_size before in
          let after_size = rules_pp_size after in
          if after_size < before_size then begin
            record_factor_saving (before_size - after_size);
            Some (after, tail)
          end
          else (None : _ option)
        else scan (candidate_summary :: skipped_rev) (fuel - 1) tail
  in
  if common = [] then
    (None
      : (Stylesheet.rule list * (Stylesheet.rule * factor_rule_summary) list)
        option)
  else scan [] 128 rest

let extend_identical_declaration_rules ~ctx (rules : Stylesheet.rule list) :
    Stylesheet.rule list =
  let items = List.map (fun r -> (r, summarize_factor_rule r)) rules in
  let rec walk acc = function
    | [] -> List.rev acc
    | (r, summary) :: rest -> (
        if not (rule_identical_extend_eligible r) then walk (r :: acc) rest
        else
          match try_extend_identical_rule ~ctx summary rest with
          | None -> walk (r :: acc) rest
          | Some (replacement, tail) ->
              walk (List.rev_append replacement acc) tail)
  in
  preserve_list rules (walk [] items)

(* [r] shares a property with the whole current group iff some property it
   declares is declared by every group member - i.e. [r]'s property set meets
   the running intersection of the group's property sets. Tracking that
   intersection avoids the per-pair [decl_property] rescans of the group. *)
let factor_common_declarations (rules : Stylesheet.rule list) :
    Stylesheet.rule list =
  let items = List.map (fun r -> (r, summarize_factor_rule r)) rules in
  let factorise_items items = factorise_group (List.rev_map fst items) in
  let rec group acc current current_props = function
    | [] -> List.rev_append (factorise_items current) acc
    | (r, summary) :: rest -> (
        if not (rule_factor_eligible r) then
          let acc = List.rev_append (factorise_items current) acc in
          group (r :: acc) [] [||] rest
        else
          let r_props = summary.factor_prop_ids in
          let shares =
            match current with
            | [] -> not (prop_ids_empty r_props)
            | _ -> not (prop_ids_disjoint r_props current_props)
          in
          if shares then
            let current_props =
              match current with
              | [] -> r_props
              | _ -> prop_ids_inter current_props r_props
            in
            group acc ((r, summary) :: current) current_props rest
          else
            match
              try_group_indexed_lookahead current ((r, summary) :: rest)
            with
            | Some (replacement, tail) ->
                group (List.rev_append replacement acc) [] [||] tail
            | None ->
                let acc = List.rev_append (factorise_items current) acc in
                group acc [ (r, summary) ] r_props rest)
  in
  preserve_list rules (List.rev (group [] [] [||] items))

(* Priority search queue keyed by pool node (stable id), ordered so the
   highest-savings factoring is the minimum and thus pops first; ties break by
   earlier id to keep the left-to-right preference of the original list pass. *)
module Factor_queue =
  Psq.Make
    (struct
      type t = Rule_pool.node

      let compare a b = Int.compare (Rule_pool.id a) (Rule_pool.id b)
    end)
    (struct
      type t = int * int

      let compare (s1, i1) (s2, i2) =
        let c = Int.compare s2 s1 in
        if c <> 0 then c else Int.compare i1 i2
    end)

(* One pass of gap-factoring: hoist a declaration shared across rules with
   non-conflicting selectors, even across intervening rules, when cascade-safe
   and smaller. Scheduling is best-first (the greedy weight order of SatCSS,
   Hague, Lin & Hong, TOPLAS 2019) over globally indexed physical intervals:
   each queued anchor stores the exact live-node interval it scored over. On
   pop, a still-live interval can be applied directly; stale intervals fall back
   to exact re-scoring. That keeps correctness tied to physical node identity
   while avoiding the old score-on-enqueue, score-again-on-pop hot path. *)
(* The next [k] live nodes after [node], in pool order. *)
let rec factor_window node k acc =
  if k <= 0 then List.rev acc
  else
    match Rule_pool.next node with
    | None -> List.rev acc
    | Some m -> factor_window m (k - 1) (m :: acc)

let rec factor_take n = function
  | x :: xs when n > 0 -> x :: factor_take (n - 1) xs
  | _ -> []

(* Pool-wide declaration index keyed on [Declaration.hash] (precomputed at
   construction, so the lookup is one int compare instead of walking the AST).
   For each declaration hash, record how many distinct rules in the pool carry a
   matching declaration -- an anchor whose every declaration appears in exactly
   one rule cannot be factored at all and we skip the full [factor_window] /
   [scan] cost. False positives from hash collisions are harmless: a
   colliding-but-different declaration may let an unfactorable anchor through,
   and the scan will reject it at the usual miss cost. *)
let build_shared_decl_predicate pool =
  let counts : (int, int) Hashtbl.t = Hashtbl.create 1024 in
  List.iter
    (fun n ->
      let r = Rule_pool.rule n in
      let seen = Hashtbl.create 8 in
      List.iter
        (fun d -> Hashtbl.replace seen (Declaration.hash d) ())
        r.Stylesheet_intf.declarations;
      Hashtbl.iter
        (fun h () ->
          let c = try Hashtbl.find counts h with Not_found -> 0 in
          Hashtbl.replace counts h (c + 1))
        seen)
    (Rule_pool.nodes pool);
  fun d ->
    match Hashtbl.find_opt counts (Declaration.hash d) with
    | Some c -> c > 1
    | None -> false

(* Score an anchor node: [None] when it cannot factor, else the replacement
   rules, the nodes the factoring consumes, and the bytes it saves. *)
let factor_anchor_score ~shared_decl n =
  counters.anchors_scored <- counters.anchors_scored + 1;
  let r = Rule_pool.rule n in
  if not (rule_factor_eligible r) then (None : _ option)
  else if
    (* Cheap pre-filter: factor_anchor needs at least one of this anchor's
       declarations to appear verbatim in some other live rule. *)
    not (List.exists shared_decl r.declarations)
  then (
    counters.anchors_prefiltered <- counters.anchors_prefiltered + 1;
    (None : _ option))
  else
    let win_nodes = factor_window n equal_factor_lookahead [] in
    let win_summaries =
      List.map
        (fun n ->
          let rule = Rule_pool.rule n in
          (rule, summarize_factor_rule rule))
        win_nodes
    in
    let summary = summarize_factor_rule r in
    match try_factor_equal_anchor ~shared_decl r summary win_summaries with
    | None -> (None : _ option)
    | Some (replacement, tail, savings) ->
        let consumed =
          factor_take (List.length win_summaries - List.length tail) win_nodes
        in
        Some (replacement, consumed, savings)

type anchor_candidate = {
  anchor : Rule_pool.node;
  replacement : Stylesheet.rule list;
  consumed : Rule_pool.node list;
  saving : int;
}

type anchor_scheduler = {
  pool : Rule_pool.t;
  shared_decl : declaration -> bool;
  frontier : Factor_queue.t ref;
  candidates : (int, anchor_candidate) Hashtbl.t;
  applied : int ref;
}

let anchor_candidate_live candidate =
  let rec loop previous = function
    | [] -> true
    | node :: rest -> (
        match Rule_pool.next previous with
        | Some next when Rule_pool.is_live node && next == node ->
            loop node rest
        | _ -> false)
  in
  Rule_pool.is_live candidate.anchor && loop candidate.anchor candidate.consumed

let anchor_candidate ~shared_decl anchor =
  match factor_anchor_score ~shared_decl anchor with
  | Some (replacement, consumed, savings) when savings > 0 ->
      Some { anchor; replacement; consumed; saving = savings }
  | _ -> None

let forget_anchor_candidate candidates candidate =
  Hashtbl.remove candidates (Rule_pool.id candidate.anchor);
  List.iter
    (fun node -> Hashtbl.remove candidates (Rule_pool.id node))
    candidate.consumed

let enqueue_anchor_candidate scheduler n =
  if Rule_pool.is_live n then
    match anchor_candidate ~shared_decl:scheduler.shared_decl n with
    | Some candidate ->
        Hashtbl.replace scheduler.candidates (Rule_pool.id n) candidate;
        scheduler.frontier :=
          Factor_queue.add n
            (candidate.saving, Rule_pool.id n)
            !(scheduler.frontier)
    | None ->
        Hashtbl.remove scheduler.candidates (Rule_pool.id n);
        scheduler.frontier := Factor_queue.remove n !(scheduler.frontier)

let apply_anchor_candidate scheduler candidate =
  incr scheduler.applied;
  counters.factorings_applied <- counters.factorings_applied + 1;
  record_factor_saving candidate.saving;
  let added =
    List.map
      (fun r -> Rule_pool.insert_before scheduler.pool candidate.anchor r)
      candidate.replacement
  in
  forget_anchor_candidate scheduler.candidates candidate;
  Rule_pool.remove scheduler.pool candidate.anchor;
  List.iter (Rule_pool.remove scheduler.pool) candidate.consumed;
  added

let rescore_anchor_candidate scheduler n =
  match anchor_candidate ~shared_decl:scheduler.shared_decl n with
  | Some candidate ->
      Hashtbl.replace scheduler.candidates (Rule_pool.id n) candidate;
      scheduler.frontier :=
        Factor_queue.add n
          (candidate.saving, Rule_pool.id n)
          !(scheduler.frontier)
  | None -> Hashtbl.remove scheduler.candidates (Rule_pool.id n)

let drain_anchor_scheduler scheduler =
  let rec loop () =
    match Factor_queue.pop !(scheduler.frontier) with
    | None -> ()
    | Some ((n, (stored, _)), rest) -> (
        scheduler.frontier := rest;
        if not (Rule_pool.is_live n) then (
          Hashtbl.remove scheduler.candidates (Rule_pool.id n);
          loop ())
        else
          match Hashtbl.find_opt scheduler.candidates (Rule_pool.id n) with
          | Some candidate
            when candidate.saving = stored && anchor_candidate_live candidate ->
              let added = apply_anchor_candidate scheduler candidate in
              List.iter (enqueue_anchor_candidate scheduler) added;
              loop ()
          | _ ->
              rescore_anchor_candidate scheduler n;
              loop ())
  in
  loop ()

let anchor_scheduler pool shared_decl =
  {
    pool;
    shared_decl;
    frontier = ref Factor_queue.empty;
    candidates = Hashtbl.create (Rule_pool.length pool);
    applied = ref 0;
  }

let factor_anchor_gaps (rules : Stylesheet.rule list) : Stylesheet.rule list =
  let pool = Rule_pool.of_rules rules in
  let scheduler = anchor_scheduler pool (build_shared_decl_predicate pool) in
  List.iter (enqueue_anchor_candidate scheduler) (Rule_pool.nodes pool);
  drain_anchor_scheduler scheduler;
  if !(scheduler.applied) > 0 then
    Log.debug (fun m ->
        m "factor_anchor_gaps: applied %d factorings" !(scheduler.applied));
  preserve_list rules (Rule_pool.to_rules pool)

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
let rec needs_layer_merge = function
  | Layer (Some prev_name, _) :: Layer (Some name, _) :: _
    when String.equal prev_name name ->
      true
  | _ :: rest -> needs_layer_merge rest
  | [] -> false

let merge_layer_blocks ~optimize_merged_block stmts =
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

let merge_consecutive_layers ~optimize_merged_block (stmts : statement list) :
    statement list =
  if needs_layer_merge stmts then
    merge_layer_blocks ~optimize_merged_block stmts
  else stmts

let rec needs_media_merge = function
  | Media (prev_cond, prev_block) :: Media (cond, block) :: _
    when Media.equal prev_cond cond
         && not
              (has_nested_preference_media prev_block
              || has_nested_preference_media block) ->
      true
  | _ :: rest -> needs_media_merge rest
  | [] -> false

let merge_media_blocks ~optimize_merged_block stmts =
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
               && not
                    (has_nested_preference_media prev_block
                    || has_nested_preference_media block) ->
            merge acc (Some (cond, prev_block @ block)) rest
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

let merge_consecutive_media ~optimize_merged_block (stmts : statement list) :
    statement list =
  if needs_media_merge stmts then
    merge_media_blocks ~optimize_merged_block stmts
  else stmts

(* CSS Conditional Rules 5: adjacent same-condition [@supports] / [@container]
   blocks may be merged because the cascade evaluates them identically. Mirror
   the [@media] approach. *)
let merge_consecutive_supports ~optimize_merged_block (stmts : statement list) :
    statement list =
  let rec needs_merge = function
    | Supports (prev_cond, _) :: Supports (cond, _) :: _
      when Supports.equal prev_cond cond ->
        true
    | _ :: rest -> needs_merge rest
    | [] -> false
  in
  if not (needs_merge stmts) then stmts
  else
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
    | Some a, Some b -> Container.compare a b
    | Some _, None -> 1
    | None, Some _ -> -1
    | None, None -> 0
  in
  let rec needs_merge = function
    | Container (prev_name, prev_cond, _) :: Container (name, cond, _) :: _
      when prev_name = name && compare_condition prev_cond cond = 0 ->
        true
    | _ :: rest -> needs_merge rest
    | [] -> false
  in
  if not (needs_merge stmts) then stmts
  else
    (* [acc] holds output in REVERSE order; reverse once at the end. *)
    let rec merge acc prev = function
      | [] -> (
          match prev with
          | Some (name, cond, block) ->
              List.rev
                (Container (name, cond, optimize_merged_block block) :: acc)
          | None -> List.rev acc)
      | Container (name, cond, block) :: rest -> (
          match prev with
          | Some (prev_name, prev_cond, prev_block)
            when prev_name = name && compare_condition prev_cond cond = 0 ->
              merge acc (Some (name, cond, prev_block @ block)) rest
          | Some (prev_name, prev_cond, prev_block) ->
              merge
                (Container
                   (prev_name, prev_cond, optimize_merged_block prev_block)
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
    list_filter_preserve
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
    | (Layer_decl names as stmt) :: rest ->
        let names' = dedup_preserving_order names in
        let stmt = if names' == names then stmt else Layer_decl names' in
        merge (stmt :: acc) rest
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
  list_filter_preserve
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
  list_filter_preserve
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
  let has_merge = ref false in
  List.iter
    (fun stmt ->
      match stmt with
      | Layer (Some name, block) when not (is_empty_block block) ->
          let prev =
            Hashtbl.find_opt content name |> Option.value ~default:[]
          in
          if prev <> [] then has_merge := true;
          Hashtbl.replace content name (prev @ block);
          if not (Hashtbl.mem first_nonempty name) then
            Hashtbl.add first_nonempty name ()
      | _ -> ())
    stmts;
  if not !has_merge then stmts
  else
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
module Suffix_rule_cover = struct
  module Selector_tbl = Hashtbl.Make (struct
    type t = Selector.t

    let equal = ( = )
    let hash = Hashtbl.hash
  end)

  let v () = Selector_tbl.create 256
  let empty = (Prop_set.empty, Prop_set.empty)

  let find t selector =
    Option.value ~default:empty (Selector_tbl.find_opt t selector)

  let covered t selector decl =
    let normal, important = find t selector in
    let prop = decl_property decl in
    if Declaration.is_important decl then Prop_set.mem prop important
    else Prop_set.mem prop normal || Prop_set.mem prop important

  let add t selector decl =
    let normal, important = find t selector in
    let prop = decl_property decl in
    let cover =
      if Declaration.is_important decl then (normal, Prop_set.add prop important)
      else (Prop_set.add prop normal, important)
    in
    Selector_tbl.replace t selector cover
end

(* Drop an earlier rule when a later rule with the same canonical selector
   writes every one of its property names at the same or stronger importance.
   The later same-property write shadows the earlier value regardless of
   intervening rules. *)
let drop_shadowed_rules (rules : rule list) : rule list =
  let later_by_selector = Suffix_rule_cover.v () in
  let rules_arr = Array.of_list rules in
  let len = Array.length rules_arr in
  let dropped = Array.make len false in
  let changed = ref false in
  for i = len - 1 downto 0 do
    let rule = rules_arr.(i) in
    dropped.(i) <-
      (rule.declarations = [] && rule.nested = [])
      || rule.declarations <> []
         && List.for_all
              (Suffix_rule_cover.covered later_by_selector
                 rule.Stylesheet_intf.selector)
              rule.declarations;
    if dropped.(i) then changed := true;
    List.iter
      (Suffix_rule_cover.add later_by_selector rule.Stylesheet_intf.selector)
      rule.declarations
  done;
  let rec filter i = function
    | [] -> []
    | rule :: rest ->
        let rest = filter (i + 1) rest in
        if dropped.(i) then rest else rule :: rest
  in
  if !changed then filter 0 rules else rules

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
let single_selector_keys (r : rule) =
  match Selector.as_list r.Stylesheet_intf.selector with
  | Some xs -> List.map canonical_selector_key xs
  | None -> [ canonical_selector_key r.Stylesheet_intf.selector ]

let later_declarations_by_selector_key (indexed : (int * rule) list) =
  let later_by_key :
      (Selector.t list, (int * Declaration.t list) list) Hashtbl.t =
    Hashtbl.create 16
  in
  List.iter
    (fun ((i, r) : int * rule) ->
      List.iter
        (fun key ->
          let prev =
            Hashtbl.find_opt later_by_key key |> Option.value ~default:[]
          in
          Hashtbl.replace later_by_key key
            ((i, r.Stylesheet_intf.declarations) :: prev))
        (single_selector_keys r))
    indexed;
  later_by_key

let declaration_shadowed_by_later ~later_by_key ~rule_index ~keys decl =
  let property_shadowed_for_key key =
    let writes =
      Hashtbl.find_opt later_by_key key |> Option.value ~default:[]
    in
    List.exists
      (fun (j, decls) ->
        j > rule_index
        && List.exists
             (fun ld ->
               same_property decl ld
               && (Declaration.is_important ld
                  || not (Declaration.is_important decl)))
             decls)
      writes
  in
  (not (is_intentionally_duplicated decl))
  && List.for_all property_shadowed_for_key keys

let drop_shadowed_declarations (rules : rule list) : rule list =
  let indexed = List.mapi (fun i r -> (i, r)) rules in
  let later_by_key = later_declarations_by_selector_key indexed in
  let changed = ref false in
  let rules' =
    List.map
      (fun (i, rule) ->
        let keys = single_selector_keys rule in
        let kept =
          list_filter_preserve
            (fun d ->
              not
                (declaration_shadowed_by_later ~later_by_key ~rule_index:i ~keys
                   d))
            rule.declarations
        in
        let rule' = rule_with_declarations rule kept in
        if not (rule' == rule) then changed := true;
        rule')
      indexed
  in
  if !changed then rules' else rules

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
    List.fold_left
      (fun acc r -> acc + Pp.size ~minify:true Stylesheet.pp_rule r)
      0 rules
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
  let arr : rule array = Array.of_list rules in
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
      |> List.exists (fun (idx, (r : rule)) ->
          (idx < start || idx >= start + len)
          && Array.exists
               (fun (m : rule) -> selectors_compete m.selector r.selector)
               members)
    in
    not related_outside
  in
  let rules' =
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
  in
  preserve_list rules rules'

(* Apply [synthesize_nesting_rules] to each maximal run of consecutive [Rule]
   statements. Runs at the statement-list level (not inside [rules_aux]) so that
   rules unwrapped from a baseline [@supports] are contiguous with their
   siblings here: the chain isolation check then sees the duplicate-selector
   competitor and declines to synthesize across the former boundary. *)
let synthesize_nesting_statements (stmts : statement list) : statement list =
  let rec should_try_synthesis count = function
    | [] -> count <= 128
    | Rule r :: _ when r.Stylesheet_intf.nested <> [] -> true
    | Rule _ :: rest when count < 128 -> should_try_synthesis (count + 1) rest
    | Rule _ :: _ -> false
    | _ :: rest -> should_try_synthesis count rest
  in
  if not (should_try_synthesis 0 stmts) then stmts
  else
    let rec span_rules stmt_acc rule_acc = function
      | (Rule r as stmt) :: rest ->
          span_rules (stmt :: stmt_acc) (r :: rule_acc) rest
      | rest -> (List.rev stmt_acc, List.rev rule_acc, rest)
    in
    let rec go acc = function
      | [] -> List.rev acc
      | Rule _ :: _ as l ->
          let stmts, rules, rest = span_rules [] [] l in
          let synthesized_rules = synthesize_nesting_rules rules in
          let synthesized =
            if synthesized_rules == rules then stmts
            else List.map (fun r -> Rule r) synthesized_rules
          in
          go (List.rev_append synthesized acc) rest
      | s :: rest -> go (s :: acc) rest
    in
    preserve_list stmts (go [] stmts)

(* A block holds a conditional named layer when it directly contains a named
   [@layer] block with content. Unwrapping a known-true [@supports] around such
   a block would move that layer's rules - and its place in the layer order (CSS
   Cascade 6.4) - from conditional to unconditional, so the wrapper must stay
   even when the condition is baseline-true. A bare [@layer name;] declaration
   carries no rules, and self-guarding declarations carry no side effect, so
   both unwrap freely. *)
let block_introduces_layer_order stmts =
  List.exists (function Layer (Some _, _ :: _) -> true | _ -> false) stmts

(* Pop the run of [Rule]s most recently pushed onto a reversed accumulator,
   returning them in forward order alongside the remaining accumulator. Used
   when unwrapping a known-true [@supports] exposes its inner rules to rules
   already emitted: those preceding rules must rejoin the merge pass so an
   adjacency the wrapper had hidden can collapse. *)
let pop_trailing_rules acc =
  let rec loop acc rules =
    match acc with
    | (Rule _ as r) :: rest -> loop rest (r :: rules)
    | _ -> (rules, acc)
  in
  loop acc []

let rec collect_rules (stmt_acc : statement list) (rules_acc : rule list) :
    statement list -> statement list * rule list * statement list = function
  | (Rule r as stmt) :: rest ->
      collect_rules (stmt :: stmt_acc) (r :: rules_acc) rest
  | rest -> (List.rev stmt_acc, List.rev rules_acc, rest)

(* When a selector-list rule sits immediately next to a rule whose selector is
   one of its branches, and that branch costs more than the shared block, split
   the branch out next to its neighbour so the same-selector merge folds the
   block in rather than repeating the long selector in the group. Only adjacent
   pairs are touched, so a group with no cheaper neighbour is left intact (a
   blanket decompose instead lets default-value factoring regroup differently
   and land in a worse local optimum). The split is cascade-neutral: the
   branch's declarations move at most across its immediate neighbour, with no
   rule in between. *)
let extract_group_branch_into_adjacent rules =
  let plain (r : Stylesheet.rule) =
    r.nested = [] && r.merge_key = None
    && not (contains_vendor_pseudo_element r.selector)
  in
  let beneficial branch_sel decls =
    let sel_size = Pp.size ~minify:true Selector.pp branch_sel in
    sel_size + 1 > decls_pp_size decls + List.length decls
  in
  (* Split from group [b] the single branch equal to [neighbour_sel]. *)
  let extract neighbour_sel (b : Stylesheet.rule) =
    match selectors_of_rule_selector b.selector with
    | _ :: _ :: _ as branches -> (
        let key = canonical_selector_key in
        let matched, others =
          List.partition (fun s -> key s = key neighbour_sel) branches
        in
        match matched with
        | [ si ] when others <> [] && beneficial si b.declarations ->
            Some
              ( { b with selector = si },
                { b with selector = merge_selector_list others } )
        | _ -> None)
    | _ -> None
  in
  let changed = ref false in
  let rec go acc = function
    | a :: b :: rest when plain a && plain b -> (
        match extract a.Stylesheet_intf.selector b with
        | Some (branch, remainder) ->
            changed := true;
            go (branch :: a :: acc) (remainder :: rest)
        | None -> (
            match extract b.Stylesheet_intf.selector a with
            | Some (branch, remainder) ->
                changed := true;
                go (branch :: remainder :: acc) (b :: rest)
            | None -> go (a :: acc) (b :: rest)))
    | x :: rest -> go (x :: acc) rest
    | [] -> List.rev acc
  in
  let result = go [] rules in
  if !changed then result else rules

let factor_pass_stable_threshold = 2
let factor_min_adaptive_iterations = 2
let factor_stalled_iteration_threshold = 2
let factor_min_marginal_saving = 128
let factor_min_marginal_ratio_ppm = 1_500

let factor_rules_units rules =
  List.fold_left
    (fun acc (rule : Stylesheet.rule) ->
      acc + 8 + (16 * List.length rule.Stylesheet_intf.declarations))
    0 rules

let run_factor_pass quiet any_active_pass_changed active_passes changed_passes
    fuel name f r =
  let q = try Hashtbl.find quiet name with Not_found -> 0 in
  if q >= factor_pass_stable_threshold then r
  else
    let s = pass_stat name in
    let t0 = Unix.gettimeofday () in
    incr active_passes;
    let r' = f r in
    let t1 = Unix.gettimeofday () in
    s.time <- s.time +. (t1 -. t0);
    s.calls <- s.calls + 1;
    s.rules_in <- s.rules_in + List.length r;
    s.rules_out <- s.rules_out + List.length r';
    if r' == r then Hashtbl.replace quiet name (q + 1)
    else begin
      s.changes <- s.changes + 1;
      incr changed_passes;
      Hashtbl.replace quiet name 0;
      any_active_pass_changed := true;
      Log.debug (fun m -> m "factor iter %d: %s changed" fuel name)
    end;
    r'

let factor_fixpoint_passes ~ctx pass rules =
  rules
  (* Gap merging/factoring is pure cascade-safety reasoning (specificity is
     world-independent), so it runs in every scope - neither pass takes a [ctx].
     It is part of the pipeline (not a one-shot before the loop) so a merge it
     can only make after another pass reorders rules is reached within a single
     fixpoint, not on a second [optimize] call. *)
  |> pass "extract_branch" extract_group_branch_into_adjacent
  |> pass "merge_same_selector" merge_same_selector_gaps
  |> pass "combine_identical" combine_identical_rules
  |> pass "extend_identical" (extend_identical_declaration_rules ~ctx)
  |> pass "factor_common" factor_common_declarations
  |> pass "factor_anchor" factor_anchor_gaps
  |> pass "extend_factored" extend_factored_declarations
  |> pass "merge_rules" merge_rules
  |> pass "finalize" (list_map_preserve (finalize_rule_without_nested ~ctx))

let low_marginal_gain before_units bytes_saved =
  bytes_saved < factor_min_marginal_saving
  || bytes_saved * 1_000_000 < before_units * factor_min_marginal_ratio_ppm

let record_factor_iteration ~fixpoint ~local_iteration ~before_rules
    ~before_bytes ~rules' ~after_bytes ~bytes_saved ~active_passes
    ~changed_passes ~elapsed =
  counters.factor_bytes_saved <- counters.factor_bytes_saved + bytes_saved;
  iteration_stats_rev :=
    {
      fixpoint;
      iteration = counters.iterations;
      local_iteration;
      before_rules;
      after_rules = List.length rules';
      before_bytes;
      after_bytes;
      bytes_saved;
      active_passes = !active_passes;
      changed_passes = !changed_passes;
      elapsed;
    }
    :: !iteration_stats_rev

let update_factor_stall ~local_iteration ~stalled ~before_units ~bytes_saved =
  if local_iteration < factor_min_adaptive_iterations then 0
  else if low_marginal_gain before_units bytes_saved then stalled + 1
  else 0

let factor_stalled stalled bytes_saved =
  counters.marginal_stops <- counters.marginal_stops + 1;
  Log.debug (fun m ->
      m
        "factor fixpoint: stopped after %d low-gain iterations (last saved %d \
         bytes)"
        stalled bytes_saved)

type factor_iteration_result = {
  result_rules : Stylesheet.rule list;
  changed : bool;
  before_units : int;
  bytes_saved : int;
}

let run_factor_iteration ~ctx ~quiet ~fixpoint ~local_iteration ~fuel rules =
  counters.iterations <- counters.iterations + 1;
  let any_active_pass_changed = ref false in
  let active_passes = ref 0 in
  let changed_passes = ref 0 in
  let before_rules = List.length rules in
  let before_units = factor_rules_units rules in
  let before_bytes =
    if !collect_profile_stats then rules_pp_size rules else 0
  in
  current_factor_savings := 0;
  let started_at = Unix.gettimeofday () in
  let pass name f r =
    run_factor_pass quiet any_active_pass_changed active_passes changed_passes
      fuel name f r
  in
  let rules' = factor_fixpoint_passes ~ctx pass rules in
  let after_bytes =
    if !collect_profile_stats then rules_pp_size rules' else 0
  in
  let elapsed = Unix.gettimeofday () -. started_at in
  let bytes_saved = !current_factor_savings in
  record_factor_iteration ~fixpoint ~local_iteration ~before_rules ~before_bytes
    ~rules' ~after_bytes ~bytes_saved ~active_passes ~changed_passes ~elapsed;
  {
    result_rules = rules';
    changed = !any_active_pass_changed;
    before_units;
    bytes_saved;
  }

let factor_rules_to_fixpoint ?(adaptive = true) ~ctx fuel rules =
  (* Per-pass quiet-streak counter. A pass that has returned its input unchanged
     [factor_pass_stable_threshold] times in a row gets skipped on subsequent
     iterations. The fixpoint usually needs 8 iterations because
     [extend_identical] keeps changing the list in ways that don't enable new
     factorings for the heavy passes; this counter lets the heavy
     [factor_anchor] / [factor_common] sit out once they've confirmed
     convergence twice running, while the cheap passes keep iterating. *)
  let quiet : (string, int) Hashtbl.t = Hashtbl.create 16 in
  let initial_fuel = fuel in
  counters.factor_fixpoints_run <- counters.factor_fixpoints_run + 1;
  let fixpoint = counters.factor_fixpoints_run in
  let rec go stalled fuel rules =
    if fuel <= 0 then (
      Log.debug (fun m ->
          m "factor fixpoint: fuel exhausted, not yet converged");
      rules)
    else begin
      let local_iteration = initial_fuel - fuel + 1 in
      let result =
        run_factor_iteration ~ctx ~quiet ~fixpoint ~local_iteration ~fuel rules
      in
      if not result.changed then (
        Log.debug (fun m ->
            m "factor fixpoint: converged after %d iterations" (16 - fuel));
        result.result_rules)
      else
        let stalled =
          if adaptive then
            update_factor_stall ~local_iteration ~stalled
              ~before_units:result.before_units ~bytes_saved:result.bytes_saved
          else 0
        in
        if stalled >= factor_stalled_iteration_threshold then begin
          factor_stalled stalled result.bytes_saved;
          result.result_rules
        end
        else go stalled (fuel - 1) result.result_rules
    end
  in
  go 0 fuel rules

module Global_factor_preflight = struct
  let small_declaration_threshold = 4_000
  let useful_gain_units = 2_048
  let useful_gain_ratio_ppm = 140_000

  type t = {
    mutable source_units : int;
    mutable rule_count : int;
    mutable declaration_count : int;
    mutable identical_body_gain : int;
    mutable shared_declaration_gain : int;
  }

  type state = {
    summary : t;
    body_groups : (int list, int) Hashtbl.t;
    declaration_counts : (int, int * int) Hashtbl.t;
  }

  let v () =
    {
      summary =
        {
          source_units = 0;
          rule_count = 0;
          declaration_count = 0;
          identical_body_gain = 0;
          shared_declaration_gain = 0;
        };
      body_groups = Hashtbl.create 256;
      declaration_counts = Hashtbl.create 1024;
    }

  let record_declaration state decl =
    let hash = Declaration.hash decl in
    let count, _size_unit =
      match Hashtbl.find_opt state.declaration_counts hash with
      | Some entry -> entry
      | None -> (0, 1)
    in
    if count > 0 then
      state.summary.shared_declaration_gain <-
        state.summary.shared_declaration_gain + 1;
    Hashtbl.replace state.declaration_counts hash (count + 1, 1)

  let record_identical_body state decls =
    let key = List.map Declaration.hash decls in
    match Hashtbl.find_opt state.body_groups key with
    | Some count ->
        state.summary.identical_body_gain <-
          state.summary.identical_body_gain + max 0 (List.length decls);
        Hashtbl.replace state.body_groups key (count + 1)
    | None -> Hashtbl.add state.body_groups key 1

  let record_rule state (rule : Stylesheet.rule) =
    let decls = rule.Stylesheet_intf.declarations in
    let decl_count = List.length decls in
    state.summary.rule_count <- state.summary.rule_count + 1;
    state.summary.source_units <-
      state.summary.source_units + 8 + (16 * decl_count);
    state.summary.declaration_count <-
      state.summary.declaration_count + decl_count;
    List.iter (record_declaration state) decls;
    if decl_count > 0 then record_identical_body state decls

  let summarize (rules : Stylesheet.rule list) =
    let state = v () in
    List.iter (record_rule state) rules;
    state.summary

  let estimated_gain summary =
    summary.identical_body_gain + (summary.shared_declaration_gain / 32)

  let useful summary =
    if summary.declaration_count <= small_declaration_threshold then true
    else
      let gain = estimated_gain summary in
      counters.factor_preflight_gain <- counters.factor_preflight_gain + gain;
      gain >= useful_gain_units
      && gain * 1_000_000 >= summary.source_units * useful_gain_ratio_ppm
end

let factor_rules_incremental ~ctx (rules : Stylesheet.rule list) =
  let summary = Global_factor_preflight.summarize rules in
  if Global_factor_preflight.useful summary then
    let adaptive =
      summary.declaration_count
      > Global_factor_preflight.small_declaration_threshold
    in
    factor_rules_to_fixpoint ~adaptive ~ctx 16 rules
  else begin
    counters.factor_fixpoints_skipped <- counters.factor_fixpoints_skipped + 1;
    rules
  end

(* [@scope] bounds are parsed selectors; canonicalize them like any other
   selector so a list bound is de-duplicated and ordered consistently. *)
let canonicalize_scope_selector sel = Selector.canonicalize sel

(* Nested rule selectors are implicitly relative to the parent [&], so drop a
   redundant leading [& <combinator>] (CSS Nesting 1 sec. 2). *)
let drop_nesting_prefix (stmt : statement) : statement =
  match stmt with
  | Rule nr ->
      Rule
        {
          nr with
          selector = Selector.drop_redundant_nesting_prefix nr.selector;
        }
  | other -> other

let rec statements ~ctx ~enforce_spec (stmts : statement list) : statement list
    =
  match stmts with
  | [] -> stmts
  | _ ->
      let optimize_merged_block = statements ~ctx ~enforce_spec in
      (* [drop_misplaced_imports] runs first: an [@import] after a style rule is
         invalid and ignored by every browser, so it is a no-op that must not
         act as a cascade boundary. Stripping it up front lets the rules it
         falsely separated merge in this same pass, which keeps [statements]
         idempotent - stripping after the merge would leave two adjacent
         same-selector rules that only a re-run would combine. *)
      let stmts' =
        let stmts =
          drop_misplaced_imports stmts |> merge_named_layers_by_name
        in
        let stmts = process_statements ~ctx ~enforce_spec [] stmts in
        let stmts = synthesize_nesting_statements stmts in
        let stmts =
          stmts
          |> merge_consecutive_media ~optimize_merged_block
          |> merge_consecutive_supports ~optimize_merged_block
          |> merge_consecutive_containers ~optimize_merged_block
        in
        stmts |> merge_layer_declarations |> drop_empty_rules
      in
      preserve_list stmts stmts'

and process_statements ~ctx ~enforce_spec (acc : statement list)
    (remaining : statement list) : statement list =
  match remaining with
  | [] -> List.rev acc
  | (Rule r as stmt) :: rest ->
      process_rule_run ~ctx ~enforce_spec acc stmt r rest
  | (Media (cond, block) as stmt) :: rest ->
      process_media_statement ~ctx ~enforce_spec acc stmt cond block rest
  | (Container (name, cond, block) as stmt) :: rest ->
      process_container_statement ~ctx ~enforce_spec acc stmt name cond block
        rest
  | Supports (cond, block) :: rest ->
      process_supports_statement ~ctx ~enforce_spec acc cond block rest
  | (Scope (start, end_, block) as stmt) :: rest ->
      let start' = option_map_preserve canonicalize_scope_selector start in
      let end_' = option_map_preserve canonicalize_scope_selector end_ in
      let optimized_block = statements ~ctx ~enforce_spec block in
      let optimized =
        if start' == start && end_' == end_ && optimized_block == block then
          stmt
        else Scope (start', end_', optimized_block)
      in
      process_statements ~ctx ~enforce_spec (optimized :: acc) rest
  | (Origin (origin, block) as stmt) :: rest ->
      let optimized_block = statements ~ctx ~enforce_spec block in
      let optimized =
        if optimized_block == block then stmt
        else Origin (origin, optimized_block)
      in
      process_statements ~ctx ~enforce_spec (optimized :: acc) rest
  | (Layer (name, block) as stmt) :: rest ->
      process_layer_statement ~ctx ~enforce_spec acc stmt name block rest
  | (Import import as stmt) :: rest ->
      process_import_statement ~ctx ~enforce_spec acc stmt import rest
  | hd :: rest ->
      (* Other statement types - keep as-is *)
      process_statements ~ctx ~enforce_spec (hd :: acc) rest

and process_rule_run ~ctx ~enforce_spec acc stmt r rest =
  let plain_stmts, plain_rules, rest = collect_rules [ stmt ] [ r ] rest in
  let optimized = rules_aux ~ctx ~enforce_spec plain_rules in
  let as_statements =
    if optimized == plain_rules then plain_stmts
    else List.map (fun r -> Rule r) optimized
  in
  process_statements ~ctx ~enforce_spec (List.rev_append as_statements acc) rest

and process_media_statement ~ctx ~enforce_spec acc stmt cond block rest =
  let cond = if enforce_spec then cond else Media.lower_for_minify cond in
  let optimized_block = statements ~ctx ~enforce_spec block in
  let optimized =
    if
      (cond == match stmt with Media (c, _) -> c | _ -> assert false)
      && optimized_block == block
    then stmt
    else Media (cond, optimized_block)
  in
  process_statements ~ctx ~enforce_spec (optimized :: acc) rest

and process_container_statement ~ctx ~enforce_spec acc stmt name cond block rest
    =
  let cond =
    if enforce_spec then cond
    else option_map_preserve Container.lower_for_minify cond
  in
  let optimized_block = statements ~ctx ~enforce_spec block in
  let optimized =
    if
      (cond == match stmt with Container (_, c, _) -> c | _ -> assert false)
      && optimized_block == block
    then stmt
    else Container (name, cond, optimized_block)
  in
  process_statements ~ctx ~enforce_spec (optimized :: acc) rest

and process_supports_statement ~ctx ~enforce_spec acc cond block rest =
  let optimized_block = statements ~ctx ~enforce_spec block in
  let baseline =
    if enforce_spec then `Cond cond else Supports.simplify_baseline cond
  in
  match baseline with
  | `True when block_introduces_layer_order optimized_block ->
      process_statements ~ctx ~enforce_spec
        (Supports (cond, optimized_block) :: acc)
        rest
  | `True ->
      let trailing, acc = pop_trailing_rules acc in
      process_statements ~ctx ~enforce_spec acc
        (trailing @ optimized_block @ rest)
  | `False -> process_statements ~ctx ~enforce_spec acc rest
  | `Cond cond' ->
      process_statements ~ctx ~enforce_spec
        (Supports (cond', optimized_block) :: acc)
        rest

and process_layer_statement ~ctx ~enforce_spec acc stmt name block rest =
  let optimized_block = statements ~ctx ~enforce_spec block in
  if is_layer_empty optimized_block then
    match name with
    | Some layer_name ->
        let all_names, remaining =
          collect_empty_layer_names [ layer_name ] rest
        in
        process_statements ~ctx ~enforce_spec
          (Layer_decl all_names :: acc)
          remaining
    | None -> process_statements ~ctx ~enforce_spec acc rest
  else
    let optimized =
      if optimized_block == block then stmt else Layer (name, optimized_block)
    in
    process_statements ~ctx ~enforce_spec (optimized :: acc) rest

and process_import_statement ~ctx ~enforce_spec acc stmt import rest =
  let stmt =
    match import.supports with
    | Some cond
      when (not enforce_spec) && Supports.simplify_baseline cond = `True ->
        Import { import with supports = None }
    | _ -> stmt
  in
  process_statements ~ctx ~enforce_spec (stmt :: acc) rest

and rules_aux ~ctx ~enforce_spec (rules : rule list) : rule list =
  (* First optimize each rule's nested statements recursively, then drop the
     redundant nesting prefix (see [drop_nesting_prefix]). *)
  let with_optimized_nested =
    list_map_preserve
      (fun rule ->
        let nested =
          match rule.nested with
          | [] -> []
          | nested ->
              let nested = statements ~ctx ~enforce_spec nested in
              if enforce_spec then nested
              else list_map_preserve drop_nesting_prefix nested
        in
        let rule = rule_with_nested rule nested in
        let rule =
          let selector = Selector.canonicalize rule.selector in
          if selector == rule.selector then rule else { rule with selector }
        in
        merge_lone_nested_rule rule)
      rules
  in
  (* Apply standard rule optimizations. Adjacent same-selector rules merge:
     [.x{a}] [.x{b}] -> [.x{a;b}], which is safe because cascade order within
     the merged block matches the source order of the originals.
     [combine_identical_rules] then groups same-declaration rules under a
     selector list ([.a, .b, .c{...}]). *)
  let prepared =
    list_map_preserve (single_rule_without_nested ~ctx) with_optimized_nested
  in
  let prepared =
    prepared |> drop_shadowed_declarations |> drop_shadowed_rules |> merge_rules
  in
  let prepared =
    list_map_preserve
      (finalize_rule_without_nested ~canonicalize_selector:false ~ctx)
      prepared
  in
  (* Factoring is greedy and global: extracting one shared declaration subset
     can leave behind leftovers that are themselves factorable. The local linear
     optimizations above always run; this incremental gate only decides whether
     the expensive global factoring fixpoint is likely to buy enough bytes to
     justify the full indexed scheduler walk. *)
  factor_rules_incremental ~ctx prepared

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
let statements_top_level ~ctx ~enforce_spec (stmts : statement list) :
    statement list =
  let optimize_merged_block = statements ~ctx ~enforce_spec in
  let stmts' =
    statements ~ctx ~enforce_spec stmts
    |> merge_consecutive_layers ~optimize_merged_block
    |> drop_redundant_layer_decls |> drop_shadowed_keyframes
  in
  preserve_list stmts stmts'

let single_rule ?scope (rule : rule) : rule =
  let ctx = ctx_of_scope scope in
  {
    rule with
    declarations = deduplicate_declarations_with ~ctx rule.declarations;
    nested = statements ~ctx ~enforce_spec:false rule.nested;
  }

let rules ?scope (rules : rule list) : rule list =
  rules_aux ~ctx:(ctx_of_scope scope) ~enforce_spec:false rules

(** {1 Nesting Flattening} *)

let scope_selector_in_context (parent : Selector.t) selector =
  if contains_nesting selector then substitute_nesting ~parent selector
  else selector

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

let rec flatten_top_statement (stmt : statement) : statement list =
  (* A flat rule with declarations is already in final form; a wrapper whose
     block does not change keeps its node. Both let an already-flat sheet stay
     physically shared. *)
  let wrap block rebuild =
    let block' = flatten_block block in
    if block' == block then [ stmt ] else [ rebuild block' ]
  in
  match stmt with
  | Rule { nested = []; declarations = _ :: _; _ } -> [ stmt ]
  | Rule rule -> flatten_rule rule
  | Media (cond, block) -> wrap block (fun b -> Media (cond, b))
  | Container (name, cond, block) ->
      wrap block (fun b -> Container (name, cond, b))
  | Supports (cond, block) -> wrap block (fun b -> Supports (cond, b))
  | Layer (name, block) -> wrap block (fun b -> Layer (name, b))
  | Origin (origin, block) -> wrap block (fun b -> Origin (origin, b))
  | Starting_style block -> wrap block (fun b -> Starting_style b)
  | When (cond, block) -> wrap block (fun b -> When (cond, b))
  | Else (cond, block) -> wrap block (fun b -> Else (cond, b))
  | Scope (s, e, block) -> wrap block (fun b -> Scope (s, e, b))
  | other -> [ other ]

and flatten_block (block : statement list) : statement list =
  concat_map_preserve flatten_top_statement block

let flatten_nesting (stylesheet : t) : t = flatten_block stylesheet

(** {1 Stylesheet Optimization} *)

let apply_property_duplication (stylesheet : t) : t =
  (* Apply only property duplication without other optimizations. Each level
     keeps its node when nothing below changed, so an untouched subtree stays
     physically shared (no whole-tree rebuild on a no-op). *)
  let rec apply_to_statements stmts =
    list_map_preserve
      (fun stmt ->
        match stmt with
        | Rule rule ->
            let declarations = duplicate_buggy_properties rule.declarations in
            if declarations == rule.declarations then stmt
            else Rule { rule with declarations }
        | Media (cond, inner) ->
            let inner' = apply_to_statements inner in
            if inner' == inner then stmt else Media (cond, inner')
        | Layer (name, inner) ->
            let inner' = apply_to_statements inner in
            if inner' == inner then stmt else Layer (name, inner')
        | Container (name, cond, inner) ->
            let inner' = apply_to_statements inner in
            if inner' == inner then stmt else Container (name, cond, inner')
        | Supports (cond, inner) ->
            let inner' = apply_to_statements inner in
            if inner' == inner then stmt else Supports (cond, inner')
        | Origin (origin, inner) ->
            let inner' = apply_to_statements inner in
            if inner' == inner then stmt else Origin (origin, inner')
        | other -> other)
      stmts
  in
  apply_to_statements stylesheet

let map_statement_block_preserve f stmt =
  let map block = list_map_preserve f block in
  match stmt with
  | Layer (name, block) ->
      let block' = map block in
      if block' == block then stmt else Layer (name, block')
  | Media (m, block) ->
      let block' = map block in
      if block' == block then stmt else Media (m, block')
  | Container (n, c, block) ->
      let block' = map block in
      if block' == block then stmt else Container (n, c, block')
  | Supports (s, block) ->
      let block' = map block in
      if block' == block then stmt else Supports (s, block')
  | Moz_document (c, block) ->
      let block' = map block in
      if block' == block then stmt else Moz_document (c, block')
  | When (c, block) ->
      let block' = map block in
      if block' == block then stmt else When (c, block')
  | Else (c, block) ->
      let block' = map block in
      if block' == block then stmt else Else (c, block')
  | Starting_style block ->
      let block' = map block in
      if block' == block then stmt else Starting_style block'
  | Origin (o, block) ->
      let block' = map block in
      if block' == block then stmt else Origin (o, block')
  | Scope (a, b, block) ->
      let block' = map block in
      if block' == block then stmt else Scope (a, b, block')
  | _ -> stmt

(** [drop_invalid] walks every declaration list in the stylesheet (rules, bare
    nesting blocks, [@page] / [@font-palette-values] / [@view-transition] /
    [@position-try]) and removes declarations whose typed value contains an
    [Invalid] arm. *)
let drop_invalid (stylesheet : t) : t =
  let filter_decls =
    list_filter_preserve (fun d -> not (Declaration.is_invalid d))
  in
  let rec statement stmt =
    match stmt with
    | Rule rule ->
        let declarations = filter_decls rule.declarations in
        let nested = list_map_preserve statement rule.nested in
        let rule' =
          rule_with_declarations_and_nested rule declarations nested
        in
        if rule' == rule then stmt else Rule rule'
    | Declarations decls ->
        let decls' = filter_decls decls in
        if decls' == decls then stmt else Declarations decls'
    | Layer _ | Media _ | Container _ | Supports _ | Moz_document _ | When _
    | Else _ | Starting_style _ | Origin _ | Scope _ ->
        map_statement_block_preserve statement stmt
    | Page (sel, decls) ->
        let decls' = filter_decls decls in
        if decls' == decls then stmt else Page (sel, decls')
    | Page_with_margins (sel, descs, margins) ->
        let descs' = filter_decls descs in
        let margins' =
          list_map_preserve
            (fun (m : page_margin_rule) ->
              let descriptors = filter_decls m.descriptors in
              if descriptors == m.descriptors then m else { m with descriptors })
            margins
        in
        if descs' == descs && margins' == margins then stmt
        else Page_with_margins (sel, descs', margins')
    | Position_try (name, decls) ->
        let decls' = filter_decls decls in
        if decls' == decls then stmt else Position_try (name, decls')
    | Supports_condition (name, decls) ->
        let decls' = filter_decls decls in
        if decls' == decls then stmt else Supports_condition (name, decls')
    | other -> other
  in
  list_map_preserve statement stylesheet

(** [drop_unknown_at_rules] removes [Unknown_at_rule] statements at every block
    depth. Used in [--minify] alongside [drop_invalid] so the typed warnings
    emitted at parse time materialise as a dropped rule, matching CSS Syntax 3
    §5.4.1 (unknown at-rules are discarded). *)
let drop_unknown_at_rules (stylesheet : t) : t =
  let rec statement stmt =
    match stmt with
    | Rule rule ->
        let nested = list_filter_map_preserve statement rule.nested in
        let rule' = rule_with_nested rule nested in
        Some (if rule' == rule then stmt else Rule rule')
    | Layer (name, block) ->
        let block' = list_filter_map_preserve statement block in
        Some (if block' == block then stmt else Layer (name, block'))
    | Media (m, block) ->
        let block' = list_filter_map_preserve statement block in
        Some (if block' == block then stmt else Media (m, block'))
    | Container (n, c, block) ->
        let block' = list_filter_map_preserve statement block in
        Some (if block' == block then stmt else Container (n, c, block'))
    | Supports (s, block) ->
        let block' = list_filter_map_preserve statement block in
        Some (if block' == block then stmt else Supports (s, block'))
    | Moz_document (c, block) ->
        let block' = list_filter_map_preserve statement block in
        Some (if block' == block then stmt else Moz_document (c, block'))
    | When (c, block) ->
        let block' = list_filter_map_preserve statement block in
        Some (if block' == block then stmt else When (c, block'))
    | Else (c, block) ->
        let block' = list_filter_map_preserve statement block in
        Some (if block' == block then stmt else Else (c, block'))
    | Starting_style block ->
        let block' = list_filter_map_preserve statement block in
        Some (if block' == block then stmt else Starting_style block')
    | Origin (o, block) ->
        let block' = list_filter_map_preserve statement block in
        Some (if block' == block then stmt else Origin (o, block'))
    | Scope (a, b, block) ->
        let block' = list_filter_map_preserve statement block in
        Some (if block' == block then stmt else Scope (a, b, block'))
    | Unknown_at_rule _ -> None
    | other -> Some other
  in
  list_filter_map_preserve statement stylesheet

(* CSS Properties and Values API 1 sec. 2: an [@property --name { syntax: ... }]
   declaration registers [name] with a typed CSS syntax, lifting later [--name:
   ...] uses out of the unregistered opaque-token-stream rule. Apply
   registrations in source order so a [@property] only affects uses that follow
   it; later registrations of the same name overwrite, matching how the browser
   registry resolves duplicate declarations. *)
let try_promote_custom_with (type a) (syntax : a Variables.syntax) components =
  match syntax with
  | Variables.Color -> Properties.try_read_custom_color components
  | Variables.Length -> Properties.try_read_custom_length components
  | Variables.Length_percentage ->
      Properties.try_read_custom_length_percentage components
  | Variables.Number -> Properties.try_read_custom_number components
  | Variables.Percentage -> Properties.try_read_custom_percentage components
  | _ -> None

(* Does the registered syntax somewhere accept a [<custom-ident>] (possibly
   under [+] / [#] / [|] modifiers)? When yes, a [<string>] whose content is a
   multi-word identifier sequence is spec-equivalent (CSS Fonts 4 sec. 15.3 for
   font-family-shaped registrations), so the promotion pass rewrites it to the
   equivalent ident sequence before parsing. *)
let rec syntax_accepts_ident_sequence : type a. a Variables.syntax -> bool =
  function
  | Variables.Custom_ident -> true
  | Variables.Plus s -> syntax_accepts_ident_sequence s
  | Variables.Hash s -> syntax_accepts_ident_sequence s
  | Variables.Or (s1, s2) ->
      syntax_accepts_ident_sequence s1 || syntax_accepts_ident_sequence s2
  | _ -> false

let promote_registered_custom_decl ~lossless registry decl =
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
          let components' =
            if syntax_accepts_ident_sequence syntax then
              Properties.unquote_font_family_strings components
            else components
          in
          match try_promote_custom_with syntax components' with
          | Some typed ->
              Declaration.v ~important (Custom_property name)
                (Custom_value
                   {
                     value =
                       Properties.normalize_custom_property_value ~lossless
                         typed;
                     layer;
                     meta;
                   })
          | None when components' == components -> decl
          | None ->
              (* Promotion failed (e.g. [<custom-ident>+] has no typed promotion
                 path yet) but the string-to-ident rewrite still produces the
                 canonical opaque AST. *)
              Declaration.v ~important (Custom_property name)
                (Custom_value { value = Tokens components'; layer; meta })))
  | _ -> decl

let promote_registered_custom_properties ~lossless (stmts : statement list) =
  let registry : (string, Variables.any_syntax) Hashtbl.t = Hashtbl.create 8 in
  let promote_decl = promote_registered_custom_decl ~lossless registry in
  let rec walk_stmt (stmt : statement) : statement =
    match stmt with
    | Property pr ->
        Hashtbl.replace registry pr.name (Variables.Syntax pr.syntax);
        stmt
    | Rule r ->
        let declarations = list_map_preserve promote_decl r.declarations in
        let nested = list_map_preserve walk_stmt r.nested in
        let r' = rule_with_declarations_and_nested r declarations nested in
        if r' == r then stmt else Rule r'
    | Declarations decls ->
        let decls' = list_map_preserve promote_decl decls in
        if decls' == decls then stmt else Declarations decls'
    | Media _ | Container _ | Supports _ | Layer _ | Origin _ | Scope _
    | Starting_style _ | Moz_document _ | When _ | Else _ ->
        map_statement_block_preserve walk_stmt stmt
    | _ -> stmt
  in
  list_map_preserve walk_stmt stmts

(* Under closed-stylesheet scope the optimiser knows every [@position-try
   --name] rule defined in the sheet. A [position-try-fallbacks: --x, --y] entry
   whose name has no matching [@position-try] rule cannot match at runtime, so
   prune unknown [Name] arms. Keep the [Flip_*] tactics and any [Var]
   indirection untouched. When every arm gets pruned the whole declaration drops
   (the property becomes equivalent to its initial). *)
let collect_position_try_names stylesheet =
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
  known

let rec prune_position_try_decl known (decl : Declaration.declaration) :
    Declaration.declaration option =
  match decl with
  | Declaration
      {
        property = Position_try_fallbacks;
        value = (Fallbacks items : Properties.position_try_fallbacks);
        important;
      } -> (
      let keep = function
        | (Properties.Name s : Properties.position_try_fallback) ->
            Hashtbl.mem known s
        | _ -> true
      in
      match List.filter keep items with
      | [] -> None
      | kept ->
          Some
            (Declaration.v ~important Position_try_fallbacks (Fallbacks kept)))
  | Theme_guarded { var_name; decl; _ } -> (
      match prune_position_try_decl known decl with
      | None -> None
      | Some decl -> Some (Declaration.theme_guarded ~var_name decl))
  | other -> Some other

let prune_position_try_fallbacks ~scope (stylesheet : t) : t =
  match scope with
  | `Fragment -> stylesheet
  | `Stylesheet ->
      let known = collect_position_try_names stylesheet in
      let prune_decls = List.filter_map (prune_position_try_decl known) in
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

(* Collect the custom properties registered with an [@property] initial-value.
   Such a property is never invalid at computed-value time, so folding its
   [var()] into a shorthand cannot widen a failure across the other
   longhands. *)
let registered_foldable (stylesheet : t) : string -> bool =
  let tbl : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  let rec collect (stmt : statement) =
    match stmt with
    | Property pr -> (
        match pr.initial_value with
        | Some _ ->
            (* [@property] names carry the [--] prefix; [var()] references store
               the bare name, so normalise to the bare form for lookup. *)
            let key =
              if String.length pr.name >= 2 && String.sub pr.name 0 2 = "--"
              then String.sub pr.name 2 (String.length pr.name - 2)
              else pr.name
            in
            Hashtbl.replace tbl key ()
        | None -> ())
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
  fun name -> Hashtbl.mem tbl name

(* Canonicalise every declaration's value so the whole AST is canonical before
   structural optimization. Cover all declaration-bearing contexts - style rules
   and their nesting, [@keyframes] frames, [@page] and its margin rules,
   [@position-try], [@supports-condition], bare nesting declarations - and
   recurse through grouping blocks, so canonicalisation is uniform wherever a
   declaration sits. *)
let normalize_keyframe ~lossless (k : keyframe) : keyframe =
  let declarations =
    list_map_preserve (Declaration.normalize ~lossless) k.declarations
  in
  if declarations == k.declarations then k else { k with declarations }

let rec normalize_block ~lossless (b : statement list) : statement list =
  list_map_preserve (normalize_statement ~lossless) b

and normalize_statement ~lossless (s : statement) : statement =
  let nd = list_map_preserve (Declaration.normalize ~lossless) in
  match s with
  | Rule r ->
      let declarations = nd r.declarations in
      let nested = normalize_block ~lossless r.nested in
      let r' = rule_with_declarations_and_nested r declarations nested in
      if r' == r then s else Rule r'
  | Declarations d ->
      let d' = nd d in
      if d' == d then s else Declarations d'
  | Page (n, d) ->
      let d' = nd d in
      if d' == d then s else Page (n, d')
  | Page_with_margins (n, descs, margins) ->
      let descs' = nd descs in
      let margins' =
        list_map_preserve
          (fun m ->
            let descriptors = nd m.descriptors in
            if descriptors == m.descriptors then m else { m with descriptors })
          margins
      in
      if descs' == descs && margins' == margins then s
      else Page_with_margins (n, descs', margins')
  | Position_try (n, d) ->
      let d' = nd d in
      if d' == d then s else Position_try (n, d')
  | Supports_condition (n, d) ->
      let d' = nd d in
      if d' == d then s else Supports_condition (n, d')
  | Keyframes (n, ks) ->
      let ks' = list_map_preserve (normalize_keyframe ~lossless) ks in
      if ks' == ks then s else Keyframes (n, ks')
  | Webkit_keyframes (n, ks) ->
      let ks' = list_map_preserve (normalize_keyframe ~lossless) ks in
      if ks' == ks then s else Webkit_keyframes (n, ks')
  | Moz_keyframes (n, ks) ->
      let ks' = list_map_preserve (normalize_keyframe ~lossless) ks in
      if ks' == ks then s else Moz_keyframes (n, ks')
  | Layer (n, b) ->
      let b' = normalize_block ~lossless b in
      if b' == b then s else Layer (n, b')
  | Media (c, b) ->
      let b' = normalize_block ~lossless b in
      if b' == b then s else Media (c, b')
  | Container (n, c, b) ->
      let b' = normalize_block ~lossless b in
      if b' == b then s else Container (n, c, b')
  | Supports (c, b) ->
      let b' = normalize_block ~lossless b in
      if b' == b then s else Supports (c, b')
  | Moz_document (c, b) ->
      let b' = normalize_block ~lossless b in
      if b' == b then s else Moz_document (c, b')
  | Starting_style b ->
      let b' = normalize_block ~lossless b in
      if b' == b then s else Starting_style b'
  | When (c, b) ->
      let b' = normalize_block ~lossless b in
      if b' == b then s else When (c, b')
  | Else (c, b) ->
      let b' = normalize_block ~lossless b in
      if b' == b then s else Else (c, b')
  | Origin (o, b) ->
      let b' = normalize_block ~lossless b in
      if b' == b then s else Origin (o, b')
  | Scope (s1, s2, b) ->
      let b' = normalize_block ~lossless b in
      if b' == b then s else Scope (s1, s2, b')
  | Property r ->
      let initial_value =
        match r.initial_value with
        | None -> r.initial_value
        | Some value ->
            let value' = Variables.normalize_value ~lossless r.syntax value in
            if value' == value then r.initial_value else Some value'
      in
      if initial_value == r.initial_value then s
      else Property { r with initial_value }
  | other -> other

let stylesheet ?scope ?(flatten_nesting = false) ?(lossless = false)
    ?(enforce_spec = false) (stylesheet : t) : t =
  clear_summary_memo ();
  Selector_summary.clear_memo ();
  reset_counters ();
  let scope = Option.value scope ~default:`Fragment in
  let stylesheet = normalize_block ~lossless stylesheet in
  let stylesheet =
    if flatten_nesting then flatten_block stylesheet else stylesheet
  in
  let stylesheet = promote_registered_custom_properties ~lossless stylesheet in
  let ctx = { scope; registered = registered_foldable stylesheet; lossless } in
  (* [drop_invalid] and [drop_unknown_at_rules] run before the main optimisation
     passes so the empty rules they leave behind get picked up by
     [drop_empty_rules]. *)
  stylesheet
  |> prune_position_try_fallbacks ~scope
  |> drop_invalid |> drop_unknown_at_rules
  |> statements_top_level ~ctx ~enforce_spec
