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

(* Typed shorthand -> longhand coverage relation. Each arm is a reachable
   [(shorthand, longhand)] pair, transitive cases included ([border] covers
   [border-top-width] directly and via [border-width] / [border-top]). Unlisted
   properties self-cover by identity, never reset others; custom and unknown
   ones go through [declaration_covers]. *)
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
  | Flex, Flex_grow -> true
  | Flex, Flex_shrink -> true
  | Flex, Flex_basis -> true
  | Flex_flow, Flex_direction -> true
  | Flex_flow, Flex_wrap -> true
  | Transition, Transition_property -> true
  | Transition, Transition_duration -> true
  | Transition, Transition_timing_function -> true
  | Transition, Transition_delay -> true
  | Transition, Transition_behavior -> true
  (* CSS Animations 2 sec. 4.11 and 4.12: [animation] resets every longhand
     [animation_keys] names, [animation-timeline] included.
     [animation-composition] and the [animation-range-*] longhands are not part
     of it and are absent here for that reason. *)
  | Animation, Animation_name -> true
  | Animation, Animation_duration -> true
  | Animation, Animation_timing_function -> true
  | Animation, Animation_delay -> true
  | Animation, Animation_iteration_count -> true
  | Animation, Animation_direction -> true
  | Animation, Animation_fill_mode -> true
  | Animation, Animation_play_state -> true
  | Animation, Animation_timeline -> true
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
  (* CSS Backgrounds 3 sec. 3.4: [border] resets every per-side longhand and the
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
  (* CSS Logical 1 sec. 4.5.4: [border-block] / [border-inline] reset their two
     flow-relative middle-tier longhands. *)
  | Border_block, Border_block_start -> true
  | Border_block, Border_block_end -> true
  | Border_inline, Border_inline_start -> true
  | Border_inline, Border_inline_end -> true
  (* CSS Masking 1 sec. 7.9: [mask] resets every mask layer longhand and
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
  (* Motion Path 1 sec. 2.6: [offset] resets the five motion path longhands. *)
  | Offset, Offset_position -> true
  | Offset, Offset_path -> true
  | Offset, Offset_distance -> true
  | Offset, Offset_rotate -> true
  | Offset, Offset_anchor -> true
  | _ -> false

(* CSS Fragmentation 3 sec. 3.4 aliases [page-break-before/after/inside] to
   [break-before/after/inside] through a value mapping ([always] maps to [page],
   every other value to itself), so a pair is one property writing one slot:
   either spelling shadows the other whatever value it carries. Symmetric,
   unlike the shorthand table above, since the alias has exactly one
   longhand. *)
let aliases_page_break : type a b.
    a Properties.property -> b Properties.property -> bool =
 fun p q ->
  match (p, q) with
  | Page_break_before, Break_before -> true
  | Break_before, Page_break_before -> true
  | Page_break_after, Break_after -> true
  | Break_after, Page_break_after -> true
  | Page_break_inside, Break_inside -> true
  | Break_inside, Page_break_inside -> true
  | _ -> false

(* CSS Cascade 5 sec. 3.2: [all] resets every property except [direction],
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

(* CSS Cascade 5 sec. 3.2: [direction] and [unicode-bidi] keep their relative
   position after an [all] declaration; partition uses this to anchor them. *)
let is_all_preserved_reorder : type a. a Properties.property -> bool = function
  | Direction -> true
  | Unicode_bidi -> true
  | _ -> false

let all_preserved_reorder_declaration decl =
  match unwrap_theme_guard decl with
  | Declaration { property; _ } -> is_all_preserved_reorder property
  | _ -> false

(* Coverage relation between two declarations. Custom properties cover
   themselves by exact name and have no typed shorthand coverage; they are
   exempt from the [all] reset. Two opaque unknown-property values do not cover
   one another: either value may belong to a browser grammar the other does not,
   so both are compatibility fallbacks. Structurally identical streams are the
   one case where either declaration can safely cover the other. *)
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
  | ( Declaration { property = Unknown_property a; value = av; _ },
      Declaration { property = Unknown_property b; value = bv; _ } ) ->
      String.equal a b && Stdlib.compare av bv = 0
  | Declaration { property = Unknown_property _; _ }, _ -> false
  | _, Declaration { property = Unknown_property _; _ } -> false
  | ( Declaration { property = covering_p; _ },
      Declaration { property = covered_p; _ } ) ->
      Declaration.same_property covering covered
      || covers_longhand covering_p covered_p
      || aliases_page_break covering_p covered_p
  | _ -> false

type overlap_key = int

let overlap_key_equal = Int.equal
let overlap_key_compare = Int.compare
let overlap_key_hash key = key land max_int

let overlap_key_of_name name =
  (* Hash collisions only add conservative ordering edges. They cannot make two
     overlapping declarations look disjoint. *)
  (hash_string name lsl 1) lor 1

let key = overlap_key_of_name
let broad_overlap_key = key "*"
let typed_key hash = (hash land (max_int lsr 1)) lsl 1

let property_key : type a. a Properties.property -> overlap_key =
 fun property ->
  match property with
  | Custom_property name -> key ("--" ^ name)
  | Unknown_property name -> key name
  | property -> typed_key (Hashtbl.seeded_hash 0 property)

let border_block_start_keys =
  [
    key "border-block-start-width";
    key "border-block-start-style";
    key "border-block-start-color";
  ]

let border_block_end_keys =
  [
    key "border-block-end-width";
    key "border-block-end-style";
    key "border-block-end-color";
  ]

let border_inline_start_keys =
  [
    key "border-inline-start-width";
    key "border-inline-start-style";
    key "border-inline-start-color";
  ]

let border_inline_end_keys =
  [
    key "border-inline-end-width";
    key "border-inline-end-style";
    key "border-inline-end-color";
  ]

(* CSS Backgrounds 3 sec. 5.7: [border-image] resets the five image longhands,
   and sec. 3.4 makes [border] reset [border-image] in turn. *)
let border_image_keys =
  [
    key "border-image-source";
    key "border-image-slice";
    key "border-image-width";
    key "border-image-outset";
    key "border-image-repeat";
  ]

let border_radius_keys =
  [
    key "border-top-left-radius";
    key "border-top-right-radius";
    key "border-bottom-right-radius";
    key "border-bottom-left-radius";
  ]

(* CSS Gaps 1 sec. 2.4. The legacy [grid-row-gap] / [grid-column-gap] spellings
   name the same cascade slots as the modern ones, so each axis carries both
   names. [grid-gap] itself is deliberately absent: it has no typed spelling,
   and leaving its name out of every footprint keeps it in the conservative
   [Unknown_property] path. *)
let row_gap_keys = [ key "row-gap"; key "grid-row-gap" ]
let column_gap_keys = [ key "column-gap"; key "grid-column-gap" ]

(* CSS Animations 2 sec. 4.11 and 4.12, which make [animation] reset
   [animation-timeline]. [animation-composition] and the [animation-range-*]
   longhands are not part of the shorthand. *)
let animation_keys =
  [
    key "animation-name";
    key "animation-duration";
    key "animation-timing-function";
    key "animation-delay";
    key "animation-iteration-count";
    key "animation-direction";
    key "animation-fill-mode";
    key "animation-play-state";
    key "animation-timeline";
  ]

let transition_keys =
  [
    key "transition-property";
    key "transition-duration";
    key "transition-timing-function";
    key "transition-delay";
    key "transition-behavior";
  ]

(* CSS Grid 1 sec. 7.4 and 7.8: [grid-template] resets the three template
   longhands, and [grid] resets those plus the three [grid-auto-*] ones. *)
let grid_template_keys =
  [
    key "grid-template-rows";
    key "grid-template-columns";
    key "grid-template-areas";
  ]

let grid_auto_keys =
  [ key "grid-auto-rows"; key "grid-auto-columns"; key "grid-auto-flow" ]

let grid_row_keys = [ key "grid-row-start"; key "grid-row-end" ]
let grid_column_keys = [ key "grid-column-start"; key "grid-column-end" ]

(* CSS Text Decoration 4 sec. 2.6. [text-underline-offset] and the
   [text-decoration-skip-*] longhands are outside the shorthand. *)
let text_decoration_keys =
  [
    key "text-decoration-line";
    key "text-decoration-style";
    key "text-decoration-color";
    key "text-decoration-thickness";
  ]

let scroll_margin_keys =
  [
    key "scroll-margin-top";
    key "scroll-margin-right";
    key "scroll-margin-bottom";
    key "scroll-margin-left";
  ]

let scroll_padding_keys =
  [
    key "scroll-padding-top";
    key "scroll-padding-right";
    key "scroll-padding-bottom";
    key "scroll-padding-left";
  ]

(* The slots a property names, before flow-relative aliasing. *)
let property_slots : type a. a Properties.property -> overlap_key list =
  function
  | All -> [ key "*" ]
  | Margin ->
      [
        key "margin-top";
        key "margin-right";
        key "margin-bottom";
        key "margin-left";
      ]
  | Margin_top -> [ key "margin-top" ]
  | Margin_right -> [ key "margin-right" ]
  | Margin_bottom -> [ key "margin-bottom" ]
  | Margin_left -> [ key "margin-left" ]
  | Margin_inline -> [ key "margin-inline-start"; key "margin-inline-end" ]
  | Margin_inline_start -> [ key "margin-inline-start" ]
  | Margin_inline_end -> [ key "margin-inline-end" ]
  | Margin_block -> [ key "margin-block-start"; key "margin-block-end" ]
  | Margin_block_start -> [ key "margin-block-start" ]
  | Margin_block_end -> [ key "margin-block-end" ]
  | Padding ->
      [
        key "padding-top";
        key "padding-right";
        key "padding-bottom";
        key "padding-left";
      ]
  | Padding_top -> [ key "padding-top" ]
  | Padding_right -> [ key "padding-right" ]
  | Padding_bottom -> [ key "padding-bottom" ]
  | Padding_left -> [ key "padding-left" ]
  | Padding_inline -> [ key "padding-inline-start"; key "padding-inline-end" ]
  | Padding_inline_start -> [ key "padding-inline-start" ]
  | Padding_inline_end -> [ key "padding-inline-end" ]
  | Padding_block -> [ key "padding-block-start"; key "padding-block-end" ]
  | Padding_block_start -> [ key "padding-block-start" ]
  | Padding_block_end -> [ key "padding-block-end" ]
  | Inset -> [ key "top"; key "right"; key "bottom"; key "left" ]
  | Top -> [ key "top" ]
  | Right -> [ key "right" ]
  | Bottom -> [ key "bottom" ]
  | Left -> [ key "left" ]
  | Inset_inline -> [ key "inset-inline-start"; key "inset-inline-end" ]
  | Inset_inline_start -> [ key "inset-inline-start" ]
  | Inset_inline_end -> [ key "inset-inline-end" ]
  | Inset_block -> [ key "inset-block-start"; key "inset-block-end" ]
  | Inset_block_start -> [ key "inset-block-start" ]
  | Inset_block_end -> [ key "inset-block-end" ]
  | Background ->
      [
        key "background-attachment";
        key "background-blend-mode";
        key "background-clip";
        key "background-color";
        key "background-image";
        key "background-origin";
        key "background-position";
        key "background-repeat";
        key "background-size";
      ]
  | Background_attachment -> [ key "background-attachment" ]
  | Background_blend_mode -> [ key "background-blend-mode" ]
  | Background_clip | Webkit_background_clip -> [ key "background-clip" ]
  | Background_color -> [ key "background-color" ]
  | Background_image -> [ key "background-image" ]
  | Background_origin -> [ key "background-origin" ]
  | Background_position -> [ key "background-position" ]
  | Background_repeat -> [ key "background-repeat" ]
  | Background_size | Webkit_background_size -> [ key "background-size" ]
  | Flex -> [ key "flex-grow"; key "flex-shrink"; key "flex-basis" ]
  | Flex_grow -> [ key "flex-grow" ]
  | Flex_shrink -> [ key "flex-shrink" ]
  | Flex_basis -> [ key "flex-basis" ]
  | Flex_flow -> [ key "flex-direction"; key "flex-wrap" ]
  | Flex_direction -> [ key "flex-direction" ]
  | Flex_wrap -> [ key "flex-wrap" ]
  (* A vendor-prefixed spelling is an alias of the unprefixed property in every
     engine that supports it - [deduplicate_declarations] already drops the
     prefixed copy of an identical twin - so it writes the same cascade slots
     and carries the same footprint. The prefixed vocabulary need not match the
     unprefixed one - [-webkit-mask-composite] spells [xor] where
     [mask-composite] spells [exclude] - since the slot is named by the property
     and not by the value. *)
  | Transform | Webkit_transform | Moz_transform | Ms_transform | O_transform ->
      [ key "transform" ]
  | Appearance | Webkit_appearance | Moz_appearance -> [ key "appearance" ]
  | Box_shadow | Webkit_box_shadow | Moz_box_shadow -> [ key "box-shadow" ]
  | Box_sizing | Webkit_box_sizing | Moz_box_sizing -> [ key "box-sizing" ]
  | User_select | Webkit_user_select | Moz_user_select | Ms_user_select ->
      [ key "user-select" ]
  | Filter | Webkit_filter | Ms_filter -> [ key "filter" ]
  | Backdrop_filter | Webkit_backdrop_filter -> [ key "backdrop-filter" ]
  | Box_decoration_break | Webkit_box_decoration_break ->
      [ key "box-decoration-break" ]
  | Hyphens | Webkit_hyphens -> [ key "hyphens" ]
  | Print_color_adjust | Webkit_print_color_adjust ->
      [ key "print-color-adjust" ]
  | Text_size_adjust | Webkit_text_size_adjust -> [ key "text-size-adjust" ]
  | Transition | Webkit_transition | Moz_transition | O_transition ->
      transition_keys
  | Transition_property | Webkit_transition_property | Moz_transition_property
    ->
      [ key "transition-property" ]
  | Transition_duration | Webkit_transition_duration | Moz_transition_duration
    ->
      [ key "transition-duration" ]
  | Transition_timing_function | Webkit_transition_timing_function
  | Moz_transition_timing_function ->
      [ key "transition-timing-function" ]
  | Transition_delay | Webkit_transition_delay | Moz_transition_delay ->
      [ key "transition-delay" ]
  | Transition_behavior -> [ key "transition-behavior" ]
  | Animation | Webkit_animation | Moz_animation -> animation_keys
  | Animation_name | Webkit_animation_name | Moz_animation_name ->
      [ key "animation-name" ]
  | Animation_duration | Webkit_animation_duration | Moz_animation_duration ->
      [ key "animation-duration" ]
  | Animation_timing_function | Webkit_animation_timing_function
  | Moz_animation_timing_function ->
      [ key "animation-timing-function" ]
  | Animation_delay | Webkit_animation_delay | Moz_animation_delay ->
      [ key "animation-delay" ]
  | Animation_iteration_count | Webkit_animation_iteration_count
  | Moz_animation_iteration_count ->
      [ key "animation-iteration-count" ]
  | Animation_direction | Webkit_animation_direction | Moz_animation_direction
    ->
      [ key "animation-direction" ]
  | Animation_fill_mode | Webkit_animation_fill_mode | Moz_animation_fill_mode
    ->
      [ key "animation-fill-mode" ]
  | Animation_play_state | Webkit_animation_play_state
  | Moz_animation_play_state ->
      [ key "animation-play-state" ]
  | Animation_timeline -> [ key "animation-timeline" ]
  (* Scroll-driven Animations 1 sec. 5.3: [animation-range] is its own
     shorthand, and [animation] does not reset either end. *)
  | Animation_range ->
      [ key "animation-range-start"; key "animation-range-end" ]
  | Animation_range_start -> [ key "animation-range-start" ]
  | Animation_range_end -> [ key "animation-range-end" ]
  | Border ->
      [
        key "border-top-width";
        key "border-right-width";
        key "border-bottom-width";
        key "border-left-width";
        key "border-top-style";
        key "border-right-style";
        key "border-bottom-style";
        key "border-left-style";
        key "border-top-color";
        key "border-right-color";
        key "border-bottom-color";
        key "border-left-color";
      ]
      @ border_image_keys
  | Border_width ->
      [
        key "border-top-width";
        key "border-right-width";
        key "border-bottom-width";
        key "border-left-width";
      ]
  | Border_style ->
      [
        key "border-top-style";
        key "border-right-style";
        key "border-bottom-style";
        key "border-left-style";
      ]
  | Border_color ->
      [
        key "border-top-color";
        key "border-right-color";
        key "border-bottom-color";
        key "border-left-color";
      ]
  | Border_top ->
      [ key "border-top-width"; key "border-top-style"; key "border-top-color" ]
  | Border_right ->
      [
        key "border-right-width";
        key "border-right-style";
        key "border-right-color";
      ]
  | Border_bottom ->
      [
        key "border-bottom-width";
        key "border-bottom-style";
        key "border-bottom-color";
      ]
  | Border_left ->
      [
        key "border-left-width";
        key "border-left-style";
        key "border-left-color";
      ]
  | Border_top_width -> [ key "border-top-width" ]
  | Border_right_width -> [ key "border-right-width" ]
  | Border_bottom_width -> [ key "border-bottom-width" ]
  | Border_left_width -> [ key "border-left-width" ]
  | Border_top_style -> [ key "border-top-style" ]
  | Border_right_style -> [ key "border-right-style" ]
  | Border_bottom_style -> [ key "border-bottom-style" ]
  | Border_left_style -> [ key "border-left-style" ]
  | Border_top_color -> [ key "border-top-color" ]
  | Border_right_color -> [ key "border-right-color" ]
  | Border_bottom_color -> [ key "border-bottom-color" ]
  | Border_left_color -> [ key "border-left-color" ]
  | Border_image -> border_image_keys
  | Border_image_source -> [ key "border-image-source" ]
  | Border_image_slice -> [ key "border-image-slice" ]
  | Border_image_width -> [ key "border-image-width" ]
  | Border_image_outset -> [ key "border-image-outset" ]
  | Border_image_repeat -> [ key "border-image-repeat" ]
  | Border_radius | Webkit_border_radius | Moz_border_radius ->
      border_radius_keys
  | Border_top_left_radius -> [ key "border-top-left-radius" ]
  | Border_top_right_radius -> [ key "border-top-right-radius" ]
  | Border_bottom_right_radius -> [ key "border-bottom-right-radius" ]
  | Border_bottom_left_radius -> [ key "border-bottom-left-radius" ]
  | Border_inline_start_width -> [ key "border-inline-start-width" ]
  | Border_inline_end_width -> [ key "border-inline-end-width" ]
  | Border_block_start_width -> [ key "border-block-start-width" ]
  | Border_block_end_width -> [ key "border-block-end-width" ]
  | Border_inline_start_style -> [ key "border-inline-start-style" ]
  | Border_inline_end_style -> [ key "border-inline-end-style" ]
  | Border_block_start_style -> [ key "border-block-start-style" ]
  | Border_block_end_style -> [ key "border-block-end-style" ]
  | Border_inline_start_color -> [ key "border-inline-start-color" ]
  | Border_inline_end_color -> [ key "border-inline-end-color" ]
  | Border_block_start_color -> [ key "border-block-start-color" ]
  | Border_block_end_color -> [ key "border-block-end-color" ]
  | Border_inline_color ->
      [ key "border-inline-start-color"; key "border-inline-end-color" ]
  | Border_block_color ->
      [ key "border-block-start-color"; key "border-block-end-color" ]
  | Border_inline_width ->
      [ key "border-inline-start-width"; key "border-inline-end-width" ]
  | Border_block_width ->
      [ key "border-block-start-width"; key "border-block-end-width" ]
  | Border_inline_style ->
      [ key "border-inline-start-style"; key "border-inline-end-style" ]
  | Border_block_style ->
      [ key "border-block-start-style"; key "border-block-end-style" ]
  | Border_block -> border_block_start_keys @ border_block_end_keys
  | Border_block_start -> border_block_start_keys
  | Border_block_end -> border_block_end_keys
  | Border_inline -> border_inline_start_keys @ border_inline_end_keys
  | Border_inline_start -> border_inline_start_keys
  | Border_inline_end -> border_inline_end_keys
  | Mask ->
      [
        key "mask-image";
        key "mask-repeat";
        key "mask-size";
        key "mask-position";
        key "mask-origin";
        key "mask-clip";
        key "mask-mode";
        key "mask-composite";
        key "mask-border";
      ]
  | Mask_image | Webkit_mask_image -> [ key "mask-image" ]
  | Mask_repeat | Webkit_mask_repeat -> [ key "mask-repeat" ]
  | Mask_size | Webkit_mask_size -> [ key "mask-size" ]
  | Mask_position | Webkit_mask_position -> [ key "mask-position" ]
  | Mask_origin | Webkit_mask_origin -> [ key "mask-origin" ]
  | Mask_clip | Webkit_mask_clip -> [ key "mask-clip" ]
  | Mask_mode -> [ key "mask-mode" ]
  | Mask_composite | Webkit_mask_composite -> [ key "mask-composite" ]
  | Mask_border -> [ key "mask-border" ]
  | Font ->
      [
        key "font-style";
        key "font-weight";
        key "font-stretch";
        key "font-size";
        key "line-height";
        key "font-family";
        key "font-variant-ligatures";
        key "font-variant-caps";
        key "font-variant-numeric";
        key "font-variant-position";
        key "font-variant-east-asian";
        key "font-variant-emoji";
        key "font-variation-settings";
        key "font-feature-settings";
        key "font-size-adjust";
        key "font-kerning";
        key "font-optical-sizing";
        key "font-language-override";
        key "font-palette";
      ]
  (* Motion Path 1 sec. 2.6. *)
  | Offset ->
      [
        key "offset-position";
        key "offset-path";
        key "offset-distance";
        key "offset-rotate";
        key "offset-anchor";
      ]
  | Font_style -> [ key "font-style" ]
  | Font_weight -> [ key "font-weight" ]
  | Font_stretch -> [ key "font-stretch" ]
  | Font_size -> [ key "font-size" ]
  | Line_height -> [ key "line-height" ]
  | Font_family -> [ key "font-family" ]
  | Font_variant_ligatures -> [ key "font-variant-ligatures" ]
  | Caps -> [ key "font-variant-caps" ]
  | Numeric -> [ key "font-variant-numeric" ]
  | Font_variant_position -> [ key "font-variant-position" ]
  | East_asian -> [ key "font-variant-east-asian" ]
  | Font_variant_emoji -> [ key "font-variant-emoji" ]
  | Font_variation_settings -> [ key "font-variation-settings" ]
  | Font_feature_settings -> [ key "font-feature-settings" ]
  | Font_size_adjust -> [ key "font-size-adjust" ]
  | Font_kerning -> [ key "font-kerning" ]
  | Font_optical_sizing -> [ key "font-optical-sizing" ]
  | Font_language_override -> [ key "font-language-override" ]
  | Font_palette -> [ key "font-palette" ]
  (* CSS Fonts 4 sec. 2.8.5: [font-synthesis] is its own shorthand, outside the
     set [font] resets. *)
  | Font_synthesis ->
      [
        key "font-synthesis-weight";
        key "font-synthesis-style";
        key "font-synthesis-small-caps";
        key "font-synthesis-position";
      ]
  | Font_synthesis_weight -> [ key "font-synthesis-weight" ]
  | Font_synthesis_style -> [ key "font-synthesis-style" ]
  | Font_synthesis_small_caps -> [ key "font-synthesis-small-caps" ]
  | Font_synthesis_position -> [ key "font-synthesis-position" ]
  | Gap -> row_gap_keys @ column_gap_keys
  | Row_gap -> row_gap_keys
  | Column_gap -> column_gap_keys
  (* CSS UI 4 sec. 3.1: [outline] resets width, style and colour. It leaves
     [outline-offset] alone - that one is a sibling, not a longhand. *)
  | Outline -> [ key "outline-width"; key "outline-style"; key "outline-color" ]
  | Outline_width -> [ key "outline-width" ]
  | Outline_style -> [ key "outline-style" ]
  | Outline_color -> [ key "outline-color" ]
  | Grid -> grid_template_keys @ grid_auto_keys
  | Grid_template -> grid_template_keys
  | Grid_template_rows -> [ key "grid-template-rows" ]
  | Grid_template_columns -> [ key "grid-template-columns" ]
  | Grid_template_areas -> [ key "grid-template-areas" ]
  | Grid_auto_rows -> [ key "grid-auto-rows" ]
  | Grid_auto_columns -> [ key "grid-auto-columns" ]
  | Grid_auto_flow -> [ key "grid-auto-flow" ]
  (* CSS Grid 1 sec. 8.4: the placement shorthands reset the four line
     longhands; [grid] resets none of them. *)
  | Grid_area -> grid_row_keys @ grid_column_keys
  | Grid_row -> grid_row_keys
  | Grid_column -> grid_column_keys
  | Grid_row_start -> [ key "grid-row-start" ]
  | Grid_row_end -> [ key "grid-row-end" ]
  | Grid_column_start -> [ key "grid-column-start" ]
  | Grid_column_end -> [ key "grid-column-end" ]
  (* CSS Box Alignment 3 sec. 5.2, 6.3 and 7.3. *)
  | Place_content -> [ key "align-content"; key "justify-content" ]
  | Place_items -> [ key "align-items"; key "justify-items" ]
  | Place_self -> [ key "align-self"; key "justify-self" ]
  | Align_content | Webkit_align_content -> [ key "align-content" ]
  | Justify_content | Webkit_justify_content -> [ key "justify-content" ]
  | Align_items | Webkit_align_items -> [ key "align-items" ]
  | Justify_items -> [ key "justify-items" ]
  | Align_self | Webkit_align_self -> [ key "align-self" ]
  | Justify_self -> [ key "justify-self" ]
  | Webkit_flex_flow -> [ key "flex-direction"; key "flex-wrap" ]
  | Webkit_flex_direction -> [ key "flex-direction" ]
  | Webkit_flex_wrap -> [ key "flex-wrap" ]
  (* CSS Overflow 3 sec. 3.1. *)
  | Overflow -> [ key "overflow-x"; key "overflow-y" ]
  | Overflow_x -> [ key "overflow-x" ]
  | Overflow_y -> [ key "overflow-y" ]
  | Overflow_block -> [ key "overflow-block" ]
  | Overflow_inline -> [ key "overflow-inline" ]
  (* CSS Multicol 1 sec. 3.3. *)
  | Columns -> [ key "column-width"; key "column-count" ]
  | Column_width -> [ key "column-width" ]
  | Column_count -> [ key "column-count" ]
  (* CSS Multicol 1 sec. 4.4: [column-rule] resets the rule width, style and
     colour. Naming those three is what keeps [column-rule-color] from reading
     as a slot of its own that [column-rule] never touches. *)
  | Column_rule ->
      [
        key "column-rule-width";
        key "column-rule-style";
        key "column-rule-color";
      ]
  | Column_rule_color -> [ key "column-rule-color" ]
  (* CSS Fragmentation 3 sec. 3.4: a [page-break-*] alias writes the slot of the
     [break-*] property it aliases. *)
  | Break_before -> [ key "break-before" ]
  | Page_break_before -> [ key "break-before" ]
  | Break_after -> [ key "break-after" ]
  | Page_break_after -> [ key "break-after" ]
  | Break_inside -> [ key "break-inside" ]
  | Page_break_inside -> [ key "break-inside" ]
  (* CSS Lists 3 sec. 3.6. *)
  | List_style ->
      [
        key "list-style-type"; key "list-style-position"; key "list-style-image";
      ]
  | List_style_type -> [ key "list-style-type" ]
  | List_style_position -> [ key "list-style-position" ]
  | List_style_image -> [ key "list-style-image" ]
  | Text_decoration | Webkit_text_decoration -> text_decoration_keys
  | Text_decoration_line -> [ key "text-decoration-line" ]
  | Text_decoration_style -> [ key "text-decoration-style" ]
  | Text_decoration_color | Webkit_text_decoration_color ->
      [ key "text-decoration-color" ]
  | Text_decoration_thickness -> [ key "text-decoration-thickness" ]
  (* CSS Text Decoration 4 sec. 2.10: [text-decoration-skip] is its own
     shorthand over the five skip longhands. *)
  | Text_decoration_skip ->
      [
        key "text-decoration-skip-self";
        key "text-decoration-skip-box";
        key "text-decoration-skip-inset";
        key "text-decoration-skip-spaces";
        key "text-decoration-skip-ink";
      ]
  | Text_decoration_skip_self -> [ key "text-decoration-skip-self" ]
  | Text_decoration_skip_box -> [ key "text-decoration-skip-box" ]
  | Text_decoration_skip_inset -> [ key "text-decoration-skip-inset" ]
  | Text_decoration_skip_spaces -> [ key "text-decoration-skip-spaces" ]
  | Text_decoration_skip_ink -> [ key "text-decoration-skip-ink" ]
  (* CSS Text Decoration 4 sec. 3.4: [text-emphasis] resets style and colour;
     [text-emphasis-position] and [text-emphasis-skip] stay independent. *)
  | Text_emphasis -> [ key "text-emphasis-style"; key "text-emphasis-color" ]
  | Text_emphasis_style -> [ key "text-emphasis-style" ]
  | Text_emphasis_color -> [ key "text-emphasis-color" ]
  (* CSS Sizing 4 sec. 5.2. The block/inline pair is flow-relative. *)
  | Contain_intrinsic_size ->
      [ key "contain-intrinsic-width"; key "contain-intrinsic-height" ]
  | Contain_intrinsic_width -> [ key "contain-intrinsic-width" ]
  | Contain_intrinsic_height -> [ key "contain-intrinsic-height" ]
  (* CSS Scroll Snap 1 sec. 4.2 and 5.1. *)
  | Scroll_margin -> scroll_margin_keys
  | Scroll_margin_top -> [ key "scroll-margin-top" ]
  | Scroll_margin_right -> [ key "scroll-margin-right" ]
  | Scroll_margin_bottom -> [ key "scroll-margin-bottom" ]
  | Scroll_margin_left -> [ key "scroll-margin-left" ]
  | Scroll_margin_inline ->
      [ key "scroll-margin-inline-start"; key "scroll-margin-inline-end" ]
  | Scroll_margin_inline_start -> [ key "scroll-margin-inline-start" ]
  | Scroll_margin_inline_end -> [ key "scroll-margin-inline-end" ]
  | Scroll_margin_block ->
      [ key "scroll-margin-block-start"; key "scroll-margin-block-end" ]
  | Scroll_margin_block_start -> [ key "scroll-margin-block-start" ]
  | Scroll_margin_block_end -> [ key "scroll-margin-block-end" ]
  | Scroll_padding -> scroll_padding_keys
  | Scroll_padding_top -> [ key "scroll-padding-top" ]
  | Scroll_padding_right -> [ key "scroll-padding-right" ]
  | Scroll_padding_bottom -> [ key "scroll-padding-bottom" ]
  | Scroll_padding_left -> [ key "scroll-padding-left" ]
  | Scroll_padding_inline ->
      [ key "scroll-padding-inline-start"; key "scroll-padding-inline-end" ]
  | Scroll_padding_inline_start -> [ key "scroll-padding-inline-start" ]
  | Scroll_padding_inline_end -> [ key "scroll-padding-inline-end" ]
  | Scroll_padding_block ->
      [ key "scroll-padding-block-start"; key "scroll-padding-block-end" ]
  | Scroll_padding_block_start -> [ key "scroll-padding-block-start" ]
  | Scroll_padding_block_end -> [ key "scroll-padding-block-end" ]
  (* CSS Overscroll Behavior 1 sec. 3. *)
  | Overscroll_behavior ->
      [ key "overscroll-behavior-x"; key "overscroll-behavior-y" ]
  | Overscroll_behavior_x -> [ key "overscroll-behavior-x" ]
  | Overscroll_behavior_y -> [ key "overscroll-behavior-y" ]
  (* CSS Contain 3 sec. 4.3. *)
  | Container -> [ key "container-name"; key "container-type" ]
  | Container_name -> [ key "container-name" ]
  | Container_type -> [ key "container-type" ]
  (* Scroll-driven Animations 1 sec. 2.3.3 and 3.4.4. *)
  | Scroll_timeline ->
      [ key "scroll-timeline-name"; key "scroll-timeline-axis" ]
  | Scroll_timeline_name -> [ key "scroll-timeline-name" ]
  | Scroll_timeline_axis -> [ key "scroll-timeline-axis" ]
  | View_timeline ->
      [
        key "view-timeline-name";
        key "view-timeline-axis";
        key "view-timeline-inset";
      ]
  | View_timeline_name -> [ key "view-timeline-name" ]
  | View_timeline_axis -> [ key "view-timeline-axis" ]
  (* CSS UI 4 sec. 5.2.4. *)
  | Caret -> [ key "caret-color"; key "caret-animation"; key "caret-shape" ]
  | Caret_color -> [ key "caret-color" ]
  | Caret_animation -> [ key "caret-animation" ]
  | Caret_shape -> [ key "caret-shape" ]
  | Interest_delay -> [ key "interest-delay-start"; key "interest-delay-end" ]
  | Interest_delay_start -> [ key "interest-delay-start" ]
  | Interest_delay_end -> [ key "interest-delay-end" ]
  (* CSS Inline 3 sec. 6.1. *)
  | Text_box -> [ key "text-box-trim"; key "text-box-edge" ]
  | Text_box_trim -> [ key "text-box-trim" ]
  | Text_box_edge -> [ key "text-box-edge" ]
  (* CSS Text 4 sec. 3 and 5.1: [white-space] and [text-wrap] both reset
     [text-wrap-mode]. *)
  | White_space -> [ key "white-space-collapse"; key "text-wrap-mode" ]
  | Text_wrap -> [ key "text-wrap-mode"; key "text-wrap-style" ]
  | Text_wrap_mode -> [ key "text-wrap-mode" ]
  | Text_wrap_style -> [ key "text-wrap-style" ]
  (* CSS Anchor Positioning 1 sec. 6.3. *)
  | Position_try -> [ key "position-try-order"; key "position-try-fallbacks" ]
  | Position_try_order -> [ key "position-try-order" ]
  | Position_try_fallbacks -> [ key "position-try-fallbacks" ]
  | Custom_property name -> [ key ("--" ^ name) ]
  | Unknown_property name -> [ key name ]
  | property -> [ property_key property ]

(* CSS Logical 1 sec. 2 and 4: a flow-relative longhand resolves to a physical
   side that the writing mode and the text direction pick, so
   [margin-inline-start] writes [margin-left] under one mode and [margin-right]
   under another. A stylesheet does not carry the mode of the elements it will
   match, so each logical family is paired here with the physical family it
   resolves into, and every logical slot takes the whole physical set. The two
   logical slots of one family then look like they share a slot, which the
   perpendicular axes never do; that direction of the approximation costs only a
   pair kept in source order, and it leaves the physical sides - the common case
   - naming one slot each. *)
let logical_alias_families =
  Properties.
    [
      ([ Prop Margin_inline; Prop Margin_block ], [ Prop Margin ]);
      ([ Prop Padding_inline; Prop Padding_block ], [ Prop Padding ]);
      ([ Prop Inset_inline; Prop Inset_block ], [ Prop Inset ]);
      ( [ Prop Border_inline_width; Prop Border_block_width ],
        [ Prop Border_width ] );
      ( [ Prop Border_inline_style; Prop Border_block_style ],
        [ Prop Border_style ] );
      ( [ Prop Border_inline_color; Prop Border_block_color ],
        [ Prop Border_color ] );
      ( [ Prop Scroll_margin_inline; Prop Scroll_margin_block ],
        [ Prop Scroll_margin ] );
      ( [ Prop Scroll_padding_inline; Prop Scroll_padding_block ],
        [ Prop Scroll_padding ] );
      ([ Prop Overflow_block; Prop Overflow_inline ], [ Prop Overflow ]);
      ([ Prop Inline_size; Prop Block_size ], [ Prop Width; Prop Height ]);
      ( [ Prop Min_inline_size; Prop Min_block_size ],
        [ Prop Min_width; Prop Min_height ] );
      ( [ Prop Max_inline_size; Prop Max_block_size ],
        [ Prop Max_width; Prop Max_height ] );
      ( [
          Prop Border_start_start_radius;
          Prop Border_start_end_radius;
          Prop Border_end_start_radius;
          Prop Border_end_end_radius;
        ],
        [ Prop Border_radius ] );
      ( [ Prop Contain_intrinsic_inline_size; Prop Contain_intrinsic_block_size ],
        [ Prop Contain_intrinsic_width; Prop Contain_intrinsic_height ] );
      ( [ Prop Overscroll_behavior_inline; Prop Overscroll_behavior_block ],
        [ Prop Overscroll_behavior_x; Prop Overscroll_behavior_y ] );
    ]

(* Each logical slot paired with the physical slots it may alias, sorted for
   binary search. One family shares a single physical list across its slots, so
   [add_logical_aliases] can fold the repeats away by physical equality. *)
let logical_alias_index =
  let entries =
    List.concat_map
      (fun (logical, physical) ->
        let physical_slots =
          List.concat_map (fun (Properties.Prop p) -> property_slots p) physical
        in
        List.concat_map
          (fun l ->
            let slots = match l with Properties.Prop p -> property_slots p in
            List.map (fun k -> (k, physical_slots)) slots)
          logical)
      logical_alias_families
  in
  let arr = Array.of_list entries in
  Array.sort (fun (a, _) (b, _) -> compare a b) arr;
  arr

(* The physical slots [k] may alias, empty when [k] is not a flow-relative slot.
   Spelled as a search over a sorted array rather than a hashtable lookup: this
   runs once per footprint key, and an option per key would allocate in the
   optimizer's hottest path. *)
let logical_alias_slots k =
  let lo = ref 0 and hi = ref (Array.length logical_alias_index - 1) in
  let physical = ref [] in
  let searching = ref true in
  while !searching && !lo <= !hi do
    let mid = (!lo + !hi) / 2 in
    let v, slots = Array.unsafe_get logical_alias_index mid in
    if overlap_key_equal v k then (
      physical := slots;
      searching := false)
    else if v < k then lo := mid + 1
    else hi := mid - 1
  done;
  !physical

let add_logical_aliases slots =
  let rec collect acc = function
    | [] -> acc
    | k :: rest -> (
        match logical_alias_slots k with
        | [] -> collect acc rest
        | physical when List.memq physical acc -> collect acc rest
        | physical -> collect (physical :: acc) rest)
  in
  match collect [] slots with
  | [] -> slots
  | groups -> List.concat (slots :: groups)

let property_footprint : type a. a Properties.property -> overlap_key list =
 fun property -> add_logical_aliases (property_slots property)

(* Spelled as explicit recursion rather than [List.exists (overlap_key_equal
   footprint)]: the partial application and the closure over [b] each allocate
   once per element, and this pair sits in the conflict test's inner loop, which
   made them the two largest allocation sites in the optimizer. *)
let rec footprint_mem footprint = function
  | [] -> false
  | k :: rest -> overlap_key_equal footprint k || footprint_mem footprint rest

let rec overlap_keys_intersect a b =
  match a with
  | [] -> false
  | footprint :: rest ->
      footprint_mem footprint b || overlap_keys_intersect rest b

(* The families whose footprints spell out every longhand name the model knows.
   Every arm of [property_footprint] not listed here has a footprint that is a
   subset of one of these. Grouped only to keep each list short. *)
let box_footprint_family_heads =
  Properties.
    [
      Prop Margin;
      Prop Margin_inline;
      Prop Margin_block;
      Prop Padding;
      Prop Padding_inline;
      Prop Padding_block;
      Prop Inset;
      Prop Inset_inline;
      Prop Inset_block;
      Prop Scroll_margin;
      Prop Scroll_margin_inline;
      Prop Scroll_margin_block;
      Prop Scroll_padding;
      Prop Scroll_padding_inline;
      Prop Scroll_padding_block;
    ]

let layout_footprint_family_heads =
  Properties.
    [
      Prop Flex;
      Prop Flex_flow;
      Prop Gap;
      Prop Grid;
      Prop Grid_area;
      Prop Place_content;
      Prop Place_items;
      Prop Place_self;
      Prop Overflow;
      Prop Overscroll_behavior;
      Prop Columns;
      Prop Column_rule;
      Prop Contain_intrinsic_size;
      Prop Container;
      Prop Offset;
    ]

let paint_footprint_family_heads =
  Properties.
    [
      Prop Background;
      Prop Border;
      Prop Border_block;
      Prop Border_inline;
      Prop Border_radius;
      Prop Mask;
      Prop Outline;
      Prop List_style;
    ]

let text_footprint_family_heads =
  Properties.
    [
      Prop Font;
      Prop Font_synthesis;
      Prop Text_decoration;
      Prop Text_decoration_skip;
      Prop Text_emphasis;
      Prop Text_box;
      Prop Text_wrap;
      Prop White_space;
      Prop Caret;
    ]

let timing_footprint_family_heads =
  Properties.
    [
      Prop Transition;
      Prop Animation;
      Prop Animation_range;
      Prop Scroll_timeline;
      Prop View_timeline;
      Prop Interest_delay;
      Prop Position_try;
    ]

let footprint_family_heads =
  List.concat
    [
      box_footprint_family_heads;
      layout_footprint_family_heads;
      paint_footprint_family_heads;
      text_footprint_family_heads;
      timing_footprint_family_heads;
    ]

(* Every longhand name those footprints mention, sorted for binary search. Taken
   from [property_footprint] itself rather than respelled, so the two cannot
   drift. *)
let known_footprint_keys =
  let keys =
    List.concat_map
      (fun (Properties.Prop p) -> property_footprint p)
      footprint_family_heads
  in
  let arr = Array.of_list keys in
  Array.sort compare arr;
  arr

let is_known_footprint_key k =
  let lo = ref 0 and hi = ref (Array.length known_footprint_keys - 1) in
  let found = ref false in
  while (not !found) && !lo <= !hi do
    let mid = (!lo + !hi) / 2 in
    let v = Array.unsafe_get known_footprint_keys mid in
    if overlap_key_equal v k then found := true
    else if v < k then lo := mid + 1
    else hi := mid - 1
  done;
  !found

(* Whether an [Unknown_property] name can be placed in the footprint model. A
   name a typed footprint mentions writes the slot it names and, for a
   flow-relative name, the physical slots [add_logical_aliases] gives it: a
   typed longhand is recovered under its own name when its value defeats the
   typed reader ([margin-top:var(--a) var(--b)] parses that way), and it
   conflicts with the same declarations the typed spelling would - the name
   carries the same footprint either way. Any other name may be a shorthand, a
   legacy alias, or a longhand of a family the footprints do not model -
   [background-position-x] writes part of [background], [grid-row-gap] is
   [row-gap] - so it has to be treated as touching whatever it is compared
   against. *)
let unknown_name_is_placeable name = is_known_footprint_key (key name)

(* Whether one name is the other with a further hyphenated component, the shape
   a longhand takes under its shorthand. Two placeable names of that shape write
   a common slot even though neither footprint mentions the other. *)
let name_extends other name =
  let n = String.length other in
  String.length name > n
  && String.starts_with ~prefix:other name
  && Char.equal name.[n] '-'

(* A name the model cannot place takes the broad key, not a key naming itself:
   [declarations_overlap_with_keys] treats it as touching whatever it meets, and
   a footprint naming only itself let a caller's key prefilter judge the pair
   disjoint and rule the real test out. *)
let rec declaration_overlap_keys decl =
  match decl with
  | Theme_guarded { decl; _ } -> declaration_overlap_keys decl
  | Declaration { property = Unknown_property name; _ }
    when not (unknown_name_is_placeable name) ->
      [ broad_overlap_key ]
  | Declaration { property; _ } -> property_footprint property

let declarations_overlap_with_keys a a_keys b b_keys =
  match (unwrap_theme_guard a, unwrap_theme_guard b) with
  | ( Declaration { property = Custom_property a; _ },
      Declaration { property = Custom_property b; _ } ) ->
      String.equal a b
  | Declaration { property = Custom_property _; _ }, _ -> false
  | _, Declaration { property = Custom_property _; _ } -> false
  | Declaration { property = All; _ }, Declaration { property; _ } ->
      not (is_excluded_from_all_reset property)
  | Declaration { property; _ }, Declaration { property = All; _ } ->
      not (is_excluded_from_all_reset property)
  | ( Declaration { property = Unknown_property a; _ },
      Declaration { property = Unknown_property b; _ } ) ->
      String.equal a b || name_extends a b || name_extends b a
      || not (unknown_name_is_placeable a && unknown_name_is_placeable b)
  | Declaration { property = Unknown_property name; _ }, Declaration _ ->
      (not (unknown_name_is_placeable name))
      || overlap_keys_intersect a_keys b_keys
  | Declaration _, Declaration { property = Unknown_property name; _ } ->
      (not (unknown_name_is_placeable name))
      || overlap_keys_intersect a_keys b_keys
  | Declaration _, Declaration _ -> overlap_keys_intersect a_keys b_keys
  | _ -> false

let declarations_overlap a b =
  declarations_overlap_with_keys a
    (declaration_overlap_keys a)
    b
    (declaration_overlap_keys b)

(* A declaration no footprint comparison separates from another: [all] resets
   every non-exempt slot, and a property outside the model expands to slots its
   name does not spell out. *)
let declaration_is_broad decl =
  match unwrap_theme_guard decl with
  | Declaration { property = All; _ } -> true
  | Declaration { property = Unknown_property _; _ } -> true
  | _ -> false

(* The name a custom property declaration writes, read through a theme guard. A
   custom property overlaps the declarations writing that same name and nothing
   else, so its name is its whole footprint. *)
let custom_property_name decl =
  match unwrap_theme_guard decl with
  | Declaration { property = Custom_property name; _ } -> Some name
  | _ -> None

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

(* A shorthand carries the same image the longhand does, so the prefixed
   spelling is the same fallback whichever property writes it. [background] and
   [mask] carry one image per layer; [border-image] and [mask-border] carry a
   single source. *)
let background_layer_is_vendor (layer : Properties.background) =
  match layer with
  | Shorthand { image = Some image; _ } -> background_image_is_vendor image
  | _ -> false

let mask_layer_is_vendor (layer : Properties.mask_layer) =
  match layer.image with
  | Some image -> background_image_is_vendor image
  | _ -> false

let mask_value_is_vendor (value : Properties.mask) =
  match value with
  | Layer layer -> mask_layer_is_vendor layer
  | Layers layers -> List.exists mask_layer_is_vendor layers
  | _ -> false

let border_image_source_is_vendor (value : Properties.border_image) =
  match value.source with
  | Some image -> background_image_is_vendor image
  | _ -> false

(* A value whose rendering begins with a vendor prefix (-webkit-, -moz-, -ms-,
   -o-). Preserves legacy fallbacks like [display:-webkit-box;display:flex]: old
   browsers understand only the prefixed spelling, so dropping the earlier
   declaration removes a real compat fallback. *)
(* All the intrinsic sizing properties carry a [length_percentage] value. A GADT
   or-pattern does not refine the value type, so each constructor is matched on
   its own; everything else is not a sizing fallback. *)
let sizing_value_is_vendor_prefixed : type a. a Properties.property -> a -> bool
    =
 fun property value ->
  let pfx = Values.length_percentage_is_vendor_prefixed in
  match property with
  | Width -> pfx value
  | Height -> pfx value
  | Min_width -> pfx value
  | Min_height -> pfx value
  | Max_width -> pfx value
  | Max_height -> pfx value
  | Block_size -> pfx value
  | Inline_size -> pfx value
  | Min_block_size -> pfx value
  | Min_inline_size -> pfx value
  | Max_block_size -> pfx value
  | Max_inline_size -> pfx value
  | _ -> false

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
  | Declaration { property = Background; value; _ } ->
      List.exists background_layer_is_vendor value
  | Declaration { property = Mask; value; _ } -> mask_value_is_vendor value
  | Declaration { property = Border_image; value; _ } ->
      border_image_source_is_vendor value
  | Declaration { property = Mask_border; value; _ } ->
      border_image_source_is_vendor value
  (* [position:-webkit-sticky;position:sticky] and the [text-align] equivalent
     are the same browser-compat pattern: old Safari only understands the
     prefixed keyword, so the earlier declaration is a real fallback. *)
  | Declaration { property = Position; value = Webkit_sticky; _ } -> true
  | Declaration { property = Text_align; value = Webkit_match_parent; _ } ->
      true
  (* [width:-webkit-max-content;width:max-content] and the other intrinsic
     sizing properties: the prefixed keyword is a fallback for old Safari /
     Firefox, so the earlier declaration must survive minify dedup. *)
  | Declaration { property; value; _ } ->
      sizing_value_is_vendor_prefixed property value

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

(* Compose 2-longhand shorthands ([gap] from [row-gap] / [column-gap],
   [place-items] from [align-items] / [justify-items], etc) when both longhands
   appear contiguously with matching importance. *)
type pair_side = Row | Column

let extract_gap_side : declaration -> (pair_side * Values.length * bool) option
    = function
  | Declaration { property = Row_gap; value; important; _ } ->
      Some (Row, value, important)
  | Declaration { property = Column_gap; value; important; _ } ->
      Some (Column, value, important)
  | _ -> None

let try_compose_gap_at idx i =
  let n = Rule_index.length idx in
  if
    i + 1 >= n
    || Rule_index.is_absorbed idx i
    || Rule_index.is_absorbed idx (i + 1)
  then None
  else
    let d1 = Rule_index.decl_at idx i in
    let d2 = Rule_index.decl_at idx (i + 1) in
    match (extract_gap_side d1, extract_gap_side d2) with
    | Some (s1, v1, imp1), Some (s2, v2, imp2)
      when imp1 = imp2 && s1 <> s2
           && (not (Values.length_has_runtime_subst v1))
           && not (Values.length_has_runtime_subst v2) ->
        let pair = [ (s1, v1); (s2, v2) ] in
        let find s = List.assoc s pair in
        Some
          (Declaration.v ~important:imp1 Gap
             (Lengths
                { row_gap = Some (find Row); column_gap = Some (find Column) }))
    | _ -> None

(* Compose [<base>-inline] / [<base>-block] from the matching [-start] / [-end]
   longhands, and the same walk over the two physical axes of an
   [overflow]-shaped family, where [Start] is the x value and [End] the y. The
   longhands carry one value of the family's own type, and [build] turns the
   ordered pair into the shorthand's payload - a [length list] for the box
   families, a [Single] / [Pair] for the border ones - or declines a pair the
   shorthand has no spelling for. *)
type axis_side = Start | End

let try_compose_axis_pair_at idx ~foldable ~extract ~build i =
  let n = Rule_index.length idx in
  if
    i + 1 >= n
    || Rule_index.is_absorbed idx i
    || Rule_index.is_absorbed idx (i + 1)
  then None
  else
    let d1 = Rule_index.decl_at idx i in
    let d2 = Rule_index.decl_at idx (i + 1) in
    match (extract d1, extract d2) with
    | Some (s1, v1, imp1), Some (s2, v2, imp2)
      when imp1 = imp2 && s1 <> s2 && foldable v1 && foldable v2 ->
        let pair = [ (s1, v1); (s2, v2) ] in
        build ~important:imp1 ~start:(List.assoc Start pair)
          ~end_:(List.assoc End pair)
    | _ -> None

let extract_margin_inline_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Margin_inline_start; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Margin_inline_end; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_margin_block_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Margin_block_start; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Margin_block_end; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

(* CSS Scroll Snap 1 sec. 6.1 and 6.2: the scroll-margin and scroll-padding
   logical axes take [<length>{1,2}], the shape the margin and padding axes
   take, so the same pair composition applies. *)
let extract_scroll_margin_block_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Scroll_margin_block_start; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Scroll_margin_block_end; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_scroll_margin_inline_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Scroll_margin_inline_start; value; important; _ }
    ->
      Some (Start, value, important)
  | Declaration { property = Scroll_margin_inline_end; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_scroll_padding_block_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Scroll_padding_block_start; value; important; _ }
    ->
      Some (Start, value, important)
  | Declaration { property = Scroll_padding_block_end; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_scroll_padding_inline_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Scroll_padding_inline_start; value; important; _ }
    ->
      Some (Start, value, important)
  | Declaration { property = Scroll_padding_inline_end; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_padding_inline_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Padding_inline_start; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Padding_inline_end; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_padding_block_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Padding_block_start; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Padding_block_end; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_inset_inline_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Inset_inline_start; value = [ v ]; important; _ }
    ->
      Some (Start, v, important)
  | Declaration { property = Inset_inline_end; value = [ v ]; important; _ } ->
      Some (End, v, important)
  | _ -> None

let extract_inset_block_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Inset_block_start; value = [ v ]; important; _ } ->
      Some (Start, v, important)
  | Declaration { property = Inset_block_end; value = [ v ]; important; _ } ->
      Some (End, v, important)
  | _ -> None

(* CSS Logical 1 sec. 4.3 and 4.4: [border-block-*] and [border-inline-*] take
   one or two values of the side longhand's own type, so the two sides compose
   the way the length axes do. *)
let extract_border_inline_width_side :
    declaration -> (axis_side * Properties.border_width * bool) option =
  function
  | Declaration { property = Border_inline_start_width; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Border_inline_end_width; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_border_block_width_side :
    declaration -> (axis_side * Properties.border_width * bool) option =
  function
  | Declaration { property = Border_block_start_width; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Border_block_end_width; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_border_inline_style_side :
    declaration -> (axis_side * Properties.border_style * bool) option =
  function
  | Declaration { property = Border_inline_start_style; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Border_inline_end_style; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_border_block_style_side :
    declaration -> (axis_side * Properties.border_style * bool) option =
  function
  | Declaration { property = Border_block_start_style; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Border_block_end_style; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_border_inline_color_side :
    declaration -> (axis_side * Values.color * bool) option = function
  | Declaration { property = Border_inline_start_color; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Border_inline_end_color; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_border_block_color_side :
    declaration -> (axis_side * Values.color * bool) option = function
  | Declaration { property = Border_block_start_color; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Border_block_end_color; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

(* CSS Overscroll 1 sec. 2.1 and CSS Sizing 4 sec. 5.1 write the x axis first
   and the y axis second, the order [overflow] uses, so the physical pair takes
   the same walk as a logical axis. *)
let extract_overscroll_side :
    declaration -> (axis_side * Properties.overscroll_behavior * bool) option =
  function
  | Declaration { property = Overscroll_behavior_x; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Overscroll_behavior_y; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_contain_intrinsic_side :
    declaration ->
    (axis_side * Properties.contain_intrinsic_longhand * bool) option = function
  | Declaration { property = Contain_intrinsic_width; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Contain_intrinsic_height; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

(* CSS Grid 2 sec. 8.3: [grid-row] and [grid-column] are [<grid-line> [/
   <grid-line>]?] over their own start and end longhands. A substituted line can
   stand for the whole [<start> / <end>], so only a resolved one takes part. *)
let extract_grid_row_side :
    declaration -> (axis_side * Properties.grid_line * bool) option = function
  | Declaration { property = Grid_row_start; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Grid_row_end; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_grid_column_side :
    declaration -> (axis_side * Properties.grid_line * bool) option = function
  | Declaration { property = Grid_column_start; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Grid_column_end; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

(* CSS Align 3 sec. 5.2, 6.3 and 7.3: [place-items] / [place-content] /
   [place-self] are the [<align> <justify>] shorthands. When the two longhands
   appear contiguously with matching importance, fold them; the per-property
   printer then collapses matching pairs to a single value. *)
let try_compose_place_at idx i =
  let n = Rule_index.length idx in
  if
    i + 1 >= n
    || Rule_index.is_absorbed idx i
    || Rule_index.is_absorbed idx (i + 1)
  then None
  else
    let d1 = Rule_index.decl_at idx i in
    let d2 = Rule_index.decl_at idx (i + 1) in
    match (d1, d2) with
    | ( Declaration { property = Align_items; value = a; important = i1; _ },
        Declaration { property = Justify_items; value = j; important = i2; _ } )
      when i1 = i2 ->
        Some
          (Declaration.v ~important:i1 Place_items
             (Align_justify (a, j) : Properties.place_items))
    | ( Declaration { property = Justify_items; value = j; important = i1; _ },
        Declaration { property = Align_items; value = a; important = i2; _ } )
      when i1 = i2 ->
        Some
          (Declaration.v ~important:i1 Place_items
             (Align_justify (a, j) : Properties.place_items))
    | ( Declaration { property = Align_content; value = a; important = i1; _ },
        Declaration { property = Justify_content; value = j; important = i2; _ }
      )
      when i1 = i2 ->
        Some
          (Declaration.v ~important:i1 Place_content
             (Align_justify (a, j) : Properties.place_content))
    | ( Declaration { property = Justify_content; value = j; important = i1; _ },
        Declaration { property = Align_content; value = a; important = i2; _ } )
      when i1 = i2 ->
        Some
          (Declaration.v ~important:i1 Place_content
             (Align_justify (a, j) : Properties.place_content))
    | ( Declaration { property = Align_self; value = a; important = i1; _ },
        Declaration { property = Justify_self; value = j; important = i2; _ } )
      when i1 = i2 ->
        Some (Declaration.v ~important:i1 Place_self (a, j))
    | ( Declaration { property = Justify_self; value = j; important = i1; _ },
        Declaration { property = Align_self; value = a; important = i2; _ } )
      when i1 = i2 ->
        Some (Declaration.v ~important:i1 Place_self (a, j))
    | _ -> None

(* Fold an axis pair into the shorthand's own payload: a length list for the box
   families, a [Single] / [Pair] for the border ones. *)
let axis_length idx property extract i =
  let build ~important ~start ~end_ =
    let value =
      if Values.equal_length start end_ then [ start ] else [ start; end_ ]
    in
    Some (Declaration.v ~important property value)
  in
  try_compose_axis_pair_at idx ~foldable:foldable_length_strict ~extract ~build
    i

let axis_border_width ~ctx idx property extract i =
  let build ~important ~start ~end_ =
    let value : Properties.logical_border_width =
      if same_border_width start end_ then Single start else Pair (start, end_)
    in
    Some (Declaration.v ~important property value)
  in
  try_compose_axis_pair_at idx
    ~foldable:(foldable_border_width ~ctx)
    ~extract ~build i

let axis_border_style ~ctx idx property extract i =
  let build ~important ~start ~end_ =
    let value : Properties.logical_border_style =
      if same_border_style start end_ then Single start else Pair (start, end_)
    in
    Some (Declaration.v ~important property value)
  in
  try_compose_axis_pair_at idx
    ~foldable:(foldable_border_style ~ctx)
    ~extract ~build i

let axis_overscroll idx i =
  let build ~important ~start ~end_ =
    let value =
      if Properties.equal_overscroll_behavior start end_ then [ start ]
      else [ start; end_ ]
    in
    Some (Declaration.v ~important Overscroll_behavior value)
  in
  let foldable : Properties.overscroll_behavior -> bool = function
    | Var _ -> false
    | _ -> true
  in
  try_compose_axis_pair_at idx ~foldable ~extract:extract_overscroll_side ~build
    i

(* [contain-intrinsic-size] says one axis at [none] only by saying it for both,
   so a [none] beside a sized axis has no shorthand spelling. *)
let axis_contain_intrinsic idx i =
  let build ~important ~start ~end_ =
    let value : Properties.contain_intrinsic_size option =
      match
        ( (start : Properties.contain_intrinsic_longhand),
          (end_ : Properties.contain_intrinsic_longhand) )
      with
      | None, None -> Some None
      | Size a, Size b ->
          if Properties.equal_contain_intrinsic_size_item a b then
            Some (Intrinsic (a, Option.None))
          else Some (Intrinsic (a, Some b))
      | _ -> Option.None
    in
    Option.map (Declaration.v ~important Contain_intrinsic_size) value
  in
  let foldable : Properties.contain_intrinsic_longhand -> bool = function
    | Var _ -> false
    | _ -> true
  in
  try_compose_axis_pair_at idx ~foldable ~extract:extract_contain_intrinsic_side
    ~build i

let axis_grid_line idx property extract i =
  let build ~important ~start ~end_ =
    Some
      (Declaration.v ~important property
         (Lines (start, end_) : Properties.grid_line_pair))
  in
  try_compose_axis_pair_at idx ~foldable:foldable_grid_line ~extract ~build i

let axis_border_color ~ctx idx property extract i =
  let build ~important ~start ~end_ =
    let value : Properties.logical_border_color =
      if Values.equal_color start end_ then Single start else Pair (start, end_)
    in
    Some (Declaration.v ~important property value)
  in
  try_compose_axis_pair_at idx
    ~foldable:(foldable_border_color ~ctx)
    ~extract ~build i

(* CSS Flexbox 1 sec. 5.1 and CSS Text Decoration 4 sec. 3.4: [flex-flow] and
   [text-emphasis] each take two longhands, one per component. A longhand
   written [initial] fills its slot in the run and contributes no value, the
   normalisers then dropping any component that names its own initial. A
   substituted longhand can stand for the whole value and is left alone. *)
let extract_flex_flow_part :
    declaration ->
    (axis_side
    * (Properties.flex_direction option * Properties.flex_wrap option)
    * bool)
    option = function
  | Declaration { property = Flex_direction; value = Var _; _ }
  | Declaration { property = Flex_wrap; value = Var _; _ } ->
      None
  | Declaration { property = Flex_direction; value; important; _ } ->
      let d = match value with Initial -> Option.None | v -> Some v in
      Some (Start, (d, Option.None), important)
  | Declaration { property = Flex_wrap; value; important; _ } ->
      let w = match value with Initial -> Option.None | v -> Some v in
      Some (End, (Option.None, w), important)
  | _ -> None

let extract_text_emphasis_part :
    declaration ->
    (axis_side
    * (Properties.text_emphasis_style option * Values.color option)
    * bool)
    option = function
  | Declaration { property = Text_emphasis_style; value = Var _; _ }
  | Declaration { property = Text_emphasis_color; value = Var _; _ } ->
      None
  | Declaration { property = Text_emphasis_style; value; important; _ } ->
      let st = match value with Initial -> Option.None | v -> Some v in
      Some (Start, (st, Option.None), important)
  | Declaration { property = Text_emphasis_color; value; important; _ } ->
      let c = match value with Initial -> Option.None | v -> Some v in
      Some (End, (Option.None, c), important)
  | _ -> None

let duo_flex_flow idx i =
  let build ~important ~start ~end_ =
    let direction, _ = start and _, wrap = end_ in
    Some
      (Declaration.v ~important Flex_flow
         (Flow (direction, wrap) : Properties.flex_flow))
  in
  try_compose_axis_pair_at idx
    ~foldable:(fun _ -> true)
    ~extract:extract_flex_flow_part ~build i

let duo_text_emphasis idx i =
  let build ~important ~start ~end_ =
    let style, _ = start and _, color = end_ in
    Some
      (Declaration.v ~important Text_emphasis
         (Emphasis (style, color) : Properties.text_emphasis))
  in
  try_compose_axis_pair_at idx
    ~foldable:(fun _ -> true)
    ~extract:extract_text_emphasis_part ~build i

(* CSS Animations 2 sec. 6.3 and CSS Scroll Animations 1 sec. 4.3:
   [animation-range] is [<start> <end>?] and [scroll-timeline] is [<name>
   <axis>?], each written over its own two longhands. *)
let extract_animation_range_part :
    declaration ->
    (axis_side
    * (Properties.animation_range_item option
      * Properties.animation_range_item option)
    * bool)
    option = function
  | Declaration { property = Animation_range_start; value = Var _; _ }
  | Declaration { property = Animation_range_end; value = Var _; _ } ->
      None
  | Declaration { property = Animation_range_start; value; important; _ } ->
      Some (Start, (Some value, Option.None), important)
  | Declaration { property = Animation_range_end; value; important; _ } ->
      Some (End, (Option.None, Some value), important)
  | _ -> None

let extract_scroll_timeline_part :
    declaration ->
    (axis_side
    * (Properties.timeline_name option * Properties.timeline_axis option)
    * bool)
    option = function
  | Declaration { property = Scroll_timeline_name; value = Var _; _ }
  | Declaration { property = Scroll_timeline_axis; value = Var _; _ } ->
      None
  | Declaration { property = Scroll_timeline_name; value; important; _ } ->
      Some (Start, (Some value, Option.None), important)
  | Declaration { property = Scroll_timeline_axis; value; important; _ } ->
      Some (End, (Option.None, Some value), important)
  | _ -> None

let duo_animation_range idx i =
  let build ~important ~start ~end_ =
    let range_start, _ = start and _, range_end = end_ in
    match range_start with
    | Option.None -> Option.None
    | Some range_start ->
        Some
          (Declaration.v ~important Animation_range
             (Range (range_start, range_end) : Properties.animation_range))
  in
  try_compose_axis_pair_at idx
    ~foldable:(fun _ -> true)
    ~extract:extract_animation_range_part ~build i

(* [scroll-timeline: none] leaves the axis at [block], so a named axis beside an
   unnamed timeline has no shorthand spelling, and neither has a name list: the
   shorthand pairs each name with its own axis. *)
let duo_scroll_timeline idx i =
  let build ~important ~start ~end_ =
    let name, _ = start and _, axis = end_ in
    let axis =
      match axis with
      | Some (Block : Properties.timeline_axis) | Some Initial -> Option.None
      | axis -> axis
    in
    let value : Properties.timeline_shorthand option =
      match (name : Properties.timeline_name option) with
      | Some None when Option.is_none axis -> Some None
      | Some (Names [ n ]) -> Some (Timelines [ { name = n; axis } ])
      | _ -> Option.None
    in
    Option.map (Declaration.v ~important Scroll_timeline) value
  in
  try_compose_axis_pair_at idx
    ~foldable:(fun _ -> true)
    ~extract:extract_scroll_timeline_part ~build i

(* One entry per logical axis family; each pairs a start longhand with its end
   longhand under the shorthand that names the axis. *)
let pair_axes ~ctx idx i =
  let axis = axis_length idx in
  let width = axis_border_width ~ctx idx in
  let style = axis_border_style ~ctx idx in
  let color = axis_border_color ~ctx idx in
  [
    (fun () -> axis Margin_inline extract_margin_inline_side i);
    (fun () -> axis Margin_block extract_margin_block_side i);
    (fun () -> axis Padding_inline extract_padding_inline_side i);
    (fun () -> axis Padding_block extract_padding_block_side i);
    (fun () -> axis Inset_inline extract_inset_inline_side i);
    (fun () -> axis Inset_block extract_inset_block_side i);
    (fun () -> axis Scroll_margin_inline extract_scroll_margin_inline_side i);
    (fun () -> axis Scroll_margin_block extract_scroll_margin_block_side i);
    (fun () -> axis Scroll_padding_inline extract_scroll_padding_inline_side i);
    (fun () -> axis Scroll_padding_block extract_scroll_padding_block_side i);
    (fun () -> width Border_inline_width extract_border_inline_width_side i);
    (fun () -> width Border_block_width extract_border_block_width_side i);
    (fun () -> style Border_inline_style extract_border_inline_style_side i);
    (fun () -> style Border_block_style extract_border_block_style_side i);
    (fun () -> color Border_inline_color extract_border_inline_color_side i);
    (fun () -> color Border_block_color extract_border_block_color_side i);
    (fun () -> axis_overscroll idx i);
    (fun () -> axis_contain_intrinsic idx i);
    (fun () -> axis_grid_line idx Grid_row extract_grid_row_side i);
    (fun () -> axis_grid_line idx Grid_column extract_grid_column_side i);
    (fun () -> duo_flex_flow idx i);
    (fun () -> duo_text_emphasis idx i);
    (fun () -> duo_animation_range idx i);
    (fun () -> duo_scroll_timeline idx i);
  ]

let compose_pair_via_index ~ctx idx =
  let try_any i =
    match try_compose_gap_at idx i with
    | Some _ as r -> r
    | None -> (
        match List.find_map (fun f -> f ()) (pair_axes ~ctx idx i) with
        | Some _ as r -> r
        | None -> try_compose_place_at idx i)
  in
  let n = Rule_index.length idx in
  let i = ref 0 in
  while !i < n do
    match try_any !i with
    | Some shorthand ->
        if Rule_index.absorb idx ~at:!i ~absorbed:[ !i; !i + 1 ] ~shorthand then
          i := !i + 2
        else incr i
    | None -> incr i
  done

(* Walk the rule for a family whose shorthand always absorbs exactly three
   longhands, absorbing every run [try_compose] accepts. *)
let compose_fixed3_via_index idx ~try_compose =
  let n = Rule_index.length idx in
  let i = ref 0 in
  while !i + 2 < n do
    match try_compose idx !i with
    | None -> incr i
    | Some shorthand ->
        if
          Rule_index.absorb idx ~at:!i
            ~absorbed:[ !i; !i + 1; !i + 2 ]
            ~shorthand
        then i := !i + 3
        else incr i
  done

(* Same walk for a family whose run length varies: [try_compose] reports the
   shorthand together with the number of positions it consumes. An earlier
   composer may already own a position, so absorbed slots are skipped. *)
let compose_run_via_index idx ~try_compose =
  let n = Rule_index.length idx in
  let i = ref 0 in
  while !i < n do
    if Rule_index.is_absorbed idx !i then incr i
    else
      match try_compose idx !i with
      | None -> incr i
      | Some (shorthand, k) ->
          let absorbed = List.init k (fun j -> !i + j) in
          if Rule_index.absorb idx ~at:!i ~absorbed ~shorthand then i := !i + k
          else incr i
  done

(* Collect the contiguous run of one family's longhands starting at [i], as
   (decl, part) pairs, with its length. *)
let take_run_at idx ~part_of i =
  let n = Rule_index.length idx in
  let rec aux j acc =
    if j >= n then (List.rev acc, j - i)
    else if Rule_index.is_absorbed idx j then (List.rev acc, j - i)
    else
      let d = Rule_index.decl_at idx j in
      match part_of d with
      | Some f -> aux (j + 1) ((d, f) :: acc)
      | None -> (List.rev acc, j - i)
  in
  aux i []

(* Compose [outline-width / -style / -color] into the [outline] shorthand when
   all three longhands appear contiguously with matching importance. *)
type line_part = Width | Style | Color

let outline_part_of : declaration -> line_part option = function
  | Declaration { property = Outline_width; _ } -> Some Width
  | Declaration { property = Outline_style; _ } -> Some Style
  | Declaration { property = Outline_color; _ } -> Some Color
  | _ -> None

let outline_width_value : declaration -> Properties.border_width option =
  function
  | Declaration { property = Outline_width; value; _ } -> Some value
  | _ -> None

let outline_style_value : declaration -> Properties.outline_style option =
  function
  | Declaration { property = Outline_style; value; _ } -> Some value
  | _ -> None

let outline_color_value : declaration -> Values.color option = function
  | Declaration { property = Outline_color; value; _ } -> Some value
  | _ -> None

(* Index-based composer: locate Outline_width / Outline_style / Outline_color in
   the rule, check that they form a contiguous run with matching importance,
   then absorb them into a single Outline shorthand. *)
let try_compose_outline_at idx i =
  let n = Rule_index.length idx in
  if i + 2 >= n then None
  else if
    Rule_index.is_absorbed idx i
    || Rule_index.is_absorbed idx (i + 1)
    || Rule_index.is_absorbed idx (i + 2)
  then None
  else
    let d1 = Rule_index.decl_at idx i in
    let d2 = Rule_index.decl_at idx (i + 1) in
    let d3 = Rule_index.decl_at idx (i + 2) in
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
          | Some w -> not (Properties.border_width_has_runtime_subst w)
          | None -> true
        in
        if no_runtime then
          Some
            (Declaration.v ~important:(is_important d1) Outline
               (Shorthand { width; style; color }))
        else None
    | _ -> None

let compose_outline_via_index idx =
  compose_fixed3_via_index idx ~try_compose:try_compose_outline_at

(* CSS Fonts 4 sec. 2.7: [font] reads [<style>? <weight>?
   <size>[/<line-height>]? <family>+]. Cascade stores [font] as a string, so
   composition renders each longhand and stitches them together; default-valued
   components ([normal] style, [400] weight, [normal] line-height) drop on emit.
   Requires font-size and font-family. *)
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

let try_compose_font_at idx i =
  let n = Rule_index.length idx in
  if i + 4 >= n then None
  else
    let positions = [ i; i + 1; i + 2; i + 3; i + 4 ] in
    if List.exists (Rule_index.is_absorbed idx) positions then None
    else
      let raw_decls = List.map (Rule_index.decl_at idx) positions in
      if
        (not (List.for_all is_font_longhand raw_decls))
        || not (same_importance raw_decls)
      then None
      else
        match render_font_shorthand raw_decls with
        | Some font_value ->
            Some
              (Declaration.v
                 ~important:(is_important (List.hd raw_decls))
                 Font font_value)
        | None -> None

let compose_font_via_index idx =
  let n = Rule_index.length idx in
  let i = ref 0 in
  while !i + 4 < n do
    if Rule_index.is_absorbed idx !i then incr i
    else
      match try_compose_font_at idx !i with
      | None -> incr i
      | Some shorthand ->
          let absorbed = [ !i; !i + 1; !i + 2; !i + 3; !i + 4 ] in
          if Rule_index.absorb idx ~at:!i ~absorbed ~shorthand then i := !i + 5
          else incr i
  done

(* The property a name spells, when the reader types one.
   [Properties.read_any_property] is the table [pp_property] inverts, so a name
   resolves to the very constructor the typed matches below test. *)
let property_of_name name =
  let t = Cursor.of_string name in
  match
    let property = Properties.read_any_property t in
    Cursor.expect_eof t;
    property
  with
  | property -> Some property
  | exception Cursor.Parse_error _ -> None

(* The property a declaration writes. A typed value reader that rejects its
   input leaves the declaration as [Unknown_property] under the property's own
   name ([font-kerning:var(--a) var(--b)] reads that way), and it still names
   that property, so the name is read back into its constructor. *)
let named_property decl =
  match unwrap_theme_guard decl with
  | Declaration { property = Unknown_property name; _ } -> property_of_name name
  | Declaration { property; _ } -> Some (Properties.Prop property)
  | _ -> None

(* CSS Fonts 4 sec. 2.7: [font] takes these six subproperties from its own value
   and returns every other one it covers to its initial. Both halves are [Font]
   arms of [covers_longhand]. *)
let is_font_slot_property : type a. a Properties.property -> bool = function
  | Font_style | Font_weight | Font_stretch | Font_size | Line_height
  | Font_family ->
      true
  | _ -> false

let is_font_reset_property : type a. a Properties.property -> bool = function
  | Font_variant_ligatures | Caps | Numeric | Font_variant_position | East_asian
  | Font_variant_emoji | Font_variation_settings | Font_feature_settings
  | Font_size_adjust | Font_kerning | Font_optical_sizing ->
      true
  | _ -> false

(* [font] resets the [font-variant-*] / [font-variation-settings] /
   [font-feature-settings] / [font-size-adjust] / [font-kerning] /
   [font-optical-sizing] subproperties to their initials. When such a reset
   precedes a foldable run of [font] longhands, move it after the run so the
   synthesised [font] does not clobber it (as
   [reorder_border_image_before_border]). *)
let reorder_font_resets_before_font decls =
  let is_font_reset d =
    match named_property (snd d) with
    | Some (Properties.Prop p) -> is_font_reset_property p
    | None -> false
  in
  let is_font_longhand d =
    match named_property (snd d) with
    | Some (Properties.Prop p) -> is_font_slot_property p
    | None -> false
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
        let has key =
          List.exists
            (fun d ->
              match named_property (snd d) with
              | Some (Properties.Prop p) -> equal_prop_key (Key p) key
              | None -> false)
            long_block
        in
        if has (Key Properties.Font_size) && has (Key Properties.Font_family)
        then
          (* [long_block ++ reset_block] reversed onto acc, tail-recursively and
             without (@) on a large LHS. *)
          go
            (List.rev_append reset_block (List.rev_append long_block acc))
            rest2
        else go (List.rev_append reset_block acc) rest1
    | d :: rest -> go (d :: acc) rest
  in
  go [] decls

(* CSS Lists 3 sec. 3.6: [list-style: <position> <image> <type>] in any order,
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

let try_compose_list_style_at idx i =
  let n = Rule_index.length idx in
  if i + 2 >= n then None
  else if
    Rule_index.is_absorbed idx i
    || Rule_index.is_absorbed idx (i + 1)
    || Rule_index.is_absorbed idx (i + 2)
  then None
  else
    let d1 = Rule_index.decl_at idx i in
    let d2 = Rule_index.decl_at idx (i + 1) in
    let d3 = Rule_index.decl_at idx (i + 2) in
    if
      is_list_style_longhand d1 && is_list_style_longhand d2
      && is_list_style_longhand d3
      && is_important d1 = is_important d2
      && is_important d2 = is_important d3
    then
      Some
        (Declaration.v ~important:(is_important d1) List_style
           (render_list_style [ d1; d2; d3 ]))
    else None

let compose_list_style_via_index idx =
  compose_fixed3_via_index idx ~try_compose:try_compose_list_style_at

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

let try_compose_flex_at idx i =
  let n = Rule_index.length idx in
  if i + 2 >= n then None
  else if
    Rule_index.is_absorbed idx i
    || Rule_index.is_absorbed idx (i + 1)
    || Rule_index.is_absorbed idx (i + 2)
  then None
  else
    let d1 = Rule_index.decl_at idx i in
    let d2 = Rule_index.decl_at idx (i + 1) in
    let d3 = Rule_index.decl_at idx (i + 2) in
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
            Some
              (Declaration.v ~important:(is_important d1) Flex (Full (g, s, b)))
        | _ -> None)
    | _ -> None

let compose_flex_via_index idx =
  compose_fixed3_via_index idx ~try_compose:try_compose_flex_at

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

let try_compose_text_decoration_at idx i =
  let n = Rule_index.length idx in
  if i + 2 >= n then None
  else if
    Rule_index.is_absorbed idx i
    || Rule_index.is_absorbed idx (i + 1)
    || Rule_index.is_absorbed idx (i + 2)
  then None
  else
    let d1 = Rule_index.decl_at idx i in
    let d2 = Rule_index.decl_at idx (i + 1) in
    let d3 = Rule_index.decl_at idx (i + 2) in
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
            Some
              (Declaration.v ~important:(is_important d1) Text_decoration
                 (Shorthand { lines; style; color; thickness = None }))
        | _ -> None)
    | _ -> None

let compose_text_decoration_via_index idx =
  compose_fixed3_via_index idx ~try_compose:try_compose_text_decoration_at

(* CSS Backgrounds 3 sec. 3.4: [border] is the shorthand for [border-{top,
   right,bottom,left}-{width,style,color}]. Cascade composes when all 12
   longhands appear in a contiguous run with matching importance, every width /
   style / color is uniform across the four sides, and no runtime-substitution
   value would change the resolved shape. *)
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

let try_compose_border_at idx i =
  let n = Rule_index.length idx in
  if i + 11 >= n then None
  else
    let absorbed =
      [
        i;
        i + 1;
        i + 2;
        i + 3;
        i + 4;
        i + 5;
        i + 6;
        i + 7;
        i + 8;
        i + 9;
        i + 10;
        i + 11;
      ]
    in
    if List.exists (Rule_index.is_absorbed idx) absorbed then None
    else
      let raw_decls = List.map (Rule_index.decl_at idx) absorbed in
      if not (same_importance raw_decls) then None
      else if List.exists has_runtime_substitution raw_decls then None
      else
        match border_parts_of raw_decls with
        | None -> None
        | Some (widths, styles, colors) ->
            Some
              (declaration_of_border_parts
                 ~important:(is_important (List.hd raw_decls))
                 widths styles colors)

let compose_border_via_index idx =
  let n = Rule_index.length idx in
  let i = ref 0 in
  while !i + 11 < n do
    if Rule_index.is_absorbed idx !i then incr i
    else
      match try_compose_border_at idx !i with
      | None -> incr i
      | Some shorthand ->
          let absorbed =
            [
              !i;
              !i + 1;
              !i + 2;
              !i + 3;
              !i + 4;
              !i + 5;
              !i + 6;
              !i + 7;
              !i + 8;
              !i + 9;
              !i + 10;
              !i + 11;
            ]
          in
          if Rule_index.absorb idx ~at:!i ~absorbed ~shorthand then i := !i + 12
          else incr i
  done

(* CSS Backgrounds 3 sec. 3.5, CSS Logical 1 sec. 4.5 and CSS Multicol 1 sec.
   4.3: each of the eight border sides is [<line-width> || <line-style> ||
   <line-color>] over its own three longhands and resets nothing else, so a
   contiguous run of the three composes the way [outline] does. Each part
   carries the slot it fills, so the run may be written in any order. *)
let no_line : Properties.border_shorthand =
  { width = None; style = None; color = None }

(* CSS Cascade 5 sec. 7.3: [initial] is the property's initial value, which is
   what the shorthand assigns to a component left out, so the longhand fills its
   slot in the run and contributes no value to it. *)
let line_of_width v : line_part * Properties.border_shorthand =
  match (v : Properties.border_width) with
  | Initial -> (Width, no_line)
  | v -> (Width, { no_line with width = Some v })

let line_of_style v : line_part * Properties.border_shorthand =
  match (v : Properties.border_style) with
  | Initial -> (Style, no_line)
  | v -> (Style, { no_line with style = Some v })

let line_of_color v : line_part * Properties.border_shorthand =
  match (v : Values.color) with
  | Initial -> (Color, no_line)
  | v -> (Color, { no_line with color = Some v })

let merge_line (a : Properties.border_shorthand)
    (b : Properties.border_shorthand) : Properties.border_shorthand =
  let pick x y = if Option.is_none x then y else x in
  {
    width = pick a.width b.width;
    style = pick a.style b.style;
    color = pick a.color b.color;
  }

let border_top_part = function
  | Declaration { property = Border_top_width; value; _ } ->
      Some (line_of_width value)
  | Declaration { property = Border_top_style; value; _ } ->
      Some (line_of_style value)
  | Declaration { property = Border_top_color; value; _ } ->
      Some (line_of_color value)
  | _ -> None

let border_right_part = function
  | Declaration { property = Border_right_width; value; _ } ->
      Some (line_of_width value)
  | Declaration { property = Border_right_style; value; _ } ->
      Some (line_of_style value)
  | Declaration { property = Border_right_color; value; _ } ->
      Some (line_of_color value)
  | _ -> None

let border_bottom_part = function
  | Declaration { property = Border_bottom_width; value; _ } ->
      Some (line_of_width value)
  | Declaration { property = Border_bottom_style; value; _ } ->
      Some (line_of_style value)
  | Declaration { property = Border_bottom_color; value; _ } ->
      Some (line_of_color value)
  | _ -> None

let border_left_part = function
  | Declaration { property = Border_left_width; value; _ } ->
      Some (line_of_width value)
  | Declaration { property = Border_left_style; value; _ } ->
      Some (line_of_style value)
  | Declaration { property = Border_left_color; value; _ } ->
      Some (line_of_color value)
  | _ -> None

let border_block_start_part = function
  | Declaration { property = Border_block_start_width; value; _ } ->
      Some (line_of_width value)
  | Declaration { property = Border_block_start_style; value; _ } ->
      Some (line_of_style value)
  | Declaration { property = Border_block_start_color; value; _ } ->
      Some (line_of_color value)
  | _ -> None

let border_block_end_part = function
  | Declaration { property = Border_block_end_width; value; _ } ->
      Some (line_of_width value)
  | Declaration { property = Border_block_end_style; value; _ } ->
      Some (line_of_style value)
  | Declaration { property = Border_block_end_color; value; _ } ->
      Some (line_of_color value)
  | _ -> None

let border_inline_start_part = function
  | Declaration { property = Border_inline_start_width; value; _ } ->
      Some (line_of_width value)
  | Declaration { property = Border_inline_start_style; value; _ } ->
      Some (line_of_style value)
  | Declaration { property = Border_inline_start_color; value; _ } ->
      Some (line_of_color value)
  | _ -> None

let border_inline_end_part = function
  | Declaration { property = Border_inline_end_width; value; _ } ->
      Some (line_of_width value)
  | Declaration { property = Border_inline_end_style; value; _ } ->
      Some (line_of_style value)
  | Declaration { property = Border_inline_end_color; value; _ } ->
      Some (line_of_color value)
  | _ -> None

let try_compose_line_at ~part_of ~property idx i =
  let n = Rule_index.length idx in
  if i + 2 >= n then None
  else
    let positions = [ i; i + 1; i + 2 ] in
    if List.exists (Rule_index.is_absorbed idx) positions then None
    else
      let raw = List.map (Rule_index.decl_at idx) positions in
      if (not (same_importance raw)) || List.exists has_runtime_substitution raw
      then None
      else
        match List.map part_of raw with
        | [ Some (p1, s1); Some (p2, s2); Some (p3, s3) ]
          when List.length (List.sort_uniq compare [ p1; p2; p3 ]) = 3 ->
            Some
              (Declaration.v
                 ~important:(is_important (List.hd raw))
                 property
                 (Shorthand (merge_line s1 (merge_line s2 s3))
                   : Properties.border))
        | _ -> None

(* CSS Logical 1 sec. 4.6: [border-block] and [border-inline] set the width,
   style and colour of both sides of their axis and reset nothing else, which is
   what the three axis shorthands set between them. An axis naming two different
   sides has no slot in the shorthand, so only a single-valued one takes
   part. *)
let border_block_axis_part = function
  | Declaration { property = Border_block_width; value = Single w; _ } ->
      Some (line_of_width w)
  | Declaration { property = Border_block_style; value = Single s; _ } ->
      Some (line_of_style s)
  | Declaration { property = Border_block_color; value = Single c; _ } ->
      Some (line_of_color c)
  | _ -> None

let border_inline_axis_part = function
  | Declaration { property = Border_inline_width; value = Single w; _ } ->
      Some (line_of_width w)
  | Declaration { property = Border_inline_style; value = Single s; _ } ->
      Some (line_of_style s)
  | Declaration { property = Border_inline_color; value = Single c; _ } ->
      Some (line_of_color c)
  | _ -> None

let line_families =
  Properties.
    [
      (border_top_part, Border_top);
      (border_right_part, Border_right);
      (border_bottom_part, Border_bottom);
      (border_left_part, Border_left);
      (border_block_start_part, Border_block_start);
      (border_block_end_part, Border_block_end);
      (border_inline_start_part, Border_inline_start);
      (border_inline_end_part, Border_inline_end);
      (border_block_axis_part, Border_block);
      (border_inline_axis_part, Border_inline);
    ]

let compose_line_via_index idx =
  List.iter
    (fun (part_of, property) ->
      compose_fixed3_via_index idx
        ~try_compose:(try_compose_line_at ~part_of ~property))
    line_families

(* Compose the [border] shorthand from the three whole-border longhands
   [border-width] / [border-style] / [border-color] when they appear as a
   contiguous run (any order, single-valued, same importance). The [border]
   shorthand also resets [border-image] to its initial, so only compose when no
   [border-image] declaration is present in the rule - otherwise the synthesised
   [border] would clobber it (the reset/reorder case is handled separately). *)
let try_compose_border_whole_at ~ctx idx i =
  let n = Rule_index.length idx in
  if i + 2 >= n then None
  else
    let positions = [ i; i + 1; i + 2 ] in
    if List.exists (Rule_index.is_absorbed idx) positions then None
    else
      let raw = List.map (Rule_index.decl_at idx) positions in
      if not (same_importance raw) then None
      else
        let width : Properties.border_width option ref = ref None in
        let style : Properties.border_style option ref = ref None in
        let color : Values.color option ref = ref None in
        List.iter
          (function
            | Declaration { property = Border_width; value = [ w ]; _ } ->
                width := Some w
            | Declaration { property = Border_style; value = [ s ]; _ } ->
                style := Some s
            | Declaration { property = Border_color; value = [ c ]; _ } ->
                color := Some c
            | _ -> ())
          raw;
        match (!width, !style, !color) with
        | Some width, Some style, Some color
          when foldable_border_width ~ctx width
               && foldable_border_style ~ctx style
               && foldable_border_color ~ctx color ->
            Some
              (Declaration.v
                 ~important:(is_important (List.hd raw))
                 Border
                 (Shorthand
                    {
                      width = Some width;
                      style = Some style;
                      color = Some color;
                    }))
        | _ -> None

(* [border-image*] and [border-width/style/color] are independent, so a
   border-image declaration immediately preceding the whole-border longhands can
   move after them without changing any cascade. That lets
   [compose_border_whole_shorthand] synthesise [border] in place: its
   border-image reset is overridden back by the now-trailing declaration. Only
   swap when the following run carries the full width/style/color trio. *)
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
        then
          go (List.rev_append img_block (List.rev_append long_block acc)) rest2
        else go (List.rev_append img_block acc) rest1
    | d :: rest -> go (d :: acc) rest
  in
  go [] decls

let compose_border_whole_via_index ~ctx idx =
  let n = Rule_index.length idx in
  (* [border] resets [border-image] to its initial, so the synthesised shorthand
     is only safe when it ends up before every [border-image] declaration. Walk
     positions; once we cross a border-image decl, stop trying to fold. *)
  let i = ref 0 in
  let seen_border_image = ref false in
  while !i < n do
    if Rule_index.is_absorbed idx !i then incr i
    else if !seen_border_image then (
      let d = Rule_index.decl_at idx !i in
      if is_border_image_decl d then ();
      incr i)
    else
      let step_over () =
        let d = Rule_index.decl_at idx !i in
        if is_border_image_decl d then seen_border_image := true;
        incr i
      in
      match try_compose_border_whole_at ~ctx idx !i with
      | Some shorthand ->
          let absorbed = [ !i; !i + 1; !i + 2 ] in
          if Rule_index.absorb idx ~at:!i ~absorbed ~shorthand then i := !i + 3
          else step_over ()
      | None -> step_over ()
  done

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

(* CSS Backgrounds 3 sec. 5.7: compose [border-image] from a contiguous run of
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

(* Collect contiguous border-image longhands starting at [i], returning the run
   and its length [k]. *)
let span_border_image_run_at idx i =
  let n = Rule_index.length idx in
  let rec aux j acc =
    if j >= n then (List.rev acc, j - i)
    else if Rule_index.is_absorbed idx j then (List.rev acc, j - i)
    else
      let d = Rule_index.decl_at idx j in
      if is_border_image_longhand_decl d then aux (j + 1) ((j, d) :: acc)
      else (List.rev acc, j - i)
  in
  aux i []

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

let try_compose_border_image_at idx i =
  let d = Rule_index.decl_at idx i in
  if not (is_border_image_longhand_decl d) then None
  else
    let run, len = span_border_image_run_at idx i in
    match compose_border_image_run run with
    | Some (_, shorthand) -> Some (shorthand, len)
    | None -> None

let compose_border_image_via_index ~ctx idx =
  if scope ctx <> `Stylesheet then ()
  else compose_run_via_index idx ~try_compose:try_compose_border_image_at

let compose_border_image_shorthand ~ctx decls =
  let idx = Rule_index.build (List.map snd decls) in
  compose_border_image_via_index ~ctx idx;
  List.mapi (fun i d -> (i, d)) (Rule_index.to_list idx)

(* CSS Backgrounds 3 sec. 2.10: [background] is the shorthand for the eight
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
let background_run_is_reset_closed (layer : Properties.background_shorthand) =
  Option.is_some layer.color && Option.is_some layer.image
  && Option.is_some layer.position
  && Option.is_some layer.size
  && Option.is_some layer.repeat
  && Option.is_some layer.attachment
  && Option.is_some layer.origin
  && Option.is_some layer.clip

(* A layer shorthand resets every layer field, so synthesizing it from a run
   silently reverts any same-family longhand sitting before the run (separated
   from it by an unrelated declaration). Even [`Stylesheet] scope, which is
   closed over external CSS only, must not shadow that earlier same-block
   longhand, so composition refuses when one exists. *)
let has_prior_family_longhand part_of idx i =
  let rec aux j =
    if j >= i then false
    else if Rule_index.is_absorbed idx j then aux (j + 1)
    else
      match part_of (Rule_index.decl_at idx j) with
      | Some _ -> true
      | None -> aux (j + 1)
  in
  aux 0

let try_compose_background_at ~ctx idx i =
  let parts, len = take_run_at idx ~part_of:background_part_of i in
  if List.length parts < 2 then None
  else
    let raw_decls = List.map fst parts in
    if not (same_importance raw_decls) then None
    else
      let layer =
        List.fold_left (fun acc (_, f) -> f acc) empty_bg_shorthand parts
      in
      (* [`Fragment] requires the run to cover every reset field; in
         [`Stylesheet] the caller asserts no prior author CSS exists that the
         shorthand could shadow. *)
      let permit =
        match scope ctx with
        | `Stylesheet ->
            not (has_prior_family_longhand background_part_of idx i)
        | `Fragment -> background_run_is_reset_closed layer
      in
      if not permit then None
      else
        let shorthand =
          Declaration.v
            ~important:(is_important (List.hd raw_decls))
            Background
            [ (Shorthand layer : Properties.background) ]
        in
        Some (shorthand, len)

let compose_background_via_index ~ctx idx =
  compose_run_via_index idx ~try_compose:(try_compose_background_at ~ctx)

let compose_background_shorthand ~ctx decls =
  let idx = Rule_index.build (List.map snd decls) in
  compose_background_via_index ~ctx idx;
  List.mapi (fun i d -> (i, d)) (Rule_index.to_list idx)

(* CSS Masking 1 sec. 7.9: [mask] is the layer shorthand for [mask-image] /
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

let try_compose_mask_at ~ctx idx i =
  let parts, len = take_run_at idx ~part_of:mask_part_of i in
  if List.length parts < 2 then None
  else
    let raw_decls = List.map fst parts in
    if not (same_importance raw_decls) then None
    else if scope ctx <> `Stylesheet then None
    else if has_prior_family_longhand mask_part_of idx i then None
    else
      let layer =
        List.fold_left (fun acc (_, f) -> f acc) empty_mask_layer parts
      in
      if layer.image = None then None
      else
        let shorthand =
          Declaration.v
            ~important:(is_important (List.hd raw_decls))
            Mask
            (Layer layer : Properties.mask)
        in
        Some (shorthand, len)

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

(* The [mask] shorthand resets [mask-border] (CSS Masking 1 sec. 7.9), so a
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
          go
            (List.rev_append border_block (List.rev_append long_block acc))
            rest2
        else go (List.rev_append border_block acc) rest1
    | d :: rest -> go (d :: acc) rest
  in
  go [] decls

let is_mask_border_decl_raw d =
  match d with Declaration { property = Mask_border; _ } -> true | _ -> false

let compose_mask_via_index ~ctx idx =
  let n = Rule_index.length idx in
  let i = ref 0 in
  let seen_mask_border = ref false in
  while !i < n do
    if Rule_index.is_absorbed idx !i then incr i
    else if !seen_mask_border then (
      let d = Rule_index.decl_at idx !i in
      if is_mask_border_decl_raw d then ();
      incr i)
    else
      let step_over () =
        let d = Rule_index.decl_at idx !i in
        if is_mask_border_decl_raw d then seen_mask_border := true;
        incr i
      in
      match try_compose_mask_at ~ctx idx !i with
      | Some (shorthand, k) ->
          let absorbed = List.init k (fun j -> !i + j) in
          if Rule_index.absorb idx ~at:!i ~absorbed ~shorthand then i := !i + k
          else step_over ()
      | None -> step_over ()
  done

(* The property a [transition-property] entry names, against the property a
   declaration writes. A shorthand there transitions every longhand it covers,
   so [transition-property: background] reaches [background-color]. A name
   outside the typed table names no property this can be asked about. *)
let named_property_covers : type a. string -> a Properties.property -> bool =
 fun name target ->
  match property_of_name name with
  | Some (Prop named) -> (
      match Properties.eq_property named target with
      | Some Equal -> true
      | None -> covers_longhand named target)
  | None -> false

(* CSS Transitions 1 sec. 2.1: [none] transitions nothing, [all] transitions
   everything, and [all] is the initial. A CSS-wide keyword resolves to that
   initial or to a list from outside the sheet, and a [var()] is unread, so both
   read as [all]. *)
let transition_entry_covers : type a.
    Properties.transition_property_value -> a Properties.property -> bool =
 fun entry target ->
  match entry with
  | None -> false
  | Property name -> named_property_covers name target
  | All | Initial | Inherit | Unset | Revert | Revert_layer | Var _ -> true

let transition_layer_covers : type a.
    Properties.transition -> a Properties.property -> bool =
 fun layer target ->
  match layer with
  | None -> false
  | Shorthand s -> transition_entry_covers s.property target
  | Inherit | Initial | Unset | Revert | Revert_layer | Var _ -> true

(* [all] is in because its only values are CSS-wide keywords, and [inherit]
   there brings the parent's transition down onto this element. *)
let declaration_transitions : type a.
    declaration -> a Properties.property -> bool =
 fun decl target ->
  let layers = List.exists (fun l -> transition_layer_covers l target) in
  let entries = List.exists (fun e -> transition_entry_covers e target) in
  match unwrap_theme_guard decl with
  | Declaration { property = Transition; value; _ } -> layers value
  | Declaration { property = Webkit_transition; value; _ } -> layers value
  | Declaration { property = Moz_transition; value; _ } -> layers value
  | Declaration { property = O_transition; value; _ } -> layers value
  | Declaration { property = Transition_property; value; _ } -> entries value
  | Declaration { property = Webkit_transition_property; value; _ } ->
      entries value
  | Declaration { property = Moz_transition_property; value; _ } ->
      entries value
  | Declaration { property = All; _ } -> true
  | _ -> false

let transitioned_in_rule decls decl =
  match unwrap_theme_guard decl with
  | Declaration { property; _ } ->
      List.exists (fun d -> declaration_transitions d property) decls
  | _ -> false

(* CSS Transitions 2 sec. 2.6: [transition] composes from
   [transition-{property,duration,timing-function,delay,behavior}]. Compose when
   a contiguous run covers a single layer (each longhand carries a one-entry
   list), the importance matches, no CSS-wide keyword leaks in, and
   [transition-property] is present (it has no default, so the shorthand needs
   it).

   The shorthand writes all five slots, so a run leaving one out resets that
   slot to its initial. That is a no-op unless something earlier in the rule
   already put another value there, which [tr_reset_hazard] rules out. *)
type tr_slot = Property | Duration | Timing | Delay | Behavior

let tr_slots = [ Property; Duration; Timing; Delay; Behavior ]

let tr_slot_bit : tr_slot -> int = function
  | Property -> 1
  | Duration -> 2
  | Timing -> 4
  | Delay -> 8
  | Behavior -> 16

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

(* [transition-behavior] is a [<single-transition>] component, so a run can
   carry it into the shorthand. A CSS-wide keyword has no component spelling,
   and a [var()] there reads back into the property slot, which the reader tries
   first. *)
let behavior_singleton :
    Properties.transition_behavior -> Properties.transition_behavior option =
  function
  | Inherit | Initial | Unset | Revert | Revert_layer | Var _ -> None
  | b -> Some b

let tr_behavior_part : declaration -> Properties.transition_behavior option =
  function
  | Declaration { property = Transition_behavior; value; _ } ->
      behavior_singleton value
  | _ -> None

type tr_part =
  Properties.transition_shorthand -> Properties.transition_shorthand

type tr_updater = declaration -> (tr_slot * tr_part) option

let lift_tr_part :
    'a.
    tr_slot ->
    (declaration -> 'a option) ->
    (Properties.transition_shorthand -> 'a -> Properties.transition_shorthand) ->
    tr_updater =
 fun slot extract set d ->
  match extract d with Some v -> Some (slot, fun s -> set s v) | None -> None

let tr_updaters : tr_updater list =
  [
    lift_tr_part Property tr_property_part (fun s v -> { s with property = v });
    lift_tr_part Duration tr_duration_part (fun s v ->
        { s with duration = Some v });
    lift_tr_part Timing tr_timing_part (fun s v ->
        { s with timing_function = Some v });
    lift_tr_part Delay tr_delay_part (fun s v -> { s with delay = Some v });
    lift_tr_part Behavior tr_behavior_part (fun s v ->
        { s with behavior = Some v });
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

let has_transition_property_decl raw_decls =
  List.exists
    (fun d ->
      match d with
      | Declaration { property = Transition_property; _ } -> true
      | _ -> false)
    raw_decls

let tr_bits slots = List.fold_left (fun acc s -> acc lor tr_slot_bit s) 0 slots
let all_tr_bits = tr_bits tr_slots

let tr_zero_duration : Values.duration -> bool = function
  | S 0. | Ms 0. -> true
  | _ -> false

let tr_layer_holds_slot (s : Properties.transition_shorthand) : tr_slot -> bool
    = function
  | Property -> ( match s.property with All -> false | _ -> true)
  | Duration -> (
      match s.duration with None -> false | Some d -> not (tr_zero_duration d))
  | Timing -> (
      match s.timing_function with None | Some Ease -> false | Some _ -> true)
  | Delay -> (
      match s.delay with None -> false | Some d -> not (tr_zero_duration d))
  | Behavior -> (
      match s.behavior with None | Some Normal -> false | Some _ -> true)

(* Slots [d] leaves holding something other than the slot initial: [all] for the
   property, [0s] for the two times, [ease] for the easing, [normal] for the
   behaviour. A slot written back to its initial is not at risk from a shorthand
   that resets it to the same thing. *)
let tr_overwritten_slots d =
  let bit slot cond = if cond then tr_slot_bit slot else 0 in
  match unwrap_theme_guard d with
  | Declaration { property = Transition_property; value; _ } ->
      bit Property (match value with [ All ] -> false | _ -> true)
  | Declaration { property = Transition_duration; value; _ } ->
      bit Duration (not (tr_zero_duration value))
  | Declaration { property = Transition_timing_function; value; _ } ->
      bit Timing (match value with Ease -> false | _ -> true)
  | Declaration { property = Transition_delay; value; _ } ->
      bit Delay (not (tr_zero_duration value))
  | Declaration { property = Transition_behavior; value; _ } ->
      bit Behavior (match value with Normal -> false | _ -> true)
  | Declaration { property = Transition; value = [ Shorthand s ]; _ } ->
      tr_bits (List.filter (tr_layer_holds_slot s) tr_slots)
  | Declaration { property = Transition; _ } -> all_tr_bits
  (* CSS Cascade 5 sec. 3.2: [all] writes every longhand. No transition longhand
     inherits, so [initial] and [unset] both leave the initials. *)
  | Declaration { property = All; value = Initial | Unset; _ } -> 0
  | Declaration { property = All; _ } -> all_tr_bits
  | _ -> 0

(* CSS Animations 2 sec. 4.11: the slots [animation] resets. The bits sit above
   the transition ones so a single [held] set can carry both families. *)
type an_slot =
  | Name
  | Duration
  | Timing
  | Delay
  | Iteration
  | Direction
  | Fill
  | Play
  | Timeline

let an_slots =
  [ Name; Duration; Timing; Delay; Iteration; Direction; Fill; Play; Timeline ]

let an_slot_bit : an_slot -> int = function
  | Name -> 32
  | Duration -> 64
  | Timing -> 128
  | Delay -> 256
  | Iteration -> 512
  | Direction -> 1024
  | Fill -> 2048
  | Play -> 4096
  | Timeline -> 8192

let an_bits slots = List.fold_left (fun acc s -> acc lor an_slot_bit s) 0 slots
let all_an_bits = an_bits an_slots

(* Which slots a declaration leaves holding something. Cruder than the
   transition answer, which compares against each slot initial: this one says
   "set at all". It is only ever consulted for slots MISSING from the run being
   composed, so the extra caution costs a contraction, never a cascade. *)
let an_overwritten_slots d =
  match unwrap_theme_guard d with
  | Declaration { property = Animation_name; _ } -> an_slot_bit Name
  | Declaration { property = Animation_duration; _ } -> an_slot_bit Duration
  | Declaration { property = Animation_timing_function; _ } ->
      an_slot_bit Timing
  | Declaration { property = Animation_delay; _ } -> an_slot_bit Delay
  | Declaration { property = Animation_iteration_count; _ } ->
      an_slot_bit Iteration
  | Declaration { property = Animation_direction; _ } -> an_slot_bit Direction
  | Declaration { property = Animation_fill_mode; _ } -> an_slot_bit Fill
  | Declaration { property = Animation_play_state; _ } -> an_slot_bit Play
  | Declaration { property = Animation_timeline; _ } -> an_slot_bit Timeline
  | Declaration { property = Animation; _ } -> all_an_bits
  (* CSS Cascade 5 sec. 3.2: [all] writes every longhand, and no animation
     longhand inherits, so [initial] and [unset] both leave the initials. *)
  | Declaration { property = All; value = Initial | Unset; _ } -> 0
  | Declaration { property = All; _ } -> all_an_bits
  | _ -> 0

(* Slots a set of declarations leaves holding something other than the slot
   initial, split by importance. Composition reads one rule, so a holder in
   another rule of the same run reaches the scan below only through this. *)
type held = { normal_slots : int; important_slots : int }

let held_none = { normal_slots = 0; important_slots = 0 }
let held_nothing h = Int.equal (h.normal_slots lor h.important_slots) 0

let held_add held decls =
  List.fold_left
    (fun held d ->
      let slots = tr_overwritten_slots d lor an_overwritten_slots d in
      if Int.equal slots 0 then held
      else if is_important d then
        { held with important_slots = held.important_slots lor slots }
      else { held with normal_slots = held.normal_slots lor slots })
    held decls

(* What the neighbours hold. The rule being composed is judged by the scan
   below, which reads position as well as value, so a slot it holds itself is
   dropped here rather than answered twice and less precisely. *)
let held_outside held decls =
  if held_nothing held then held
  else
    let own = held_add held_none decls in
    {
      normal_slots = held.normal_slots land lnot own.normal_slots;
      important_slots = held.important_slots land lnot own.important_slots;
    }

(* Whether something that reaches the same element holds a slot the run leaves
   out. The composed shorthand resets every such slot, so composing would drop
   that value. An important declaration outranks a non-important shorthand
   whatever the order, so it is only at risk when the run is important too. *)
let tr_reset_hazard idx i ~held ~missing ~important =
  (not (Int.equal missing 0))
  &&
  let outside =
    if important then held.normal_slots lor held.important_slots
    else held.normal_slots
  in
  (not (Int.equal (missing land outside) 0))
  ||
  let rec scan j =
    j >= 0
    &&
    let d = Rule_index.decl_at idx j in
    (important || not (is_important d))
    && not (Int.equal (tr_overwritten_slots d land missing) 0)
    || scan (j - 1)
  in
  scan (i - 1)

let try_compose_transition_at ~held idx i =
  let parts, len = take_run_at idx ~part_of:transition_part_of i in
  if List.length parts < 2 then None
  else
    let raw_decls = List.map fst parts in
    if not (same_importance raw_decls) then None
    else if not (has_transition_property_decl raw_decls) then None
    else
      let important = is_important (List.hd raw_decls) in
      let written =
        List.fold_left
          (fun acc (_, (slot, _)) -> acc lor tr_slot_bit slot)
          0 parts
      in
      let missing = all_tr_bits land lnot written in
      if tr_reset_hazard idx i ~held ~missing ~important then None
      else
        let layer =
          List.fold_left (fun acc (_, (_, f)) -> f acc) empty_tr_shorthand parts
        in
        let shorthand =
          Declaration.v ~important Transition
            [ (Shorthand layer : Properties.transition) ]
        in
        Some (shorthand, len)

let compose_transition_via_index ~held idx =
  compose_run_via_index idx ~try_compose:(try_compose_transition_at ~held)

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
type an_updater = declaration -> (an_slot * an_part) option

let lift_an_part :
    'a.
    an_slot ->
    (declaration -> 'a option) ->
    (Properties.animation_shorthand -> 'a -> Properties.animation_shorthand) ->
    an_updater =
 fun slot extract set d ->
  match extract d with Some v -> Some (slot, fun s -> set s v) | None -> None

let an_updaters : an_updater list =
  [
    lift_an_part Name an_name_part (fun s v -> { s with name = Some v });
    lift_an_part Duration an_duration_part (fun s v ->
        { s with duration = Some v });
    lift_an_part Timing an_timing_part (fun s v ->
        { s with timing_function = Some v });
    lift_an_part Delay an_delay_part (fun s v -> { s with delay = Some v });
    lift_an_part Iteration an_iteration_part (fun s v ->
        { s with iteration_count = Some v });
    lift_an_part Direction an_direction_part (fun s v ->
        { s with direction = Some v });
    lift_an_part Fill an_fill_mode_part (fun s v ->
        { s with fill_mode = Some v });
    lift_an_part Play an_play_state_part (fun s v ->
        { s with play_state = Some v });
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

(* The same reset hazard [tr_reset_hazard] answers for transition: a slot the
   run does not write is reset to its initial by the shorthand, so composing is
   only safe when nothing else in the cascade holds that slot. *)
let an_reset_hazard idx i ~held ~missing ~important =
  (not (Int.equal missing 0))
  &&
  let outside =
    if important then held.normal_slots lor held.important_slots
    else held.normal_slots
  in
  (not (Int.equal (missing land outside) 0))
  ||
  let rec scan j =
    j >= 0
    &&
    let d = Rule_index.decl_at idx j in
    (important || not (is_important d))
    && not (Int.equal (an_overwritten_slots d land missing) 0)
    || scan (j - 1)
  in
  scan (i - 1)

let try_compose_animation_at ~held idx i =
  let parts, len = take_run_at idx ~part_of:animation_part_of i in
  if List.length parts < 2 then None
  else
    let raw_decls = List.map fst parts in
    if not (same_importance raw_decls) then None
    else
      let important = is_important (List.hd raw_decls) in
      let written =
        List.fold_left
          (fun acc (_, (slot, _)) -> acc lor an_slot_bit slot)
          0 parts
      in
      let missing = all_an_bits land lnot written in
      if an_reset_hazard idx i ~held ~missing ~important then None
      else
        let layer =
          List.fold_left (fun acc (_, (_, f)) -> f acc) empty_an_shorthand parts
        in
        let shorthand =
          Declaration.v ~important Animation
            [ (Shorthand layer : Properties.animation) ]
        in
        Some (shorthand, len)

let compose_animation_via_index ~held idx =
  compose_run_via_index idx ~try_compose:(try_compose_animation_at ~held)

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

let property_covered_by_important kept decl =
  List.exists
    (fun (_, existing) ->
      (not (is_intentionally_duplicated existing))
      && is_important existing
      && declaration_covers existing decl)
    kept

let same_property = Declaration.same_property

(* Rebuilt through the smart constructors: they recompute the cached
   [Declaration.hash] over the new payload, and a record update would leave it
   fingerprinting the importance that was just cleared. *)
let rec without_importance = function
  | Declaration { property; value; _ } ->
      Declaration.v ~important:false property value
  | Theme_guarded { var_name; decl; _ } ->
      Declaration.theme_guarded ~var_name (without_importance decl)

(* Value equality ignoring importance. Every caller establishes [same_property]
   first, so the two declarations share a value type and this is a structural
   value comparison: on canonical ASTs it matches minified-text equality without
   rendering. *)
let same_value a b =
  Declaration.equal_declaration (without_importance a) (without_importance b)

(* Only a pair that writes a common cascade slot at the same importance with
   different values constrains its own order: [!important] beats the plain
   declaration wherever the two sit, and an identical pair is its own winner
   either way. The footprint is computed once per declaration rather than once
   per pair, since the test below is quadratic. *)
type commute_fact = {
  decl : declaration;
  important : bool;
  keys : overlap_key list;
}

let commute_fact decl =
  {
    decl;
    important = Declaration.is_important decl;
    keys = declaration_overlap_keys decl;
  }

let commute_facts_conflict a b =
  a.important = b.important
  && (not (Declaration.same_minified a.decl b.decl))
  && declarations_overlap_with_keys a.decl a.keys b.decl b.keys

let declarations_commute left right =
  let left = List.map commute_fact left in
  let right = List.map commute_fact right in
  not
    (List.exists
       (fun a -> List.exists (fun b -> commute_facts_conflict a b) right)
       left)

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

(* [vendor_alias_twin vendor twin] is [true] when [vendor] is the
   vendor-prefixed spelling of its unprefixed [twin], carrying the same value
   and importance. Each pair is matched on both property constructors so the two
   values share a type and compare with a typed (=) - no rendering. This
   relation records only which constructors alias each other; whether the prefix
   is then dead is a Baseline question, answered in [drop_vendor_aliases].

   Absent on purpose:
   - [-webkit-appearance]: its value type [webkit_appearance] is a superset of
     [appearance] (extra non-standard values like [listbox]/[checkbox]/[radio]),
     so there is no typed equality between the two.
   - [-webkit-text-decoration]: dropping the WebKit copy regresses documented
     inheritance quirks.
   - [-webkit-background-clip]: its prefix is value-dependent, not
     property-dependent. web-features tracks [background-clip-text] as its own
     feature, so [background-clip] reads widely available at property
     granularity while [-webkit-background-clip: text] is still the spelling
     WebKit honours. Baseline cannot see that at property granularity, so the
     pair stays out of the relation. *)
(* WebKit animation longhand vendor-alias pairs ([-webkit-] vs unprefixed). *)
let vendor_alias_twin_webkit_animation vendor twin =
  match (vendor, twin) with
  | ( Declaration { property = Webkit_animation; value = v1; important = i1; _ },
      Declaration { property = Animation; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_animation_delay; value = v1; important = i1; _ },
      Declaration { property = Animation_delay; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_animation_duration; value = v1; important = i1; _ },
      Declaration
        { property = Animation_duration; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_animation_direction; value = v1; important = i1; _ },
      Declaration
        { property = Animation_direction; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        {
          property = Webkit_animation_iteration_count;
          value = v1;
          important = i1;
          _;
        },
      Declaration
        { property = Animation_iteration_count; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_animation_name; value = v1; important = i1; _ },
      Declaration { property = Animation_name; value = v2; important = i2; _ } )
    ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        {
          property = Webkit_animation_timing_function;
          value = v1;
          important = i1;
          _;
        },
      Declaration
        { property = Animation_timing_function; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_animation_fill_mode; value = v1; important = i1; _ },
      Declaration
        { property = Animation_fill_mode; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        {
          property = Webkit_animation_play_state;
          value = v1;
          important = i1;
          _;
        },
      Declaration
        { property = Animation_play_state; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | _ -> false

(* Mozilla animation longhand vendor-alias pairs ([-moz-] vs unprefixed). *)
let vendor_alias_twin_moz_animation vendor twin =
  match (vendor, twin) with
  | ( Declaration { property = Moz_animation; value = v1; important = i1; _ },
      Declaration { property = Animation; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Moz_animation_delay; value = v1; important = i1; _ },
      Declaration { property = Animation_delay; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Moz_animation_duration; value = v1; important = i1; _ },
      Declaration
        { property = Animation_duration; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Moz_animation_direction; value = v1; important = i1; _ },
      Declaration
        { property = Animation_direction; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        {
          property = Moz_animation_iteration_count;
          value = v1;
          important = i1;
          _;
        },
      Declaration
        { property = Animation_iteration_count; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Moz_animation_name; value = v1; important = i1; _ },
      Declaration { property = Animation_name; value = v2; important = i2; _ } )
    ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        {
          property = Moz_animation_timing_function;
          value = v1;
          important = i1;
          _;
        },
      Declaration
        { property = Animation_timing_function; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Moz_animation_fill_mode; value = v1; important = i1; _ },
      Declaration
        { property = Animation_fill_mode; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Moz_animation_play_state; value = v1; important = i1; _ },
      Declaration
        { property = Animation_play_state; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | _ -> false

let vendor_alias_twin_animation vendor twin =
  vendor_alias_twin_webkit_animation vendor twin
  || vendor_alias_twin_moz_animation vendor twin

(* Transition longhand vendor-alias pairs ([-webkit-]/[-moz-]/[-o-] vs
   unprefixed). *)
let vendor_alias_twin_transition vendor twin =
  match (vendor, twin) with
  | ( Declaration { property = Webkit_transition; value = v1; important = i1; _ },
      Declaration { property = Transition; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_transition_delay; value = v1; important = i1; _ },
      Declaration { property = Transition_delay; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_transition_duration; value = v1; important = i1; _ },
      Declaration
        { property = Transition_duration; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_transition_property; value = v1; important = i1; _ },
      Declaration
        { property = Transition_property; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        {
          property = Webkit_transition_timing_function;
          value = v1;
          important = i1;
          _;
        },
      Declaration
        { property = Transition_timing_function; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Moz_transition; value = v1; important = i1; _ },
      Declaration { property = Transition; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Moz_transition_delay; value = v1; important = i1; _ },
      Declaration { property = Transition_delay; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Moz_transition_duration; value = v1; important = i1; _ },
      Declaration
        { property = Transition_duration; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Moz_transition_property; value = v1; important = i1; _ },
      Declaration
        { property = Transition_property; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        {
          property = Moz_transition_timing_function;
          value = v1;
          important = i1;
          _;
        },
      Declaration
        { property = Transition_timing_function; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = O_transition; value = v1; important = i1; _ },
      Declaration { property = Transition; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | _ -> false

(* Modern flexbox alignment vendor-alias pairs (-webkit- vs unprefixed). *)
let vendor_alias_twin_flex vendor twin =
  match (vendor, twin) with
  | ( Declaration
        { property = Webkit_flex_direction; value = v1; important = i1; _ },
      Declaration { property = Flex_direction; value = v2; important = i2; _ } )
    ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Webkit_flex_wrap; value = v1; important = i1; _ },
      Declaration { property = Flex_wrap; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Webkit_flex_flow; value = v1; important = i1; _ },
      Declaration { property = Flex_flow; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_justify_content; value = v1; important = i1; _ },
      Declaration { property = Justify_content; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_align_items; value = v1; important = i1; _ },
      Declaration { property = Align_items; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_align_content; value = v1; important = i1; _ },
      Declaration { property = Align_content; value = v2; important = i2; _ } )
    ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Webkit_align_self; value = v1; important = i1; _ },
      Declaration { property = Align_self; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | _ -> false

(* Border-radius / box-shadow / background-size / filter vendor-alias pairs. *)
let vendor_alias_twin_visual vendor twin =
  match (vendor, twin) with
  | ( Declaration
        { property = Webkit_border_radius; value = v1; important = i1; _ },
      Declaration { property = Border_radius; value = v2; important = i2; _ } )
    ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Moz_border_radius; value = v1; important = i1; _ },
      Declaration { property = Border_radius; value = v2; important = i2; _ } )
    ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Webkit_box_shadow; value = v1; important = i1; _ },
      Declaration { property = Box_shadow; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Moz_box_shadow; value = v1; important = i1; _ },
      Declaration { property = Box_shadow; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_background_size; value = v1; important = i1; _ },
      Declaration { property = Background_size; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Webkit_filter; value = v1; important = i1; _ },
      Declaration { property = Filter; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_backdrop_filter; value = v1; important = i1; _ },
      Declaration { property = Backdrop_filter; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | _ -> false

let decoration_color_alias_twin vendor twin =
  match (vendor, twin) with
  | ( Declaration
        {
          property = Webkit_text_decoration_color;
          value = v1;
          important = i1;
          _;
        },
      Declaration
        { property = Text_decoration_color; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | _ -> false

let vendor_alias_twin vendor twin =
  match (vendor, twin) with
  | ( Declaration { property = Webkit_transform; value = v1; important = i1; _ },
      Declaration { property = Transform; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Webkit_box_sizing; value = v1; important = i1; _ },
      Declaration { property = Box_sizing; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Moz_box_sizing; value = v1; important = i1; _ },
      Declaration { property = Box_sizing; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Webkit_text_decoration_color; _ },
      Declaration { property = Text_decoration_color; _ } ) ->
      decoration_color_alias_twin vendor twin
  | ( Declaration { property = Webkit_mask_image; value = v1; important = i1; _ },
      Declaration { property = Mask_image; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_user_select; value = v1; important = i1; _ },
      Declaration { property = User_select; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Moz_user_select; value = v1; important = i1; _ },
      Declaration { property = User_select; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration { property = Webkit_hyphens; value = v1; important = i1; _ },
      Declaration { property = Hyphens; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_text_size_adjust; value = v1; important = i1; _ },
      Declaration { property = Text_size_adjust; value = v2; important = i2; _ }
    ) ->
      v1 = v2 && Bool.equal i1 i2
  | ( Declaration
        { property = Webkit_print_color_adjust; value = v1; important = i1; _ },
      Declaration
        { property = Print_color_adjust; value = v2; important = i2; _ } ) ->
      v1 = v2 && Bool.equal i1 i2
  | _ ->
      vendor_alias_twin_animation vendor twin
      || vendor_alias_twin_transition vendor twin
      || vendor_alias_twin_flex vendor twin
      || vendor_alias_twin_visual vendor twin

(* Canonical comparison follows Cascade's configured normalization for this
   typed compatibility alias without enabling every target-dependent optimizer
   rewrite. A differing fallback, mismatched importance, or prefix without a
   standard twin remains observable and is kept. *)
let drop_redundant_decoration_color_aliases declarations =
  filter_preserve
    (fun declaration ->
      not (List.exists (decoration_color_alias_twin declaration) declarations))
    declarations

(* If [name] starts with a CSS vendor prefix ([-webkit-] / [-moz-] / [-ms-] /
   [-o-]) return the unprefixed remainder; otherwise [None]. *)
let strip_vendor_prefix name =
  let n = String.length name in
  let try_prefix p =
    let pn = String.length p in
    if n > pn && String.sub name 0 pn = p then
      Some (String.sub name pn (n - pn))
    else None
  in
  match try_prefix "-webkit-" with
  | Some _ as r -> r
  | None -> (
      match try_prefix "-moz-" with
      | Some _ as r -> r
      | None -> (
          match try_prefix "-ms-" with
          | Some _ as r -> r
          | None -> try_prefix "-o-"))

(* Structural vendor-alias drop for [Unknown_property] pairs. Both [vendor] and
   [twin] are [Unknown_property] declarations; we drop [vendor] when stripping
   its vendor prefix yields [twin]'s name and the component-value lists are
   structurally equal under matching importance. The typed-twin case (e.g.
   vendor [Unknown_property "-webkit-animation-delay"] + modern
   [Animation_delay]) needs typed vendor longhand properties to compare
   structurally; that remains a follow-up. *)
let text_vendor_alias_twin vendor twin =
  match (vendor, twin) with
  | ( Declaration
        { property = Unknown_property vname; value = vv; important = vi; _ },
      Declaration
        { property = Unknown_property tname; value = tv; important = ti; _ } )
    when Bool.equal vi ti -> (
      match strip_vendor_prefix vname with
      | Some modern -> String.equal tname modern && vv = tv
      | None -> false)
  | _ -> false

(* {!Baseline.greenfield_properties} is generated from web-features as names.
   Resolve each once into the property it spells, so the test below compares
   property tags instead of rendering one. A name the reader does not type keeps
   its [Unknown_property] tag, which is the shape it reaches
   [drop_vendor_aliases] in as a [text_vendor_alias_twin] twin. *)
let greenfield_keys =
  let tbl = Hashtbl.create 512 in
  let add key = Hashtbl.replace tbl key () in
  List.iter
    (fun name ->
      add (Key (Properties.Unknown_property name));
      match property_of_name name with
      | Some (Properties.Prop property) -> add (Key property)
      | None -> ())
    Baseline.greenfield_properties;
  tbl

(* An unprefixed property supersedes its prefix only once every maintained
   browser reads it. {!Baseline.greenfield_properties} lists the properties that
   are not yet Baseline "widely available", which is exactly the set whose
   prefix is still load-bearing: Safari reads only [-webkit-backdrop-filter] up
   to 17.6, and [-webkit-text-size-adjust] has no unprefixed Safari support at
   all. *)
let unprefixed_is_widely_available twin =
  let key =
    match Declaration.property_key twin with
    (* CSS Syntax 3 (ED) sec. 8.1: property names are case-insensitive. *)
    | Key (Unknown_property name) ->
        Key (Properties.Unknown_property (String.lowercase_ascii_preserve name))
    | key -> key
  in
  not (Hashtbl.mem greenfield_keys key)

(* Drop a vendor-prefixed declaration when its unprefixed sibling appears in the
   same rule with the same value and importance and is widely available. The
   name is only rendered once the cheap typed twin match has fired. *)
let drop_vendor_aliases ~ctx (kept : (int * declaration) list) :
    (int * declaration) list =
  if Ctx.enforce_spec ctx then kept (* spec-literal: keep every vendor prefix *)
  else
    let has_dead_prefix (_, decl) =
      List.exists
        (fun (_, other) ->
          (vendor_alias_twin decl other || text_vendor_alias_twin decl other)
          && unprefixed_is_widely_available other)
        kept
    in
    filter_preserve (fun item -> not (has_dead_prefix item)) kept

(* Run every index-based composer against the same Rule_index so we pay one
   [build] + [to_list] per rule for the whole group. *)
let compose_index_group_a ~ctx kept =
  let idx = Rule_index.build (List.map snd kept) in
  compose_box_via_index ~ctx idx;
  compose_pair_via_index ~ctx idx;
  compose_outline_via_index idx;
  List.mapi (fun i d -> (i, d)) (Rule_index.to_list idx)

(* Second index group runs after the font-reset reorder. Font + list-style +
   flex + text-decoration + border all share the same index. *)
let compose_index_group_b kept =
  let idx = Rule_index.build (List.map snd kept) in
  compose_font_via_index idx;
  compose_list_style_via_index idx;
  compose_flex_via_index idx;
  compose_text_decoration_via_index idx;
  compose_border_via_index idx;
  compose_line_via_index idx;
  List.mapi (fun i d -> (i, d)) (Rule_index.to_list idx)

(* Third index group runs at the very end: mask + transition + animation share
   one index. *)
let compose_index_group_c ~ctx ~held kept =
  let idx = Rule_index.build (List.map snd kept) in
  compose_mask_via_index ~ctx idx;
  compose_transition_via_index ~held idx;
  compose_animation_via_index ~held idx;
  List.mapi (fun i d -> (i, d)) (Rule_index.to_list idx)

let compose_border_whole_step ~ctx kept =
  let idx = Rule_index.build (List.map snd kept) in
  compose_border_whole_via_index ~ctx idx;
  List.mapi (fun i d -> (i, d)) (Rule_index.to_list idx)

let compose_shorthands ?(held = held_none) ~ctx kept =
  kept |> compose_index_group_a ~ctx |> reorder_font_resets_before_font
  |> compose_index_group_b |> reorder_border_image_before_border
  |> compose_border_whole_step ~ctx
  |> drop_bimg_shadowed_by_border
  |> compose_border_image_shorthand ~ctx
  |> compose_background_shorthand ~ctx
  |> reorder_mask_border_before_mask
  |> compose_index_group_c ~ctx ~held
  |> fun kept ->
  merge_box_shorthand_longhands kept kept |> merge_overflow_longhands

(* The longhand declaration a shorthand assigns to a slot it covers: the slot
   value if set, else that longhand's initial. A later longhand equal to this is
   a redundant no-op. [None] when the shorthand does not cover the longhand or
   the value cannot be proven. Extend per shorthand; background first. *)
let implied_longhand covering covered : Declaration.declaration option =
  match unwrap_theme_guard covering with
  | Declaration { property = Properties.Background; value; _ } -> (
      (* Single-layer only; a multi-layer background assigns per-layer values a
         single longhand rarely matches, so leave those conservative. *)
      match (value : Properties.background list) with
      | [ Properties.Shorthand s ] -> (
          match unwrap_theme_guard covered with
          | Declaration { property = Properties.Background_size; _ } ->
              Some
                (background_size (Option.value s.size ~default:Properties.Auto))
          | Declaration { property = Properties.Background_repeat; _ } ->
              Some
                (background_repeat
                   (Option.value s.repeat ~default:Properties.Repeat))
          | Declaration { property = Properties.Background_attachment; _ } ->
              Some
                (background_attachment
                   (Option.value s.attachment ~default:Properties.Scroll))
          | _ -> None)
      | _ -> None)
  | Declaration { property = Properties.Flex; value; _ } -> (
      (* [flex] always sets grow/shrink/basis; expand the keyword and numeric
         forms. flex-basis (the [0%] default) is left for a follow-up. *)
      let grow_shrink =
        match (value : Properties.flex) with
        | Initial -> Some (0., 1.)
        | Auto -> Some (1., 1.)
        | None -> Some (0., 0.)
        | Grow (Number g) -> Some (g, 1.)
        | Basis _ -> Some (1., 1.)
        | Grow_shrink (Number g, Number s) -> Some (g, s)
        | Full (Number g, Number s, _) -> Some (g, s)
        | _ -> Option.none
      in
      match (unwrap_theme_guard covered, grow_shrink) with
      | Declaration { property = Properties.Flex_grow; _ }, Some (g, _) ->
          Some (flex_grow g)
      | Declaration { property = Properties.Flex_shrink; _ }, Some (_, s) ->
          Some (flex_shrink s)
      | _ -> None)
  | Declaration { property = Properties.Transition; value; _ } -> (
      (* Single transition only; a comma list assigns per-item values. *)
      match (value : Properties.transition list) with
      | [ Properties.Shorthand s ] -> (
          match unwrap_theme_guard covered with
          | Declaration { property = Properties.Transition_duration; _ } ->
              Some
                (transition_duration
                   (Option.value s.duration ~default:(Values.S 0.)))
          | Declaration { property = Properties.Transition_delay; _ } ->
              Some
                (transition_delay (Option.value s.delay ~default:(Values.S 0.)))
          | Declaration { property = Properties.Transition_timing_function; _ }
            ->
              Some
                (transition_timing_function
                   (Option.value s.timing_function ~default:Properties.Ease))
          | Declaration { property = Properties.Transition_behavior; _ } ->
              Some
                (transition_behavior
                   (Option.value s.behavior ~default:Properties.Normal))
          | _ -> None)
      | _ -> None)
  | Declaration { property = Properties.Border; value; _ } -> (
      (* Only the slots [border] sets explicitly; its initials (medium / none /
         currentcolor) are rarely written back, so leave those. *)
      match (value : Properties.border) with
      | Properties.Shorthand s -> (
          match unwrap_theme_guard covered with
          | Declaration { property = Properties.Border_color; _ } ->
              Option.map border_color s.color
          | Declaration { property = Properties.Border_width; _ } ->
              Option.map border_width s.width
          | Declaration { property = Properties.Border_style; _ } ->
              Option.map border_style s.style
          | _ -> None)
      | _ -> None)
  | Declaration { property = Properties.Font; value; _ } -> (
      (* [font] resets style/weight/stretch/line-height to [normal] unless set;
         font-variant uses a narrower type in the shorthand, so leave it. *)
      match (value : Properties.font) with
      | Properties.Shorthand s -> (
          match unwrap_theme_guard covered with
          | Declaration { property = Properties.Font_style; _ } ->
              Some (font_style (Option.value s.style ~default:Normal))
          | Declaration { property = Properties.Font_weight; _ } ->
              Some (font_weight (Option.value s.weight ~default:Normal))
          | Declaration { property = Properties.Font_stretch; _ } ->
              Some (font_stretch (Option.value s.stretch ~default:Normal))
          | Declaration { property = Properties.Line_height; _ } ->
              Some (line_height (Option.value s.line_height ~default:Normal))
          | _ -> None)
      | _ -> None)
  | _ -> None

(* CSS Cascade: a shorthand sets every longhand it covers (to its slot value or
   the initial), so a later longhand in the same rule that writes the same value
   at the same importance is redundant and dropped. Guard on equal value, so
   [background:red;background-size:cover] keeps the override. *)
let drop_longhands_after_covering_shorthand props =
  let arr = Array.of_list props in
  let dropped = Array.make (Array.length arr) false in
  let redundant_via j li =
    (not (Declaration.same_property arr.(j) li))
    && Bool.equal (is_important arr.(j)) (is_important li)
    &&
    match implied_longhand arr.(j) li with
    | Some implied ->
        (* [implied] is built here from the shorthand's slots and the initials
           it resets the rest to, so it has not been through the normalisation
           every declaration in [props] already carries. Canonicalise it before
           comparing, or a slot filled by an initial reads as different from the
           same value written out. *)
        String.equal
          (string_of_value ~minify:true (Declaration.normalize implied))
          (string_of_value ~minify:true li)
    | None -> false
  in
  Array.iteri
    (fun i li ->
      let rec nearest j =
        if j < 0 then ()
        else if dropped.(j) then nearest (j - 1)
        else if declaration_covers arr.(j) li then
          if redundant_via j li then dropped.(i) <- true else ()
        else nearest (j - 1)
      in
      nearest (i - 1))
    arr;
  let kept = List.filteri (fun i _ -> not dropped.(i)) props in
  preserve_list props kept

let deduplicate_declarations_with ?(held = held_none) ~ctx ?(merge_box = true)
    props =
  let held = held_outside held props in
  let props = drop_longhands_after_covering_shorthand props in
  let indexed_props = List.mapi (fun i decl -> (i, decl)) props in
  let kept = List.rev (List.fold_left deduplicate_step [] indexed_props) in
  let kept =
    let kept = if merge_box then compose_shorthands ~held ~ctx kept else kept in
    let kept = drop_vendor_aliases ~ctx kept in
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
