open Declaration
open Common

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
  | Background, Background_position_x -> true
  | Background, Background_position_y -> true
  | Background_position, Background_position_x -> true
  | Background_position, Background_position_y -> true
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
     [animation_keys] names, [animation-timeline] included. Scroll-driven
     Animations 1 appendix A.3 makes the two [animation-range-*] longhands
     reset-only sub-properties of it, so they are reset as well.
     [animation-composition] is no part of it and is absent for that reason. *)
  | Animation, Animation_name -> true
  | Animation, Animation_duration -> true
  | Animation, Animation_timing_function -> true
  | Animation, Animation_delay -> true
  | Animation, Animation_iteration_count -> true
  | Animation, Animation_direction -> true
  | Animation, Animation_fill_mode -> true
  | Animation, Animation_play_state -> true
  | Animation, Animation_timeline -> true
  | Animation, Animation_range -> true
  | Animation, Animation_range_start -> true
  | Animation, Animation_range_end -> true
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
  (* CSS Fonts 4 sec. 2.8.5: [font-synthesis] resets all four synthesis
     longhands, the position one included. *)
  | Font_synthesis, Font_synthesis_weight -> true
  | Font_synthesis, Font_synthesis_style -> true
  | Font_synthesis, Font_synthesis_small_caps -> true
  | Font_synthesis, Font_synthesis_position -> true
  | Webkit_text_stroke, Webkit_text_stroke_width -> true
  | Webkit_text_stroke, Webkit_text_stroke_color -> true
  (* CSS Multicol 1 sec. 4.3: [column-rule] is the three rule longhands. *)
  | Columns, Column_width -> true
  | Columns, Column_count -> true
  | Columns, Column_height -> true
  | Columns, Column_wrap -> true
  | Column_rule, Column_rule_width -> true
  | Column_rule, Column_rule_style -> true
  | Column_rule, Column_rule_color -> true
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
  | Font, Font_variant_alternates -> true
  (* CSS Fonts 4 sec. 6.10: [font-variant] resets its seven longhands. *)
  | Font_variant, Font_variant_ligatures -> true
  | Font_variant, Font_variant_alternates -> true
  | Font_variant, Caps -> true
  | Font_variant, Numeric -> true
  | Font_variant, East_asian -> true
  | Font_variant, Font_variant_position -> true
  | Font_variant, Font_variant_emoji -> true
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
    key "animation-range-start";
    key "animation-range-end";
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
        key "background-position-x";
        key "background-position-y";
        key "background-repeat";
        key "background-size";
      ]
  | Background_attachment -> [ key "background-attachment" ]
  | Background_blend_mode -> [ key "background-blend-mode" ]
  | Background_clip | Webkit_background_clip -> [ key "background-clip" ]
  | Background_color -> [ key "background-color" ]
  | Background_image -> [ key "background-image" ]
  | Background_origin -> [ key "background-origin" ]
  | Background_position ->
      [ key "background-position-x"; key "background-position-y" ]
  | Background_position_x -> [ key "background-position-x" ]
  | Background_position_y -> [ key "background-position-y" ]
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
  (* Scroll-driven Animations 1 appendix A.3: [animation-range] is its own
     shorthand over the two ends. *)
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
  | Mask_position | Webkit_mask_position ->
      [ key "-webkit-mask-position-x"; key "-webkit-mask-position-y" ]
  | Webkit_mask_position_x -> [ key "-webkit-mask-position-x" ]
  | Webkit_mask_position_y -> [ key "-webkit-mask-position-y" ]
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
        key "font-variant-alternates";
      ]
  (* CSS Fonts 4 sec. 6.10. *)
  | Font_variant ->
      [
        key "font-variant-ligatures";
        key "font-variant-alternates";
        key "font-variant-caps";
        key "font-variant-numeric";
        key "font-variant-east-asian";
        key "font-variant-position";
        key "font-variant-emoji";
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
  | Font_variant_alternates -> [ key "font-variant-alternates" ]
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
  (* CSS Multicol 2 sec. 4.5: [columns] sets the width, the count and the
     height, and Chrome 146 has it reset [column-wrap] as well. *)
  | Columns ->
      [
        key "column-width";
        key "column-count";
        key "column-height";
        key "column-wrap";
      ]
  | Column_width -> [ key "column-width" ]
  | Column_count -> [ key "column-count" ]
  | Column_height -> [ key "column-height" ]
  | Column_wrap -> [ key "column-wrap" ]
  (* CSS Multicol 1 sec. 4.4: [column-rule] resets the rule width, style and
     colour. Naming those three is what keeps [column-rule-color] from reading
     as a slot of its own that [column-rule] never touches. *)
  | Column_rule ->
      [
        key "column-rule-width";
        key "column-rule-style";
        key "column-rule-color";
      ]
  | Column_rule_width -> [ key "column-rule-width" ]
  | Column_rule_style -> [ key "column-rule-style" ]
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
  (* [-webkit-text-stroke] is [<line-width> || <color>] over its two
     longhands. *)
  | Webkit_text_stroke ->
      [ key "-webkit-text-stroke-width"; key "-webkit-text-stroke-color" ]
  | Webkit_text_stroke_width -> [ key "-webkit-text-stroke-width" ]
  | Webkit_text_stroke_color -> [ key "-webkit-text-stroke-color" ]
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
  | White_space_collapse -> [ key "white-space-collapse" ]
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
