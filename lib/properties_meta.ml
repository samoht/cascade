(* Property names and per-property metadata: inheritance, spec-invalid values
   and the value kind a property carries. *)

open Common
open Values
open Properties_intf
open Prop_multicol

let pp_property : type a. a property Pp.t =
 fun ctx -> function
  (* CSS Syntax 3 (ED) sec. 4.3.7 lets an escape carry a [;], a [}] or any other
     non-name code point into a custom-property name, so the name is written
     back with the escapes that read it as the same name. Printed raw it ends
     its own declaration or closes the rule around it. *)
  | Custom_property name ->
      (* The payload carries the [--]; the rename sees the bare name, as it does
         at a [var()], so one function answers for both sites. *)
      let bare = String.sub name 2 (String.length name - 2) in
      Pp.string ctx
        (Parser.escape_ident
           (String.concat "" [ "--"; Pp.rename_custom_property ctx bare ]))
  | Unknown_property name -> Pp.string ctx name
  | All -> Pp.string ctx "all"
  | Background_color -> Pp.string ctx "background-color"
  | Color -> Pp.string ctx "color"
  | Border_color -> Pp.string ctx "border-color"
  | Border_style -> Pp.string ctx "border-style"
  | Border_top_style -> Pp.string ctx "border-top-style"
  | Border_right_style -> Pp.string ctx "border-right-style"
  | Border_bottom_style -> Pp.string ctx "border-bottom-style"
  | Border_left_style -> Pp.string ctx "border-left-style"
  | Border_inline_start_style -> Pp.string ctx "border-inline-start-style"
  | Border_inline_end_style -> Pp.string ctx "border-inline-end-style"
  | Border_block_start_style -> Pp.string ctx "border-block-start-style"
  | Border_block_end_style -> Pp.string ctx "border-block-end-style"
  | Padding -> Pp.string ctx "padding"
  | Padding_left -> Pp.string ctx "padding-left"
  | Padding_right -> Pp.string ctx "padding-right"
  | Padding_bottom -> Pp.string ctx "padding-bottom"
  | Padding_top -> Pp.string ctx "padding-top"
  | Padding_inline -> Pp.string ctx "padding-inline"
  | Padding_inline_start -> Pp.string ctx "padding-inline-start"
  | Padding_inline_end -> Pp.string ctx "padding-inline-end"
  | Padding_block -> Pp.string ctx "padding-block"
  | Padding_block_start -> Pp.string ctx "padding-block-start"
  | Padding_block_end -> Pp.string ctx "padding-block-end"
  | Margin -> Pp.string ctx "margin"
  | Margin_inline_end -> Pp.string ctx "margin-inline-end"
  | Margin_inline_start -> Pp.string ctx "margin-inline-start"
  | Margin_left -> Pp.string ctx "margin-left"
  | Margin_right -> Pp.string ctx "margin-right"
  | Margin_top -> Pp.string ctx "margin-top"
  | Margin_bottom -> Pp.string ctx "margin-bottom"
  | Margin_inline -> Pp.string ctx "margin-inline"
  | Margin_block -> Pp.string ctx "margin-block"
  | Margin_block_start -> Pp.string ctx "margin-block-start"
  | Margin_block_end -> Pp.string ctx "margin-block-end"
  | Gap -> Pp.string ctx "gap"
  | Column_gap -> Pp.string ctx "column-gap"
  | Row_gap -> Pp.string ctx "row-gap"
  | Width -> Pp.string ctx "width"
  | Height -> Pp.string ctx "height"
  | Min_width -> Pp.string ctx "min-width"
  | Min_height -> Pp.string ctx "min-height"
  | Max_width -> Pp.string ctx "max-width"
  | Max_height -> Pp.string ctx "max-height"
  | Inline_size -> Pp.string ctx "inline-size"
  | Min_inline_size -> Pp.string ctx "min-inline-size"
  | Max_inline_size -> Pp.string ctx "max-inline-size"
  | Block_size -> Pp.string ctx "block-size"
  | Min_block_size -> Pp.string ctx "min-block-size"
  | Max_block_size -> Pp.string ctx "max-block-size"
  | Font_size -> Pp.string ctx "font-size"
  | Line_height -> Pp.string ctx "line-height"
  | Font_weight -> Pp.string ctx "font-weight"
  | Font_style -> Pp.string ctx "font-style"
  | Text_align -> Pp.string ctx "text-align"
  | Text_decoration -> Pp.string ctx "text-decoration"
  | Text_decoration_line -> Pp.string ctx "text-decoration-line"
  | Text_decoration_style -> Pp.string ctx "text-decoration-style"
  | Text_decoration_color -> Pp.string ctx "text-decoration-color"
  | Text_decoration_thickness -> Pp.string ctx "text-decoration-thickness"
  | Text_underline_offset -> Pp.string ctx "text-underline-offset"
  | Text_decoration_skip -> Pp.string ctx "text-decoration-skip"
  | Text_decoration_skip_self -> Pp.string ctx "text-decoration-skip-self"
  | Text_decoration_skip_box -> Pp.string ctx "text-decoration-skip-box"
  | Text_decoration_skip_inset -> Pp.string ctx "text-decoration-skip-inset"
  | Text_decoration_skip_spaces -> Pp.string ctx "text-decoration-skip-spaces"
  | Text_emphasis -> Pp.string ctx "text-emphasis"
  | Text_emphasis_style -> Pp.string ctx "text-emphasis-style"
  | Text_emphasis_color -> Pp.string ctx "text-emphasis-color"
  | Text_emphasis_position -> Pp.string ctx "text-emphasis-position"
  | Text_emphasis_skip -> Pp.string ctx "text-emphasis-skip"
  | Text_orientation -> Pp.string ctx "text-orientation"
  | Text_transform -> Pp.string ctx "text-transform"
  | Letter_spacing -> Pp.string ctx "letter-spacing"
  | List_style_type -> Pp.string ctx "list-style-type"
  | List_style_position -> Pp.string ctx "list-style-position"
  | List_style_image -> Pp.string ctx "list-style-image"
  | Display -> Pp.string ctx "display"
  | Position -> Pp.string ctx "position"
  | Visibility -> Pp.string ctx "visibility"
  | Baseline_source -> Pp.string ctx "baseline-source"
  | Alignment_baseline -> Pp.string ctx "alignment-baseline"
  | Baseline_shift -> Pp.string ctx "baseline-shift"
  | Flex_direction -> Pp.string ctx "flex-direction"
  | Flex_wrap -> Pp.string ctx "flex-wrap"
  | Flex_flow -> Pp.string ctx "flex-flow"
  | Flex -> Pp.string ctx "flex"
  | Flex_grow -> Pp.string ctx "flex-grow"
  | Flex_shrink -> Pp.string ctx "flex-shrink"
  | Flex_basis -> Pp.string ctx "flex-basis"
  | Order -> Pp.string ctx "order"
  | Align_items -> Pp.string ctx "align-items"
  | Justify_content -> Pp.string ctx "justify-content"
  | Justify_items -> Pp.string ctx "justify-items"
  | Align_content -> Pp.string ctx "align-content"
  | Align_self -> Pp.string ctx "align-self"
  | Justify_self -> Pp.string ctx "justify-self"
  | Place_content -> Pp.string ctx "place-content"
  | Place_items -> Pp.string ctx "place-items"
  | Place_self -> Pp.string ctx "place-self"
  | Grid_template_columns -> Pp.string ctx "grid-template-columns"
  | Grid_template_rows -> Pp.string ctx "grid-template-rows"
  | Grid_template_areas -> Pp.string ctx "grid-template-areas"
  | Grid_template -> Pp.string ctx "grid-template"
  | Grid -> Pp.string ctx "grid"
  | Grid_area -> Pp.string ctx "grid-area"
  | Grid_auto_flow -> Pp.string ctx "grid-auto-flow"
  | Grid_auto_columns -> Pp.string ctx "grid-auto-columns"
  | Grid_auto_rows -> Pp.string ctx "grid-auto-rows"
  | Grid_column -> Pp.string ctx "grid-column"
  | Grid_row -> Pp.string ctx "grid-row"
  | Grid_column_start -> Pp.string ctx "grid-column-start"
  | Grid_column_end -> Pp.string ctx "grid-column-end"
  | Grid_row_start -> Pp.string ctx "grid-row-start"
  | Grid_row_end -> Pp.string ctx "grid-row-end"
  | Border_width -> Pp.string ctx "border-width"
  | Border_top_width -> Pp.string ctx "border-top-width"
  | Border_right_width -> Pp.string ctx "border-right-width"
  | Border_bottom_width -> Pp.string ctx "border-bottom-width"
  | Border_left_width -> Pp.string ctx "border-left-width"
  | Border_inline_start_width -> Pp.string ctx "border-inline-start-width"
  | Border_inline_end_width -> Pp.string ctx "border-inline-end-width"
  | Border_block_start_width -> Pp.string ctx "border-block-start-width"
  | Border_block_end_width -> Pp.string ctx "border-block-end-width"
  | Border_image -> Pp.string ctx "border-image"
  | Border_radius -> Pp.string ctx "border-radius"
  | Border_top_left_radius -> Pp.string ctx "border-top-left-radius"
  | Border_top_right_radius -> Pp.string ctx "border-top-right-radius"
  | Border_bottom_left_radius -> Pp.string ctx "border-bottom-left-radius"
  | Border_bottom_right_radius -> Pp.string ctx "border-bottom-right-radius"
  | Border_top_color -> Pp.string ctx "border-top-color"
  | Border_right_color -> Pp.string ctx "border-right-color"
  | Border_bottom_color -> Pp.string ctx "border-bottom-color"
  | Border_left_color -> Pp.string ctx "border-left-color"
  | Border_inline_start_color -> Pp.string ctx "border-inline-start-color"
  | Border_inline_end_color -> Pp.string ctx "border-inline-end-color"
  | Border_block_start_color -> Pp.string ctx "border-block-start-color"
  | Border_block_end_color -> Pp.string ctx "border-block-end-color"
  | Border_inline_color -> Pp.string ctx "border-inline-color"
  | Border_block_color -> Pp.string ctx "border-block-color"
  | Border_inline_width -> Pp.string ctx "border-inline-width"
  | Border_block_width -> Pp.string ctx "border-block-width"
  | Border_inline_style -> Pp.string ctx "border-inline-style"
  | Border_block_style -> Pp.string ctx "border-block-style"
  | Border_start_start_radius -> Pp.string ctx "border-start-start-radius"
  | Border_start_end_radius -> Pp.string ctx "border-start-end-radius"
  | Border_end_start_radius -> Pp.string ctx "border-end-start-radius"
  | Border_end_end_radius -> Pp.string ctx "border-end-end-radius"
  | Box_shadow -> Pp.string ctx "box-shadow"
  | Fill -> Pp.string ctx "fill"
  | Stroke -> Pp.string ctx "stroke"
  | Stroke_width -> Pp.string ctx "stroke-width"
  | Fill_rule -> Pp.string ctx "fill-rule"
  | Clip_rule -> Pp.string ctx "clip-rule"
  | Stroke_linecap -> Pp.string ctx "stroke-linecap"
  | Stroke_linejoin -> Pp.string ctx "stroke-linejoin"
  | Stroke_miterlimit -> Pp.string ctx "stroke-miterlimit"
  | Stroke_dashoffset -> Pp.string ctx "stroke-dashoffset"
  | Stroke_dasharray -> Pp.string ctx "stroke-dasharray"
  | Paint_order -> Pp.string ctx "paint-order"
  | Vector_effect -> Pp.string ctx "vector-effect"
  | Stop_color -> Pp.string ctx "stop-color"
  | Flood_color -> Pp.string ctx "flood-color"
  | Lighting_color -> Pp.string ctx "lighting-color"
  | Opacity -> Pp.string ctx "opacity"
  | Fill_opacity -> Pp.string ctx "fill-opacity"
  | Stroke_opacity -> Pp.string ctx "stroke-opacity"
  | Stop_opacity -> Pp.string ctx "stop-opacity"
  | Flood_opacity -> Pp.string ctx "flood-opacity"
  | Mix_blend_mode -> Pp.string ctx "mix-blend-mode"
  | Transition -> Pp.string ctx "transition"
  | Transform -> Pp.string ctx "transform"
  | Translate -> Pp.string ctx "translate"
  | Cursor -> Pp.string ctx "cursor"
  | Interactivity -> Pp.string ctx "interactivity"
  | Caret_animation -> Pp.string ctx "caret-animation"
  | Caret_shape -> Pp.string ctx "caret-shape"
  | Caret -> Pp.string ctx "caret"
  | Interest_delay -> Pp.string ctx "interest-delay"
  | Interest_delay_start -> Pp.string ctx "interest-delay-start"
  | Interest_delay_end -> Pp.string ctx "interest-delay-end"
  | Nav_up -> Pp.string ctx "nav-up"
  | Nav_right -> Pp.string ctx "nav-right"
  | Nav_down -> Pp.string ctx "nav-down"
  | Nav_left -> Pp.string ctx "nav-left"
  | Table_layout -> Pp.string ctx "table-layout"
  | Border_collapse -> Pp.string ctx "border-collapse"
  | Border_spacing -> Pp.string ctx "border-spacing"
  | User_select -> Pp.string ctx "user-select"
  | Pointer_events -> Pp.string ctx "pointer-events"
  | Overflow -> Pp.string ctx "overflow"
  | Inset -> Pp.string ctx "inset"
  | Inset_inline -> Pp.string ctx "inset-inline"
  | Inset_inline_start -> Pp.string ctx "inset-inline-start"
  | Inset_inline_end -> Pp.string ctx "inset-inline-end"
  | Inset_block -> Pp.string ctx "inset-block"
  | Inset_block_start -> Pp.string ctx "inset-block-start"
  | Inset_block_end -> Pp.string ctx "inset-block-end"
  | Top -> Pp.string ctx "top"
  | Right -> Pp.string ctx "right"
  | Bottom -> Pp.string ctx "bottom"
  | Left -> Pp.string ctx "left"
  | Z_index -> Pp.string ctx "z-index"
  | Outline -> Pp.string ctx "outline"
  | Outline_style -> Pp.string ctx "outline-style"
  | Outline_width -> Pp.string ctx "outline-width"
  | Outline_color -> Pp.string ctx "outline-color"
  | Outline_offset -> Pp.string ctx "outline-offset"
  | Forced_color_adjust -> Pp.string ctx "forced-color-adjust"
  | Scroll_snap_type -> Pp.string ctx "scroll-snap-type"
  | Clip -> Pp.string ctx "clip"
  | Clear -> Pp.string ctx "clear"
  | Float -> Pp.string ctx "float"
  | White_space -> Pp.string ctx "white-space"
  | White_space_collapse -> Pp.string ctx "white-space-collapse"
  | Border -> Pp.string ctx "border"
  | Background -> Pp.string ctx "background"
  | Tab_size -> Pp.string ctx "tab-size"
  | Zoom -> Pp.string ctx "zoom"
  | Webkit_text_size_adjust -> Pp.string ctx "-webkit-text-size-adjust"
  | Font_feature_settings -> Pp.string ctx "font-feature-settings"
  | Font_variation_settings -> Pp.string ctx "font-variation-settings"
  | Webkit_tap_highlight_color -> Pp.string ctx "-webkit-tap-highlight-color"
  | Webkit_text_fill_color -> Pp.string ctx "-webkit-text-fill-color"
  | Webkit_text_stroke_color -> Pp.string ctx "-webkit-text-stroke-color"
  | Webkit_text_stroke -> Pp.string ctx "-webkit-text-stroke"
  | Webkit_text_stroke_width -> Pp.string ctx "-webkit-text-stroke-width"
  | Webkit_user_select -> Pp.string ctx "-webkit-user-select"
  | Ms_user_select -> Pp.string ctx "-ms-user-select"
  | Moz_user_select -> Pp.string ctx "-moz-user-select"
  | Webkit_text_decoration -> Pp.string ctx "-webkit-text-decoration"
  | Webkit_text_decoration_color ->
      Pp.string ctx "-webkit-text-decoration-color"
  | Text_indent -> Pp.string ctx "text-indent"
  | List_style -> Pp.string ctx "list-style"
  | Font -> Pp.string ctx "font"
  | Source -> Pp.string ctx "src"
  | Webkit_appearance -> Pp.string ctx "-webkit-appearance"
  | Container_type -> Pp.string ctx "container-type"
  | Container_name -> Pp.string ctx "container-name"
  | Container -> Pp.string ctx "container"
  | Anchor_name -> Pp.string ctx "anchor-name"
  | Position_anchor -> Pp.string ctx "position-anchor"
  | Position_try_fallbacks -> Pp.string ctx "position-try-fallbacks"
  | Position_try_order -> Pp.string ctx "position-try-order"
  | Position_try -> Pp.string ctx "position-try"
  | Position_visibility -> Pp.string ctx "position-visibility"
  | Position_area -> Pp.string ctx "position-area"
  | Shape_outside -> Pp.string ctx "shape-outside"
  | Shape_margin -> Pp.string ctx "shape-margin"
  | Shape_image_threshold -> Pp.string ctx "shape-image-threshold"
  | Overflow_clip_margin -> Pp.string ctx "overflow-clip-margin"
  | Overflow_anchor -> Pp.string ctx "overflow-anchor"
  | Scrollbar_width -> Pp.string ctx "scrollbar-width"
  | Scrollbar_color -> Pp.string ctx "scrollbar-color"
  | Scrollbar_gutter -> Pp.string ctx "scrollbar-gutter"
  | Line_height_step -> Pp.string ctx "line-height-step"
  | Font_palette -> Pp.string ctx "font-palette"
  | Font_synthesis -> Pp.string ctx "font-synthesis"
  | Text_wrap_mode -> Pp.string ctx "text-wrap-mode"
  | Text_wrap_style -> Pp.string ctx "text-wrap-style"
  | Text_box_trim -> Pp.string ctx "text-box-trim"
  | Text_underline_position -> Pp.string ctx "text-underline-position"
  | Text_box_edge -> Pp.string ctx "text-box-edge"
  | Text_box -> Pp.string ctx "text-box"
  | Inline_sizing -> Pp.string ctx "inline-sizing"
  | Line_fit_edge -> Pp.string ctx "line-fit-edge"
  | Interpolate_size -> Pp.string ctx "interpolate-size"
  | Min_intrinsic_sizing -> Pp.string ctx "min-intrinsic-sizing"
  | Ruby_align -> Pp.string ctx "ruby-align"
  | Ruby_merge -> Pp.string ctx "ruby-merge"
  | Ruby_overhang -> Pp.string ctx "ruby-overhang"
  | Ruby_position -> Pp.string ctx "ruby-position"
  | Glyph_orientation_vertical -> Pp.string ctx "glyph-orientation-vertical"
  | Animation_timeline -> Pp.string ctx "animation-timeline"
  | Animation_range -> Pp.string ctx "animation-range"
  | Animation_range_start -> Pp.string ctx "animation-range-start"
  | Animation_range_end -> Pp.string ctx "animation-range-end"
  | Scroll_timeline -> Pp.string ctx "scroll-timeline"
  | Scroll_timeline_name -> Pp.string ctx "scroll-timeline-name"
  | Scroll_timeline_axis -> Pp.string ctx "scroll-timeline-axis"
  | View_transition_name -> Pp.string ctx "view-transition-name"
  | View_transition_class -> Pp.string ctx "view-transition-class"
  | Image_orientation -> Pp.string ctx "image-orientation"
  | Image_rendering -> Pp.string ctx "image-rendering"
  | Image_resolution -> Pp.string ctx "image-resolution"
  | Contain_intrinsic_size -> Pp.string ctx "contain-intrinsic-size"
  | Contain_intrinsic_width -> Pp.string ctx "contain-intrinsic-width"
  | Contain_intrinsic_height -> Pp.string ctx "contain-intrinsic-height"
  | Contain_intrinsic_block_size -> Pp.string ctx "contain-intrinsic-block-size"
  | Contain_intrinsic_inline_size ->
      Pp.string ctx "contain-intrinsic-inline-size"
  | Margin_trim -> Pp.string ctx "margin-trim"
  | Offset_path -> Pp.string ctx "offset-path"
  | Offset -> Pp.string ctx "offset"
  | Offset_anchor -> Pp.string ctx "offset-anchor"
  | Offset_position -> Pp.string ctx "offset-position"
  | Offset_distance -> Pp.string ctx "offset-distance"
  | Offset_rotate -> Pp.string ctx "offset-rotate"
  | Font_size_adjust -> Pp.string ctx "font-size-adjust"
  | Font_variant_emoji -> Pp.string ctx "font-variant-emoji"
  | Text_spacing_trim -> Pp.string ctx "text-spacing-trim"
  | Hyphenate_limit_chars -> Pp.string ctx "hyphenate-limit-chars"
  | Initial_letter -> Pp.string ctx "initial-letter"
  | Initial_letter_align -> Pp.string ctx "initial-letter-align"
  | Initial_letter_wrap -> Pp.string ctx "initial-letter-wrap"
  | Dominant_baseline -> Pp.string ctx "dominant-baseline"
  | View_timeline_name -> Pp.string ctx "view-timeline-name"
  | View_timeline_axis -> Pp.string ctx "view-timeline-axis"
  | View_timeline_inset -> Pp.string ctx "view-timeline-inset"
  | View_timeline -> Pp.string ctx "view-timeline"
  | Timeline_scope -> Pp.string ctx "timeline-scope"
  | Perspective -> Pp.string ctx "perspective"
  | Perspective_origin -> Pp.string ctx "perspective-origin"
  | Transform_style -> Pp.string ctx "transform-style"
  | Backface_visibility -> Pp.string ctx "backface-visibility"
  | Object_position -> Pp.string ctx "object-position"
  | Rotate -> Pp.string ctx "rotate"
  | Scale -> Pp.string ctx "scale"
  | Transition_duration -> Pp.string ctx "transition-duration"
  | Transition_timing_function -> Pp.string ctx "transition-timing-function"
  | Transition_delay -> Pp.string ctx "transition-delay"
  | Transition_property -> Pp.string ctx "transition-property"
  | Transition_behavior -> Pp.string ctx "transition-behavior"
  | Overlay -> Pp.string ctx "overlay"
  | Will_change -> Pp.string ctx "will-change"
  | Contain -> Pp.string ctx "contain"
  | Isolation -> Pp.string ctx "isolation"
  | Break_before -> Pp.string ctx "break-before"
  | Break_after -> Pp.string ctx "break-after"
  | Break_inside -> Pp.string ctx "break-inside"
  | Page_break_before ->
      Pp.string ctx
        (if Pp.minified ctx then "break-before" else "page-break-before")
  | Page_break_after ->
      Pp.string ctx
        (if Pp.minified ctx then "break-after" else "page-break-after")
  | Page_break_inside ->
      Pp.string ctx
        (if Pp.minified ctx then "break-inside" else "page-break-inside")
  | Page_size -> Pp.string ctx "size"
  | Columns -> Pp.string ctx "columns"
  | Column_width -> Pp.string ctx "column-width"
  | Column_height -> Pp.string ctx "column-height"
  | Column_wrap -> Pp.string ctx "column-wrap"
  | Column_count -> Pp.string ctx "column-count"
  | Column_rule -> Pp.string ctx "column-rule"
  | Column_rule_width -> Pp.string ctx "column-rule-width"
  | Column_rule_style -> Pp.string ctx "column-rule-style"
  | Column_rule_color -> Pp.string ctx "column-rule-color"
  | Column_span -> Pp.string ctx "column-span"
  | Word_spacing -> Pp.string ctx "word-spacing"
  | Background_attachment -> Pp.string ctx "background-attachment"
  | Border_top -> Pp.string ctx "border-top"
  | Border_right -> Pp.string ctx "border-right"
  | Border_bottom -> Pp.string ctx "border-bottom"
  | Border_left -> Pp.string ctx "border-left"
  | Border_block -> Pp.string ctx "border-block"
  | Border_block_start -> Pp.string ctx "border-block-start"
  | Border_block_end -> Pp.string ctx "border-block-end"
  | Border_inline -> Pp.string ctx "border-inline"
  | Border_inline_start -> Pp.string ctx "border-inline-start"
  | Border_inline_end -> Pp.string ctx "border-inline-end"
  | Transform_origin -> Pp.string ctx "transform-origin"
  | Transform_box -> Pp.string ctx "transform-box"
  | Text_shadow -> Pp.string ctx "text-shadow"
  | Clip_path -> Pp.string ctx "clip-path"
  | Mask -> Pp.string ctx "mask"
  | Mask_border -> Pp.string ctx "mask-border"
  | Content_visibility -> Pp.string ctx "content-visibility"
  | Filter -> Pp.string ctx "filter"
  | Background_image -> Pp.string ctx "background-image"
  | Background_origin -> Pp.string ctx "background-origin"
  | Background_clip -> Pp.string ctx "background-clip"
  | Webkit_background_clip -> Pp.string ctx "-webkit-background-clip"
  | Animation -> Pp.string ctx "animation"
  | Aspect_ratio -> Pp.string ctx "aspect-ratio"
  | Overflow_x -> Pp.string ctx "overflow-x"
  | Overflow_y -> Pp.string ctx "overflow-y"
  | Overflow_block -> Pp.string ctx "overflow-block"
  | Overflow_inline -> Pp.string ctx "overflow-inline"
  | Vertical_align -> Pp.string ctx "vertical-align"
  | Font_family -> Pp.string ctx "font-family"
  | Background_position -> Pp.string ctx "background-position"
  | Background_position_x -> Pp.string ctx "background-position-x"
  | Background_position_y -> Pp.string ctx "background-position-y"
  | Webkit_mask_position_x -> Pp.string ctx "-webkit-mask-position-x"
  | Webkit_mask_position_y -> Pp.string ctx "-webkit-mask-position-y"
  | Background_repeat -> Pp.string ctx "background-repeat"
  | Background_size -> Pp.string ctx "background-size"
  | Webkit_font_smoothing -> Pp.string ctx "-webkit-font-smoothing"
  | Moz_osx_font_smoothing -> Pp.string ctx "-moz-osx-font-smoothing"
  | Webkit_line_clamp -> Pp.string ctx "-webkit-line-clamp"
  | Webkit_box_orient -> Pp.string ctx "-webkit-box-orient"
  | Moz_orient -> Pp.string ctx "-moz-orient"
  | Text_overflow -> Pp.string ctx "text-overflow"
  | Text_wrap -> Pp.string ctx "text-wrap"
  | Word_break -> Pp.string ctx "word-break"
  | Overflow_wrap -> Pp.string ctx "overflow-wrap"
  | Line_break -> Pp.string ctx "line-break"
  | Hyphens -> Pp.string ctx "hyphens"
  | Webkit_hyphens -> Pp.string ctx "-webkit-hyphens"
  | Text_rendering -> Pp.string ctx "text-rendering"
  | Webkit_touch_callout -> Pp.string ctx "-webkit-touch-callout"
  | Font_stretch -> Pp.string ctx "font-stretch"
  | Font_optical_sizing -> Pp.string ctx "font-optical-sizing"
  | Font_kerning -> Pp.string ctx "font-kerning"
  | Font_language_override -> Pp.string ctx "font-language-override"
  | Font_synthesis_style -> Pp.string ctx "font-synthesis-style"
  | Font_synthesis_weight -> Pp.string ctx "font-synthesis-weight"
  | Font_synthesis_small_caps -> Pp.string ctx "font-synthesis-small-caps"
  | Font_synthesis_position -> Pp.string ctx "font-synthesis-position"
  | Font_variant_ligatures -> Pp.string ctx "font-variant-ligatures"
  | Caps -> Pp.string ctx "font-variant-caps"
  | Numeric -> Pp.string ctx "font-variant-numeric"
  | Font_variant_position -> Pp.string ctx "font-variant-position"
  | Font_variant_alternates -> Pp.string ctx "font-variant-alternates"
  | Font_variant -> Pp.string ctx "font-variant"
  | East_asian -> Pp.string ctx "font-variant-east-asian"
  | Backdrop_filter -> Pp.string ctx "backdrop-filter"
  | Webkit_backdrop_filter -> Pp.string ctx "-webkit-backdrop-filter"
  | Webkit_mask_image -> Pp.string ctx "-webkit-mask-image"
  | Webkit_mask_composite -> Pp.string ctx "-webkit-mask-composite"
  | Webkit_mask_source_type -> Pp.string ctx "-webkit-mask-source-type"
  | Webkit_mask_size -> Pp.string ctx "-webkit-mask-size"
  | Webkit_mask_position -> Pp.string ctx "-webkit-mask-position"
  | Webkit_mask_repeat -> Pp.string ctx "-webkit-mask-repeat"
  | Webkit_mask_clip -> Pp.string ctx "-webkit-mask-clip"
  | Webkit_mask_origin -> Pp.string ctx "-webkit-mask-origin"
  | Border_image_source -> Pp.string ctx "border-image-source"
  | Border_image_slice -> Pp.string ctx "border-image-slice"
  | Border_image_repeat -> Pp.string ctx "border-image-repeat"
  | Border_image_width -> Pp.string ctx "border-image-width"
  | Border_image_outset -> Pp.string ctx "border-image-outset"
  | Mask_image -> Pp.string ctx "mask-image"
  | Mask_composite -> Pp.string ctx "mask-composite"
  | Mask_mode -> Pp.string ctx "mask-mode"
  | Mask_size -> Pp.string ctx "mask-size"
  | Mask_position -> Pp.string ctx "mask-position"
  | Mask_repeat -> Pp.string ctx "mask-repeat"
  | Mask_clip -> Pp.string ctx "mask-clip"
  | Mask_origin -> Pp.string ctx "mask-origin"
  | Mask_type -> Pp.string ctx "mask-type"
  | Scroll_snap_align -> Pp.string ctx "scroll-snap-align"
  | Scroll_snap_stop -> Pp.string ctx "scroll-snap-stop"
  | Scroll_behavior -> Pp.string ctx "scroll-behavior"
  | Box_sizing -> Pp.string ctx "box-sizing"
  | Webkit_box_sizing -> Pp.string ctx "-webkit-box-sizing"
  | Moz_box_sizing -> Pp.string ctx "-moz-box-sizing"
  | Field_sizing -> Pp.string ctx "field-sizing"
  | Caption_side -> Pp.string ctx "caption-side"
  | Resize -> Pp.string ctx "resize"
  | Object_fit -> Pp.string ctx "object-fit"
  | Object_view_box -> Pp.string ctx "object-view-box"
  | Appearance -> Pp.string ctx "appearance"
  | Color_scheme -> Pp.string ctx "color-scheme"
  | Print_color_adjust -> Pp.string ctx "print-color-adjust"
  | Webkit_print_color_adjust -> Pp.string ctx "-webkit-print-color-adjust"
  | Box_decoration_break -> Pp.string ctx "box-decoration-break"
  | Webkit_box_decoration_break -> Pp.string ctx "-webkit-box-decoration-break"
  | Content -> Pp.string ctx "content"
  | Counter_reset -> Pp.string ctx "counter-reset"
  | Counter_increment -> Pp.string ctx "counter-increment"
  | Quotes -> Pp.string ctx "quotes"
  | Text_size_adjust -> Pp.string ctx "text-size-adjust"
  | Touch_action -> Pp.string ctx "touch-action"
  | Direction -> Pp.string ctx "direction"
  | Unicode_bidi -> Pp.string ctx "unicode-bidi"
  | Writing_mode -> Pp.string ctx "writing-mode"
  | Text_combine_upright -> Pp.string ctx "text-combine-upright"
  | Text_decoration_skip_ink -> Pp.string ctx "text-decoration-skip-ink"
  | Animation_name -> Pp.string ctx "animation-name"
  | Animation_duration -> Pp.string ctx "animation-duration"
  | Animation_timing_function -> Pp.string ctx "animation-timing-function"
  | Animation_delay -> Pp.string ctx "animation-delay"
  | Animation_iteration_count -> Pp.string ctx "animation-iteration-count"
  | Animation_direction -> Pp.string ctx "animation-direction"
  | Animation_fill_mode -> Pp.string ctx "animation-fill-mode"
  | Animation_play_state -> Pp.string ctx "animation-play-state"
  | Animation_composition -> Pp.string ctx "animation-composition"
  | Background_blend_mode -> Pp.string ctx "background-blend-mode"
  | Scroll_margin -> Pp.string ctx "scroll-margin"
  | Scroll_margin_top -> Pp.string ctx "scroll-margin-top"
  | Scroll_margin_right -> Pp.string ctx "scroll-margin-right"
  | Scroll_margin_bottom -> Pp.string ctx "scroll-margin-bottom"
  | Scroll_margin_left -> Pp.string ctx "scroll-margin-left"
  | Scroll_margin_inline -> Pp.string ctx "scroll-margin-inline"
  | Scroll_margin_inline_start -> Pp.string ctx "scroll-margin-inline-start"
  | Scroll_margin_inline_end -> Pp.string ctx "scroll-margin-inline-end"
  | Scroll_margin_block -> Pp.string ctx "scroll-margin-block"
  | Scroll_margin_block_start -> Pp.string ctx "scroll-margin-block-start"
  | Scroll_margin_block_end -> Pp.string ctx "scroll-margin-block-end"
  | Scroll_padding -> Pp.string ctx "scroll-padding"
  | Scroll_padding_top -> Pp.string ctx "scroll-padding-top"
  | Scroll_padding_right -> Pp.string ctx "scroll-padding-right"
  | Scroll_padding_bottom -> Pp.string ctx "scroll-padding-bottom"
  | Scroll_padding_left -> Pp.string ctx "scroll-padding-left"
  | Scroll_padding_inline -> Pp.string ctx "scroll-padding-inline"
  | Scroll_padding_inline_start -> Pp.string ctx "scroll-padding-inline-start"
  | Scroll_padding_inline_end -> Pp.string ctx "scroll-padding-inline-end"
  | Scroll_padding_block -> Pp.string ctx "scroll-padding-block"
  | Scroll_padding_block_start -> Pp.string ctx "scroll-padding-block-start"
  | Scroll_padding_block_end -> Pp.string ctx "scroll-padding-block-end"
  | Overscroll_behavior -> Pp.string ctx "overscroll-behavior"
  | Overscroll_behavior_x -> Pp.string ctx "overscroll-behavior-x"
  | Overscroll_behavior_y -> Pp.string ctx "overscroll-behavior-y"
  | Overscroll_behavior_block -> Pp.string ctx "overscroll-behavior-block"
  | Overscroll_behavior_inline -> Pp.string ctx "overscroll-behavior-inline"
  | Accent_color -> Pp.string ctx "accent-color"
  | Caret_color -> Pp.string ctx "caret-color"
  | Webkit_transform -> Pp.string ctx "-webkit-transform"
  | Moz_transform -> Pp.string ctx "-moz-transform"
  | Ms_transform -> Pp.string ctx "-ms-transform"
  | O_transform -> Pp.string ctx "-o-transform"
  | Webkit_transition -> Pp.string ctx "-webkit-transition"
  | Webkit_transition_delay -> Pp.string ctx "-webkit-transition-delay"
  | Webkit_transition_duration -> Pp.string ctx "-webkit-transition-duration"
  | Webkit_transition_property -> Pp.string ctx "-webkit-transition-property"
  | Webkit_transition_timing_function ->
      Pp.string ctx "-webkit-transition-timing-function"
  | Webkit_animation -> Pp.string ctx "-webkit-animation"
  | Webkit_animation_delay -> Pp.string ctx "-webkit-animation-delay"
  | Webkit_animation_duration -> Pp.string ctx "-webkit-animation-duration"
  | Webkit_animation_direction -> Pp.string ctx "-webkit-animation-direction"
  | Webkit_animation_iteration_count ->
      Pp.string ctx "-webkit-animation-iteration-count"
  | Webkit_animation_name -> Pp.string ctx "-webkit-animation-name"
  | Webkit_animation_timing_function ->
      Pp.string ctx "-webkit-animation-timing-function"
  | Webkit_animation_fill_mode -> Pp.string ctx "-webkit-animation-fill-mode"
  | Webkit_animation_play_state -> Pp.string ctx "-webkit-animation-play-state"
  | Webkit_flex_direction -> Pp.string ctx "-webkit-flex-direction"
  | Webkit_flex_wrap -> Pp.string ctx "-webkit-flex-wrap"
  | Webkit_flex_flow -> Pp.string ctx "-webkit-flex-flow"
  | Webkit_justify_content -> Pp.string ctx "-webkit-justify-content"
  | Webkit_align_items -> Pp.string ctx "-webkit-align-items"
  | Webkit_align_content -> Pp.string ctx "-webkit-align-content"
  | Webkit_align_self -> Pp.string ctx "-webkit-align-self"
  | Webkit_border_radius -> Pp.string ctx "-webkit-border-radius"
  | Webkit_box_shadow -> Pp.string ctx "-webkit-box-shadow"
  | Webkit_background_size -> Pp.string ctx "-webkit-background-size"
  | Webkit_filter -> Pp.string ctx "-webkit-filter"
  | Moz_appearance -> Pp.string ctx "-moz-appearance"
  | Moz_animation -> Pp.string ctx "-moz-animation"
  | Moz_animation_delay -> Pp.string ctx "-moz-animation-delay"
  | Moz_animation_duration -> Pp.string ctx "-moz-animation-duration"
  | Moz_animation_direction -> Pp.string ctx "-moz-animation-direction"
  | Moz_animation_iteration_count ->
      Pp.string ctx "-moz-animation-iteration-count"
  | Moz_animation_name -> Pp.string ctx "-moz-animation-name"
  | Moz_animation_timing_function ->
      Pp.string ctx "-moz-animation-timing-function"
  | Moz_animation_fill_mode -> Pp.string ctx "-moz-animation-fill-mode"
  | Moz_animation_play_state -> Pp.string ctx "-moz-animation-play-state"
  | Moz_transition -> Pp.string ctx "-moz-transition"
  | Moz_transition_delay -> Pp.string ctx "-moz-transition-delay"
  | Moz_transition_duration -> Pp.string ctx "-moz-transition-duration"
  | Moz_transition_property -> Pp.string ctx "-moz-transition-property"
  | Moz_transition_timing_function ->
      Pp.string ctx "-moz-transition-timing-function"
  | Moz_border_radius -> Pp.string ctx "-moz-border-radius"
  | Moz_box_shadow -> Pp.string ctx "-moz-box-shadow"
  | Ms_filter -> Pp.string ctx "-ms-filter"
  | O_transition -> Pp.string ctx "-o-transition"

(* Per-property semantic metadata. Keep this exhaustive over the sealed property
   GADT: adding a constructor must decide both its default inheritance and
   whether its typed value can contain a parse-time [Invalid] arm. Names without
   a typed constructor stay in [Unknown_property], where the payload is the only
   identity available. Shorthands count as inherited when every longhand they
   reset inherits. *)
type _ property_class =
  | Inherited : 'a property_class
  | Non_inherited : 'a property_class
  | Checks_length_percentage : length_percentage property_class
  | Checks_rotate : rotate_value property_class
  | Checks_clip_path : clip_path property_class
  | Checks_text_indent : text_indent_value property_class
  | Checks_font_family : font_family property_class

let property_class : type a. a property -> a property_class = function
  | Unknown_property name -> (
      match String.lowercase_ascii name with
      | "empty-cells" | "font-variant" | "font-variant-alternates" | "orphans"
      | "text-align-last" | "text-justify" | "widows" | "word-wrap" ->
          Inherited
      | _ -> Non_inherited)
  | Width -> Checks_length_percentage
  | Height -> Checks_length_percentage
  | Min_width -> Checks_length_percentage
  | Min_height -> Checks_length_percentage
  | Max_width -> Checks_length_percentage
  | Max_height -> Checks_length_percentage
  | Block_size -> Checks_length_percentage
  | Inline_size -> Checks_length_percentage
  | Min_block_size -> Checks_length_percentage
  | Min_inline_size -> Checks_length_percentage
  | Max_block_size -> Checks_length_percentage
  | Max_inline_size -> Checks_length_percentage
  | Shape_margin -> Checks_length_percentage
  | Offset_distance -> Checks_length_percentage
  | Rotate -> Checks_rotate
  | Clip_path -> Checks_clip_path
  | Text_indent -> Checks_text_indent
  | Font_family -> Checks_font_family
  | Color -> Inherited
  | Font_size -> Inherited
  | Line_height -> Inherited
  | Font_weight -> Inherited
  | Font_style -> Inherited
  | Text_align -> Inherited
  | Text_underline_offset -> Inherited
  | Text_decoration_skip -> Inherited
  | Text_decoration_skip_box -> Inherited
  | Text_decoration_skip_inset -> Inherited
  | Text_decoration_skip_spaces -> Inherited
  | Text_emphasis -> Inherited
  | Text_emphasis_style -> Inherited
  | Text_emphasis_color -> Inherited
  | Text_emphasis_position -> Inherited
  | Text_emphasis_skip -> Inherited
  | Text_orientation -> Inherited
  | Text_transform -> Inherited
  | Letter_spacing -> Inherited
  | List_style_type -> Inherited
  | List_style_position -> Inherited
  | List_style_image -> Inherited
  | Visibility -> Inherited
  | Fill_opacity -> Inherited
  | Stroke_opacity -> Inherited
  | Cursor -> Inherited
  | Interactivity -> Inherited
  | Caret_animation -> Inherited
  | Caret_shape -> Inherited
  | Caret -> Inherited
  | Interest_delay -> Inherited
  | Interest_delay_start -> Inherited
  | Interest_delay_end -> Inherited
  | Border_collapse -> Inherited
  | Border_spacing -> Inherited
  | Pointer_events -> Inherited
  | Forced_color_adjust -> Inherited
  | White_space -> Inherited
  | White_space_collapse -> Inherited
  | Font_variant_alternates -> Inherited
  | Font_variant -> Inherited
  | Tab_size -> Inherited
  | Webkit_text_size_adjust -> Inherited
  | Font_feature_settings -> Inherited
  | Font_variation_settings -> Inherited
  | Webkit_tap_highlight_color -> Inherited
  | Webkit_text_fill_color -> Inherited
  | Webkit_text_stroke_color -> Inherited
  | Webkit_text_stroke -> Inherited
  | Webkit_text_stroke_width -> Inherited
  | List_style -> Inherited
  | Font -> Inherited
  | Scrollbar_color -> Inherited
  | Line_height_step -> Inherited
  | Font_palette -> Inherited
  | Font_synthesis -> Inherited
  | Text_wrap_mode -> Inherited
  | Text_wrap_style -> Inherited
  | Text_underline_position -> Inherited
  | Text_box_edge -> Inherited
  | Inline_sizing -> Inherited
  | Line_fit_edge -> Inherited
  | Interpolate_size -> Inherited
  | Ruby_align -> Inherited
  | Ruby_merge -> Inherited
  | Ruby_overhang -> Inherited
  | Ruby_position -> Inherited
  | Glyph_orientation_vertical -> Inherited
  | Text_combine_upright -> Inherited
  | Image_orientation -> Inherited
  | Image_rendering -> Inherited
  | Image_resolution -> Inherited
  | Font_size_adjust -> Inherited
  | Font_variant_emoji -> Inherited
  | Text_spacing_trim -> Inherited
  | Hyphenate_limit_chars -> Inherited
  | Initial_letter_align -> Inherited
  | Initial_letter_wrap -> Inherited
  | Dominant_baseline -> Inherited
  | Word_spacing -> Inherited
  | Text_shadow -> Inherited
  | Webkit_font_smoothing -> Inherited
  | Moz_osx_font_smoothing -> Inherited
  | Text_wrap -> Inherited
  | Word_break -> Inherited
  | Overflow_wrap -> Inherited
  | Line_break -> Inherited
  | Hyphens -> Inherited
  | Webkit_hyphens -> Inherited
  | Text_rendering -> Inherited
  | Webkit_touch_callout -> Inherited
  | Font_stretch -> Inherited
  | Font_optical_sizing -> Inherited
  | Font_kerning -> Inherited
  | Font_language_override -> Inherited
  | Font_synthesis_style -> Inherited
  | Font_synthesis_weight -> Inherited
  | Font_synthesis_small_caps -> Inherited
  | Font_synthesis_position -> Inherited
  | Font_variant_ligatures -> Inherited
  | Caps -> Inherited
  | Numeric -> Inherited
  | Font_variant_position -> Inherited
  | East_asian -> Inherited
  | Caption_side -> Inherited
  | Color_scheme -> Inherited
  | Print_color_adjust -> Inherited
  | Webkit_print_color_adjust -> Inherited
  | Quotes -> Inherited
  | Text_size_adjust -> Inherited
  | Fill -> Inherited
  | Stroke -> Inherited
  | Stroke_width -> Inherited
  | Fill_rule -> Inherited
  | Clip_rule -> Inherited
  | Stroke_linecap -> Inherited
  | Stroke_linejoin -> Inherited
  | Stroke_miterlimit -> Inherited
  | Stroke_dashoffset -> Inherited
  | Stroke_dasharray -> Inherited
  | Paint_order -> Inherited
  | Direction -> Inherited
  | Writing_mode -> Inherited
  | Text_decoration_skip_ink -> Inherited
  | Accent_color -> Inherited
  | Caret_color -> Inherited
  | Custom_property _ | All | Background_color | Border_color | Border_style
  | Border_top_style | Border_right_style | Border_bottom_style
  | Border_left_style | Border_inline_start_style | Border_inline_end_style
  | Border_block_start_style | Border_block_end_style | Padding | Padding_left
  | Padding_right | Padding_bottom | Padding_top | Padding_inline
  | Padding_inline_start | Padding_inline_end | Padding_block
  | Padding_block_start | Padding_block_end | Margin | Margin_inline_end
  | Margin_inline_start | Margin_left | Margin_right | Margin_top
  | Margin_bottom | Margin_inline | Margin_block | Margin_block_start
  | Margin_block_end | Gap | Column_gap | Row_gap | Text_decoration
  | Text_decoration_line | Text_decoration_style | Text_decoration_color
  | Text_decoration_skip_self | Display | Position | Baseline_source
  | Alignment_baseline | Baseline_shift | Flex_direction | Flex_wrap | Flex_flow
  | Flex | Flex_grow | Flex_shrink | Flex_basis | Order | Align_items
  | Justify_content | Justify_items | Justify_self | Align_content | Align_self
  | Place_content | Place_items | Place_self | Grid_template_columns
  | Grid_template_rows | Grid_template_areas | Grid_template | Grid | Grid_area
  | Grid_auto_flow | Grid_auto_columns | Grid_auto_rows | Grid_column | Grid_row
  | Grid_column_start | Grid_column_end | Grid_row_start | Grid_row_end
  | Border_width | Border_top_width | Border_right_width | Border_bottom_width
  | Border_left_width | Border_inline_start_width | Border_inline_end_width
  | Border_block_start_width | Border_block_end_width | Border_inline_width
  | Border_block_width | Border_image | Border_image_source | Border_image_slice
  | Border_image_repeat | Border_image_width | Border_image_outset
  | Border_radius | Border_top_left_radius | Border_top_right_radius
  | Border_bottom_left_radius | Border_bottom_right_radius | Border_top_color
  | Border_right_color | Border_bottom_color | Border_left_color
  | Border_inline_start_color | Border_inline_end_color
  | Border_block_start_color | Border_block_end_color | Border_inline_color
  | Border_block_color | Border_inline_style | Border_block_style
  | Border_start_start_radius | Border_start_end_radius
  | Border_end_start_radius | Border_end_end_radius | Opacity | Stop_opacity
  | Flood_opacity | Mix_blend_mode | Transform | Translate | Nav_up | Nav_right
  | Nav_down | Nav_left | Table_layout | User_select | Overflow | Inset
  | Inset_inline | Inset_inline_start | Inset_inline_end | Inset_block
  | Inset_block_start | Inset_block_end | Top | Right | Bottom | Left | Z_index
  | Outline | Outline_style | Outline_width | Outline_color | Outline_offset
  | Scroll_snap_type | Border | Border_block | Border_block_start
  | Border_block_end | Border_inline | Border_inline_start | Border_inline_end
  | Background | Zoom | Webkit_user_select | Moz_user_select | Ms_user_select
  | Webkit_text_decoration | Webkit_text_decoration_color | Source
  | Webkit_appearance | Webkit_transform | Moz_transform | Ms_transform
  | O_transform | Webkit_transition | Webkit_transition_delay
  | Webkit_transition_duration | Webkit_transition_property
  | Webkit_transition_timing_function | Webkit_animation
  | Webkit_animation_delay | Webkit_animation_duration
  | Webkit_animation_direction | Webkit_animation_iteration_count
  | Webkit_animation_name | Webkit_animation_timing_function
  | Webkit_animation_fill_mode | Webkit_animation_play_state
  | Webkit_flex_direction | Webkit_flex_wrap | Webkit_flex_flow
  | Webkit_justify_content | Webkit_align_items | Webkit_align_content
  | Webkit_align_self | Webkit_border_radius | Webkit_box_sizing
  | Moz_box_sizing | Webkit_box_shadow | Webkit_background_size | Webkit_filter
  | Moz_appearance | Moz_animation | Moz_animation_delay
  | Moz_animation_duration | Moz_animation_direction
  | Moz_animation_iteration_count | Moz_animation_name
  | Moz_animation_timing_function | Moz_animation_fill_mode
  | Moz_animation_play_state | Moz_transition | Moz_transition_delay
  | Moz_transition_duration | Moz_transition_property
  | Moz_transition_timing_function | Moz_border_radius | Moz_box_shadow
  | Ms_filter | O_transition | Container_type | Container_name | Container
  | Anchor_name | Position_anchor | Position_try_fallbacks | Position_try_order
  | Position_try | Position_visibility | Position_area | Shape_outside
  | Shape_image_threshold | Overflow_clip_margin | Overflow_anchor
  | Scrollbar_width | Scrollbar_gutter | Text_box_trim | Text_box
  | Min_intrinsic_sizing | Animation_timeline | Animation_range
  | Animation_range_start | Animation_range_end | Scroll_timeline
  | Scroll_timeline_name | Scroll_timeline_axis | View_transition_name
  | View_transition_class | Contain_intrinsic_size | Contain_intrinsic_width
  | Contain_intrinsic_height | Contain_intrinsic_block_size
  | Contain_intrinsic_inline_size | Margin_trim | Offset_path | Offset_rotate
  | Initial_letter | View_timeline_name | View_timeline_axis
  | View_timeline_inset | View_timeline | Timeline_scope | Perspective
  | Perspective_origin | Transform_style | Backface_visibility | Object_position
  | Transition_duration | Transition_timing_function | Transition_delay
  | Transition_property | Transition_behavior | Overlay | Will_change | Contain
  | Isolation | Break_before | Break_after | Break_inside | Page_break_before
  | Page_break_after | Page_break_inside | Page_size | Columns | Column_width
  | Column_height | Column_wrap | Column_count | Column_rule | Column_rule_color
  | Column_rule_width | Column_rule_style | Column_span | Background_attachment
  | Border_top | Border_right | Border_bottom | Border_left | Transform_origin
  | Transform_box | Mask | Mask_border | Content_visibility | Filter
  | Background_image | Background_origin | Background_clip
  | Webkit_background_clip | Animation | Aspect_ratio | Overflow_x | Overflow_y
  | Overflow_block | Overflow_inline | Vertical_align | Background_position
  | Background_position_x | Background_position_y | Webkit_mask_position_x
  | Webkit_mask_position_y | Background_repeat | Background_size
  | Webkit_line_clamp | Webkit_box_orient | Moz_orient | Text_overflow
  | Backdrop_filter | Webkit_backdrop_filter | Webkit_mask_image
  | Webkit_mask_composite | Webkit_mask_source_type | Webkit_mask_size
  | Webkit_mask_position | Webkit_mask_repeat | Webkit_mask_clip
  | Webkit_mask_origin | Mask_image | Mask_composite | Mask_mode | Mask_size
  | Mask_position | Mask_repeat | Mask_clip | Mask_origin | Mask_type
  | Scroll_snap_align | Scroll_snap_stop | Scroll_behavior | Box_sizing
  | Field_sizing | Resize | Object_fit | Object_view_box | Appearance
  | Box_decoration_break | Webkit_box_decoration_break | Content | Counter_reset
  | Counter_increment | Text_decoration_thickness | Touch_action | Clip | Clear
  | Float | Scale | Transition | Box_shadow | Vector_effect | Stop_color
  | Flood_color | Lighting_color | Unicode_bidi | Animation_name
  | Animation_duration | Animation_timing_function | Animation_delay
  | Animation_iteration_count | Animation_direction | Animation_fill_mode
  | Animation_play_state | Animation_composition | Background_blend_mode
  | Scroll_margin | Scroll_margin_top | Scroll_margin_right
  | Scroll_margin_bottom | Scroll_margin_left | Scroll_margin_inline
  | Scroll_margin_inline_start | Scroll_margin_inline_end | Scroll_margin_block
  | Scroll_margin_block_start | Scroll_margin_block_end | Scroll_padding
  | Scroll_padding_top | Scroll_padding_right | Scroll_padding_bottom
  | Scroll_padding_left | Scroll_padding_inline | Scroll_padding_inline_start
  | Scroll_padding_inline_end | Scroll_padding_block
  | Scroll_padding_block_start | Scroll_padding_block_end | Overscroll_behavior
  | Overscroll_behavior_x | Overscroll_behavior_y | Overscroll_behavior_block
  | Overscroll_behavior_inline | Offset_anchor | Offset_position | Offset ->
      Non_inherited

let property_is_inherited : type a. a property -> bool =
 fun property ->
  match property_class property with
  | Inherited -> true
  | Non_inherited -> false
  | Checks_length_percentage -> false
  | Checks_rotate -> false
  | Checks_clip_path -> false
  | Checks_text_indent -> true
  | Checks_font_family -> true

(* Whether the name [pp_property] gives [property] under minify can carry
   [value]. CSS Fragmentation 3 sec. 3.4 defines the [page-break-*] alias of
   [break-*] by a value mapping table, so the [break-*] name a [page-break-*]
   property minifies to is a spelling the declaration can use only for a value
   the table names: [always] maps to [page], the rest map to themselves, and a
   [var()] maps to nothing here since substitution happens at computed-value
   time. Every other property names itself the same whatever it carries. *)
let minified_name_carries : type a. a property -> a -> bool =
 fun property value ->
  match property with
  | Page_break_before -> Option.is_some (break_of_page_break value)
  | Page_break_after -> Option.is_some (break_of_page_break value)
  | Page_break_inside -> Option.is_some (break_inside_of_page_break value)
  | _ -> true

let pp_any_property ctx (Prop p) = pp_property ctx p

(* CSS 2.1 sec. 4.1.2.1 reserves a name that starts with a [-] for a vendor's
   own extension. A typed property answers by its constructor, and a name the
   reader types none of by its leading [-]; a custom property's [--] is not a
   prefix. The match carries no wildcard, so a new constructor fails this build
   until it is classified. *)
let is_vendor_prefixed : type a. a property -> bool = function
  | Custom_property _ -> false
  | Unknown_property name ->
      String.starts_with ~prefix:"-" name
      && not (String.starts_with ~prefix:"--" name)
  | Webkit_text_size_adjust | Webkit_tap_highlight_color | Webkit_user_select
  | Moz_user_select | Ms_user_select | Webkit_text_decoration
  | Webkit_text_decoration_color | Webkit_text_fill_color
  | Webkit_text_stroke_color | Webkit_text_stroke | Webkit_text_stroke_width
  | Webkit_appearance | Webkit_transform | Moz_transform | Ms_transform
  | O_transform | Webkit_transition | Webkit_transition_delay
  | Webkit_transition_duration | Webkit_transition_property
  | Webkit_transition_timing_function | Webkit_animation
  | Webkit_animation_delay | Webkit_animation_duration
  | Webkit_animation_direction | Webkit_animation_iteration_count
  | Webkit_animation_name | Webkit_animation_timing_function
  | Webkit_animation_fill_mode | Webkit_animation_play_state
  | Webkit_flex_direction | Webkit_flex_wrap | Webkit_flex_flow
  | Webkit_justify_content | Webkit_align_items | Webkit_align_content
  | Webkit_align_self | Webkit_border_radius | Webkit_box_sizing
  | Moz_box_sizing | Webkit_box_shadow | Webkit_background_size | Webkit_filter
  | Moz_appearance | Moz_animation | Moz_animation_delay
  | Moz_animation_duration | Moz_animation_direction
  | Moz_animation_iteration_count | Moz_animation_name
  | Moz_animation_timing_function | Moz_animation_fill_mode
  | Moz_animation_play_state | Moz_transition | Moz_transition_delay
  | Moz_transition_duration | Moz_transition_property
  | Moz_transition_timing_function | Moz_border_radius | Moz_box_shadow
  | Ms_filter | O_transition | Webkit_background_clip | Webkit_mask_position_x
  | Webkit_mask_position_y | Webkit_font_smoothing | Moz_osx_font_smoothing
  | Webkit_line_clamp | Webkit_box_orient | Moz_orient | Webkit_hyphens
  | Webkit_touch_callout | Webkit_backdrop_filter | Webkit_mask_image
  | Webkit_mask_composite | Webkit_mask_source_type | Webkit_mask_size
  | Webkit_mask_position | Webkit_mask_repeat | Webkit_mask_clip
  | Webkit_mask_origin | Webkit_print_color_adjust | Webkit_box_decoration_break
    ->
      true
  | All | Background_color | Color | Border_color | Border_style -> false
  | Border_top_style | Border_right_style | Border_bottom_style -> false
  | Border_left_style | Border_inline_start_style -> false
  | Border_inline_end_style | Border_block_start_style -> false
  | Border_block_end_style | Padding | Padding_left | Padding_right -> false
  | Padding_bottom | Padding_top | Padding_inline -> false
  | Padding_inline_start | Padding_inline_end | Padding_block -> false
  | Padding_block_start | Padding_block_end | Margin -> false
  | Margin_inline_end | Margin_inline_start | Margin_left -> false
  | Margin_right | Margin_top | Margin_bottom | Margin_inline -> false
  | Margin_block | Margin_block_start | Margin_block_end | Gap -> false
  | Column_gap | Row_gap | Width | Height | Min_width | Min_height -> false
  | Max_width | Max_height | Inline_size | Min_inline_size -> false
  | Max_inline_size | Block_size | Min_block_size | Max_block_size -> false
  | Font_size | Line_height | Font_weight | Font_style | Text_align -> false
  | Text_decoration | Text_decoration_line | Text_decoration_style -> false
  | Text_decoration_color | Text_underline_offset -> false
  | Text_decoration_skip | Text_decoration_skip_self -> false
  | Text_decoration_skip_box | Text_decoration_skip_inset -> false
  | Text_decoration_skip_spaces | Text_emphasis | Text_emphasis_style -> false
  | Text_emphasis_color | Text_emphasis_position | Text_emphasis_skip -> false
  | Text_orientation | Text_transform | Letter_spacing -> false
  | List_style_type | List_style_position | List_style_image | Display -> false
  | Position | Visibility | Baseline_source | Alignment_baseline -> false
  | Baseline_shift | Flex_direction | Flex_wrap | Flex_flow | Flex -> false
  | Flex_grow | Flex_shrink | Flex_basis | Order | Align_items -> false
  | Justify_content | Justify_items | Justify_self | Align_content -> false
  | Align_self | Place_content | Place_items | Place_self -> false
  | Grid_template_columns | Grid_template_rows | Grid_template_areas -> false
  | Grid_template | Grid | Grid_area | Grid_auto_flow -> false
  | Grid_auto_columns | Grid_auto_rows | Grid_column | Grid_row -> false
  | Grid_column_start | Grid_column_end | Grid_row_start | Grid_row_end -> false
  | Border_width | Border_top_width | Border_right_width -> false
  | Border_bottom_width | Border_left_width | Border_inline_start_width -> false
  | Border_inline_end_width | Border_block_start_width -> false
  | Border_block_end_width | Border_inline_width | Border_block_width -> false
  | Border_image | Border_image_source | Border_image_slice -> false
  | Border_image_repeat | Border_image_width | Border_image_outset -> false
  | Border_radius | Border_top_left_radius | Border_top_right_radius -> false
  | Border_bottom_left_radius | Border_bottom_right_radius -> false
  | Border_top_color | Border_right_color | Border_bottom_color -> false
  | Border_left_color | Border_inline_start_color -> false
  | Border_inline_end_color | Border_block_start_color -> false
  | Border_block_end_color | Border_inline_color | Border_block_color -> false
  | Border_inline_style | Border_block_style -> false
  | Border_start_start_radius | Border_start_end_radius -> false
  | Border_end_start_radius | Border_end_end_radius | Opacity -> false
  | Fill_opacity | Stroke_opacity | Stop_opacity | Flood_opacity -> false
  | Mix_blend_mode | Transform | Translate | Cursor | Interactivity -> false
  | Caret_animation | Caret_shape | Caret | Interest_delay -> false
  | Interest_delay_start | Interest_delay_end | Nav_up | Nav_right -> false
  | Nav_down | Nav_left | Table_layout | Border_collapse -> false
  | Border_spacing | User_select | Pointer_events | Overflow | Inset -> false
  | Inset_inline | Inset_inline_start | Inset_inline_end | Inset_block -> false
  | Inset_block_start | Inset_block_end | Top | Right | Bottom | Left -> false
  | Z_index | Outline | Outline_style | Outline_width | Outline_color -> false
  | Outline_offset | Forced_color_adjust | Scroll_snap_type -> false
  | White_space | White_space_collapse | Font_variant_alternates -> false
  | Font_variant | Border | Border_block | Border_block_start -> false
  | Border_block_end | Border_inline | Border_inline_start -> false
  | Border_inline_end | Background | Tab_size | Zoom -> false
  | Font_feature_settings | Font_variation_settings | Text_indent -> false
  | List_style | Font | Source | Container_type | Container_name -> false
  | Container | Anchor_name | Position_anchor | Position_try_fallbacks -> false
  | Position_try_order | Position_try | Position_visibility -> false
  | Position_area | Shape_outside | Shape_margin -> false
  | Shape_image_threshold | Overflow_clip_margin | Overflow_anchor -> false
  | Scrollbar_width | Scrollbar_color | Scrollbar_gutter -> false
  | Line_height_step | Font_palette | Font_synthesis | Text_wrap_mode -> false
  | Text_wrap_style | Text_box_trim | Text_underline_position -> false
  | Text_box_edge | Text_box | Inline_sizing | Line_fit_edge -> false
  | Interpolate_size | Min_intrinsic_sizing | Ruby_align | Ruby_merge -> false
  | Ruby_overhang | Ruby_position | Glyph_orientation_vertical -> false
  | Text_combine_upright | Animation_timeline | Animation_range -> false
  | Animation_range_start | Animation_range_end | Scroll_timeline -> false
  | Scroll_timeline_name | Scroll_timeline_axis | View_transition_name -> false
  | View_transition_class | Image_orientation | Image_rendering -> false
  | Image_resolution | Contain_intrinsic_size | Contain_intrinsic_width -> false
  | Contain_intrinsic_height | Contain_intrinsic_block_size -> false
  | Contain_intrinsic_inline_size | Margin_trim | Offset_path -> false
  | Offset_distance | Offset_rotate | Font_size_adjust -> false
  | Font_variant_emoji | Text_spacing_trim | Hyphenate_limit_chars -> false
  | Initial_letter | Initial_letter_align | Initial_letter_wrap -> false
  | Dominant_baseline | View_timeline_name | View_timeline_axis -> false
  | View_timeline_inset | View_timeline | Timeline_scope | Perspective -> false
  | Perspective_origin | Transform_style | Backface_visibility -> false
  | Object_position | Rotate | Transition_duration -> false
  | Transition_timing_function | Transition_delay | Transition_property -> false
  | Transition_behavior | Overlay | Will_change | Contain | Isolation -> false
  | Break_before | Break_after | Break_inside | Page_break_before -> false
  | Page_break_after | Page_break_inside | Page_size | Columns -> false
  | Column_width | Column_height | Column_wrap | Column_count -> false
  | Column_rule | Column_rule_color | Column_rule_width -> false
  | Column_rule_style | Column_span | Word_spacing -> false
  | Background_attachment | Border_top | Border_right | Border_bottom -> false
  | Border_left | Transform_origin | Transform_box | Text_shadow -> false
  | Clip_path | Mask | Mask_border | Content_visibility | Filter -> false
  | Background_image | Background_origin | Background_clip | Animation -> false
  | Aspect_ratio | Overflow_x | Overflow_y | Overflow_block -> false
  | Overflow_inline | Vertical_align | Font_family -> false
  | Background_position | Background_position_x | Background_position_y -> false
  | Background_repeat | Background_size | Text_overflow | Text_wrap -> false
  | Word_break | Overflow_wrap | Line_break | Hyphens | Text_rendering -> false
  | Font_stretch | Font_optical_sizing | Font_kerning -> false
  | Font_language_override | Font_synthesis_style -> false
  | Font_synthesis_weight | Font_synthesis_small_caps -> false
  | Font_synthesis_position | Font_variant_ligatures | Caps | Numeric -> false
  | Font_variant_position | East_asian | Backdrop_filter | Mask_image -> false
  | Mask_composite | Mask_mode | Mask_size | Mask_position -> false
  | Mask_repeat | Mask_clip | Mask_origin | Mask_type -> false
  | Scroll_snap_align | Scroll_snap_stop | Scroll_behavior | Box_sizing -> false
  | Field_sizing | Caption_side | Resize | Object_fit | Object_view_box -> false
  | Appearance | Color_scheme | Print_color_adjust -> false
  | Box_decoration_break | Content | Counter_reset | Counter_increment -> false
  | Quotes | Text_decoration_thickness | Text_size_adjust -> false
  | Touch_action | Clip | Clear | Float | Scale | Transition -> false
  | Box_shadow | Fill | Stroke | Stroke_width | Fill_rule | Clip_rule -> false
  | Stroke_linecap | Stroke_linejoin | Stroke_miterlimit -> false
  | Stroke_dashoffset | Stroke_dasharray | Paint_order | Vector_effect -> false
  | Stop_color | Flood_color | Lighting_color | Direction -> false
  | Unicode_bidi | Writing_mode | Text_decoration_skip_ink -> false
  | Animation_name | Animation_duration | Animation_timing_function -> false
  | Animation_delay | Animation_iteration_count | Animation_direction -> false
  | Animation_fill_mode | Animation_play_state | Animation_composition -> false
  | Background_blend_mode | Scroll_margin | Scroll_margin_top -> false
  | Scroll_margin_right | Scroll_margin_bottom | Scroll_margin_left -> false
  | Scroll_margin_inline | Scroll_margin_inline_start -> false
  | Scroll_margin_inline_end | Scroll_margin_block -> false
  | Scroll_margin_block_start | Scroll_margin_block_end -> false
  | Scroll_padding | Scroll_padding_top | Scroll_padding_right -> false
  | Scroll_padding_bottom | Scroll_padding_left | Scroll_padding_inline -> false
  | Scroll_padding_inline_start | Scroll_padding_inline_end -> false
  | Scroll_padding_block | Scroll_padding_block_start -> false
  | Scroll_padding_block_end | Overscroll_behavior -> false
  | Overscroll_behavior_x | Overscroll_behavior_y -> false
  | Overscroll_behavior_block | Overscroll_behavior_inline -> false
  | Accent_color | Caret_color | Offset_anchor | Offset_position -> false
  | Offset -> false

(* WHATWG Compatibility Standard sec. 3.4.1 lists the [-webkit-] properties that
   are legacy name aliases of an unprefixed property, and CSS Cascade 5 sec. 3.1
   makes such an alias the same property under a second name. A prefix the list
   does not name is a property of its own: the [-moz-] spellings and
   [-webkit-user-select] among them. The list's [-webkit-mask-box-image] set is
   left out: no Chromium or Firefox release reads [mask-border], so across the
   targets each of those is a property of its own. An alias cascade types none
   of is read as an unknown property, so its name decides. *)
let legacy_alias_of_unknown = function
  | "-webkit-backface-visibility" -> Some (Prop Backface_visibility)
  | "-webkit-background-origin" -> Some (Prop Background_origin)
  | "-webkit-border-bottom-left-radius" -> Some (Prop Border_bottom_left_radius)
  | "-webkit-border-bottom-right-radius" ->
      Some (Prop Border_bottom_right_radius)
  | "-webkit-border-top-left-radius" -> Some (Prop Border_top_left_radius)
  | "-webkit-border-top-right-radius" -> Some (Prop Border_top_right_radius)
  | "-webkit-flex" -> Some (Prop Flex)
  | "-webkit-flex-basis" -> Some (Prop Flex_basis)
  | "-webkit-flex-grow" -> Some (Prop Flex_grow)
  | "-webkit-flex-shrink" -> Some (Prop Flex_shrink)
  | "-webkit-mask" -> Some (Prop Mask)
  | "-webkit-order" -> Some (Prop Order)
  | "-webkit-perspective" -> Some (Prop Perspective)
  | "-webkit-perspective-origin" -> Some (Prop Perspective_origin)
  | "-webkit-transform-origin" -> Some (Prop Transform_origin)
  | "-webkit-transform-style" -> Some (Prop Transform_style)
  | _ -> None

let legacy_alias : type a. a property -> any_property option = function
  | Custom_property _ -> None
  | Unknown_property name -> legacy_alias_of_unknown name
  | Webkit_align_content -> Some (Prop Align_content)
  | Webkit_align_items -> Some (Prop Align_items)
  | Webkit_align_self -> Some (Prop Align_self)
  | Webkit_animation -> Some (Prop Animation)
  | Webkit_animation_delay -> Some (Prop Animation_delay)
  | Webkit_animation_direction -> Some (Prop Animation_direction)
  | Webkit_animation_duration -> Some (Prop Animation_duration)
  | Webkit_animation_fill_mode -> Some (Prop Animation_fill_mode)
  | Webkit_animation_iteration_count -> Some (Prop Animation_iteration_count)
  | Webkit_animation_name -> Some (Prop Animation_name)
  | Webkit_animation_play_state -> Some (Prop Animation_play_state)
  | Webkit_animation_timing_function -> Some (Prop Animation_timing_function)
  | Webkit_background_clip -> Some (Prop Background_clip)
  | Webkit_background_size -> Some (Prop Background_size)
  | Webkit_border_radius -> Some (Prop Border_radius)
  | Webkit_box_shadow -> Some (Prop Box_shadow)
  | Webkit_box_sizing -> Some (Prop Box_sizing)
  | Webkit_filter -> Some (Prop Filter)
  | Webkit_flex_direction -> Some (Prop Flex_direction)
  | Webkit_flex_flow -> Some (Prop Flex_flow)
  | Webkit_flex_wrap -> Some (Prop Flex_wrap)
  | Webkit_justify_content -> Some (Prop Justify_content)
  | Webkit_mask_clip -> Some (Prop Mask_clip)
  | Webkit_mask_composite -> Some (Prop Mask_composite)
  | Webkit_mask_image -> Some (Prop Mask_image)
  | Webkit_mask_origin -> Some (Prop Mask_origin)
  | Webkit_mask_position -> Some (Prop Mask_position)
  | Webkit_mask_repeat -> Some (Prop Mask_repeat)
  | Webkit_mask_size -> Some (Prop Mask_size)
  | Webkit_transform -> Some (Prop Transform)
  | Webkit_transition -> Some (Prop Transition)
  | Webkit_transition_delay -> Some (Prop Transition_delay)
  | Webkit_transition_duration -> Some (Prop Transition_duration)
  | Webkit_transition_property -> Some (Prop Transition_property)
  | Webkit_transition_timing_function -> Some (Prop Transition_timing_function)
  | All | Background_color | Color | Border_color | Border_style -> None
  | Border_top_style | Border_right_style | Border_bottom_style -> None
  | Border_left_style | Border_inline_start_style -> None
  | Border_inline_end_style | Border_block_start_style -> None
  | Border_block_end_style | Padding | Padding_left | Padding_right -> None
  | Padding_bottom | Padding_top | Padding_inline | Padding_inline_start -> None
  | Padding_inline_end | Padding_block | Padding_block_start -> None
  | Padding_block_end | Margin | Margin_inline_end | Margin_inline_start -> None
  | Margin_left | Margin_right | Margin_top | Margin_bottom -> None
  | Margin_inline | Margin_block | Margin_block_start | Margin_block_end -> None
  | Gap | Column_gap | Row_gap | Width | Height | Min_width | Min_height -> None
  | Max_width | Max_height | Inline_size | Min_inline_size -> None
  | Max_inline_size | Block_size | Min_block_size | Max_block_size -> None
  | Font_size | Line_height | Font_weight | Font_style | Text_align -> None
  | Text_decoration | Text_decoration_line | Text_decoration_style -> None
  | Text_decoration_color | Text_underline_offset | Text_decoration_skip -> None
  | Text_decoration_skip_self | Text_decoration_skip_box -> None
  | Text_decoration_skip_inset | Text_decoration_skip_spaces -> None
  | Text_emphasis | Text_emphasis_style | Text_emphasis_color -> None
  | Text_emphasis_position | Text_emphasis_skip | Text_orientation -> None
  | Text_transform | Letter_spacing | List_style_type -> None
  | List_style_position | List_style_image | Display | Position -> None
  | Visibility | Baseline_source | Alignment_baseline | Baseline_shift -> None
  | Flex_direction | Flex_wrap | Flex_flow | Flex | Flex_grow -> None
  | Flex_shrink | Flex_basis | Order | Align_items | Justify_content -> None
  | Justify_items | Justify_self | Align_content | Align_self -> None
  | Place_content | Place_items | Place_self | Grid_template_columns -> None
  | Grid_template_rows | Grid_template_areas | Grid_template | Grid -> None
  | Grid_area | Grid_auto_flow | Grid_auto_columns | Grid_auto_rows -> None
  | Grid_column | Grid_row | Grid_column_start | Grid_column_end -> None
  | Grid_row_start | Grid_row_end | Border_width | Border_top_width -> None
  | Border_right_width | Border_bottom_width | Border_left_width -> None
  | Border_inline_start_width | Border_inline_end_width -> None
  | Border_block_start_width | Border_block_end_width -> None
  | Border_inline_width | Border_block_width | Border_image -> None
  | Border_image_source | Border_image_slice | Border_image_repeat -> None
  | Border_image_width | Border_image_outset | Border_radius -> None
  | Border_top_left_radius | Border_top_right_radius -> None
  | Border_bottom_left_radius | Border_bottom_right_radius -> None
  | Border_top_color | Border_right_color | Border_bottom_color -> None
  | Border_left_color | Border_inline_start_color -> None
  | Border_inline_end_color | Border_block_start_color -> None
  | Border_block_end_color | Border_inline_color | Border_block_color -> None
  | Border_inline_style | Border_block_style | Border_start_start_radius -> None
  | Border_start_end_radius | Border_end_start_radius -> None
  | Border_end_end_radius | Opacity | Fill_opacity | Stroke_opacity -> None
  | Stop_opacity | Flood_opacity | Mix_blend_mode | Transform -> None
  | Translate | Cursor | Interactivity | Caret_animation | Caret_shape -> None
  | Caret | Interest_delay | Interest_delay_start | Interest_delay_end -> None
  | Nav_up | Nav_right | Nav_down | Nav_left | Table_layout -> None
  | Border_collapse | Border_spacing | User_select | Pointer_events -> None
  | Overflow | Inset | Inset_inline | Inset_inline_start -> None
  | Inset_inline_end | Inset_block | Inset_block_start | Inset_block_end -> None
  | Top | Right | Bottom | Left | Z_index | Outline | Outline_style -> None
  | Outline_width | Outline_color | Outline_offset | Forced_color_adjust -> None
  | Scroll_snap_type | White_space | White_space_collapse -> None
  | Font_variant_alternates | Font_variant | Border | Border_block -> None
  | Border_block_start | Border_block_end | Border_inline -> None
  | Border_inline_start | Border_inline_end | Background | Tab_size -> None
  | Zoom | Webkit_text_size_adjust | Font_feature_settings -> None
  | Font_variation_settings | Webkit_tap_highlight_color -> None
  | Webkit_user_select | Moz_user_select | Ms_user_select -> None
  | Webkit_text_decoration | Webkit_text_decoration_color -> None
  | Webkit_text_fill_color | Webkit_text_stroke_color -> None
  | Webkit_text_stroke | Webkit_text_stroke_width | Text_indent -> None
  | List_style | Font | Source | Webkit_appearance | Moz_transform -> None
  | Ms_transform | O_transform | Moz_box_sizing | Moz_appearance -> None
  | Moz_animation | Moz_animation_delay | Moz_animation_duration -> None
  | Moz_animation_direction | Moz_animation_iteration_count -> None
  | Moz_animation_name | Moz_animation_timing_function -> None
  | Moz_animation_fill_mode | Moz_animation_play_state | Moz_transition -> None
  | Moz_transition_delay | Moz_transition_duration -> None
  | Moz_transition_property | Moz_transition_timing_function -> None
  | Moz_border_radius | Moz_box_shadow | Ms_filter | O_transition -> None
  | Container_type | Container_name | Container | Anchor_name -> None
  | Position_anchor | Position_try_fallbacks | Position_try_order -> None
  | Position_try | Position_visibility | Position_area | Shape_outside -> None
  | Shape_margin | Shape_image_threshold | Overflow_clip_margin -> None
  | Overflow_anchor | Scrollbar_width | Scrollbar_color -> None
  | Scrollbar_gutter | Line_height_step | Font_palette | Font_synthesis -> None
  | Text_wrap_mode | Text_wrap_style | Text_box_trim -> None
  | Text_underline_position | Text_box_edge | Text_box | Inline_sizing -> None
  | Line_fit_edge | Interpolate_size | Min_intrinsic_sizing | Ruby_align -> None
  | Ruby_merge | Ruby_overhang | Ruby_position -> None
  | Glyph_orientation_vertical | Text_combine_upright -> None
  | Animation_timeline | Animation_range | Animation_range_start -> None
  | Animation_range_end | Scroll_timeline | Scroll_timeline_name -> None
  | Scroll_timeline_axis | View_transition_name | View_transition_class -> None
  | Image_orientation | Image_rendering | Image_resolution -> None
  | Contain_intrinsic_size | Contain_intrinsic_width -> None
  | Contain_intrinsic_height | Contain_intrinsic_block_size -> None
  | Contain_intrinsic_inline_size | Margin_trim | Offset_path -> None
  | Offset_distance | Offset_rotate | Font_size_adjust -> None
  | Font_variant_emoji | Text_spacing_trim | Hyphenate_limit_chars -> None
  | Initial_letter | Initial_letter_align | Initial_letter_wrap -> None
  | Dominant_baseline | View_timeline_name | View_timeline_axis -> None
  | View_timeline_inset | View_timeline | Timeline_scope | Perspective -> None
  | Perspective_origin | Transform_style | Backface_visibility -> None
  | Object_position | Rotate | Transition_duration -> None
  | Transition_timing_function | Transition_delay | Transition_property -> None
  | Transition_behavior | Overlay | Will_change | Contain | Isolation -> None
  | Break_before | Break_after | Break_inside | Page_break_before -> None
  | Page_break_after | Page_break_inside | Page_size | Columns -> None
  | Column_width | Column_height | Column_wrap | Column_count -> None
  | Column_rule | Column_rule_color | Column_rule_width -> None
  | Column_rule_style | Column_span | Word_spacing -> None
  | Background_attachment | Border_top | Border_right | Border_bottom -> None
  | Border_left | Transform_origin | Transform_box | Text_shadow -> None
  | Clip_path | Mask | Mask_border | Content_visibility | Filter -> None
  | Background_image | Background_origin | Background_clip | Animation -> None
  | Aspect_ratio | Overflow_x | Overflow_y | Overflow_block -> None
  | Overflow_inline | Vertical_align | Font_family | Background_position -> None
  | Background_position_x | Background_position_y -> None
  | Webkit_mask_position_x | Webkit_mask_position_y | Background_repeat -> None
  | Background_size | Webkit_font_smoothing | Moz_osx_font_smoothing -> None
  | Webkit_line_clamp | Webkit_box_orient | Moz_orient | Text_overflow -> None
  | Text_wrap | Word_break | Overflow_wrap | Line_break | Hyphens -> None
  | Webkit_hyphens | Text_rendering | Webkit_touch_callout -> None
  | Font_stretch | Font_optical_sizing | Font_kerning -> None
  | Font_language_override | Font_synthesis_style -> None
  | Font_synthesis_weight | Font_synthesis_small_caps -> None
  | Font_synthesis_position | Font_variant_ligatures | Caps | Numeric -> None
  | Font_variant_position | East_asian | Backdrop_filter -> None
  | Webkit_backdrop_filter | Webkit_mask_source_type | Mask_image -> None
  | Mask_composite | Mask_mode | Mask_size | Mask_position | Mask_repeat -> None
  | Mask_clip | Mask_origin | Mask_type | Scroll_snap_align -> None
  | Scroll_snap_stop | Scroll_behavior | Box_sizing | Field_sizing -> None
  | Caption_side | Resize | Object_fit | Object_view_box | Appearance -> None
  | Color_scheme | Print_color_adjust | Webkit_print_color_adjust -> None
  | Box_decoration_break | Webkit_box_decoration_break | Content -> None
  | Counter_reset | Counter_increment | Quotes -> None
  | Text_decoration_thickness | Text_size_adjust | Touch_action | Clip -> None
  | Clear | Float | Scale | Transition | Box_shadow | Fill | Stroke -> None
  | Stroke_width | Fill_rule | Clip_rule | Stroke_linecap -> None
  | Stroke_linejoin | Stroke_miterlimit | Stroke_dashoffset -> None
  | Stroke_dasharray | Paint_order | Vector_effect | Stop_color -> None
  | Flood_color | Lighting_color | Direction | Unicode_bidi -> None
  | Writing_mode | Text_decoration_skip_ink | Animation_name -> None
  | Animation_duration | Animation_timing_function | Animation_delay -> None
  | Animation_iteration_count | Animation_direction -> None
  | Animation_fill_mode | Animation_play_state | Animation_composition -> None
  | Background_blend_mode | Scroll_margin | Scroll_margin_top -> None
  | Scroll_margin_right | Scroll_margin_bottom | Scroll_margin_left -> None
  | Scroll_margin_inline | Scroll_margin_inline_start -> None
  | Scroll_margin_inline_end | Scroll_margin_block -> None
  | Scroll_margin_block_start | Scroll_margin_block_end | Scroll_padding -> None
  | Scroll_padding_top | Scroll_padding_right | Scroll_padding_bottom -> None
  | Scroll_padding_left | Scroll_padding_inline -> None
  | Scroll_padding_inline_start | Scroll_padding_inline_end -> None
  | Scroll_padding_block | Scroll_padding_block_start -> None
  | Scroll_padding_block_end | Overscroll_behavior -> None
  | Overscroll_behavior_x | Overscroll_behavior_y -> None
  | Overscroll_behavior_block | Overscroll_behavior_inline -> None
  | Accent_color | Caret_color | Offset_anchor | Offset_position -> None
  | Offset -> None

(* Cascade detected the value is spec-invalid (an [Invalid] arm in one of the
   typed value types). [Optimize.drop_invalid], which every serialisation runs,
   uses this to discard the declaration. *)
let invalid_angle : angle -> bool = function Invalid _ -> true | _ -> false

let invalid_length_percentage : length_percentage -> bool = function
  | Invalid _ -> true
  | _ -> false

let invalid_rotate_value : rotate_value -> bool = function
  | Angle a | X a | Y a | Z a | Axis (_, _, _, a) -> invalid_angle a
  | _ -> false

let invalid_clip_path : clip_path -> bool = function
  | Invalid _ -> true
  | _ -> false

let invalid_text_indent_value : text_indent_value -> bool = function
  | Indent { length; _ } -> invalid_length_percentage length
  | _ -> false

let is_invalid_value : type a. a property -> a -> bool =
 fun property value ->
  match property_class property with
  | Inherited -> false
  | Non_inherited -> false
  | Checks_length_percentage -> invalid_length_percentage value
  | Checks_rotate -> invalid_rotate_value value
  | Checks_clip_path -> invalid_clip_path value
  | Checks_text_indent -> invalid_text_indent_value value
  | Checks_font_family -> ( match value with Invalid _ -> true | _ -> false)

let property_value_kind : type a. a property -> a property_value_kind option =
  function
  | Padding_left -> Some Length
  | Padding_right -> Some Length
  | Padding_bottom -> Some Length
  | Padding_top -> Some Length
  | Padding_inline -> Some Lengths
  | Padding_inline_start -> Some Length
  | Padding_inline_end -> Some Length
  | Padding_block -> Some Lengths
  | Padding_block_start -> Some Length
  | Padding_block_end -> Some Length
  | Margin_inline_end -> Some Length
  | Margin_inline_start -> Some Length
  | Margin_left -> Some Length
  | Margin_right -> Some Length
  | Margin_top -> Some Length
  | Margin_bottom -> Some Length
  | Margin_block_start -> Some Length
  | Margin_block_end -> Some Length
  | Column_gap -> Some Length
  | Row_gap -> Some Length
  | Text_underline_offset -> Some Length
  | Letter_spacing -> Some Length
  | Border_top_left_radius -> Some Lengths
  | Border_top_right_radius -> Some Lengths
  | Border_bottom_left_radius -> Some Lengths
  | Border_bottom_right_radius -> Some Lengths
  | Border_start_start_radius -> Some Lengths
  | Border_start_end_radius -> Some Lengths
  | Border_end_start_radius -> Some Lengths
  | Border_end_end_radius -> Some Lengths
  | Outline_width -> Some Border_width
  | Border_top_width -> Some Border_width
  | Border_right_width -> Some Border_width
  | Border_bottom_width -> Some Border_width
  | Border_left_width -> Some Border_width
  | Border_inline_start_width -> Some Border_width
  | Border_inline_end_width -> Some Border_width
  | Border_block_start_width -> Some Border_width
  | Border_block_end_width -> Some Border_width
  | Outline_offset -> Some Length
  | Text_indent -> None
  | Line_height_step -> Some Length
  | Perspective -> Some Length
  | Text_decoration_thickness -> Some Length
  | Stroke_width -> Some Stroke_width
  | Scroll_margin_top -> Some Length
  | Scroll_margin_right -> Some Length
  | Scroll_margin_bottom -> Some Length
  | Scroll_margin_left -> Some Length
  | Scroll_margin_inline_start -> Some Length
  | Scroll_margin_inline_end -> Some Length
  | Scroll_margin_block_start -> Some Length
  | Scroll_margin_block_end -> Some Length
  | Scroll_padding_top -> Some Length
  | Scroll_padding_right -> Some Length
  | Scroll_padding_bottom -> Some Length
  | Scroll_padding_left -> Some Length
  | Scroll_padding_inline_start -> Some Length
  | Scroll_padding_inline_end -> Some Length
  | Scroll_padding_block_start -> Some Length
  | Scroll_padding_block_end -> Some Length
  | Padding -> Some Lengths
  | Margin -> Some Lengths
  | Margin_inline -> Some Lengths
  | Margin_block -> Some Lengths
  | Inset -> Some Lengths
  | Inset_inline -> Some Lengths
  | Inset_inline_start -> Some Lengths
  | Inset_inline_end -> Some Lengths
  | Inset_block -> Some Lengths
  | Inset_block_start -> Some Lengths
  | Inset_block_end -> Some Lengths
  | Top -> Some Lengths
  | Right -> Some Lengths
  | Bottom -> Some Lengths
  | Left -> Some Lengths
  | Border_width -> Some Border_widths
  | Scroll_margin -> Some Lengths
  | Scroll_margin_inline -> Some Lengths
  | Scroll_margin_block -> Some Lengths
  | Scroll_padding -> Some Lengths
  | Scroll_padding_inline -> Some Lengths
  | Scroll_padding_block -> Some Lengths
  | Width -> Some Length_percentage
  | Height -> Some Length_percentage
  | Min_width -> Some Length_percentage
  | Min_height -> Some Length_percentage
  | Max_width -> Some Length_percentage
  | Max_height -> Some Length_percentage
  | Inline_size -> Some Length_percentage
  | Min_inline_size -> Some Length_percentage
  | Max_inline_size -> Some Length_percentage
  | Block_size -> Some Length_percentage
  | Min_block_size -> Some Length_percentage
  | Max_block_size -> Some Length_percentage
  | Shape_margin -> Some Length_percentage
  | Font_size -> Some Font_size
  | Opacity -> Some Opacity
  | Fill_opacity -> Some Opacity
  | Stroke_opacity -> Some Opacity
  | Stop_opacity -> Some Opacity
  | Flood_opacity -> Some Opacity
  | Rotate -> Some Rotate
  | Animation_duration -> Some Duration
  | Animation_delay -> Some Duration
  | Webkit_animation_duration -> Some Duration
  | Webkit_animation_delay -> Some Duration
  | Moz_animation_duration -> Some Duration
  | Moz_animation_delay -> Some Duration
  | Transition_duration -> Some Duration
  | Transition_delay -> Some Duration
  | Webkit_transition_duration -> Some Duration
  | Webkit_transition_delay -> Some Duration
  | Moz_transition_duration -> Some Duration
  | Moz_transition_delay -> Some Duration
  | Display -> Some Display
  | Position -> Some Position
  | Visibility -> Some Visibility
  | Clear -> Some Clear
  | Float -> Some Float
  | Scale -> Some Scale
  | Translate -> Some Translate
  | Transform -> Some Transform
  | Webkit_transform -> Some Transform
  | Animation -> Some (Animation : animation list property_value_kind)
  | Webkit_animation -> Some (Animation : animation list property_value_kind)
  | Moz_animation -> Some (Animation : animation list property_value_kind)
  | Transition -> Some (Transition : transition list property_value_kind)
  | Webkit_transition -> Some (Transition : transition list property_value_kind)
  | Moz_transition -> Some (Transition : transition list property_value_kind)
  | O_transition -> Some (Transition : transition list property_value_kind)
  | Filter -> Some Filter
  | Backdrop_filter -> Some Filter
  | Webkit_backdrop_filter -> Some Filter
  | Webkit_filter -> Some Filter
  | Ms_filter -> Some Filter
  | Box_shadow -> Some Shadow
  | Webkit_box_shadow -> Some Shadow
  | Moz_box_shadow -> Some Shadow
  | Border_radius -> Some Border_radius
  | Webkit_border_radius -> Some Border_radius
  | Moz_border_radius -> Some Border_radius
  | Offset_distance -> Some Length_percentage
  | Background_color -> Some Color
  | Animation_name -> Some Animation_name
  | Webkit_animation_name -> Some Animation_name
  | Moz_animation_name -> Some Animation_name
  | Color -> Some Color
  | Border_color -> Some Colors
  | Text_decoration_color -> Some Color
  | Border_top_color -> Some Color
  | Border_right_color -> Some Color
  | Border_bottom_color -> Some Color
  | Border_left_color -> Some Color
  | Outline_color -> Some Color
  | Webkit_tap_highlight_color -> Some Color
  | Webkit_text_decoration_color -> Some Color
  | Webkit_text_fill_color -> Some Color
  | Webkit_text_stroke_color -> Some Color
  | Webkit_text_stroke_width -> Some Border_width
  | Column_rule_color -> Some Colors
  | Column_rule_width -> Some Border_widths
  | Accent_color -> Some Color
  | Caret_color -> Some Color
  | Stop_color -> Some Color
  | Flood_color -> Some Color
  | Lighting_color -> Some Color
  | Background_image -> Some Background_images
  | Background -> Some Background
  | Webkit_mask_image -> Some Background_image
  | Border_image_source -> Some Background_image
  | Mask_image -> Some Background_image
  | Source -> Some Font_src
  | Font_family -> Some Font_family
  | _ -> None

(* ===== Readers moved here from Declaration so the API consistency script can
   surface them in [properties.mli]. ===== *)

let pp_property_value_kind : type a. a property_value_kind Pp.t =
 fun ctx -> function
  | Length -> Pp.string ctx "length"
  | Lengths -> Pp.string ctx "lengths"
  | Length_percentage -> Pp.string ctx "length-percentage"
  | Border_width -> Pp.string ctx "border-width"
  | Border_widths -> Pp.string ctx "border-widths"
  | Opacity -> Pp.string ctx "opacity"
  | Rotate -> Pp.string ctx "rotate"
  | Duration -> Pp.string ctx "duration"
  | Number_percentage -> Pp.string ctx "number-percentage"
  | Font_size -> Pp.string ctx "font-size"
  | Display -> Pp.string ctx "display"
  | Position -> Pp.string ctx "position"
  | Visibility -> Pp.string ctx "visibility"
  | Clear -> Pp.string ctx "clear"
  | Float -> Pp.string ctx "float"
  | Scale -> Pp.string ctx "scale"
  | Translate -> Pp.string ctx "translate"
  | Transform -> Pp.string ctx "transform"
  | Animation -> Pp.string ctx "animation"
  | Transition -> Pp.string ctx "transition"
  | Filter -> Pp.string ctx "filter"
  | Shadow -> Pp.string ctx "shadow"
  | Border_radius -> Pp.string ctx "border-radius"
  | Color -> Pp.string ctx "color"
  | Colors -> Pp.string ctx "colors"
  | Animation_name -> Pp.string ctx "animation-name"
  | Background -> Pp.string ctx "background"
  | Background_image -> Pp.string ctx "background-image"
  | Background_images -> Pp.string ctx "background-images"
  | Font_src -> Pp.string ctx "font-src"
  | Font_family -> Pp.string ctx "font-family"
  | Stroke_width -> Pp.string ctx "stroke-width"

let read_property_value_kind (type a) (_ : Cursor.t) : a property_value_kind =
  invalid_arg
    "Properties.read_property_value_kind: property_value_kind is a phantom \
     GADT and cannot be parsed standalone"
