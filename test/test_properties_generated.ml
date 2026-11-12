(** Property tests: generated spec vectors, one property group per test. *)

open Alcotest
open Cascade
open Css.Properties
open Css_test_helpers
open Properties_helpers

let check_alignment_baseline =
  check_value_cursor "alignment_baseline" read_alignment_baseline
    pp_alignment_baseline

let check_anchor_name =
  check_value_cursor "anchor_name" read_anchor_name pp_anchor_name

let check_animation_composition =
  check_value_cursor "animation_composition" read_animation_composition
    pp_animation_composition

let check_animation_composition_item =
  check_value_cursor "animation_composition_item"
    read_animation_composition_item pp_animation_composition_item

let check_animation_name =
  check_value_cursor "animation_name" read_animation_name pp_animation_name

let check_animation_range =
  check_value_cursor "animation_range" read_animation_range pp_animation_range

let check_animation_range_item =
  check_value_cursor "animation_range_item" read_animation_range_item
    pp_animation_range_item

let check_animation_range_name =
  check_value_cursor "animation_range_name" read_animation_range_name
    pp_animation_range_name

let check_animation_timeline =
  check_value_cursor "animation_timeline" read_animation_timeline
    pp_animation_timeline

let check_baseline_shift =
  check_value_cursor "baseline_shift" read_baseline_shift pp_baseline_shift

let check_baseline_source =
  check_value_cursor "baseline_source" read_baseline_source pp_baseline_source

let check_border_image =
  check_value_cursor "border_image" read_border_image pp_border_image

let check_border_image_outset_item =
  check_value_cursor "border_image_outset_item" read_border_image_outset_item
    pp_border_image_outset_item

let check_border_image_repeat_keyword =
  check_value_cursor "border_image_repeat_keyword"
    read_border_image_repeat_keyword pp_border_image_repeat_keyword

let check_border_image_slice =
  check_value_cursor "border_image_slice" read_border_image_slice
    pp_border_image_slice

let check_border_image_slice_offsets =
  check_value_cursor "border_image_slice_offsets"
    read_border_image_slice_offsets pp_border_image_slice_offsets

let check_border_image_slice_item =
  check_value_cursor "border_image_slice_item" read_border_image_slice_item
    pp_border_image_slice_item

let check_border_image_width_item =
  check_value_cursor "border_image_width_item" read_border_image_width_item
    pp_border_image_width_item

let check_border_spacing =
  check_value_cursor "border_spacing" read_border_spacing pp_border_spacing

let check_caret = check_value_cursor "caret" read_caret pp_caret

let check_caret_animation =
  check_value_cursor "caret_animation" read_caret_animation pp_caret_animation

let check_caret_shape =
  check_value_cursor "caret_shape" read_caret_shape pp_caret_shape

let check_column_span =
  check_value_cursor "column_span" read_column_span pp_column_span

let check_contain_intrinsic_longhand =
  check_value_cursor "contain_intrinsic_longhand"
    read_contain_intrinsic_longhand pp_contain_intrinsic_longhand

let check_contain_intrinsic_size =
  check_value_cursor "contain_intrinsic_size" read_contain_intrinsic_size
    pp_contain_intrinsic_size

let check_contain_intrinsic_size_item =
  check_value_cursor "contain_intrinsic_size_item"
    read_contain_intrinsic_size_item pp_contain_intrinsic_size_item

let check_container_name =
  check_value_cursor "container_name" read_container_name pp_container_name

let check_counter_item =
  check_value_cursor "counter_item" read_counter_item pp_counter_item

let check_counter_set =
  check_value_cursor "counter_set" read_counter_set pp_counter_set

let check_dominant_baseline =
  check_value_cursor "dominant_baseline" read_dominant_baseline
    pp_dominant_baseline

let check_flex_factor =
  check_value_cursor "flex_factor" read_flex_factor pp_flex_factor

let check_flex_flow = check_value_cursor "flex_flow" read_flex_flow pp_flex_flow

let check_font_kerning =
  check_value_cursor "font_kerning" read_font_kerning pp_font_kerning

let check_font_language_override =
  check_value_cursor "font_language_override" read_font_language_override
    pp_font_language_override

let check_font_optical_sizing =
  check_value_cursor "font_optical_sizing" read_font_optical_sizing
    pp_font_optical_sizing

let check_font_palette =
  check_value_cursor "font_palette" read_font_palette pp_font_palette

let check_font_size_adjust =
  check_value_cursor "font_size_adjust" read_font_size_adjust
    pp_font_size_adjust

let check_font_size_adjust_metric =
  check_value_cursor "font_size_adjust_metric" read_font_size_adjust_metric
    pp_font_size_adjust_metric

let check_font_synthesis =
  check_value_cursor "font_synthesis" read_font_synthesis pp_font_synthesis

let check_font_synthesis_feature =
  check_value_cursor "font_synthesis_feature" read_font_synthesis_feature
    pp_font_synthesis_feature

let check_font_synthesis_position =
  check_value_cursor "font_synthesis_position" read_font_synthesis_position
    pp_font_synthesis_position

let check_font_synthesis_small_caps =
  check_value_cursor "font_synthesis_small_caps" read_font_synthesis_small_caps
    pp_font_synthesis_small_caps

let check_font_synthesis_style =
  check_value_cursor "font_synthesis_style" read_font_synthesis_style
    pp_font_synthesis_style

let check_font_synthesis_weight =
  check_value_cursor "font_synthesis_weight" read_font_synthesis_weight
    pp_font_synthesis_weight

let check_font_variant =
  check_value_cursor "font_variant" read_font_variant pp_font_variant

let check_font_variant_alternates =
  check_value_cursor "font_variant_alternates" read_font_variant_alternates
    pp_font_variant_alternates

let check_white_space_collapse =
  check_value_cursor "white_space_collapse" read_white_space_collapse
    pp_white_space_collapse

let check_column_height =
  check_value_cursor "column_height" read_column_height pp_column_height

let check_column_wrap =
  check_value_cursor "column_wrap" read_column_wrap pp_column_wrap

let check_webkit_text_stroke =
  check_value_cursor "webkit_text_stroke" read_webkit_text_stroke
    pp_webkit_text_stroke

let check_font_variant_caps =
  check_value_cursor "font_variant_caps" read_font_variant_caps
    pp_font_variant_caps

let check_font_variant_east_asian =
  check_value_cursor "font_variant_east_asian" read_font_variant_east_asian
    pp_font_variant_east_asian

let check_font_variant_emoji =
  check_value_cursor "font_variant_emoji" read_font_variant_emoji
    pp_font_variant_emoji

let check_font_variant_ligature =
  check_value_cursor "font_variant_ligature" read_font_variant_ligature
    pp_font_variant_ligature

let check_font_variant_ligatures =
  check_value_cursor "font_variant_ligatures" read_font_variant_ligatures
    pp_font_variant_ligatures

let check_font_variant_position =
  check_value_cursor "font_variant_position" read_font_variant_position
    pp_font_variant_position

let check_glyph_orientation_vertical =
  check_value_cursor "glyph_orientation_vertical"
    read_glyph_orientation_vertical pp_glyph_orientation_vertical

let check_hyphenate_limit_chars_item =
  check_value_cursor "hyphenate_limit_chars_item"
    read_hyphenate_limit_chars_item pp_hyphenate_limit_chars_item

let check_hyphenate_limit_chars =
  check_value_cursor "hyphenate_limit_chars" read_hyphenate_limit_chars
    pp_hyphenate_limit_chars

let check_image_orientation =
  check_value_cursor "image_orientation" read_image_orientation
    pp_image_orientation

let check_image_rendering =
  check_value_cursor "image_rendering" read_image_rendering pp_image_rendering

let check_image_resolution =
  check_value_cursor "image_resolution" read_image_resolution
    pp_image_resolution

let check_initial_letter =
  check_value_cursor "initial_letter" read_initial_letter pp_initial_letter

let check_initial_letter_align =
  check_value_cursor "initial_letter_align" read_initial_letter_align
    pp_initial_letter_align

let check_initial_letter_align_keyword =
  check_value_cursor "initial_letter_align_keyword"
    read_initial_letter_align_keyword pp_initial_letter_align_keyword

let check_initial_letter_wrap =
  check_value_cursor "initial_letter_wrap" read_initial_letter_wrap
    pp_initial_letter_wrap

let check_inline_sizing =
  check_value_cursor "inline_sizing" read_inline_sizing pp_inline_sizing

let check_interactivity =
  check_value_cursor "interactivity" read_interactivity pp_interactivity

let check_interest_delay =
  check_value_cursor "interest_delay" read_interest_delay pp_interest_delay

let check_interest_delay_item =
  check_value_cursor "interest_delay_item" read_interest_delay_item
    pp_interest_delay_item

let check_interpolate_size =
  check_value_cursor "interpolate_size" read_interpolate_size
    pp_interpolate_size

let check_line_break =
  check_value_cursor "line_break" read_line_break pp_line_break

let check_line_fit_edge =
  check_value_cursor "line_fit_edge" read_line_fit_edge pp_line_fit_edge

let check_line_fit_edge_keyword =
  check_value_cursor "line_fit_edge_keyword" read_line_fit_edge_keyword
    pp_line_fit_edge_keyword

let check_logical_border_color =
  check_value_cursor "logical_border_color" read_logical_border_color
    pp_logical_border_color

let check_logical_border_style =
  check_value_cursor "logical_border_style" read_logical_border_style
    pp_logical_border_style

let check_logical_border_width =
  check_value_cursor "logical_border_width" read_logical_border_width
    pp_logical_border_width

let check_margin_trim =
  check_value_cursor "margin_trim" read_margin_trim pp_margin_trim

let check_margin_trim_axis =
  check_value_cursor "margin_trim_axis" read_margin_trim_axis
    pp_margin_trim_axis

let check_margin_trim_edge =
  check_value_cursor "margin_trim_edge" read_margin_trim_edge
    pp_margin_trim_edge

let check_mask = check_value_cursor "mask" read_mask pp_mask

let check_mask_layer =
  check_value_cursor "mask_layer" read_mask_layer pp_mask_layer

let check_min_intrinsic_sizing =
  check_value_cursor "min_intrinsic_sizing" read_min_intrinsic_sizing
    pp_min_intrinsic_sizing

let check_min_intrinsic_sizing_keyword =
  check_value_cursor "min_intrinsic_sizing_keyword"
    read_min_intrinsic_sizing_keyword pp_min_intrinsic_sizing_keyword

let check_nav = check_value_cursor "nav" read_nav pp_nav
let check_nav_scope = check_value_cursor "nav_scope" read_nav_scope pp_nav_scope

let check_object_view_box =
  check_value_cursor "object_view_box" read_object_view_box pp_object_view_box

let check_offset_path =
  check_value_cursor "offset_path" read_offset_path pp_offset_path

let check_offset_rotate =
  check_value_cursor "offset_rotate" read_offset_rotate pp_offset_rotate

let check_offset_rotate_mode =
  check_value_cursor "offset_rotate_mode" read_offset_rotate_mode
    pp_offset_rotate_mode

let check_overflow_anchor =
  check_value_cursor "overflow_anchor" read_overflow_anchor pp_overflow_anchor

let check_overflow_clip_box =
  check_value_cursor "overflow_clip_box" read_overflow_clip_box
    pp_overflow_clip_box

let check_overflow_clip_margin =
  check_value_cursor "overflow_clip_margin" read_overflow_clip_margin
    pp_overflow_clip_margin

let check_overlay = check_value_cursor "overlay" read_overlay pp_overlay

let check_position_anchor =
  check_value_cursor "position_anchor" read_position_anchor pp_position_anchor

let check_position_area =
  check_value_cursor "position_area" read_position_area pp_position_area

let check_position_area_keyword =
  check_value_cursor "position_area_keyword" read_position_area_keyword
    pp_position_area_keyword

let check_position_try_fallback =
  check_value_cursor "position_try_fallback" read_position_try_fallback
    pp_position_try_fallback

let check_position_try_fallback_entry =
  check_value_cursor "position_try_fallback_entry"
    read_position_try_fallback_entry pp_position_try_fallback_entry

let check_position_try_fallbacks =
  check_value_cursor "position_try_fallbacks" read_position_try_fallbacks
    pp_position_try_fallbacks

let check_position_try_order =
  check_value_cursor "position_try_order" read_position_try_order
    pp_position_try_order

let check_position_visibility =
  check_value_cursor "position_visibility" read_position_visibility
    pp_position_visibility

let check_position_visibility_condition =
  check_value_cursor "position_visibility_condition"
    read_position_visibility_condition pp_position_visibility_condition

let check_ray = check_value_cursor "ray" read_ray pp_ray

let check_resolution =
  check_value_cursor "resolution" read_resolution pp_resolution

let check_ruby_align =
  check_value_cursor "ruby_align" read_ruby_align pp_ruby_align

let check_ruby_merge =
  check_value_cursor "ruby_merge" read_ruby_merge pp_ruby_merge

let check_ruby_overhang =
  check_value_cursor "ruby_overhang" read_ruby_overhang pp_ruby_overhang

let check_ruby_position =
  check_value_cursor "ruby_position" read_ruby_position pp_ruby_position

let check_ruby_position_keyword =
  check_value_cursor "ruby_position_keyword" read_ruby_position_keyword
    pp_ruby_position_keyword

let check_scrollbar_color =
  check_value_cursor "scrollbar_color" read_scrollbar_color pp_scrollbar_color

let check_scrollbar_gutter =
  check_value_cursor "scrollbar_gutter" read_scrollbar_gutter
    pp_scrollbar_gutter

let check_scrollbar_width =
  check_value_cursor "scrollbar_width" read_scrollbar_width pp_scrollbar_width

let check_shape_image_threshold =
  check_value_cursor "shape_image_threshold" read_shape_image_threshold
    pp_shape_image_threshold

let check_text_box = check_value_cursor "text_box" read_text_box pp_text_box

let check_text_box_edge =
  check_value_cursor "text_box_edge" read_text_box_edge pp_text_box_edge

let check_text_box_edge_keyword =
  check_value_cursor "text_box_edge_keyword" read_text_box_edge_keyword
    pp_text_box_edge_keyword

let check_text_box_trim =
  check_value_cursor "text_box_trim" read_text_box_trim pp_text_box_trim

let check_text_combine_upright =
  check_value_cursor "text_combine_upright" read_text_combine_upright
    pp_text_combine_upright

let check_text_decoration_skip =
  check_value_cursor "text_decoration_skip" read_text_decoration_skip
    pp_text_decoration_skip

let check_text_decoration_skip_box =
  check_value_cursor "text_decoration_skip_box" read_text_decoration_skip_box
    pp_text_decoration_skip_box

let check_text_decoration_skip_inset =
  check_value_cursor "text_decoration_skip_inset"
    read_text_decoration_skip_inset pp_text_decoration_skip_inset

let check_text_decoration_skip_self =
  check_value_cursor "text_decoration_skip_self" read_text_decoration_skip_self
    pp_text_decoration_skip_self

let check_text_decoration_skip_space =
  check_value_cursor "text_decoration_skip_space"
    read_text_decoration_skip_space pp_text_decoration_skip_space

let check_text_decoration_skip_spaces =
  check_value_cursor "text_decoration_skip_spaces"
    read_text_decoration_skip_spaces pp_text_decoration_skip_spaces

let check_text_emphasis =
  check_value_cursor "text_emphasis" read_text_emphasis pp_text_emphasis

let check_text_emphasis_fill =
  check_value_cursor "text_emphasis_fill" read_text_emphasis_fill
    pp_text_emphasis_fill

let check_text_emphasis_line =
  check_value_cursor "text_emphasis_line" read_text_emphasis_line
    pp_text_emphasis_line

let check_text_emphasis_position =
  check_value_cursor "text_emphasis_position" read_text_emphasis_position
    pp_text_emphasis_position

let check_text_emphasis_shape =
  check_value_cursor "text_emphasis_shape" read_text_emphasis_shape
    pp_text_emphasis_shape

let check_text_emphasis_side =
  check_value_cursor "text_emphasis_side" read_text_emphasis_side
    pp_text_emphasis_side

let check_text_emphasis_skip =
  check_value_cursor "text_emphasis_skip" read_text_emphasis_skip
    pp_text_emphasis_skip

let check_text_emphasis_skip_keyword =
  check_value_cursor "text_emphasis_skip_keyword"
    read_text_emphasis_skip_keyword pp_text_emphasis_skip_keyword

let check_text_emphasis_style =
  check_value_cursor "text_emphasis_style" read_text_emphasis_style
    pp_text_emphasis_style

let check_text_orientation =
  check_value_cursor "text_orientation" read_text_orientation
    pp_text_orientation

let check_text_spacing_trim =
  check_value_cursor "text_spacing_trim" read_text_spacing_trim
    pp_text_spacing_trim

let check_text_underline_position =
  check_value_cursor "text_underline_position" read_text_underline_position
    pp_text_underline_position

let check_text_underline_position_keyword =
  check_value_cursor "text_underline_position_keyword"
    read_text_underline_position_keyword pp_text_underline_position_keyword

let check_text_wrap_mode =
  check_value_cursor "text_wrap_mode" read_text_wrap_mode pp_text_wrap_mode

let check_text_wrap_style =
  check_value_cursor "text_wrap_style" read_text_wrap_style pp_text_wrap_style

let check_timeline_inset =
  check_value_cursor "timeline_inset" read_timeline_inset pp_timeline_inset

let check_timeline_inset_item =
  check_value_cursor "timeline_inset_item" read_timeline_inset_item
    pp_timeline_inset_item

let check_timeline_ident =
  check_value_cursor "timeline_ident" read_timeline_ident pp_timeline_ident

let check_timeline_name =
  check_value_cursor "name" read_timeline_name pp_timeline_name

let check_timeline_scope =
  check_value_cursor "timeline_scope" read_timeline_scope pp_timeline_scope

let check_timeline_shorthand_item =
  check_value_cursor "timeline_shorthand_item" read_timeline_shorthand_item
    pp_timeline_shorthand_item

let check_view_transition_class =
  check_value_cursor "view_transition_class" read_view_transition_class
    pp_view_transition_class

let check_view_transition_name =
  check_value_cursor "view_transition_name" read_view_transition_name
    pp_view_transition_name

(* Length-percentage tests for width/height using the value reader/printer *)

(* ignore-test: grouped generated-surface vectors. Each row is still a real
   positive/negative spec assertion; the grouping keeps this file navigable. *)
let spec_generated_animation_font_edges () =
  check_animation_composition ~expected:"replace,add" "replace, add";
  check_animation_composition_item "accumulate";
  check_animation_name ~expected:"fade,slide" "fade, slide";
  check_animation_range ~expected:"entry 0%exit 100%" "entry 0% exit 100%";
  check_animation_range "entry";
  (* Scroll-driven Animations 1 secs. 3.1 to 3.3 put a [#] on both ends and on
     the shorthand, one entry per animation. *)
  check_animation_range "normal,normal";
  check_animation_range "cover,contain";
  check_animation_range ~expected:"entry 10%exit 90%,cover"
    "entry 10% exit 90%, cover";
  check_animation_range_item ~expected:"10%,normal" "10%, normal";
  check_animation_range_item ~expected:"cover,contain" "cover, contain";
  (* Scroll Animations 1 sec. 5.1 names [normal], a length-percentage and a
     range name, so the intrinsic-sizing keywords a bare length would accept are
     out. *)
  neg_cursor read_animation_range "auto";
  neg_cursor read_animation_range_item "auto";
  (* the <length-percentage> offset takes a held (non-reducible) calc(). *)
  check_animation_range_item "entry calc(50% + 10px)";
  check_animation_range_item "cover 20%";
  check_animation_range_item "entry";
  check_animation_range_name "entry-crossing";
  check_animation_timeline ~expected:"scroll(root)" "scroll(root block)";
  check_font_kerning "normal";
  check_font_language_override "\"TRK\"";
  check_font_optical_sizing "auto";
  check_font_palette "--brand";
  check_font_size_adjust ~expected:"cap-height .5" "cap-height 0.5";
  check_font_size_adjust_metric "ic-height";
  check_font_synthesis "weight style";
  check_font_synthesis_feature "small-caps";
  check_font_synthesis_position "none";
  check_font_synthesis_small_caps "auto";
  check_font_synthesis_style "oblique-only";
  check_font_synthesis_weight "auto";
  check_font_variant "small-caps";
  check_font_variant "common-ligatures small-caps tabular-nums";
  check_font_variant_alternates "swash(fancy)";
  check_font_variant_alternates "styleset(a,b) historical-forms";
  check_white_space_collapse "preserve-breaks";
  check_column_height "10em";
  check_column_wrap "wrap";
  check_webkit_text_stroke "1px red";
  check_font_variant_caps "small-caps";
  check_font_variant_east_asian "jis78 ruby";
  check_east_asian_feature "jis04";
  check_font_variant_emoji "unicode";
  check_font_variant_ligature "common-ligatures";
  check_font_variant_ligatures "common-ligatures contextual";
  check_font_variant_position "super";
  check_glyph_orientation_vertical "90deg";
  check_image_orientation "from-image";
  check_image_rendering "pixelated";
  check_image_resolution "from-image 2dppx snap";
  check_resolution "2dppx";
  neg_cursor ~allow_partial:true read_animation_composition "replace blend";
  neg_cursor read_animation_composition_item "blend";
  neg_cursor read_animation_name "1fade";
  neg_cursor read_animation_range_name "enter";
  neg_cursor read_animation_timeline "scroll(";
  neg_cursor read_font_kerning "kern";
  neg_cursor read_font_language_override "TRK";
  neg_cursor read_font_optical_sizing "manual";
  neg_cursor read_font_size_adjust "-1";
  neg_cursor read_font_size_adjust_metric "x-height";
  neg_cursor read_font_synthesis "weight weight";
  neg_cursor read_font_synthesis_feature "ligatures";
  neg_cursor read_font_synthesis_position "manual";
  neg_cursor read_font_synthesis_small_caps "manual";
  neg_cursor read_font_synthesis_style "italic";
  neg_cursor read_font_synthesis_weight "manual";
  neg_cursor read_font_variant_caps "caps";
  neg_cursor read_font_variant_east_asian "jis78 jis78";
  neg_cursor read_east_asian_feature "jis05";
  neg_cursor ~allow_partial:true read_font_variant_emoji "emoji text";
  neg_cursor read_font_variant_ligature "common";
  neg_cursor read_font_variant_ligatures "common-ligatures common-ligatures";
  neg_cursor ~allow_partial:true read_font_variant_position "sub super";
  neg_cursor read_glyph_orientation_vertical "45deg";
  neg_cursor read_image_orientation "image";
  neg_cursor read_image_rendering "crisp";
  neg_cursor read_image_resolution "snap 2dppx";
  neg_cursor read_resolution "2px"

(* ignore-test: grouped generated-surface vectors. *)
let spec_generated_box_layout_edges () =
  check_alignment_baseline "text-top";
  check_baseline_shift "10%";
  check_baseline_source "first";
  check_border_image ~expected:"url(border.png)30" "url(border.png) 30";
  check_border_image_outset_item "2px";
  check_border_image_repeat_keyword "round";
  check_border_image_slice "30 fill";
  (* CSS Cascade 5 sec. 7.3 gives the longhand the CSS-wide keywords; the
     offsets the shorthand takes are the same value without them. *)
  check_border_image_slice "initial";
  check_border_image_slice "inherit";
  check_border_image_slice "unset";
  check_border_image_slice "revert";
  check_border_image_slice "revert-layer";
  check_border_image_slice_offsets "30 fill";
  check_border_image_slice_item "30%";
  check_border_image_width_item "auto";
  check_border_spacing "1px 2px";
  check_column_span "all";
  check_contain_intrinsic_longhand "auto 10px";
  check_contain_intrinsic_size "auto 10px 20px";
  check_contain_intrinsic_size_item "auto 10px";
  (* CSS Sizing 4 sec. 5.2 spells a slot [auto? [ none | <length [0,inf]> ]], so
     [none] is one of the two things the optional [auto] may precede and it sits
     in a slot rather than standing for the whole value. Chrome 153 takes each
     of these. *)
  check_contain_intrinsic_longhand "auto none";
  check_contain_intrinsic_longhand "none";
  check_contain_intrinsic_size "none 2ch";
  check_contain_intrinsic_size "auto none";
  check_contain_intrinsic_size "none";
  check_contain_intrinsic_size_item "none";
  check_contain_intrinsic_size_item "auto none";
  check_counter_item "section 2";
  check_counter_set "section 2 subsection";
  check_dominant_baseline "text-bottom";
  check_flex_factor "2";
  (* flex-grow/flex-shrink are <number> (CSS Flexbox 1), so var() and calc() are
     valid. parse + lexical pp (no optimize) holds every calc() unfolded - a
     constant calc(1 + 2) and a calc(var(--g) + 1) alike. The constant fold to 3
     is an optimize+minify transform, asserted in test_optimize. *)
  check_flex_factor "var(--g)";
  check_flex_factor "calc(1 + 2)";
  check_flex_factor "calc(var(--g) + 1)";
  check_flex_flow "row wrap";
  check_grid_line_pair ~expected:"1/span 2" "1 / span 2";
  check_grid_template_areas ~expected:"\"a a\"\"b c\"" "\"a a\" \"b c\"";
  check_hyphenate_limit_chars_item "auto";
  check_hyphenate_limit_chars_item "6";
  check_hyphenate_limit_chars "3 4 5";
  (* CSS Text 4 sec. 6.3.4 gives every slot [auto | <integer>], and CSS Values 4
     sec. 10.1 puts a math function where the integer is. *)
  check_hyphenate_limit_chars "auto";
  check_hyphenate_limit_chars "auto 3";
  check_hyphenate_limit_chars "3 auto auto";
  check_hyphenate_limit_chars "calc(1 + 2)";
  check_hyphenate_limit_chars "auto calc(1 + 2)";
  check_initial_letter "2 3";
  check_initial_letter_align "border-box alphabetic";
  check_initial_letter_align_keyword "hanging";
  check_initial_letter_wrap "first";
  check_inline_sizing "stretch";
  check_interpolate_size "allow-keywords";
  check_line_break "anywhere";
  check_line_fit_edge "text alphabetic";
  check_line_fit_edge_keyword "ideographic-ink";
  check_logical_border_color "red blue";
  check_logical_border_style "solid dashed";
  check_logical_border_width "1px 2px";
  decl_optimizes ~prop:"border-inline-color" ~held:"red blue" ~into:"red #00f"
    "red blue";
  check_min_intrinsic_sizing "legacy zero-if-scroll";
  check_min_intrinsic_sizing_keyword "zero-if-extrinsic";
  check_overflow_clip_box "content-box";
  check_overflow_clip_margin "content-box 1px";
  check_shape_image_threshold ".5";
  (* CSS Shapes 1 sec. 6.2 takes an [<opacity-value>], which CSS Color 4 spells
     [<number> | <percentage>], and clamps it to [0,1] at computed-value time,
     so a value outside the range still reads. *)
  check_shape_image_threshold "-1";
  check_shape_image_threshold "2";
  check_value_cursor "shape_image_threshold" read_shape_image_threshold
    pp_shape_image_threshold ~expected:".5" "50%";
  (* CSS Values 4 sec. 10.1 puts a math function wherever its type is, and
     [opacity] reads the same [<opacity-value>] that way already. Chrome 153
     takes each of these. *)
  check_shape_image_threshold "calc(.2 + .3)";
  check_value_cursor "shape_image_threshold" read_shape_image_threshold
    pp_shape_image_threshold ~expected:"calc(.5 + .25)" "calc(50% + 25%)";
  check_shape_image_threshold ~expected:".2" "min(.2,.8)";
  neg_cursor read_shape_image_threshold "2px";
  check_tab_size "4";
  check_zoom "50%";
  check_zoom "normal";
  neg_cursor ~allow_partial:true read_alignment_baseline "baseline middle";
  neg_cursor ~allow_partial:true read_baseline_shift "sub super";
  neg_cursor read_baseline_source "middle";
  neg_cursor read_border_image "fill";
  neg_cursor read_border_image_outset_item "-1";
  neg_cursor read_border_image_repeat_keyword "tile";
  neg_cursor read_border_image_slice "fill";
  neg_cursor read_border_image_slice_item "-1";
  neg_cursor read_border_image_width_item "-1";
  neg_cursor read_border_spacing "-1px";
  neg_cursor read_column_span "auto";
  neg_cursor read_contain_intrinsic_longhand "auto";
  neg_cursor read_contain_intrinsic_size "auto";
  neg_cursor read_contain_intrinsic_size_item "auto";
  neg_cursor read_counter_item "1section";
  neg_cursor ~allow_partial:true read_counter_set "none section";
  neg_cursor read_dominant_baseline "baseline baseline";
  neg_cursor read_flex_factor "-1";
  neg_cursor ~allow_partial:true read_flex_flow "row row";
  neg_cursor read_grid_line_pair "span";
  neg_cursor read_grid_template_areas "\"a .\" \". a\"";
  neg_cursor read_hyphenate_limit_chars "3 4 5 6";
  neg_cursor read_hyphenate_limit_chars "3.0";
  neg_cursor read_hyphenate_limit_chars "calc(2px)";
  neg_cursor read_initial_letter ".5";
  neg_cursor read_initial_letter_align "alphabetic alphabetic";
  neg_cursor read_initial_letter_align_keyword "cap-height";
  neg_cursor read_initial_letter_wrap "wrap";
  neg_cursor read_inline_sizing "auto";
  neg_cursor read_interpolate_size "keywords";
  neg_cursor read_line_break "break";
  neg_cursor read_line_fit_edge "text text";
  neg_cursor read_line_fit_edge_keyword "baseline";
  neg_cursor ~allow_partial:true read_logical_border_color "red blue green";
  neg_cursor ~allow_partial:true read_logical_border_style "solid dashed dotted";
  neg_cursor ~allow_partial:true read_logical_border_width "1px 2px 3px";
  neg_cursor read_min_intrinsic_sizing "legacy legacy";
  neg_cursor read_min_intrinsic_sizing_keyword "zero";
  neg_cursor read_overflow_clip_box "margin-box";
  neg_cursor ~allow_partial:true read_overflow_clip_margin "1px 2px";
  (* CSS Shapes 1 sec. 6.2 clamps the threshold at computed-value time, so a
     value outside [0,1] reads; a unit is what it has no place for. *)
  neg_cursor read_shape_image_threshold "2px";
  neg_cursor read_tab_size "-1"

(* ignore-test: grouped generated-surface vectors. *)
let spec_generated_position_interaction_edges () =
  check_anchor_name ~expected:"--hero,--toast" "--hero, --toast";
  check_caret "red manual block";
  check_caret "red auto";
  check_caret_animation "manual";
  check_caret_shape "underscore";
  check_container_name "main sidebar";
  check_interactivity "inert";
  check_interest_delay "100ms 200ms";
  check_interest_delay "normal 120ms";
  check_interest_delay_item "normal";
  check_interest_delay_item "120ms";
  check_margin_trim "block-start inline-end";
  check_margin_trim_axis "inline";
  check_margin_trim_edge "block-end";
  check_mask "url(mask.png)";
  check_mask_layer "url(mask.png)";
  (* CSS Masking 1 sec. 8.7 spells [mask] as [<mask-layer>#], so a comma in the
     shorthand separates layers and the image slot of a layer holds one image:
     the comma list belongs to the [mask-image] longhand. A layer after one that
     opened with an image is an ordinary layer, so it need not open with one.
     Chrome 153 reads each of these and gives the second layer the initial
     image. *)
  check_mask "none,50%";
  check_mask "url(a.png),50%";
  check_mask "url(a.png),50%,url(b.png)";
  check_mask "none,alpha";
  check_mask "50%,none";
  check_mask "none,url(b.png)";
  check_nav "#next current";
  check_nav_scope "root";
  check_object_view_box "inset(10px 20px)";
  check_object_view_box "rect(0px 0px 10px 10px)";
  check_offset_path "ray(45deg sides contain at center)";
  check_offset_rotate "auto 45deg";
  check_offset_rotate_mode "reverse";
  check_overflow_anchor "none";
  check_overlay "auto";
  check_position_anchor "--menu";
  check_position_area "center span-all";
  (* css-anchor-position-1 sec. 3.1.2: a lone keyword behaves as if the second
     were [span-all] when it names one axis, and repeats itself only when it
     names neither. So [top center] is a different area from [top], and Chrome
     151 agrees: it computes them as "center top" and "top", and lays a
     percentage-width box out at 200px wide 50px against 0px wide 600px. Only
     the axis-ambiguous keywords fold. *)
  check_position_area "top center";
  check_position_area "left center";
  check_position_area "block-start center";
  check_position_area "x-start center";
  check_position_area "span-left center";
  check_position_area "center top";
  check_position_area ~expected:"center" "center center";
  check_position_area ~expected:"span-all" "span-all span-all";
  (* css-anchor-position-1 sec. 3.1.2 has two {1,2} branches over the plain
     logical keywords, [start] [end] [span-start] [span-end] [span-all], and
     over their [self-] equivalents. The spec calls every start/end keyword that
     names neither the block nor the inline axis ambiguous, so a lone one
     repeats itself instead of standing for [X span-all]: [start start] folds to
     [start], while [start end] and [end start] name distinct areas and survive
     minification unchanged. *)
  check_position_area "start";
  check_position_area ~expected:"start" "start start";
  check_position_area "start end";
  check_position_area "end start";
  check_position_area "start span-end";
  check_position_area "span-start end";
  check_position_area ~expected:"self-start" "self-start self-start";
  check_position_area "self-start span-self-end";
  check_position_area "span-self-start self-end";
  (* Section 3.1.2 also spells out [self-x-start]/[self-x-end] and
     [self-y-start]/[self-y-end] inside the two physical groups, and gives
     [self-block-start]/[self-inline-start] a branch of their own, each with a
     [span-] form. Unlike [self-start], every one of these names its axis, so
     the ambiguity clause does not reach them: a lone one behaves as if the
     second keyword were [span-all] instead of repeating itself, which leaves a
     repeated one naming a single axis twice, a shape no branch admits. *)
  check_position_area "self-x-start";
  check_position_area "self-x-end";
  check_position_area "self-y-start";
  check_position_area "self-y-end";
  check_position_area "span-self-x-start";
  check_position_area "span-self-x-end";
  check_position_area "span-self-y-start";
  check_position_area "span-self-y-end";
  check_position_area "self-block-start";
  check_position_area "self-block-end";
  check_position_area "self-inline-start";
  check_position_area "self-inline-end";
  check_position_area "span-self-block-start";
  check_position_area "span-self-block-end";
  check_position_area "span-self-inline-start";
  check_position_area "span-self-inline-end";
  (* Two keywords on opposite axes name one row and one column, so they are a
     distinct area from either alone and stay as written. *)
  check_position_area "self-x-start self-y-end";
  check_position_area "span-self-x-end self-y-start";
  check_position_area "self-block-start self-inline-end";
  check_position_area "span-self-block-end self-inline-start";
  (* [span-all] is the ambiguous partner, so it takes the free axis and the pair
     is held exactly as [top span-all] is. *)
  check_position_area "top span-all";
  check_position_area "self-x-start span-all";
  check_position_area "span-all self-block-end";
  (* Naming one axis twice has no branch, in contrast with the ambiguous
     [self-start self-start] above, which folds. *)
  neg_cursor read_position_area "self-x-start self-x-end";
  neg_cursor read_position_area "self-x-start self-x-start";
  neg_cursor read_position_area "self-y-start span-self-y-end";
  neg_cursor read_position_area "self-block-start self-block-end";
  neg_cursor read_position_area "self-block-start self-block-start";
  neg_cursor read_position_area "self-inline-start span-self-inline-end";
  (* Sec. 3.1.2 spells <position-area> as five top-level alternatives: the
     physical [ left | ... ] || [ top | ... ], the logical [ block-start | ... ]
     || [ inline-start | ... ], its [self-] twin, and the two {1,2} groups over
     [ start | ... ] and [ self-start | ... ]. Only [center] and [span-all]
     appear in every group, so any other pair has to come from one alternative.
     Naming two axes is not enough on its own: each pair below names one row and
     one column yet crosses two alternatives, and Chrome 151 rejects every one
     of them, leaving position-area at its [none] initial value. *)
  neg_cursor read_position_area "left block-start";
  neg_cursor read_position_area "block-start left";
  neg_cursor read_position_area "inline-start y-end";
  neg_cursor read_position_area "x-start self-block-start";
  neg_cursor read_position_area "self-x-start self-block-start";
  neg_cursor read_position_area "self-inline-end top";
  neg_cursor read_position_area "block-start self-inline-end";
  neg_cursor read_position_area "span-inline-start span-self-block-end";
  (* The {1,2} groups are alternatives of their own, so a plain [start] pairs
     with nothing outside its own group, not with a physical or logical keyword
     and not with a [self-] one. *)
  neg_cursor read_position_area "start top";
  neg_cursor read_position_area "span-end left";
  neg_cursor read_position_area "start block-end";
  neg_cursor read_position_area "inline-end span-start";
  neg_cursor read_position_area "start self-inline-start";
  neg_cursor read_position_area "self-block-end span-start";
  neg_cursor read_position_area "self-start right";
  neg_cursor read_position_area "span-self-end y-start";
  neg_cursor read_position_area "self-end block-start";
  neg_cursor read_position_area "self-start self-x-end";
  neg_cursor read_position_area "self-start self-block-end";
  neg_cursor read_position_area "span-self-inline-start self-end";
  neg_cursor read_position_area "start self-start";
  neg_cursor read_position_area "span-self-start end";
  (* Within one alternative the two sides of the [||] still combine in either
     order, and [center] and [span-all] pair with anything. *)
  check_position_area "left top";
  check_position_area "x-start self-y-end";
  check_position_area "block-start inline-end";
  check_position_area "span-inline-start block-end";
  check_position_area "self-inline-start self-block-end";
  check_position_area "inline-start center";
  check_position_area "span-all self-inline-end";
  check_position_area "start span-all";
  check_position_area "center self-end";
  check_position_area "top span-right";
  check_position_area "top span-left";
  check_position_area_keyword "span-inline-start";
  check_position_try_fallback "flip-block";
  check_position_try_fallback_entry "flip-block";
  check_position_try_fallback_entry "start end";
  check_position_try_fallbacks ~expected:"flip-block,--fallback"
    "flip-block, --fallback";
  check_position_try_order "most-inline-size";
  check_position_visibility "anchors-visible no-overflow";
  check_position_visibility_condition "no-overflow";
  check_ray ~expected:"45deg sides contain at center"
    "ray(45deg sides contain at center)";
  check_ray_size "sides";
  check_ruby_align "space-around";
  check_ruby_merge "merge";
  check_ruby_overhang "none";
  check_ruby_position "alternate over";
  check_ruby_position_keyword "inter-character";
  check_scrollbar_color "red blue";
  decl_optimizes ~prop:"scrollbar-color" ~held:"red blue" ~into:"red #00f"
    "red blue";
  check_scrollbar_gutter "stable both-edges";
  check_scrollbar_width "thin";
  neg_cursor ~allow_partial:true read_anchor_name "none --x";
  neg_cursor read_caret "manual manual";
  neg_cursor read_caret_animation "blink";
  neg_cursor read_caret_shape "line";
  neg_cursor ~allow_partial:true read_container_name "none main";
  neg_cursor read_interactivity "enabled";
  neg_cursor ~allow_partial:true read_interest_delay "-1s";
  neg_cursor read_margin_trim "block block";
  neg_cursor read_margin_trim_axis "both";
  neg_cursor read_margin_trim_edge "block";
  neg_cursor read_mask "url(mask.png) url(other.png)";
  neg_cursor read_mask_layer "url(mask.png) url(other.png)";
  neg_cursor read_nav "next current";
  neg_cursor read_nav_scope "document";
  neg_cursor read_object_view_box "xywh(0 0 1px)";
  (* CSS Images 5 sec. 4.1 gives [object-view-box]'s [inset()]/[xywh()]/[rect()]
     a fixed quad (plus an optional [round] tail for the latter two);
     [Cursor.call] did not require any of the three readers to consume their
     whole sub-cursor, so trailing content past the quad (or, for [inset()],
     even a fifth value) was silently dropped instead of invalidating the
     declaration. *)
  neg_cursor read_object_view_box "inset(10px 20px 30px 40px 50px)";
  neg_cursor read_object_view_box "xywh(0 0 1px 1px foo)";
  neg_cursor read_object_view_box "rect(0px 0px 10px 10px foo)";
  neg_cursor read_offset_path "ray()";
  neg_cursor ~allow_partial:true read_offset_rotate "auto reverse";
  neg_cursor read_offset_rotate_mode "left";
  neg_cursor read_overflow_anchor "visible";
  neg_cursor ~allow_partial:true read_overlay "none auto";
  neg_cursor read_position_anchor "menu";
  neg_cursor read_position_area "top top top";
  neg_cursor read_position_area_keyword "middle";
  neg_cursor read_position_try_fallback "flip";
  neg_cursor ~allow_partial:true read_position_try_fallbacks "none flip-block";
  (* Sec. 6.1 keeps the tactic group and the position area apart, so an entry
     never mixes one with the other. *)
  neg_cursor ~allow_partial:true read_position_try_fallbacks "--a center";
  neg_cursor read_position_try_order "most-size";
  neg_cursor ~allow_partial:true read_position_visibility
    "always anchors-visible";
  neg_cursor read_position_visibility_condition "visible";
  neg_cursor read_ray "ray()";
  neg_cursor read_ray_size "closest";
  neg_cursor read_ruby_align "justify";
  neg_cursor read_ruby_merge "merged";
  neg_cursor read_ruby_overhang "all";
  neg_cursor read_ruby_position "over over";
  neg_cursor read_ruby_position_keyword "above";
  neg_cursor read_scrollbar_color "red";
  neg_cursor read_scrollbar_gutter "both-edges";
  neg_cursor read_scrollbar_width "wide"

(* ignore-test: grouped generated-surface vectors. *)
let spec_generated_text_timeline_edges () =
  check_text_box "trim-both text alphabetic";
  check_text_box "cap alphabetic";
  check_text_box "normal";
  check_text_box ~expected:"trim-both cap alphabetic" "cap alphabetic trim-both";
  check_text_box_edge "text ideographic";
  check_text_box_edge_keyword "ideographic-ink";
  check_text_box_trim "trim-both";
  check_text_combine_upright "digits 3";
  check_text_decoration_skip "auto";
  check_text_decoration_skip_box "all";
  check_text_decoration_skip_inset "auto";
  check_text_decoration_skip_self "objects";
  check_text_decoration_skip_space "start";
  check_text_decoration_skip_spaces "start end";
  check_text_emphasis "filled dot red";
  check_text_emphasis_fill "open";
  check_text_emphasis_line "under";
  check_text_emphasis_position "under left";
  check_text_emphasis_shape "sesame";
  check_text_emphasis_side "right";
  check_text_emphasis_skip "spaces punctuation";
  check_text_emphasis_skip_keyword "symbols";
  check_text_emphasis_style "\"*\"";
  check_text_orientation "sideways";
  check_text_spacing_trim "space-first";
  check_text_underline_position "under right";
  check_text_underline_position_keyword "under";
  check_text_wrap_mode "nowrap";
  check_text_wrap_style "stable";
  check_timeline_inset "auto 100%";
  (* Sec. 5.3 spells the inset [[ [ auto | <length-percentage> ]{1,2} ]#]. *)
  check_timeline_inset "auto,1rem";
  check_timeline_inset "auto 1rem,2px";
  check_timeline_inset_item "100%";
  (* timeline-inset takes [ auto | <length-percentage> ], so a held calc()
     too. *)
  check_timeline_inset_item "calc(50% + 10px)";
  check_timeline_name ~expected:"--main,--alt" "--main, --alt";
  check_timeline_ident "none";
  check_timeline_ident "--main";
  (* Scroll-driven Animations 1 secs. 4.1 and 5.1 spell the name [[ none |
     <dashed-ident> ]#], so [none] names one timeline among others. *)
  check_timeline_name "none,none";
  check_timeline_name ~expected:"--main,none" "--main, none";
  (* Sec. 6 keeps [timeline-scope] at [none | <dashed-ident>#], where the
     keyword stands for the whole value. *)
  check_timeline_scope "none";
  check_timeline_scope ~expected:"--main,--alt" "--main, --alt";
  neg_cursor ~allow_partial:true read_timeline_scope "none,--main";
  check_timeline_shorthand_item "--main block";
  check_timeline_shorthand_item "--main";
  check_timeline_shorthand_item "none";
  check_view_transition_class "card active";
  check_view_transition_name "match-element";
  neg_cursor ~allow_partial:true read_text_box "trim-start trim-end";
  neg_cursor read_text_box "cap";
  neg_cursor read_text_box_edge "text text";
  neg_cursor read_text_box_edge_keyword "baseline";
  neg_cursor read_text_box_trim "trim";
  neg_cursor ~allow_partial:true read_text_combine_upright "digits 5";
  neg_cursor read_text_decoration_skip "all";
  neg_cursor read_text_decoration_skip_box "auto";
  neg_cursor read_text_decoration_skip_inset "all";
  neg_cursor read_text_decoration_skip_self "all";
  neg_cursor read_text_decoration_skip_space "middle";
  neg_cursor read_text_decoration_skip_spaces "all start";
  neg_cursor read_text_emphasis "filled open";
  neg_cursor read_text_emphasis_fill "solid";
  neg_cursor read_text_emphasis_line "above";
  neg_cursor read_text_emphasis_position "under over";
  neg_cursor read_text_emphasis_shape "square";
  neg_cursor read_text_emphasis_side "center";
  neg_cursor read_text_emphasis_skip "spaces spaces";
  neg_cursor read_text_emphasis_skip_keyword "letters";
  neg_cursor read_text_emphasis_style "filled open";
  neg_cursor ~allow_partial:true read_text_orientation "upright sideways";
  neg_cursor read_text_spacing_trim "trim";
  neg_cursor ~allow_partial:true read_text_underline_position "left right";
  neg_cursor read_text_underline_position_keyword "below";
  neg_cursor read_text_wrap_mode "pretty";
  neg_cursor read_text_wrap_style "nowrap";
  neg_cursor ~allow_partial:true read_timeline_inset "auto auto auto";
  (* Scroll Animations 1 sec. 3.4.3 gives each item [auto] or a plain
     [<length-percentage>], with no range restriction, so a negative reads. *)
  check_value_cursor "timeline_inset_item" read_timeline_inset_item
    pp_timeline_inset_item "-1px";
  neg_cursor read_timeline_inset_item "1deg";
  neg_cursor ~allow_partial:true read_timeline_name "none --main";
  neg_cursor ~allow_partial:true read_timeline_shorthand_item "--main z";
  neg_cursor ~allow_partial:true read_view_transition_class "none card";
  neg_cursor ~allow_partial:true read_view_transition_name "match-element card"

let suite =
  ( "properties_generated",
    [
      test_case "spec generated animation/font edges" `Quick
        spec_generated_animation_font_edges;
      test_case "spec generated box/layout edges" `Quick
        spec_generated_box_layout_edges;
      test_case "spec generated position/interaction edges" `Quick
        spec_generated_position_interaction_edges;
      test_case "spec generated text/timeline edges" `Quick
        spec_generated_text_timeline_edges;
    ] )
