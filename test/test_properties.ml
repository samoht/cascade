open Alcotest
open Css.Properties

(* Generic check function for property values *)
let check_value name pp reader ?(minify = true) ?expected input =
  let expected = Option.value ~default:input expected in
  (* First pass: parse + print equals expected (minified) *)
  let t = Css.Reader.of_string input in
  let v = reader t in
  let s = Css.Pp.to_string ~minify pp v in
  check string (Fmt.str "%s %s" name input) expected s;
  (* Roundtrip stability: read printed output and ensure idempotent printing *)
  let t2 = Css.Reader.of_string s in
  let v2 = reader t2 in
  let s2 = Css.Pp.to_string ~minify pp v2 in
  check string (Fmt.str "roundtrip %s %s" name input) s s2

(* Check functions for each type - one-liner definitions *)
let check_display = check_value "display" pp_display read_display
let check_position = check_value "position" pp_position read_position
let check_overflow = check_value "overflow" pp_overflow read_overflow

let check_border_style =
  check_value "border-style" pp_border_style read_border_style

let check_border = check_value "border" pp_border read_border
let check_visibility = check_value "visibility" pp_visibility read_visibility
let check_z_index = check_value "z-index" pp_z_index read_z_index

let check_flex_direction =
  check_value "flex-direction" pp_flex_direction read_flex_direction

let check_flex_wrap = check_value "flex-wrap" pp_flex_wrap read_flex_wrap
let check_align_self = check_value "align-self" pp_align_self read_align_self
let check_font_style = check_value "font-style" pp_font_style read_font_style
let check_text_align = check_value "text-align" pp_text_align read_text_align

let check_text_decoration_style =
  check_value "text-decoration-style" pp_text_decoration_style
    read_text_decoration_style

let check_text_overflow =
  check_value "text-overflow" pp_text_overflow read_text_overflow

let check_text_wrap = check_value "text-wrap" pp_text_wrap read_text_wrap

let check_white_space =
  check_value "white-space" pp_white_space read_white_space

let check_word_break = check_value "word-break" pp_word_break read_word_break

let check_text_decoration_shorthand =
  check_value "text_decoration_shorthand" pp_text_decoration_shorthand
    read_text_decoration_shorthand

let check_justify_self =
  check_value "justify_self" pp_justify_self read_justify_self

let check_align_content =
  check_value "align_content" pp_align_content read_align_content

let check_border_shorthand =
  check_value "border_shorthand" pp_border_shorthand read_border_shorthand

let check_justify_items =
  check_value "justify_items" pp_justify_items read_justify_items

let check_transition_shorthand =
  check_value "transition_shorthand" pp_transition_shorthand
    read_transition_shorthand

let check_flex_basis = check_value "flex_basis" pp_flex_basis read_flex_basis

let check_background_shorthand =
  check_value "background_shorthand" pp_background_shorthand
    read_background_shorthand

let check_animation_shorthand =
  check_value "animation_shorthand" pp_animation_shorthand
    read_animation_shorthand

let check_text_decoration_line =
  check_value "text_decoration_line" pp_text_decoration_line
    read_text_decoration_line

let check_text_size_adjust =
  check_value "text_size_adjust" pp_text_size_adjust read_text_size_adjust

let check_any_property =
  check_value "any_property" pp_any_property read_any_property

let check_overflow_wrap =
  check_value "overflow-wrap" pp_overflow_wrap read_overflow_wrap

let check_hyphens = check_value "hyphens" pp_hyphens read_hyphens

let check_line_height =
  check_value "line-height" pp_line_height read_line_height

(* Additional check functions for list, table, and other types *)
let check_list_style_type =
  check_value "list-style-type" pp_list_style_type read_list_style_type

let check_list_style_position =
  check_value "list-style-position" pp_list_style_position
    read_list_style_position

let check_list_style_image =
  check_value "list-style-image" pp_list_style_image read_list_style_image

let check_table_layout =
  check_value "table-layout" pp_table_layout read_table_layout

let check_border_collapse =
  check_value "border-collapse" pp_border_collapse read_border_collapse

let check_user_select =
  check_value "user-select" pp_user_select read_user_select

let check_pointer_events =
  check_value "pointer-events" pp_pointer_events read_pointer_events

let check_touch_action =
  check_value "touch-action" pp_touch_action read_touch_action

let check_resize = check_value "resize" pp_resize read_resize
let check_box_sizing = check_value "box-sizing" pp_box_sizing read_box_sizing
let check_object_fit = check_value "object-fit" pp_object_fit read_object_fit

let check_content_visibility =
  check_value "content-visibility" pp_content_visibility read_content_visibility

let check_container_type =
  check_value "container-type" pp_container_type read_container_type

let check_contain = check_value "contain" pp_contain read_contain
let check_isolation = check_value "isolation" pp_isolation read_isolation

let check_scroll_behavior =
  check_value "scroll-behavior" pp_scroll_behavior read_scroll_behavior

(* More check functions for complex types *)
let check_scroll_snap_align =
  check_value "scroll-snap-align" pp_scroll_snap_align read_scroll_snap_align

let check_scroll_snap_stop =
  check_value "scroll-snap-stop" pp_scroll_snap_stop read_scroll_snap_stop

let check_scroll_snap_type =
  check_value "scroll-snap-type" pp_scroll_snap_type read_scroll_snap_type

let check_svg_paint = check_value "svg-paint" pp_svg_paint read_svg_paint
let check_direction = check_value "direction" pp_direction read_direction

let check_unicode_bidi =
  check_value "unicode-bidi" pp_unicode_bidi read_unicode_bidi

let check_writing_mode =
  check_value "writing-mode" pp_writing_mode read_writing_mode

let check_webkit_appearance =
  check_value "-webkit-appearance" pp_webkit_appearance read_webkit_appearance

let check_webkit_font_smoothing =
  check_value "-webkit-font-smoothing" pp_webkit_font_smoothing
    read_webkit_font_smoothing

let check_moz_osx_font_smoothing =
  check_value "-moz-osx-font-smoothing" pp_moz_osx_font_smoothing
    read_moz_osx_font_smoothing

let check_webkit_box_orient =
  check_value "-webkit-box-orient" pp_webkit_box_orient read_webkit_box_orient

let check_forced_color_adjust =
  check_value "forced-color-adjust" pp_forced_color_adjust
    read_forced_color_adjust

let check_appearance = check_value "appearance" pp_appearance read_appearance
let check_clear = check_value "clear" pp_clear read_clear
let check_float_side = check_value "float" pp_float_side read_float_side

let check_text_decoration_skip_ink =
  check_value "text-decoration-skip-ink" pp_text_decoration_skip_ink
    read_text_decoration_skip_ink

let check_vertical_align =
  check_value "vertical-align" pp_vertical_align read_vertical_align

let check_outline_style =
  check_value "outline-style" pp_outline_style read_outline_style

let check_font_family =
  check_value "font-family" pp_font_family read_font_family

let check_font_stretch =
  check_value "font-stretch" pp_font_stretch read_font_stretch

let check_font_variant_numeric =
  check_value "font-variant-numeric" pp_font_variant_numeric
    read_font_variant_numeric

let check_font_feature_settings =
  check_value "font-feature-settings" pp_font_feature_settings
    read_font_feature_settings

let check_font_variation_settings =
  check_value "font-variation-settings" pp_font_variation_settings
    read_font_variation_settings

let check_backface_visibility =
  check_value "backface-visibility" pp_backface_visibility
    read_backface_visibility

let check_scale = check_value "scale" pp_scale read_scale

let check_timing_function =
  check_value "timing-function" pp_timing_function read_timing_function

let check_transition_property =
  check_value "transition-property" pp_transition_property
    read_transition_property

let check_transition = check_value "transition" pp_transition read_transition

let check_animation_direction =
  check_value "animation-direction" pp_animation_direction
    read_animation_direction

let check_animation_fill_mode =
  check_value "animation-fill-mode" pp_animation_fill_mode
    read_animation_fill_mode

let check_animation_iteration_count =
  check_value "animation-iteration-count" pp_animation_iteration_count
    read_animation_iteration_count

let check_animation_play_state =
  check_value "animation-play-state" pp_animation_play_state
    read_animation_play_state

let check_animation = check_value "animation" pp_animation read_animation

let check_blend_mode =
  check_value "mix-blend-mode" pp_blend_mode read_blend_mode

let check_text_shadow =
  check_value "text-shadow" pp_text_shadow read_text_shadow

let check_box_shadow =
  check_value "box-shadow"
    (Css.Pp.list ~sep:Css.Pp.comma pp_shadow)
    (Css.Reader.list ~sep:Css.Reader.comma ~at_least:1 read_shadow)

(* Reader alias for box-shadow list for negative tests readability *)
let read_box_shadow t : shadow list =
  (* box-shadow is either 'none' or a comma-separated list of shadows *)
  (* It cannot be "none,none" or mix none with other shadows *)
  match Css.Reader.option read_shadow t with
  | Some None ->
      (* Check if there's a comma after 'none' *)
      Css.Reader.ws t;
      if Css.Reader.peek t = Some ',' then
        failwith "box-shadow 'none' cannot be part of a list"
      else [ None ]
  | Some shadow ->
      (* Read remaining shadows if any *)
      let shadows = ref [ shadow ] in
      Css.Reader.ws t;
      while Css.Reader.peek t = Some ',' do
        Css.Reader.skip t;
        Css.Reader.ws t;
        let s = read_shadow t in
        if s = None then failwith "box-shadow 'none' cannot be part of a list"
        else shadows := s :: !shadows;
        Css.Reader.ws t
      done;
      List.rev !shadows
  | None -> failwith "invalid box-shadow"

let check_filter = check_value "filter" pp_filter read_filter

let check_background_attachment =
  check_value "background-attachment" pp_background_attachment
    read_background_attachment

let check_background_repeat =
  check_value "background-repeat" pp_background_repeat read_background_repeat

let check_background_size =
  check_value "background-size" pp_background_size read_background_size

let check_background_image =
  check_value "background-image" pp_background_image read_background_image

let check_overscroll_behavior =
  check_value "overscroll-behavior" pp_overscroll_behavior
    read_overscroll_behavior

let check_aspect_ratio =
  check_value "aspect-ratio" pp_aspect_ratio read_aspect_ratio

let check_content = check_value "content" pp_content read_content

let check_grid_auto_flow =
  check_value "grid-auto-flow" pp_grid_auto_flow read_grid_auto_flow

let check_grid_track_size =
  check_value "grid-track-size" pp_grid_template read_grid_template

let check_grid_template =
  check_value "grid-template" pp_grid_template read_grid_template

let check_grid_line = check_value "grid-line" pp_grid_line read_grid_line

let check_align_items =
  check_value "align-items" pp_align_items read_align_items

let check_justify_content =
  check_value "justify-content" pp_justify_content read_justify_content

let check_flex = check_value "flex" pp_flex read_flex

let check_place_items =
  check_value "place-items" pp_place_items read_place_items

let check_place_content =
  check_value "place-content" pp_place_content read_place_content

let check_transform = check_value "transform" pp_transform read_transform

let check_gradient_direction =
  check_value "gradient-direction" pp_gradient_direction read_gradient_direction

let check_gradient_stop =
  check_value "gradient-stop" pp_gradient_stop read_gradient_stop

(* Helper for any_property type let pp_any_property : any_property Css.Pp.t =
   fun ctx (Prop p) -> pp_property ctx p *)

let check_font_weight =
  check_value "font_weight" pp_font_weight read_font_weight

let check_cursor = check_value "cursor" pp_cursor read_cursor

let check_scroll_snap_strictness =
  check_value "scroll_snap_strictness" pp_scroll_snap_strictness
    read_scroll_snap_strictness

let check_transform_style =
  check_value "transform_style" pp_transform_style read_transform_style

let check_font_variant_numeric_token =
  check_value "font_variant_numeric_token" pp_font_variant_numeric_token
    read_font_variant_numeric_token

let check_transform_origin =
  check_value "transform_origin" pp_transform_origin read_transform_origin

let check_gap = check_value "gap" pp_gap read_gap

let check_text_decoration =
  check_value "text_decoration" pp_text_decoration read_text_decoration

(* scroll_snap_axis type doesn't exist let check_scroll_snap_axis = check_value
   "scroll_snap_axis" pp_scroll_snap_axis read_scroll_snap_axis *)

let check_border_width =
  check_value "border_width" pp_border_width read_border_width

let check_text_transform =
  check_value "text_transform" pp_text_transform read_text_transform

(* Helper for property-value pairs printing *)
let check_property_value expected (prop, value) =
  let pp = pp_property_value in
  let to_string f = Css.Pp.to_string ~minify:true f in
  let actual = to_string pp (prop, value) in
  let name = Fmt.str "%s value" (Css.Pp.to_string pp_property prop) in
  check string name expected actual

(* Test functions that use the check_<type> functions *)
let test_display () =
  check_display "none";
  check_display "block";
  check_display "inline";
  check_display "inline-block";
  check_display "flex";
  check_display "inline-flex";
  check_display "grid";
  check_display "inline-grid";
  check_display "flow-root";
  check_display "table";
  check_display "table-row";
  check_display "table-cell";
  check_display "table-caption";
  check_display "table-column";
  check_display "table-column-group";
  check_display "table-footer-group";
  check_display "table-header-group";
  check_display "table-row-group";
  check_display "inline-table";
  check_display "list-item";
  check_display "contents";
  (* Intentional legacy: accepted for compatibility in some engines *)
  check_display "-webkit-box";
  (* CSS-wide keyword supported by this reader *)
  check_display "unset"

let test_position () =
  check_position "static";
  check_position "relative";
  check_position "absolute";
  check_position "fixed";
  check_position "sticky"

let test_overflow () =
  check_overflow "visible";
  check_overflow "hidden";
  check_overflow "scroll";
  check_overflow "auto";
  check_overflow "clip"

let test_border_style () =
  check_border_style "none";
  check_border_style "solid";
  check_border_style "dashed";
  check_border_style "dotted";
  check_border_style "double";
  check_border_style "groove";
  check_border_style "ridge";
  check_border_style "inset";
  check_border_style "outset";
  check_border_style "hidden"

let test_border () =
  (* Test individual components *)
  check_border "2px";
  check_border "solid";
  check_border "red";

  (* Test combinations *)
  check_border "1px solid";
  check_border "2px red";
  check_border "solid red";
  check_border "1px solid red";

  (* Test with different order - parser should normalize *)
  check_border ~expected:"2px solid red" "red solid 2px";
  check_border ~expected:"2px solid" "solid 2px";

  (* Test with zero width *)
  check_border "0 solid";
  check_border "0 solid black";

  (* Test with inherit/initial *)
  check_border "inherit";
  check_border "initial"

let test_visibility () =
  check_visibility "visible";
  check_visibility "hidden";
  check_visibility "collapse"

let test_z_index () =
  check_z_index "auto";
  check_z_index "10";
  check_z_index "-1"

let test_flex_direction () =
  check_flex_direction "row";
  check_flex_direction "row-reverse";
  check_flex_direction "column";
  check_flex_direction "column-reverse"

let test_flex_wrap () =
  check_flex_wrap "nowrap";
  check_flex_wrap "wrap";
  check_flex_wrap "wrap-reverse"

let test_align_self () =
  check_align_self "auto";
  check_align_self "flex-start";
  check_align_self "flex-end";
  check_align_self "center";
  check_align_self "baseline";
  check_align_self "stretch"

let test_font_style () =
  check_font_style "normal";
  check_font_style "italic";
  check_font_style "oblique";
  check_font_style "inherit"

let test_css_wide_keywords_subset () =
  (* A small subset where readers support CSS-wide keywords beyond
     inherit/initial *)
  check_display "unset";
  (* font-family supports 'unset' via reader *)
  check_font_family "unset"

let test_text_align () =
  check_text_align "left";
  check_text_align "right";
  check_text_align "center";
  check_text_align "justify";
  check_text_align "start";
  check_text_align "end";
  check_text_align "inherit"

let test_text_decoration_style () =
  check_text_decoration_style "solid";
  check_text_decoration_style "double";
  check_text_decoration_style "dotted";
  check_text_decoration_style "dashed";
  check_text_decoration_style "wavy";
  check_text_decoration_style "inherit"

let test_text_overflow () =
  check_text_overflow "clip";
  check_text_overflow "ellipsis";
  check_text_overflow "inherit"

let test_text_wrap () =
  check_text_wrap "wrap";
  check_text_wrap "nowrap";
  check_text_wrap "balance";
  check_text_wrap "pretty";
  check_text_wrap "inherit"

let test_white_space () =
  check_white_space "normal";
  check_white_space "nowrap";
  check_white_space "pre";
  check_white_space "pre-wrap";
  check_white_space "pre-line";
  check_white_space "break-spaces";
  check_white_space "inherit"

let test_word_break () =
  check_word_break "normal";
  check_word_break "break-all";
  check_word_break "keep-all";
  (* Intentional legacy: word-break: break-word is non-standard, kept for
     compatibility; modern alternative is overflow-wrap:anywhere *)
  check_word_break "break-word";
  check_word_break "inherit"

let test_overflow_wrap () =
  check_overflow_wrap "normal";
  check_overflow_wrap "break-word";
  check_overflow_wrap "anywhere";
  check_overflow_wrap "inherit"

let test_hyphens () =
  check_hyphens "none";
  check_hyphens "manual";
  check_hyphens "auto";
  check_hyphens "inherit"

let test_line_height () =
  (* Only test values supported by the simplified reader *)
  check_line_height "normal";
  check_line_height "inherit";
  check_line_height "1.5";
  check_line_height "120%"

let test_list_style () =
  check_list_style_type "none";
  check_list_style_type "disc";
  check_list_style_type "circle";
  check_list_style_type "square";
  check_list_style_type "decimal";
  check_list_style_type "lower-alpha";
  check_list_style_type "upper-alpha";
  check_list_style_type "lower-roman";
  check_list_style_type "upper-roman";
  check_list_style_position "inside";
  check_list_style_position "outside";
  check_list_style_position "inherit";
  check_list_style_image "none";
  check_list_style_image "inherit";
  check_list_style_image "url(https://example.com/x.png)"

let test_table_layout () =
  check_table_layout "auto";
  check_table_layout "fixed";
  check_table_layout "inherit"

let test_border_collapse () =
  check_border_collapse "collapse";
  check_border_collapse "separate";
  check_border_collapse "inherit"

let test_user_pointer () =
  check_user_select "none";
  check_user_select "auto";
  check_user_select "text";
  check_user_select "all";
  check_user_select "contain";
  check_pointer_events "auto";
  check_pointer_events "none";
  check_pointer_events "visiblepainted";
  check_pointer_events "visiblefill";
  check_pointer_events "visiblestroke";
  check_pointer_events "visible";
  check_pointer_events "painted";
  check_pointer_events "fill";
  check_pointer_events "stroke";
  check_pointer_events "all";
  check_pointer_events "inherit"

let test_touch_resize_box_sizing () =
  check_touch_action "auto";
  check_touch_action "none";
  check_touch_action "pan-x";
  check_touch_action "pan-y";
  check_touch_action "manipulation";
  check_touch_action "inherit";
  check_resize "none";
  check_resize "both";
  check_resize "horizontal";
  check_resize "vertical";
  check_resize "block";
  check_resize "inline";
  check_resize "inherit";
  check_box_sizing "border-box";
  check_box_sizing "content-box";
  check_box_sizing "inherit"

let test_object_and_content_visibility () =
  check_object_fit "fill";
  check_object_fit "contain";
  check_object_fit "cover";
  check_object_fit "none";
  check_object_fit "scale-down";
  check_object_fit "inherit";
  check_content_visibility "visible";
  check_content_visibility "auto";
  check_content_visibility "hidden";
  check_content_visibility "inherit"

let test_container_and_contain () =
  check_container_type "normal";
  check_container_type "inline-size";
  check_container_type "size";
  check_contain "none";
  check_contain "strict";
  check_contain "content";
  check_contain "size";
  check_contain "layout";
  check_contain "style";
  check_contain "paint";
  (* Spec: combinations are allowed; test canonical combos *)
  check_contain "size layout style paint";
  check_contain "layout paint";
  check_contain "size style"

let test_isolation_and_scroll () =
  check_isolation "auto";
  check_isolation "isolate";
  check_isolation "inherit";
  check_scroll_behavior "auto";
  check_scroll_behavior "smooth";
  check_scroll_behavior "inherit"

let test_scroll_snap () =
  check_scroll_snap_align "none";
  check_scroll_snap_align "start";
  check_scroll_snap_align "end";
  check_scroll_snap_align "center";
  check_scroll_snap_stop "normal";
  check_scroll_snap_stop "always";
  check_scroll_snap_stop "inherit";
  check_scroll_snap_type "none";
  check_scroll_snap_type "inherit";
  (* Additional axis+strictness combinations supported by the reader *)
  check_scroll_snap_type "x mandatory";
  check_scroll_snap_type "y mandatory";
  check_scroll_snap_type "inline mandatory";
  check_scroll_snap_type "block mandatory";
  check_scroll_snap_type "both mandatory";
  check_scroll_snap_type "x proximity";
  check_scroll_snap_type "y proximity";
  check_scroll_snap_type "inline proximity";
  check_scroll_snap_type "block proximity";
  check_scroll_snap_type "both proximity";
  check_scroll_snap_strictness "proximity";
  check_scroll_snap_strictness "mandatory"

let test_svg_direction_writing () =
  check_svg_paint "none";
  check_svg_paint "currentcolor";
  (* Spec: allow url() paint servers with optional fallback color *)
  check_svg_paint "url(#grad) red";
  check_direction "ltr";
  check_direction "rtl";
  check_direction "inherit";
  check_unicode_bidi "normal";
  check_unicode_bidi "embed";
  check_unicode_bidi "isolate";
  check_unicode_bidi "bidi-override";
  check_unicode_bidi "isolate-override";
  check_unicode_bidi "plaintext";
  check_unicode_bidi "inherit";
  check_writing_mode "horizontal-tb";
  check_writing_mode "vertical-rl";
  check_writing_mode "vertical-lr";
  check_writing_mode "inherit"

(* Intentional vendor/legacy properties: kept for compatibility coverage *)
let test_vendor_misc () =
  check_webkit_appearance "none";
  check_webkit_appearance "auto";
  check_webkit_appearance "button";
  check_webkit_appearance "textfield";
  check_webkit_appearance "inherit";
  check_webkit_font_smoothing "auto";
  check_webkit_font_smoothing "antialiased";
  check_webkit_font_smoothing "subpixel-antialiased";
  check_webkit_font_smoothing "inherit";
  check_moz_osx_font_smoothing "auto";
  check_moz_osx_font_smoothing "grayscale";
  check_moz_osx_font_smoothing "inherit";
  check_webkit_box_orient "horizontal";
  check_webkit_box_orient "vertical";
  check_webkit_box_orient "inherit";
  check_forced_color_adjust "none";
  check_forced_color_adjust "auto";
  check_forced_color_adjust "inherit";
  check_appearance "none";
  check_appearance "auto";
  check_appearance "button";
  check_appearance "textfield";
  check_appearance "menulist";
  check_appearance "inherit"

let test_clear_float_vertical_outline () =
  check_clear "none";
  check_clear "left";
  check_clear "right";
  check_clear "both";
  check_float_side "none";
  check_float_side "left";
  check_float_side "right";
  check_float_side "inline-start";
  check_float_side "inline-end";
  check_float_side "inherit";
  check_text_decoration_skip_ink "auto";
  check_text_decoration_skip_ink "none";
  check_text_decoration_skip_ink "all";
  check_text_decoration_skip_ink "inherit";
  check_vertical_align "baseline";
  check_vertical_align "top";
  check_vertical_align "middle";
  check_vertical_align "bottom";
  check_vertical_align "text-top";
  check_vertical_align "text-bottom";
  check_vertical_align "sub";
  check_vertical_align "super";
  check_vertical_align "inherit";
  check_outline_style "none";
  check_outline_style "solid";
  check_outline_style "dashed";
  check_outline_style "dotted";
  check_outline_style "double";
  check_outline_style "groove";
  check_outline_style "ridge";
  check_outline_style "inset";
  check_outline_style "outset";
  check_outline_style "auto"

let test_fonts_misc_effects () =
  check_font_family "sans-serif";
  check_font_family "serif";
  check_font_family "monospace";
  check_font_family "cursive";
  check_font_family "fantasy";
  check_font_family "system-ui";
  check_font_stretch "normal";
  check_font_stretch "50%";
  check_font_stretch "ultra-condensed";
  check_font_stretch "ultra-expanded";
  check_font_stretch "inherit";
  check_font_variant_numeric "normal";
  check_font_variant_numeric "lining-nums";
  check_font_variant_numeric "tabular-nums";
  check_font_feature_settings "normal";
  check_font_feature_settings "inherit";
  (* Spec: quoted feature tags, with optional numeric toggle or on/off; lists
     allowed *)
  check_font_feature_settings "\"kern\"";
  check_font_feature_settings "\"liga\" 0";
  check_font_feature_settings "\"kern\" 1, \"liga\" 0";
  check_font_variation_settings "normal";
  check_font_variation_settings "inherit";
  (* Spec: axis tags must be quoted 4-char strings with numeric values; lists
     allowed *)
  check_font_variation_settings "\"wght\" 350";
  check_font_variation_settings "\"wght\" 350, \"wdth\" 120";
  check_backface_visibility "visible";
  check_backface_visibility "hidden";
  check_backface_visibility "inherit";
  check_scale "none";
  check_scale "1";
  check_scale ~expected:".5" "0.5";
  check_timing_function "ease";
  check_timing_function "linear";
  check_timing_function "ease-in";
  check_timing_function "ease-out";
  check_timing_function "ease-in-out";
  check_timing_function "step-start";
  check_timing_function "step-end";
  check_transition_property "all";
  check_transition_property "none";
  check_transition_property "opacity";
  check_transition "opacity";
  check_transition "transform";
  check_animation_direction "normal";
  check_animation_direction "reverse";
  check_animation_direction "alternate";
  check_animation_direction "alternate-reverse";
  check_animation_fill_mode "none";
  check_animation_fill_mode "forwards";
  check_animation_fill_mode "backwards";
  check_animation_fill_mode "both";
  check_animation_iteration_count "infinite";
  check_animation_iteration_count "1";
  check_animation_iteration_count "2.5";
  check_animation_play_state "running";
  check_animation_play_state "paused";
  check_animation "fade";
  check_animation "spin";
  check_blend_mode "normal";
  check_blend_mode "multiply";
  check_blend_mode "screen";
  check_blend_mode "overlay";
  check_blend_mode "darken";
  check_blend_mode "lighten";
  check_blend_mode "color-dodge";
  check_blend_mode "color-burn";
  check_blend_mode "hard-light";
  check_blend_mode "soft-light";
  check_blend_mode "difference";
  check_blend_mode "exclusion";
  check_blend_mode "hue";
  check_blend_mode "saturation";
  check_blend_mode "color";
  check_blend_mode "luminosity"

let test_shadows_filters_background () =
  check_text_shadow "none";
  check_text_shadow "inherit";
  check_text_shadow "2px 2px";
  (* h-offset v-offset *)
  check_text_shadow "2px 2px 4px";
  (* h-offset v-offset blur *)
  check_text_shadow "2px 2px red";
  (* h-offset v-offset color *)
  check_text_shadow "2px 2px 4px red";
  (* h-offset v-offset blur color *)
  check_text_shadow "red 2px 2px" ~expected:"2px 2px red";
  (* color first normalizes to end *)
  check_text_shadow "red 2px 2px 4px" ~expected:"2px 2px 4px red";
  (* color first normalizes to end *)
  check_text_shadow "-2px -2px";
  (* negative offsets *)
  check_text_shadow "0 0 10px";

  (* zero offsets with blur *)

  (* Box-shadow tests - comprehensive coverage *)
  check_box_shadow "none";
  check_box_shadow "inherit";
  check_box_shadow "2px 2px";
  (* h-offset v-offset *)
  check_box_shadow "2px 2px 4px";
  (* h-offset v-offset blur *)
  check_box_shadow "2px 2px 4px 1px";
  (* h-offset v-offset blur spread *)
  check_box_shadow "2px 2px red";
  (* h-offset v-offset color *)
  check_box_shadow "2px 2px 4px red";
  (* h-offset v-offset blur color *)
  check_box_shadow "2px 2px 4px 1px red";
  (* h-offset v-offset blur spread color *)
  check_box_shadow "red 2px 2px" ~expected:"2px 2px red";
  (* color first normalizes to end *)
  check_box_shadow "red 2px 2px 4px" ~expected:"2px 2px 4px red";
  (* color first normalizes to end *)
  check_box_shadow "red 2px 2px 4px 1px" ~expected:"2px 2px 4px 1px red";
  (* color first normalizes to end *)
  check_box_shadow "inset 2px 2px";
  (* inset h-offset v-offset *)
  check_box_shadow "inset 2px 2px 4px";
  (* inset h-offset v-offset blur *)
  check_box_shadow "inset 2px 2px 4px 1px";
  (* inset h-offset v-offset blur spread *)
  check_box_shadow "inset 2px 2px red";
  (* inset h-offset v-offset color *)
  check_box_shadow "inset 2px 2px 4px red";
  (* inset h-offset v-offset blur color *)
  check_box_shadow "inset 2px 2px 4px 1px red";
  (* inset h-offset v-offset blur spread color *)
  check_box_shadow "inset red 2px 2px" ~expected:"inset 2px 2px red";
  (* color normalizes to end *)
  check_box_shadow "inset red 2px 2px 4px" ~expected:"inset 2px 2px 4px red";
  (* color normalizes to end *)
  check_box_shadow "inset red 2px 2px 4px 1px"
    ~expected:"inset 2px 2px 4px 1px red";
  (* color normalizes to end *)
  (* List variant for shadows *)
  check_box_shadow "none";
  check_box_shadow "2px 2px,4px 4px red";

  check_filter "none";
  check_background_attachment "scroll";
  check_background_attachment "fixed";
  check_background_attachment "local";
  check_background_attachment "inherit";
  check_background_repeat "repeat";
  check_background_repeat "repeat-x";
  check_background_repeat "repeat-y";
  check_background_repeat "no-repeat";
  check_background_repeat "space";
  check_background_repeat "round";
  check_background_repeat "inherit";
  check_background_size "auto";
  check_background_size "cover";
  check_background_size "contain";
  check_background_size "inherit";
  check_background_image "none";
  check_background_image "url(./img.png)";
  (* Gradients are supported for background-image; expect minified commas *)
  check_background_image ~expected:"linear-gradient(to right,red,blue)"
    "linear-gradient(to right, red, blue)"

let test_property_names () =
  let to_s : type a. a property -> string =
   fun prop -> Css.Pp.to_string pp_property prop
  in
  (* Test color properties *)
  check string "property name" "background-color" (to_s Background_color);
  check string "property name" "color" (to_s Color);
  check string "property name" "border-color" (to_s Border_color);
  check string "property name" "outline-color" (to_s Outline_color);
  (* Test border style property *)
  check string "property name" "border-style" (to_s Border_style);
  (* Test length properties *)
  check string "property name" "padding-left" (to_s Padding_left);
  check string "property name" "margin-top" (to_s Margin_top);
  check string "property name" "width" (to_s Width);
  check string "property name" "height" (to_s Height);
  check string "property name" "font-size" (to_s Font_size);
  check string "property name" "line-height" (to_s Line_height);
  (* Test other properties *)
  check string "property name" "display" (to_s Display);
  check string "property name" "position" (to_s Position);
  check string "property name" "visibility" (to_s Visibility);
  check string "property name" "z-index" (to_s Z_index);
  check string "property name" "transform" (to_s Transform);
  check string "property name" "cursor" (to_s Cursor)

let test_pp_property_value () =
  check_property_value "10px" (Width, Css.Values.Px 10.);
  check_property_value "red" (Color, Css.Values.Named Css.Values.Red);
  check_property_value "url(./x.png),none"
    (Background_image, [ Url "./x.png"; None ]);
  check_property_value "none" (Transform, [ None ]);
  check_property_value "\"hello\"" (Content, String "hello")

let test_grid_template_line () =
  check_grid_template "none";
  check_grid_template "subgrid";
  check_grid_line "auto";
  (* span with number requires space per CSS spec *)
  check_grid_line "span 2";
  check_grid_line "3";
  check_grid_line "name"

let test_place_align_justify_flex () =
  check_align_items "center";
  check_align_items "flex-start";
  check_align_items "flex-end";
  check_align_items "baseline";
  check_align_items "stretch";
  check_justify_content "center";
  check_justify_content "flex-start";
  check_justify_content "flex-end";
  check_justify_content "space-between";
  check_justify_content "space-around";
  check_place_items "center";
  check_place_items "start end";
  check_place_content "center";
  check_place_content "space-between";
  check_justify_content "space-evenly";
  check_flex "none";
  check_flex "auto";
  check_flex "initial";
  check_flex "1";
  check_flex "2";
  (* Basis-only forms per spec: equivalent to 1 1 <basis> *)
  check_flex "10px";
  check_flex "50%";
  check_flex "content";
  check_place_items "center";
  check_place_items "stretch baseline"

let test_transform () =
  (* Transform tests - comprehensive coverage *)
  check_transform "none";

  (* Translate functions *)
  check_transform "translateX(10px)";
  check_transform "translateX(-50%)";
  check_transform "translateY(2em)";
  check_transform "translateZ(100px)";
  check_transform "translate(10px)";
  check_transform "translate(10px, 20px)" ~expected:"translate(10px,20px)";
  check_transform "translate3d(10px, 20px, 30px)"
    ~expected:"translate3d(10px,20px,30px)";

  (* Rotate functions *)
  check_transform "rotate(45deg)";
  check_transform "rotate(0.5turn)" ~expected:"rotate(.5turn)";
  check_transform "rotate(3.14rad)";
  check_transform "rotateX(45deg)";
  check_transform "rotateY(90deg)";
  check_transform "rotateZ(180deg)";
  check_transform "rotate3d(1, 0, 0, 45deg)" ~expected:"rotate3d(1,0,0,45deg)";
  check_transform "rotate3d(0, 1, 0, 90deg)" ~expected:"rotate3d(0,1,0,90deg)";
  check_transform "rotate3d(1, 1, 1, 60deg)" ~expected:"rotate3d(1,1,1,60deg)";

  (* Scale functions *)
  check_transform "scale(2)";
  check_transform "scale(0.5)" ~expected:"scale(.5)";
  check_transform "scale(2, 3)" ~expected:"scale(2,3)";
  check_transform "scaleX(2)";
  check_transform "scaleY(0.5)" ~expected:"scaleY(.5)";
  check_transform "scaleZ(1.5)";
  check_transform "scale3d(2, 3, 4)" ~expected:"scale3d(2,3,4)";

  (* Skew functions *)
  check_transform "skew(30deg)";
  check_transform "skew(30deg, 45deg)" ~expected:"skew(30deg,45deg)";
  check_transform "skewX(45deg)";
  check_transform "skewY(30deg)";

  (* Matrix functions *)
  check_transform "matrix(1, 0, 0, 1, 0, 0)" ~expected:"matrix(1,0,0,1,0,0)";
  check_transform "matrix(0.866, 0.5, -0.5, 0.866, 0, 0)"
    ~expected:"matrix(.866,.5,-.5,.866,0,0)";
  check_transform "matrix3d(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)"
    ~expected:"matrix3d(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1)";

  (* Transform style *)
  check_transform_style "flat";
  check_transform_style "preserve-3d";

  (* Transform origin *)
  check_transform_origin "center";
  check_transform_origin "left top";
  check_transform_origin "50% 50% 10px"

let test_gap () =
  check_gap "10px";
  check_gap "1rem 2rem";
  check_gap ~expected:"0" "0px"

let test_font_variant_numeric_token () =
  check_font_variant_numeric_token "normal";
  check_font_variant_numeric_token "ordinal";
  check_font_variant_numeric_token "slashed-zero"

let test_gradients_direction_stop () =
  check_gradient_direction "to right";
  check_gradient_direction "45deg";
  (* Corners are also supported *)
  check_gradient_direction "to bottom right";
  check_gradient_stop "red"

let test_overscroll_aspect_content () =
  check_overscroll_behavior "auto";
  check_overscroll_behavior "contain";
  check_overscroll_behavior "none";
  check_overscroll_behavior "inherit";
  check_aspect_ratio "auto";
  check_aspect_ratio "inherit";
  check_aspect_ratio "16/9";
  check_aspect_ratio "4/3";
  check_aspect_ratio "1.5/2.5";
  (* content: strings must be quoted per CSS spec *)
  check_content "\"hello\"";
  check_content "none"

let test_grid_auto_flow () =
  check_grid_auto_flow "row";
  check_grid_auto_flow "column";
  check_grid_auto_flow "dense"

let test_grid_track_size () =
  (* Conservative inputs supported by current reader *)
  check_grid_track_size "auto";
  check_grid_track_size "10px"

(* Any-property value printing for representative properties *)
let test_pp_property_value_samples () =
  let to_s f = Css.Pp.to_string ~minify:true f in
  let ppv = pp_property_value in
  let check_pp name expected prop value =
    let actual = to_s ppv (prop, value) in
    check string name expected actual
  in
  check_pp "width 10px" "10px" Width (Css.Values.Px 10.);
  check string "color red" "red"
    (to_s ppv (Color, Css.Values.Named Css.Values.Red));
  let imgs : background_image list = [ Url "./x.png"; None ] in
  check string "background-image list" "url(./x.png),none"
    (to_s ppv (Background_image, imgs));
  check string "transform none" "none"
    (to_s ppv (Transform, ([ None ] : transform list)));
  check string "content hello" "\"hello\""
    (to_s ppv (Content, (String "hello" : content)))

let test_negative_property_values () =
  let open Css.Reader in
  let neg reader s =
    let r = of_string s in
    let result = option reader r in
    Alcotest.(check bool) ("should reject: " ^ s) true (Option.is_none result)
  in
  neg read_align_items "diagonal";
  neg read_justify_content "aroundish";
  neg read_grid_auto_flow "stack";
  neg read_aspect_ratio "1";
  neg read_overscroll_behavior "bounce";
  neg read_background_size "bogus";
  neg read_content_visibility "supervisible";
  neg read_shadow "10px";
  neg read_transform "invalidfunc()";
  neg read_transform "translate3d(10px,20px)";
  neg read_transform "scale3d(1,2)";
  neg read_transform "matrix(1,2,3,4,5)";
  neg read_transform "matrix3d(1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0)";
  neg read_background_image "linear-gradient(red)";
  neg read_background_image "radial-gradient(red)";
  neg read_transitions "";
  neg read_animations "";
  (* Note: blur(5px)contrast(1.2) is actually valid CSS - functions don't require spaces between them *)
  (* Content property: unquoted strings are invalid per CSS spec *)
  neg read_content "hello";
  (* Invalid gradient direction and angle unit in property context *)
  neg read_gradient_direction "to middle";
  neg read_gradient_direction "twentydeg";
  (* font-weight out of range *)
  neg read_font_weight "0";
  (* Alignment "safe" duplicates invalid *)
  neg read_justify_self "safe safe";
  neg read_align_content "safe safe";
  neg read_justify_items "safe safe";
  (* Invalid scroll-snap-type combo *)
  neg read_scroll_snap_type "x invalid";
  (* box-shadow: 'none' should not be listable *)
  neg read_box_shadow "none,none";
  (* cursor url without mandatory keyword fallback at end *)
  neg read_cursor "url(a.cur)";
  (* contain duplicate tokens is invalid *)
  neg read_contain "size size";
  (* font-variation-settings must have numeric value per axis; 4-char tags *)
  neg read_font_variation_settings "\"wght\"";
  neg read_font_variation_settings "\"weight\" 350";
  (* font-feature-settings identifiers must be quoted *)
  neg read_font_feature_settings "kern";
  (* svg-paint invalid url *)
  neg read_svg_paint "url()"

let test_typed_positions () =
  let to_s f = Css.Pp.to_string ~minify:true f in
  let ppv = pp_property_value in
  (* background-position: list *)
  check string "bg-pos center" "center"
    (to_s ppv (Background_position, [ Center ]));
  check string "bg-pos left, top" "left center,center top"
    (to_s ppv (Background_position, [ pos_left; pos_top ]));
  (* object-position: single *)
  check string "object-pos bottom" "center bottom"
    (to_s ppv (Object_position, pos_bottom));
  (* transform-origin: 2D and 3D *)
  check string "origin left top" "left top"
    (to_s ppv (Transform_origin, Left_top));
  check string "origin3d right bottom 10px" "100% 100% 10px"
    (to_s ppv (Transform_origin, origin3d (Pct 100.) (Pct 100.) (Px 10.)))

let test_background_box () =
  let check =
    check_value "background_box" pp_background_box read_background_box
  in
  check "border-box";
  check "padding-box";
  check "content-box"

let test_background () =
  let check = check_value "background" pp_background read_background in
  check "red";
  check "url(image.png)";
  check ~expected:"linear-gradient(to right,red,blue)"
    "linear-gradient(to right, red, blue)";
  check ~expected:"url(image.png) center/cover no-repeat fixed red"
    "red url(image.png) center/cover no-repeat fixed";
  check "none"

let test_filter () =
  let check = check_value "filter" pp_filter read_filter in
  check "none";
  check "blur(5px)";
  check ~expected:"blur(5px) contrast(1.2)" "blur(5px) contrast(1.2)";
  check ~expected:"hue-rotate(30deg) opacity(.5)"
    "hue-rotate(30deg) opacity(0.5)";
  check ~expected:"drop-shadow(2px 4px 6px red)" "drop-shadow(2px 4px 6px red)"

(** Test that font family lists in custom declarations are properly printed *)
let test_font_family_custom_declarations () =
  let fonts = [ Ui_sans_serif; System_ui; Sans_serif; Apple_color_emoji ] in
  let decl, _var =
    Css.Variables.var "--test-fonts" Css.Declaration.Font_family fonts
  in
  let css_output = Css.Declaration.string_of_value decl in
  let expected = "ui-sans-serif,system-ui,sans-serif,\"Apple Color Emoji\"" in
  check string "font family list in custom declaration" expected css_output

let test_font_family_with_var_fallback () =
  let minified =
    "var(--default-font-family,ui-sans-serif,system-ui,sans-serif,\"Apple \
     Color Emoji\")"
  in
  check_font_family minified;

  let pretty =
    "var(--default-font-family, ui-sans-serif, system-ui, sans-serif, \"Apple \
     Color Emoji\")"
  in
  check_font_family ~minify:false ~expected:pretty minified

let tests =
  [
    test_case "display" `Quick test_display;
    test_case "position" `Quick test_position;
    test_case "overflow" `Quick test_overflow;
    test_case "border-style" `Quick test_border_style;
    test_case "border" `Quick test_border;
    test_case "visibility" `Quick test_visibility;
    test_case "z-index" `Quick test_z_index;
    test_case "flex-direction" `Quick test_flex_direction;
    test_case "flex-wrap" `Quick test_flex_wrap;
    test_case "align-self" `Quick test_align_self;
    test_case "font-style" `Quick test_font_style;
    test_case "css-wide subset" `Quick test_css_wide_keywords_subset;
    test_case "text-align" `Quick test_text_align;
    test_case "text-decoration-style" `Quick test_text_decoration_style;
    test_case "text-overflow" `Quick test_text_overflow;
    test_case "text-wrap" `Quick test_text_wrap;
    test_case "white-space" `Quick test_white_space;
    test_case "word-break" `Quick test_word_break;
    test_case "overflow-wrap" `Quick test_overflow_wrap;
    test_case "hyphens" `Quick test_hyphens;
    test_case "line-height" `Quick test_line_height;
    test_case "list-style" `Quick test_list_style;
    test_case "table-layout" `Quick test_table_layout;
    test_case "border-collapse" `Quick test_border_collapse;
    test_case "user/pointer" `Quick test_user_pointer;
    test_case "touch/resize/box-sizing" `Quick test_touch_resize_box_sizing;
    test_case "object/content-visibility" `Quick
      test_object_and_content_visibility;
    test_case "container/contain" `Quick test_container_and_contain;
    test_case "isolation/scroll" `Quick test_isolation_and_scroll;
    test_case "scroll-snap" `Quick test_scroll_snap;
    test_case "svg+direction+writing" `Quick test_svg_direction_writing;
    test_case "vendor/misc" `Quick test_vendor_misc;
    test_case "clear/float/vertical/outline" `Quick
      test_clear_float_vertical_outline;
    test_case "fonts/misc/effects" `Quick test_fonts_misc_effects;
    test_case "shadows/filters/background" `Quick
      test_shadows_filters_background;
    test_case "property names" `Quick test_property_names;
    (* Negative property tokens via try_parse to assert failure behavior *)
    test_case "negative property values" `Quick test_negative_property_values;
    test_case "pp_property_value samples" `Quick test_pp_property_value_samples;
    test_case "typed positions helpers" `Quick test_typed_positions;
    (* Additional coverage for missing readers *)
    test_case "grid auto-flow" `Quick test_grid_auto_flow;
    test_case "grid track-size" `Quick test_grid_track_size;
    test_case "grid template+line" `Quick test_grid_template_line;
    test_case "place/align/justify/flex" `Quick test_place_align_justify_flex;
    test_case "transform" `Quick test_transform;
    test_case "gradients (direction/stop)" `Quick test_gradients_direction_stop;
    test_case "overscroll/aspect/content" `Quick test_overscroll_aspect_content;
    test_case "background_box" `Quick test_background_box;
    test_case "background" `Quick test_background;
    test_case "filter" `Quick test_filter;
    test_case "font family custom declarations" `Quick
      test_font_family_custom_declarations;
    test_case "pp property value" `Quick test_pp_property_value;
    test_case "font family with var fallback" `Quick
      test_font_family_with_var_fallback;
  ]

(* Tests for newly added check functions *)
let test_font_weight () =
  check_font_weight "normal";
  check_font_weight "bold";
  check_font_weight "700";
  check_font_weight "1000";
  check_font_weight "lighter"

let test_text_transform () =
  check_text_transform "uppercase";
  check_text_transform "lowercase";
  check_text_transform "capitalize";
  check_text_transform "none"

let test_text_decoration_line () =
  check_text_decoration_line "underline";
  check_text_decoration_line "overline";
  check_text_decoration_line "line-through"

let test_cursor () =
  check_cursor "pointer";
  check_cursor "default";
  check_cursor "text";
  check_cursor "wait";
  check_cursor "help";
  (* Spec-compliant url + hotspot + fallback keyword *)
  check_cursor ~expected:"url(./cursor.cur) 4 12,pointer"
    "url(./cursor.cur) 4 12, pointer";
  check_cursor ~expected:"url(a.cur),url(b.cur) 1 2,move"
    "url(\"a.cur\"), url(b.cur) 1 2, move"

let test_border_width () =
  check_border_width "thin";
  check_border_width "medium";
  check_border_width "thick";
  check_border_width "2px"

let test_text_decoration () =
  check_text_decoration "underline";
  check_text_decoration "line-through";
  check_text_decoration ~expected:"none" "none"

let test_text_decoration_shorthand () =
  (* Test individual parts *)
  check_text_decoration_shorthand "underline";
  check_text_decoration_shorthand "underline solid";
  check_text_decoration_shorthand "underline solid red";
  check_text_decoration_shorthand "underline solid red 2px";
  (* Test multiple lines *)
  check_text_decoration_shorthand ~expected:"underline overline"
    "underline overline";
  check_text_decoration_shorthand ~expected:"underline overline dashed"
    "underline overline dashed";
  (* Test order independence *)
  check_text_decoration_shorthand ~expected:"underline solid red"
    "red solid underline";
  check_text_decoration_shorthand ~expected:"underline wavy blue 3px"
    "3px wavy blue underline"

let test_justify_self () =
  check_justify_self "auto";
  check_justify_self "normal";
  check_justify_self "stretch";
  check_justify_self "center";
  check_justify_self "start";
  check_justify_self "end";
  check_justify_self "flex-start";
  check_justify_self "flex-end";
  check_justify_self "self-start";
  check_justify_self "self-end";
  check_justify_self "left";
  check_justify_self "right";
  check_justify_self "baseline";
  check_justify_self "first baseline";
  check_justify_self "last baseline";
  check_justify_self "unsafe center";
  check_justify_self "unsafe start";
  (* Spec: safe alignment modifier *)
  check_justify_self "safe center"

let test_align_content () =
  check_align_content "normal";
  check_align_content "baseline";
  check_align_content "first baseline";
  check_align_content "last baseline";
  check_align_content "center";
  check_align_content "start";
  check_align_content "end";
  check_align_content "flex-start";
  check_align_content "flex-end";
  check_align_content "unsafe center";
  check_align_content "safe center";
  check_align_content "space-between";
  check_align_content "space-around";
  check_align_content "space-evenly";
  check_align_content "stretch"

let test_border_shorthand () =
  check_border_shorthand "1px";
  check_border_shorthand "solid";
  check_border_shorthand "red";
  check_border_shorthand "1px solid";
  check_border_shorthand "1px solid red";
  check_border_shorthand ~expected:"solid red" "red solid";
  check_border_shorthand ~expected:"2px dashed blue" "blue 2px dashed"

let test_justify_items () =
  check_justify_items "normal";
  check_justify_items "stretch";
  check_justify_items "center";
  check_justify_items "start";
  check_justify_items "end";
  check_justify_items "flex-start";
  check_justify_items "flex-end";
  check_justify_items "self-start";
  check_justify_items "self-end";
  check_justify_items "left";
  check_justify_items "right";
  check_justify_items "baseline";
  check_justify_items "first baseline";
  check_justify_items "last baseline";
  check_justify_items "unsafe center";
  check_justify_items "safe end"

let test_transition_shorthand () =
  check_transition_shorthand "all";
  check_transition_shorthand "opacity 1s";
  check_transition_shorthand "opacity 1s ease-in";
  check_transition_shorthand ~expected:"opacity 1s ease-in .5s"
    "opacity 1s ease-in 0.5s";
  check_transition_shorthand "width 2s";
  check_transition_shorthand ~expected:"all .3s linear" "all 0.3s linear"

let test_flex_basis () =
  check_flex_basis "auto";
  check_flex_basis "content";
  check_flex_basis "0";
  check_flex_basis "100px";
  check_flex_basis "50%";
  check_flex_basis "inherit"

let test_background_shorthand () =
  check_background_shorthand "red";
  check_background_shorthand "url(image.png)";
  check_background_shorthand "center";
  check_background_shorthand "no-repeat";
  check_background_shorthand ~expected:"url(image.png) red" "red url(image.png)";
  check_background_shorthand ~expected:"url(image.png) center"
    "url(image.png) center";
  check_background_shorthand ~expected:"url(image.png) center no-repeat red"
    "red url(image.png) center no-repeat"

let test_animation_shorthand () =
  check_animation_shorthand "none";
  check_animation_shorthand "slide 1s";
  check_animation_shorthand "slide 1s ease-in";
  check_animation_shorthand ~expected:"slide 1s ease-in .5s infinite"
    "slide 1s ease-in 0.5s infinite";
  check_animation_shorthand "slide 1s infinite";
  check_animation_shorthand "slide 1s reverse";
  check_animation_shorthand "slide 1s forwards";
  check_animation_shorthand "slide 1s paused"

let test_text_size_adjust () =
  check_text_size_adjust "none";
  check_text_size_adjust "auto";
  check_text_size_adjust "100%";
  check_text_size_adjust "80%"

(* Test negative cases - values that should fail parsing *)
let test_negative_cases () =
  let try_parse reader s label =
    let r = Css.Reader.of_string s in
    try
      let _ = reader r in
      Alcotest.fail (Fmt.str "%s should have failed but didn't" label)
    with _ -> ()
  in

  (* Invalid justify-self values *)
  try_parse read_justify_self "invalid" "invalid justify-self";
  try_parse read_justify_self "safe safe" "duplicate safe";

  (* Invalid align-content values *)
  try_parse read_align_content "invalid" "invalid align-content";

  (* Invalid flex-basis values *)
  try_parse read_flex_basis "invalid" "invalid flex-basis";
  try_parse read_flex_basis "-100px" "negative flex-basis";

  (* Invalid border shorthand *)
  try_parse read_border_shorthand "1px 2px" "two widths in border";

  (* Invalid text-size-adjust *)
  try_parse read_text_size_adjust "invalid" "invalid text-size-adjust";
  try_parse read_text_size_adjust "-50%" "negative text-size-adjust"

let test_any_property () =
  check_any_property "display";
  check_any_property "position";
  check_any_property "color";
  check_any_property "width";
  check_any_property "margin";
  check_any_property "padding";
  check_any_property "font-size"

let additional_tests =
  [
    test_case "font_weight" `Quick test_font_weight;
    test_case "text_transform" `Quick test_text_transform;
    test_case "text_decoration_line" `Quick test_text_decoration_line;
    test_case "text_decoration" `Quick test_text_decoration;
    test_case "cursor" `Quick test_cursor;
    test_case "border_width" `Quick test_border_width;
    (* New test cases *)
    test_case "text_decoration_shorthand" `Quick test_text_decoration_shorthand;
    test_case "justify_self" `Quick test_justify_self;
    test_case "align_content_values" `Quick test_align_content;
    test_case "border_shorthand" `Quick test_border_shorthand;
    test_case "justify_items" `Quick test_justify_items;
    test_case "transition_shorthand" `Quick test_transition_shorthand;
    test_case "flex_basis" `Quick test_flex_basis;
    test_case "background_shorthand" `Quick test_background_shorthand;
    test_case "animation_shorthand" `Quick test_animation_shorthand;
    test_case "text_size_adjust" `Quick test_text_size_adjust;
    test_case "any_property" `Quick test_any_property;
    test_case "negative_cases" `Quick test_negative_cases;
    test_case "gap" `Quick test_gap;
    test_case "font_variant_numeric_token" `Quick
      test_font_variant_numeric_token;
  ]

let suite = ("properties", tests @ additional_tests)
