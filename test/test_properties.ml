open Alcotest

let to_string pp v = Css.Pp.to_string ~minify:true pp v

(* Parse -> pretty-print (minified) and compare to expected (defaults to input),
   then re-read the pretty-printed output and ensure printing is stable. *)
let check_prop_rt ~name reader pp ?expected input =
  let expected = Option.value ~default:input expected in
  (* First pass: parse + print equals expected (minified) *)
  let t = Css.Reader.of_string input in
  let v = reader t in
  let s = to_string pp v in
  check string (Fmt.str "%s %s" name input) expected s;
  (* Roundtrip stability: read printed output and ensure idempotent printing *)
  let t2 = Css.Reader.of_string s in
  let v2 = reader t2 in
  let s2 = to_string pp v2 in
  check string (Fmt.str "roundtrip %s" name) s s2

let test_display () =
  let r = Css.Properties.read_display and p = Css.Properties.pp_display in
  List.iter
    (check_prop_rt ~name:"display" r p)
    [
      "none";
      "block";
      "inline";
      "inline-block";
      "flex";
      "inline-flex";
      "grid";
      "inline-grid";
      "flow-root";
      "table";
      "table-row";
      "table-cell";
      "table-caption";
      "table-column";
      "table-column-group";
      "table-footer-group";
      "table-header-group";
      "table-row-group";
      "inline-table";
      "list-item";
      "contents";
      "-webkit-box";
    ];
  ()

let test_position () =
  let r = Css.Properties.read_position and p = Css.Properties.pp_position in
  List.iter
    (check_prop_rt ~name:"position" r p)
    [ "static"; "relative"; "absolute"; "fixed"; "sticky" ]

let test_overflow () =
  let r = Css.Properties.read_overflow and p = Css.Properties.pp_overflow in
  List.iter
    (check_prop_rt ~name:"overflow" r p)
    [ "visible"; "hidden"; "scroll"; "auto"; "clip" ]

let test_border_style () =
  let r = Css.Properties.read_border_style
  and p = Css.Properties.pp_border_style in
  List.iter
    (check_prop_rt ~name:"border-style" r p)
    [
      "none";
      "solid";
      "dashed";
      "dotted";
      "double";
      "groove";
      "ridge";
      "inset";
      "outset";
      "hidden";
    ]

let test_visibility () =
  let r = Css.Properties.read_visibility and p = Css.Properties.pp_visibility in
  List.iter
    (check_prop_rt ~name:"visibility" r p)
    [ "visible"; "hidden"; "collapse" ]

let test_z_index () =
  let r = Css.Properties.read_z_index and p = Css.Properties.pp_z_index in
  List.iter (check_prop_rt ~name:"z-index" r p) [ "auto"; "10"; "-1" ]

let test_flex_direction () =
  let r = Css.Properties.read_flex_direction
  and p = Css.Properties.pp_flex_direction in
  List.iter
    (check_prop_rt ~name:"flex-direction" r p)
    [ "row"; "row-reverse"; "column"; "column-reverse" ]

let test_flex_wrap () =
  let r = Css.Properties.read_flex_wrap and p = Css.Properties.pp_flex_wrap in
  List.iter
    (check_prop_rt ~name:"flex-wrap" r p)
    [ "nowrap"; "wrap"; "wrap-reverse" ]

let test_align () =
  let r = Css.Properties.read_align and p = Css.Properties.pp_align in
  List.iter
    (check_prop_rt ~name:"align" r p)
    [ "normal"; "start"; "end"; "center"; "stretch" ]

let test_align_self () =
  let r = Css.Properties.read_align_self and p = Css.Properties.pp_align_self in
  List.iter
    (check_prop_rt ~name:"align-self" r p)
    [ "auto"; "flex-start"; "flex-end"; "center"; "baseline"; "stretch" ]

let test_justify () =
  let r = Css.Properties.read_justify and p = Css.Properties.pp_justify in
  List.iter
    (check_prop_rt ~name:"justify" r p)
    [
      "auto";
      "normal";
      "stretch";
      "center";
      "start";
      "end";
      "flex-start";
      "flex-end";
      "self-start";
      "self-end";
      "left";
      "right";
      "baseline";
      "inherit";
    ]

let test_font_style () =
  let r = Css.Properties.read_font_style and p = Css.Properties.pp_font_style in
  List.iter
    (check_prop_rt ~name:"font-style" r p)
    [ "normal"; "italic"; "oblique"; "inherit" ]

let test_text_align () =
  let r = Css.Properties.read_text_align and p = Css.Properties.pp_text_align in
  List.iter
    (check_prop_rt ~name:"text-align" r p)
    [ "left"; "right"; "center"; "justify"; "start"; "end"; "inherit" ]

let test_text_decoration_style () =
  let r = Css.Properties.read_text_decoration_style
  and p = Css.Properties.pp_text_decoration_style in
  List.iter
    (check_prop_rt ~name:"text-decoration-style" r p)
    [ "solid"; "double"; "dotted"; "dashed"; "wavy"; "inherit" ]

let test_text_overflow () =
  let r = Css.Properties.read_text_overflow
  and p = Css.Properties.pp_text_overflow in
  List.iter
    (check_prop_rt ~name:"text-overflow" r p)
    [ "clip"; "ellipsis"; "inherit" ]

let test_text_wrap () =
  let r = Css.Properties.read_text_wrap and p = Css.Properties.pp_text_wrap in
  List.iter
    (check_prop_rt ~name:"text-wrap" r p)
    [ "wrap"; "nowrap"; "balance"; "pretty"; "inherit" ]

let test_white_space () =
  let r = Css.Properties.read_white_space
  and p = Css.Properties.pp_white_space in
  List.iter
    (check_prop_rt ~name:"white-space" r p)
    [
      "normal";
      "nowrap";
      "pre";
      "pre-wrap";
      "pre-line";
      "break-spaces";
      "inherit";
    ]

let test_word_break () =
  let r = Css.Properties.read_word_break and p = Css.Properties.pp_word_break in
  List.iter
    (check_prop_rt ~name:"word-break" r p)
    [ "normal"; "break-all"; "keep-all"; "break-word"; "inherit" ]

let test_overflow_wrap () =
  let r = Css.Properties.read_overflow_wrap
  and p = Css.Properties.pp_overflow_wrap in
  List.iter
    (check_prop_rt ~name:"overflow-wrap" r p)
    [ "normal"; "break-word"; "anywhere"; "inherit" ]

let test_hyphens () =
  let r = Css.Properties.read_hyphens and p = Css.Properties.pp_hyphens in
  List.iter
    (check_prop_rt ~name:"hyphens" r p)
    [ "none"; "manual"; "auto"; "inherit" ]

let test_line_height () =
  let r = Css.Properties.read_line_height
  and p = Css.Properties.pp_line_height in
  (* Only test values supported by the simplified reader *)
  List.iter
    (check_prop_rt ~name:"line-height" r p)
    [ "normal"; "inherit"; "1.5"; "120%" ]

let test_list_style () =
  let r1 = Css.Properties.read_list_style_type
  and p1 = Css.Properties.pp_list_style_type in
  List.iter
    (check_prop_rt ~name:"list-style-type" r1 p1)
    [
      "none";
      "disc";
      "circle";
      "square";
      "decimal";
      "lower-alpha";
      "upper-alpha";
      "lower-roman";
      "upper-roman";
    ];
  let r2 = Css.Properties.read_list_style_position
  and p2 = Css.Properties.pp_list_style_position in
  List.iter
    (check_prop_rt ~name:"list-style-position" r2 p2)
    [ "inside"; "outside"; "inherit" ];
  let r3 = Css.Properties.read_list_style_image
  and p3 = Css.Properties.pp_list_style_image in
  List.iter
    (check_prop_rt ~name:"list-style-image" r3 p3)
    [ "none"; "inherit"; "url(https://example.com/x.png)" ]

let test_table_layout () =
  let r = Css.Properties.read_table_layout
  and p = Css.Properties.pp_table_layout in
  List.iter
    (check_prop_rt ~name:"table-layout" r p)
    [ "auto"; "fixed"; "inherit" ]

let test_border_collapse () =
  let r = Css.Properties.read_border_collapse
  and p = Css.Properties.pp_border_collapse in
  List.iter
    (check_prop_rt ~name:"border-collapse" r p)
    [ "collapse"; "separate"; "inherit" ]

let test_user_pointer () =
  let ru = Css.Properties.read_user_select
  and pu = Css.Properties.pp_user_select in
  List.iter
    (check_prop_rt ~name:"user-select" ru pu)
    [ "none"; "auto"; "text"; "all"; "contain" ];
  let rp = Css.Properties.read_pointer_events
  and pp = Css.Properties.pp_pointer_events in
  List.iter
    (check_prop_rt ~name:"pointer-events" rp pp)
    [
      "auto";
      "none";
      "visiblepainted";
      "visiblefill";
      "visiblestroke";
      "visible";
      "painted";
      "fill";
      "stroke";
      "all";
      "inherit";
    ]

let test_touch_resize_box_sizing () =
  let rt = Css.Properties.read_touch_action
  and pt = Css.Properties.pp_touch_action in
  List.iter
    (check_prop_rt ~name:"touch-action" rt pt)
    [ "auto"; "none"; "pan-x"; "pan-y"; "manipulation"; "inherit" ];
  let rr = Css.Properties.read_resize and pr = Css.Properties.pp_resize in
  List.iter
    (check_prop_rt ~name:"resize" rr pr)
    [ "none"; "both"; "horizontal"; "vertical"; "block"; "inline"; "inherit" ];
  let rb = Css.Properties.read_box_sizing
  and pb = Css.Properties.pp_box_sizing in
  List.iter
    (check_prop_rt ~name:"box-sizing" rb pb)
    [ "border-box"; "content-box"; "inherit" ]

let test_object_and_content_visibility () =
  let ro = Css.Properties.read_object_fit
  and po = Css.Properties.pp_object_fit in
  List.iter
    (check_prop_rt ~name:"object-fit" ro po)
    [ "fill"; "contain"; "cover"; "none"; "scale-down"; "inherit" ];
  let rcv = Css.Properties.read_content_visibility
  and pcv = Css.Properties.pp_content_visibility in
  List.iter
    (check_prop_rt ~name:"content-visibility" rcv pcv)
    [ "visible"; "auto"; "hidden"; "inherit" ]

let test_container_and_contain () =
  let rct = Css.Properties.read_container_type
  and pct = Css.Properties.pp_container_type in
  List.iter
    (check_prop_rt ~name:"container-type" rct pct)
    [ "normal"; "inline-size"; "size" ];
  let rc = Css.Properties.read_contain and pc = Css.Properties.pp_contain in
  List.iter
    (check_prop_rt ~name:"contain" rc pc)
    [ "none"; "strict"; "content"; "size"; "layout"; "style"; "paint" ]

let test_isolation_and_scroll () =
  let ri = Css.Properties.read_isolation and pi = Css.Properties.pp_isolation in
  List.iter
    (check_prop_rt ~name:"isolation" ri pi)
    [ "auto"; "isolate"; "inherit" ];
  let rs = Css.Properties.read_scroll_behavior
  and ps = Css.Properties.pp_scroll_behavior in
  List.iter
    (check_prop_rt ~name:"scroll-behavior" rs ps)
    [ "auto"; "smooth"; "inherit" ]

let test_scroll_snap () =
  let ra = Css.Properties.read_scroll_snap_align
  and pa = Css.Properties.pp_scroll_snap_align in
  List.iter
    (check_prop_rt ~name:"scroll-snap-align" ra pa)
    [ "none"; "start"; "end"; "center" ];
  let rr = Css.Properties.read_scroll_snap_stop
  and pr = Css.Properties.pp_scroll_snap_stop in
  List.iter
    (check_prop_rt ~name:"scroll-snap-stop" rr pr)
    [ "normal"; "always"; "inherit" ];
  let rt = Css.Properties.read_scroll_snap_type
  and pt = Css.Properties.pp_scroll_snap_type in
  List.iter (check_prop_rt ~name:"scroll-snap-type" rt pt) [ "none" ]

let test_svg_direction_writing () =
  let rs = Css.Properties.read_svg_paint and ps = Css.Properties.pp_svg_paint in
  List.iter (check_prop_rt ~name:"svg-paint" rs ps) [ "none"; "currentcolor" ];
  let rd = Css.Properties.read_direction and pd = Css.Properties.pp_direction in
  List.iter (check_prop_rt ~name:"direction" rd pd) [ "ltr"; "rtl"; "inherit" ];
  let rub = Css.Properties.read_unicode_bidi
  and pub = Css.Properties.pp_unicode_bidi in
  List.iter
    (check_prop_rt ~name:"unicode-bidi" rub pub)
    [
      "normal";
      "embed";
      "isolate";
      "bidi-override";
      "isolate-override";
      "plaintext";
      "inherit";
    ];
  let rwm = Css.Properties.read_writing_mode
  and pwm = Css.Properties.pp_writing_mode in
  List.iter
    (check_prop_rt ~name:"writing-mode" rwm pwm)
    [ "horizontal-tb"; "vertical-rl"; "vertical-lr"; "inherit" ]

let test_vendor_misc () =
  let rwa = Css.Properties.read_webkit_appearance
  and pwa = Css.Properties.pp_webkit_appearance in
  List.iter
    (check_prop_rt ~name:"-webkit-appearance" rwa pwa)
    [ "none"; "auto"; "button"; "textfield"; "inherit" ];
  let rwfs = Css.Properties.read_webkit_font_smoothing
  and pwfs = Css.Properties.pp_webkit_font_smoothing in
  List.iter
    (check_prop_rt ~name:"-webkit-font-smoothing" rwfs pwfs)
    [ "auto"; "antialiased"; "subpixel-antialiased"; "inherit" ];
  let rmoz = Css.Properties.read_moz_osx_font_smoothing
  and pmoz = Css.Properties.pp_moz_osx_font_smoothing in
  List.iter
    (check_prop_rt ~name:"-moz-osx-font-smoothing" rmoz pmoz)
    [ "auto"; "grayscale"; "inherit" ];
  let rwbo = Css.Properties.read_webkit_box_orient
  and pwbo = Css.Properties.pp_webkit_box_orient in
  List.iter
    (check_prop_rt ~name:"-webkit-box-orient" rwbo pwbo)
    [ "horizontal"; "vertical"; "inherit" ];
  let rfca = Css.Properties.read_forced_color_adjust
  and pfca = Css.Properties.pp_forced_color_adjust in
  List.iter
    (check_prop_rt ~name:"forced-color-adjust" rfca pfca)
    [ "none"; "auto"; "inherit" ];
  let rap = Css.Properties.read_appearance
  and pap = Css.Properties.pp_appearance in
  List.iter
    (check_prop_rt ~name:"appearance" rap pap)
    [ "none"; "auto"; "button"; "textfield"; "menulist"; "inherit" ]

let test_clear_float_vertical_outline () =
  let rc = Css.Properties.read_clear and pc = Css.Properties.pp_clear in
  List.iter
    (check_prop_rt ~name:"clear" rc pc)
    [ "none"; "left"; "right"; "both" ];
  let rf = Css.Properties.read_float_side
  and pf = Css.Properties.pp_float_side in
  List.iter
    (check_prop_rt ~name:"float" rf pf)
    [ "none"; "left"; "right"; "inline-start"; "inline-end"; "inherit" ];
  let rtds = Css.Properties.read_text_decoration_skip_ink
  and ptds = Css.Properties.pp_text_decoration_skip_ink in
  List.iter
    (check_prop_rt ~name:"text-decoration-skip-ink" rtds ptds)
    [ "auto"; "none"; "all"; "inherit" ];
  let rva = Css.Properties.read_vertical_align
  and pva = Css.Properties.pp_vertical_align in
  List.iter
    (check_prop_rt ~name:"vertical-align" rva pva)
    [
      "baseline";
      "top";
      "middle";
      "bottom";
      "text-top";
      "text-bottom";
      "sub";
      "super";
      "inherit";
    ];
  let ros = Css.Properties.read_outline_style
  and pos = Css.Properties.pp_outline_style in
  List.iter
    (check_prop_rt ~name:"outline-style" ros pos)
    [
      "none";
      "solid";
      "dashed";
      "dotted";
      "double";
      "groove";
      "ridge";
      "inset";
      "outset";
      "auto";
    ]

let test_fonts_misc_effects () =
  let rff = Css.Properties.read_font_family
  and pff = Css.Properties.pp_font_family in
  List.iter
    (check_prop_rt ~name:"font-family" rff pff)
    [ "sans-serif"; "serif"; "monospace"; "cursive"; "fantasy"; "system-ui" ];
  let rfs = Css.Properties.read_font_stretch
  and pfs = Css.Properties.pp_font_stretch in
  List.iter
    (check_prop_rt ~name:"font-stretch" rfs pfs)
    [ "normal"; "50%"; "ultra-condensed"; "ultra-expanded"; "inherit" ];
  let rfn = Css.Properties.read_font_variant_numeric
  and pfn = Css.Properties.pp_font_variant_numeric in
  List.iter
    (check_prop_rt ~name:"font-variant-numeric" rfn pfn)
    [ "normal"; "lining-nums"; "tabular-nums" ];
  let rffs = Css.Properties.read_font_feature_settings
  and pffs = Css.Properties.pp_font_feature_settings in
  List.iter
    (check_prop_rt ~name:"font-feature-settings" rffs pffs)
    [ "normal"; "inherit"; "\"kern\"" ];
  let rfvs = Css.Properties.read_font_variation_settings
  and pfvs = Css.Properties.pp_font_variation_settings in
  List.iter
    (check_prop_rt ~name:"font-variation-settings" rfvs pfvs)
    [ "normal"; "inherit"; "\"wght\"" ];
  let rbfv = Css.Properties.read_backface_visibility
  and pbfv = Css.Properties.pp_backface_visibility in
  List.iter
    (check_prop_rt ~name:"backface-visibility" rbfv pbfv)
    [ "visible"; "hidden"; "inherit" ];
  let rscale = Css.Properties.read_scale and pscale = Css.Properties.pp_scale in
  List.iter (check_prop_rt ~name:"scale" rscale pscale) [ "none"; "1"; "0.5" ];
  let rtf = Css.Properties.read_timing_function
  and ptf = Css.Properties.pp_timing_function in
  List.iter
    (check_prop_rt ~name:"timing-function" rtf ptf)
    [
      "ease";
      "linear";
      "ease-in";
      "ease-out";
      "ease-in-out";
      "step-start";
      "step-end";
    ];
  let rtp = Css.Properties.read_transition_property
  and ptp = Css.Properties.pp_transition_property in
  List.iter
    (check_prop_rt ~name:"transition-property" rtp ptp)
    [ "all"; "none"; "opacity" ];
  let rt = Css.Properties.read_transition
  and pt = Css.Properties.pp_transition in
  List.iter (check_prop_rt ~name:"transition" rt pt) [ "opacity"; "transform" ];
  let rad = Css.Properties.read_animation_direction
  and pad = Css.Properties.pp_animation_direction in
  List.iter
    (check_prop_rt ~name:"animation-direction" rad pad)
    [ "normal"; "reverse"; "alternate"; "alternate-reverse" ];
  let raf = Css.Properties.read_animation_fill_mode
  and paf = Css.Properties.pp_animation_fill_mode in
  List.iter
    (check_prop_rt ~name:"animation-fill-mode" raf paf)
    [ "none"; "forwards"; "backwards"; "both" ];
  let raic = Css.Properties.read_animation_iteration_count
  and paic = Css.Properties.pp_animation_iteration_count in
  List.iter
    (check_prop_rt ~name:"animation-iteration-count" raic paic)
    [ "infinite"; "1"; "2.5" ];
  let raps = Css.Properties.read_animation_play_state
  and paps = Css.Properties.pp_animation_play_state in
  List.iter
    (check_prop_rt ~name:"animation-play-state" raps paps)
    [ "running"; "paused" ];
  let ra = Css.Properties.read_animation and pa = Css.Properties.pp_animation in
  List.iter (check_prop_rt ~name:"animation" ra pa) [ "fade"; "spin" ];
  let rbm = Css.Properties.read_blend_mode
  and pbm = Css.Properties.pp_blend_mode in
  List.iter
    (check_prop_rt ~name:"mix-blend-mode" rbm pbm)
    [
      "normal";
      "multiply";
      "screen";
      "overlay";
      "darken";
      "lighten";
      "color-dodge";
      "color-burn";
      "hard-light";
      "soft-light";
      "difference";
      "exclusion";
      "hue";
      "saturation";
      "color";
      "luminosity";
    ]

let test_shadows_filters_background () =
  let rts = Css.Properties.read_text_shadow
  and pts = Css.Properties.pp_text_shadow in
  List.iter (check_prop_rt ~name:"text-shadow" rts pts) [ "none" ];
  let rf = Css.Properties.read_filter and pf = Css.Properties.pp_filter in
  List.iter (check_prop_rt ~name:"filter" rf pf) [ "none" ];
  let rba = Css.Properties.read_background_attachment
  and pba = Css.Properties.pp_background_attachment in
  List.iter
    (check_prop_rt ~name:"background-attachment" rba pba)
    [ "scroll"; "fixed"; "local"; "inherit" ];
  let rbr = Css.Properties.read_background_repeat
  and pbr = Css.Properties.pp_background_repeat in
  List.iter
    (check_prop_rt ~name:"background-repeat" rbr pbr)
    [
      "repeat"; "repeat-x"; "repeat-y"; "no-repeat"; "space"; "round"; "inherit";
    ];
  let rbs = Css.Properties.read_background_size
  and pbs = Css.Properties.pp_background_size in
  List.iter
    (check_prop_rt ~name:"background-size" rbs pbs)
    [ "auto"; "cover"; "contain"; "inherit" ];
  let rbi = Css.Properties.read_background_image
  and pbi = Css.Properties.pp_background_image in
  List.iter
    (check_prop_rt ~name:"background-image" rbi pbi)
    [ "none"; "url(./img.png)" ]

let test_property_names () =
  let to_s : type a. a Css.Properties.property -> string =
   fun prop -> Css.Pp.to_string Css.Properties.pp_property prop
  in
  (* Test color properties *)
  check string "property name" "background-color"
    (to_s Css.Properties.Background_color);
  check string "property name" "color" (to_s Css.Properties.Color);
  check string "property name" "border-color" (to_s Css.Properties.Border_color);
  check string "property name" "outline-color"
    (to_s Css.Properties.Outline_color);
  (* Test border style property *)
  check string "property name" "border-style" (to_s Css.Properties.Border_style);
  (* Test length properties *)
  check string "property name" "padding-left" (to_s Css.Properties.Padding_left);
  check string "property name" "margin-top" (to_s Css.Properties.Margin_top);
  check string "property name" "width" (to_s Css.Properties.Width);
  check string "property name" "height" (to_s Css.Properties.Height);
  check string "property name" "font-size" (to_s Css.Properties.Font_size);
  check string "property name" "line-height" (to_s Css.Properties.Line_height);
  (* Test other properties *)
  check string "property name" "display" (to_s Css.Properties.Display);
  check string "property name" "position" (to_s Css.Properties.Position);
  check string "property name" "visibility" (to_s Css.Properties.Visibility);
  check string "property name" "z-index" (to_s Css.Properties.Z_index);
  check string "property name" "transform" (to_s Css.Properties.Transform);
  check string "property name" "cursor" (to_s Css.Properties.Cursor)

(* Roundtrip tests for property names via read_property/pp_property *)
let pp_any_property : Css.Properties.any_property Css.Pp.t =
 fun ctx (Css.Properties.Prop p) -> Css.Properties.pp_property ctx p

let test_property_read_pp_roundtrip () =
  let r = Css.Properties.read_property and p = pp_any_property in
  List.iter
    (check_prop_rt ~name:"property" r p)
    [
      "width";
      "height";
      "color";
      "background-color";
      "display";
      "position";
      "overflow";
      "margin";
      "padding";
      "border-width";
      "font-size";
    ]

let test_pp_property_value () =
  let pp = Css.Properties.pp_property_value in
  check string "width 10px" "10px"
    (to_string pp (Css.Properties.Width, Css.Values.Px 10.));
  check string "color red" "red"
    (to_string pp (Css.Properties.Color, Css.Values.Named Css.Values.Red));
  check string "background-image list" "url(./x.png), none"
    (to_string pp
       ( Css.Properties.Background_image,
         [ Css.Properties.Url "./x.png"; Css.Properties.None ] ));
  check string "transform none" "none"
    (to_string pp (Css.Properties.Transform, [ Css.Properties.None ]));
  check string "content hello" "\"hello\""
    (to_string pp (Css.Properties.Content, Css.Properties.String "hello"))

let test_grid_template_line () =
  let rt = Css.Properties.read_grid_template
  and pt = Css.Properties.pp_grid_template in
  List.iter (check_prop_rt ~name:"grid-template" rt pt) [ "none"; "subgrid" ];
  let rl = Css.Properties.read_grid_line and pl = Css.Properties.pp_grid_line in
  check_prop_rt ~name:"grid-line" rl pl "auto";
  (* span2 pretty-prints as "span 2" *)
  check_prop_rt ~name:"grid-line" rl pl ~expected:"span 2" "span2";
  check_prop_rt ~name:"grid-line" rl pl "3";
  check_prop_rt ~name:"grid-line" rl pl "name"

let test_place_align_justify_flex () =
  let ra = Css.Properties.read_align_items
  and pa = Css.Properties.pp_align_items in
  List.iter
    (check_prop_rt ~name:"align-items" ra pa)
    [ "center"; "flex-start"; "flex-end"; "baseline"; "stretch" ];
  let rj = Css.Properties.read_justify_content
  and pj = Css.Properties.pp_justify_content in
  List.iter
    (check_prop_rt ~name:"justify-content" rj pj)
    [
      "center";
      "flex-start";
      "flex-end";
      "space-between";
      "space-around";
      "space-evenly";
    ];
  let rfi = Css.Properties.read_flex and pfi = Css.Properties.pp_flex in
  List.iter
    (check_prop_rt ~name:"flex" rfi pfi)
    [ "none"; "auto"; "initial"; "1"; "2" ];
  let rpl = Css.Properties.read_place_items
  and ppl = Css.Properties.pp_place_items in
  List.iter
    (check_prop_rt ~name:"place-items" rpl ppl)
    [ "center"; "stretch baseline" ]

let test_transform_box_shadow_minimal () =
  let rt = Css.Properties.read_transform and pt = Css.Properties.pp_transform in
  List.iter (check_prop_rt ~name:"transform" rt pt) [ "none" ];
  (* Simplified box-shadow reader only handles "none" *)
  let rbs = Css.Properties.read_box_shadow
  and pbs = Css.Properties.pp_box_shadow in
  List.iter (check_prop_rt ~name:"box-shadow" rbs pbs) [ "none" ];
  (* Lists of box-shadows *)
  let rlsl t =
    let rec read_list acc t =
      let shadow = Css.Properties.read_box_shadow t in
      let acc = shadow :: acc in
      Css.Reader.skip_ws t;
      if Css.Reader.looking_at t "," then (
        Css.Reader.expect t ',';
        Css.Reader.skip_ws t;
        read_list acc t)
      else List.rev acc
    in
    read_list [] t
  and pls = Css.Pp.list ~sep:Css.Pp.comma Css.Properties.pp_box_shadow in
  List.iter (check_prop_rt ~name:"box-shadows" rlsl pls) [ "none"; "none,none" ]

let test_gradients_direction_stop () =
  let rd = Css.Properties.read_gradient_direction
  and pd = Css.Properties.pp_gradient_direction in
  List.iter
    (check_prop_rt ~name:"gradient-direction" rd pd)
    [ "to right"; "45deg" ];
  let rs = Css.Properties.read_gradient_stop
  and ps = Css.Properties.pp_gradient_stop in
  List.iter (check_prop_rt ~name:"gradient-stop" rs ps) [ "red" ]

let test_overscroll_aspect_content () =
  let ro = Css.Properties.read_overscroll_behavior
  and po = Css.Properties.pp_overscroll_behavior in
  List.iter
    (check_prop_rt ~name:"overscroll-behavior" ro po)
    [ "auto"; "contain"; "none"; "inherit" ];
  let rar = Css.Properties.read_aspect_ratio
  and par = Css.Properties.pp_aspect_ratio in
  List.iter (check_prop_rt ~name:"aspect-ratio" rar par) [ "auto"; "inherit" ];
  let rc = Css.Properties.read_content and pc = Css.Properties.pp_content in
  (* content: bare id prints as quoted string, set expected accordingly *)
  check_prop_rt ~name:"content" rc pc ~expected:"\"hello\"" "hello";
  check_prop_rt ~name:"content" rc pc "none"

let test_grid_auto_flow () =
  let r = Css.Properties.read_grid_auto_flow
  and p = Css.Properties.pp_grid_auto_flow in
  List.iter
    (check_prop_rt ~name:"grid-auto-flow" r p)
    [ "row"; "column"; "dense" ]

let test_grid_track_size () =
  let r = Css.Properties.read_grid_track_size
  and p = Css.Properties.pp_grid_track_size in
  (* Conservative inputs supported by current reader *)
  List.iter (check_prop_rt ~name:"grid-track-size" r p) [ "auto"; "10px" ]

(* Any-property value printing for representative properties *)
let test_pp_property_value_samples () =
  let to_s f = Css.Pp.to_string ~minify:true f in
  let ppv = Css.Properties.pp_property_value in
  let check_pp name expected prop value =
    let actual = to_s ppv (prop, value) in
    check string name expected actual
  in
  check_pp "width 10px" "10px" Css.Properties.Width (Css.Values.Px 10.);
  check string "color red" "red"
    (to_s ppv (Css.Properties.Color, Css.Values.Named Css.Values.Red));
  let imgs : Css.Properties.background_image list = [ Url "./x.png"; None ] in
  check string "background-image list" "url(./x.png), none"
    (to_s ppv (Css.Properties.Background_image, imgs));
  check string "transform none" "none"
    (to_s ppv
       (Css.Properties.Transform, ([ None ] : Css.Properties.transform list)));
  check string "content hello" "\"hello\""
    (to_s ppv
       (Css.Properties.Content, (String "hello" : Css.Properties.content)))

let tests =
  [
    test_case "display" `Quick test_display;
    test_case "position" `Quick test_position;
    test_case "overflow" `Quick test_overflow;
    test_case "border-style" `Quick test_border_style;
    test_case "visibility" `Quick test_visibility;
    test_case "z-index" `Quick test_z_index;
    test_case "flex-direction" `Quick test_flex_direction;
    test_case "flex-wrap" `Quick test_flex_wrap;
    test_case "align" `Quick test_align;
    test_case "align-self" `Quick test_align_self;
    test_case "justify" `Quick test_justify;
    test_case "font-style" `Quick test_font_style;
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
    test_case "read_property/pp_property roundtrip" `Quick
      test_property_read_pp_roundtrip;
    (* Negative property tokens via try_parse to assert failure behavior *)
    test_case "negative property values" `Quick (fun () ->
        let open Css.Reader in
        let neg reader s label =
          let r = of_string s in
          check bool label true (Option.is_none (try_parse reader r))
        in
        neg Css.Properties.read_align_items "diagonal" "align-items invalid";
        neg Css.Properties.read_justify_content "aroundish"
          "justify-content invalid";
        neg Css.Properties.read_grid_auto_flow "stack" "grid-auto-flow invalid";
        neg Css.Properties.read_aspect_ratio "1" "aspect-ratio missing /";
        neg Css.Properties.read_overscroll_behavior "bounce"
          "overscroll-behavior invalid";
        neg Css.Properties.read_background_size "bogus"
          "background-size invalid";
        neg Css.Properties.read_content_visibility "supervisible"
          "content-visibility invalid";
        neg Css.Properties.read_box_shadow "10px"
          "box-shadow simplified rejects";
        neg Css.Properties.read_transform "rotate(45deg)"
          "transform simplified rejects");
    test_case "pp_property_value samples" `Quick test_pp_property_value_samples;
    (* Additional coverage for missing readers *)
    test_case "grid auto-flow" `Quick (fun () ->
        let r = Css.Properties.read_grid_auto_flow
        and p = Css.Properties.pp_grid_auto_flow in
        List.iter
          (check_prop_rt ~name:"grid-auto-flow" r p)
          [ "row"; "column"; "dense" ]);
    test_case "grid track-size" `Quick (fun () ->
        let r = Css.Properties.read_grid_track_size
        and p = Css.Properties.pp_grid_track_size in
        (* Conservative inputs supported by current reader *)
        List.iter (check_prop_rt ~name:"grid-track-size" r p) [ "auto"; "10px" ]);
    test_case "grid template+line" `Quick (fun () ->
        let rt = Css.Properties.read_grid_template
        and pt = Css.Properties.pp_grid_template in
        List.iter
          (check_prop_rt ~name:"grid-template" rt pt)
          [ "none"; "subgrid" ];
        let rl = Css.Properties.read_grid_line
        and pl = Css.Properties.pp_grid_line in
        check_prop_rt ~name:"grid-line" rl pl "auto";
        (* span2 pretty-prints as "span 2" *)
        check_prop_rt ~name:"grid-line" rl pl ~expected:"span 2" "span2";
        check_prop_rt ~name:"grid-line" rl pl "3";
        check_prop_rt ~name:"grid-line" rl pl "name");
    test_case "place/align/justify/flex" `Quick (fun () ->
        let ra = Css.Properties.read_align_items
        and pa = Css.Properties.pp_align_items in
        List.iter
          (check_prop_rt ~name:"align-items" ra pa)
          [ "normal"; "center"; "stretch" ];
        let rj = Css.Properties.read_justify_content
        and pj = Css.Properties.pp_justify_content in
        List.iter
          (check_prop_rt ~name:"justify-content" rj pj)
          [ "center"; "flex-start"; "space-between" ];
        let rpc = Css.Properties.read_place_content
        and ppc = Css.Properties.pp_place_content in
        List.iter
          (check_prop_rt ~name:"place-content" rpc ppc)
          [ "normal"; "center"; "stretch" ];
        let rpi = Css.Properties.read_place_items
        and ppi = Css.Properties.pp_place_items in
        List.iter
          (check_prop_rt ~name:"place-items" rpi ppi)
          [ "normal"; "center"; "stretch" ];
        let rf = Css.Properties.read_flex and pf = Css.Properties.pp_flex in
        List.iter (check_prop_rt ~name:"flex" rf pf) [ "auto"; "none"; "1.5" ]);
    test_case "transform/box-shadow minimal" `Quick (fun () ->
        let rt = Css.Properties.read_transform
        and pt = Css.Properties.pp_transform in
        List.iter (check_prop_rt ~name:"transform" rt pt) [ "none" ];
        let rbs = Css.Properties.read_box_shadow
        and pbs = Css.Properties.pp_box_shadow in
        List.iter (check_prop_rt ~name:"box-shadow" rbs pbs) [ "none" ];
        (* List variant for shadows: use list reader/printer *)
        let rlsl = Css.Properties.read_box_shadows in
        let pls = Css.Pp.list ~sep:Css.Pp.comma Css.Properties.pp_box_shadow in
        List.iter
          (check_prop_rt ~name:"box-shadows" rlsl pls)
          [ "none"; "none,none" ]);
    test_case "gradients (direction/stop)" `Quick (fun () ->
        let rd = Css.Properties.read_gradient_direction
        and pd = Css.Properties.pp_gradient_direction in
        List.iter
          (check_prop_rt ~name:"gradient-direction" rd pd)
          [ "to right"; "45deg" ];
        let rs = Css.Properties.read_gradient_stop
        and ps = Css.Properties.pp_gradient_stop in
        List.iter (check_prop_rt ~name:"gradient-stop" rs ps) [ "red" ]);
    test_case "overscroll/aspect/content" `Quick (fun () ->
        let ro = Css.Properties.read_overscroll_behavior
        and po = Css.Properties.pp_overscroll_behavior in
        List.iter
          (check_prop_rt ~name:"overscroll-behavior" ro po)
          [ "auto"; "contain"; "none"; "inherit" ];
        let rar = Css.Properties.read_aspect_ratio
        and par = Css.Properties.pp_aspect_ratio in
        List.iter
          (check_prop_rt ~name:"aspect-ratio" rar par)
          [ "auto"; "inherit" ];
        let rc = Css.Properties.read_content
        and pc = Css.Properties.pp_content in
        (* content: bare id prints as quoted string, set expected accordingly *)
        check_prop_rt ~name:"content" rc pc ~expected:"\"hello\"" "hello";
        check_prop_rt ~name:"content" rc pc "none");
  ]

let suite = [ ("properties", tests) ]
