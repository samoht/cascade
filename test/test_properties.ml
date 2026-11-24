(** Property tests: names, printing, CSS-wide keywords, math functions and the
    checks that span several properties. *)

open Alcotest
open Cascade
open Css.Properties
open Css_test_helpers
open Properties_helpers

let check_any_property =
  check_value_cursor "any_property" read_any_property pp_any_property

let check_css_wide = check_value_cursor "css_wide" read_css_wide pp_css_wide

(* Helper for property-value pairs printing *)
let check_property_value expected (prop, value) =
  let pp = pp_property_value in
  let to_string f = Css.Pp.to_string ~minify:true f in
  let actual = to_string pp (prop, value) in
  let name = Fmt.str "%s value" (Css.Pp.to_string pp_property prop) in
  check string name expected actual

(* Verifies property constructors map to correct CSS property names *)
(* Not a roundtrip test *)
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

(* Verifies property-value pairs print correctly *)
(* Not a roundtrip test *)
let test_pp_property_value () =
  check_property_value "10px" (Width, Css.Values.Length (Css.Values.Px 10.));
  check_property_value "red" (Color, Css.Values.Named Css.Values.Red);
  check_property_value "url(./x.png),none"
    (Background_image, [ Url "./x.png"; None ]);
  check_property_value "none" (Transform, [ None ]);
  check_property_value "\"hello\"" (Content, String "hello");
  (* Additional samples *)
  let to_s f = Css.Pp.to_string ~minify:true f in
  let ppv = pp_property_value in
  check string "color red" "red"
    (to_s ppv (Color, Css.Values.Named Css.Values.Red));
  let imgs : background_image list = [ Url "./x.png"; None ] in
  check string "background-image list" "url(./x.png),none"
    (to_s ppv (Background_image, imgs));
  check string "transform none" "none" (to_s ppv (Transform, [ None ]));
  check string "content hello" "\"hello\""
    (to_s ppv (Content, (String "hello" : content)))

let any_property_names () =
  check_any_property "display";
  check_any_property "position";
  check_any_property "color";
  check_any_property "width";
  check_any_property "margin";
  check_any_property "padding";
  check_any_property "font-size";
  neg_cursor read_any_property "not-a-property";
  neg_cursor ~allow_partial:true read_any_property "font size";
  (* space instead of hyphen *)
  neg_cursor read_any_property "_private";
  (* invalid start *)
  neg_cursor read_any_property "123-prop";
  (* starts with number *)
  (* empty string *)
  neg_cursor read_any_property ""

(* CSS Values 4 (ED) sec. 10.9: a math function that resolves to <number> is
   valid wherever an <integer> is, so every integer position reads a calc(). The
   positions here are tab-size's <number> (CSS Text 4 (ED) sec. 4.4),
   column-count's <integer> and the columns shorthand that carries it (CSS
   Multicol 1 (ED) sec. 3.2 and 3.3), repeat()'s count (CSS Grid 2 (ED) sec.
   7.2.3.1) and the span count of <grid-line> (sec. 8.3). A <track-breadth> is
   not one of them: sec. 7.2.1 admits a <length-percentage> or a <flex> there,
   never a bare <number>. *)
let math_function_at_integer_positions () =
  (* [tab-size] keeps the authored call because its value is a {!number}; the
     positions below still hold an [int], so their reader folds to reach it. *)
  check_tab_size "calc(1 + 2)";
  check_tab_size "4";
  check_tab_size "10.5";
  check_tab_size "2px";
  check_column_count ~expected:"3" "calc(1 + 2)";
  check_column_count "3";
  check_columns_value ~expected:"3" "calc(1 + 2)";
  check_columns_value "2";
  check_grid_template ~expected:"repeat(3,1fr)" "repeat(calc(1 + 2),1fr)";
  check_grid_template "repeat(2,calc(1px + 2px))";
  check_grid_line ~expected:"span 3" "span calc(1 + 2)";
  check_grid_line "span 3";
  neg_cursor read_grid_template "calc(1 + 2)"

let test_css_wide () =
  check_css_wide "initial";
  check_css_wide "inherit";
  check_css_wide "unset";
  check_css_wide "revert";
  check_css_wide "revert-layer";
  neg_cursor read_css_wide "red";
  neg_cursor read_css_wide ""

let spec_property_grammar_edges () =
  check_font_feature_settings "\"kern\" on";
  check_font_feature_settings ~expected:"\"liga\" off,\"calt\" 1"
    "\"liga\" off, \"calt\" 1";
  check_font_variation_settings ~expected:"\"wght\" 650,\"wdth\" 75"
    "\"wght\" 650, \"wdth\" 75";
  check_timing_function "linear(0, .25 50%, 1)";
  check_timing_function ~expected:"steps(4,jump-none)" "steps(4, jump-none)";
  check_transform "translate(10px, 20%)" ~expected:"translate(10px,20%)";
  check_transform "rotate(1 0 0 45deg)";
  check_transform "scale(1.2 0.8)" ~expected:"scale(1.2 .8)";
  check_transforms ~expected:"translate(10px,20%)rotate(45deg)scale(1.2)"
    "translate(10px,20%) rotate(45deg) scale(1.2)";
  check_container_shorthand "card / inline-size" ~expected:"card/inline-size";
  check_container_shorthand "card / normal" ~expected:"card/normal";
  check_scroll_snap_type "x mandatory";
  check_scroll_snap_type "block proximity";
  check_clip_path "path(\"M 0 0 L 10 10\")";
  check_clip_path "xywh(0 0 100% 100% round 10px)";
  check_clip_path "rect(0px 0px 10px 10px)";
  check_content "counter(page)";
  check_content ~expected:"counters(section,\".\")" "counters(section, \".\")";
  neg_cursor read_font_feature_settings "\"kern\" -1";
  neg_cursor read_font_variation_settings "\"wg\" 400";
  neg_cursor read_timing_function "linear()";
  neg_cursor read_timing_function "steps(0, jump-end)";
  neg_cursor read_transform "rotate(1 0 45deg)";
  neg_cursor read_transform "scale(1 2 3 4)";
  neg_cursor ~allow_partial:true read_container_shorthand
    "card / inline-size / size";
  neg_cursor read_scroll_snap_type "mandatory x";
  neg_cursor read_clip_path "xywh(0 0)";
  (* CSS Shapes 1 sec. 3.1 gives [xywh()]/[rect()] a fixed quad plus an optional
     [round <'border-radius'>] tail; when that tail is not present,
     [Cursor.call] did not require the reader to consume its whole sub-cursor,
     so a trailing token past the quad was silently dropped instead of
     invalidating the declaration. *)
  neg_cursor read_clip_path "xywh(0 0 1px 1px foo)";
  neg_cursor read_clip_path "rect(0px 0px 10px 10px foo)";
  neg_cursor read_content "counter()";
  (* CSS Lists 3 sec. 4.6: [counter( <counter-name>, <counter-style>? )] and
     [counters( <counter-name>, <string>, <counter-style>? )], with
     [<counter-style> = <counter-style-name> | <symbols()>]. Chrome 153 keeps
     every name form, [none] among them, and serialises [decimal], the default,
     as nothing. *)
  check_content "counter(n,c)";
  check_content "counter(n,disc)";
  check_content "counter(n,none)";
  check_content "counter(n,Foo)";
  check_content ~expected:"counter(n,decimal)" "counter(n, DECIMAL)";
  check_content ~expected:"counter(n,symbols(cyclic\"*\"))"
    "counter(n,symbols(cyclic \"*\"))";
  check_content "counters(n,\".\",c)";
  check_content "counters(n,\".\",symbols(\"*\"))";
  check_content "\"a\" counter(n,c)";
  decl_optimizes ~prop:"content" ~into:"counter(n,c)" "counter(n,c)";
  decl_optimizes ~prop:"content" ~into:"counter(n)" "counter(n,decimal)";
  decl_optimizes ~prop:"content" ~into:"counters(n,\".\")"
    "counters(n,\".\",decimal)";
  neg_cursor read_content "counter(n,\"x\")";
  neg_cursor read_content "counter(n,inherit)";
  neg_cursor read_content "counter(n,default)";
  neg_cursor read_content "counter(n,)";
  neg_cursor read_content "counter(n,c,d)";
  neg_cursor read_content "counters(n,\".\",\"x\")";
  (* CSS Generated Content 3 sec. 1.1 follows the content with an optional [/ [
     <string> | <counter> | attr() ]+] alternative text. Chrome 153 keeps each
     of these, and refuses an empty alternative, an image or a keyword in it,
     and one after [none] or [normal]. *)
  check_content "\"a\"/\"b\"";
  check_content ~expected:"\"a\"/\"b\"" "\"a\" / \"b\"";
  check_content "url(a.png)/\"b\"";
  check_content "counter(n)/\"b\"";
  check_content "\"a\"/attr(x)";
  check_content "\"a\"/counter(n,upper-roman)";
  check_content "\"a\"/counters(n,\".\")";
  check_content "\"a\"/\"b\" attr(x) counter(n)";
  check_content "open-quote/\"x\"";
  check_content "\"a\" counter(n)/\"\"";
  check_content ~minify:false "\"a\" / \"b\" \"c\"";
  decl_optimizes ~prop:"content" ~into:"\"a\"/\"b\"" "'a' / 'b'";
  neg_cursor read_content "\"a\" /";
  neg_cursor ~allow_partial:true read_content "\"a\" / url(b.png)";
  neg_cursor ~allow_partial:true read_content "none / \"b\"";
  neg_cursor ~allow_partial:true read_content "normal / \"b\"";
  neg_cursor ~allow_partial:true read_content "\"a\" / open-quote";
  neg_cursor ~allow_partial:true read_content "\"a\" / \"b\" / \"c\""

let spec_ui_property_edges () =
  check_text_wrap "pretty";
  check_text_wrap "balance";
  check_white_space "break-spaces";
  check_word_break "auto-phrase";
  check_overflow_wrap "anywhere";
  check_hyphens "manual";
  check_field_sizing "content";
  check_appearance "base-select";
  check_user_select "contain";
  neg_cursor ~allow_partial:true read_text_wrap "pretty balance";
  neg_cursor ~allow_partial:true read_white_space "normal nowrap pre";
  neg_cursor read_word_break "auto phrase";
  neg_cursor ~allow_partial:true read_overflow_wrap "break-word anywhere";
  neg_cursor read_hyphens "soft";
  neg_cursor read_field_sizing "auto content";
  neg_cursor read_appearance "base button";
  neg_cursor ~allow_partial:true read_user_select "none text"

let spec_mask_clip_property_edges () =
  check_mask_box "view-box";
  check_mask_box "stroke-box";
  check_mask_mode "match-source";
  check_mask_composite "exclude";
  check_webkit_mask_composite "source-over";
  check_webkit_mask_source_type "auto";
  check_clip_path "shape(from 0 0, line to 100% 0, close)";
  check_clip_path ~expected:"polygon(0 0,100%0,100%100%)"
    "polygon(0 0, 100% 0, 100% 100%)";
  neg_cursor read_mask_box "margin-box";
  neg_cursor read_mask_mode "source";
  neg_cursor ~allow_partial:true read_mask_composite "add subtract";
  neg_cursor read_webkit_mask_composite "add";
  neg_cursor read_webkit_mask_source_type "match-source";
  neg_cursor read_clip_path "polygon()"

let enum_readers_accept_leading_ws () =
  List.iter
    (fun check -> check ())
    [
      (fun () -> check_position ~expected:"relative" " relative");
      (fun () -> check_flex_direction ~expected:"row" " row");
      (fun () -> check_align_self ~expected:"center" " center");
      (fun () -> check_text_align ~expected:"start" " start");
      (fun () -> check_text_decoration_style ~expected:"dashed" " dashed");
      (fun () -> check_overflow ~expected:"hidden" " hidden");
      (fun () -> check_visibility ~expected:"hidden" " hidden");
      (fun () -> check_scroll_snap_align ~expected:"start" " start");
      (fun () -> check_scroll_snap_stop ~expected:"always" " always");
      (fun () -> check_scroll_snap_axis ~expected:"inline" " inline");
      (fun () ->
        check_scroll_snap_strictness ~expected:"mandatory" " mandatory");
      (fun () -> check_user_select ~expected:"none" " none");
      (fun () -> check_pointer_events ~expected:"none" " none");
      (fun () -> check_resize ~expected:"both" " both");
      (fun () -> check_box_sizing ~expected:"border-box" " border-box");
      (fun () -> check_object_fit ~expected:"cover" " cover");
      (fun () -> check_content_visibility ~expected:"auto" " auto");
      (fun () -> check_direction ~expected:"rtl" " rtl");
      (fun () -> check_writing_mode ~expected:"vertical-rl" " vertical-rl");
      (fun () -> check_print_color_adjust ~expected:"exact" " exact");
      (fun () -> check_appearance ~expected:"button" " button");
      (fun () -> check_clear ~expected:"both" " both");
      (fun () -> check_float_side ~expected:"left" " left");
    ]

(* A square or curly block is not a grouping operand in the CSS math grammar.
   Canonical whitespace therefore leaves math context when it enters one, just
   as the component-value printer does. *)
let canonical_math_block_context () =
  let components = Cursor.remaining (Cursor.of_string "calc([f() + 1])") in
  let actual =
    components |> canonicalize_math_whitespace_components
    |> Parser.string_of_components
  in
  check string "square block ends math context" "calc([f()+ 1])" actual

(* WHATWG Compatibility Standard sec. 3.4.1 lists the [-webkit-] properties that
   are legacy name aliases of an unprefixed property. The table below is that
   list, less the [-webkit-mask-box-image] set and the three aliases a target
   engine reads under a grammar of their own; a name off it, such as
   [-webkit-user-select] or any [-moz-] spelling, is a property of its own. *)
let whatwg_legacy_aliases =
  [
    ("-webkit-align-content", "align-content");
    ("-webkit-align-items", "align-items");
    ("-webkit-align-self", "align-self");
    ("-webkit-animation", "animation");
    ("-webkit-animation-delay", "animation-delay");
    ("-webkit-animation-direction", "animation-direction");
    ("-webkit-animation-duration", "animation-duration");
    ("-webkit-animation-fill-mode", "animation-fill-mode");
    ("-webkit-animation-iteration-count", "animation-iteration-count");
    ("-webkit-animation-name", "animation-name");
    ("-webkit-animation-play-state", "animation-play-state");
    ("-webkit-animation-timing-function", "animation-timing-function");
    ("-webkit-backface-visibility", "backface-visibility");
    ("-webkit-background-clip", "background-clip");
    ("-webkit-background-size", "background-size");
    ("-webkit-border-bottom-left-radius", "border-bottom-left-radius");
    ("-webkit-border-bottom-right-radius", "border-bottom-right-radius");
    ("-webkit-border-radius", "border-radius");
    ("-webkit-border-top-left-radius", "border-top-left-radius");
    ("-webkit-border-top-right-radius", "border-top-right-radius");
    ("-webkit-box-shadow", "box-shadow");
    ("-webkit-box-sizing", "box-sizing");
    ("-webkit-filter", "filter");
    ("-webkit-flex", "flex");
    ("-webkit-flex-basis", "flex-basis");
    ("-webkit-flex-direction", "flex-direction");
    ("-webkit-flex-flow", "flex-flow");
    ("-webkit-flex-grow", "flex-grow");
    ("-webkit-flex-shrink", "flex-shrink");
    ("-webkit-flex-wrap", "flex-wrap");
    ("-webkit-justify-content", "justify-content");
    ("-webkit-mask-clip", "mask-clip");
    ("-webkit-mask-composite", "mask-composite");
    ("-webkit-mask-image", "mask-image");
    ("-webkit-mask-origin", "mask-origin");
    ("-webkit-mask-position", "mask-position");
    ("-webkit-mask-repeat", "mask-repeat");
    ("-webkit-mask-size", "mask-size");
    ("-webkit-order", "order");
    ("-webkit-perspective-origin", "perspective-origin");
    ("-webkit-transform", "transform");
    ("-webkit-transform-origin", "transform-origin");
    ("-webkit-transform-style", "transform-style");
    ("-webkit-transition", "transition");
    ("-webkit-transition-delay", "transition-delay");
    ("-webkit-transition-duration", "transition-duration");
    ("-webkit-transition-property", "transition-property");
    ("-webkit-transition-timing-function", "transition-timing-function");
  ]

let name_of (Prop p) = Css.Pp.to_string ~minify:false pp_property p

let is_typed : type a. a property -> bool = function
  | Unknown_property _ -> false
  | _ -> true

(* CLAUDE.md: a longhand that parses as [Unknown_property] is typed first, so
   every alias on the list reads as a typed property under the unprefixed
   property's grammar, never as a name the optimizer has to look up. *)
let vendor_prefix_and_aliases_are_property_facts () =
  List.iter
    (fun (alias, property) ->
      let (Prop a) = of_name alias in
      let (Prop p) = of_name property in
      check bool (alias ^ " is typed") true (is_typed a);
      check bool (alias ^ " is vendor-prefixed") true (is_vendor_prefixed a);
      check bool (property ^ " is not") false (is_vendor_prefixed p);
      check (option string)
        (alias ^ " aliases " ^ property)
        (Some property)
        (Option.map name_of (legacy_alias a));
      check (option string)
        (property ^ " aliases nothing")
        None
        (Option.map name_of (legacy_alias p)))
    whatwg_legacy_aliases;
  List.iter
    (fun name ->
      let (Prop p) = of_name name in
      check (option string)
        (name ^ " aliases nothing")
        None
        (Option.map name_of (legacy_alias p)))
    [
      "-webkit-user-select";
      (* Sec. 3.4.1 also lists the [-webkit-mask-box-image] set as aliases of
         [mask-border], but no Chromium or Firefox release reads [mask-border]
         (web-features: Safari 17.2 alone), so across the targets each is a
         property of its own. Chromium 153: [CSS.supports] is true for
         [-webkit-mask-box-image-width] and false for [mask-border-width], and
         [mask-border-width:3px] leaves [-webkit-mask-box-image-width] at [auto]
         where the prefixed spelling computes [3px]. *)
      "-webkit-mask-box-image";
      "-webkit-mask-box-image-outset";
      "-webkit-mask-box-image-repeat";
      "-webkit-mask-box-image-slice";
      "-webkit-mask-box-image-source";
      "-webkit-mask-box-image-width";
      (* Chromium 153 reads [-webkit-perspective:5] as a perspective of [5px]
         and [-webkit-background-origin:padding] as [padding-box], and refuses
         both values under the unprefixed name, so neither alias takes its
         property's grammar and cascade types neither under it. *)
      "-webkit-perspective";
      "-webkit-background-origin";
      (* WebKit declares [-webkit-mask] as a shorthand of its own, not an alias
         of [mask]: its longhands are [-webkit-mask-source-type] and
         [-webkit-mask-clip] in place of [mask-mode] and [mask-clip], and it has
         no [mask-composite] (Source/WebCore/css/CSSProperties.json, tag
         WebKit-7615.1.26.11.23 of Safari 16.4 and main). *)
      "-webkit-mask";
      "-moz-transform";
      "-moz-box-sizing";
      "-webkit-text-decoration-color";
      "-x-unknown";
    ];
  List.iter
    (fun (name, prefixed) ->
      let (Prop p) = of_name name in
      check bool name prefixed (is_vendor_prefixed p))
    [
      ("-moz-box-sizing", true);
      ("-x-unknown", true);
      ("--custom", false);
      ("page-break-before", false);
      ("unknown-property", false);
    ]

let suite =
  ( "properties",
    [
      test_case "enum readers accept leading whitespace" `Quick
        enum_readers_accept_leading_ws;
      test_case "property names" `Quick test_property_names;
      test_case "pp property value" `Quick test_pp_property_value;
      test_case "spec current property grammar edges" `Quick
        spec_property_grammar_edges;
      test_case "spec UI property edges" `Quick spec_ui_property_edges;
      test_case "spec mask and clip property edges" `Quick
        spec_mask_clip_property_edges;
      test_case "canonical math block context" `Quick
        canonical_math_block_context;
      test_case "any_property" `Quick any_property_names;
      test_case "math_function_at_integer_positions" `Quick
        math_function_at_integer_positions;
      test_case "css_wide" `Quick test_css_wide;
      test_case "vendor prefix and aliases are property facts" `Quick
        vendor_prefix_and_aliases_are_property_facts;
    ] )
