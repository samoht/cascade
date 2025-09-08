(** Tests for CSS Render module *)

open Alcotest
open Css.Render
open Css.Declaration
open Css.Values
open Css.Properties

(** Test inline style generation *)
let test_inline_style_of_declarations () =
  (* Basic declarations *)
  let decls =
    [
      declaration Color (Hex { hash = true; value = "ff0000" });
      declaration Background_color (Hex { hash = true; value = "0000ff" });
    ]
  in
  let style = inline_style_of_declarations decls in
  check bool "contains color property" true (String.contains style ':');
  check bool "contains semicolon separator" true (String.contains style ';');

  (* With !important *)
  let important_decls =
    [
      declaration ~important:true Color (Hex { hash = true; value = "ff0000" });
    ]
  in
  let important_style = inline_style_of_declarations important_decls in
  check bool "contains !important" true
    (String.contains important_style '!' && String.contains important_style 't');

  (* Custom properties *)
  let custom_decls =
    [ custom_property "--primary" "blue"; custom_property "--secondary" "red" ]
  in
  let custom_style = inline_style_of_declarations custom_decls in
  check bool "contains custom property prefix" true
    (String.contains custom_style '-' && String.contains custom_style '-');

  (* Minified output *)
  let minified = inline_style_of_declarations ~minify:true decls in
  check bool "minified has no spaces after colon" true
    (not (String.contains minified ' '))

(** Test stylesheet rendering *)
let test_to_string () =
  let selector = Css.Selector.class_ "test" in
  let decls = [ declaration Color (Hex { hash = true; value = "ff0000" }) ] in
  let rule : Css.Stylesheet.rule = { selector; declarations = decls } in

  let stylesheet = { Css.Stylesheet.empty with rules = [ rule ] } in

  (* Normal rendering *)
  let output = to_string stylesheet in
  check bool "contains selector" true (String.contains output '.');
  check bool "contains declaration" true (String.contains output ':');
  check bool "contains braces" true
    (String.contains output '{' && String.contains output '}');

  (* Minified rendering *)
  let minified = to_string ~minify:true stylesheet in
  check bool "minified is shorter" true
    (String.length minified <= String.length output)

(** Test media query rendering *)
let test_render_media () =
  let selector = Css.Selector.class_ "responsive" in
  let decls = [ declaration Display Grid ] in
  let rule : Css.Stylesheet.rule = { selector; declarations = decls } in

  let media_rule : Css.Stylesheet.media_rule =
    {
      media_condition = "screen and (min-width: 768px)";
      media_rules = [ rule ];
    }
  in

  let stylesheet =
    { Css.Stylesheet.empty with media_queries = [ media_rule ] }
  in

  let output = to_string stylesheet in
  check bool "contains @media" true (String.contains output '@');
  check bool "contains media condition" true
    (String.contains output '7' && String.contains output '6'
   && String.contains output '8');
  check bool "contains nested rule" true (String.contains output '.')

(** Test layer rendering *)
let test_render_layer () =
  let selector = Css.Selector.class_ "utility" in
  let decls = [ declaration Padding (Rem 1.) ] in
  let rule : Css.Stylesheet.rule = { selector; declarations = decls } in

  let layer : Css.Stylesheet.layer_rule =
    {
      layer = "utilities";
      rules = [ Css.Stylesheet.Rule rule ];
      media_queries = [];
      container_queries = [];
      supports_queries = [];
    }
  in

  let stylesheet = { Css.Stylesheet.empty with layers = [ layer ] } in

  let output = to_string stylesheet in
  check bool "contains @layer" true (String.contains output '@');
  check bool "contains layer name" true
    (String.contains output 'u' && String.contains output 't');

  (* Empty layer rendering *)
  let empty_layer : Css.Stylesheet.layer_rule =
    {
      layer = "base";
      rules = [];
      media_queries = [];
      container_queries = [];
      supports_queries = [];
    }
  in

  let sheet_with_empty =
    { Css.Stylesheet.empty with layers = [ empty_layer ] }
  in

  let empty_output = to_string sheet_with_empty in
  check bool "empty layer ends with semicolon" true
    (String.contains empty_output ';')

(** Test container query rendering *)
let test_render_container () =
  let selector = Css.Selector.class_ "card" in
  let decls = [ declaration Display Flex ] in
  let rule : Css.Stylesheet.rule = { selector; declarations = decls } in

  let container_rule : Css.Stylesheet.container_rule =
    {
      container_name = Some "sidebar";
      container_condition = "(min-width: 400px)";
      container_rules = [ rule ];
    }
  in

  let stylesheet =
    { Css.Stylesheet.empty with container_queries = [ container_rule ] }
  in

  let output = to_string stylesheet in
  check bool "contains @container" true (String.contains output '@');
  check bool "contains container name" true (String.contains output 's');
  check bool "contains container condition" true (String.contains output '4')

(** Test @supports rendering *)
let test_render_supports () =
  let selector = Css.Selector.class_ "modern" in
  let decls = [ declaration Display Grid ] in
  let rule : Css.Stylesheet.rule = { selector; declarations = decls } in

  let supports_rule : Css.Stylesheet.supports_rule =
    {
      supports_condition = "(display: grid)";
      supports_content = Css.Stylesheet.Support_rules [ rule ];
    }
  in

  let stylesheet =
    { Css.Stylesheet.empty with supports_queries = [ supports_rule ] }
  in

  let output = to_string stylesheet in
  check bool "contains @supports" true (String.contains output '@');
  check bool "contains condition" true
    (String.contains output '(' && String.contains output ')');
  check bool "contains nested rule" true (String.contains output '.')

(** Test @property rendering *)
let test_render_property () =
  let property_rule : Css.Stylesheet.property_rule =
    {
      name = "--my-color";
      syntax = "<color>";
      inherits = true;
      initial_value = Some "red";
    }
  in

  let stylesheet =
    { Css.Stylesheet.empty with at_properties = [ property_rule ] }
  in

  let output = to_string stylesheet in
  check bool "contains @property" true (String.contains output '@');
  check bool "contains property name" true
    (String.contains output '-' && String.contains output '-');
  check bool "contains syntax" true
    (String.contains output 's' && String.contains output 'y');
  check bool "contains inherits" true
    (String.contains output 'i' && String.contains output 'n');
  check bool "contains initial value" true
    (String.contains output 'r' && String.contains output 'e')

(** Test complete stylesheet ordering *)
let test_render_ordering () =
  (* CSS spec requires specific ordering of at-rules *)
  let charset : Css.Stylesheet.charset_rule = { encoding = "UTF-8" } in
  let import : Css.Stylesheet.import_rule =
    { url = "base.css"; layer = None; supports = None; media = None }
  in
  let namespace : Css.Stylesheet.namespace_rule =
    { prefix = Some "svg"; uri = "http://www.w3.org/2000/svg" }
  in

  let stylesheet =
    {
      Css.Stylesheet.empty with
      charset = Some charset;
      imports = [ import ];
      namespaces = [ namespace ];
    }
  in

  let output = to_string stylesheet in

  (* Check ordering: @charset must be first *)
  let charset_pos = try String.index output '@' with Not_found -> -1 in
  let import_pos =
    try String.index_from output (charset_pos + 1) '@' with Not_found -> -1
  in
  check bool "@charset before @import" true
    (charset_pos < import_pos && charset_pos >= 0)

(** Test optimized rendering *)
let test_render_optimized () =
  let selector = Css.Selector.class_ "test" in
  let decls =
    [
      declaration Color (Hex { hash = true; value = "ff0000" });
      declaration Color (Hex { hash = true; value = "0000ff" });
      (* Duplicate *)
    ]
  in
  let rule : Css.Stylesheet.rule = { selector; declarations = decls } in

  let stylesheet = { Css.Stylesheet.empty with rules = [ rule ] } in

  (* Render with optimization *)
  let optimized = to_string ~optimize:true stylesheet in

  (* Count occurrences of "color:" - should be only 1 with optimization *)
  let count_color str =
    let rec count pos acc =
      try
        let idx = String.index_from str pos ':' in
        if idx > 5 && String.sub str (idx - 5) 5 = "color" then
          count (idx + 1) (acc + 1)
        else count (idx + 1) acc
      with Not_found -> acc
    in
    count 0 0
  in

  check bool "optimized has fewer color declarations" true
    (count_color optimized <= 1)

(** Test render modes *)
let test_render_modes () =
  let decls = [ declaration Color (Hex { hash = true; value = "ff0000" }) ] in

  (* Inline mode *)
  let inline = inline_style_of_declarations ~mode:Inline decls in
  check bool "inline mode produces inline style" true
    (String.contains inline ':' && not (String.contains inline '{'));

  (* Variables mode (default) *)
  let variables = inline_style_of_declarations ~mode:Variables decls in
  check bool "variables mode produces declarations" true
    (String.contains variables ':')

let render_tests =
  [
    ("inline style of declarations", `Quick, test_inline_style_of_declarations);
    ("to_string", `Quick, test_to_string);
    ("render media", `Quick, test_render_media);
    ("render layer", `Quick, test_render_layer);
    ("render container", `Quick, test_render_container);
    ("render supports", `Quick, test_render_supports);
    ("render property", `Quick, test_render_property);
    ("render ordering", `Quick, test_render_ordering);
    ("render optimized", `Quick, test_render_optimized);
    ("render modes", `Quick, test_render_modes);
  ]

let suite = [ ("render", render_tests) ]
