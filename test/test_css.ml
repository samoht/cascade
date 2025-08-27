(* Tests for CSS module *)
open Css

let test_property_creation () =
  let color_prop = color (Hex { hash = true; value = "ff0000" }) in
  let padding_prop = padding (Px 10) in

  (* Verify properties are created correctly *)
  let sheet1 = stylesheet [ Rule (rule ~selector:".test" [ color_prop ]) ] in
  let sheet2 = stylesheet [ Rule (rule ~selector:".test" [ padding_prop ]) ] in
  let css1 = to_string sheet1 in
  let css2 = to_string sheet2 in

  Alcotest.(check bool)
    "color property creates valid CSS" true
    (Astring.String.is_infix ~affix:"color:" css1);
  Alcotest.(check bool)
    "padding property creates valid CSS" true
    (Astring.String.is_infix ~affix:"padding:" css2)

let test_property_deduplication () =
  let props =
    [
      color (Hex { hash = true; value = "ff0000" });
      padding (Px 10);
      color (Hex { hash = true; value = "0000ff" });
      (* Should override first color *)
      margin (Px 5);
    ]
  in

  let deduped = deduplicate_declarations props in

  (* Should have 3 properties after deduplication *)
  Alcotest.(check int)
    "deduplication removes duplicates" 3 (List.length deduped);

  (* Create a stylesheet to check the output *)
  let sheet = stylesheet [ Rule (rule ~selector:".test" deduped) ] in
  let css = to_string sheet in

  (* Should have blue, not red *)
  Alcotest.(check bool)
    "last property wins" true
    (Astring.String.is_infix ~affix:"#0000ff" css);
  Alcotest.(check bool)
    "red was overridden" false
    (Astring.String.is_infix ~affix:"#ff0000" css)

let test_minification () =
  let test_rule =
    rule ~selector:".test"
      [ padding Zero; margin Zero; color (Rgb { r = 255; g = 0; b = 0 }) ]
  in

  let sheet = stylesheet [ Rule test_rule ] in
  let normal = to_string ~minify:false sheet in
  let minified = to_string ~minify:true sheet in

  (* Minified should be shorter *)
  Alcotest.(check bool)
    "minified is shorter" true
    (String.length minified < String.length normal);

  (* Minified should not have newlines or extra spaces *)
  Alcotest.(check bool)
    "no newlines in minified" false
    (String.contains minified '\n');

  (* Should still contain the selector and properties *)
  Alcotest.(check bool)
    "contains selector" true
    (Astring.String.is_infix ~affix:".test" minified);
  Alcotest.(check bool)
    "contains color" true
    (Astring.String.is_infix ~affix:"color:" minified)

let test_media_query () =
  let rules = [ rule ~selector:".responsive" [ padding (Px 20) ] ] in
  let mq = media ~condition:"(min-width: 768px)" rules in
  let sheet = stylesheet [ Media mq ] in
  let output = to_string sheet in

  (* Should contain media query *)
  Alcotest.(check bool)
    "contains media query" true
    (Astring.String.is_infix ~affix:"@media (min-width: 768px)" output);

  (* Should contain nested rule *)
  Alcotest.(check bool)
    "contains nested selector" true
    (Astring.String.is_infix ~affix:".responsive" output)

let test_inline_style () =
  let props =
    [
      color (Hex { hash = true; value = "ff0000" });
      padding (Px 10);
      margin (Px 5);
    ]
  in

  let inline = inline_style_of_declarations props in

  (* Should be semicolon-separated without trailing semicolon *)
  Alcotest.(check bool)
    "contains color" true
    (Astring.String.is_infix ~affix:"color: #ff0000" inline);
  Alcotest.(check bool)
    "contains padding" true
    (Astring.String.is_infix ~affix:"padding: 10px" inline);
  Alcotest.(check bool)
    "no trailing semicolon" false
    (Astring.String.is_suffix ~affix:";" inline)

let test_property_names () =
  (* Test that property names are converted correctly *)
  let names =
    [
      ( "background-color",
        background_color (Hex { hash = true; value = "0000ff" }) );
      ("padding-left", padding_left (Px 5));
      ("margin-top", margin_top (Px 10));
      ("border-radius", border_radius (Px 4));
    ]
  in

  List.iter
    (fun (expected_name, prop) ->
      let sheet = stylesheet [ Rule (rule ~selector:".x" [ prop ]) ] in
      let css = to_string sheet in
      Alcotest.(check bool)
        (Fmt.str "CSS contains %s" expected_name)
        true
        (Astring.String.is_infix ~affix:expected_name css))
    names

let modes () =
  (* Test the different CSS generation modes *)

  (* Create a color variable with default value *)
  let _, color_var =
    var "color-blue-500" Color (Oklch { l = 62.3; c = 0.214; h = 259.815 })
  in
  let bg_prop = background_color (Var color_var) in
  let rule = rule ~selector:".bg-blue-500" [ bg_prop ] in
  let sheet = stylesheet [ Rule rule ] in

  (* Test Variables mode - should output var(--color-blue-500) *)
  let css_variables = to_string ~mode:Variables sheet in
  Alcotest.(check bool)
    "Variables mode contains var()" true
    (Astring.String.is_infix ~affix:"var(--color-blue-500)" css_variables);

  (* Test Inline mode - should output the actual OKLCH value *)
  let css_inline = to_string ~mode:Inline sheet in
  Alcotest.(check bool)
    "Inline mode contains oklch()" true
    (Astring.String.is_infix ~affix:"oklch(" css_inline);
  Alcotest.(check bool)
    "Inline mode does not contain var()" false
    (Astring.String.is_infix ~affix:"var(--" css_inline)

let suite =
  [
    ( "css",
      [
        Alcotest.test_case "property creation" `Quick test_property_creation;
        Alcotest.test_case "property deduplication" `Quick
          test_property_deduplication;
        Alcotest.test_case "minification" `Quick test_minification;
        Alcotest.test_case "media query" `Quick test_media_query;
        Alcotest.test_case "inline style" `Quick test_inline_style;
        Alcotest.test_case "property names" `Quick test_property_names;
        Alcotest.test_case "CSS modes" `Quick modes;
      ] );
  ]
