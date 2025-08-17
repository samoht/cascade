(* Tests for CSS module *)
open Css

let test_property_creation () =
  let _color_prop = color "red" in
  let _padding_prop = padding "10px" in

  (* Properties should be created - just verify no exceptions *)
  Alcotest.(check pass) "color property created" () ();
  Alcotest.(check pass) "padding property created" () ()

let test_property_deduplication () =
  let props =
    [
      color "red";
      padding "10px";
      color "blue";
      (* Should override first color *)
      margin "5px";
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
    (Astring.String.is_infix ~affix:"blue" css);
  Alcotest.(check bool)
    "red was overridden" false
    (Astring.String.is_infix ~affix:"red" css)

let test_minification () =
  let test_rule =
    rule ~selector:".test" [ padding "0"; margin "0"; color "rgb(255, 0, 0)" ]
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
  let rules = [ rule ~selector:".responsive" [ padding "20px" ] ] in
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
  let props = [ color "red"; padding "10px"; margin "5px" ] in

  let inline = inline_style_of_declarations props in

  (* Should be semicolon-separated without trailing semicolon *)
  Alcotest.(check bool)
    "contains color" true
    (Astring.String.is_infix ~affix:"color: red" inline);
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
      ("background-color", background_color "blue");
      ("padding-left", padding_left "5px");
      ("margin-top", margin_top "10px");
      ("border-radius", border_radius "4px");
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

let suite =
  [
    ( "Css",
      [
        Alcotest.test_case "property creation" `Quick test_property_creation;
        Alcotest.test_case "property deduplication" `Quick
          test_property_deduplication;
        Alcotest.test_case "minification" `Quick test_minification;
        Alcotest.test_case "media query" `Quick test_media_query;
        Alcotest.test_case "inline style" `Quick test_inline_style;
        Alcotest.test_case "property names" `Quick test_property_names;
      ] );
  ]
