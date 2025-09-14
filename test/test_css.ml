(** High-level CSS integration tests

    Tests the integration between different CSS modules and end-to-end CSS
    generation using only the public Css module interface.

    Detailed module functionality is tested in dedicated files:
    - test_values.ml - CSS value types
    - test_properties.ml - CSS properties
    - test_selector.ml - CSS selectors
    - test_declaration.ml - CSS declarations
    - test_stylesheet.ml - Stylesheet construction
    - test_optimize.ml - CSS optimization
    - test_variables.ml - CSS variables
    - test_pp.ml - Pretty printing *)

open Css

(* Helper selectors for tests *)
let btn = Selector.class_ "btn"
let card = Selector.class_ "card"

(* Test end-to-end CSS generation *)
let generation () =
  let stylesheet =
    v
      [
        rule ~selector:btn
          [ color (Hex { hash = true; value = "ff0000" }); padding [ Px 10. ] ];
        rule ~selector:card
          [
            margin [ Px 5. ];
            background_color (Hex { hash = false; value = "ffffff" });
          ];
      ]
  in

  let css = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string)
    "exact css generation"
    ".btn{color:#ff0000;padding:10px}.card{margin:5px;background-color:#ffffff}\n"
    css

(* Test optimization flag works *)
let optimization_flag () =
  let stylesheet =
    v
      [
        rule ~selector:btn
          [
            color (Hex { hash = true; value = "ff0000" });
            color (Hex { hash = true; value = "0000ff" });
            (* duplicate - should be optimized *)
          ];
      ]
  in

  let css_optimized = Css.to_string ~minify:true ~optimize:true stylesheet in
  Alcotest.(check string)
    "optimized exact" ".btn{color:#0000ff}\n" css_optimized

(* Test layers work end-to-end *)
let layers_integration () =
  let utility_rule = rule ~selector:btn [ padding [ Px 10. ] ] in
  let stylesheet = layer ~name:"utilities" [ utility_rule ] in

  let css = Css.to_string ~minify:true stylesheet in

  (* Should contain @layer *)
  Alcotest.(check bool) "contains @layer" true (String.contains css '@');
  Alcotest.(check bool)
    "contains layer name" true
    (String.contains css 'u' && String.contains css 't')

(* Test media queries work end-to-end *)
let media_integration () =
  let mobile_rule = rule ~selector:btn [ font_size (Rem 0.875) ] in
  let stylesheet = media ~condition:"(max-width: 640px)" [ mobile_rule ] in

  let css = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string)
    "media exact" "@media (max-width: 640px){.btn{font-size:.875rem}}\n" css

(* Test minify flag *)
let minify_flag () =
  let stylesheet =
    v [ rule ~selector:btn [ color (Hex { hash = true; value = "ff0000" }) ] ]
  in

  let css_minified = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string) "minified exact" ".btn{color:#ff0000}\n" css_minified

(* Test important declarations *)
let important_integration () =
  let stylesheet =
    v
      [
        rule ~selector:btn
          [
            important (color (Hex { hash = true; value = "ff0000" }));
            padding [ Px 10. ];
          ];
      ]
  in

  let css = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string)
    "important exact" ".btn{color:#ff0000!important;padding:10px}\n" css

(* Test custom properties integration *)
let custom_properties_integration () =
  let stylesheet =
    v
      [
        rule ~selector:btn
          [ custom_property "--primary-color" "blue"; color (Named Blue) ];
      ]
  in

  let css = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string)
    "custom properties exact" ".btn{--primary-color:blue;color:blue}\n" css

(* CSS Roundtrip Test: Parse generated CSS and compare roundtrip *)
let roundtrip () =
  let original_css =
    match Examples.read "empty_tailwind.css" with
    | Some css -> css
    | None -> Alcotest.fail "Could not read empty_tailwind.css from examples"
  in

  (* Parse the CSS with context on failure *)
  let parsed_stylesheet =
    match Css.of_string original_css with
    | Ok stylesheet -> stylesheet
    | Error err ->
        (* Format the structured error *)
        let formatted_error = Css.pp_parse_error err in
        Alcotest.fail ("Failed to parse CSS: " ^ formatted_error)
  in

  (* Render it back to string with same settings (minified) *)
  let roundtrip_css = Css.to_string ~minify:true parsed_stylesheet in

  (* Compare - they should be identical *)
  if original_css <> roundtrip_css then (
    (* Show a helpful diff around the first mismatch to ease debugging *)
    let len_a = String.length original_css in
    let len_b = String.length roundtrip_css in
    let rec first_diff i =
      if i >= len_a || i >= len_b then i
      else if original_css.[i] <> roundtrip_css.[i] then i
      else first_diff (i + 1)
    in
    let i = first_diff 0 in
    let start = max 0 (i - 40) in
    let take n s =
      let l = String.length s in
      let k = min n (l - start) in
      if k <= 0 then "" else String.sub s start k
    in
    let a_snip = take 80 original_css in
    let b_snip = take 80 roundtrip_css in
    let caret_pos = i - start in
    let caret = String.make (max 0 caret_pos) ' ' ^ "^" in
    Fmt.epr "Roundtrip mismatch at byte %d (orig len=%d, roundtrip len=%d)\n" i
      len_a len_b;
    Fmt.epr "Original:  %S\n" a_snip;
    Fmt.epr "Roundtrip: %S\n" b_snip;
    Fmt.epr "           %s\n" caret;
    Alcotest.fail "CSS roundtrip should be identical")
  else
    Alcotest.(check string)
      "CSS roundtrip should be identical" original_css roundtrip_css

let suite =
  [
    ( "css",
      [
        (* Integration tests using public Css interface only *)
        Alcotest.test_case "CSS generation end-to-end" `Quick generation;
        Alcotest.test_case "optimization flag works" `Quick optimization_flag;
        Alcotest.test_case "layers integration" `Quick layers_integration;
        Alcotest.test_case "media queries integration" `Quick media_integration;
        Alcotest.test_case "minify flag" `Quick minify_flag;
        Alcotest.test_case "important declarations" `Quick important_integration;
        Alcotest.test_case "custom properties" `Quick
          custom_properties_integration;
        Alcotest.test_case "CSS roundtrip parsing" `Quick roundtrip;
      ] );
  ]
