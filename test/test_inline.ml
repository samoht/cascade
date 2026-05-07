open Cascade

let parse s =
  match Css.of_string s with
  | Ok css -> css
  | Error e -> Alcotest.fail (Css.pp_parse_error e)

let minified css = Css.to_string ~minify:true ~newline:false css

let check_inline input expected =
  let actual = input |> parse |> Css.inline_vars |> minified in
  Alcotest.(check string) "minified output" expected actual

let test_inline_substitutes_vars () =
  check_inline ":root{--brand:blue}.button{color:var(--brand)}"
    ".button{color:#00f}"

let test_inline_keep_vars () =
  let css =
    parse ":root{--brand:blue}.button{color:var(--brand);border-color:blue}"
    |> Css.inline_vars ~keep_vars:[ "brand" ]
    |> minified
  in
  Alcotest.(check string)
    "kept custom property remains live"
    ":root{--brand:#00f}.button{color:var(--brand);border-color:#00f}" css

let test_decode_import_url () =
  Alcotest.(check string)
    "url form" "theme.css"
    (Css.decode_import_url "url(\"theme.css\")");
  Alcotest.(check string)
    "string form" "theme.css"
    (Css.decode_import_url "\"theme.css\"")

let test_inline_import_loader () =
  let loader =
    Css.Context.loader ~base_url:"entry.css"
      ~imports:[ ("base.css", ".base{display:block}") ]
      ()
  in
  let css =
    parse "@import url(\"base.css\");.button{color:red}"
    |> Css.inline_imports loader |> minified
  in
  Alcotest.(check string)
    "resolved import is replaced" ".base{display:block}.button{color:red}" css

let test_inline_import_missing () =
  let loader = Css.Context.loader ~base_url:"entry.css" () in
  let css =
    parse "@import url(\"missing.css\");.button{color:red}"
    |> Css.inline_imports loader |> minified
  in
  Alcotest.(check string)
    "unresolved import survives" "@import\"missing.css\";.button{color:red}" css

let suite =
  ( "inline",
    [
      Alcotest.test_case "inline vars substitute visible custom properties"
        `Quick test_inline_substitutes_vars;
      Alcotest.test_case "inline vars keep requested references" `Quick
        test_inline_keep_vars;
      Alcotest.test_case "decode import url forms" `Quick test_decode_import_url;
      Alcotest.test_case "inline imports resolves loader stylesheet" `Quick
        test_inline_import_loader;
      Alcotest.test_case "inline imports preserves unresolved import" `Quick
        test_inline_import_missing;
    ] )
