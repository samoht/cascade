open Cascade

let parse s = Css.of_string_exn ~strict:false s
let minified css = Css.to_string ~minify:true css

let check_inline input expected =
  let actual = input |> parse |> Css.inline_vars |> minified in
  Alcotest.(check string) "minified output" expected actual

let check_inline_case name input expected =
  let actual = input |> parse |> Css.inline_vars |> minified in
  Alcotest.(check string) name expected actual

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

let test_inline_vars_fallback_edges () =
  check_inline_case "nested missing vars use deepest fallback"
    ".a{color:var(--undef,var(--also-undef,blue))}.b{color:var(--undef,var(--also-undef,var(--third,fallback)))}"
    ".a{color:#00f}.b{color:fallback}";
  check_inline_case "missing var without fallback stays unresolved"
    ".a{color:var(--undef)}" ".a{color:var(--undef)}";
  check_inline_case "calc fallback resolves and canonicalizes"
    ".a{width:var(--undef,calc(1px + \
     2px))}.b{width:var(--undef,calc(var(--gap,8px)*2))}"
    ".a{width:3px}.b{width:16px}";
  check_inline_case "resolved var ignores fallback"
    ":root{--gap:10px}.a{padding:var(--gap,20px)}" ".a{padding:10px}"

let test_inline_fallback_lists () =
  check_inline_case "font-family multi-comma fallback substitutes as token list"
    ".a{font-family:var(--font,\"Helvetica Neue\",sans-serif)}"
    ".a{font-family:\"Helvetica Neue\",sans-serif}";
  check_inline_case
    "invalid color multi-comma fallback drops under minification"
    ".a{color:var(--undef,red,blue)}" "";
  check_inline_case "empty fallback makes invalid color declaration drop"
    ".a{color:var(--undef,)}" ""

let test_inline_cycle_fallbacks () =
  check_inline_case "self-cycle uses consumer fallback and strips dead var"
    ":root{--x:var(--x)}.a{color:var(--x,red)}" ".a{color:red}";
  check_inline_case "two-cycle uses consumer fallback"
    ":root{--a:var(--b);--b:var(--a)}.x{color:var(--a,fallback)}"
    ":root{--a:var(--b);--b:var(--a)}.x{color:fallback}";
  check_inline_case "three-cycle uses consumer fallback"
    ":root{--a:var(--b);--b:var(--c);--c:var(--a)}.x{color:var(--a,fallback)}"
    ":root{--a:var(--b);--b:var(--c);--c:var(--a)}.x{color:fallback}"

let test_inline_shorthand_functions () =
  check_inline_case "transition variable resolves into property slot"
    ":root{--prop:opacity}.a{transition:var(--prop) .3s ease}"
    ".a{transition:opacity .3s}";
  check_inline_case "animation variable resolves into name slot"
    ":root{--anim:slide}.b{animation:var(--anim) 1s ease infinite}"
    ".b{animation:1s infinite slide}";
  check_inline_case "comma-list variable expands inside gradient function"
    ":root{--colors:red,blue,green}.a{background:linear-gradient(var(--colors))}"
    ".a{background:linear-gradient(red,#00f,green)}";
  check_inline_case "rgb channel variables canonicalize after substitution"
    ":root{--r:255;--g:0;--b:0}.a{color:rgb(var(--r) var(--g) var(--b))}"
    ".a{color:red}"

let test_inline_vars_runtime_boundaries () =
  check_inline_case "var inside string token is not substituted"
    ":root{--label:\"brand\"}.a:before{content:\"var(--label)\"}"
    ".a:before{content:\"var(--label)\"}";
  check_inline_case "media query var is preserved outside property values"
    ":root{--bp:30em}@media (min-width:var(--bp)){.x{color:red}}"
    ":root{--bp:30em}@media(width>=var(--bp)){.x{color:red}}";
  check_inline_case "container query var is preserved outside property values"
    ":root{--bp:30em}@container (min-width:var(--bp)){.x{color:red}}"
    ":root{--bp:30em}@container(width>=var(--bp)){.x{color:red}}"

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
      Alcotest.test_case "inline vars fallback edges" `Quick
        test_inline_vars_fallback_edges;
      Alcotest.test_case "inline vars fallback token lists" `Quick
        test_inline_fallback_lists;
      Alcotest.test_case "inline vars cycle fallback edges" `Quick
        test_inline_cycle_fallbacks;
      Alcotest.test_case "inline vars shorthand and function edges" `Quick
        test_inline_shorthand_functions;
      Alcotest.test_case "inline vars runtime boundaries" `Quick
        test_inline_vars_runtime_boundaries;
    ] )
