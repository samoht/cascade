open Cascade

let parse s = Css.of_string_exn ~strict:false s
let minified css = Css.to_string ~minify:true css
let optimized_minified css = css |> Css.optimize |> minified

let check_inline ?optimized input expected =
  let inlined = input |> parse |> Css.inline_vars in
  Alcotest.(check string) "minified output" expected (minified inlined);
  match optimized with
  | None -> ()
  | Some expected ->
      Alcotest.(check string)
        "optimize+minify output" expected
        (optimized_minified inlined)

let check_inline_case ?optimized name input expected =
  let inlined = input |> parse |> Css.inline_vars in
  Alcotest.(check string) name expected (minified inlined);
  match optimized with
  | None -> ()
  | Some expected ->
      Alcotest.(check string)
        (name ^ " optimize+minify")
        expected
        (optimized_minified inlined)

let test_inline_substitutes_vars () =
  check_inline ~optimized:".button{color:#00f}"
    ":root{--brand:blue}.button{color:var(--brand)}" ".button{color:blue}"

let test_inline_keep_vars () =
  let inlined =
    parse ":root{--brand:blue}.button{color:var(--brand);border-color:blue}"
    |> Css.inline_vars ~keep_vars:[ "brand" ]
  in
  Alcotest.(check string)
    "kept custom property remains live"
    ":root{--brand:blue}.button{color:var(--brand);border-color:blue}"
    (minified inlined);
  Alcotest.(check string)
    "kept custom property optimize+minify"
    ":root{--brand:blue}.button{color:var(--brand);border-color:#00f}"
    (optimized_minified inlined)

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

let test_inline_import_supports_query_modes () =
  let loader =
    Css.Context.loader ~base_url:"entry.css"
      ~imports:[ ("grid.css", ".grid{display:grid}") ]
      ()
  in
  let input = parse "@import url(\"grid.css\") supports(display:grid);" in
  Alcotest.(check string)
    "without query preserves import supports wrapper"
    "@supports(display:grid){.grid{display:grid}}"
    (input |> Css.inline_imports loader |> minified);
  let evergreen =
    Css.Context.query ~supports:[ Css.Supports.property "display" "grid" ] ()
  in
  Alcotest.(check string)
    "matching query unwraps import supports wrapper" ".grid{display:grid}"
    (input |> Css.inline_imports ~query:evergreen loader |> minified)

let test_inline_vars_fallback_edges () =
  check_inline_case "nested missing vars use deepest fallback"
    ".a{color:var(--undef,var(--also-undef,blue))}.b{color:var(--undef,var(--also-undef,var(--third,fallback)))}"
    ~optimized:".a{color:#00f}.b{color:fallback}"
    ".a{color:blue}.b{color:fallback}";
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
    ".a{font-family:Helvetica Neue,sans-serif}";
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
    ".b{animation:slide 1s infinite}";
  check_inline_case "comma-list variable expands inside gradient function"
    ":root{--colors:red,blue,green}.a{background:linear-gradient(var(--colors))}"
    ~optimized:".a{background:linear-gradient(red,#00f,green)}"
    ".a{background:linear-gradient(red,blue,green)}";
  check_inline_case "rgb channel variables canonicalize after substitution"
    ":root{--r:255;--g:0;--b:0}.a{color:rgb(var(--r) var(--g) var(--b))}"
    ~optimized:".a{color:red}" ".a{color:rgb(255 0 0)}"

let test_inline_vars_runtime_boundaries () =
  check_inline_case "var inside string token is not substituted"
    ":root{--label:\"brand\"}.a:before{content:\"var(--label)\"}"
    ".a:before{content:\"var(--label)\"}";
  check_inline_case "media query var is preserved outside property values"
    ":root{--bp:30em}@media (min-width:var(--bp)){.x{color:red}}"
    ":root{--bp:30em}@media(min-width:var(--bp)){.x{color:red}}";
  check_inline_case "container query var is preserved outside property values"
    ":root{--bp:30em}@container (min-width:var(--bp)){.x{color:red}}"
    ":root{--bp:30em}@container(min-width:var(--bp)){.x{color:red}}"

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
      Alcotest.test_case "inline imports supports query modes" `Quick
        test_inline_import_supports_query_modes;
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
