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

(* Inlining deletes a non-kept variable's definition only when it has a single
   definition scope; a variable overridden in another scope is kept as a live
   var() chain and reported through [warn]. *)
let test_inline_fold_deletes_defs () =
  let fold css =
    parse css |> Css.inline_vars ~keep_vars:[ "brand" ] |> minified
  in
  Alcotest.(check string)
    "single definition folds into the kept var and is deleted"
    ":root{--brand:red}.btn{color:var(--brand)}"
    (fold
       ":root{--brand:var(--palette-red);--palette-red:red}.btn{color:var(--brand)}");
  Alcotest.(check string)
    "a class-scope override keeps the variable as a live chain"
    ":root{--brand:var(--palette-red);--palette-red:red}.dark{--palette-red:black}.btn{color:var(--brand)}"
    (fold
       ":root{--brand:var(--palette-red);--palette-red:red}.dark{--palette-red:black}.btn{color:var(--brand)}");
  Alcotest.(check string)
    "an @media override keeps the variable as a live chain"
    ":root{--brand:var(--palette-red);--palette-red:red}@media(prefers-color-scheme:dark){:root{--palette-red:black}}.btn{color:var(--brand)}"
    (fold
       ":root{--brand:var(--palette-red);--palette-red:red}@media \
        (prefers-color-scheme:dark){:root{--palette-red:black}}.btn{color:var(--brand)}");
  let warned = ref [] in
  ignore
    (parse
       ":root{--brand:var(--palette-red);--palette-red:red}.dark{--palette-red:black}.btn{color:var(--brand)}"
    |> Css.inline_vars ~keep_vars:[ "brand" ] ~warn:(fun n ->
        warned := n :: !warned));
  Alcotest.(check (list string))
    "an overridden variable is reported via warn" [ "--palette-red" ] !warned

let test_inline_keep_vars_calc_identity () =
  (* A kept theme var stays a live reference, but the value-independent calc
     identities still simplify: [p-1] expands to [calc(var(--spacing) * 1)],
     which must collapse to [var(--spacing)] (Tailwind's form), and [* 0] to a
     bare zero. Only value resolution ([* n], n >= 2) keeps the
     multiplication. *)
  let inline css = parse css |> Css.inline_vars ~keep_vars:[ "spacing" ] in
  Alcotest.(check string)
    "kept var times one folds to the operand"
    ":root{--spacing:.25rem}.p{padding:var(--spacing)}"
    (minified
       (inline ":root{--spacing:.25rem}.p{padding:calc(var(--spacing) * 1)}"));
  Alcotest.(check string)
    "kept var times zero folds to zero" ":root{--spacing:.25rem}.p{padding:0}"
    (minified
       (inline ":root{--spacing:.25rem}.p{padding:calc(var(--spacing) * 0)}"));
  Alcotest.(check string)
    "kept var times four keeps the var() reference"
    ":root{--spacing:.25rem}.p{padding:calc(var(--spacing)*4)}"
    (minified
       (inline ":root{--spacing:.25rem}.p{padding:calc(var(--spacing) * 4)}"))

let test_inline_zero_value_collapse () =
  (* [divide-x-0] multiplies a literal [0px] by a kept reverse var; [0 * x] is
     zero for every [x], so the [calc()] and the [var()] both drop out. The
     printer keeps the zero's unit ([0px]); stripping it to [0] is the
     optimizer's zero-length minification, not the printer's job. *)
  let inline css =
    parse css |> Css.inline_vars ~keep_vars:[ "tw-divide-x-reverse" ]
  in
  Alcotest.(check string)
    "literal zero times a kept var collapses, dropping the calc and var"
    ":root{--tw-divide-x-reverse:0}.d{border-inline-start-width:0px}"
    (minified
       (inline
          ":root{--tw-divide-x-reverse:0}.d{border-inline-start-width:calc(0px \
           * var(--tw-divide-x-reverse))}"))

let test_inline_oklch_number_lightness () =
  (* An oklch lightness written as a bare number ([.7]) is the number 0.7, equal
     to [70%] but not to [.7%]. Simplifying the colour while inlining must keep
     the number form rather than reissue it as a percentage, which would corrupt
     [.7] into [.7%] (0.7%). *)
  check_inline_case "oklch number lightness keeps its number form"
    ".b{color:oklch(.7 .15 145.5)}" ".b{color:oklch(.7 .15 145.5)}"

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
    ":root{--a:var(--b);--b:var(--c);--c:var(--a)}.x{color:fallback}";
  (* A cyclic custom property is invalid at computed-value time: its own var()
     fallback does not rescue it, so the consumer's fallback wins. *)
  check_inline_case
    "self-cycle ignores its own fallback, consumer fallback wins"
    ":root{--a:var(--a,red)}.x{color:var(--a,blue)}" ".x{color:blue}";
  (* A custom property resolving to an undefined var() is itself invalid, so a
     consumer fallback applies rather than leaving a dead var() behind. *)
  check_inline_case "chain to an undefined var resolves the consumer fallback"
    ":root{--a:var(--b)}.x{color:var(--a,red)}" ".x{color:red}"

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

let test_inline_across_layers () =
  (* A cascade layer never scopes custom-property visibility: layers only order
     competing declarations, so a variable defined in one @layer resolves for a
     consumer in another layer (or none). inline_vars must substitute across the
     boundary and drop the now unused definition, exactly as for unlayered
     rules. *)
  check_inline_case ~optimized:".p-6{padding:1.5rem}"
    "definition and reference in different layers substitute"
    "@layer theme{:root{--spacing:.25rem}}@layer \
     utilities{.p-6{padding:calc(var(--spacing)*6)}}"
    ".p-6{padding:calc(.25rem*6)}";
  check_inline_case ~optimized:".x{padding:1rem}"
    "definition in a layer, consumer at top level"
    "@layer t{:root{--s:.25rem}}.x{padding:calc(var(--s)*4)}"
    ".x{padding:calc(.25rem*4)}";
  check_inline_case ~optimized:".x{padding:1rem}"
    "definition at top level, consumer inside a layer"
    ":root{--s:.25rem}@layer u{.x{padding:calc(var(--s)*4)}}"
    ".x{padding:calc(.25rem*4)}";
  (* A variable redefined across layers on the same element is a statically
     decidable cascade override, so it folds to the winning layer's value (the
     later layer wins for normal declarations) instead of staying a live
     reference. *)
  check_inline_case ~optimized:".x{color:#00f}"
    "a cross-layer redefinition folds to the winning layer"
    "@layer a{:root{--c:red}}@layer b{:root{--c:blue}}@layer \
     u{.x{color:var(--c)}}"
    ".x{color:blue}"

(* A cascade layer only orders competing declarations, so a variable defined on
   the same element across several layers has a statically decidable winner and
   folds to it (CSS Cascade 5 sec. 6.4.3). An override that depends on runtime
   state (a media query, a different selector) cannot be decided statically and
   stays a live var(). *)
let test_inline_layer_winner () =
  check_inline_case ~optimized:".z{width:2px}"
    "the later layer wins for a normal declaration"
    "@layer a{:root{--x:1px}}@layer b{:root{--x:2px}}.z{width:var(--x)}"
    ".z{width:2px}";
  check_inline_case ~optimized:".z{width:2px}"
    "an unlayered definition beats a layered one regardless of order"
    ":root{--x:2px}@layer a{:root{--x:1px}}.z{width:var(--x)}" ".z{width:2px}";
  check_inline_case ~optimized:".z{width:1px}"
    "revert-layer rolls back to the earlier layer"
    "@layer a{:root{--x:1px}}@layer \
     b{:root{--x:revert-layer}}.z{width:var(--x)}"
    ".z{width:1px}";
  check_inline_case ~optimized:".z{width:1px}"
    "an important definition inverts the layer order"
    "@layer a{:root{--x:1px!important}}@layer \
     b{:root{--x:2px}}.z{width:var(--x)}"
    ".z{width:1px}";
  check_inline_case "a media-query override stays a live reference"
    ":root{--c:red}@media(prefers-color-scheme:dark){:root{--c:blue}}.x{color:var(--c)}"
    ":root{--c:red}@media(prefers-color-scheme:dark){:root{--c:blue}}.x{color:var(--c)}";
  check_inline_case "a different-selector override stays a live reference"
    ":root{--c:red}.dark{--c:blue}.x{color:var(--c)}"
    ":root{--c:red}.dark{--c:blue}.x{color:var(--c)}"

let suite =
  ( "inline",
    [
      Alcotest.test_case "inline vars substitute visible custom properties"
        `Quick test_inline_substitutes_vars;
      Alcotest.test_case "inline vars keep requested references" `Quick
        test_inline_keep_vars;
      Alcotest.test_case "inline vars fold and delete non-kept definitions"
        `Quick test_inline_fold_deletes_defs;
      Alcotest.test_case "inline vars apply calc identities to kept vars" `Quick
        test_inline_keep_vars_calc_identity;
      Alcotest.test_case "inline vars collapse a literal zero times a kept var"
        `Quick test_inline_zero_value_collapse;
      Alcotest.test_case "inline vars keep an oklch number lightness" `Quick
        test_inline_oklch_number_lightness;
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
      Alcotest.test_case "inline vars substitute across cascade layers" `Quick
        test_inline_across_layers;
      Alcotest.test_case
        "inline vars fold a layer-decided override to its winner" `Quick
        test_inline_layer_winner;
    ] )
