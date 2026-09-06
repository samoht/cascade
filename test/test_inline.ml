open Cascade
open Css_test_helpers

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

(* Every [\@font-face] descriptor, with a value it takes. *)
let font_face_descriptors =
  [
    ("font-family", "b");
    ("src", "local(y)");
    ("font-style", "italic");
    ("font-weight", "400");
    ("font-stretch", "50%");
    ("font-display", "swap");
    ("unicode-range", "U+0-7f");
    ("font-variant", "normal");
    ("font-feature-settings", "normal");
    ("font-variation-settings", "normal");
    ("font-tech", "variations");
    ("size-adjust", "100%");
    ("ascent-override", "normal");
    ("descent-override", "normal");
    ("line-gap-override", "normal");
  ]

let holds_substring sub s =
  let n = String.length s and m = String.length sub in
  let rec from i = i + m <= n && (String.sub s i m = sub || from (i + 1)) in
  from 0

let font_face_descriptor_count sheet =
  List.length
    (List.concat_map
       (fun statement ->
         match Css.as_font_face statement with
         | Some descriptors -> descriptors
         | None -> [])
       sheet)

(* CSS Fonts 4 sec. 4.1: no descriptor grammar accepts a [var()], so a browser
   drops the declaration holding one. cascade substitutes at build time instead,
   but only where the inline pass has a resolution path, and the parser keeps a
   [var()] exactly there. The two are one decision, so read them together: a
   descriptor that survives the parse leaves [inline_vars] with no [var()]
   behind, and no other descriptor survives. *)
let test_inline_font_face_var_descriptors () =
  let survives (name, value) =
    let css =
      String.concat ""
        [
          ":root{--v:";
          value;
          "}@font-face{font-family:a;src:local(anchor);";
          name;
          ":var(--v)}";
        ]
    in
    let sheet = parse css in
    let inlined = minified (Css.inline_vars sheet) in
    Alcotest.(check bool)
      (name ^ ": no var() reaches the output")
      false
      (holds_substring "var(" inlined);
    font_face_descriptor_count sheet = 3
  in
  let kept = List.map fst (List.filter survives font_face_descriptors) in
  Alcotest.(check (list string))
    "descriptors the parser keeps a var() for"
    [
      "font-family";
      "src";
      "font-style";
      "font-weight";
      "font-stretch";
      "font-display";
      "unicode-range";
      "font-variant";
      "font-feature-settings";
      "font-variation-settings";
      "font-tech";
      "size-adjust";
      "ascent-override";
      "descent-override";
      "line-gap-override";
    ]
    kept

(* Surviving the parse is only half of it: the value a [var()] stands for has to
   reach the output. Each row reads a variable whose value the descriptor's own
   grammar accepts. *)
let test_inline_font_face_var_resolves () =
  let resolves (name, value) =
    let css =
      String.concat ""
        [
          ":root{--v:";
          value;
          "}@font-face{font-family:a;src:local(anchor);";
          name;
          ":var(--v)}";
        ]
    in
    let inlined = minified (Css.inline_vars (parse css)) in
    Alcotest.(check bool)
      (Fmt.str "%s: resolves to %s (%s)" name value inlined)
      true
      (holds_substring (name ^ ":" ^ value) inlined)
  in
  List.iter resolves
    [
      ("font-family", "b");
      ("font-style", "italic");
      ("font-weight", "700");
      ("font-stretch", "50%");
      ("font-display", "swap");
      (* CSS Syntax 3 sec. 4.3.10 reads the range case-insensitively and the
         printer writes the hex digits upper-case. *)
      ("unicode-range", "U+0-7F");
      ("font-variant", "normal");
      ("font-variant", "small-caps");
      ("font-feature-settings", "normal");
      ("font-variation-settings", "normal");
      ("ascent-override", "normal");
      ("descent-override", "90%");
      ("line-gap-override", "10%");
      ("font-tech", "variations");
      ("size-adjust", "100%");
    ]

(* CSS Fonts 4 sec. 4.2 makes the [font-family] descriptor exactly one
   [<font-family-name>]. A build-time [var()] may stand for that name, but a
   property-style family stack does not become valid when it arrives through a
   reference. Raw and referenced list entries are rejected at the same
   descriptor boundary. *)
let test_inline_font_face_family_name () =
  let check (name, css, expected) =
    let inlined = minified (Css.inline_vars (parse css)) in
    Alcotest.(check string) name expected inlined
  in
  List.iter check
    [
      ( "a var() standing for the whole stack",
        ":root{--f:Arial,sans-serif}@font-face{font-family:var(--f);src:local(a)}",
        "" );
      ( "a var() standing for one entry",
        ":root{--f:sans-serif}@font-face{font-family:Arial,var(--f);src:local(a)}",
        "" );
      ( "a var() naming itself inside a stack",
        ":root{--f:Arial,var(--f)}@font-face{font-family:b,var(--f);src:local(a)}",
        "" );
    ]

(* CSS Fonts 4 sec. 4.4 takes the font-width descriptor as
   [<'font-width'>{1,2}], so the range form has two endpoints and a [var()] can
   stand for either. A range built from one raw endpoint and one reference has
   to reach the output with neither reference left in it. *)
let test_inline_font_face_var_stretch_range () =
  check_inline_case "font-stretch range endpoint from a var()"
    ":root{--wide:200%}@font-face{font-family:a;src:local(anchor);font-stretch:50% \
     var(--wide)}"
    "@font-face{font-family:a;src:local(anchor);font-stretch:50% 200%}"

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

let test_inline_triple_dash_custom_property () =
  Alcotest.(check string)
    "a third dash starts the custom-property name" ".x{color:red}"
    (parse ":root{---foo:red}.x{color:var(---foo)}"
    |> Css.inline_vars |> minified)

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

(* An at-rule that carries declarations outside a nested block consumes a
   [var()] like any rule does. Whatever the substitution manages to resolve
   there, a reference left in the output must keep the definition it names, or
   the sheet ships a name nothing defines. *)
let test_inline_at_rule_reference_keeps_its_binding () =
  let same name css =
    Alcotest.(check string) name css (minified (Css.inline_vars (parse css)))
  in
  same "a page margin box reference keeps its definition"
    ":root{--t:\"x\"}@page{@top-center{content:var(--t)}}";
  (* A [@supports-condition] body is a feature test rather than applied style
     (CSS Conditional 5 sec. 3), so the reference stays as authored. *)
  same "a @supports-condition reference keeps its definition"
    ":root{--c:red}@supports-condition --x{color:var(--c)}"

(* The census decides which overridden variable to report, so it has to see a
   reference wherever declarations live. *)
let test_inline_warns_for_an_at_rule_reference () =
  let warned = ref [] in
  ignore
    (parse
       ":root{--c:red}@media print{:root{--c:blue}}@keyframes \
        k{from{color:var(--c)}}"
    |> Css.inline_vars ~warn:(fun n -> warned := n :: !warned));
  Alcotest.(check (list string))
    "an override referenced only from a keyframe frame is reported" [ "--c" ]
    !warned

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

(* CSS Variables 1 sec. 3 puts the fallback in only where the custom property
   holds the guaranteed-invalid initial value. A definition the pass cannot
   prove reaches the consumer is not an absent one: [#o] may well hold [#i], so
   the reference stays live rather than taking the fallback. *)
let test_inline_vars_out_of_scope_definition_stays_live () =
  check_inline_case "a definition on another selector keeps the reference live"
    "#o{--x:red}#i{color:var(--x,lime)}" "#o{--x:red}#i{color:var(--x,lime)}";
  check_inline_case "an empty definition on another selector keeps it live"
    "#o{--x:}#i{color:var(--x,lime)}" "#o{--x:}#i{color:var(--x,lime)}";
  check_inline_case "a definition that covers the consumer still folds"
    ":root{--x:red}#i{color:var(--x,lime)}" "#i{color:red}";
  check_inline_case "a name the sheet never defines takes the fallback"
    "#i{color:var(--nope,lime)}" "#i{color:lime}";
  (* A reference the pass leaves live is answered by the browser's own cascade,
     so every definition of the name it might reach has to reach the output with
     it. *)
  check_inline_case "a live reference keeps the definition it may reach"
    "@media (min-width:1px){:root{--x:red}}#i{color:var(--x,lime)}"
    "@media(min-width:1px){:root{--x:red}}#i{color:var(--x,lime)}"

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
  (* CSS Transitions 1 (ED) sec. 2.5: [ease] is the easing initial, so pp holds
     the spelled-out form and the optimizer folds it away. *)
  check_inline_case "transition variable resolves into property slot"
    ~optimized:".a{transition:opacity .3s}"
    ":root{--prop:opacity}.a{transition:var(--prop) .3s ease}"
    ".a{transition:opacity .3s ease}";
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

let test_inline_runtime_var_metadata () =
  let runtime_ref : Css.length Css.var =
    Css.var_ref ~runtime:true
      ~fallback:(Css.Values.syntax_fallback "var(--fallback)")
      "external"
  in
  let runtime_cursor : Css.cursor Css.var =
    Css.var_ref ~runtime:true
      ~fallback:(Css.Values.syntax_fallback "var(--cursor-fallback)")
      "cursor"
  in
  let stylesheet =
    Css.v
      [
        Css.rule ~selector:(Css.Selector.class_ "x")
          [
            Css.padding [ Css.Var runtime_ref ];
            Css.cursor (Css.Var runtime_cursor);
          ];
      ]
  in
  Alcotest.(check string)
    "runtime vars keep their live fallback wrappers"
    ".x{padding:var(--external,var(--fallback));cursor:var(--cursor,var(--cursor-fallback))}"
    (stylesheet |> Css.inline_vars |> minified)

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

(* A custom declaration carries a cascade layer and caller metadata beside its
   value, and both are cascade- and caller-significant. [inline_vars] rewrites
   the value of a custom property it keeps - it canonicalises a value that is
   one colour, and folds the variables that value references - so every rewrite
   has to hand the layer, the metadata and [!important] on. *)
let test_inline_keeps_what_a_custom_declaration_carries () =
  let inject, project = Css.meta () in
  let kept ?(extra = []) name decl =
    let stylesheet =
      Css.v [ Css.rule ~selector:(Css.Selector.class_ "a") (extra @ [ decl ]) ]
    in
    match
      List.concat_map Css.Stylesheet.statement_declarations
        (Css.inline_vars ~keep_vars:[ "--x" ] stylesheet)
    with
    | [ d ] -> d
    | ds ->
        Alcotest.failf "%s: expected one declaration, got %d" name
          (List.length ds)
  in
  let layer d = Css.custom_declaration_layer d in
  let meta d = Option.bind (Css.meta_of_declaration d) project in
  (* A value that is one colour is the value [inline_vars] canonicalises. *)
  let red : Css.color = Css.Named Css.Values.Red in
  let layered, _ = Css.var ~layer:"theme" "x" Css.Color red in
  let d = kept "layer" layered in
  Alcotest.(check (option string)) "layer survives" (Some "theme") (layer d);
  let tagged, _ = Css.var ~meta:(inject "tag") "x" Css.Color red in
  let d = kept "meta" tagged in
  Alcotest.(check (option string)) "meta survives" (Some "tag") (meta d);
  let both, _ = Css.var ~layer:"theme" ~meta:(inject "tag") "x" Css.Color red in
  let d = kept "layer and meta" both in
  Alcotest.(check (option string))
    "layer survives beside meta" (Some "theme") (layer d);
  Alcotest.(check (option string))
    "meta survives beside layer" (Some "tag") (meta d);
  (* Importance already travels; it must keep travelling. *)
  let d = kept "important" (Css.important (Css.custom_property "--x" "red")) in
  Alcotest.(check bool)
    "important survives" true
    (Css.Declaration.is_important d);
  Alcotest.(check string)
    "important is written back" "--x:red!important"
    (Css.Declaration.to_string ~minify:true d);
  (* A declaration carrying none of the three is left exactly as it was. *)
  let d = kept "plain" (Css.custom_property "--x" "red") in
  Alcotest.(check (option string)) "no layer appears" None (layer d);
  Alcotest.(check (option string)) "no meta appears" None (meta d);
  Alcotest.(check bool) "not important" false (Css.Declaration.is_important d);
  Alcotest.(check string)
    "plain value is unchanged" "--x:red"
    (Css.Declaration.to_string ~minify:true d);
  (* Folding a variable the kept value references is the other rewrite. *)
  let d =
    kept "folds a reference"
      ~extra:[ Css.custom_property "--y" "1px solid" ]
      (Css.custom_property ~layer:"theme" "--x" "var(--y)")
  in
  Alcotest.(check string)
    "the reference folded" "--x:1px solid"
    (Css.Declaration.to_string ~minify:true d);
  Alcotest.(check (option string))
    "layer survives the fold" (Some "theme") (layer d)

(* The closed-world cleanup that follows substitution reaches inside a rule: CSS
   nesting puts an [@layer] and a [@property] registration there, and a cleanup
   that stops at the top level leaves the same sheet half cleaned. The [var()]
   inside the nested [@media] is substituted either way; the wrapper itself
   stays, since [Inline.flattening_layers_is_safe] cannot rank a sheet whose
   rule carries nested content and unwrapping is only sound where the layer
   stack and document order already agree. *)
let test_inline_cleanup_inside_a_rule () =
  check_inline_case "a nested @layer wrapper keeps an order it cannot decide"
    ":root{--c:red}.a{@layer m{@media print{.n{color:var(--c)}}}}"
    ".a{@layer m{@media print{.n{color:red}}}}";
  check_inline_case "a nested @property registration is dropped"
    ".b{color:red;@property --y{syntax:\"*\";inherits:false}}" ".b{color:red}"

(* CSS Properties and Values API 1 sec. 2: a registration gives its property an
   [initial-value], used as the computed value wherever no declaration wins, and
   an [inherits] descriptor deciding whether it inherits at all. Both change
   computed values, so a registration is dead only once nothing is left for it
   to govern: no declaration of the property, and no live [var()] reading it. *)
let test_inline_property_registration_kept () =
  check_inline_case "a registration for a kept-live property survives with it"
    "@property \
     --c{syntax:\"<color>\";inherits:false;initial-value:red}a{--c:#00f}b{color:var(--c)}"
    "@property \
     --c{syntax:\"<color>\";inherits:false;initial-value:red}a{--c:#00f}b{color:var(--c)}";
  check_inline_case "an inheriting registration is kept on the same terms"
    "@property \
     --c{syntax:\"<color>\";inherits:true;initial-value:red}a{--c:#00f}b{color:var(--c)}"
    "@property \
     --c{syntax:\"<color>\";inherits:true;initial-value:red}a{--c:#00f}b{color:var(--c)}";
  check_inline_case
    "a registration for a declaration the cascade cannot see is kept"
    "@property \
     --c{syntax:\"<color>\";inherits:false;initial-value:red}.theme{--c:#00f}.other{color:var(--c)}"
    "@property \
     --c{syntax:\"<color>\";inherits:false;initial-value:red}.theme{--c:#00f}.other{color:var(--c)}";
  check_inline_case
    "a registration whose declaration survives unreferenced is kept"
    "@property \
     --c{syntax:\"<color>\";inherits:false;initial-value:red}a{--c:#00f}"
    "@property \
     --c{syntax:\"<color>\";inherits:false;initial-value:red}a{--c:#00f}"

let test_inline_property_registration_dropped () =
  check_inline_case "a resolved-away property takes its registration with it"
    "@property \
     --gap{syntax:\"<length>\";inherits:false;initial-value:16px}.a{padding:var(--gap)}"
    ".a{padding:16px}";
  check_inline_case
    "a registration nothing declares and nothing reads has nothing to govern"
    "@property --y{syntax:\"*\";inherits:false}.b{color:red}" ".b{color:red}"

(* Inlining is a rewrite to a fixpoint: running it on its own output must change
   nothing. A pass that keeps a declaration because a registration made its
   property look multi-defined, then deletes that registration, contradicts
   itself - the next pass sees a single-definition variable and prunes it. *)
let test_inline_property_idempotent () =
  let check name input =
    let once = minified (Css.inline_vars (parse input)) in
    let twice = minified (Css.inline_vars (parse once)) in
    Alcotest.(check string) name once twice
  in
  check "a kept-live registered property is a fixpoint"
    "@property \
     --c{syntax:\"<color>\";inherits:false;initial-value:red}a{--c:#00f}b{color:var(--c)}";
  check "an unreferenced registered declaration is a fixpoint"
    "@property \
     --c{syntax:\"<color>\";inherits:false;initial-value:red}a{--c:#00f}";
  check "an unused registration is a fixpoint"
    "@property --y{syntax:\"*\";inherits:false}.b{color:red}"

(* CSS Conditional 5 sec. 6.2 (style container features): a [style()] query is
   evaluated against the computed value the queried custom property has on the
   query container, and its boolean form against that property's initial value.
   The queried name is a reference like any [var()]: whatever supplies that
   computed value decides whether the block applies at all. Chrome 146 paints
   [.z] green for [:root{--c:red}@container style(--c:red){.z{color:green}}] and
   black once [:root] loses the declaration. *)
let test_inline_style_query_keeps_queried_property () =
  check_inline_case "a declaration style() query keeps the property it reads"
    ":root{--c:red}@container style(--c: red){.z{color:green}}"
    ":root{--c:red}@container style(--c:red){.z{color:green}}";
  check_inline_case "a boolean style() query keeps the property it tests"
    ":root{--c:red}@container style(--c){.z{color:green}}"
    ":root{--c:red}@container style(--c){.z{color:green}}";
  check_inline_case "a queried property and the var() in its value both stay"
    ":root{--c:red;--d:red}@container style(--c: var(--d)){.z{color:green}}"
    ":root{--c:red;--d:red}@container style(--c:var(--d)){.z{color:green}}";
  check_inline_case "a range style() query keeps the property it bounds"
    ":root{--n:5}@container style(3 < --n < 10){.z{color:green}}"
    ":root{--n:5}@container style(3<--n<10){.z{color:green}}";
  (* Conditional Rules 5 sec. 6.2: a <style-range-value> that is a
     <custom-property-name> is substituted as if it were wrapped in a var(), so
     an operand spelled as a custom property is read like the bounded one. *)
  check_inline_case "a range style() query keeps the property bounding it"
    ":root{--lo:3;--n:5}@container style(--lo < --n < 10){.z{color:green}}"
    ":root{--lo:3;--n:5}@container style(--lo<--n<10){.z{color:green}}";
  check_inline_case "a comparison style() query keeps both properties it reads"
    ":root{--hi:9;--n:5}@container style(--n = --hi){.z{color:green}}"
    ":root{--hi:9;--n:5}@container style(--n=--hi){.z{color:green}}";
  check_inline_case "both sides of a combined style() query stay live"
    ":root{--a:red;--b:blue}@container style(--a: red) and style(--b: \
     blue){.z{color:green}}"
    ":root{--a:red;--b:blue}@container style(--a:red) and \
     style(--b:blue){.z{color:green}}";
  (* Negation reverses which elements a lost declaration paints: with [--c] red
     the block does not apply, without it the query is true and it does. *)
  check_inline_case "a negated style() query keeps the property it tests"
    ":root{--c:red}@container not style(--c: red){.z{color:green}}"
    ":root{--c:red}@container not style(--c:red){.z{color:green}}";
  check_inline_case "a named container's style() query keeps its property"
    ":root{--c:red}@container tall style(--c: red){.z{color:green}}"
    ":root{--c:red}@container tall style(--c:red){.z{color:green}}";
  check_inline_case "a style() query nested under @media keeps its property"
    ":root{--c:red}@media screen{@container style(--c: red){.z{color:green}}}"
    ":root{--c:red}@media screen{@container style(--c:red){.z{color:green}}}"

(* A style() query reads a computed value, so the [@property] registration that
   supplies the initial value when nothing declares the property keeps the query
   answerable. Chrome 146 paints [.z] green for the registration below and black
   without it. *)
let test_inline_style_query_keeps_registration () =
  check_inline_case "a style() query keeps the registration it reads"
    "@property \
     --c{syntax:\"<color>\";inherits:false;initial-value:red}@container \
     style(--c: red){.z{color:green}}"
    "@property \
     --c{syntax:\"<color>\";inherits:false;initial-value:red}@container \
     style(--c:red){.z{color:green}}"

(* The other half of the guard: a style() query keeps the property it names, not
   every property in sight, and a container name is a custom-ident that happens
   to be spelled with two dashes, not a custom property. *)
let test_inline_style_query_keeps_no_more () =
  check_inline_case "a style() query keeps only the property it names"
    ":root{--c:red;--other:blue}@container style(--c: red){.z{color:green}}"
    ":root{--c:red}@container style(--c:red){.z{color:green}}";
  check_inline_case "a container name is not a reference to a custom property"
    ":root{--c:red}@container --c (min-width:1px){.z{color:green}}"
    "@container --c (min-width:1px){.z{color:green}}"

(* Layers are ordered by first appearance (CSS Cascade 5 sec. 6.4.2), so where a
   layer is first named decides which definition of a cross-layer custom
   property wins. A conditional group only introduces its layers when its
   condition holds, which cascade cannot decide, while a [@container] block
   holds rules that always exist and is evaluated per element. *)
let test_inline_layer_order_sites () =
  check_inline_case "a layer first named inside @media leaves the order open"
    "@media screen{@layer b{.z{outline-color:red}}}@layer \
     a{:root{--c:red}}@layer b{:root{--c:blue}}.x{color:var(--c)}"
    "@media screen{@layer b{.z{outline-color:red}}}@layer \
     a{:root{--c:red}}@layer b{:root{--c:blue}}.x{color:var(--c)}";
  check_inline_case "a layer first named inside @container orders the sheet"
    "@container (min-width:1px){@layer b{.z{outline-color:red}}}@layer \
     a{:root{--c:red}}@layer b{:root{--c:blue}}.x{color:var(--c)}"
    "@container(min-width:1px){.z{outline-color:red}}.x{color:red}"

(* Unwrapping a [@layer] replays the layer stack as document order and hands the
   decision back to specificity, so it holds only where no cascade slot is
   written from two layers at once (CSS Cascade 5 sec. 6.4.3). No custom
   property has to be involved for the unwrapping to change the render. *)
let test_inline_layer_flattening () =
  check_inline_case "a declared layer order outranks document order"
    "@layer a,b;@layer b{.x{color:blue}}@layer a{.x{color:red}}"
    "@layer a,b;@layer b{.x{color:blue}}@layer a{.x{color:red}}";
  check_inline_case "an unlayered declaration keeps its win over a layer"
    ".x{color:red}@layer a{.x{color:blue}}"
    ".x{color:red}@layer a{.x{color:blue}}";
  check_inline_case "a layer outranks a more specific selector below it"
    "@layer a,b;@layer a{#x{color:red}}@layer b{.x{color:blue}}"
    "@layer a,b;@layer a{#x{color:red}}@layer b{.x{color:blue}}";
  check_inline_case "a longhand in a weaker layer loses to a shorthand"
    "@layer a,b;@layer b{.x{margin:0}}@layer a{.x{margin-left:5px}}"
    "@layer a,b;@layer b{.x{margin:0}}@layer a{.x{margin-left:5px}}";
  check_inline_case "layers writing disjoint slots still flatten"
    "@layer b{.x{color:blue}}@layer a{.y{padding:1px}}"
    ".x{color:blue}.y{padding:1px}"

(* An at-rule path is a containment test, not an equality: a custom property is
   folded into a consumer wrapped in every block it sits in and then some, and
   into nobody outside its own. A [@layer] is transparent to that test, so a
   path crossing one still has to line the conditional blocks up. Both
   directions pin the orientation the paths are compared in.

   The path decides what may be folded, never what a reference left standing can
   reach: the browser answers that one from its own cascade under whatever
   condition holds, so the definition goes to the output with the reference. *)
let test_inline_at_path_containment () =
  check_inline_case "an outer definition reaches a deeper consumer"
    "@media print{@media (min-width:10px){:root{--x:red}@layer l{@media \
     (min-width:20px){.a{color:var(--x)}}}}}"
    "@media print{@media(min-width:10px){@layer \
     l{@media(min-width:20px){.a{color:red}}}}}";
  check_inline_case "an inner definition is not folded into an outer consumer"
    "@media print{:root{--x:red}}.a{color:var(--x)}"
    "@media print{:root{--x:red}}.a{color:var(--x)}";
  check_inline_case "a sibling block at the same depth is not an enclosing one"
    "@media print{:root{--x:red}}@media screen{.a{color:var(--x)}}"
    "@media print{:root{--x:red}}@media screen{.a{color:var(--x)}}"

(* Liveness is decided by a fixpoint: a variable is live when a rule that can
   see it reads it, and a variable read by a live one is live too. The chain
   below is only reachable through several rounds of that propagation, and each
   link sits in a different at-rule scope, so it also pins the visibility rule
   the propagation carries. The tail is dead and must go. *)
let test_inline_vars_liveness_propagates_through_scopes () =
  let css =
    String.concat ""
      [
        ":root{--a:red;--dead:blue}";
        "@media print{:root{--b:var(--a)}}";
        "@media print{@supports (display:grid){:root{--c:var(--b)}}}";
        "@media print{@supports (display:grid){.x{color:var(--c)}}}";
      ]
  in
  let out = minified (Css.inline_vars (parse css)) in
  Alcotest.(check bool)
    ("the chain resolves to its root: " ^ out)
    true
    (holds_substring "color:red" out);
  Alcotest.(check bool)
    ("the unread variable is dropped: " ^ out)
    false
    (holds_substring "--dead" out)

(* --- allocation / complexity guard --- *)

(* [depth] nested [@media print] blocks around one rule that declares and reads
   a single custom property. The variable count, the scope count and the
   declaration count are all held at one, so the at-rule path depth is the only
   thing that varies. *)
let nested_media depth =
  let b = Buffer.create ((depth * 14) + 32) in
  for _ = 1 to depth do
    Buffer.add_string b "@media print{"
  done;
  Buffer.add_string b ".a{--x:red;color:var(--x)}";
  for _ = 1 to depth do
    Buffer.add_char b '}'
  done;
  parse (Buffer.contents b)

(* Each of the passes below descends carrying the chain of enclosing at-rules.
   Extending that chain by copying it costs one cell per enclosing block, so a
   depth-[d] chain pays 1 + 2 + ... + d per pass before a single liveness
   question is asked; sharing the tail costs one cell per level. That is the
   difference between quadrupling and doubling on twice the depth, so pin the
   ratio below the halfway point. Minor words rather than total allocation: the
   chain is short-lived scratch, and counting the major heap would fold in an
   output string that grows with the depth. *)
let test_inline_at_path_cost_is_linear () =
  let shallow = nested_media 100 and deep = nested_media 200 in
  let a1 = measure (fun () -> Css.inline_vars shallow) in
  let a2 = measure (fun () -> Css.inline_vars deep) in
  Alcotest.(check bool)
    (Fmt.str "alloc %.0f -> %.0f (%.2fx for 2x depth)" a1 a2 (a2 /. a1))
    true
    (a1 = 0. || a2 < a1 *. 2.5)

(* A [page-break-*] declaration holding a [var()] is inlined like any other, and
   the property it names has to survive the substitution. [page-break-*]
   serialises under minify as its CSS Fragmentation 3 sec. 3.4 [break-*] alias,
   so a pass that rebuilds the declaration from that spelling rebuilds a
   different property: [break-inside] does not even accept [always]. Built
   through the typed API because the reader has no [var()] arm for these
   three. *)
let test_inline_keeps_a_page_break_property () =
  let inline_one property value decl =
    Css.inline_vars
      [
        Stylesheet.Rule
          (Stylesheet.rule
             ~selector:(Selector.of_string ":root")
             [ Declaration.custom_property property value ]);
        Stylesheet.Rule
          (Stylesheet.rule ~selector:(Selector.of_string ".a") [ decl ]);
      ]
    |> Css.to_string
  in
  let check name expected decl value =
    Alcotest.(check string) name expected (inline_one "--pb" value decl)
  in
  check "page-break-after keeps its own property"
    ".a {\n  page-break-after: always;\n}"
    (Declaration.v Properties.Page_break_after (Var (Values.var_ref "pb")))
    "always";
  check "page-break-before keeps its own property"
    ".a {\n  page-break-before: always;\n}"
    (Declaration.v Properties.Page_break_before (Var (Values.var_ref "pb")))
    "always";
  check "page-break-inside keeps its own property"
    ".a {\n  page-break-inside: avoid;\n}"
    (Declaration.v Properties.Page_break_inside (Var (Values.var_ref "pb")))
    "avoid"

(* The same substitution as [test_inline_keeps_a_page_break_property], reached
   from CSS text rather than from a hand-built declaration. Reading a [var()]
   into the property the author wrote is what puts a [Page_break_*] carrying a
   [var()] in front of the inliner at all, so the property tag it rebuilds on
   has to survive the whole round trip. [break-inside] accepts neither [always]
   nor [avoid-page]. *)
let test_inline_keeps_a_page_break_property_from_css () =
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string)
        input expected
        (input |> parse |> Css.inline_vars |> minified))
    [
      ( ":root{--pb:always}.a{page-break-after:var(--pb)}",
        ".a{break-after:page}" );
      ( ":root{--pb:always}.a{page-break-before:var(--pb)}",
        ".a{break-before:page}" );
      ( ":root{--pb:avoid}.a{page-break-inside:var(--pb)}",
        ".a{break-inside:avoid}" );
      (":root{--pb:left}.a{page-break-after:var(--pb)}", ".a{break-after:left}");
      (* A substitution the legacy grammar rejects leaves the declaration in
         place rather than retyping it as a property that would accept it. *)
      ( ":root{--pb:avoid-page}.a{page-break-inside:var(--pb)}",
        ".a{page-break-inside:avoid-page}" );
    ]

let suite =
  ( "inline",
    [
      Alcotest.test_case "inline vars resolve a typed @font-face descriptor"
        `Quick test_inline_font_face_var_resolves;
      Alcotest.test_case "inline vars resolve a @font-face range endpoint"
        `Quick test_inline_font_face_var_stretch_range;
      Alcotest.test_case "inline vars propagate liveness across scopes" `Quick
        test_inline_vars_liveness_propagates_through_scopes;
      Alcotest.test_case "inline vars keep an out-of-scope definition live"
        `Quick test_inline_vars_out_of_scope_definition_stays_live;
      Alcotest.test_case "inline vars contain a definition to its at-rule path"
        `Quick test_inline_at_path_containment;
      Alcotest.test_case "inline vars cost stays linear in at-rule depth" `Quick
        test_inline_at_path_cost_is_linear;
      Alcotest.test_case "inline vars substitute visible custom properties"
        `Quick test_inline_substitutes_vars;
      Alcotest.test_case "inline vars keep requested references" `Quick
        test_inline_keep_vars;
      Alcotest.test_case "inline vars accept a third name dash" `Quick
        test_inline_triple_dash_custom_property;
      Alcotest.test_case "inline vars fold and delete non-kept definitions"
        `Quick test_inline_fold_deletes_defs;
      Alcotest.test_case "inline vars keep an at-rule reference's binding"
        `Quick test_inline_at_rule_reference_keeps_its_binding;
      Alcotest.test_case "inline vars warn for an at-rule reference" `Quick
        test_inline_warns_for_an_at_rule_reference;
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
      Alcotest.test_case "inline vars honour runtime metadata" `Quick
        test_inline_runtime_var_metadata;
      Alcotest.test_case "inline vars substitute across cascade layers" `Quick
        test_inline_across_layers;
      Alcotest.test_case
        "inline vars fold a layer-decided override to its winner" `Quick
        test_inline_layer_winner;
      Alcotest.test_case "inline vars keep what a custom declaration carries"
        `Quick test_inline_keeps_what_a_custom_declaration_carries;
      Alcotest.test_case "inline vars clean up inside a rule too" `Quick
        test_inline_cleanup_inside_a_rule;
      Alcotest.test_case
        "inline vars keep the registration of a property they keep" `Quick
        test_inline_property_registration_kept;
      Alcotest.test_case
        "inline vars drop the registration of a property they remove" `Quick
        test_inline_property_registration_dropped;
      Alcotest.test_case "inline vars reach a fixpoint on registrations" `Quick
        test_inline_property_idempotent;
      Alcotest.test_case "inline vars keep the property a style() query reads"
        `Quick test_inline_style_query_keeps_queried_property;
      Alcotest.test_case
        "inline vars keep the registration a style() query reads" `Quick
        test_inline_style_query_keeps_registration;
      Alcotest.test_case "inline vars keep no more than a style() query reads"
        `Quick test_inline_style_query_keeps_no_more;
      Alcotest.test_case "inline vars order layers by where they are named"
        `Quick test_inline_layer_order_sites;
      Alcotest.test_case "inline vars keep a layer that still decides a slot"
        `Quick test_inline_layer_flattening;
      Alcotest.test_case "inline vars keep a page-break property" `Quick
        test_inline_keeps_a_page_break_property;
      Alcotest.test_case "inline vars keep a page-break property read from CSS"
        `Quick test_inline_keeps_a_page_break_property_from_css;
      Alcotest.test_case
        "inline vars resolve the @font-face descriptors the parser keeps" `Quick
        test_inline_font_face_var_descriptors;
      Alcotest.test_case "inline vars reject @font-face family lists" `Quick
        test_inline_font_face_family_name;
    ] )
