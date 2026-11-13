(** Stylesheet tests: canonical spellings under minify and faithful pretty
    printing. *)

open Cascade
module Selector = Css.Selector
open Stylesheet_helpers

(* CSS Cascade Module Level 6, section 2 (Importing Style Sheets): @import
   identifies an external style sheet by URL. The cascade treats the import as a
   substitution point but does not fetch or inline the referenced sheet at the
   syntax layer; the URL string and conditional clauses must survive parse and
   print without resolution or rewriting. *)
let c6_2_import_preserved_verbatim () =
  let url = "https://example.test/path/to/theme.css?v=1" in
  let input =
    "@import url(\"" ^ url
    ^ "\") layer(framework.theme) supports(display:grid) screen and \
       (min-width:30em);"
  in
  let r = Cursor.of_string input in
  let sheet = Css.Stylesheet.read r in
  let printed = String.trim (Css.Stylesheet.to_string ~minify:true sheet) in
  let contains_url s = Astring.String.is_infix ~affix:url s in
  Alcotest.(check bool)
    "import URL string survives parse and print verbatim" true
    (contains_url printed);
  Alcotest.(check bool)
    "layer name preserved on import" true
    (Astring.String.is_infix ~affix:"layer(framework.theme)" printed);
  Alcotest.(check bool)
    "supports() condition preserved on import" true
    (Astring.String.is_infix ~affix:"supports(display:grid)" printed);
  Alcotest.(check bool)
    "media query list preserved on import" true
    (Astring.String.is_infix ~affix:"screen and (min-width:30em)" printed);
  let optimized = Css.Optimize.stylesheet sheet in
  let opt_printed =
    String.trim (Css.Stylesheet.to_string ~minify:true optimized)
  in
  Alcotest.(check bool)
    "optimize does not resolve, inline, or rewrite the import URL" true
    (contains_url opt_printed);
  Alcotest.(check int)
    "no rule statements are synthesized from the import" 0
    (List.length (Css.Stylesheet.rules optimized))

(* CSS Syntax Level 3, section 4.3.2 (Consume comments): comments are consumed
   during tokenization and produce no tokens. The /*# sourceMappingURL=... */
   and /*# sourceURL=... */ pragmas are developer-tool conventions with no W3C
   CSS specification: to a conforming CSS parser they are ordinary comments at
   every position they may appear (top of file, between rules, inside a rule's
   declaration block, and inside an at-rule's block). They must not survive into
   the AST or the serialized output, and they must not influence the surrounding
   rule. *)
let s3432_sourcemap_comment () =
  let cases =
    [
      ( "pragma at top of file",
        "/*# sourceMappingURL=app.css.map */ .a { color: red }",
        ".a{color:red}" );
      ( "pragma between rules",
        ".a { color: red } /*# sourceMappingURL=app.css.map */ .b { color: \
         blue }",
        ".a{color:red}.b{color:blue}" );
      ( "pragma at end of file",
        ".a { color: red } /*# sourceMappingURL=app.css.map */",
        ".a{color:red}" );
      ( "pragma inside declaration block",
        ".a { color: red; /*# sourceMappingURL=app.css.map */ padding: 1px }",
        ".a{color:red;padding:1px}" );
      ( "pragma inside at-rule block",
        "@media screen { /*# sourceMappingURL=app.css.map */ .a { color: red } \
         }",
        "@media screen{.a{color:red}}" );
      ( "sourceURL pragma alongside sourceMappingURL",
        "/*# sourceMappingURL=app.css.map */ /*# sourceURL=app.css */ .a { \
         color: red }",
        ".a{color:red}" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      let r = Cursor.of_string input in
      let sheet = Css.Stylesheet.read r in
      let printed = String.trim (Css.Stylesheet.to_string ~minify:true sheet) in
      Alcotest.(check string) name expected printed)
    cases

(* CSS Cascade Module Level 5, section 6.4.4.2 (The Layer Statement Rule): the
   statement form [@layer foo, bar;] is defined as equivalent to declaring each
   named layer with an empty block in the same order. The two surface shapes
   must therefore parse to the same effective layer order and produce the same
   serialization once empty blocks are normalized. *)
let c6442_empty_blocks_equiv () =
  let parse css =
    Css.Stylesheet.read (Cursor.of_string css)
    |> Css.Optimize.stylesheet
    |> Css.Stylesheet.to_string ~minify:true
    |> String.trim
  in
  let statement_form = parse "@layer reset, theme, components;" in
  let empty_block_form =
    parse "@layer reset {} @layer theme {} @layer components {}"
  in
  Alcotest.(check string)
    "statement form and empty-block form normalize to the same output"
    statement_form empty_block_form;
  let statement_then_block =
    parse "@layer reset, theme; @layer reset { .a { color: red } }"
  in
  let two_blocks =
    parse "@layer reset {} @layer theme {} @layer reset { .a { color: red } }"
  in
  Alcotest.(check string)
    "subsequent block in a previously-declared layer is order-equivalent"
    statement_then_block two_blocks

(* CSS Cascade Module Level 5, section 6.4.2 (Layer Naming and Nesting): a
   dotted layer name is shorthand for nested layer blocks. The spec says [@layer
   foo.bar { ... }] declares the same nested layer as [@layer foo { @layer bar {
   ... } }], so the rule placed inside either form must end up in the same
   effective cascade layer named [foo.bar]. *)
let c643_dotted_nested_layer () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let dotted = parse "@layer foo.bar { .x { color: red } }" in
  let nested = parse "@layer foo { @layer bar { .x { color: red } } }" in
  let inner_rule_text = function
    | Some stmts -> Css.to_string ~minify:true (Css.v stmts) |> String.trim
    | None -> "<no foo.bar layer>"
  in
  Alcotest.(check string)
    "the inner rule appears in the foo.bar layer regardless of input form"
    (inner_rule_text (Css.layer_block [ "foo"; "bar" ] dotted))
    (inner_rule_text (Css.layer_block [ "foo"; "bar" ] nested));
  Alcotest.(check (list (list string)))
    "both forms expose the same set of declared layers" (Css.layers dotted)
    (Css.layers nested)

(* CSS Syntax Level 3, section 4.3.2 (Consume comments): comments are stripped
   at tokenization. The /*# sourceMappingURL */ and /*# sourceURL */ pragmas are
   developer-tool conventions with no W3C status, so a conforming printer must
   never emit them on output regardless of what was in the input. The absence of
   the [/*#] sequence in the printed stylesheet is an implementation-independent
   guarantee that there is no source-map support. *)
let s3432_no_sourcemap_print () =
  let inputs =
    [
      ".a{color:red}";
      "/*# sourceMappingURL=foo.css.map */ .a { color: red }";
      "@media screen { /*# sourceMappingURL=foo.css.map */ .a { color: red } }";
      "@import url(\"a.css\"); /*# sourceURL=a.css */ .a { color: red }";
    ]
  in
  List.iter
    (fun input ->
      let r = Cursor.of_string input in
      let sheet = Css.Stylesheet.read r in
      let printed = Css.Stylesheet.to_string ~minify:true sheet in
      Alcotest.(check bool)
        ("printed stylesheet contains no /*# pragma for input: " ^ input)
        false
        (Astring.String.is_infix ~affix:"/*#" printed);
      Alcotest.(check bool)
        ("printed stylesheet contains no sourceMappingURL for input: " ^ input)
        false
        (Astring.String.is_infix ~affix:"sourceMappingURL" printed);
      Alcotest.(check bool)
        ("printed stylesheet contains no sourceURL for input: " ^ input)
        false
        (Astring.String.is_infix ~affix:"sourceURL" printed))
    inputs

(* CSS Values and Units Module Level 4, section 6 (Distance Units): "A 0 length
   is unitless and may be substituted for 0px or 0em (etc.) in any
   length-accepting context." Two property values that differ only in the
   spelling of a zero length are spec-equivalent, so the optimizer is free to
   pick any of them - but parsing two equivalent forms must produce the same
   computed declaration after a round-trip through the optimizer. *)
let v461_zero_length_equiv () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let zero_unitless = normalize ".a { margin: 0 }" in
  let zero_px = normalize ".a { margin: 0px }" in
  let zero_em = normalize ".a { margin: 0em }" in
  let zero_rem = normalize ".a { margin: 0rem }" in
  Alcotest.(check string)
    "0 and 0px are spec-equivalent for <length>" zero_unitless zero_px;
  Alcotest.(check string)
    "0 and 0em are spec-equivalent for <length>" zero_unitless zero_em;
  Alcotest.(check string)
    "0 and 0rem are spec-equivalent for <length>" zero_unitless zero_rem

(* CSS Color Module Level 4, section 5.2 (Hex Notation): a 6-digit hex color
   [#rrggbb] and the equivalent 3-digit shorthand [#rgb] (where each pair is the
   same character) denote the identical sRGB color. They are spec- equivalent
   forms; the optimizer may choose either, but a round trip must yield the same
   color regardless of input spelling. *)
let color4121_hex_equiv () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let pairs =
    [
      ("#ffffff", "#fff");
      ("#000000", "#000");
      ("#112233", "#123");
      ("#aabbcc", "#abc");
    ]
  in
  List.iter
    (fun (long, short) ->
      let long_form = normalize (".a { color: " ^ long ^ " }") in
      let short_form = normalize (".a { color: " ^ short ^ " }") in
      Alcotest.(check string)
        ("hex " ^ long ^ " and " ^ short ^ " are spec-equivalent")
        long_form short_form)
    pairs

(* CSS Cascade and Inheritance Module Level 5, section 6.1 (Cascade Sorting
   Order): when all higher-priority criteria tie, declarations are ordered by
   source order, with later declarations winning. The optimizer is free to merge
   or rewrite rules, but it MUST NOT reorder declarations of the same property
   in a way that changes the cascaded result. A round trip through the optimizer
   must preserve the winning value when two declarations of the same property
   tie on origin, importance, layer and specificity. *)
let rec color_value_stop printed j =
  if j >= String.length printed then j
  else if printed.[j] = ';' || printed.[j] = '}' then j
  else color_value_stop printed (j + 1)

let rec last_color_value printed start_idx best =
  match Astring.String.find_sub ~start:start_idx ~sub:"color:" printed with
  | None -> best
  | Some i ->
      let after = i + String.length "color:" in
      let stop = color_value_stop printed after in
      let value = String.sub printed after (stop - after) in
      last_color_value printed (i + 1) (Some value)

let c61_keeps_winner () =
  let winning_color css =
    match Css.of_string ~strict:false css with
    | Ok parsed ->
        let printed = minify parsed.stylesheet |> String.trim in
        (* Last occurrence of "color:" in the optimized output is the cascaded
           winner under section 6.1's "later wins" rule for tied candidates. *)
        last_color_value printed 0 None
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check (option string))
    "later declaration wins after optimization (same selector)" (Some "#00f")
    (winning_color ".a { color: red; color: blue }");
  Alcotest.(check (option string))
    "later rule wins after optimization (same selector consecutive)"
    (Some "#00f")
    (winning_color ".a { color: red } .a { color: blue }")

(* CSS Color Module Level 4, section 5.1 (The RGB functions): the named color
   [red], the hex notations [#f00] and [#ff0000], and the rgb function [rgb(255,
   0, 0)] all denote the same sRGB color. Under industry-standard minification
   (cssnano / Lightning CSS / clean-css) the printer canonicalizes to the
   shortest equivalent form, so all of these resolve to the same serialized
   output. *)
let color414_form_equiv () =
  (* These forms parse to distinct color nodes; collapsing them to the shortest
     spelling is an optimize transform, not a pp same-node choice. Canonicalize
     through optimize+minify. *)
  let canonical s =
    match
      Css.of_string ~strict:false (String.concat "" [ ".x{color:"; s; "}" ])
    with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse color: %s" s
  in
  (* These hex forms all denote the same sRGB color (255, 0, 0) and optimize
     canonicalizes each to the named color [red]. The rgb() function forms are
     also spec-equivalent and could canonicalize the same way; that is an
     additional optimizer freedom not asserted here. *)
  let forms = [ "red"; "#f00"; "#ff0000" ] in
  match forms with
  | [] -> ()
  | first :: rest ->
      let first_canonical = canonical first in
      List.iter
        (fun form ->
          Alcotest.(check string)
            (form ^ " is spec-equivalent to " ^ first)
            first_canonical (canonical form))
        rest

(* CSS Color Module Level 4 defines named colors and hex colors as alternate
   notations for the same sRGB colors. For minification, the suite uses a single
   deterministic shortest-form policy: choose the shortest spelling, and when a
   named color and a short hex spelling are the same length, choose hex. This
   matches the pinned Lightning CSS trace for equal-length examples such as
   [yellow] -> [#ff0] and keeps parse -> minify -> parse -> minify idempotent
   for [blue] / [#00f] and [lime] / [#0f0]. *)
let color4_hex_tie_policy () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let cases =
    [
      ("red is shorter than #f00", ".x{color:red}", ".x { color: red }");
      ("blue ties #00f, hex wins", ".x{color:#00f}", ".x { color: blue }");
      ("#0000ff shortens to #00f", ".x{color:#00f}", ".x { color: #0000ff }");
      ("lime ties #0f0, hex wins", ".x{color:#0f0}", ".x { color: lime }");
      ( "hsl green canonicalizes through the same tie policy",
        ".x{color:#0f0}",
        ".x { color: hsl(120 100% 50%) }" );
      ("yellow ties #ff0, hex wins", ".x{color:#ff0}", ".x { color: yellow }");
      ("black uses shorter hex", ".x{color:#000}", ".x { color: black }");
      ("white uses shorter hex", ".x{color:#fff}", ".x { color: white }");
      ("gray is shorter than #808080", ".x{color:gray}", ".x { color: gray }");
    ]
  in
  List.iter
    (fun (name, expected, css) ->
      Alcotest.(check string) name expected (normalize css))
    cases

(* CSS Values and Units Module Level 4, section 6 only allows dropping units
   from zero lengths. Percentages keep their percent sign because [0%] is still
   a percentage token, not the bare number [0]. *)
let v465_zero_percentage_equiv () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let zero = normalize ".a { width: 0 }" in
  let zero_pct = normalize ".a { width: 0% }" in
  let zero_px = normalize ".a { width: 0px }" in
  Alcotest.(check string)
    "0 and 0px are spec-equivalent for zero length" zero zero_px;
  Alcotest.(check string)
    "0% preserves its percentage token" ".a{width:0%}" zero_pct;
  Alcotest.(check bool)
    "0 and 0% stay distinct for width" false
    (String.equal zero zero_pct)

(* CSSOM Level 1, section 6.6 (Serialize a CSS declaration): the serialized form
   puts ":" between property name and value with no surrounding spaces in
   minified mode, and "!important" follows the value with no extra whitespace.
   The property name is serialized as-is (already lowercased by the syntax layer
   per CSS Syntax sec. 8.1) regardless of input case. *)
let cssom662_decl_serialization () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "uppercase property name lowercased on serialize" ".a{color:red}"
    (normalize ".a { COLOR: red }");
  Alcotest.(check string)
    "mixed-case property name lowercased on serialize"
    ".a{background-color:red}"
    (normalize ".a { Background-Color: red }");
  Alcotest.(check string)
    "important serialized without internal whitespace" ".a{color:red!important}"
    (normalize ".a { color: red !important }");
  Alcotest.(check string)
    "important whitespace after ! collapsed" ".a{color:red!important}"
    (normalize ".a { color: red !  IMPORTANT }")

(* CSS Color Module Level 4, section 6.3 (The "transparent" color keyword): the
   [transparent] keyword is defined as equivalent to [rgba(0, 0, 0, 0)]. The
   8-digit hex [#00000000] (or its 4-digit shorthand [#0000]) is also equivalent
   to that fully-transparent black per section 5.2. The optimizer may pick any
   spec-equivalent form; the test asserts the parsed colors denote the same
   value via the canonical printer. *)
let color4_6_4_transparent_equivalence () =
  (* These four forms parse to distinct color nodes, so collapsing them to one
     canonical spelling is an optimize transform, not a pp one: canonicalize
     through optimize, not bare pp. *)
  let canonical s =
    match Css.of_string ~strict:false (".x{color:" ^ s ^ "}") with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse color: %s" s
  in
  let forms =
    [ "transparent"; "rgba(0, 0, 0, 0)"; "rgb(0 0 0 / 0)"; "#0000" ]
  in
  match forms with
  | [] -> ()
  | first :: rest ->
      let first_canonical = canonical first in
      List.iter
        (fun form ->
          Alcotest.(check string)
            (form ^ " is spec-equivalent to " ^ first)
            first_canonical (canonical form))
        rest

(* CSS Color Module Level 4, section 6.1 (Named Colors): "CSS named colors are
   ASCII case-insensitive." [RED], [Red], and [red] all denote the same color
   and must serialize identically. *)
let color461_named_case () =
  let canonical s =
    match Css.parse_color s with
    | Some c -> Css.Pp.to_string ~minify:true Css.pp_color c
    | None -> Alcotest.failf "failed to parse color: %s" s
  in
  let cases =
    [
      ("red", "RED");
      ("red", "Red");
      ("blue", "BLUE");
      ("rebeccapurple", "RebeccaPurple");
    ]
  in
  List.iter
    (fun (lower, mixed) ->
      Alcotest.(check string)
        (mixed ^ " is case-equivalent to " ^ lower)
        (canonical lower) (canonical mixed))
    cases

(* CSS Values and Units Module Level 4, section 5.3 (Numbers and Numeric Data
   Types): [<number>] tokens accept several syntactic spellings of the same
   numeric value. [.5], [0.5] and [0.50] all denote 0.5; [1.0] and [1] denote
   the integer 1. The optimizer is free to pick the shortest spelling, but a
   round trip must yield the same numeric value regardless of input format. *)
let v481_number_format () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let pairs =
    [
      (".5", "0.5"); (".5", "0.50"); ("1", "1.0"); ("1", "1.00"); (".25", "0.25");
    ]
  in
  List.iter
    (fun (a, b) ->
      let a_form = normalize (".x { opacity: " ^ a ^ " }") in
      let b_form = normalize (".x { opacity: " ^ b ^ " }") in
      Alcotest.(check string)
        ("opacity " ^ a ^ " and " ^ b ^ " are spec-equivalent")
        a_form b_form)
    pairs

(* CSS Selectors Module Level 4, section 5.2 (The Universal Selector): "If the
   universal selector is not the only component of a compound selector, the [*]
   may be omitted." So [*.foo] and [.foo] are spec-equivalent compound
   selectors, and [*#id] and [#id] are equivalent. *)
let s435_universal_redundant () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let pairs =
    [
      ("*.foo { color: red }", ".foo { color: red }");
      ("*#main { color: red }", "#main { color: red }");
      ("*[data-x] { color: red }", "[data-x] { color: red }");
      ("*:hover { color: red }", ":hover { color: red }");
    ]
  in
  List.iter
    (fun (with_star, without_star) ->
      Alcotest.(check string)
        (with_star ^ " is spec-equivalent to " ^ without_star)
        (normalize without_star) (normalize with_star))
    pairs;
  (* Dropping the redundant universal is a node change, so pp must HOLD it in
     both pretty and minify; only optimize drops it. A minify-only drop is a
     pp-purity bug - these assert the held minify form so it is detected. *)
  assert_minify_and_optimize "*.foo { color: red }" ~minified:"*.foo{color:red}"
    ~optimized:".foo{color:red}";
  assert_minify_and_optimize "*#main { color: red }"
    ~minified:"*#main{color:red}" ~optimized:"#main{color:red}";
  assert_minify_and_optimize "*[data-x] { color: red }"
    ~minified:"*[data-x]{color:red}" ~optimized:"[data-x]{color:red}";
  assert_minify_and_optimize "*:hover { color: red }"
    ~minified:"*:hover{color:red}" ~optimized:":hover{color:red}"

(* A comma-separated selector list is a union of its branches: reordering the
   branches and removing duplicate branches leaves both matching and the cascade
   unchanged, so each is a safe rewrite. But each is also a node change, so pp
   must HOLD the authored branch order (duplicates included) in BOTH pretty and
   minify; sorting into Cascade's canonical selector order and deduping branches
   are optimize transforms. Selector.pp doing the sort+dedup on every print is a
   pp-purity bug (and a per-print cost: a Pp.to_string key per branch on every
   to_string and every size) - these assert the held minify form so it is
   detected, alongside the canonical optimize form. *)
let selector_list_canonical_order () =
  assert_minify_and_optimize "div, .class, #id { color: red }"
    ~minified:"div,.class,#id{color:red}" ~optimized:"#id,.class,div{color:red}";
  assert_minify_and_optimize ".a, .a, .b { color: red }"
    ~minified:".a,.a,.b{color:red}" ~optimized:".a,.b{color:red}"

(* CSS Cascading and Inheritance / Scoping: the @scope prelude carries two
   selector lists - the scope-start [(<scope-start>)] and the scope-end [to
   (<scope-end>)], each a forgiving selector list whose branches are an
   unordered set. The selector-list policy applies to both: pp holds the
   authored branch order and duplicates in both pretty and minify ([minify]
   sides), and optimize sorts and de-duplicates both preludes ([minify+optimize]
   sides). *)
let scope_selector_list_canonical () =
  (* scope-start list: sorted by optimize, held by pp *)
  assert_minify_and_optimize "@scope (.b, .a) { .x { color: red } }"
    ~minified:"@scope(.b,.a){.x{color:red}}"
    ~optimized:"@scope(.a,.b){.x{color:red}}";
  (* scope-start list: duplicate branch dropped by optimize *)
  assert_minify_and_optimize "@scope (.a, .a) { .x { color: red } }"
    ~minified:"@scope(.a,.a){.x{color:red}}"
    ~optimized:"@scope(.a){.x{color:red}}";
  (* scope-end list: duplicate branch dropped by optimize *)
  assert_minify_and_optimize "@scope (:root) to (.b, .b) { .x { color: red } }"
    ~minified:"@scope(:root)to (.b,.b){.x{color:red}}"
    ~optimized:"@scope(:root)to (.b){.x{color:red}}";
  (* both scope-start and scope-end lists canonicalised at once *)
  assert_minify_and_optimize "@scope (.b, .a) to (.d, .c) { .x { color: red } }"
    ~minified:"@scope(.b,.a)to (.d,.c){.x{color:red}}"
    ~optimized:"@scope(.a,.b)to (.c,.d){.x{color:red}}"

(* CSS Color Module Level 4, section 4.3 (the <hue> syntax): the [<hue>]
   component accepts angles or numbers and is normalised modulo 360deg. So
   [hsl(360 100% 50%)] denotes the same color as [hsl(0 100% 50%)] and as the
   named color [red]; [hsl(720 ...)] is also red. Hue values outside [0, 360)
   must reduce, and the fully-saturated red hue must canonicalize to the named
   color under industry-standard minification. *)
let color4_3_hue_modulo_canonicalization () =
  (* Hue-modulo reduction and hsl -> named are optimize transforms, not pp
     same-node spellings; route each form through optimize+minify. *)
  let canonical s =
    match
      Css.of_string ~strict:false (String.concat "" [ ".x{color:"; s; "}" ])
    with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse color: %s" s
  in
  let red = canonical "red" in
  Alcotest.(check string)
    "hsl(0 100% 50%) is red" red
    (canonical "hsl(0 100% 50%)");
  Alcotest.(check string)
    "hsl(360 100% 50%) reduces modulo 360" red
    (canonical "hsl(360 100% 50%)");
  Alcotest.(check string)
    "hsl(720 100% 50%) reduces modulo 360" red
    (canonical "hsl(720 100% 50%)");
  Alcotest.(check string)
    "hsl(-360 100% 50%) reduces modulo 360" red
    (canonical "hsl(-360 100% 50%)")

(* CSS Color Module Level 4, section 4.2 (the <alpha-value> syntax): an alpha
   value may be expressed as a [<number>] in the closed interval [\[0, 1\]] or
   as a [<percentage>] in [\[0%, 100%\]]; the two forms are spec-equivalent.
   [rgb(255 0 0 / .5)] and [rgb(255 0 0 / 50%)] denote the identical color. *)
let color413_alpha_equiv () =
  let canonical s =
    match Css.of_string ~strict:false (".x{color:" ^ s ^ "}") with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse color: %s" s
  in
  let pairs =
    [
      ("rgb(255 0 0 / 0.5)", "rgb(255 0 0 / 50%)");
      ("rgb(0 0 0 / 0.25)", "rgb(0 0 0 / 25%)");
      ("hsl(0 100% 50% / 0.75)", "hsl(0 100% 50% / 75%)");
    ]
  in
  List.iter
    (fun (number_form, percent_form) ->
      Alcotest.(check string)
        (number_form ^ " is spec-equivalent to " ^ percent_form)
        (canonical number_form) (canonical percent_form))
    pairs

(* CSS Animations Module Level 1, section 3 (Keyframe Selectors): the keyframe
   selectors [from] and [to] are defined as equivalent to [0%] and [100%]
   respectively. A round trip through the parser and printer therefore places
   the rule in the same logical keyframe regardless of which spelling was used
   in the input. *)
let anim171_keyframe_equiv () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "@keyframes [from] is spec-equivalent to [0%]"
    (normalize "@keyframes fade { from { opacity: 0 } to { opacity: 1 } }")
    (normalize "@keyframes fade { 0% { opacity: 0 } 100% { opacity: 1 } }");
  Alcotest.(check string)
    "@keyframes mixed [from]/[100%] and [0%]/[to] are spec-equivalent"
    (normalize "@keyframes fade { from { opacity: 0 } 100% { opacity: 1 } }")
    (normalize "@keyframes fade { 0% { opacity: 0 } to { opacity: 1 } }")

(* CSS Color Module Level 4, sections 1.4 and 12 (rgb() Functional Notation):
   integer channels outside [0, 255] are clamped to that range, and a
   fully-opaque rgb() with all-zero channels is the same color as [black]. Both
   Lightning CSS and cssnano canonicalize the clamped result to the shortest
   equivalent spelling. *)
let color4_12_rgb_clamp_canonicalization () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "rgb(300, 0, 0) clamps to red and canonicalizes" ".x{color:red}"
    (normalize ".x { color: rgb(300, 0, 0) }");
  Alcotest.(check string)
    "rgb(-10, 0, 0) clamps to black and canonicalizes to #000" ".x{color:#000}"
    (normalize ".x { color: rgb(-10, 0, 0) }")

(* CSS Color Module Level 4, section 4.2 (the <alpha-value> syntax): an alpha
   value of [1] (or [100%]) means fully opaque, which is by definition the same
   color as the form without an alpha channel. So [rgba(255, 0, 0, 1)] is
   spec-equivalent to [rgb(255, 0, 0)] and to [red]. Both Lightning CSS and
   cssnano collapse the redundant alpha. *)
let color413_opaque_alpha_collapse () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "rgba(255, 0, 0, 1) collapses to red" ".x{color:red}"
    (normalize ".x { color: rgba(255, 0, 0, 1) }");
  Alcotest.(check string)
    "hsla(0, 100%, 50%, 1) collapses to red" ".x{color:red}"
    (normalize ".x { color: hsla(0, 100%, 50%, 1) }");
  Alcotest.(check string)
    "rgba(0, 0, 0, 100%) collapses to #000" ".x{color:#000}"
    (normalize ".x { color: rgba(0, 0, 0, 100%) }")

(* CSS Values and Units Module Level 4, section 5.3 (Numbers): [<number>] tokens
   with trailing zeroes after a decimal point ([1.000], [1.500]) are equivalent
   to the same value without the trailing zeroes ([1], [1.5]). Both Lightning
   CSS and cssnano normalise these. *)
let v481_trailing_zero () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "1.000em loses trailing zeroes" ".x{width:1em}"
    (normalize ".x { width: 1.000em }");
  Alcotest.(check string)
    "1.500em loses trailing zero" ".x{width:1.5em}"
    (normalize ".x { width: 1.500em }");
  Alcotest.(check string)
    "0.500 loses both leading and trailing zero" ".x{opacity:.5}"
    (normalize ".x { opacity: 0.500 }")

(* CSS Fonts Module Level 4, section 2.2 (Common Weight Name Mapping): the
   keywords [normal] and [bold] for [font-weight] are defined as numeric values
   [400] and [700] respectively. Both Lightning CSS and cssnano canonicalize to
   the numeric form. *)
let fonts4512_weight_number () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "font-weight: normal canonicalizes to 400"
    (normalize ".x { font-weight: 400 }")
    (normalize ".x { font-weight: normal }");
  Alcotest.(check string)
    "font-weight: bold canonicalizes to 700"
    (normalize ".x { font-weight: 700 }")
    (normalize ".x { font-weight: bold }")

(* CSS Box Model Module Level 4, sections 7.1 (margin) and 8.1 (padding): the
   margin / padding shorthand expands to four sides and collapses to shorter
   forms when sides repeat. [1px 1px 1px 1px] equals [1px]; [1px 2px 1px 2px]
   equals [1px 2px]. Both Lightning CSS and cssnano apply this collapse. *)
let box4_margin_shorthand_collapse () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "margin: 1px 1px 1px 1px collapses to 1px"
    (normalize ".x { margin: 1px }")
    (normalize ".x { margin: 1px 1px 1px 1px }");
  Alcotest.(check string)
    "margin: 0 0 0 0 collapses to 0"
    (normalize ".x { margin: 0 }")
    (normalize ".x { margin: 0 0 0 0 }");
  Alcotest.(check string)
    "padding: 1px 2px 1px 2px collapses to 1px 2px"
    (normalize ".x { padding: 1px 2px }")
    (normalize ".x { padding: 1px 2px 1px 2px }")

(* CSS Color Module Level 4, section 6.3 (The transparent keyword): all forms of
   fully-transparent black ([transparent], [rgba(0, 0, 0, 0)], [#00000000],
   [#0000]) denote the same color. The spec leaves the canonical serialized form
   to implementations; the printer canonicalizes to the shortest spec-equivalent
   spelling, matching Lightning CSS. *)
let color464_transparent_shortest () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let canonical = ".x{color:#0000}" in
  Alcotest.(check string)
    "#0000 is the shortest canonical for transparent" canonical
    (normalize ".x { color: transparent }");
  Alcotest.(check string)
    "rgba(0, 0, 0, 0) canonicalizes to #0000" canonical
    (normalize ".x { color: rgba(0, 0, 0, 0) }");
  Alcotest.(check string)
    "#00000000 canonicalizes to #0000" canonical
    (normalize ".x { color: #00000000 }")

(* CSS Values and Units Module Level 4, section 6 lets zero lengths drop their
   unit. It does not turn zero percentages into numbers. *)
let v465_zero_length_shortest () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let canonical = ".x{width:0}" in
  Alcotest.(check string)
    "0px canonicalizes to 0" canonical
    (normalize ".x { width: 0px }");
  Alcotest.(check string)
    "0em canonicalizes to 0" canonical
    (normalize ".x { width: 0em }");
  Alcotest.(check string)
    "0% stays a percentage" ".x{width:0%}"
    (normalize ".x { width: 0% }");
  Alcotest.(check string)
    "0vh canonicalizes to 0" canonical
    (normalize ".x { width: 0vh }")

(* CSS Backgrounds and Borders Module Level 3, section 4.1 (Border Radius):
   [border-radius] takes one to four [<length-percentage>] values. When all four
   sides are equal, the shorthand collapses to a single value. The spec does not
   mandate the collapsed form; the printer takes the freedom and picks the
   single-value shorthand, matching Lightning CSS. *)
let bg35_radius_collapse () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "border-radius: 0 0 0 0 collapses to 0" ".x{border-radius:0}"
    (normalize ".x { border-radius: 0 0 0 0 }");
  Alcotest.(check string)
    "border-radius: 1px 1px 1px 1px collapses to 1px" ".x{border-radius:1px}"
    (normalize ".x { border-radius: 1px 1px 1px 1px }")

(* CSS Values 4 sec. 10.9 determines a math function's type before simplifying
   it. A unitless zero is a [<number>], so adding it to a dimension or using the
   numeric result where a dimension is required makes the declaration invalid.
   Variables remain deferred until substitution, and zero terms with compatible
   dimension types remain valid. *)
let v410_calc_add_zero () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let rejects label css =
    Alcotest.(check bool)
      label true
      (match Css.of_string ~strict:true css with
      | Error _ -> true
      | Ok _ -> false)
  in
  let accepts label css =
    Alcotest.(check bool)
      label true
      (match Css.of_string ~strict:true css with
      | Ok _ -> true
      | Error _ -> false)
  in
  rejects "number plus length is rejected" ".x { width: calc(0 + 1px) }";
  rejects "length plus number is rejected" ".x { width: calc(1px + 0) }";
  rejects "a numeric calc cannot be a width" ".x { width: calc(0) }";
  rejects "number plus angle is rejected" ".x { rotate: calc(45deg + 0) }";
  rejects "number plus time is rejected"
    ".x { transition-duration: calc(1s + 0) }";
  rejects "number plus percentage is rejected" ".x { opacity: calc(.5 + 0%) }";
  rejects "line-height number plus percentage is rejected"
    ".x { line-height: calc(1 + 0%) }";
  accepts "number plus number remains valid" ".x { opacity: calc(.5 + 0) }";
  accepts "a variable keeps type checking deferred"
    ".x { width: calc(var(--size) + 0) }";
  Alcotest.(check string)
    "a cross-unit zero stays in a length sum" ".x{width:calc(1em + 0px)}"
    (normalize ".x { width: calc(1em + 0px) }");
  Alcotest.(check string)
    "a zero percentage stays in a length sum" ".x{width:calc(1px + 0%)}"
    (normalize ".x { width: calc(1px + 0%) }");
  Alcotest.(check string)
    "calc(1px + 0px) simplifies to 1px" ".x{width:1px}"
    (normalize ".x { width: calc(1px + 0px) }")

(* CSS Backgrounds and Borders Module Level 3, section 2.6
   (background-position): the two-value form [<x> <y>] collapses to the
   single-value form when [x = y]. The single-value form is the shortest
   spec-equivalent spelling that Lightning CSS picks. *)
let bg336_bgpos_collapse () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "background-position: 50% 50% collapses to 50%"
    ".x{background-position:50%}"
    (normalize ".x { background-position: 50% 50% }");
  Alcotest.(check string)
    "background-position: 0 0 preserves both axes" ".x{background-position:0 0}"
    (normalize ".x { background-position: 0 0 }")

(* {2 Non-minified fidelity}

   Under the default [~minify:false] mode the printer must be faithful to the
   input: spec-allowed canonicalizations like hex shortening, named-color
   conversion, [from]/[to] -> [0%]/[100%] mapping, universal-selector stripping,
   alpha number/percentage canonicalization, and zero-unit drop are all
   minify-only optimizations. A pretty-printed stylesheet should preserve the
   source spelling so the output is suitable for human reading and diffing
   without losing information. *)

(* CSS Color 4 section 5.2 + cascade convention: authored hex colours decode to
   sRGB bytes but keep their source spelling for pretty-printing, while
   minify+optimize emits the shorter equivalent spelling. *)
(* The pretty printer formats every braced at-rule body with the same line
   discipline as a style rule body: one declaration per line at one indent
   level deeper, a trailing semicolon on the last declaration, and the closing
   brace back at the parent's indentation. Sibling blocks inside @keyframes
   are separated by a blank line, like statements in any other block. The
   oracle is Tailwind's authored output format (e.g. the @keyframes bounce
   block in tailwindcss theme.css). *)
let pretty_at_rule_block_bodies () =
  let pretty css =
    match Css.of_string ~strict:false css with
    | Error _ -> Alcotest.failf "failed to parse: %s" css
    | Ok parsed -> Css.to_string parsed.stylesheet |> String.trim
  in
  Alcotest.(check string)
    "@property body formats like a rule body"
    "@property --x {\n\
    \  syntax: \"<length>\";\n\
    \  inherits: true;\n\
    \  initial-value: 0px;\n\
     }"
    (pretty
       "@property --x { syntax: \"<length>\"; inherits: true; initial-value: \
        0px }");
  Alcotest.(check string)
    "@font-face body formats like a rule body"
    "@font-face {\n  font-family: MyFont;\n  src: url(font.woff2);\n}"
    (pretty "@font-face { font-family: MyFont; src: url(font.woff2) }");
  Alcotest.(check string)
    "@keyframes frames format like sibling blocks"
    "@keyframes spin {\n\
    \  from {\n\
    \    transform: rotate(0deg);\n\
    \  }\n\n\
    \  to {\n\
    \    transform: rotate(360deg);\n\
    \  }\n\
     }"
    (pretty
       "@keyframes spin { from { transform: rotate(0deg) } to { transform: \
        rotate(360deg) } }")

(* CSS Syntax 3 sec. 4.3.14 lets each half of a unicode-range carry leading
   zeros up to six digits, and the two halves are padded independently: the AST
   records a width per half so pretty output gives the author back what they
   wrote. Minification drops the padding, since the shorter spelling names the
   same range, so pretty printing is the only instrument here. The first two
   cases pad one half ON ITS OWN, which a reader asking whether BOTH are padded
   answers exactly as it answers the pair that pads neither. *)
let fidelity_unicode_range_padding_preserved () =
  let sheet range =
    String.concat ""
      [
        "@font-face { font-family: X; src: url(a); unicode-range: "; range; " }";
      ]
  in
  List.iter
    (fun range -> pretty_preserves (sheet range) [ range ])
    [ "U+0000-7F"; "U+0-007F"; "U+0000-007F"; "U+0-7F"; "U+0026" ]

let fidelity_hex_form_preserved () =
  pretty_preserves ".x { color: #ff0000 }" [ "#ff0000" ];
  pretty_preserves ".x { color: #f00 }" [ "#f00" ];
  pretty_preserves ".x { color: #FF0000 }" [ "#FF0000" ];
  pretty_preserves ".x { color: #ABCDEF }" [ "#ABCDEF" ];
  assert_minify_and_optimize ".x { color: #ff0000 }" ~minified:".x{color:#f00}"
    ~optimized:".x{color:red}";
  (* Optimisation is the node-changing pass, and CSS Color 4 sec. 5.2 makes the
     digit case carry no meaning, so the authored spelling does not survive it:
     pretty output taken after [Css.optimize] carries the canonical colour, the
     same way [#ff0000] already arrives there as [red]. *)
  Alcotest.(check string)
    "optimize drops the authored hex spelling" ".x {\n  color: #abcdef;\n}"
    (Css.stylesheet ".x { color: #ABCDEF }"
    |> Css.optimize |> Css.to_string |> String.trim)

(* CSS Color 4 section 5.1 + cascade convention: under non-minified output, the
   named-color and rgb() forms are preserved as written - no cross-form
   canonicalization. *)
let fidelity_color_form_preserved () =
  (* Pretty preserves the colour value and the named/hex spelling, but a colour
     function decodes to one node (no legacy flag), so its syntax canonicalizes
     to modern in pretty too - commas -> spaces, rgba/hsla -> rgb/hsl - exactly
     as a hex colour case-folds (#FF0000 -> #ffffff). *)
  pretty_preserves ".x { color: red }" [ "red" ];
  pretty_preserves ".x { color: rgb(255, 0, 0) }" [ "rgb(255 0 0)" ];
  pretty_preserves ".x { color: hsl(0, 100%, 50%) }" [ "hsl(0 100% 50%)" ];
  pretty_preserves ".x { color: transparent }" [ "transparent" ];
  pretty_preserves ".x { color: rgba(0, 0, 0, 0) }" [ "rgb(0 0 0 / 0)" ];
  pretty_preserves ".x { color: #0000 }" [ "#0000" ]

(* CSS Animations 1 section 3 + cascade convention: [from] / [to] are
   canonicalized to [0%] / [100%] only under minify; the pretty printer keeps
   the source keyword. *)
let fidelity_keyframe_selector_preserved () =
  pretty_preserves "@keyframes fade { from { opacity: 0 } to { opacity: 1 } }"
    [ "from"; "to" ];
  pretty_preserves "@keyframes fade { 0% { opacity: 0 } 100% { opacity: 1 } }"
    [ "0%"; "100%" ]

(* CSS Selectors 4 section 5.2 + cascade convention: stripping [*] in a
   non-solitary compound is a minify-only optimization; the pretty printer keeps
   the universal selector as written. *)
let fidelity_universal_in_compound_preserved () =
  pretty_preserves "*.foo { color: red }" [ "*.foo" ];
  pretty_preserves "*#main { color: red }" [ "*#main" ];
  pretty_preserves "*[data-x] { color: red }" [ "*[data-x]" ]

(* CSS Color 4 section 4.2 + cascade convention: alpha unit form is preserved in
   pretty output, while numeric spelling follows the shared shortest decimal
   convention. *)
let fidelity_alpha_form_preserved () =
  pretty_preserves ".x { color: rgba(255, 0, 0, 0.5) }" [ ".5" ];
  pretty_preserves ".x { color: rgb(255 0 0 / 50%) }" [ "50%" ];
  pretty_preserves ".x { color: hsl(180 50% 25% / 30%) }" [ "30%" ]

let fidelity_percentage_precision_preserved () =
  (* [w-1/3] and friends author repeating-fraction percentages; the printer
     keeps every digit (Tailwind does) rather than rounding to six significant
     figures, in pretty and minified output alike. *)
  pretty_preserves ".x { width: 33.333333% }" [ "33.333333%" ];
  Alcotest.(check bool)
    "minify keeps the full percentage" true
    (match Css.of_string ~strict:false ".x { width: 66.666667% }" with
    | Ok parsed ->
        Astring.String.is_infix ~affix:"66.666667%"
          (Css.to_string ~minify:true parsed.stylesheet)
    | Error _ -> false)

(* CSS Values 4 section 6 + cascade convention: dropping the unit on a zero
   length is a minify-only optimization; the pretty printer keeps the source
   spelling. *)
let fidelity_zero_length_preserved () =
  pretty_preserves ".x { width: 0px }" [ "0px" ];
  pretty_preserves ".x { width: 0em }" [ "0em" ];
  pretty_preserves ".x { width: 0% }" [ "0%" ];
  pretty_preserves ".x { margin: 0px 0px 0px 0px }" [ "0px 0px 0px 0px" ]

(* CSS Animations 1 section 3 + cascade convention: shorthand collapses
   ([margin: 1px 1px 1px 1px] -> [margin: 1px], [border-radius: 0 0 0 0] -> [0])
   are minify-only optimizations; the pretty printer keeps the source
   spelling. *)
let fidelity_shorthand_form_preserved () =
  pretty_preserves ".x { margin: 1px 1px 1px 1px }" [ "1px 1px 1px 1px" ];
  pretty_preserves ".x { padding: 1px 2px 1px 2px }" [ "1px 2px 1px 2px" ];
  pretty_preserves ".x { border-radius: 0 0 0 0 }" [ "0 0 0 0" ];
  pretty_preserves ".x { background-position: 50% 50% }" [ "50% 50%" ]

(* CSS Fonts 4 section 2.2 + cascade convention: mapping the [normal] / [bold]
   keywords to the numeric weights [400] / [700] is a minify-only optimization;
   the pretty printer keeps the keyword. *)
let fidelity_font_weight_keyword_preserved () =
  pretty_preserves ".x { font-weight: normal }" [ "normal" ];
  pretty_preserves ".x { font-weight: bold }" [ "bold" ]

(* CSS Selectors Level 4, section 13.3.1 (The :nth-child() Pseudo-class): the
   [<an+b>] microsyntax has multiple spec-equivalent spellings - [2n+1] is the
   same set as [odd]; [2n+0] equals [2n] (matches even); a constant b like
   [(0n+5)] equals [(5)]; and [(1)] is equivalent to the [:first-child]
   pseudo-class. Both Lightning CSS and cssnano canonicalize these to the
   shortest spelling. *)
let s4_14_nth_child_canonicalization () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    ":nth-child(2n+1) canonicalizes to :nth-child(odd)"
    (normalize ".x :nth-child(odd) { color: red }")
    (normalize ".x :nth-child(2n+1) { color: red }");
  Alcotest.(check string)
    ":nth-child(even) canonicalizes to :nth-child(2n)"
    (normalize ".x :nth-child(2n) { color: red }")
    (normalize ".x :nth-child(even) { color: red }");
  Alcotest.(check string)
    ":nth-child(1) canonicalizes to :first-child"
    (normalize ".x :first-child { color: red }")
    (normalize ".x :nth-child(1) { color: red }")

(* CSS Selectors Level 4, section 6.1 (Attribute selectors): the value in
   [\[attr=value\]] may be an identifier or a string. When the value matches the
   [<ident-token>] grammar, the quotes are redundant and may be dropped; both
   single and double quotes are equivalent. Both minifiers strip redundant
   quotes. *)
let s462_attr_quote_canonical () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "[type=\"text\"] drops redundant quotes to [type=text]"
    (normalize ".x [type=text] { color: red }")
    (normalize ".x [type=\"text\"] { color: red }");
  Alcotest.(check string)
    "[data-x='hello'] drops redundant quotes to [data-x=hello]"
    (normalize ".x [data-x=hello] { color: red }")
    (normalize ".x [data-x='hello'] { color: red }")

(* CSS Values and Units Module Level 4, section 5.3 (Numbers): scientific
   notation [<number>] tokens like [1e3] and [1.5e2] are spec-equivalent to
   their decimal expansion. Both minifiers expand these to the decimal form when
   shorter. *)
let v481_scientific_notation () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "1e3px expands to 1000px" ".x{width:1000px}"
    (normalize ".x { width: 1e3px }");
  Alcotest.(check string)
    "1.5e2px expands to 150px" ".x{width:150px}"
    (normalize ".x { width: 1.5e2px }")

(* CSS Values and Units Module Level 4, section 5.3 (Numbers): negative zero is
   the same value as zero; [-0px] is the same length as [0]. Both minifiers
   normalise. *)
let v481_negative_zero () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "-0px canonicalizes to 0" ".x{width:0}"
    (normalize ".x { width: -0px }");
  Alcotest.(check string)
    "-0 canonicalizes to 0" ".x{width:0}"
    (normalize ".x { width: -0 }")

(* CSS Values and Units Module Level 4, section 4.5 (URLs): the [<url>] type
   accepts both quoted strings and unquoted token sequences when the URL does
   not contain whitespace, parentheses, or non-printable characters. Both
   minifiers drop the quotes when not needed. *)
let v4_7_url_quote_canonicalization () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "url('image.png') drops quotes" ".x{background:url(image.png)}"
    (normalize ".x { background: url('image.png') }");
  Alcotest.(check string)
    "url(\"image.png\") drops quotes" ".x{background:url(image.png)}"
    (normalize ".x { background: url(\"image.png\") }")

(* CSS Values and Units Module Level 4, section 10 (Mathematical Expressions):
   when all operands of [calc()] are dimensions in the same unit (or numbers),
   the expression simplifies to a single value. Nested [calc()] collapses to a
   single [calc()] (and to a bare value when constant). *)
let v410_calc_nested_constant () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(calc(1px + 2px)) simplifies to 3px" ".x{width:3px}"
    (normalize ".x { width: calc(calc(1px + 2px)) }");
  Alcotest.(check string)
    "calc(1px + 2px) simplifies to 3px" ".x{width:3px}"
    (normalize ".x { width: calc(1px + 2px) }")

(* CSS Cascading and Inheritance Module Level 5, section 6.1 (Cascade Sorting
   Order): consecutive rules with the same condition (same [@media], same
   [@layer]) may be merged into one block, since the cascade evaluates them
   identically. The merge is spec-allowed when no rule with conflicting
   conditions appears between them. *)
let c61_same_condition_merge () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "consecutive @media screen blocks merge into one"
    (normalize "@media screen { .a { color: red } .b { color: blue } }")
    (normalize
       "@media screen { .a { color: red } } @media screen { .b { color: blue } \
        }");
  Alcotest.(check string)
    "consecutive @layer base blocks merge into one"
    (normalize "@layer base { .a { color: red } .b { color: blue } }")
    (normalize
       "@layer base { .a { color: red } } @layer base { .b { color: blue } }")

(* CSS Cascade 6.1 (Cascade Sorting Order): [.a] and the intervening [.b] tie on
   specificity (0,1,0), but the merge folds the later [.a] up into the earlier
   slot and canonicalises the merged body. [.b] writes [color], which neither
   [.a] property conflicts with, so the move is unobservable and the two [.a]
   rules combine across it. A conflicting, same-specificity intervening rule
   would block it. *)
let c61_merge_across_nonconflicting () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let output =
    normalize ".a { padding: 10px } .b { color: red } .a { margin: 5px }"
  in
  Alcotest.(check string)
    "non-conflicting intervening rule does not block the merge"
    ".a{margin:5px;padding:10px}.b{color:red}" output

(* {2 More fidelity tests for the new edges}

   Each canonicalization above is a minify-only optimization; the pretty printer
   must keep the source spelling. *)

let fidelity_nth_child_form_preserved () =
  pretty_preserves ".x :nth-child(2n+1) { color: red }" [ "2n+1" ];
  pretty_preserves ".x :nth-child(even) { color: red }" [ "even" ];
  pretty_preserves ".x :nth-child(1) { color: red }" [ ":nth-child(1)" ]

let fidelity_attribute_quotes_preserved () =
  pretty_preserves ".x [type=\"text\"] { color: red }" [ "[type=\"text\"]" ];
  pretty_preserves ".x [data-x='hello'] { color: red }" [ "[data-x='hello']" ]

let fidelity_scientific_notation_preserved () =
  pretty_preserves ".x { width: 1e3px }" [ "1e3px" ];
  pretty_preserves ".x { width: 1.5e2px }" [ "1.5e2px" ]

let fidelity_url_quotes_preserved () =
  pretty_preserves ".x { background: url('image.png') }" [ "url('image.png')" ];
  pretty_preserves ".x { background: url(\"image.png\") }"
    [ "url(\"image.png\")" ]

let fidelity_calc_form_preserved () =
  pretty_preserves ".x { width: calc(1px + 2px) }" [ "calc(1px + 2px)" ];
  pretty_preserves ".x { width: calc(calc(1px + 2px)) }"
    [ "calc(calc(1px + 2px))" ]

let suite =
  ( "stylesheet_minify",
    [
      ( "spec cascade 6.2 import preserved verbatim",
        `Quick,
        c6_2_import_preserved_verbatim );
      ( "spec CSS Syntax 4.3.2 source-map pragma is a comment",
        `Quick,
        s3432_sourcemap_comment );
      ( "spec cascade 6.4.4.2 layer statement equiv empty blocks",
        `Quick,
        c6442_empty_blocks_equiv );
      ( "spec cascade 6.4.3 dotted layer equiv nested layer",
        `Quick,
        c643_dotted_nested_layer );
      ( "spec CSS Syntax 4.3.2 printer never emits source-map",
        `Quick,
        s3432_no_sourcemap_print );
      ( "spec values 4 6.1 zero length equivalence",
        `Quick,
        v461_zero_length_equiv );
      ( "spec color 4 12.1 hex shorthand equivalence",
        `Quick,
        color4121_hex_equiv );
      ( "spec cascade 6.1 optimizer preserves winning value",
        `Quick,
        c61_keeps_winner );
      ("spec color 4 1.4 color form equivalence", `Quick, color414_form_equiv);
      ( "spec color 4 named/hex minification tie policy",
        `Quick,
        color4_hex_tie_policy );
      ( "spec values 4 6.5 zero percentage length equivalence",
        `Quick,
        v465_zero_percentage_equiv );
      ( "spec CSSOM 6.6.2 declaration serialization",
        `Quick,
        cssom662_decl_serialization );
      ( "spec color 4 6.4 transparent equivalence",
        `Quick,
        color4_6_4_transparent_equivalence );
      ( "spec color 4 6.1 named color case insensitive",
        `Quick,
        color461_named_case );
      ("spec values 4 8.1 number format equivalence", `Quick, v481_number_format);
      ( "spec selectors 4 3.5 universal in compound redundant",
        `Quick,
        s435_universal_redundant );
      ( "spec selectors 4 selector list canonical order is optimize only",
        `Quick,
        selector_list_canonical_order );
      ( "spec scope start/end selector lists canonicalize in optimize",
        `Quick,
        scope_selector_list_canonical );
      ( "spec color 4 3 hue modulo canonicalization",
        `Quick,
        color4_3_hue_modulo_canonicalization );
      ( "spec color 4 1.3 alpha number percentage equivalence",
        `Quick,
        color413_alpha_equiv );
      ( "spec animations 1 7.1 keyframe from/to equivalence",
        `Quick,
        anim171_keyframe_equiv );
      ( "spec color 4 12 rgb clamp canonicalization",
        `Quick,
        color4_12_rgb_clamp_canonicalization );
      ( "spec color 4 1.3 fully opaque alpha collapse",
        `Quick,
        color413_opaque_alpha_collapse );
      ( "spec values 4 8.1 number trailing zero canonicalization",
        `Quick,
        v481_trailing_zero );
      ( "spec fonts 4 5.1.2 font-weight keyword to number",
        `Quick,
        fonts4512_weight_number );
      ( "spec box 4 margin shorthand collapse",
        `Quick,
        box4_margin_shorthand_collapse );
      ( "spec color 4 6.4 transparent canonical shortest",
        `Quick,
        color464_transparent_shortest );
      ( "spec values 4 6.5 zero length canonical shortest",
        `Quick,
        v465_zero_length_shortest );
      ( "spec bg 3 5 border-radius collapse shortest",
        `Quick,
        bg35_radius_collapse );
      ( "spec values 4 10 calc add zero simplification",
        `Quick,
        v410_calc_add_zero );
      ( "spec bg 3 3.6 background-position collapse shortest",
        `Quick,
        bg336_bgpos_collapse );
      (* Non-minified fidelity: pretty printer preserves the source spelling. *)
      ("pretty at-rule block bodies", `Quick, pretty_at_rule_block_bodies);
      ("fidelity hex form preserved", `Quick, fidelity_hex_form_preserved);
      ( "fidelity unicode-range padding preserved",
        `Quick,
        fidelity_unicode_range_padding_preserved );
      ("fidelity color form preserved", `Quick, fidelity_color_form_preserved);
      ( "fidelity keyframe selector preserved",
        `Quick,
        fidelity_keyframe_selector_preserved );
      ( "fidelity universal in compound preserved",
        `Quick,
        fidelity_universal_in_compound_preserved );
      ("fidelity alpha form preserved", `Quick, fidelity_alpha_form_preserved);
      ( "fidelity percentage precision preserved",
        `Quick,
        fidelity_percentage_precision_preserved );
      ("fidelity zero length preserved", `Quick, fidelity_zero_length_preserved);
      ( "fidelity shorthand form preserved",
        `Quick,
        fidelity_shorthand_form_preserved );
      ( "fidelity font-weight keyword preserved",
        `Quick,
        fidelity_font_weight_keyword_preserved );
      ( "spec selectors 4 14 nth-child canonicalization",
        `Quick,
        s4_14_nth_child_canonicalization );
      ( "spec selectors 4 6.2 attribute quote canonicalization",
        `Quick,
        s462_attr_quote_canonical );
      ( "spec values 4 8.1 scientific notation expansion",
        `Quick,
        v481_scientific_notation );
      ("spec values 4 8.1 negative zero canonical", `Quick, v481_negative_zero);
      ( "spec values 4 7 url quote canonicalization",
        `Quick,
        v4_7_url_quote_canonicalization );
      ( "spec values 4 10 calc nested constant simplification",
        `Quick,
        v410_calc_nested_constant );
      ( "spec cascade 6.1 consecutive same-condition merge",
        `Quick,
        c61_same_condition_merge );
      ( "spec cascade 6.1 merge across non-conflicting intervening rule pair",
        `Quick,
        c61_merge_across_nonconflicting );
      ( "fidelity nth-child form preserved",
        `Quick,
        fidelity_nth_child_form_preserved );
      ( "fidelity attribute quotes preserved",
        `Quick,
        fidelity_attribute_quotes_preserved );
      ( "fidelity scientific notation preserved",
        `Quick,
        fidelity_scientific_notation_preserved );
      ("fidelity url quotes preserved", `Quick, fidelity_url_quotes_preserved);
      ("fidelity calc form preserved", `Quick, fidelity_calc_form_preserved);
    ] )
