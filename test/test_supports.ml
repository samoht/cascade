open Cascade
open Css.Supports
module Supports_inventory = Cascade_spec_inventory.Supports_grammar

let rec supports_of_expected = function
  | Supports_inventory.Property (name, value) -> property name value
  | Supports_inventory.Func (name, value) -> func name value
  | Supports_inventory.Not condition -> Not (supports_of_expected condition)
  | Supports_inventory.And (left, right) ->
      And (supports_of_expected left, supports_of_expected right)
  | Supports_inventory.Or (left, right) ->
      Or (supports_of_expected left, supports_of_expected right)

let test_string_output () =
  let cases =
    [
      (property "display" "grid", "(display: grid)");
      (Not (property "display" "grid"), "not (display: grid)");
      ( Or (property "display" "grid", property "gap" "1rem"),
        "(display: grid) or (gap: 1rem)" );
      ( And (property "display" "grid", property "gap" "1rem"),
        "(display: grid) and (gap: 1rem)" );
      ( Or
          ( Not (property "-webkit-appearance" "-apple-pay-button"),
            property "contain-intrinsic-size" "1px" ),
        "(not (-webkit-appearance: -apple-pay-button)) or \
         (contain-intrinsic-size: 1px)" );
    ]
  in
  List.iter
    (fun (supports, expected) ->
      Alcotest.(check string) expected expected (to_string supports))
    cases

let test_of_string () =
  let check name input expected =
    let actual = of_string input in
    Alcotest.(check string) name (to_string expected) (to_string actual)
  in
  check "simple property" "(display: grid)" (property "display" "grid");
  check "property no space" "(display:grid)" (property "display" "grid");
  check "not condition" "(not (display: grid))"
    (Not (property "display" "grid"));
  check "or condition" "(display: grid) or (gap: 1rem)"
    (Or (property "display" "grid", property "gap" "1rem"));
  check "and condition" "(display: grid) and (gap: 1rem)"
    (And (property "display" "grid", property "gap" "1rem"));
  check "complex: not or property"
    "(not (-webkit-appearance: -apple-pay-button)) or (contain-intrinsic-size: \
     1px)"
    (Or
       ( Not (property "-webkit-appearance" "-apple-pay-button"),
         property "contain-intrinsic-size" "1px" ));
  check "nested function value" "(color: color-mix(in lab, red, red))"
    (property "color" "color-mix(in lab, red, red)");
  check "double parens around property" "((-webkit-hyphens: none))"
    (property "-webkit-hyphens" "none");
  check "complex browser detection"
    "((-webkit-hyphens: none) and (not (margin-trim: inline))) or \
     ((-moz-orient: inline) and (not (color: rgb(from red r g b))))"
    (Or
       ( And
           ( property "-webkit-hyphens" "none",
             Not (property "margin-trim" "inline") ),
         And
           ( property "-moz-orient" "inline",
             Not (property "color" "rgb(from red r g b)") ) ))

let test_current_work_vectors () =
  let check name input expected =
    let actual = of_string input in
    Alcotest.(check string) name expected (to_string actual)
  in
  check "supports selector has" "selector(:has(img))" "selector(:has(img))";
  check "supports font tech" "(font-tech(color-COLRv1))"
    "font-tech(color-COLRv1)";
  check "supports complex selector and property"
    "selector(:has(img)) and (container-type: inline-size)"
    "selector(:has(img)) and (container-type: inline-size)"

let spec_supports_feature_vectors () =
  let check name input expected =
    let actual = of_string input in
    Alcotest.(check string) name expected (to_string actual)
  in
  (* CSS Conditional Rules 3 section 6 includes parenthesized general-enclosed
     alongside declaration and nested-condition features. *)
  List.iter
    (fun input -> check "parenthesized general-enclosed" input input)
    [
      "not (future syntax)";
      "()";
      "(display)";
      "(: grid)";
      "(display: grid;)";
      "((display: grid) and)";
      "(future syntax) or (display: grid)";
    ];
  (* Conditional Rules general-enclosed preserves functions whose arguments do
     not match a supported feature grammar, including under negation. *)
  check "unknown font format under not" "not font-format(cascade-nope)"
    "not font-format(cascade-nope)";
  check "unknown font tech under not" "not font-tech(cascade-nope)"
    "not font-tech(cascade-nope)";
  check "selector forgiving relative branch" "selector(:has(+ img))"
    "selector(:has(+ img))";
  check "selector current pseudo" "selector(:popover-open)"
    "selector(:popover-open)";
  check "font format feature" "font-format(woff2)" "font-format(woff2)";
  check "font tech feature" "font-tech(variations)" "font-tech(variations)";
  check "at-rule feature" "at-rule(@container)" "at-rule(@container)";
  check "named feature function" "named-feature(--compact)"
    "named-feature(--compact)";
  check "general enclosed function" "unknown-feature(foo bar)"
    "unknown-feature(foo bar)";
  check "unknown declaration feature" "(-vendor-flag: enabled)"
    "(-vendor-flag: enabled)";
  check "empty declaration value" "(display:)" "(display:)";
  check "nested not selector" "not selector(:has(article > img))"
    "not selector(:has(article > img))";
  check "and chain" "(display: grid) and (gap: 1rem) and (selector(:has(img)))"
    "(display: grid) and (gap: 1rem) and selector(:has(img))";
  check "or chain"
    "(font-format(woff2)) or (font-tech(color-COLRv1)) or (display: grid)"
    "font-format(woff2) or font-tech(color-COLRv1) or (display: grid)";
  check "grouped and inside or"
    "((display: grid) and (gap: 1rem)) or selector(:has(img))"
    "((display: grid) and (gap: 1rem)) or selector(:has(img))"

let spec_supports_negative_vectors () =
  List.iter
    (fun input ->
      Alcotest.(check string) input input (to_string (of_string input)))
    Supports_inventory.general_enclosed_functions;
  let expect_error name input =
    try
      ignore (of_string input);
      Alcotest.failf "%s: expected invalid @supports condition" name
    with Cursor.Parse_error _ -> ()
  in
  List.iteri
    (fun i input ->
      expect_error ("invalid shared vector " ^ string_of_int i) input)
    Cascade_spec_inventory.Supports_grammar.invalid

let spec_supports_structural_vectors () =
  let unknown = of_string "(future syntax)" in
  Alcotest.(check bool)
    "unknown enclosed atom differs from a function" false
    (equal unknown (of_string "future(syntax)"));
  Alcotest.(check bool)
    "unknown feature is false in an empty explicit context" true
    (Css.Context.matches_supports (Css.Context.query ()) (Not unknown));
  Alcotest.(check bool)
    "explicit future capability is respected" true
    (Css.Context.matches_supports
       (Css.Context.query ~supports:[ unknown ] ())
       unknown);
  (match simplify_under ~context:[] unknown with
  | `Cond retained ->
      Alcotest.(check bool)
        "open world retains unknown guard" true (equal unknown retained)
  | `True | `False -> Alcotest.fail "unknown guard folded without context");
  List.iter
    (fun (row : Supports_inventory.row) ->
      let actual = of_string row.input in
      Alcotest.(check bool)
        row.name true
        (equal (supports_of_expected row.expected) actual))
    Supports_inventory.rows

let spec_supports_nested_edges () =
  let check name input expected =
    let actual = of_string input in
    Alcotest.(check string) name expected (to_string actual)
  in
  check "not wraps grouped or" "not ((display: grid) or (display: flex))"
    "not ((display: grid) or (display: flex))";
  check "and preserves grouped or branch"
    "((display: grid) or (display: flex)) and (gap: 1rem)"
    "((display: grid) or (display: flex)) and (gap: 1rem)";
  check "or preserves grouped and branch"
    "((container-type: inline-size) and selector(:has(img))) or (display: grid)"
    "((container-type: inline-size) and selector(:has(img))) or (display: grid)";
  check "declaration value with nested function"
    "(background-image: image-set(url(a.png) 1x, url(a@2x.png) 2x))"
    "(background-image: image-set(url(a.png) 1x, url(a@2x.png) 2x))";
  check "supports unknown general-enclosed block"
    "selector(:has(:is(img, picture)))" "selector(:has(:is(img, picture)))";
  check "font tech and format mixed"
    "font-format(woff2) and font-tech(color-COLRv1)"
    "font-format(woff2) and font-tech(color-COLRv1)";
  check "custom property registration feature"
    "(--theme-color: color(display-p3 1 0 0))"
    "(--theme-color: color(display-p3 1 0 0))"

let spec_supports_context_vectors () =
  List.iter
    (fun input ->
      Alcotest.(check string)
        "supports condition syntax" input
        (to_string (of_string input)))
    [
      "(display: grid)";
      "selector(:has(img))";
      "font-tech(variations)";
      "(display: grid) and selector(:has(img))";
      "not ((display: grid) or (gap: 1rem))";
    ]

let test_roundtrip () =
  let cases =
    [
      property "display" "grid";
      Not (property "display" "grid");
      Or (property "display" "grid", property "gap" "1rem");
      And (property "display" "grid", property "gap" "1rem");
      Or
        ( Not (property "-webkit-appearance" "-apple-pay-button"),
          property "contain-intrinsic-size" "1px" );
    ]
  in
  List.iter
    (fun cond ->
      let s = to_string cond in
      let parsed = of_string s in
      Alcotest.(check string) ("roundtrip: " ^ s) s (to_string parsed))
    cases

(* [property] takes authored CSS text, so a pair it accepts has to write back as
   the one feature test it names. CSS Conditional 3 sec. 6 gives
   [<supports-decl>] the grammar [( <declaration> )], and CSS Syntax 3 sec. 8.2
   keeps out of a [<declaration-value>] any unmatched closing bracket, any
   top-level [;], and any [<bad-string-token>] or [<bad-url-token>]. Written
   between the feature's own parentheses such a value closes them, and the tail
   becomes a second branch of the condition: Chrome answers
   [CSS.supports("color", "red) or (color:blue")] false while the emitted
   [(color:red) or (color:blue)] is true, so the rules it guards apply. CSS
   Syntax 3 sec. 4.3.6 returns the [<url-token>] at end of input, so [url(foo]
   is the value [url(foo)] is. *)
let property_guard () =
  (* Print the condition into a stylesheet and read the stylesheet back. The
     answer comes from the round-trip, not from a pinned spelling. *)
  let body =
    [ Css.rule ~selector:(Css.Selector.class_ "a") [ Css.color (Named Red) ] ]
  in
  let build name value =
    match property name value with
    | exception Failure _ -> "<refused>"
    | cond -> (
        let text =
          Css.to_string ~minify:true
            (Css.v [ Css.Stylesheet.Supports (cond, body) ])
        in
        match Css.of_string ~strict:false text with
        | Error _ -> String.concat "" [ text; " reads back as <unparsable>" ]
        | Ok { Css.stylesheet = [ Css.Stylesheet.Supports (back, [ _ ]) ]; _ }
          when equal cond back ->
            "<round-trips>"
        | Ok { Css.stylesheet; _ } ->
            String.concat ""
              [
                text;
                " reads back as ";
                Css.to_string ~minify:true (Css.v stylesheet);
              ])
  in
  List.iter
    (fun (value, expected) ->
      Alcotest.(check string)
        (String.concat "" [ "color:"; value ])
        expected (build "color" value))
    [
      (* A [<declaration-value>]. *)
      ("red", "<round-trips>");
      ("rgb(1,2,3)", "<round-trips>");
      ("\"a;b\"", "<round-trips>");
      ("url(foo", "<round-trips>");
      (* CSS Syntax 3 (ED) sec. 4.3.5 ends a string at EOF as the string it
         read, so a value the input ended inside one carries a string like any
         other. Only a newline makes a [<bad-string-token>]. *)
      ("\"abc", "<round-trips>");
      (* Not one: an unmatched closing bracket, a top-level [;], an unterminated
         function or block, a [<bad-url-token>]. *)
      ("red) or (color:blue", "<refused>");
      ("red)", "<refused>");
      ("red]", "<refused>");
      ("red}", "<refused>");
      ("red;--b:blue", "<refused>");
      ("rgb(1,2,3", "<refused>");
      ("{a:b", "<refused>");
      ("url(foo bar)", "<refused>");
    ];
  (* The same holds for a property whose value cascade does not model, which is
     the arm that keeps the caller's text. *)
  List.iter
    (fun (value, expected) ->
      Alcotest.(check string)
        (String.concat "" [ "wibble-prop:"; value ])
        expected
        (build "wibble-prop" value))
    [ ("1", "<round-trips>"); ("1) or (color:blue", "<refused>") ]

(* The pipeline [cascade --minify] runs: the optimiser over a parsed sheet, then
   the minified printer. *)
let minified ?(enforce_spec = false) css =
  Css.to_string ~minify:true ~enforce_spec
    (Css.optimize ~enforce_spec (Css.of_string_exn css))

(* Every input below is already in the minified spelling, so the sheet the
   author wrote is the whole expectation. Rows are collected rather than
   asserted one at a time, so a run names every condition that moved. *)
let check_preserved name sheets =
  let moved =
    List.concat_map
      (fun css ->
        let one label actual =
          if String.equal actual css then []
          else [ String.concat "" [ label; actual ] ]
        in
        one "--minify -> " (minified css)
        @ one "--minify --enforce-spec -> " (minified ~enforce_spec:true css))
      sheets
  in
  Alcotest.(check (list string)) name [] moved

(* Everywhere else in a stylesheet two spellings the spec calls equal are
   interchangeable, and [--minify] takes the shorter one. A feature query is the
   exception. CSS Conditional 3 sec. 6 gives the declaration feature the grammar
   [( <declaration> )] and the UA answers it by handing that exact declaration
   to its own parser, so the value there is the question, not a value. The
   construct exists to find the parsers that deviate from the spec, which is why
   spec-equivalence says nothing about whether two spellings come back with the
   same answer: the author's spelling is what has to survive. *)
let condition_value_preserved () =
  check_preserved "the author's condition survives --minify"
    [
      (* Legacy and modern colour syntax are two grammars shipped years apart,
         so a guard written in one does not ask what the other asks. *)
      "@supports(color:rgba(0,0,0,.5)){.b{color:red}}";
      "@supports(color:rgb(0,0,0)){.b{color:red}}";
      "@supports(color:hsl(0,0%,0%)){.b{color:red}}";
      (* Eight-digit hex is its own syntax, and dropping an opaque alpha channel
         probes the six-digit one instead. *)
      "@supports(color:#ff0000ff){.b{color:red}}";
      (* The same rule with no divergence to name: a [calc()] wrapper, a
         trailing zero and a quoted [url()] argument are the author's tokens
         either way. *)
      "@supports(width:calc(10px)){.b{color:red}}";
      "@supports(width:10.0px){.b{color:red}}";
      "@supports(background:url(\"a.png\")){.b{color:red}}";
      (* The [supports()] clause of an [@import] is the same declaration feature
         in prefix position, and it decides whether the sheet is fetched at
         all. *)
      "@import\"t.css\"supports(color:rgba(0,0,0,.5));.a{opacity:1}";
      (* Two guards asking different questions must not be rewritten into one:
         the UA that answers them differently loses the distinction. *)
      "@supports(width:calc(10px)){.b{color:red}}@supports(width:10.0px){.c{color:red}}";
    ]

(* Controls for the rows above: conditions the default minify already hands back
   unchanged. A [calc()] carrying arithmetic, a condition already in the modern
   colour syntax, a zero length with its unit, a [scale()] with two equal
   arguments, and a property cascade does not model at all, which keeps its
   value byte for byte and is what the modelled rows are measured against. *)
let condition_value_controls () =
  check_preserved "conditions the default minify already keeps"
    [
      "@supports(width:calc(10px*2)){.b{color:red}}";
      "@supports(color:rgb(0 0 0)){.b{color:red}}";
      "@supports(margin:0px){.b{color:red}}";
      "@supports(transform:scale(1,1)){.b{color:red}}";
      "@supports(nonsense-prop:10.0px){.b{color:red}}";
      "@supports(nonsense-prop:rgba(0,0,0,.5)){.b{color:red}}";
    ]

let suite =
  let open Alcotest in
  ( "supports",
    [
      test_case "to_string" `Quick test_string_output;
      test_case "of_string" `Quick test_of_string;
      test_case "current-work vectors" `Quick test_current_work_vectors;
      test_case "spec conditional supports feature vectors" `Quick
        spec_supports_feature_vectors;
      test_case "spec conditional supports negative vectors" `Quick
        spec_supports_negative_vectors;
      test_case "spec conditional supports structural vectors" `Quick
        spec_supports_structural_vectors;
      test_case "spec conditional supports nested algorithm edges" `Quick
        spec_supports_nested_edges;
      test_case "spec conditional supports context syntax vectors" `Quick
        spec_supports_context_vectors;
      test_case "roundtrip" `Quick test_roundtrip;
      test_case "property guard" `Quick property_guard;
      test_case "condition value preserved" `Quick condition_value_preserved;
      test_case "condition value controls" `Quick condition_value_controls;
    ] )
