(** Stylesheet tests: statement walkers, rendering, equality and hashing. *)

open Cascade
module Selector = Css.Selector
open Css.Stylesheet
open Css_test_helpers

(* Every shape a statement can take, so the walkers are exercised on the block
   at-rules and on the at-rules that hold declarations outside a block. *)
let every_statement_shape =
  "@charset \"utf-8\";\n\
   @import url(a.css);\n\
   @namespace svg url(http://www.w3.org/2000/svg);\n\
   @property --p { syntax: \"<color>\"; inherits: false; initial-value: red }\n\
   @layer base, theme;\n\
   @layer base { .a { color: red; & .b { color: blue } } }\n\
   @media print { .c { color: red } }\n\
   @container card (width > 10px) { .d { color: red } }\n\
   @supports (display: grid) { .e { color: red } }\n\
   @-moz-document url-prefix(\"http://x\") { .f { color: red } }\n\
   @starting-style { .g { color: red } }\n\
   @scope (.h) to (.i) { .j { color: red } }\n\
   @keyframes k { from { color: red } to { color: blue } }\n\
   @-webkit-keyframes wk { from { color: red } }\n\
   @font-face { font-family: F; src: url(f.woff2) }\n\
   @counter-style cs { system: cyclic; symbols: a }\n\
   @page { margin: 1cm }\n\
   @page :first { margin: 1cm; @top-left { content: \"x\" } }\n\
   @font-palette-values --fp { font-family: F }\n\
   @view-transition { navigation: auto }\n\
   @position-try --pt { top: 1px }\n\
   @viewport { width: device-width }\n\
   .k { color: red }\n"

let parse_shapes () =
  match Css.of_string every_statement_shape with
  | Ok { Css.stylesheet; _ } -> stylesheet
  | Error err ->
      Alcotest.failf "shape corpus did not parse: %s"
        (Cascade.Error.to_string err)

(* One declaration per place a statement can hold one, each under a property
   name that names only that place, so a walk that misses a place is a missing
   name rather than a smaller count. *)
let every_declaration_place =
  ".el { color: red }\n\
   @media print { .m { display: none } }\n\
   @layer l { .l { float: left } }\n\
   @container (width > 1px) { .c { clear: both } }\n\
   @supports (display: grid) { .s { z-index: 1 } }\n\
   @-moz-document url-prefix(\"x\") { .d { direction: rtl } }\n\
   @starting-style { .ss { opacity: 0 } }\n\
   @scope (.a) to (.b) { .sc { visibility: hidden } }\n\
   @when media(print) { .w { order: 1 } }\n\
   @else { .e { order: 2 } }\n\
   .parent { & .nested { overflow: hidden } }\n\
   @keyframes k { from { rotate: 0deg } }\n\
   @page { size: a4 }\n\
   @page :first { @top-left { content: \"tl\" } }\n\
   @position-try --pt { inset: 1px }\n\
   @supports-condition --sc { padding: 1px }\n"

let parsed source =
  match Css.of_string source with
  | Ok { Css.stylesheet; _ } -> stylesheet
  | Error err ->
      Alcotest.failf "declaration-place corpus did not parse: %s"
        (Cascade.Error.to_string err)

let places () =
  (* [Origin] has no CSS syntax, so it can only be built. *)
  parsed every_declaration_place
  @ [ with_origin Author (parsed ".o { top: 1px }") ]

let properties_folded ?sites () =
  List.sort compare
    (fold_declarations ?sites
       (fun acc decls ->
         List.rev_append (List.map Css.Declaration.property_name decls) acc)
       [] (places ()))

let element_places =
  [
    "clear";
    "color";
    "direction";
    "display";
    "float";
    "opacity";
    "order";
    "order";
    "overflow";
    "top";
    "visibility";
    "z-index";
  ]

let other_places = [ "content"; "inset"; "padding"; "rotate"; "size" ]

(* --- rendering --- *)

(* [to_string] and the bare formatter run the same printer, so they agree byte
   for byte; the only difference either may have is how the output bytes are
   collected. *)
let one_pass ~minify sheet =
  let pp ctx () = pp_stylesheet ctx sheet in
  Css.Pp.to_string ~minify pp ()

let render_sheet n =
  let b = Buffer.create (n * 96) in
  let out = Fmt.with_buffer b in
  for i = 0 to n - 1 do
    Fmt.pf out
      ".c%d .d%d > span:hover{color:rgb(%d,%d,%d);margin:%dpx \
       %dpx;padding:%dpx;display:flex}@media (min-width:%dpx){.m%d{outline:1px \
       solid #abc}}"
      i i (i mod 256)
      (i * 7 mod 256)
      (i * 13 mod 256)
      (i mod 40)
      (i * 3 mod 40)
      (i mod 20)
      (300 + (i mod 900))
      i
  done;
  Fmt.flush out ();
  Css.of_string_exn (Buffer.contents b)

(* A serialiser walks the tree once. Presizing the buffer from a [Pp.size]
   prepass walks it a second time, and the counter sink skips only the output
   bytes: [normalise], [printable_statements] and every printer below them run
   twice. Buffer growth is amortised, so the second walk buys nothing, and its
   cost is the whole formatter rather than a rounding error - pin it well below
   the 1.8x the prepass measured. *)
let to_string_renders_once minify () =
  let sheet = render_sheet 400 in
  let a_one = measure (fun () -> one_pass ~minify sheet) in
  let a_full = measure (fun () -> to_string ~minify sheet) in
  Alcotest.(check bool)
    (Fmt.str "alloc %.0f -> %.0f (%.2fx the one-pass walk)" a_one a_full
       (a_full /. a_one))
    true
    (a_one = 0. || a_full < a_one *. 1.25)

let to_string_matches_one_pass minify () =
  let sheet = render_sheet 400 in
  Alcotest.(check string)
    "same bytes" (one_pass ~minify sheet) (to_string ~minify sheet)

(* {2 Statement equality and fingerprint} *)

(* One statement per shape, spelled in the CSS that produces it. Every shape is
   parsed rather than assembled so the test reads what a stylesheet holds, not
   what a constructor was handed. *)
let parsed_shapes =
  [
    ("rule", ".a{color:red}");
    ("bang comment", "/*! keep */");
    ("charset", "@charset \"UTF-8\";");
    ("import", "@import url(a.css) layer(x) supports(display:grid) screen;");
    ("namespace", "@namespace svg url(http://www.w3.org/2000/svg);");
    ( "property",
      "@property --x{syntax:\"<length>\";inherits:false;initial-value:0px}" );
    ("layer statement", "@layer a,b;");
    ("layer block", "@layer a{.x{color:red}}");
    ("media", "@media screen{.x{color:red}}");
    ("container", "@container card (min-width:10px){.x{color:red}}");
    ("supports", "@supports (display:grid){.x{color:red}}");
    ("moz-document", "@-moz-document url-prefix(){.x{color:red}}");
    ("starting-style", "@starting-style{.x{color:red}}");
    ("when", "@when media(width>0px){.x{color:red}}");
    ("supports-condition", "@supports-condition --n{color:red}");
    ("scope", "@scope (.a) to (.b){.x{color:red}}");
    ("keyframes", "@keyframes k{from{opacity:0}to{opacity:1}}");
    ("webkit keyframes", "@-webkit-keyframes k{from{opacity:0}}");
    ("moz keyframes", "@-moz-keyframes k{from{opacity:0}}");
    ("font-face", "@font-face{font-family:A;src:url(a.woff2)}");
    ("counter-style", "@counter-style c{system:cyclic;symbols:\"a\"}");
    ("page with margins", "@page{margin:1cm;@top-left{content:\"x\"}}");
    ( "font-palette-values",
      "@font-palette-values --p{font-family:A;override-colors:0 red}" );
    ("font-feature-values", "@font-feature-values A{@styleset{nice:1}}");
    ("view-transition", "@view-transition{navigation:auto}");
    ("position-try", "@position-try --p{top:0}");
    ("viewport", "@viewport{width:100px}");
    ("unknown at-rule", "@wibble foo{bar:1}");
  ]

let sole_statement css =
  match Css.statements (Css.of_string_exn css) with
  | [ s ] -> s
  | l -> Alcotest.failf "%s: expected one statement, got %d" css (List.length l)

(* The four shapes with no source text of their own. [Declarations] holds the
   bare declarations of a nested block, [Else] needs the [@when] it answers,
   [Origin] is an API-level wrapper the CSS grammar has no spelling for, and the
   parser folds every [@page] into [Page_with_margins]. *)
let bare_declarations () =
  match
    Css.statements (Css.of_string_exn ".a{color:red;@media screen{color:blue}}")
  with
  | [ Rule { nested = [ Media (_, [ (Declarations _ as d) ]) ]; _ } ] -> d
  | _ -> Alcotest.fail "expected bare declarations inside a nested @media"

let else_branch () =
  match
    Css.statements
      (Css.of_string_exn
         "@when media(width>0px){.x{color:red}}@else{.y{color:red}}")
  with
  | [ _; (Else _ as e) ] -> e
  | _ -> Alcotest.fail "expected an @else branch"

let statement_shapes () =
  List.map (fun (label, css) -> (label, sole_statement css)) parsed_shapes
  @ [
      ("bare declarations", bare_declarations ());
      ("else", else_branch ());
      ("origin", with_origin User [ sole_statement ".x{color:red}" ]);
      ("page", Page ([], [ ok (Css.Declaration.of_string "margin:1cm") ]));
    ]

(* Two independent builds of the list, so no answer below can come from physical
   equality of a shared node. *)
let shape_matrix f =
  let left = statement_shapes () and right = statement_shapes () in
  List.concat_map
    (fun (la, sa) -> List.filter_map (fun (lb, sb) -> f la sa lb sb) right)
    left

let statement_equality_separates_every_shape () =
  let wrong =
    shape_matrix (fun la sa lb sb ->
        let same_shape = String.equal la lb in
        if Bool.equal (Css.equal_statement sa sb) same_shape then None
        else Some (String.concat "" [ la; " vs "; lb ]))
  in
  Alcotest.(check (list string))
    "each shape equals its own copy and no other" [] wrong

(* A hash that disagreed with the equality it serves would put one statement in
   two buckets. The converse is not required: a collision is allowed. *)
let statement_hash_agrees_with_equality () =
  let wrong =
    shape_matrix (fun la sa lb sb ->
        if
          Css.equal_statement sa sb
          && Css.hash_statement sa <> Css.hash_statement sb
        then Some (String.concat "" [ la; " vs "; lb ])
        else None)
  in
  Alcotest.(check (list string)) "equal statements hash alike" [] wrong;
  (* A constant hash would satisfy the rule above and be useless. *)
  Alcotest.(check bool)
    "two shapes are not one bucket" false
    (Css.hash_statement (sole_statement ".a{color:red}")
    = Css.hash_statement (sole_statement "@media screen{.x{color:red}}"))

(* What a statement holds is read, not just its shape. *)
let statement_equality_reads_the_payload () =
  let differs label a b =
    if Css.equal_statement (sole_statement a) (sole_statement b) then Some label
    else None
  in
  let folded =
    List.filter_map
      (fun (label, a, b) -> differs label a b)
      [
        ("a selector", ".a{color:red}", ".b{color:red}");
        ("a declaration", ".a{color:red}", ".a{color:blue}");
        ( "a media query",
          "@media screen{.x{color:red}}",
          "@media print{.x{color:red}}" );
        ( "a media block",
          "@media screen{.x{color:red}}",
          "@media screen{.x{color:blue}}" );
        ( "a supports condition",
          "@supports (display:grid){.x{color:red}}",
          "@supports (display:flex){.x{color:red}}" );
        ( "a container name",
          "@container a (min-width:10px){.x{color:red}}",
          "@container b (min-width:10px){.x{color:red}}" );
        ( "a container condition",
          "@container a (min-width:10px){.x{color:red}}",
          "@container a (min-width:20px){.x{color:red}}" );
        ("a layer name", "@layer a{.x{color:red}}", "@layer b{.x{color:red}}");
        ( "a nested rule",
          ".a{color:red;&:hover{color:blue}}",
          ".a{color:red;&:focus{color:blue}}" );
        ( "an import condition",
          "@import url(a.css) supports(display:grid);",
          "@import url(a.css) supports(display:flex);" );
        ( "a @property name",
          "@property --x{syntax:\"*\";inherits:false}",
          "@property --y{syntax:\"*\";inherits:false}" );
        (* A registration whose syntax is not [*] has to declare an initial
           value, so the pair below holds one fixed and moves the syntax. *)
        ( "a @property syntax",
          "@property --x{syntax:\"<length>\";inherits:false;initial-value:0px}",
          "@property \
           --x{syntax:\"<length-percentage>\";inherits:false;initial-value:0px}"
        );
        ( "a @property initial value",
          "@property --x{syntax:\"<length>\";inherits:false;initial-value:0px}",
          "@property --x{syntax:\"<length>\";inherits:false;initial-value:1px}"
        );
        ( "a scope end selector",
          "@scope (.a) to (.b){.x{color:red}}",
          "@scope (.a) to (.c){.x{color:red}}" );
      ]
  in
  Alcotest.(check (list string)) "each pair stays two statements" [] folded

(* The pipeline [cascade --minify] runs: the optimiser over a parsed sheet, then
   the minified printer. *)
let optimized_statement css =
  match Css.statements (Css.optimize (Css.of_string_exn css)) with
  | [ s ] -> s
  | l -> Alcotest.failf "%s: expected one statement, got %d" css (List.length l)

let minified_statement s = Css.to_string ~minify:true (Css.v [ s ])

(* The rule [Declaration.hash] already holds, one level up: a statement that
   minifies to the text another statement minifies to has to hash the same, or a
   table keyed on the fingerprint files one CSS statement under two keys. As
   there, the rule is stated over the canonicalised sheet, and equality is the
   finer relation, true for every pair below as well. *)
let same_text_same_hash label a b =
  let text = minified_statement a in
  Alcotest.(check string)
    (label ^ ": one minified text")
    text (minified_statement b);
  Alcotest.(check int)
    (label ^ ": one hash") (Css.hash_statement a) (Css.hash_statement b);
  Alcotest.(check bool)
    (label ^ ": one statement")
    true (Css.equal_statement a b)

let statement_hash_follows_the_minified_text () =
  let pair label a b =
    same_text_same_hash label (optimized_statement a) (optimized_statement b)
  in
  (* Optional whitespace the printer drops. *)
  pair "declaration spacing" ".a{color : red}" ".a{color:red}";
  pair "selector spacing" ".a > .b{color:red}" ".a>.b{color:red}";
  pair "media spacing" "@media screen and (min-width: 10px){.a{color:red}}"
    "@media screen and (min-width:10px){.a{color:red}}";
  pair "layer spacing" "@layer a {.x{color:red}}" "@layer a{.x{color:red}}";
  pair "keyframe spacing" "@keyframes k{from{opacity: 0}}"
    "@keyframes k{from{opacity:0}}";
  (* A zero length keeps no unit under minify. *)
  pair "zero length" ".a{margin:0px}" ".a{margin:0}";
  (* CSS Color 4 sec. 6.1 gives [red] the sRGB bytes 255, 0, 0, so the three
     spellings below are one colour and canonicalise to one node. *)
  pair "a named colour" ".a{color:red}" ".a{color:#f00}";
  pair "the sRGB function" ".a{color:red}" ".a{color:rgb(255 0 0)}";
  (* An [\@supports] condition keeps the token stream the author wrote, and the
     optimiser canonicalises the whitespace in it, so this pair reaches one
     node. *)
  pair "supports condition spacing"
    "@supports(color:rgba(0, 0, 0, .5)){.b{color:red}}"
    "@supports(color:rgba(0,0,0,.5)){.b{color:red}}"

(* A fingerprint that stopped at the statement's shape would satisfy the rule
   above and file every rule of a kind under one key. *)
let statement_hash_reads_the_descriptors () =
  let differs label a b =
    if
      Css.hash_statement (sole_statement a)
      = Css.hash_statement (sole_statement b)
    then Some label
    else None
  in
  let colliding =
    List.filter_map
      (fun (label, a, b) -> differs label a b)
      [
        ( "@font-face descriptors",
          "@font-face{font-family:A;src:url(a.woff2)}",
          "@font-face{font-family:B;src:url(b.woff2)}" );
        ( "@counter-style descriptors",
          "@counter-style c{system:cyclic;symbols:\"a\"}",
          "@counter-style c{system:cyclic;symbols:\"b\"}" );
        ( "@view-transition descriptors",
          "@view-transition{navigation:auto}",
          "@view-transition{navigation:none}" );
        ( "a keyframe selector",
          "@keyframes k{from{opacity:0}}",
          "@keyframes k{to{opacity:0}}" );
        ( "a @property syntax",
          "@property --x{syntax:\"<length>\";inherits:false;initial-value:0px}",
          "@property \
           --x{syntax:\"<length-percentage>\";inherits:false;initial-value:0px}"
        );
        ( "a @-moz-document condition",
          "@-moz-document domain(\"a.test\"){.x{color:red}}",
          "@-moz-document domain(\"b.test\"){.x{color:red}}" );
      ]
  in
  Alcotest.(check (list string)) "each pair keys apart" [] colliding

(* CSS Conditional 3 sec. 6 answers a declaration feature by handing that exact
   declaration to the browser's own parser, so the value spelled in the
   condition is the question. [calc(10px)] and [10.0px] name grammars a browser
   can accept one of and refuse the other: two guards, not one, and the
   optimiser keeps both spellings. *)
let supports_spelling_stays_two_statements () =
  let calc = optimized_statement "@supports(width:calc(10px)){.b{color:red}}" in
  let plain = optimized_statement "@supports(width:10.0px){.b{color:red}}" in
  Alcotest.(check bool)
    "two texts" false
    (String.equal (minified_statement calc) (minified_statement plain));
  Alcotest.(check bool) "two statements" false (Css.equal_statement calc plain);
  (* The control: the same guard over the same block is one statement, so the
     verdict above is about the value and not about the parse. *)
  let calc' =
    optimized_statement "@supports(width:calc(10px)){.b{color:red}}"
  in
  Alcotest.(check bool)
    "the same guard is one statement" true
    (Css.equal_statement calc calc')

let suite =
  ( "stylesheet_statement",
    [
      ( "map_statement_children keeps an unchanged statement",
        `Quick,
        fun () ->
          (* A rewriting walk short-circuits on physical equality, so a map that
             reallocates a statement it did not change costs the caller the
             sharing its fixed point converges on. *)
          List.iter
            (fun stmt ->
              if not (map_statement_children Fun.id stmt == stmt) then
                Alcotest.failf "children map rebuilt %s"
                  (Css.to_string ~minify:true [ stmt ]))
            (parse_shapes ()) );
      ( "map_statement_declarations keeps an unchanged statement",
        `Quick,
        fun () ->
          List.iter
            (fun stmt ->
              if not (map_statement_declarations Fun.id stmt == stmt) then
                Alcotest.failf "declaration map rebuilt %s"
                  (Css.to_string ~minify:true [ stmt ]))
            (parse_shapes ()) );
      ( "fold_declarations reaches every place a declaration sits",
        `Quick,
        fun () ->
          Alcotest.(check (list string))
            "every property"
            (List.sort compare (element_places @ other_places))
            (properties_folded ()) );
      ( "fold_declarations keeps to the sites it is given",
        `Quick,
        fun () ->
          (* The narrow walk names the places it wants rather than the
             statements it expects to meet, so leaving one out is a stated
             choice. *)
          let sites =
            {
              element_rule = true;
              animation_frame = false;
              page_box = false;
              position_fallback = false;
              condition_test = false;
            }
          in
          Alcotest.(check (list string))
            "element declarations only"
            (List.sort compare element_places)
            (properties_folded ~sites ()) );
      ( "at_declaration_site makes the fold's choice outside the fold",
        `Quick,
        fun () ->
          (* A walk that carries something down the tree recurses itself, so it
             reads the sites rather than passing them; it must land on the same
             declarations the fold does. *)
          let sites =
            {
              element_rule = true;
              animation_frame = false;
              page_box = false;
              position_fallback = false;
              condition_test = false;
            }
          in
          let rec walk acc stmt =
            let acc =
              if at_declaration_site sites stmt then
                List.rev_append
                  (List.map Css.Declaration.property_name
                     (statement_declarations stmt))
                  acc
              else acc
            in
            List.fold_left walk acc (statement_children stmt)
          in
          Alcotest.(check (list string))
            "the sites the fold folds over"
            (properties_folded ~sites ())
            (List.sort compare (List.fold_left walk [] (places ()))) );
      ( "map_declarations rewrites every place a declaration sits",
        `Quick,
        fun () ->
          let emptied = map_declarations (fun _ -> []) (places ()) in
          Alcotest.(check (list string))
            "nothing left" []
            (fold_declarations
               (fun acc decls ->
                 List.rev_append
                   (List.map Css.Declaration.property_name decls)
                   acc)
               [] emptied) );
      ( "map_declarations keeps an unchanged tree",
        `Quick,
        fun () ->
          let block = places () in
          if not (map_declarations Fun.id block == block) then
            Alcotest.fail "declaration map rebuilt an unchanged stylesheet" );
      ( "iter_statements reaches a statement inside every at-rule",
        `Quick,
        fun () ->
          let seen = ref [] in
          iter_statements
            (fun stmt ->
              match stmt with
              | Rule r ->
                  seen := Selector.to_string ~minify:true r.selector :: !seen
              | _ -> ())
            (places ());
          Alcotest.(check (list string))
            "every rule"
            (List.sort compare
               [
                 ".el";
                 ".m";
                 ".l";
                 ".c";
                 ".s";
                 ".d";
                 ".ss";
                 ".sc";
                 ".w";
                 ".e";
                 ".parent";
                 "& .nested";
                 ".o";
               ])
            (List.sort compare !seen) );
      ( "edit_statements drops a statement inside every at-rule",
        `Quick,
        fun () ->
          let dropped =
            edit_statements (function Rule _ -> Drop | _ -> Keep) (places ())
          in
          Alcotest.(check (list string))
            "no rule left" []
            (fold_statements
               (fun acc stmt ->
                 match stmt with
                 | Rule r -> Selector.to_string ~minify:true r.selector :: acc
                 | _ -> acc)
               [] dropped) );
      ( "edit_statements walks into a replacement",
        `Quick,
        fun () ->
          (* The nested rule is reachable only through the rule that was
             replaced, so it is marked when the walk continues into the
             replacement rather than into the statement it replaced. *)
          let mark = function
            | Rule r -> Replace (Rule { r with merge_key = Some "k" })
            | _ -> Keep
          in
          Alcotest.(check (list string))
            "every rule marked" []
            (fold_statements
               (fun acc stmt ->
                 match stmt with
                 | Rule r when r.merge_key = None ->
                     Selector.to_string ~minify:true r.selector :: acc
                 | _ -> acc)
               []
               (edit_statements mark (places ()))) );
      ( "edit_statements keeps an unchanged tree",
        `Quick,
        fun () ->
          let block = places () in
          if not (edit_statements (fun _ -> Keep) block == block) then
            Alcotest.fail "statement edit rebuilt an unchanged stylesheet" );
      ( "to_string minified matches the bare formatter",
        `Quick,
        to_string_matches_one_pass true );
      ( "to_string pretty matches the bare formatter",
        `Quick,
        to_string_matches_one_pass false );
      ("to_string renders minified once", `Quick, to_string_renders_once true);
      ("to_string renders pretty once", `Quick, to_string_renders_once false);
      ( "equal_statement separates every shape",
        `Quick,
        statement_equality_separates_every_shape );
      ( "hash_statement agrees with equal_statement",
        `Quick,
        statement_hash_agrees_with_equality );
      ( "equal_statement reads the payload",
        `Quick,
        statement_equality_reads_the_payload );
      ( "hash_statement follows the minified text",
        `Quick,
        statement_hash_follows_the_minified_text );
      ( "hash_statement reads the descriptors",
        `Quick,
        statement_hash_reads_the_descriptors );
      ( "an @supports spelling stays two statements",
        `Quick,
        supports_spelling_stays_two_statements );
    ] )
