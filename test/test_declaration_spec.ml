(** Declaration tests: spec property vectors and the grammar each property site
    accepts. *)

open Alcotest
open Cascade
open Css.Declaration
open Css_test_helpers
open Declaration_helpers

(* CSS Syntax 3 (ED) sec. 5.5.6 gives a declaration a value of one or more
   component values, and CSS Variables 1 sec. 2 gives the empty one to a custom
   property alone. *)
let an_empty_value_is_no_declaration () =
  check_declaration ~expected:"--x:" "--x:";
  neg_cursor read_declaration "quotes:";
  neg_cursor read_declaration "quotes: ";
  neg_cursor read_declaration "position-try: ";
  neg_cursor read_declaration "color: "

(* CSS Anchor Positioning 1 sec. 3.2 allows [anchor()] in the inset properties
   and nowhere else, and sec. 5.1 allows [anchor-size()] in the properties
   [@position-try] accepts: the inset, margin, sizing and self-alignment ones,
   plus [position-anchor] and [position-area]. *)
let anchor_query_where_the_property_takes_one () =
  check_declaration ~expected:"left:anchor(right)" "left: anchor(right)";
  check_declaration ~expected:"width:anchor-size(width)"
    "width: anchor-size(width)";
  check_declaration ~expected:"margin-left:anchor-size(width)"
    "margin-left: anchor-size(width)";
  check_declaration ~expected:"width:calc(anchor-size(width) + 2px)"
    "width: calc(anchor-size(width) + 2px)";
  neg_cursor read_declaration "width: anchor(right)";
  neg_cursor read_declaration "padding-bottom: anchor-size(width)";
  neg_cursor read_declaration "translate: anchor-size(width)";
  neg_cursor read_declaration "scroll-margin: anchor-size(width)"

(* CSS Grid 2 sec. 7.4 spells [grid-template] as [none], a
   [<'grid-template-rows'> / <'grid-template-columns'>] pair, or a track list
   built from strings, and sec. 7.8 gives [grid] those forms plus the two
   [auto-flow] ones. A bare track list names one axis, which is a
   [grid-template-rows] value and no shorthand one. *)
let grid_shorthand_names_both_axes () =
  check_declaration ~expected:"grid-template:10px/20px"
    "grid-template: 10px / 20px";
  check_declaration ~expected:"grid-template:none" "grid-template: none";
  check_declaration ~expected:"grid-template:\"a\" 10px"
    "grid-template: \"a\" 10px";
  (* CSS Grid 2 sec. 7.4 spells the areas form as [ <line-names>? <string>
     <track-size>? <line-names>? ]+ [ / <explicit-track-list> ]?, and sec. 7.2
     leaves repeat() out of an explicit track list. *)
  check_declaration ~expected:"grid-template:[x]\"a\" 1fr[y]"
    "grid-template: [x] \"a\" 1fr [y]";
  neg_cursor read_declaration "grid-template: #next \"sidebar\"";
  neg_cursor read_declaration "grid-template: 45deg, \"-\"";
  neg_cursor read_declaration "grid-template: 50% \"text\" infinite";
  neg_cursor read_declaration "grid-template: \"a\" repeat(2, 1fr)";
  neg_cursor read_declaration "grid-template: \"a\" auto / repeat(2, 1fr)";
  neg_cursor read_declaration "grid-template: \"a\" / \"b\"";
  check_declaration ~expected:"grid:none/1fr" "grid: auto-flow / 1fr";
  neg_cursor read_declaration "grid-template: 10px";
  neg_cursor read_declaration "grid-template: auto";
  neg_cursor read_declaration "grid: 10px";
  neg_cursor read_declaration "grid: subgrid"

(* CSS Color 5 sec. 3 closes the [<color>] production, so a function name it
   does not hold is no colour and every browser drops the declaration. A vendor
   name is the exception the reader keeps: the prefix says the vocabulary is an
   engine's own, which no specification settles either way. *)
let color_takes_only_a_colour_function () =
  check_declaration ~expected:"color:-webkit-magic(1)" "color: -webkit-magic(1)";
  check_declaration ~expected:"color:color-mix(var(--a),var(--b))"
    "color: color-mix(var(--a), var(--b))";
  neg_cursor read_declaration "border-top-color: calc(2px - 3px)";
  neg_cursor read_declaration "border-left-color: brightness()";
  neg_cursor read_declaration "text-decoration-color: counters(section, \".\")";
  neg_cursor read_declaration "border-block-start-color: minmax(0, 1fr)"

(* CSS Lists 3 sec. 3.5 gives [list-style-image] one [<image>], where
   [background-image] takes a comma-separated list of them, so the comma is
   where the two vocabularies part. *)
let list_style_image_takes_one_image () =
  check_declaration ~expected:"list-style-image:linear-gradient(red,blue)"
    "list-style-image: linear-gradient(red, blue)";
  check_declaration ~expected:"list-style-image:image-set(url(a.png)1x)"
    "list-style-image: image-set(url(a.png) 1x)";
  neg_cursor read_declaration "list-style-image: url(a.png), url(b.png)"

(* [parse_custom_property] builds a declaration from a name and value that came
   from outside the parser, so it takes only a pair that writes back as the one
   declaration it claims to be: a [<dashed-ident>] name, written back with the
   escapes that read it (CSS Syntax 3 sec. 4.3.7), and the
   [<declaration-value>?] CSS Variables 1 sec. 2 gives a custom property. *)
let parse_custom_property_guard () =
  let bind name value =
    match parse_custom_property name value with
    | Some d -> to_string ~minify:true d
    | None -> "<rejected>"
  in
  List.iter
    (fun (value, expected) ->
      Alcotest.(check string) ("--x:" ^ value) expected (bind "--x" value))
    [
      (* One declaration value: a matched block, a [;] inside a string and a
         comment all stay inside the declaration. *)
      ("red", "--x:red");
      ("0 0 var(--spacing) black", "--x:0 0 var(--spacing) black");
      ("\"a;b\"", "--x:\"a;b\"");
      ("{a:b;}", "--x:{a:b;}");
      ("red/*", "--x:red");
      (* Sec. 5.5.6 discards whitespace at both ends of a declaration value, so
         a whitespace-only one is the empty value sec. 2.2 of CSS Custom
         Properties 1 (ED) calls valid. *)
      (" ", "--x:");
      (* CSS Syntax 3 (ED) sec. 4.3.5 ends a string at EOF as the string it
         read, so this is one declaration value and writes back closed. *)
      ("\"abc", "--x:\"abc\"");
      (* Not one declaration value: each of these closes the enclosing block,
         starts a second declaration, or runs to end of input. *)
      ("red}", "<rejected>");
      ("red}.evil{color:lime", "<rejected>");
      ("red}@media print{.x{color:red}", "<rejected>");
      ("red;--y:lime", "<rejected>");
      ("red)", "<rejected>");
      ("red]", "<rejected>");
      ("rgb(1,2,3", "<rejected>");
      ("{a:b", "<rejected>");
      (* CSS Variables 1 sec. 2 gives a custom property the value grammar
         [<declaration-value>?], so the empty value is one of its values and
         writes back as the declaration it names. *)
      ("", "--x:");
    ];
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string) (name ^ ":red") expected (bind name "red"))
    [
      ("--x", "--x:red");
      ("--color-red-500", "--color-red-500:red");
      ("--x;y", "--x\\;y:red");
      ("--x}y", "--x\\}y:red");
      ("--x y", "--x\\ y:red");
      ("--", "<rejected>");
      ("-x", "<rejected>");
      ("color", "<rejected>");
    ]

(* [custom_property] takes authored CSS text, so a pair it accepts has to write
   back as the one declaration it names. CSS Variables 1 sec. 2 gives a custom
   property the value grammar [<declaration-value>?] and a [<dashed-ident>]
   name; a value carrying a top-level [;] or [}], an unterminated function,
   block or string, or an unmatched closing bracket stops being part of the
   declaration as soon as the text is read back, so it is not a pair this
   library can write. A name carrying one of those is written back with the
   escapes that read it (CSS Syntax 3 sec. 4.3.7), so it names the declaration
   it made. *)
let custom_property_guard () =
  (* Build the declaration, print it into a rule and read the rule back. The
     answer is derived from the round-trip, not from a pinned spelling. *)
  let build name value =
    match custom_property name value with
    | exception Invalid_argument _ -> "<refused>"
    | d -> (
        let text = to_string ~minify:true d in
        let back =
          match
            Css.of_string ~strict:false
              (String.concat "" [ ":root{"; text; "}" ])
          with
          | Ok { Css.stylesheet = [ Css.Stylesheet.Rule r ]; _ } ->
              List.map (to_string ~minify:true) r.declarations
          | Ok { Css.stylesheet; _ } ->
              [
                String.concat ""
                  [
                    "<"; string_of_int (List.length stylesheet); " statements>";
                  ];
              ]
          | Error _ -> [ "<unparsable>" ]
        in
        match back with
        | [ text' ] when String.equal text text' -> "<round-trips>"
        | back ->
            String.concat ""
              [ text; " reads back as "; String.concat " + " back ])
  in
  List.iter
    (fun (value, expected) ->
      Alcotest.(check string)
        (String.concat "" [ "--x:"; value ])
        expected (build "--x" value))
    [
      (* A [<declaration-value>], and the empty value CSS Variables 1 allows. *)
      ("red", "<round-trips>");
      ("", "<round-trips>");
      (" ", "<round-trips>");
      ("0 0 var(--spacing) black", "<round-trips>");
      ("\"a;b\"", "<round-trips>");
      ("{a:b;}", "<round-trips>");
      ("red/*", "<round-trips>");
      (* CSS Syntax 3 sec. 4.3.6 returns the [<url-token>] at end of input, so
         [url(foo] is the same token [url(foo)] is. *)
      ("url(foo", "<round-trips>");
      (* sec. 4.3.5 ends a string at EOF as the string it read, so this one is a
         declaration value and writes back closed. *)
      ("\"abc", "<round-trips>");
      (* Not a [<declaration-value>]: a second declaration, a closed rule, an
         unterminated function or block, an unmatched bracket. *)
      ("red;--b:blue", "<refused>");
      ("red} .evil{color:lime", "<refused>");
      ("red}", "<refused>");
      ("rgb(1,2,3", "<refused>");
      ("url(foo bar)", "<refused>");
      ("{a:b", "<refused>");
      ("red)", "<refused>");
      ("red]", "<refused>");
    ];
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        (String.concat "" [ name; ":red" ])
        expected (build name "red"))
    [
      ("--x", "<round-trips>");
      ("--color-red-500", "<round-trips>");
      ("--x}y", "<round-trips>");
      ("--x;y", "<round-trips>");
      ("--x y", "<round-trips>");
      ("--", "<refused>");
      ("-x", "<refused>");
      ("color", "<refused>");
    ]

(* [parse_declaration] is handed the name and the value as two strings, and CSS
   Syntax 3 sec. 4.3.7 lets an escape carry a [;], a [}] or a space into a
   custom property's name. Written into the text in front of the [:], such a
   name ends its own declaration, closes the rule around it, or names a
   different property, so the two never meet as text: the name reaches the
   reader as the one ident it is. *)
let parse_declaration_name_case () =
  let bound name value =
    match parse_declaration name value with
    | None -> "<none>"
    | Some d -> (
        let text = to_string ~minify:true d in
        let names decl =
          Option.equal String.equal
            (Css.custom_declaration_name decl)
            (Some name)
        in
        let read_back =
          Css.of_string ~strict:true (String.concat "" [ ":root{"; text; "}" ])
        in
        match read_back with
        | Ok { Css.stylesheet = [ Css.Stylesheet.Rule r ]; _ } -> (
            match r.declarations with
            | [ d' ] when String.equal text (to_string ~minify:true d') ->
                if names d && names d' then text
                else String.concat "" [ text; " names another property" ]
            | back ->
                String.concat ""
                  [
                    text;
                    " reads back as ";
                    String.concat " + " (List.map (to_string ~minify:true) back);
                  ])
        | Ok { Css.stylesheet; _ } ->
            String.concat ""
              [
                text;
                " reads back as ";
                string_of_int (List.length stylesheet);
                " statements";
              ]
        | Error _ -> String.concat "" [ text; " does not read back" ])
  in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string) name expected (bound name "red"))
    [
      ("--x", "--x:red");
      ("--x;y", "--x\\;y:red");
      ("--x}y", "--x\\}y:red");
      ("--x y", "--x\\ y:red");
    ];
  (* A name is one ident or it names nothing: a [:] or a [;] inside it belongs
     to no property cascade can write. *)
  List.iter
    (fun name ->
      Alcotest.(check bool)
        (String.concat "" [ name; " names no declaration" ])
        true
        (parse_declaration name "red" = None))
    [ "a:b"; "color;background"; "color:red"; "" ]

let spec_custom_tokens () =
  check_specified_value "custom property token stream specified"
    "--tokens: [a, b] (c) { d: e; }" "[a, b] (c) { d: e; }";
  check_declaration ~expected:"--tokens:[a,b](c){d:e;}"
    "--tokens: [a, b] (c) { d: e; }";
  check_declaration ~expected:"--empty:" "--empty:";
  check_declaration ~expected:"--commented:a b" "--commented: a /*x*/ b";
  (* CSS Syntax 3 (ED) sec. 5.5.6 takes the flag off when the last two
     non-whitespace values are a [!] delim and an ident matching [important], so
     the whitespace between the two is inside the flag and not in the value. *)
  check_declaration ~expected:"--important-token:1!important"
    "--important-token: 1 ! important";
  check_declaration ~expected:"--real-important:1!important"
    "--real-important: 1 !important";
  check_declaration ~expected:"--fallback:var(--missing,)"
    "--fallback: var(--missing,)";
  check_specified_value "custom property nested fallback specified"
    "--nested-var: var(--a, var(--b, { color: red; }))"
    "var(--a, var(--b, { color: red; }))";
  check_declaration ~expected:"--nested-var:var(--a,var(--b,{color:red;}))"
    "--nested-var: var(--a, var(--b, { color: red; }))";
  (* CSS Syntax 3 sec. 4.3.5 returns the string token when the input ends before
     the closing quote, so the value holds a string and prints as one. Keeping
     the original bytes instead left it swallowing whatever followed. *)
  check_declaration ~expected:"--bad-string:\"unterminated\""
    "--bad-string: \"unterminated";
  neg_cursor read_declaration "--: value";
  neg_cursor read_declaration "-x: value";
  neg_cursor read_declaration "--x";

  (* CSS Syntax 3 sec. 7.2: a <declaration-value> is any token sequence that
     holds no unmatched closer and no bad-url, so a custom property whose value
     carries one is invalid rather than opaque. *)
  check_declaration ~expected:"--x:hover{}" "--x: hover { }";
  check_declaration ~expected:"--x:a b" "--x: a b";
  neg_cursor read_declaration "--x: hover { ] }";
  neg_cursor read_declaration "--x: a ] b";
  neg_cursor read_declaration "--x: a ) b";
  neg_cursor read_declaration "--x: url(a b)";
  (* Sec. 7.2 keeps a top-level [!] out of the production too, so what is left
     once the flag is off has to hold none. *)
  neg_cursor read_declaration "--x: a!b";
  neg_cursor read_declaration "--x: 1px!";
  neg_cursor read_declaration "--x:! 1px";
  check_declaration ~expected:"--x:(a!b)" "--x: (a!b)"

(* CSS Fonts 4 sec. 2.1.1 gives a [<font-family-name>] two spellings, a
   [<string>] and a [<custom-ident>+], and they name the same family whether
   that ident sequence runs to one word or several, so the structural-diff key
   folds the quoted spelling onto the bare one. The fold needs a generic family
   in the stream to prove the stream is a font-family list: a custom property is
   otherwise an arbitrary token stream, in which dropping the quotes is a
   different value wherever the property substitutes into another grammar. A
   name the section excludes from [<custom-ident>] keeps its quotes, since
   unquoting it would name the keyword instead of the family. *)
let custom_font_equivalence_key () =
  let key css =
    Css.declaration_value_for_equivalence (ok (Css.Declaration.of_string css))
  in
  List.iter
    (fun (css, expected) -> Alcotest.(check string) css expected (key css))
    [
      (* [sans-serif] is only valid in a font-family list, so both spellings of
         the multi-word name reach the unquoted one. *)
      ({|--f: a,"Segoe UI",sans-serif|}, "a,Segoe UI,sans-serif");
      ({|--f: a,Segoe UI,sans-serif|}, "a,Segoe UI,sans-serif");
      (* No generic family, no proof: the quotes are part of the value. *)
      ({|--f: a,"Segoe UI",b|}, {|a,"Segoe UI",b|});
      ({|--f: a,Segoe UI,b|}, "a,Segoe UI,b");
      (* A [<custom-ident>+] of one word is still a [<custom-ident>+], so the
         lone name reaches the unquoted spelling from either side of the generic
         family. *)
      ({|--f: "Arial",sans-serif|}, "Arial,sans-serif");
      ({|--f: Arial,sans-serif|}, "Arial,sans-serif");
      ({|--f: sans-serif,"Arial"|}, "sans-serif,Arial");
      (* The gate rules the lone name as it rules the sequence. *)
      ({|--f: "Arial",b|}, {|"Arial",b|});
      ({|--f: Arial,b|}, "Arial,b");
      (* The words sec. 2.1.1 excludes from [<custom-ident>] keep their quotes:
         [font-family:serif] names the generic family and [font-family:inherit]
         the CSS-wide keyword, so unquoting either loses the author's family. *)
      ({|--f: "serif",sans-serif|}, {|"serif",sans-serif|});
      ({|--f: "default",sans-serif|}, {|"default",sans-serif|});
      ({|--f: "inherit",sans-serif|}, {|"inherit",sans-serif|});
      ({|--f: "emoji",sans-serif|}, {|"emoji",sans-serif|});
      (* A name with no [<custom-ident>] spelling at all keeps its quotes: [+]
         is no ident code point, and a word opening on a digit tokenises as a
         number rather than an ident. *)
      ({|--f: "Foo+Bar",sans-serif|}, {|"Foo+Bar",sans-serif|});
      ({|--f: "Foo Bar 2",sans-serif|}, {|"Foo Bar 2",sans-serif|});
    ];
  let equal a b = String.equal (key a) (key b) in
  Alcotest.(check bool)
    "a generic family equates the two spellings" true
    (equal {|--f: a,"Segoe UI",sans-serif|} {|--f: a,Segoe UI,sans-serif|});
  Alcotest.(check bool)
    "without one they stay distinct" false
    (equal {|--f: a,"Segoe UI",b|} {|--f: a,Segoe UI,b|});
  Alcotest.(check bool)
    "a single-word name equates too" true
    (equal {|--f: "Arial",sans-serif|} {|--f: Arial,sans-serif|});
  Alcotest.(check bool)
    "a single-word name without a generic family stays distinct" false
    (equal {|--f: "Arial",b|} {|--f: Arial,b|});
  Alcotest.(check bool)
    "a quoted generic family stays distinct from the keyword" false
    (equal {|--f: "serif",sans-serif|} {|--f: serif,sans-serif|})

let color_functions () =
  (* color() with alternate spaces and alpha *)
  check_declaration ~expected:"color:color(display-p3 1 0 0/.5)"
    "color: color(display-p3 1 0 0 / 0.5)"

let filter_omitted_arguments () =
  (* Filter Effects 1 section 6.1: omitted blur/hue-rotate arguments default to
     zero; all optional number-percentage arguments default to one. *)
  List.iter
    (fun prop ->
      List.iter
        (fun (fn, default) ->
          let omitted = String.concat "" [ prop; ":"; fn; "()" ] in
          let explicit =
            String.concat "" [ prop; ":"; fn; "("; default; ")" ]
          in
          check_declaration ~expected:omitted omitted;
          check_declaration ~expected:omitted
            (String.concat "" [ prop; ":"; fn; "( /**/ )" ]);
          let canonical text =
            Css_test_helpers.ok (of_string text)
            |> normalize
            |> string_of_value ~minify:true
          in
          Alcotest.(check string)
            omitted (canonical explicit) (canonical omitted))
        [
          ("blur", "0px");
          ("brightness", "1");
          ("contrast", "1");
          ("grayscale", "1");
          ("hue-rotate", "0deg");
          ("invert", "1");
          ("opacity", "1");
          ("saturate", "1");
          ("sepia", "1");
        ])
    [ "filter"; "backdrop-filter"; "-webkit-backdrop-filter" ]

let filter_blur_grammar () =
  (* Filter Effects 1 section 6.1: blur takes a non-negative length, never a
     percentage. CSS Values 4 section 10: math ranges clamp at computed-value
     time, but a percentage cannot acquire a length type by cancelling out. *)
  List.iter
    (fun prop ->
      List.iter
        (fun value ->
          none_cursor read_declaration (prop ^ ":blur(" ^ value ^ ")"))
        [
          "0%";
          "10%";
          "-1px";
          "auto";
          "none";
          "min-content";
          "calc(0%)";
          "calc(1px + 0%)";
          "calc(10% - 10%)";
          "min(1px, 2%)";
          "min(0, 1px)";
          "clamp(0, 1px, 2px)";
          "max(0%, 1px)";
          "clamp(0px, 1%, 2px)";
          "minmax(1px, 2px)";
          "fit-content(1px)";
        ];
      List.iter
        (fun value ->
          let input = prop ^ ":blur(" ^ value ^ ")" in
          let declarations = check_declarations input 1 in
          List.iter
            (fun declaration ->
              let printed =
                Css.Declaration.to_string ~minify:true declaration
              in
              ignore (check_declarations printed 1))
            declarations)
        [
          "";
          "0";
          "1px";
          "1em";
          "calc(1px + 2px)";
          "calc(-1px)";
          "min(-1px, 2px)";
          "max(0px, 1em)";
          "clamp(0px, 1em, 2px)";
          "var(--radius)";
        ])
    [ "filter"; "backdrop-filter"; "-webkit-backdrop-filter" ]

let angle_units () =
  check_declaration ~expected:"transform:rotate(.5turn)"
    "transform: rotate(0.5turn)";
  check_declaration ~expected:"transform:rotate(1.5708rad)"
    "transform: rotate(1.5708rad)";
  (* deg/turn/grad conversion is an optimize rewrite, not a pp spelling: pp
     holds the authored units, optimize converts to the shortest. *)
  decl_optimizes_to ~held:"transform:skew(.25turn,100grad)"
    ~into:"transform:skew(90deg,90deg)" "transform: skew(0.25turn, 100grad)";
  (* A radian converts through pi, so it is never exactly a whole number of
     degrees - except at zero, which is the same angle in every unit. *)
  decl_optimizes_to ~held:"transform:rotate(0rad)"
    ~into:"transform:rotate(0deg)" "transform: rotate(0rad)";
  decl_optimizes_to ~held:"filter:hue-rotate(0rad)" ~into:"filter:hue-rotate()"
    "filter: hue-rotate(0rad)";
  (* hue-rotate() takes [ <angle> | <zero> ]? and means 0 when omitted, so the
     other zero spellings drop the argument as well. *)
  decl_optimizes ~prop:"filter" ~into:"hue-rotate()" "hue-rotate(0turn)"

let property_case () =
  (* Property names are ASCII case-insensitive *)
  check_declaration ~expected:"color:red" "COLOR: red";
  check_declaration ~expected:"background-image:url(\"a b (1).png\")"
    "BACKGROUND-IMAGE: url(\"a b (1).png\")"

let url_values () =
  check_declaration
    ~expected:"background-image:url(https://example.com/img/a.png)"
    "background-image: url(https://example.com/img/a.png)";
  check_declaration ~expected:"background-image:url(\"a b.png\")"
    "background-image: url(\"a b.png\")";
  check_declaration
    ~expected:"background-image:url(data:image/svg+xml;utf8,<svg/>)"
    "background-image: url(data:image/svg+xml;utf8,<svg/>)"

let spec_platform_property_vectors () =
  List.iter
    (fun (input, expected) -> check_declaration ~expected input)
    [
      ("display: block", "display:block");
      ("position: absolute", "position:absolute");
      ("float: left", "float:left");
      ("clear: both", "clear:both");
      ("table-layout: fixed", "table-layout:fixed");
      ("border-collapse: collapse", "border-collapse:collapse");
      ("caption-side: bottom", "caption-side:bottom");
      ("z-index: auto", "z-index:auto");
      ("width: stretch", "width:stretch");
      ("height: contain", "height:contain");
      ("display: contents", "display:contents");
      ("anchor-name: --tooltip", "anchor-name:--tooltip");
      ("position-anchor: --tooltip", "position-anchor:--tooltip");
      ( "position-try-fallbacks: --below, --above",
        "position-try-fallbacks:--below,--above" );
      ("grid-template-columns: subgrid", "grid-template-columns:subgrid");
      ("grid-template-rows: masonry", "grid-template-rows:masonry");
      ("shape-outside: circle(50%)", "shape-outside:circle(50%)");
      ("shape-margin: 1rem", "shape-margin:1rem");
      ("overflow-clip-margin: 1px", "overflow-clip-margin:1px");
      ("overflow-anchor: auto", "overflow-anchor:auto");
      ("scrollbar-width: thin", "scrollbar-width:thin");
      ( "scrollbar-gutter: stable both-edges",
        "scrollbar-gutter:stable both-edges" );
      ("line-height-step: 4px", "line-height-step:4px");
      ("font-palette: --brand", "font-palette:--brand");
      ( "font-synthesis: weight style small-caps",
        "font-synthesis:weight style small-caps" );
      ("text-wrap-style: pretty", "text-wrap-style:pretty");
      ("text-box-trim: trim-both", "text-box-trim:trim-both");
      ("writing-mode: sideways-rl", "writing-mode:sideways-rl");
      ("animation-timeline: scroll()", "animation-timeline:scroll()");
      ("animation-timeline: scroll(Block)", "animation-timeline:scroll()");
      ( "animation-range: entry 0% exit 100%",
        "animation-range:entry 0%exit 100%" );
      ("animation-range: Cover 10%", "animation-range:cover 10%");
      ( "transition-behavior: allow-discrete",
        "transition-behavior:allow-discrete" );
      (* CSS Transitions 2 sec. 2 spells the property
         [<transition-behavior-value>#], so it takes one behaviour per
         transition the way every other transition longhand does. *)
      ( "transition-behavior: normal, normal",
        "transition-behavior:normal,normal" );
      ( "transition-behavior: allow-discrete, normal",
        "transition-behavior:allow-discrete,normal" );
      ("view-transition-name: card", "view-transition-name:card");
      ("image-orientation: from-image", "image-orientation:from-image");
      ("background-clip: text", "background-clip:text");
      ("box-sizing: border-box", "box-sizing:border-box");
      ("aspect-ratio: 16 / 9", "aspect-ratio:16/9");
      ("contain: size layout style paint", "contain:size layout style paint");
      ("content-visibility: auto", "content-visibility:auto");
      ("contain-intrinsic-size: auto 300px", "contain-intrinsic-size:auto 300px");
      ("container-type: inline-size", "container-type:inline-size");
      ("container-name: card", "container-name:card");
      ("container: card / inline-size", "container:card/inline-size");
      (* Name-only form (no slash, no type) - the spec allows [container:
         <name>] alone; pins the canonical form for the typed shorthand emitter
         ([container : ?type_:_ -> container_name -> declaration]). *)
      ("container: card", "container:card");
      (* CSS Conditional 5 sec. 3.3 makes every keyword before the slash part of
         the required [<'container-name'>], which sec. 3.2 spells [none |
         <custom-ident>+]. *)
      ("container: card inline-size", "container:card inline-size");
      ("container: inline-size", "container:inline-size");
      ("overscroll-behavior: contain", "overscroll-behavior:contain");
      ("overscroll-behavior-inline: none", "overscroll-behavior-inline:none");
      ("overscroll-behavior-block: contain", "overscroll-behavior-block:contain");
      ("scroll-snap-type: x mandatory", "scroll-snap-type:x mandatory");
      ("scroll-snap-align: none start", "scroll-snap-align:none start");
      ("scroll-margin-block: 1px 1px", "scroll-margin-block:1px 1px");
      ("scroll-margin: 1px 2px 3px 4px", "scroll-margin:1px 2px 3px 4px");
      ("scroll-margin-block: 1rem 2rem", "scroll-margin-block:1rem 2rem");
      ("scroll-padding-inline: 10px", "scroll-padding-inline:10px");
      ("scroll-padding: 1px 2px", "scroll-padding:1px 2px");
      ("scroll-padding: 1px 2px 3px 4px", "scroll-padding:1px 2px 3px 4px");
      ("scroll-padding-block: auto 10%", "scroll-padding-block:auto 10%");
      ("margin-trim: block", "margin-trim:block");
      ("field-sizing: content", "field-sizing:content");
      ("color-scheme: light dark", "color-scheme:light dark");
      ("color-scheme: light mytheme", "color-scheme:light mytheme");
      ("accent-color: auto", "accent-color:auto");
      ("mask-mode: alpha", "mask-mode:alpha");
      ("mask-composite: add", "mask-composite:add");
      (* CSS Grid 3 (ED) removed the masonry track vocabulary, and Firefox ships
         this value behind a pref, so cascade keeps reading it rather than
         dropping author CSS a browser renders. The other properties of that
         draft are not modelled, and ride the unknown-property path any unknown
         name rides. *)
      ("grid-template-rows: masonry", "grid-template-rows:masonry");
      ("grid-template-columns: masonry", "grid-template-columns:masonry");
      ("masonry-direction: row", "masonry-direction:row");
      ("item-flow: row", "item-flow:row");
      (* Sec. 6.1 lists the slots source, slice, width, outset then repeat, so
         the canonical serialisation reorders what the author interleaved. *)
      ("border-image: 50% none", "border-image:none 50%");
      ("border-image: 1 none", "border-image:none 1");
      ("border-image: round none 30", "border-image:none 30 round");
      ("border-image: 30 / 2 url(a.png)", "border-image:url(a.png)30/2");
      ("ruby-overhang: spaces", "ruby-overhang:spaces");
      ("-webkit-mask-origin: content", "-webkit-mask-origin:content");
      ("-webkit-mask-origin: padding", "-webkit-mask-origin:padding");
      ("-webkit-mask-origin: border", "-webkit-mask-origin:border");
      ("-webkit-mask-clip: content", "-webkit-mask-clip:content");
      ("-webkit-mask-clip: text", "-webkit-mask-clip:text");
      ("zoom: calc(.5)", "zoom:calc(.5)");
      ("zoom: calc(50%)", "zoom:50%");
      ("border-image-slice: calc(10%)", "border-image-slice:10%");
      ("border-image-slice: calc(-10%)", "border-image-slice:calc(-10%)");
      ("interest-delay: normal 120ms", "interest-delay:normal 120ms");
      ("interest-delay: 120ms normal", "interest-delay:120ms normal");
      ("interest-delay: 1s 2s", "interest-delay:1s 2s");
      ("interest-delay: normal", "interest-delay:normal");
      ("scroll-margin-block: 0 0", "scroll-margin-block:0 0");
      ("scroll-margin-inline: 0 0", "scroll-margin-inline:0 0");
      ("grid-row-end: calc(.5)", "grid-row-end:calc(.5)");
      ("grid-column-start: calc(1.4)", "grid-column-start:calc(1.4)");
      ("z-index: calc(.5)", "z-index:calc(.5)");
      ("order: calc(1.4)", "order:calc(1.4)");
      ("font-language-override: \"ENG\"", "font-language-override:\"ENG\"");
      ("font-language-override: \"A\"", "font-language-override:\"A\"");
      ("-webkit-mask-composite: clear", "-webkit-mask-composite:clear");
      ("-webkit-mask-composite: copy", "-webkit-mask-composite:copy");
      ( "-webkit-mask-composite: source-atop",
        "-webkit-mask-composite:source-atop" );
      ( "-webkit-mask-composite: destination-over",
        "-webkit-mask-composite:destination-over" );
      ( "-webkit-mask-composite: destination-in",
        "-webkit-mask-composite:destination-in" );
      ( "-webkit-mask-composite: destination-out",
        "-webkit-mask-composite:destination-out" );
      ( "-webkit-mask-composite: destination-atop",
        "-webkit-mask-composite:destination-atop" );
      ( "-webkit-mask-composite: plus-lighter",
        "-webkit-mask-composite:plus-lighter" );
      ( "-webkit-mask-composite: copy, destination-in",
        "-webkit-mask-composite:copy,destination-in" );
      ("offset-path: path('M 0 0 L 1 1')", "offset-path:path(\"M 0 0 L 1 1\")");
      ("offset-distance: 50%", "offset-distance:50%");
      ("font-size-adjust: from-font", "font-size-adjust:from-font");
      ("font-variant-emoji: emoji", "font-variant-emoji:emoji");
      ("text-spacing-trim: trim-start", "text-spacing-trim:trim-start");
      ("hyphenate-limit-chars: 6 3 2", "hyphenate-limit-chars:6 3 2");
      ("initial-letter: 2 3", "initial-letter:2 3");
      ( "text-decoration-thickness: from-font",
        "text-decoration-thickness:from-font" );
      ("view-timeline-name: --reveal", "view-timeline-name:--reveal");
      ("view-timeline-axis: block", "view-timeline-axis:block");
      ("timeline-scope: --reveal", "timeline-scope:--reveal");
      ("width: min(10px, 5vw)", "width:min(10px,5vw)");
      ("width: max(10px, 5vw)", "width:max(10px,5vw)");
      ("width: clamp(10px, 5vw, 100px)", "width:clamp(10px,5vw,100px)");
      ( "width: calc-size(auto, size + 1rem)",
        "width:calc-size(auto,size + 1rem)" );
      ("opacity: abs(-0.5)", "opacity:.5");
      ("opacity: sign(var(--delta))", "opacity:sign(var(--delta))");
      ( "background-image: image-set(url(a.avif) type(\"image/avif\") 1x, \
         url(a.png) type(\"image/png\") 1x)",
        "background-image:image-set(url(a.avif) \
         type(\"image/avif\")1x,url(a.png) type(\"image/png\")1x)" );
      ("width: attr(data-w px, 10px)", "width:attr(data-w px,10px)");
      ( "width: attr(data-w px, calc(100% - 1rem))",
        "width:attr(data-w px,calc(100% - 1rem))" );
      ( "width: attr(data-w px, var(--fallback, 10px))",
        "width:attr(data-w px,var(--fallback,10px))" );
      ( "height: attr(data-h type(<length>), 1rem)",
        "height:attr(data-h type(<length>),1rem)" );
      ( "color: attr(data-color type(<color>), red)",
        "color:attr(data-color type(<color>),red)" );
      ( "content: attr(data-label string, \"x y\")",
        "content:attr(data-label string,\"x y\")" );
      ( "content: attr(data-label string, var(--label, \"x y\"))",
        "content:attr(data-label string,var(--label,\"x y\"))" );
      ( "font: italic small-caps 650 condensed 16px/1.5 \"Brand\", serif",
        "font:italic small-caps 650 condensed 16px/1.5 Brand,serif" );
      ("display: block flex", "display:block flex");
      ( "grid-template: \"head head\" auto \"nav main\" 1fr / 12rem 1fr",
        "grid-template:\"head head\" auto \"nav main\" 1fr/12rem 1fr" );
      ( "transition: opacity 1s ease-in .2s allow-discrete",
        "transition:opacity 1s ease-in .2s allow-discrete" );
      ("animation-range: exit entry", "animation-range:exit entry");
      ("scroll-timeline: --scroller block", "scroll-timeline:--scroller block");
      ("view-timeline: --reveal inline", "view-timeline:--reveal inline");
    ];
  check_declaration ~expected:"scrollbar-color:red blue"
    ~optimized:"scrollbar-color:red #00f" "scrollbar-color: red blue";
  (* All-constant math reductions are optimize transforms; pp holds the authored
     function node and only drops default arguments. *)
  check_declaration ~expected:"width:round(10px,3px)" ~optimized:"width:9px"
    "width: round(nearest, 10px, 3px)";
  check_declaration ~expected:"width:mod(18px,5px)" ~optimized:"width:3px"
    "width: mod(18px, 5px)";
  check_declaration ~expected:"width:rem(18px,5px)" ~optimized:"width:3px"
    "width: rem(18px, 5px)";
  check_declaration ~expected:"width:hypot(3px,4px)" ~optimized:"width:5px"
    "width: hypot(3px, 4px)";
  check_declaration ~expected:"color:color-mix(in oklab,red 40%,blue)"
    ~optimized:"color:#7551b6" "color: color-mix(in oklab, red 40%, blue)";
  check_declaration ~expected:"color:light-dark(canvastext,white)"
    ~optimized:"color:light-dark(canvastext,#fff)"
    "color: light-dark(CanvasText, white)";
  check_declaration ~expected:"width:attr(data-w px,calc(10px + 0px))"
    ~optimized:"width:attr(data-w px,calc(10px + 0px))"
    "width: attr(data-w px, calc(10px + 0px))";
  check_declaration
    ~expected:"border-image:linear-gradient(red,blue)30 fill/10px/1 stretch"
      (* [stretch] is the repeat's initial (CSS Backgrounds 3 sec. 5.5), which
         is what leaving the component out names. *)
    ~optimized:"border-image:linear-gradient(red,blue)30 fill/10px/1"
    "border-image: linear-gradient(red, blue) 30 fill / 10px / 1 stretch";
  List.iter
    (fun input -> neg_cursor read_declaration input)
    [
      "position: sticky absolute";
      "float: center";
      "table-layout: grid";
      "anchor-name: tooltip";
      "position-anchor: tooltip";
      "shape-margin: -1px";
      "overflow-anchor: sometimes";
      "scrollbar-width: wide";
      "scrollbar-gutter: stable auto";
      "line-height-step: -4px";
      "font-palette: 1";
      "text-wrap-style: loud";
      "text-box-trim: 1px";
      "animation-timeline: scroll(";
      "view-transition-name: none card";
      "image-orientation: upside-down";
      "aspect-ratio: 16 /";
      "contain: layout layout";
      "container-type: inline-size size";
      "container: / inline-size";
      "scroll-snap-type: mandatory x";
      "margin-trim: block inline block";
      "field-sizing: auto";
      "mask-composite: plus";
      (* CSS Box 4 sec. 3.1 gives every margin longhand [<length-percentage> |
         auto], so a sizing function reaches none of them; the shorthand already
         refused one. CSS Logical 1 sec. 4.2 spells the flow-relative pair from
         the same production. *)
      "margin-right: fit-content(20rem)";
      (* The prefixed stroke is a [||] of one width and one colour, so a second
         width fills no slot; cascade kept the last and dropped the first, which
         is a miscompile rather than a loose read. *)
      "-webkit-text-stroke: 100px 200px";
      (* CSS Backgrounds 3 sec. 2.1 gives the colour to the FINAL layer alone,
         so a colour in an earlier one is no background. *)
      "background: red, currentcolor";
      "background: red, url(a.png)";
      (* CSS Box Alignment 3 sec. 8.1 gives every gap a [0,inf] range, and the
         row-gap and column-gap longhands already refused a negative one. CSS
         Text Decoration 4 sec. 3 builds the shorthand from its own longhands,
         so a sizing function reaches neither. *)
      "gap: -10%";
      "gap: 1px -10%";
      "text-decoration: fit-content(20rem)";
      (* CSS Position 3 sec. 3.1 gives every inset longhand the same [auto |
         <length-percentage>], and sec. 3.2 builds the shorthand from it, so no
         sizing function reaches those either. *)
      "bottom: fit-content(20rem)";
      "top: calc-size(auto, size)";
      "inset: fit-content(20rem)";
      "inset-inline-start: fit-content(20rem)";
      "inset-block: fit-content(20rem)";
      "margin-left: fit-content(20rem)";
      "margin-block-end: fit-content(20rem)";
      "margin-inline: fit-content(20rem)";
      "margin-top: calc-size(auto, size)";
      "mask-origin: no-clip";
      "-webkit-mask-origin: fill-box";
      "-webkit-mask-origin: view-box";
      "-webkit-mask-clip: no-clip";
      "-webkit-mask-origin: text";
      "mask-clip: text";
      "zoom: -50%";
      "border-image-slice: -10%";
      "interest-delay: normal normal normal";
      "interest-delay-start: normal 1s";
      "grid-row-end: calc(0)";
      "grid-column-start: balance 0";
      "grid-row-end: balance 0";
      "border-image: none, none";
      "border-image: url(a.png), none";
      "border-image-source: none, none";
      "mask-border: none, none";
      "font-language-override: \"default\"";
      "font-language-override: \"ENGLISH\"";
      "font-language-override: \"\"";
      "grid: \"<\" \">\"";
      "grid: \"\"";
      "grid: \"a\" \"b c\"";
      "grid: \"a b\" \"b a\"";
      "grid-template: \"nav/main\"";
      "grid-template: \"a\" \"a a\"";
      "grid-template: \"a .\" \". a\"";
      "-webkit-mask-composite: plus-darker";
      "-webkit-mask-composite: add";
      "offset-distance: -10% -10%";
      "font-size-adjust: from-font 1";
      "font-variant-emoji: smile";
      "initial-letter: 0";
      "view-timeline-axis: diagonal";
      "width: clamp(10px, 20px)";
      "width: round(10px)";
      "width: mod(10px)";
      "opacity: abs()";
      "opacity: sign()";
      "background-image: image-set(url(a.png))";
      "border-image: linear-gradient(red, blue) fill fill";
      "font: bold serif";
      "transition: allow-discrete allow-discrete allow-discrete";
      "scroll-timeline: block --scroller";
      "view-timeline: inline --reveal";
    ]

let spec_values_l45_edges () =
  List.iter
    (fun (input, expected) -> check_declaration ~expected input)
    [
      ("width: calc(100% - 2rem)", "width:calc(100% - 2rem)");
      (* pp is pure: it holds calc() unfolded and serializes lexically; the
         all-constant fold to 2px / 50px is an optimize transform. *)
      ("width: calc(1px * 2)", "width:calc(1px*2)");
      ("width: calc(100px / 2)", "width:calc(100px/2)");
      ("width: min(10px, 5cqw)", "width:min(10px,5cqw)");
      ("width: max(10svw, 20lvw)", "width:max(10svw,20lvw)");
      ("height: clamp(10dvh, 50%, 100dvh)", "height:clamp(10dvh,50%,100dvh)");
      ("margin: anchor-size(width)", "margin:anchor-size(width)");
      ("top: anchor(bottom)", "top:anchor(bottom)");
      ("font-size: calc(1rem + 1cqi)", "font-size:calc(1rem + 1cqi)");
      ("color: oklab(60% .1 .2)", "color:oklab(60%.1 .2)");
      ("color: oklch(60% .2 120)", "color:oklch(60%.2 120)");
      ("color: color(display-p3 1 0 0 / .5)", "color:color(display-p3 1 0 0/.5)");
      ( "color: rgb(from var(--c) r g b / 50%)",
        "color:rgb(from var(--c) r g b/.5)" );
      (* Each channel is one component value, so a space between two of them is
         a separator the tokens do not need: [20%g] is a percentage and an
         ident, and [calc(...)10] a function and a number. *)
      ("color: rgb(from red 20%g b / alpha)", "color:rgb(from red 20%g b/alpha)");
      ( "color: rgb(from red 20% g b / alpha)",
        "color:rgb(from red 20%g b/alpha)" );
      ( "color: rgb(from red r calc(g * 2)10)",
        "color:rgb(from red r calc(g * 2)10)" );
      ( "color: rgb(from red r calc(g * 2) 10)",
        "color:rgb(from red r calc(g * 2)10)" );
      (* CSS Color 5 sec. 4.1 puts the origin's [alpha] keyword, and math over
         it, wherever the function takes an [<alpha-value>], [color()] included.
         The sRGB self-substitution folds only a numeric alpha, so the keyword
         forms keep the call. *)
      ( "color: color(from red srgb r g b / alpha)",
        "color:color(from red srgb r g b/alpha)" );
      ( "color: color(from red srgb r g b / calc(alpha * 2))",
        "color:color(from red srgb r g b/calc(alpha * 2))" );
      (* Sec. 4.1 substitutes a channel keyword as a [<number>], so the ordinary
         math typing decides the rest: multiplying by a percentage gives a
         percentage, which the slot takes. *)
      ( "color: hwb(from red h calc(w * 1%) b)",
        "color:hwb(from red h calc(w * 1%) b)" );
      ( "color: lch(from red l c calc(h + 90))",
        "color:lch(from red l c calc(h + 90))" );
      (* pp holds the authored node for the Named blue, the rgb()/alpha, and the
         turn unit. The colour cross-fold and angle conversion are optimize
         transforms. *)
      ( "background: conic-gradient(from 45deg, red, blue)",
        "background:conic-gradient(from 45deg,red,blue)" );
      ( "background: cross-fade(url(a.png) 40%, url(b.png))",
        "background:cross-fade(url(a.png) 40%,url(b.png))" );
      ( "filter: drop-shadow(0 0 2px rgb(0 0 0 / .4))",
        "filter:drop-shadow(0 0 2px rgb(0 0 0/.4))" );
      ( "transform: translate(10px, 20%) rotate(.25turn) scale(1.2)",
        "transform:translate(10px,20%)rotate(.25turn)scale(1.2)" );
      ( "background-position: left 10px top 20%",
        "background-position:left 10px top 20%" );
      ("border-radius: 10px / 20px", "border-radius:10px/20px");
      ( "clip-path: xywh(0 0 100% 100% round 10px)",
        "clip-path:xywh(0 0 100% 100% round 10px)" );
    ];
  (* optimize+minify owns the folds pp holds above: Named->hex, rgb()->hex,
     turn->shortest angle, and the position-keyword canonicalization. *)
  check_declaration ~expected:"color:lab(50%20 30)" ~optimized:"color:#a16945"
    "color: lab(50% 20 30)";
  check_declaration ~expected:"color:lch(50%30 40)" ~optimized:"color:#a26757"
    "color: lch(50% 30 40)";
  decl_optimizes ~prop:"background" ~into:"conic-gradient(from 45deg,red,#00f)"
    "conic-gradient(from 45deg, red, blue)";
  decl_optimizes ~prop:"filter" ~into:"drop-shadow(0 0 2px #0006)"
    "drop-shadow(0 0 2px rgb(0 0 0 / .4))";
  decl_optimizes ~prop:"transform"
    ~into:"translate(10px,20%)rotate(90deg)scale(1.2)"
    "translate(10px, 20%) rotate(.25turn) scale(1.2)";
  (* translateX(v) is translate(v, 0) = translate(v), one byte shorter; pp holds
     the authored function, optimize folds it. translateY has no shorter
     form. *)
  decl_optimizes ~prop:"transform" ~held:"translateX(10px)"
    ~into:"translate(10px)" "translateX(10px)";
  decl_optimizes ~prop:"transform" ~held:"translateY(10px)"
    ~into:"translateY(10px)" "translateY(10px)";
  decl_optimizes ~prop:"background-position" ~into:"10px 20%"
    "left 10px top 20%";
  decl_optimizes ~prop:"background-position" ~into:"50%" "center";
  decl_optimizes ~prop:"background-position" ~into:"top" "center top";
  decl_optimizes_to
    ~into:"-webkit-mask-position:10px 20px;mask-position:10px 20px"
    "mask-position:left 10px top 20px";
  List.iter
    (fun input -> neg_cursor read_declaration input)
    [
      "width: calc(1px + )";
      "width: calc(* 1px)";
      "width: min()";
      "height: clamp(10px, 20px)";
      "margin: anchor-size()";
      "top: anchor()";
      "color: lab(50% 20)";
      "color: oklch(60% .2)";
      "color: color(display-p3 1 0)";
      "color: rgb(from red r g)";
      "background: conic-gradient()";
      "background: cross-fade(url(a.png), )";
      "filter: drop-shadow()";
      "transform: translate()";
      "border-radius: 10px /";
      "clip-path: xywh(0 0)";
    ]

let spec_remaining_prop_vectors () =
  List.iter
    (fun (input, expected) -> check_declaration ~expected input)
    [
      ("display: ruby", "display:ruby");
      ("display: table-caption", "display:table-caption");
      ("position: fixed", "position:fixed");
      ("float: inline-start", "float:inline-start");
      ("clear: inline-end", "clear:inline-end");
      ("contain: strict", "contain:strict");
      ("content-visibility: hidden", "content-visibility:hidden");
      ( "contain-intrinsic-width: auto 10rem",
        "contain-intrinsic-width:auto 10rem" );
      ("contain-intrinsic-height: 20px", "contain-intrinsic-height:20px");
      ("overflow: clip auto", "overflow:clip auto");
      ("overflow: clip visible", "overflow:clip visible");
      ("overflow-block: scroll", "overflow-block:scroll");
      ("overflow-inline: hidden", "overflow-inline:hidden");
      ( "overscroll-behavior-inline: contain",
        "overscroll-behavior-inline:contain" );
      ("scroll-snap-align: start end", "scroll-snap-align:start end");
      ("scroll-snap-stop: always", "scroll-snap-stop:always");
      ("scroll-margin: 1px 2px 3px 4px", "scroll-margin:1px 2px 3px 4px");
      ("scroll-padding: 1rem 2rem", "scroll-padding:1rem 2rem");
      ("columns: 12rem 3", "columns:12rem 3");
      ( "column-rule: 1px solid currentColor",
        "column-rule:1px solid currentColor" );
      ("column-span: all", "column-span:all");
      ("break-before: page", "break-before:page");
      ("break-after: avoid-page", "break-after:avoid-page");
      ("break-inside: avoid-column", "break-inside:avoid-column");
      ("box-decoration-break: clone", "box-decoration-break:clone");
      ("background-origin: content-box", "background-origin:content-box");
      ("background-clip: padding-box", "background-clip:padding-box");
      ("background-size: contain", "background-size:contain");
      ("border-block: 1px solid red", "border-block:1px solid red");
      ("border-start-start-radius: 1rem", "border-start-start-radius:1rem");
      ("outline: 2px solid Highlight", "outline:2px solid highlight");
      ("outline-offset: -2px", "outline-offset:-2px");
      ( "text-decoration: underline wavy red 2px",
        "text-decoration:underline wavy red 2px" );
      ("text-underline-offset: 2px", "text-underline-offset:2px");
      ( "text-underline-position: under left",
        "text-underline-position:under left" );
      ("text-decoration-skip: auto", "text-decoration-skip:auto");
      ("text-decoration-skip-self: objects", "text-decoration-skip-self:objects");
      ("text-decoration-skip-box: none", "text-decoration-skip-box:none");
      ("text-decoration-skip-inset: auto", "text-decoration-skip-inset:auto");
      ( "text-decoration-skip-spaces: start end",
        "text-decoration-skip-spaces:start end" );
      ("text-emphasis: filled dot red", "text-emphasis:filled dot red");
      ("text-emphasis-style: open sesame", "text-emphasis-style:open sesame");
      ("text-emphasis-color: currentColor", "text-emphasis-color:currentColor");
      ("text-emphasis-position: over right", "text-emphasis-position:over right");
      ( "text-emphasis-skip: punctuation symbols",
        "text-emphasis-skip:punctuation symbols" );
      ("text-orientation: mixed", "text-orientation:mixed");
      ("tab-size: 4", "tab-size:4");
      ("line-break: anywhere", "line-break:anywhere");
      ("overflow-wrap: anywhere", "overflow-wrap:anywhere");
      ("hyphens: manual", "hyphens:manual");
      ("font-optical-sizing: auto", "font-optical-sizing:auto");
      ("font-kerning: normal", "font-kerning:normal");
      ("font-language-override: \"TRK\"", "font-language-override:\"TRK\"");
      ("font-synthesis-style: oblique-only", "font-synthesis-style:oblique-only");
      ("font-synthesis-style: auto", "font-synthesis-style:auto");
      ("font-synthesis-weight: none", "font-synthesis-weight:none");
      ("font-synthesis-small-caps: auto", "font-synthesis-small-caps:auto");
      ("font-synthesis-position: none", "font-synthesis-position:none");
      ( "font-variant-ligatures: common-ligatures",
        "font-variant-ligatures:common-ligatures" );
      ( "font-variant-ligatures: common-ligatures no-discretionary-ligatures \
         historical-ligatures contextual",
        "font-variant-ligatures:common-ligatures no-discretionary-ligatures \
         historical-ligatures contextual" );
      ("font-variant-caps: small-caps", "font-variant-caps:small-caps");
      ( "font-variant-numeric: tabular-nums slashed-zero",
        "font-variant-numeric:tabular-nums slashed-zero" );
      ( "font-variant-numeric: oldstyle-nums tabular-nums stacked-fractions \
         ordinal slashed-zero",
        "font-variant-numeric:oldstyle-nums tabular-nums stacked-fractions \
         ordinal slashed-zero" );
      ("font-variant-position: sub", "font-variant-position:sub");
      (* CSS Fonts 4 (ED) sec. 6.6. Chrome 146 reads each of these and refuses a
         repeated component, a reserved feature name and a bare ident. *)
      ("font-variant-alternates: normal", "font-variant-alternates:normal");
      (* CSS Fonts 4 (ED) sec. 6.10. Chrome 146 reads each of these and refuses
         a component written twice over. *)
      ("font-variant: normal", "font-variant:normal");
      (* CSS Fonts 4 (ED) sec. 5.3 writes the shorthand's width slot as
         [<font-width-css3>], so the keyword is the only spelling that reads
         back. *)
      ("font: expanded 12px serif", "font:expanded 12px serif");
      ("font: ultra-condensed 12px serif", "font:ultra-condensed 12px serif");
      ("font-variant: none", "font-variant:none");
      ("font-variant: small-caps", "font-variant:small-caps");
      ( "font-variant: common-ligatures small-caps tabular-nums ruby sub text",
        "font-variant:common-ligatures small-caps tabular-nums ruby sub text" );
      ("font-variant: swash(a) small-caps", "font-variant:swash(a) small-caps");
      ( "font-variant-alternates: historical-forms",
        "font-variant-alternates:historical-forms" );
      ( "font-variant-alternates: swash(fancy)",
        "font-variant-alternates:swash(fancy)" );
      ( "font-variant-alternates: styleset(a, b) historical-forms",
        "font-variant-alternates:styleset(a,b) historical-forms" );
      ( "font-variant-alternates: character-variant(a, b) annotation(c)",
        "font-variant-alternates:character-variant(a,b) annotation(c)" );
      ( "font-variant-alternates: stylistic(a) ornaments(b)",
        "font-variant-alternates:stylistic(a) ornaments(b)" );
      ("font-variant-east-asian: ruby", "font-variant-east-asian:ruby");
      ( "font-variant-east-asian: traditional proportional-width ruby",
        "font-variant-east-asian:traditional proportional-width ruby" );
      ("object-view-box: inset(0 0 10% 0)", "object-view-box:inset(0 0 10%0)");
      ("image-rendering: pixelated", "image-rendering:pixelated");
      ("image-resolution: from-image 2dppx", "image-resolution:from-image 2dppx");
      ("mask-border: url(mask.svg) 30 fill", "mask-border:url(mask.svg)30 fill");
      ("mask-size: contain", "mask-size:contain");
      ("mask-repeat: no-repeat", "mask-repeat:no-repeat");
      ("mask-position: left 10px top 20px", "mask-position:left 10px top 20px");
      ( "backdrop-filter: blur(4px) saturate(120%)",
        "backdrop-filter:blur(4px)saturate(120%)" );
      ("will-change: transform, opacity", "will-change:transform,opacity");
      ("touch-action: pan-x pinch-zoom", "touch-action:pan-x pinch-zoom");
      ("caret-color: auto", "caret-color:auto");
      ("resize: block", "resize:block");
      ( "transition-property: opacity, display",
        "transition-property:opacity,display" );
      (* CSS Transitions 1 sec. 2.1: none | <single-transition-property>#, where
         <single-transition-property> is [all | <custom-ident>]. The exclusion
         clause names none, inherit and initial as the values a list of more
         than one may not hold; all is not among them. *)
      ("transition-property: all, opacity", "transition-property:all,opacity");
      ("transition-property: opacity, all", "transition-property:opacity,all");
      ("transition-property: all, all", "transition-property:all,all");
      ("animation-composition: add", "animation-composition:add");
      ("animation-range-start: entry 10%", "animation-range-start:entry 10%");
      ("animation-range-end: exit 90%", "animation-range-end:exit 90%");
      ("scroll-timeline-name: --scroller", "scroll-timeline-name:--scroller");
      ("scroll-timeline-axis: inline", "scroll-timeline-axis:inline");
      ("view-timeline-inset: auto 10%", "view-timeline-inset:auto 10%");
      ("position-try-order: most-width", "position-try-order:most-width");
      ( "position-visibility: anchors-visible",
        "position-visibility:anchors-visible" );
      ("accent-color: auto", "accent-color:auto");
      ("color-scheme: only light dark", "color-scheme:light dark only");
      ( "forced-color-adjust: preserve-parent-color",
        "forced-color-adjust:preserve-parent-color" );
      ("print-color-adjust: exact", "print-color-adjust:exact");
      ("isolation: isolate", "isolation:isolate");
      ("mix-blend-mode: plus-lighter", "mix-blend-mode:plus-lighter");
      ("shape-margin: 2cqi", "shape-margin:2cqi");
      ("shape-image-threshold: .4", "shape-image-threshold:.4");
      ("container-type: inline-size", "container-type:inline-size");
      ("container-name: card layout", "container-name:card layout");
      ("container: card / inline-size", "container:card/inline-size");
      ("anchor-name: --target, --tooltip", "anchor-name:--target,--tooltip");
      ("position-anchor: --target", "position-anchor:--target");
      ("position-area: top span-left", "position-area:top span-left");
      ( "position-try-fallbacks: --below, flip-block",
        "position-try-fallbacks:--below,flip-block" );
      (* CSS Anchor Positioning 1 sec. 6.1: [<dashed-ident> || <try-tactic>],
         and <try-tactic> is itself [flip-block || flip-inline || flip-start].
         || is order-free, so each component appears at most once in any order
         and reads back name-first. *)
      ( "position-try-fallbacks: flip-block --below",
        "position-try-fallbacks:--below flip-block" );
      ( "position-try-fallbacks: --below flip-block",
        "position-try-fallbacks:--below flip-block" );
      ( "position-try-fallbacks: flip-inline flip-block",
        "position-try-fallbacks:flip-block flip-inline" );
      ( "position-try-fallbacks: --a flip-start, flip-inline",
        "position-try-fallbacks:--a flip-start,flip-inline" );
      ("scrollbar-color: auto", "scrollbar-color:auto");
      ("overlay: auto", "overlay:auto");
      ( "transition-behavior: allow-discrete",
        "transition-behavior:allow-discrete" );
      ("font-size-adjust: ex-height 0.5", "font-size-adjust:ex-height .5");
      ("font-variant-emoji: text", "font-variant-emoji:text");
      ("initial-letter: 2 3", "initial-letter:2 3");
      ("line-height-step: 4px", "line-height-step:4px");
      ("text-box: trim-both cap alphabetic", "text-box:trim-both cap alphabetic");
      (* CSS Inline 3 sec. 4.4 gives [text-box-edge] [auto | <text-edge>], so
         [auto] is one half of the sec. 6.1 shorthand, not a keyword the
         longhand keeps to itself. *)
      ("text-box: auto", "text-box:auto");
      ("text-box: trim-both auto", "text-box:trim-both auto");
      ("field-sizing: content", "field-sizing:content");
      ("margin-trim: block inline", "margin-trim:block inline");
      ("offset-distance: 10%", "offset-distance:10%");
      ("offset-rotate: reverse 45deg", "offset-rotate:reverse 45deg");
      ( "view-transition-class: card primary",
        "view-transition-class:card primary" );
    ];
  check_declaration ~expected:"border-inline-color:red blue"
    ~optimized:"border-inline-color:red #00f" "border-inline-color: red blue";
  check_declaration ~expected:"border-block-color:red blue"
    ~optimized:"border-block-color:red #00f" "border-block-color: red blue";
  check_declaration ~expected:"border-inline-width:1px 2px"
    "border-inline-width: 1px 2px";
  check_declaration ~expected:"border-block-width:4px" "border-block-width: 4px";
  List.iter
    (fun input -> neg_cursor read_declaration input)
    [
      "display: ruby block";
      "float: inline-start left";
      "contain: strict layout";
      "content-visibility: visible hidden";
      "contain-intrinsic-width: auto auto 10px";
      "overflow-block: visible hidden";
      "overscroll-behavior-inline: contain auto none";
      "scroll-snap-align: start center end";
      "scroll-snap-stop: normal always";
      "columns: 1 2 3";
      "column-rule: solid solid";
      "column-span: all none";
      "break-before: page column";
      "box-decoration-break: slice clone";
      "background-origin: border-box padding-box content-box border-box";
      "background-size: contain cover";
      "border-inline-color: red blue green";
      "border-block-color: red blue green";
      "border-inline-width: 1px 2px 3px";
      "border-start-start-radius: 1rem /";
      "text-decoration: underline none";
      "text-underline-position: auto under";
      "text-decoration-skip: none auto";
      "text-decoration-skip-self: auto";
      "text-decoration-skip-box: all none";
      "text-decoration-skip-inset: 1px";
      "text-decoration-skip-spaces: start start";
      "text-emphasis: filled open";
      "text-emphasis-style: dot circle";
      "text-emphasis-color: red blue";
      "text-emphasis-position: over under";
      "text-emphasis-skip: spaces spaces";
      "tab-size: -1";
      "line-break: anywhere strict";
      "font-optical-sizing: auto none";
      "font-kerning: normal none";
      "font-language-override: 1";
      "font-synthesis-style: auto none";
      "font-synthesis-small-caps: auto none";
      "font-synthesis-position: normal";
      "font-variant-ligatures: normal common-ligatures";
      "font-variant-ligatures: common-ligatures no-common-ligatures";
      "font-variant-caps: small-caps unicase";
      "font-variant-numeric: normal tabular-nums";
      "font-variant-numeric: lining-nums oldstyle-nums";
      "font-variant-position: sub super";
      "font-variant-alternates: swash(fancy) swash(x)";
      "font-variant: small-caps unicase";
      "font-variant: common-ligatures no-common-ligatures";
      "font-variant: jis78 jis83";
      "font-variant: bogus";
      "font: 125% 12px serif";
      "font: 110% 12px serif";
      "font-variant-alternates: swash(inherit)";
      "font-variant-alternates: swash(default)";
      "font-variant-alternates: bogus";
      "font-variant-alternates: normal historical-forms";
      "font-variant-east-asian: jis78 jis83";
      "font-variant-east-asian: normal ruby";
      "object-view-box: inset()";
      "image-rendering: pixelated smooth";
      "image-resolution: from-image from-image";
      "mask-border: fill fill";
      "mask-size: contain cover";
      "mask-repeat: no-repeat repeat repeat-x";
      "backdrop-filter: blur(1px, 2px)";
      "will-change: auto, transform";
      (* CSS Will Change 1 sec. 2.1 excludes these from the ident a feature
         names, and CSS Values 4 sec. 3.2 excludes [default] from every one. *)
      "will-change: none";
      "will-change: all";
      "will-change: default";
      "will-change: will-change";
      "will-change: opacity, none";
      "touch-action: pan-x pan-left";
      "resize: block inline both";
      "transition-property: none, opacity";
      "transition-property: opacity, none";
      "transition-property: inherit, opacity";
      "animation-composition: add replace";
      "animation-range-start: exit entry";
      "view-timeline-inset: auto auto auto";
      "position-try-order: most-width normal";
      "position-visibility: anchors-visible always";
      "accent-color: auto red";
      "color-scheme: only only";
      "color-scheme: only light only";
      "color-scheme: only dark only";
      (* CSS Values 4 sec. 2.2 makes each operand of a [&&] a contiguous run, so
         sec. 2.2's [[light | dark | <custom-ident>]+ && only?] puts [only] at
         one end of the list and never inside it. Chrome 153 drops each of these
         and reads [light dark only] and [only light dark]. *)
      "color-scheme: light only light";
      "color-scheme: dark only dark";
      "color-scheme: both only both";
      "color-scheme: light only dark";
      (* CSS Color Adjust 1 sec. 2.2 spells the list item [light | dark |
         <custom-ident>], and CSS Values 4 sec. 4.2 keeps [default] out of every
         one of those in all ASCII case permutations. *)
      "color-scheme: default";
      "color-scheme: DEFAULT";
      "color-scheme: light default";
      "forced-color-adjust: auto none";
      "print-color-adjust: exact economy";
      "isolation: isolate auto";
      "mix-blend-mode: multiply plus-lighter";
      "shape-image-threshold: 1.5 2";
      "container-name: none card";
      "container-name: and";
      "anchor-name: target";
      "anchor-name: --a none";
      "position-anchor: --a --b";
      "position-try-fallbacks: --a --b";
      "position-try-fallbacks: flip-block flip-block";
      "position-area: top bottom";
      "overlay: auto none";
      "transition-behavior: allow-discrete normal";
      "font-size-adjust: ex-height";
      "font-variant-emoji: text emoji";
      "initial-letter: 2 0";
      "text-box: cap trim-both";
      "field-sizing: content fixed";
      "margin-trim: block block";
      "offset-rotate: reverse reverse";
      "view-transition-class: none card";
    ]

let test_declaration () =
  (* Basic declarations - test the declaration type itself *)
  check "color:red";
  check "margin:10px";
  check "display:block";

  (* Custom properties *)
  check "--custom:value";
  check "--color:red";

  (* Important declarations *)
  check "color:red!important";
  check "--custom:value!important";

  (* Complex values. Per CSS Transforms 1 section 8 the printer drops whitespace
     between back-to-back transform functions under minify. *)
  check_declaration ~expected:"background:linear-gradient(to right,red,blue)"
    "background:linear-gradient(to right,red,blue)";
  decl_optimizes ~prop:"background" ~into:"linear-gradient(90deg,red,#00f)"
    "linear-gradient(to right,red,blue)";
  check "transform:translateX(10px)rotate(45deg)";
  check "font-family:Arial,sans-serif";

  (* Vendor prefixes *)
  check "-webkit-transform:rotate(45deg)";
  check "-moz-appearance:none";

  (* Test invalid declarations using none *)
  none_cursor read_declaration "color red";
  (* Missing colon *)
  none_cursor read_declaration "color:";
  (* Missing value *)
  none_cursor read_declaration ":red";
  (* Missing property *)
  none_cursor read_declaration "123invalid:value";
  (* Invalid property name *)
  none_cursor read_declaration "color:not-a-color";

  (* Invalid value *)

  (* Duplicate !important should fail *)
  neg_cursor read_declaration "color: red blue !important !important"

let css_wide_custom_property_vectors () =
  (* CSS Cascading: CSS-wide keywords are whole declaration values for standard
     properties, not component values inside a larger value. *)
  List.iter
    (fun keyword ->
      check_declaration ("color:" ^ keyword);
      (* pp holds the CSS-wide keyword verbatim; folding margin:initial to 0 is
         an optimize transform, not a pp serialization. *)
      check_declaration ("margin:" ^ keyword);
      none_cursor read_declaration ("color:" ^ keyword ^ " red");
      none_cursor read_declaration ("margin:1px " ^ keyword);
      none_cursor read_declaration ("background:red " ^ keyword))
    [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ];
  (* CSS Custom Properties: specified values preserve authored token streams;
     the minified declaration path may compact block/list whitespace where
     tokenization is unchanged. *)
  check_specified_value "custom property block token stream specified"
    "--tokens: { color: red }" "{ color: red }";
  check_declaration ~expected:"--tokens:{color:red}" "--tokens: { color: red }";
  check_specified_value "custom property list token stream specified"
    "--list: [a, b, c]" "[a, b, c]";
  check_declaration ~expected:"--list:[a,b,c]" "--list: [a, b, c]";
  check_declaration ~expected:"--empty-fallback:var(--missing,)"
    "--empty-fallback: var(--missing,)";
  (* CSS Syntax 3 (ED) sec. 5.5.6 reads the last two non-whitespace values, so
     the whitespace between [!] and [important] is inside the flag. *)
  check_declaration ~expected:"--spaced-important:1!important"
    "--spaced-important: 1 ! important";
  check_declaration ~expected:"--is-important:1!important"
    "--is-important: 1 !important";
  none_cursor read_declaration "--: invalid"

let substitution_defers_css_wide_mix_validation () =
  List.iter
    (fun (input, expected) -> check_declaration ~expected input)
    [
      ("margin: var(--x) inherit", "margin:var(--x) inherit");
      ("color: var(--x) initial", "color:var(--x) initial");
      ("all: var(--x) initial", "all:var(--x) initial");
      ( "margin: env(safe-area-inset-top) inherit",
        "margin:env(safe-area-inset-top) inherit" );
      ("width: attr(data-width px) initial", "width:attr(data-width px) initial");
    ];
  (* With no substitution, the CSS-wide keyword is still invalid beside any
     other component. Empty calls match none of the substitution grammars and do
     not defer validation either. *)
  List.iter
    (fun value -> none_cursor read_declaration ("margin:" ^ value))
    [ "1px inherit"; "var() inherit"; "env() inherit"; "attr() inherit" ]

let suite =
  ( "declaration_spec",
    [
      (* Core declaration type testing *)
      test_case "declaration" `Quick test_declaration;
      test_case "parse_custom_property rejects an escaping pair" `Quick
        parse_custom_property_guard;
      test_case "list-style-image takes one image" `Quick
        list_style_image_takes_one_image;
      test_case "a colour takes only a colour function" `Quick
        color_takes_only_a_colour_function;
      test_case "a grid shorthand names both axes" `Quick
        grid_shorthand_names_both_axes;
      test_case "an anchor query where the property takes one" `Quick
        anchor_query_where_the_property_takes_one;
      test_case "an empty value is no declaration" `Quick
        an_empty_value_is_no_declaration;
      test_case "custom_property refuses an escaping pair" `Quick
        custom_property_guard;
      test_case "parse_declaration keeps the name out of the value" `Quick
        parse_declaration_name_case;
      test_case "spec custom property token stream values" `Quick
        spec_custom_tokens;
      test_case "a generic family gates the equivalence key" `Quick
        custom_font_equivalence_key;
      test_case "color functions" `Quick color_functions;
      test_case "filter omitted arguments" `Quick filter_omitted_arguments;
      test_case "filter blur grammar" `Quick filter_blur_grammar;
      test_case "angle units" `Quick angle_units;
      test_case "url values" `Quick url_values;
      test_case "spec platform property vectors" `Quick
        spec_platform_property_vectors;
      test_case "spec values level 4/5 edge vectors" `Quick
        spec_values_l45_edges;
      test_case "spec remaining property vectors" `Quick
        spec_remaining_prop_vectors;
      test_case "spec declaration additional grammar vectors" `Quick
        css_wide_custom_property_vectors;
      test_case "substitution defers CSS-wide mix validation" `Quick
        substitution_defers_css_wide_mix_validation;
      test_case "property name case" `Quick property_case;
    ] )
