(** Stylesheet tests: per-module minify edges, from easing to [!important]. *)

open Cascade
open Stylesheet_helpers

(* {2 Easing / timing functions (CSS Easing L1)} *)

(* CSS Easing Module Level 1, section 2 (The easing functions): the named
   keywords [linear], [ease], [ease-in], [ease-out], [ease-in-out] are
   spec-defined aliases for specific [<cubic-bezier>] / step-function values.
   Both Lightning CSS and cssnano canonicalize the cubic-bezier form to the
   named keyword when applicable. *)
let easing1_2_named_alias_canonicalization () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "cubic-bezier(0.25, 0.1, 0.25, 1) -> ease"
    (normalize ".x { transition: 0.3s ease }")
    (normalize ".x { transition: 0.3s cubic-bezier(0.25, 0.1, 0.25, 1) }");
  Alcotest.(check string)
    "steps(1, jump-start) -> step-start"
    (normalize ".x { transition: 0.3s step-start }")
    (normalize ".x { transition: 0.3s steps(1, jump-start) }")

let easing1_2_invalid_rejected () =
  Alcotest.(check bool)
    "cubic-bezier with two args is rejected" true
    (match
       Css.of_string ~strict:true
         ".x { transition: 0.3s cubic-bezier(0.5, 0.5) }"
     with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "steps with no args is rejected" true
    (match Css.of_string ~strict:true ".x { transition: 0.3s steps() }" with
    | Error _ -> true
    | _ -> false)

let fidelity_easing_preserved () =
  pretty_preserves ".x { transition: 0.3s ease }" [ "ease" ];
  pretty_preserves ".x { transition: 0.3s linear }" [ "linear" ];
  pretty_preserves ".x { transition: 0.3s cubic-bezier(0.25, 0.1, 0.25, 1) }"
    [ "cubic-bezier(0.25, 0.1, 0.25, 1)" ]

(* {2 Background shorthand (CSS Backgrounds L3 sec. 2.1)} *)

(* CSS Backgrounds and Borders L3, section 2.1 (background shorthand): the
   shorthand expands to several longhands. Default values ([background-position]
   = [0% 0%], [background-repeat] = [repeat], etc.) may be elided in the
   serialized form. Both minifiers drop default position [0% 0%] when no other
   components require it; per shortest- wins cascade picks the elided form. *)
let bg321_default_pos_elision () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "background: red 0% 0% -> background: red" ".x{background:red}"
    (normalize ".x { background: red 0% 0% }")

let bg321_multi_layer_kept () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  (* Sec. 2.10 spells the shorthand [<bg-layer>#? , <final-bg-layer>], and only
     the final layer carries a [<background-color>]: sec. 2.1 paints the colour
     once below every layer rather than per layer. So the colour goes LAST, and
     a colour in an earlier layer is no background at all. Chrome 153 agrees: it
     drops [background: red, url(x.png)] and keeps the reverse. *)
  Alcotest.(check bool)
    "multi-layer background preserves both layers" true
    (let out = normalize ".x { background: url(x.png), red }" in
     Astring.String.is_infix ~affix:"red" out
     && Astring.String.is_infix ~affix:"url(x.png)" out);
  Alcotest.(check bool)
    "a colour in an earlier layer is dropped" true
    (match Css.of_string ~strict:true ".x { background: red, url(x.png) }" with
    | Ok _ -> false
    | Error _ -> true)

let fidelity_background_preserved () =
  pretty_preserves ".x { background: red 0% 0% }" [ "0% 0%" ];
  pretty_preserves ".x { background: red 50% 50% / cover no-repeat }"
    [ "50% 50%"; "cover"; "no-repeat" ];
  (* The colour belongs to the final layer (sec. 2.10), so the round trip is
     asked of the spelling that has one. *)
  pretty_preserves ".x { background: url(x.png), red }" [ "red"; "url(x.png)" ]

(* {2 Strings and escapes (CSS Syntax L3 sec. 4.3.7)} *)

(* CSS Syntax L3, section 4.3.7 (Consume an escaped code point): hex escapes
   [\41] decode to the Unicode code point [U+0041 = 'A']. The trailing
   whitespace after a 1-6 digit escape is consumed. The decoded string is
   shorter than the escape; Lightning CSS decodes printable ASCII characters
   where safe. *)
let s3437_string_escape () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "\\41 in string decodes to A" ".x{content:\"A\"}"
    (normalize ".x { content: \"\\41\" }");
  Alcotest.(check string)
    "\\41 followed by space decodes to A" ".x{content:\"A\"}"
    (normalize ".x { content: \"\\41 \" }");
  Alcotest.(check string)
    "\\\\ preserved" ".x{content:\"\\\\\"}"
    (normalize ".x { content: \"\\\\\" }")

(* CSS Syntax 3 sec. 4.3.7 reads an escape as the code point it names, so a
   custom-property name can hold a [;] or a [}]. Serializing an ident writes
   those back escaped (CSS Syntax 3 sec. 2.1): printed raw, the name ends its
   own declaration or closes the rule around it, and cascade's reader stops
   reading what the input meant. *)
(* Print [css] and hold the printer to its own reader: the output parses in
   strict mode, says what the input said, and printing it again is a byte
   fixpoint. The tree comparison stands in for AST equality, which a token
   carrying its source position makes sensitive to how long the escape was.
   Returns the minified output, which is what a caller's spellings are checked
   against. *)
let escape_roundtrip css =
  match Css.of_string ~strict:false css with
  | Error e ->
      Alcotest.failf "parse failed: %s (%s)" css (Cascade.Error.to_string e)
  | Ok parsed ->
      let check ~minify label out =
        match Css.of_string ~strict:true out with
        | Error e ->
            Alcotest.failf "%s output is not readable: %s (%s)" label out
              (Cascade.Error.to_string e)
        | Ok again ->
            Alcotest.(check bool)
              (String.concat "" [ label; " says what "; css; " said" ])
              true
              (Cascade_diff.Css_compare.equal ~mode:`Tree css out);
            Alcotest.(check string)
              (String.concat "" [ label; " is a byte fixpoint: "; out ])
              out
              (Css.to_string ~minify again.Css.stylesheet)
      in
      let minified = Css.to_string ~minify:true parsed.Css.stylesheet in
      check ~minify:true "minified" minified;
      check ~minify:false "pretty" (Css.to_string parsed.Css.stylesheet);
      minified

let check_escape_roundtrips cases =
  List.iter
    (fun (css, expected) ->
      Alcotest.(check string) css expected (escape_roundtrip css))
    cases

let s4370_custom_property_name_escapes () =
  check_escape_roundtrips
    [
      (* The declaration name. *)
      (":root{--x\\3b y:red}", ":root{--x\\;y:red}");
      (":root{--x\\7d y:red}", ":root{--x\\}y:red}");
      (":root{--x\\7b y:red}", ":root{--x\\{y:red}");
      (":root{--x\\3a y:red}", ":root{--x\\:y:red}");
      (":root{--x\\ y:red}", ":root{--x\\ y:red}");
      (":root{--x\\22 y:red}", ":root{--x\\\"y:red}");
      (":root{--x\\\\y:red}", ":root{--x\\\\y:red}");
      (* The [var()] reference name, with each shape of fallback. *)
      (".a{color:var(--x\\7d y)}", ".a{color:var(--x\\}y)}");
      (".a{color:var(--a\\3b b,red)}", ".a{color:var(--a\\;b,red)}");
      ( ".a{color:var(--a\\3b b,var(--c\\3b d))}",
        ".a{color:var(--a\\;b,var(--c\\;d))}" );
      (".a{color:var(--a\\3b b,)}", ".a{color:var(--a\\;b,)}");
      (* The [@property] prelude and the [style()] container query. *)
      ( "@property --x\\3b y{syntax:\"*\";inherits:false}",
        "@property --x\\;y{syntax:\"*\";inherits:false}" );
      ( "@container style(--x\\3b y:red){.a{color:red}}",
        "@container style(--x\\;y:red){.a{color:red}}" );
      (* A name needing no escape keeps its spelling: [--] takes a digit
         straight after it, and a code point CSS Syntax 3 sec. 4.2 admits in an
         ident is written as itself. *)
      (":root{--x:red}.a{color:var(--x)}", ":root{--x:red}.a{color:var(--x)}");
      (":root{--0:red}.a{color:var(--0)}", ":root{--0:red}.a{color:var(--0)}");
      ( ":root{--\\e9 x:red}.a{color:var(--\\e9 x)}",
        ":root{--\xc3\xa9x:red}.a{color:var(--\xc3\xa9x)}" );
    ]

(* CSS Syntax 3 sec. 4.3.7 reads an escape as the code point it names, so any
   ident cascade stores can hold a [;], a [}] or another code point CSS Syntax 3
   sec. 4.2 keeps out of an ident. Serializing an ident writes those back
   escaped (CSS Syntax 3 sec. 2.1), and every prelude below names something with
   a [<custom-ident>] or a [<dashed-ident>]: printed raw the name ends the
   at-rule or closes the block around it. *)
let s4370_at_rule_prelude_name_escapes () =
  check_escape_roundtrips
    [
      (* CSS Cascade 5 sec. 6.4.1: a [<layer-name>] is [<ident> ['.' <ident>]*],
         so each dot-separated part takes the escapes and the [.] separators
         stay bare. *)
      ("@layer a\\3b b;", "@layer a\\;b;");
      ("@layer a\\3b b{.x{color:red}}", "@layer a\\;b{.x{color:red}}");
      ("@layer a\\3b b,c\\3b d;", "@layer a\\;b,c\\;d;");
      ("@layer a.b\\3b c;", "@layer a.b\\;c;");
      ("@import\"a.css\"layer(l\\3b x);", "@import\"a.css\"layer(l\\;x);");
      (* CSS Counter Styles 3 sec. 2 [@counter-style <counter-style-name>], and
         sec. 3.1.4 [system: extends <counter-style-name>]. *)
      ( "@counter-style a\\3b b{system:cyclic;symbols:\"x\";suffix:\" \"}",
        "@counter-style a\\;b{system:cyclic;symbols:\"x\";suffix:\" \"}" );
      ( "@counter-style c{system:extends d\\3b y}",
        "@counter-style c{system:extends d\\;y}" );
      (* CSS Anchor Positioning 1 sec. 4.2 [@position-try <dashed-ident>] and
         CSS Fonts 4 sec. 11.1 [@font-palette-values <dashed-ident>]. *)
      ("@position-try --x\\3b y{top:0}", "@position-try --x\\;y{top:0}");
      ( "@font-palette-values --x\\3b y{font-family:Foo;base-palette:1}",
        "@font-palette-values --x\\;y{font-family:Foo;base-palette:1}" );
      (* CSS Containment 3 sec. 5.2: [@container] names a [<custom-ident>], and
         CSS Fonts 4 sec. 6.5 names a feature value the same way. *)
      ( "@container n\\3b m (width>10px){.a{color:red}}",
        "@container n\\;m (width>10px){.a{color:red}}" );
      ( "@font-feature-values Foo{@styleset{s\\3b x:1}}",
        "@font-feature-values Foo{@styleset{s\\;x:1}}" );
      (* A name needing no escape keeps its spelling, and the leading-digit rule
         of CSS Syntax 3 sec. 4.3.11 still applies. *)
      ("@layer a.b{.x{color:red}}", "@layer a.b{.x{color:red}}");
      ( "@counter-style \\31 a{system:cyclic;symbols:\"x\";suffix:\" \"}",
        "@counter-style \\31 a{system:cyclic;symbols:\"x\";suffix:\" \"}" );
      ( "@counter-style \\e9 x{system:cyclic;symbols:\"x\";suffix:\" \"}",
        "@counter-style \xc3\xa9x{system:cyclic;symbols:\"x\";suffix:\" \"}" );
    ]

(* The same rule for a name a declaration's value carries: printed raw it ends
   its own declaration, so it is written with the escapes CSS Syntax 3 sec.
   4.3.7 reads back as the same name. *)
let s4370_property_value_name_escapes () =
  check_escape_roundtrips
    [
      (* CSS Anchor Positioning 1 sec. 2.1 / 3.1 / 4.1 name
         [<dashed-ident>]s. *)
      (".a{anchor-name:--a\\3b b}", ".a{anchor-name:--a\\;b}");
      (".a{position-anchor:--p\\3b q}", ".a{position-anchor:--p\\;q}");
      ( ".a{position-try-fallbacks:--f\\3b g}",
        ".a{position-try-fallbacks:--f\\;g}" );
      (* CSS Scroll Animations 1 sec. 2.1 / 3.1 / 4 name [<dashed-ident>]s, and
         CSS Fonts 4 sec. 2.7 [font-palette] takes one. *)
      (".a{animation-timeline:--t\\3b u}", ".a{animation-timeline:--t\\;u}");
      (".a{scroll-timeline-name:--s\\3b t}", ".a{scroll-timeline-name:--s\\;t}");
      (".a{view-timeline-name:--v\\3b w}", ".a{view-timeline-name:--v\\;w}");
      ( ".a{scroll-timeline:--s\\3b t block}",
        ".a{scroll-timeline:--s\\;t block}" );
      (".a{timeline-scope:--z\\3b y}", ".a{timeline-scope:--z\\;y}");
      (".a{font-palette:--f\\3b g}", ".a{font-palette:--f\\;g}");
      (* CSS Transitions 1 sec. 2.1: [transition-property] names a property, and
         a custom property is a [<dashed-ident>]. *)
      (".a{transition-property:--t\\3b x}", ".a{transition-property:--t\\;x}");
      (* [<custom-ident>] names: CSS Containment 3 sec. 3.1, CSS Animations 1
         sec. 4.1, CSS View Transitions 1 sec. 3.1 and 2 sec. 3.2, and CSS Will
         Change 1 sec. 2. *)
      (".a{container-name:c\\3b d}", ".a{container-name:c\\;d}");
      (".a{animation-name:n\\3b m}", ".a{animation-name:n\\;m}");
      (".a{view-transition-name:v\\3b w}", ".a{view-transition-name:v\\;w}");
      (".a{view-transition-class:c\\3b d}", ".a{view-transition-class:c\\;d}");
      (".a{will-change:w\\3b x}", ".a{will-change:w\\;x}");
      (* CSS Lists 3 sec. 3 names a counter with a [<custom-ident>], both where
         it is set and where [counter()] reads it, and sec. 4 names a string the
         same way. *)
      (".a{counter-reset:c\\3b d 1}", ".a{counter-reset:c\\;d 1}");
      (".a{counter-increment:c\\3b d 1}", ".a{counter-increment:c\\;d 1}");
      (".a{content:counter(c\\3b d)}", ".a{content:counter(c\\;d)}");
      (".a{content:string(s\\3b t)}", ".a{content:string(s\\;t)}");
      (* CSS Grid 2 sec. 8.1: a grid line is named by a [<custom-ident>], in a
         placement property and in a track list. *)
      (".a{grid-area:g\\3b h}", ".a{grid-area:g\\;h}");
      (".a{grid-row-start:g\\3b h}", ".a{grid-row-start:g\\;h}");
      (".a{grid-row-start:span g\\3b h}", ".a{grid-row-start:span g\\;h}");
      ( ".a{grid-template-columns:[l\\3b m]1fr}",
        ".a{grid-template-columns:[l\\;m]1fr}" );
      (* A name needing no escape keeps its spelling. *)
      (".a{grid-area:g}", ".a{grid-area:g}");
      (".a{anchor-name:--a}", ".a{anchor-name:--a}");
    ]

(* CSS Cascade 5 sec. 6.4.1: a [<layer-name>] is [<ident> ['.' <ident>]*], so a
   dot inside an ident and a dot between two idents name different layers.
   [@layer a\2e b] is one layer, [@layer a.b] is the sublayer [b] of [a], and
   the printed name says which: a dot an ident carries is written back escaped
   (CSS Syntax 3 sec. 2.1), the separators stay bare. *)
let s641_layer_name_parts () =
  check_escape_roundtrips
    [
      (* One ident holding a dot, against the two idents it would pass for. *)
      ("@layer a\\2e b;", "@layer a\\.b;");
      ("@layer a.b;", "@layer a.b;");
      ("@layer a\\2e b{.x{color:red}}", "@layer a\\.b{.x{color:red}}");
      ("@layer a.b{.x{color:red}}", "@layer a.b{.x{color:red}}");
      ("@layer a\\2e b,a.b;", "@layer a\\.b,a.b;");
      (* A sublayer of the layer named [a.b], and the layer named [a.b.c]. *)
      ("@layer a\\2e b.c;", "@layer a\\.b.c;");
      ("@layer a\\2e b\\2e c;", "@layer a\\.b\\.c;");
      (* The [@import] prelude names a layer the same way. *)
      ("@import\"a.css\"layer(a\\2e b);", "@import\"a.css\"layer(a\\.b);");
      ("@import\"a.css\"layer(a.b);", "@import\"a.css\"layer(a.b);");
      (* Each part still takes the escaping of every other code point. *)
      ("@layer a\\3b b.c\\3b d;", "@layer a\\;b.c\\;d;");
    ]

(* CSS Conditional 3 sec. 6: a [<supports-decl>] holds a declaration, and a
   custom property's name can carry a [;] or a [}] through an escape (CSS Syntax
   3 sec. 4.3.7). The condition is a capability predicate for that exact name,
   so it survives the round trip with the escapes that read it back. *)
let s4370_supports_property_name_escapes () =
  check_escape_roundtrips
    [
      (* The name a [<supports-decl>] tests, in each shape the feature takes: a
         declaration, an empty value, and an operand of [and]. *)
      ( "@supports (--x\\3b y:red){.a{color:red}}",
        "@supports(--x\\;y:red){.a{color:red}}" );
      ( "@supports (--x\\7d y:red){.a{color:red}}",
        "@supports(--x\\}y:red){.a{color:red}}" );
      ( "@supports (--x\\3b y:){.a{color:red}}",
        "@supports(--x\\;y:){.a{color:red}}" );
      ( "@supports (--x\\3b y:red) and (color:red){.a{color:red}}",
        "@supports(--x\\;y:red)and (color:red){.a{color:red}}" );
      ( "@supports not (--x\\3b y:red){.a{color:red}}",
        "@supports not (--x\\;y:red){.a{color:red}}" );
      (* Sec. 7.4 allows a token stream simplification and forbids a logical
         one. An escape decodes to the same [<ident-token>] in every conformant
         implementation, so minified output may take the shortest spelling of a
         name however the author wrote it, and the name of a typed property
         answers to that as a custom property's does. What the author wrote
         survives unminified; see the pretty check below. *)
      ( "@supports (colo\\r:green){.a{color:red}}",
        "@supports(color:green){.a{color:red}}" );
      ("@supports (colo\\r:){.a{color:red}}", "@supports(color:){.a{color:red}}");
      ( "@supports (unknown\\-prop:x){.a{color:red}}",
        "@supports(unknown-prop:x){.a{color:red}}" );
      (* Both halves take their canonical spelling under minify, and each is
         independent of the other. *)
      ( "@supports (colo\\r:gre\\en){.a{color:red}}",
        "@supports(color:gre\\E n){.a{color:red}}" );
      ( "@supports (color:gre\\en){.a{color:red}}",
        "@supports(color:gre\\E n){.a{color:red}}" );
      (* A name needing no escape keeps its spelling. *)
      ( "@supports (--xy:red){.a{color:red}}",
        "@supports(--xy:red){.a{color:red}}" );
      ( "@supports (color:red){.a{color:red}}",
        "@supports(color:red){.a{color:red}}" );
    ]

(* Sec. 7.4 has [conditionText] return "the condition that was specified", and
   the token stream simplifications it allows are permitted rather than
   required, so unminified output writes back the bytes the author wrote.
   Measured on Chrome 153: conditionText echoes those same bytes, normalising
   neither [colo\r] to [color] nor [--x\3b y] to [--x\;y]. *)
let s4370_supports_property_name_pretty_verbatim () =
  List.iter
    (fun (css, fragment) -> pretty_preserves css [ fragment ])
    [
      ("@supports (colo\\r:green){.a{color:red}}", "colo\\r");
      ("@supports (colo\\r:){.a{color:red}}", "colo\\r");
      ("@supports (--x\\3b y:red){.a{color:red}}", "--x\\3b y");
      ("@supports (unknown\\-prop:x){.a{color:red}}", "unknown\\-prop");
      ("@supports not (colo\\r:green){.a{color:red}}", "colo\\r");
      (* The value half keeps the author's spelling on the same terms. *)
      ("@supports (color:gre\\en){.a{color:red}}", "gre\\en");
      ("@supports (colo\\r:gre\\en){.a{color:red}}", "gre\\en");
    ]

let fidelity_string_escape_preserved () =
  pretty_preserves ".x { content: \"\\41\" }" [ "\\41" ];
  pretty_preserves ".x { content: \"hello\" }" [ "hello" ];
  pretty_preserves ".x { content: \"\\\\\" }" [ "\\\\" ]

(* {2 Color function spaces (CSS Color L4)} *)

(* CSS Color Module Level 4, sections 10-13 (Color spaces): static in-gamut
   colors may fold to the shortest sRGB spelling under minify. Forms Cascade
   cannot reduce without losing information, such as out-of-gamut colors or
   missing components, stay in functional notation. *)
let color4_10_color_space_preserved () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "static lab folds to shortest sRGB form" ".x{color:red}"
    (normalize ".x { color: lab(54.29 80.81 69.89) }");
  Alcotest.(check bool)
    "out-of-gamut oklch preserved" true
    (Astring.String.is_infix ~affix:"oklch"
       (normalize ".x { color: oklch(0.628 0.258 29.23) }"));
  Alcotest.(check bool)
    "none channel preserved" true
    (Astring.String.is_infix ~affix:"color(display-p3 none"
       (normalize ".x { color: color(display-p3 none 0.5 1) }"))

let color4_invalid_color_function_rejected () =
  Alcotest.(check bool)
    "oklch with no args is rejected" true
    (match Css.of_string ~strict:true ".x { color: oklch() }" with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "color() with no space is rejected" true
    (match Css.of_string ~strict:true ".x { color: color(1 0 0) }" with
    | Error _ -> true
    | _ -> false)

let fidelity_color_space_preserved () =
  pretty_preserves ".x { color: oklch(0.628 0.258 29.23) }" [ ".258"; "29.23" ];
  pretty_preserves ".x { color: lab(54.29 80.81 69.89) }" [ "lab" ];
  pretty_preserves ".x { color: color(srgb 1 0 0) }" [ "color(srgb 1 0 0)" ];
  (* Pretty keeps oklab/oklch coefficients in full so the value round-trips,
     matching the [color()] function. *)
  pretty_preserves ".x { color: oklab(21% -0.00316127 -0.0338527 / 0.1) }"
    [ "-.00316127"; "-.0338527" ];
  (* The minified printer keeps them too: rounding a coefficient changes the
     colour, so it is a lossy fold that belongs in [optimize], not in [pp]. A
     consumer that serialises a typed colour with [to_string ~minify:true] (no
     optimize collapse) must get the authored value, matching Tailwind. *)
  let minify_pp css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> Css.to_string ~minify:true parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let oklab =
    minify_pp ".x { color: oklab(21% -0.00316127 -0.0338527 / 0.1) }"
  in
  Alcotest.(check bool)
    "minify pp keeps full oklab precision" true
    (Astring.String.is_infix ~affix:"-.00316127" oklab
    && Astring.String.is_infix ~affix:"-.0338527" oklab);
  let oklch = minify_pp ".x { color: oklch(0.628 0.2584567 29.23) }" in
  Alcotest.(check bool)
    "minify pp keeps full oklch chroma" true
    (Astring.String.is_infix ~affix:".2584567" oklch);
  (* The hue is a coefficient too: at high chroma a rounded hue shifts the
     colour, so the printer keeps it in full and [optimize] does the
     rounding. *)
  let hue = minify_pp ".x { color: oklch(0.628 0.2 264.123456) }" in
  Alcotest.(check bool)
    "minify pp keeps full oklch hue" true
    (Astring.String.is_infix ~affix:"264.123456" hue)

(* {2 @supports (CSS Conditional L4 sec. 2)} *)

(* CSS Conditional 3 sec. 6.1: a feature query asks the browser rendering the
   sheet whether it supports a declaration, so the answer belongs to that
   browser and the guard stays whatever shape the condition takes. Chrome
   answers false to both [(-webkit-hyphens: none)] and [(-moz-orient: inline)];
   a UA older than custom properties answers false to [(--x: y)], which sec. 6.1
   calls supported. *)
let conditional4_2_supports_guard_kept () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed ->
        parsed.stylesheet |> Css.optimize |> Css.to_string ~minify:true
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "a prefixed feature query keeps its guard"
    "@supports(-webkit-hyphens:none){.x{color:red}}"
    (normalize "@supports (-webkit-hyphens: none) { .x { color: red } }");
  Alcotest.(check string)
    "a negated prefixed feature query keeps its rule"
    "@supports not (-webkit-hyphens:none){.x{color:red}}"
    (normalize "@supports not (-webkit-hyphens: none) { .x { color: red } }");
  Alcotest.(check string)
    "a prefixed property with a prefixed value keeps its guard"
    "@supports(-webkit-appearance:-apple-pay-button){.x{color:red}}"
    (normalize
       "@supports (-webkit-appearance: -apple-pay-button) { .x { color: red } }");
  Alcotest.(check bool)
    "a prefixed conjunct survives a mixed condition" true
    (Astring.String.is_infix ~affix:"-webkit-hyphens"
       (normalize
          "@supports ((-webkit-hyphens: none) and (not (margin-trim: inline))) \
           { .x { color: red } }"));
  (* A conjunction is one condition, not two facts to settle separately.
     Dropping the conjunct a support table calls always-true rewrites the
     question the author asked the browser. *)
  Alcotest.(check string)
    "a mixed conjunction keeps both conjuncts"
    "@supports(display:grid)and (text-wrap:balance){.x{text-wrap:balance}}"
    (normalize
       "@supports (display: grid) and (text-wrap: balance) { .x { text-wrap: \
        balance } }");
  Alcotest.(check string)
    "a custom property guard is kept" "@supports(--x:y){.x{color:red}}"
    (normalize "@supports (--x: y) { .x { color: red } }");
  Alcotest.(check string)
    "an unprefixed widely-available guard is kept"
    "@supports(display:grid){.x{display:grid}}"
    (normalize "@supports (display: grid) { .x { display: grid } }")

(* CSS Conditional Rules Module Level 4, section 2 (The @supports rule): the
   rule body parses as a rule list (like a stylesheet) and round- trips
   preserved. The supports-condition grammar accepts declarations, [not], [and],
   [or], [selector()], and [font-tech()]. *)
let conditional4_2_supports_preserved () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed ->
        parsed.stylesheet |> Css.optimize |> Css.to_string ~minify:true
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let grid = "@supports (display: grid) { .x { display: grid } }" in
  Alcotest.(check string)
    "a feature query round-trips" "@supports(display:grid){.x{display:grid}}"
    (normalize grid);
  let not_grid = "@supports not (display: grid) { .x { display: block } }" in
  Alcotest.(check string)
    "a negated feature query round-trips"
    "@supports not (display:grid){.x{display:block}}" (normalize not_grid);
  Alcotest.(check bool)
    "@supports selector(:has(img)) preserved" true
    (Astring.String.is_infix ~affix:"selector(:has(img))"
       (normalize "@supports selector(:has(img)) { .x { color: red } }"));
  Alcotest.(check bool)
    "a boolean condition keeps its operator" true
    (Astring.String.is_infix ~affix:"and"
       (normalize
          "@supports (display: grid) and (color: red) { .x { color: red } }"))

let conditional4_2_supports_invalid_rejected () =
  Alcotest.(check bool)
    "@supports with bare keyword rejected" true
    (match
       Css.of_string ~strict:true "@supports invalid { .x { color: red } }"
     with
    | Ok parsed ->
        (* Bad supports condition is dropped; rule body may or may not survive
           depending on recovery mode. Use the partial parser. *)
        let { Css.warnings; _ } =
          match
            Css.of_string ~strict:false
              "@supports invalid { .x { color: red } }"
          with
          | Ok parsed -> parsed
          | Error err ->
              Alcotest.failf "lenient @supports recovery failed: %s"
                (Cascade.Error.to_string err)
        in
        List.length warnings >= 1
        || not
             (Astring.String.is_infix ~affix:"@supports"
                (minify parsed.stylesheet))
    | Error _ -> true)

let fidelity_supports_preserved () =
  pretty_preserves "@supports (display: grid) { .x { display: grid } }"
    [ "@supports"; "display: grid" ];
  pretty_preserves "@supports not (display: grid) { .x { display: block } }"
    [ "not" ]

(* {2 Logical Properties (CSS Logical L1)} *)

(* CSS Logical Properties and Values Module Level 1, section 3 (Flow- relative
   box model): logical properties [inline-size], [block-size],
   [margin-inline-start], [padding-block-end], [inset-inline], etc. round-trip
   preserved. The mapping to physical properties is computed-time (depends on
   writing-mode), so the syntax layer keeps the logical spelling. *)
let logical1_3_logical_property_preserved () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "inline-size preserved" ".x{inline-size:100px}"
    (normalize ".x { inline-size: 100px }");
  Alcotest.(check string)
    "block-size preserved" ".x{block-size:50px}"
    (normalize ".x { block-size: 50px }");
  Alcotest.(check string)
    "margin-inline-start preserved" ".x{margin-inline-start:10px}"
    (normalize ".x { margin-inline-start: 10px }");
  Alcotest.(check string)
    "margin-inline two-value shorthand preserved" ".x{margin-inline:10px 20px}"
    (normalize ".x { margin-inline: 10px 20px }");
  Alcotest.(check string)
    "padding-block two-value shorthand preserved" ".x{padding-block:5px 10px}"
    (normalize ".x { padding-block: 5px 10px }");
  Alcotest.(check string)
    "inset preserved" ".x{inset:0}"
    (normalize ".x { inset: 0 }");
  Alcotest.(check string)
    "inset-inline preserved" ".x{inset-inline:10px}"
    (normalize ".x { inset-inline: 10px }")

let fidelity_logical_property_preserved () =
  pretty_preserves ".x { inline-size: 100px }" [ "inline-size" ];
  pretty_preserves ".x { margin-inline-start: 10px }" [ "margin-inline-start" ];
  pretty_preserves ".x { padding-block: 5px 10px }" [ "padding-block" ]

(* {2 Container Queries (CSS Containment L3 sec. 4.4)} *)

(* CSS Containment Module Level 3, section 4.4 (Container Queries): the
   [@container] rule supports an optional name and a query expression. Both
   forms round-trip preserved, including the named form [@container card
   (...)]. *)
let containment3_6_container_query_preserved () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check bool)
    "@container with min-width preserved" true
    (Astring.String.is_infix ~affix:"@container"
       (normalize "@container (min-width: 400px) { .x { color: red } }"));
  Alcotest.(check bool)
    "@container with name preserved" true
    (Astring.String.is_infix ~affix:"card"
       (normalize "@container card (min-width: 400px) { .x { color: red } }"));
  Alcotest.(check bool)
    "@container with inline-size range preserved" true
    (Astring.String.is_infix ~affix:"inline-size"
       (normalize "@container card (inline-size > 30em) { .x { color: red } }"))

let fidelity_container_query_preserved () =
  pretty_preserves "@container (min-width: 400px) { .x { color: red } }"
    [ "@container"; "min-width: 400px" ];
  pretty_preserves "@container card (inline-size > 30em) { .x { color: red } }"
    [ "card"; "inline-size > 30em" ]

(* {2 Font shorthand (CSS Fonts L4 sec. 2.7)} *)

(* CSS Fonts Module Level 4, section 2.7 (Shorthand font property): the [font]
   shorthand expands to several longhands; the keyword [bold] inside the
   shorthand canonicalizes to [700] under minify per sec. 2.2. System font
   keywords ([caption], [icon], [menu]) round-trip preserved. *)
let fonts4_6_5_font_shorthand () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "font: 16px Arial preserved" ".x{font:16px Arial}"
    (normalize ".x { font: 16px Arial }");
  Alcotest.(check bool)
    "font with bold canonicalizes to 700" true
    (Astring.String.is_infix ~affix:"700"
       (normalize ".x { font: italic small-caps bold 16px/1.2 sans-serif }"));
  Alcotest.(check string)
    "font: caption preserved" ".x{font:caption}"
    (normalize ".x { font: caption }");
  Alcotest.(check string)
    "font: icon preserved" ".x{font:icon}"
    (normalize ".x { font: icon }")

let fonts4_invalid_font_rejected () =
  Alcotest.(check bool)
    "font with no family rejected" true
    (match Css.of_string ~strict:true ".x { font: 16px }" with
    | Error _ -> true
    | _ -> false)

let fidelity_font_shorthand_preserved () =
  pretty_preserves ".x { font: 16px Arial }" [ "16px Arial" ];
  pretty_preserves ".x { font: italic small-caps bold 16px/1.2 sans-serif }"
    [ "italic"; "small-caps"; "bold" ];
  pretty_preserves ".x { font: caption }" [ "caption" ]

(* {2 List-style shorthand (CSS Lists L3 sec. 3.6)} *)

(* CSS Lists Module Level 3, section 3.6 (list-style shorthand): the shorthand
   sets [list-style-type], [list-style-position], and [list- style-image].
   Default values may be elided per shortest-wins. *)
let lists3_list_style_shorthand () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "list-style: none preserved" ".x{list-style:none}"
    (normalize ".x { list-style: none }");
  Alcotest.(check string)
    "list-style-type: none preserved" ".x{list-style-type:none}"
    (normalize ".x { list-style-type: none }");
  Alcotest.(check string)
    "list-style-position: inside preserved" ".x{list-style-position:inside}"
    (normalize ".x { list-style-position: inside }")

let fidelity_list_style_preserved () =
  pretty_preserves ".x { list-style: none }" [ "none" ];
  pretty_preserves ".x { list-style: disc inside }" [ "disc"; "inside" ];
  pretty_preserves ".x { list-style-type: none }" [ "list-style-type" ]

(* {2 Cascade origin + !important interaction (Cascade L5 sec. 6.3)} *)

(* CSS Cascading and Inheritance Module Level 5, section 6.3 (Importance): for
   important declarations the origin precedence is inverted - user-agent
   !important > user !important > author !important. The cascade test surface
   validates that important declarations preserve their authored !important
   suffix. *)
let c6_3_important_serialization_preserved () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "!important suffix preserved" ".x{color:red!important}"
    (normalize ".x { color: red !important }");
  Alcotest.(check string)
    "!important with whitespace canonicalized to no-space form"
    ".x{color:red!important}"
    (normalize ".x { color: red ! important }")

let fidelity_important_preserved () =
  pretty_preserves ".x { color: red !important }" [ "!important" ]

let suite =
  ( "stylesheet_families",
    [
      ( "spec easing 1 2 named alias",
        `Quick,
        easing1_2_named_alias_canonicalization );
      ("spec easing 1 2 invalid (negative)", `Quick, easing1_2_invalid_rejected);
      ("fidelity easing preserved", `Quick, fidelity_easing_preserved);
      ( "spec bg 3 2.1 default position elision",
        `Quick,
        bg321_default_pos_elision );
      ("spec bg 3 2.1 multi-layer preserved", `Quick, bg321_multi_layer_kept);
      ("fidelity background preserved", `Quick, fidelity_background_preserved);
      ("spec syntax 3 4.3.7 string escape decoding", `Quick, s3437_string_escape);
      ( "spec syntax 3 4.3.7 custom-property name escapes",
        `Quick,
        s4370_custom_property_name_escapes );
      ( "spec syntax 3 4.3.7 at-rule prelude name escapes",
        `Quick,
        s4370_at_rule_prelude_name_escapes );
      ( "spec syntax 3 4.3.7 property value name escapes",
        `Quick,
        s4370_property_value_name_escapes );
      ( "spec conditional 3 2.2 supports property name escapes",
        `Quick,
        s4370_supports_property_name_escapes );
      ( "spec conditional 3 7.4 supports property name pretty verbatim",
        `Quick,
        s4370_supports_property_name_pretty_verbatim );
      ("spec cascade 5 6.4.1 layer name parts", `Quick, s641_layer_name_parts);
      ( "fidelity string escape preserved",
        `Quick,
        fidelity_string_escape_preserved );
      ( "spec color 4 10 color space preserved",
        `Quick,
        color4_10_color_space_preserved );
      ( "spec color 4 invalid color function rejected (negative)",
        `Quick,
        color4_invalid_color_function_rejected );
      ("fidelity color space preserved", `Quick, fidelity_color_space_preserved);
      ( "spec conditional 4 2 supports preserved",
        `Quick,
        conditional4_2_supports_preserved );
      ( "spec conditional 4 2 author guard kept",
        `Quick,
        conditional4_2_supports_guard_kept );
      ( "spec conditional 4 2 supports invalid (negative)",
        `Quick,
        conditional4_2_supports_invalid_rejected );
      ("fidelity supports preserved", `Quick, fidelity_supports_preserved);
      ( "spec logical 1 3 logical property preserved",
        `Quick,
        logical1_3_logical_property_preserved );
      ( "fidelity logical property preserved",
        `Quick,
        fidelity_logical_property_preserved );
      ( "spec containment 3 6 container query preserved",
        `Quick,
        containment3_6_container_query_preserved );
      ( "fidelity container query preserved",
        `Quick,
        fidelity_container_query_preserved );
      ("spec fonts 4 6.5 font shorthand", `Quick, fonts4_6_5_font_shorthand);
      ( "spec fonts 4 invalid font rejected (negative)",
        `Quick,
        fonts4_invalid_font_rejected );
      ( "fidelity font shorthand preserved",
        `Quick,
        fidelity_font_shorthand_preserved );
      ("spec lists 3 list-style shorthand", `Quick, lists3_list_style_shorthand);
      ("fidelity list-style preserved", `Quick, fidelity_list_style_preserved);
      ( "spec cascade 6.3 important serialization preserved",
        `Quick,
        c6_3_important_serialization_preserved );
      ("fidelity important preserved", `Quick, fidelity_important_preserved);
    ] )
