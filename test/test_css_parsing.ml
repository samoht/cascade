open Cascade
open Css
open Css_helpers

let public_parse_edges () =
  match of_string ~strict:true ~filename:"spec.css" ".a{color:rgb(300)}" with
  | Ok _ -> Alcotest.fail "strict parser accepted invalid declaration"
  | Error err ->
      let msg = Cascade.Error.to_string err in
      Alcotest.(check bool)
        "parse error carries filename" true
        (Astring.String.is_infix ~affix:"spec.css" msg);
      let parsed =
        match
          of_string ~strict:false ~filename:"spec.css"
            ".a{color:rgb(300)}.b{color:red}.c{color:rgb(301)}"
        with
        | Ok parsed -> parsed
        | Error err ->
            Alcotest.failf "lenient parse rejected recoverable CSS: %s"
              (Cascade.Error.to_string err)
      in
      let rules = rule_statements parsed.stylesheet in
      let declaration_counts =
        List.map
          (fun statement ->
            match as_rule statement with
            | Some (_, declarations, _) -> List.length declarations
            | None -> Alcotest.fail "expected a qualified rule")
          rules
      in
      Alcotest.(check (list int))
        "partial parser discards invalid declarations, not containing rules"
        [ 0; 1; 0 ] declaration_counts;
      Alcotest.(check int)
        "partial parser reports invalid rules" 2
        (List.length parsed.warnings)

(* A newline inside a string produces a <bad-string-token>. In an at-rule
   prelude that token used to serialize to nothing, so [@media <bad-string>]
   reached the media-condition reader as an empty condition: the rule kept its
   body but lost its condition, with no diagnostic. *)
let public_bad_string_prelude_edges () =
  match of_string "@media \"abc\n{ .a { color: red } }" with
  | Error err ->
      Alcotest.failf "lenient parse rejected recoverable CSS: %s"
        (Cascade.Error.to_string err)
  | Ok parsed ->
      Alcotest.(check bool)
        "the lost media condition is reported" true (parsed.warnings <> []);
      Alcotest.(check bool)
        "no unconditional @media is emitted" false
        (Astring.String.is_infix ~affix:"@media{" (minify parsed.stylesheet))

(* CSS Syntax 3 (ED) sec. 4.3.5 ends a <bad-string-token> at the newline, so the
   token carries an opening quote and nothing to close it. Sec. 7.2 keeps one
   out of a <declaration-value>, and CSS Custom Properties 1 sec. 2.1 repeats
   the exclusion for a custom property, so a declaration carrying one is
   invalid. Emitting it anyway hands the next reader a bare quote: the [}] the
   rule closes with becomes string content and everything after it is swallowed.
   Assert the round trip rather than the spelling, which is the property the
   output owes its own reader. *)
let public_bad_string_declaration_edges () =
  let parse label css =
    match of_string ~strict:false css with
    | Ok parsed -> parsed
    | Error err ->
        Alcotest.failf "%s: lenient parse rejected recoverable CSS: %s" label
          (Cascade.Error.to_string err)
  in
  let label_of parts = String.concat "" parts in
  let reparses ~survivor label css =
    let out = Css.to_string ~minify:true (parse label css).stylesheet in
    let reread = parse (label_of [ label; " (re-read)" ]) out in
    Alcotest.(check (list string))
      (label_of [ label; ": minified output re-reads clean" ])
      []
      (List.map Cascade.Error.to_string reread.warnings);
    Alcotest.(check string)
      (label_of [ label; ": minification is a fixed point" ])
      out
      (Css.to_string ~minify:true reread.stylesheet);
    Alcotest.(check bool)
      (label_of [ label; ": keeps "; survivor ])
      true
      (Astring.String.is_infix ~affix:survivor out)
  in
  reparses ~survivor:"b{color:red}" "bad string last in a rule"
    "a{--t:\"abc\n}b{color:red}";
  reparses ~survivor:"a{color:red}" "bad string before a later declaration"
    "a{--t:\"abc\n;color:red}";
  reparses ~survivor:"b{color:red}" "bad string last in a nested rule"
    "a{&{--t:\"abc\n}}b{color:red}";
  reparses ~survivor:"b{color:red}" "bad string last in a conditional group"
    "@media screen{a{--t:\"abc\n}b{color:red}}";
  reparses ~survivor:"color:red" "bad string last in a keyframe"
    "@keyframes k{from{--t:\"abc\n}to{color:red}}";
  reparses ~survivor:"color:red" "bad string last in @page"
    "@page{--t:\"abc\n;color:red}";
  (* Control: a closed string is a <declaration-value> and survives whole. *)
  reparses ~survivor:"--t:\"abc\"" "a closed string round-trips"
    "a{--t:\"abc\";color:red}b{color:red}"

(* CSS Syntax 3 sec. 5.4.2 "consume an at-rule" builds an at-rule node for any
   at-keyword, recognised or not; the block it consumes keeps its contents.
   Discarding an unrecognised at-rule is a user-agent cascade step (CSS 2.1 sec.
   4.2 "Rules for handling parsing errors"), not a serialisation step, and a
   transform cannot know which agent reads its output: an agent that later
   implements the name renders the input and a stripped output differently.
   Lightning CSS, esbuild, csso, clean-css and cssnano all keep the at-rule and
   its block. So [to_string] keeps it too, at every block depth, with a body or
   without one. [Optimize.drop_unknown_at_rules] stays available for a caller
   that does want user-agent-equivalent output. *)
(* CSS Values 4 sec. 4.1: "Keywords are identifiers and are interpreted ASCII
   case-insensitively (i.e., [a-z] and [A-Z] are equivalent)." A keyword the
   author capitalised is the same keyword, so it reads to the same node and
   prints as that node prints. *)
let spec_keyword_case_insensitive () =
  let render input =
    match of_string ~strict:false input with
    | Ok parsed -> to_string ~minify:true parsed.stylesheet
    | Error err ->
        Alcotest.failf "lenient parse rejected %S: %s" input
          (Cascade.Error.to_string err)
  in
  (* [expect] is the shortest spelling of the one node both inputs name. *)
  let pins name ~upper ~lower ~expect =
    Alcotest.(check string)
      (String.concat "" [ name; " (lower-case control)" ])
      expect (render lower);
    Alcotest.(check string)
      (String.concat "" [ name; " (capitalised keyword)" ])
      expect (render upper)
  in
  (* Where the printed spelling is a canonicalisation of cascade's own, the pin
     is that both spellings reach it, not what it is. *)
  let agrees name ~upper ~lower =
    let out = render lower in
    Alcotest.(check bool)
      (String.concat "" [ name; " (lower-case control parses)" ])
      true (out <> "");
    Alcotest.(check string)
      (String.concat "" [ name; " (capitalised keyword)" ])
      out (render upper)
  in
  (* CSS Grid 2 sec. 8.3: [span] is a keyword of [<grid-line>], so [SPAN 2] is
     the span of 2 tracks and not the line named [SPAN] on track 2. *)
  pins "grid-column span" ~upper:".a{grid-column:SPAN 2}"
    ~lower:".a{grid-column:span 2}" ~expect:".a{grid-column:span 2}";
  (* CSS Animations 1 sec. 4.1 makes [from] equivalent to [0%] and [to] to
     [100%]; the shorter of each pair is the printed form. *)
  pins "keyframe selector" ~upper:"@keyframes f{FROM{opacity:0}TO{opacity:1}}"
    ~lower:"@keyframes f{from{opacity:0}to{opacity:1}}"
    ~expect:"@keyframes f{0%{opacity:0}to{opacity:1}}";
  (* CSS Cascade 5 sec. 2.1 takes a [<url>] or a [<string>]; they name the same
     resource and the string is shorter. *)
  pins "@import url()" ~upper:"@import URL(\"reset.css\");"
    ~lower:"@import url(\"reset.css\");" ~expect:"@import\"reset.css\";";
  (* CSS Backgrounds 3 sec. 6.2: [inset] is a keyword of [<shadow>]. *)
  pins "box-shadow inset" ~upper:".a{box-shadow:INSET 1px 1px red}"
    ~lower:".a{box-shadow:inset 1px 1px red}"
    ~expect:".a{box-shadow:inset 1px 1px red}";
  agrees "nth-child of" ~upper:".a:nth-child(2n OF .item){color:red}"
    ~lower:".a:nth-child(2n of .item){color:red}";
  agrees "@import layer()" ~upper:"@import \"a.css\" LAYER(base);"
    ~lower:"@import \"a.css\" layer(base);";
  agrees "@property descriptors"
    ~upper:"@property --x{SYNTAX:\"<length>\";INHERITS:false;INITIAL-VALUE:0px}"
    ~lower:"@property --x{syntax:\"<length>\";inherits:false;initial-value:0px}";
  agrees "@font-face descriptors"
    ~upper:"@font-face{FONT-FAMILY:Foo;SRC:url(a.woff)}"
    ~lower:"@font-face{font-family:Foo;src:url(a.woff)}";
  agrees "gradient corner keywords"
    ~upper:".a{background:linear-gradient(TO RIGHT,red,blue)}"
    ~lower:".a{background:linear-gradient(to right,red,blue)}";
  agrees "@counter-style system"
    ~upper:"@counter-style c{system:FIXED;symbols:\"a\"}"
    ~lower:"@counter-style c{system:fixed;symbols:\"a\"}";
  (* CSS Color 4 sec. 5 names each colour function; the name is a keyword, so
     the capitalised spelling is the same function. *)
  agrees "rgb() function name" ~upper:".a{color:RGB(1,2,3)}"
    ~lower:".a{color:rgb(1,2,3)}";
  agrees "oklch() function name" ~upper:".a{color:OKLCH(50% .1 30)}"
    ~lower:".a{color:oklch(50% .1 30)}";
  agrees "light-dark() function name" ~upper:".a{color:LIGHT-DARK(red,blue)}"
    ~lower:".a{color:light-dark(red,blue)}";
  (* CSS Color 5 sec. 3: [in] introduces the optional
     [<color-interpolation-method>], so it is a keyword of the grammar. *)
  agrees "color-mix() interpolation method"
    ~upper:".a{color:color-mix(IN srgb,red,blue)}"
    ~lower:".a{color:color-mix(in srgb,red,blue)}";
  (* CSS Values 4 sec. 4.5.1 writes a [<url>] as the [url()] name over a
     [<string>], or as the url-token a URL with nothing to quote spells
     shorter. *)
  pins "background url()" ~upper:".a{background:URL(\"a.png\")}"
    ~lower:".a{background:url(\"a.png\")}" ~expect:".a{background:url(a.png)}";
  (* CSS Paged Media 3 sec. 4.3: a [<pseudo-page>] comes from the closed set
     [first | left | right | blank], so it is a keyword and not a page name. *)
  pins "@page pseudo-page" ~upper:"@page :FIRST{margin:1in}"
    ~lower:"@page :first{margin:1in}" ~expect:"@page:first{margin:1in}";
  (* The legacy shadow-piercing combinator spells one fixed ident between its
     two slashes, so that ident belongs to the combinator's syntax. *)
  pins "/deep/ combinator" ~upper:".a /DEEP/ .b{color:red}"
    ~lower:".a /deep/ .b{color:red}" ~expect:".a/deep/.b{color:red}";
  (* CSS Paged Media 3 sec. 5.2 names the sixteen margin boxes, so each name is
     a keyword of the [@page] body. *)
  pins "@page margin box" ~upper:"@page{@TOP-LEFT{content:\"x\"}}"
    ~lower:"@page{@top-left{content:\"x\"}}"
    ~expect:"@page{@top-left{content:\"x\"}}";
  (* An at-rule name is an identifier of the grammar, so sec. 4.1 reaches it
     too. The name beside it is the author's and sec. 4.2 keeps it. *)
  pins "@keyframes at-rule name" ~upper:"@KEYFRAMES Slide{FROM{opacity:0}}"
    ~lower:"@keyframes Slide{from{opacity:0}}"
    ~expect:"@keyframes Slide{0%{opacity:0}}";
  pins "@media at-rule name" ~upper:"@MEDIA screen{a{color:red}}"
    ~lower:"@media screen{a{color:red}}" ~expect:"@media screen{a{color:red}}";
  pins "@media nested in a style rule" ~upper:".a{@MEDIA screen{color:red}}"
    ~lower:".a{@media screen{color:red}}" ~expect:".a{@media screen{color:red}}";
  agrees "@font-face at-rule name"
    ~upper:"@Font-Face{font-family:F;src:url(a.woff)}"
    ~lower:"@font-face{font-family:F;src:url(a.woff)}";
  (* CSS Custom Properties 1 sec. 3 names the substitution function [var()];
     capitalising it names the same function, even where a typed reader still
     matched the function name as a bare string. *)
  agrees "display var()" ~upper:".a{display:VAR(--x)}"
    ~lower:".a{display:var(--x)}";
  agrees "animation-timeline var()" ~upper:".a{animation-timeline:VAR(--x)}"
    ~lower:".a{animation-timeline:var(--x)}";
  (* scroll-animations-1 sec. 3/4 name [scroll()] and [view()] as the timeline
     functions; sec. 4.1 folds a function name too. *)
  agrees "animation-timeline scroll()"
    ~upper:".a{animation-timeline:SCROLL(root block)}"
    ~lower:".a{animation-timeline:scroll(root block)}";
  agrees "animation-timeline view()" ~upper:".a{animation-timeline:VIEW(block)}"
    ~lower:".a{animation-timeline:view(block)}";
  agrees "transition shorthand var() property"
    ~upper:".a{transition:VAR(--x) 1s ease}"
    ~lower:".a{transition:var(--x) 1s ease}";
  agrees "box-shadow inset var()" ~upper:".a{box-shadow:inset VAR(--x)}"
    ~lower:".a{box-shadow:inset var(--x)}";
  (* Sibling gap: #620 folded [var()]'s name here but left [inset] itself, so a
     capitalised inset before [var()] still misses the typed reader. *)
  agrees "box-shadow INSET var()" ~upper:".a{box-shadow:INSET VAR(--x)}"
    ~lower:".a{box-shadow:inset var(--x)}";
  agrees "container shorthand var()" ~upper:".a{container:VAR(--x)}"
    ~lower:".a{container:var(--x)}";
  agrees "@font-face unicode-range var()"
    ~upper:"@font-face{font-family:F;src:url(a.woff);unicode-range:VAR(--x)}"
    ~lower:"@font-face{font-family:F;src:url(a.woff);unicode-range:var(--x)}";
  agrees "gradient position var()"
    ~upper:".a{background:linear-gradient(VAR(--dir),red,blue)}"
    ~lower:".a{background:linear-gradient(var(--dir),red,blue)}";
  (* CSS Color 5 sec. 9 spells one [<color-interpolation-method>] for every
     place it appears, so the colour space and the hue keyword fold in a
     gradient exactly as [color-mix()]'s own reader already folds them. *)
  agrees "gradient interpolation colour space"
    ~upper:".a{background:linear-gradient(to right IN OKLAB,red,blue)}"
    ~lower:".a{background:linear-gradient(to right in oklab,red,blue)}";
  agrees "gradient interpolation hue method"
    ~upper:".a{background:linear-gradient(IN OKLCH SHORTER HUE,red,blue)}"
    ~lower:".a{background:linear-gradient(in oklch shorter hue,red,blue)}";
  agrees "color-mix interpolation colour space"
    ~upper:".a{color:color-mix(IN OKLAB,red,blue)}"
    ~lower:".a{color:color-mix(in oklab,red,blue)}";
  (* [shape-outside] is a [string property]: a valid value is handed back as
     the raw source text, uppercase var() included, so there is no folded
     form to agree on here. Only the empty-var() rejection below is a shared
     path. *)
  (* [from()]/[to()] name the two required stops of [-webkit-gradient()]; the
     sibling [color-stop()] fold below (#620) left these two untouched. *)
  agrees "-webkit-gradient from()/to()"
    ~upper:
      ".a{background:-webkit-gradient(linear,left top,left \
       bottom,FROM(red),TO(blue))}"
    ~lower:
      ".a{background:-webkit-gradient(linear,left top,left \
       bottom,from(red),to(blue))}";
  (* WebKit's legacy gradient names [color-stop()] as a helper function of
     [-webkit-gradient()], so sec. 4.1 folds it exactly as any other function
     name. *)
  agrees "-webkit-gradient color-stop()"
    ~upper:
      ".a{background:-webkit-gradient(linear,left top,left \
       bottom,from(red),COLOR-STOP(50%,green),to(blue))}"
    ~lower:
      ".a{background:-webkit-gradient(linear,left top,left \
       bottom,from(red),color-stop(50%,green),to(blue))}";
  (* A guard predicate that decides which reader a value goes to reads its ident
     raw, so sec. 4.1 has to reach the guard and not only the reader behind it.
     Each of these is a distinct guard. *)
  pins "touch-action none" ~upper:".a{touch-action:NONE}"
    ~lower:".a{touch-action:none}" ~expect:".a{touch-action:none}";
  pins "touch-action manipulation" ~upper:".a{touch-action:MANIPULATION}"
    ~lower:".a{touch-action:manipulation}"
    ~expect:".a{touch-action:manipulation}";
  pins "touch-action CSS-wide keyword" ~upper:".a{touch-action:INHERIT}"
    ~lower:".a{touch-action:inherit}" ~expect:".a{touch-action:inherit}";
  pins "text-transform uppercase" ~upper:".a{text-transform:UPPERCASE}"
    ~lower:".a{text-transform:uppercase}" ~expect:".a{text-transform:uppercase}";
  agrees "white-space shorthand" ~upper:".a{white-space:PRESERVE NOWRAP}"
    ~lower:".a{white-space:preserve nowrap}";
  pins "paint-order stroke" ~upper:".a{paint-order:STROKE}"
    ~lower:".a{paint-order:stroke}" ~expect:".a{paint-order:stroke}";
  pins "vector-effect" ~upper:".a{vector-effect:NON-SCALING-STROKE}"
    ~lower:".a{vector-effect:non-scaling-stroke}"
    ~expect:".a{vector-effect:non-scaling-stroke}";
  agrees "display two-keyword form" ~upper:".a{display:LIST-ITEM FLOW-ROOT}"
    ~lower:".a{display:list-item flow-root}";
  agrees "align-items baseline qualifier"
    ~upper:".a{align-items:FIRST BASELINE}"
    ~lower:".a{align-items:first baseline}";
  agrees "perspective-origin position keywords"
    ~upper:".a{perspective-origin:LEFT 10PX TOP 20PX}"
    ~lower:".a{perspective-origin:left 10px top 20px}";
  pins "contain-intrinsic-size auto"
    ~upper:".a{contain-intrinsic-size:AUTO 300PX}"
    ~lower:".a{contain-intrinsic-size:auto 300px}"
    ~expect:".a{contain-intrinsic-size:auto 300px}";
  agrees "aspect-ratio auto" ~upper:".a{aspect-ratio:AUTO 1/1}"
    ~lower:".a{aspect-ratio:auto 1/1}";
  pins "text-indent each-line" ~upper:".a{text-indent:2EM EACH-LINE}"
    ~lower:".a{text-indent:2em each-line}"
    ~expect:".a{text-indent:2em each-line}";
  pins "grid repeat()" ~upper:".a{grid-template-columns:REPEAT(3, 1FR)}"
    ~lower:".a{grid-template-columns:repeat(3, 1fr)}"
    ~expect:".a{grid-template-columns:repeat(3,1fr)}";
  pins "grid minmax()" ~upper:".a{grid-auto-rows:MINMAX(0, 1FR)}"
    ~lower:".a{grid-auto-rows:minmax(0, 1fr)}"
    ~expect:".a{grid-auto-rows:minmax(0,1fr)}";
  agrees "color() colour space" ~upper:".a{color:COLOR(DISPLAY-P3 1 0 0)}"
    ~lower:".a{color:color(display-p3 1 0 0)}";
  agrees "font shorthand"
    ~upper:".a{font:ITALIC SMALL-CAPS BOLD 16PX/1.5 SERIF}"
    ~lower:".a{font:italic small-caps bold 16px/1.5 serif}";
  pins "scrollbar-gutter modifier"
    ~upper:".a{scrollbar-gutter:STABLE BOTH-EDGES}"
    ~lower:".a{scrollbar-gutter:stable both-edges}"
    ~expect:".a{scrollbar-gutter:stable both-edges}";
  pins "color-scheme CSS-wide keyword" ~upper:".a{color-scheme:Initial}"
    ~lower:".a{color-scheme:initial}" ~expect:".a{color-scheme:initial}";
  pins "border-image-slice CSS-wide keyword"
    ~upper:".a{border-image-slice:UnSet}" ~lower:".a{border-image-slice:unset}"
    ~expect:".a{border-image-slice:unset}";
  agrees "@font-face metric overrides"
    ~upper:
      "@font-face{font-family:F;src:url(a.woff);ascent-override:NORMAL;descent-override:NORMAL}"
    ~lower:
      "@font-face{font-family:F;src:url(a.woff);ascent-override:normal;descent-override:normal}";
  (* An empty [var()] is invalid regardless of case (CSS Custom Properties 1
     sec. 3); the diagnosis must agree, not just the rejection. *)
  let empty_var_diagnostic input =
    match of_string ~strict:true input with
    | Ok _ -> Alcotest.failf "%s: strict parse accepted an empty var()" input
    | Error err -> Cascade.Error.to_string err
  in
  let lower_diag = empty_var_diagnostic ".a{shape-outside:var()}" in
  Alcotest.(check bool)
    (String.concat ""
       [ "lower-case empty var() names the reason, got "; lower_diag ])
    true
    (Astring.String.is_infix ~affix:"empty var()" lower_diag);
  let upper_diag = empty_var_diagnostic ".a{shape-outside:VAR()}" in
  Alcotest.(check bool)
    (String.concat ""
       [ "capitalised empty var() names the reason, got "; upper_diag ])
    true
    (Astring.String.is_infix ~affix:"empty var()" upper_diag)

(* CSS Values 4 sec. 4.2: an author-defined identifier is "fully case-sensitive
   [...] even in the ASCII range (e.g. example and EXAMPLE are two different,
   unrelated user-defined identifiers)". Reading a keyword case-insensitively
   must not reach any of these. *)
let spec_author_ident_case_sensitive () =
  let render input =
    match of_string ~strict:false input with
    | Ok parsed -> to_string ~minify:true parsed.stylesheet
    | Error err ->
        Alcotest.failf "lenient parse rejected %S: %s" input
          (Cascade.Error.to_string err)
  in
  let keeps name input affix =
    let out = render input in
    Alcotest.(check bool)
      (String.concat "" [ name; ": "; affix; " survives, got "; out ])
      true
      (Astring.String.is_infix ~affix out)
  in
  keeps "custom property name" ".a{--Foo:1px;color:var(--Foo)}" "--Foo:1px";
  keeps "custom property name (var reference)" ".a{--Foo:1px;color:var(--Foo)}"
    "var(--Foo)";
  (* [--Foo] and [--foo] are two unrelated properties, so neither absorbs the
     other. *)
  keeps "custom property names stay distinct" ".a{--Foo:1px;--foo:2px}"
    "--Foo:1px";
  keeps "custom property names stay distinct" ".a{--Foo:1px;--foo:2px}"
    "--foo:2px";
  keeps "@keyframes name" "@keyframes Slide{from{opacity:0}}" "Slide";
  keeps "@layer name" "@layer Base{a{color:red}}" "Base";
  keeps "container name" "@container Card (width>0px){a{color:red}}" "Card";
  keeps "view-transition type" "@view-transition{types:Foo}" "Foo";
  keeps "counter name" ".a{counter-reset:Chapter}" "Chapter";
  keeps "grid line name" ".a{grid-column:Foo}" "Foo";
  (* [span] is excluded from the name, so [SPAN Foo] is a span of the line named
     [Foo] and the name keeps its case. *)
  keeps "grid line name after span" ".a{grid-column:SPAN Foo}" "span Foo";
  keeps "class selector" ".Foo{color:red}" ".Foo";
  keeps "id selector" "#Bar{color:red}" "#Bar";
  keeps "class selectors stay distinct" ".Foo{color:red}.foo{color:red}" ".foo";
  keeps "attribute value" "[data-x=\"Foo\"]{color:red}" "Foo";
  keeps "font family name" ".a{font-family:MyFont,sans-serif}" "MyFont";
  keeps "@property name" "@property --Foo{syntax:\"*\";inherits:false}" "--Foo";
  (* CSS Fonts 4 sec. 12.1: [light] and [dark] are the keywords of
     [base-palette], so any other ident there is the author's own. *)
  keeps "base-palette ident"
    "@font-palette-values --P{font-family:F;base-palette:MyPalette}" "MyPalette";
  (* CSS Paged Media 3 sec. 4.3 puts an optional [<ident-token>] before the
     pseudo-pages; that one is the page's own name. *)
  keeps "@page name" "@page Invoice:FIRST{margin:1in}" "Invoice";
  (* A class beside the folded [/deep/] ident is still a name the author
     chose. *)
  keeps "class next to /deep/" ".a /DEEP/ .DEEP{color:red}" ".DEEP";
  (* CSS Values 4 sec. 4.5.1 carries the [<url>] as a string, not an identifier,
     so nothing in it folds. *)
  keeps "url path" ".a{background:URL(\"A.PNG\")}" "A.PNG";
  (* A [var()] reference inside a folded colour function still names a custom
     property. *)
  keeps "custom property inside color-mix()"
    ".a{--Foo:red;color:color-mix(IN srgb,var(--Foo),blue)}" "var(--Foo)";
  (* [var()]'s function name folds (sec. 4.1); the custom property name in its
     argument does not (sec. 4.2). *)
  keeps "display var() argument name" ".a{display:VAR(--Foo)}" "var(--Foo)";
  keeps "animation-timeline var() argument name"
    ".a{animation-timeline:VAR(--Foo)}" "var(--Foo)";
  (* scroll-animations-1 sec. 3: a bare [<dashed-ident>] here names a
     scroll-timeline/view-timeline, not the [scroll()]/[view()] function;
     folding the function name (above) must not reach this. *)
  keeps "animation-timeline name" ".a{animation-timeline:--Foo}" "--Foo";
  keeps "transition shorthand var() argument name"
    ".a{transition:VAR(--Foo) 1s ease}" "var(--Foo)";
  keeps "box-shadow inset var() argument name" ".a{box-shadow:inset VAR(--Foo)}"
    "var(--Foo)";
  keeps "box-shadow INSET var() argument name" ".a{box-shadow:INSET VAR(--Foo)}"
    "var(--Foo)";
  keeps "container shorthand var() argument name" ".a{container:VAR(--Foo)}"
    "var(--Foo)";
  keeps "@font-face unicode-range var() argument name"
    "@font-face{font-family:F;src:url(a.woff);unicode-range:VAR(--Foo)}"
    "var(--Foo)";
  keeps "gradient position var() argument name"
    ".a{background:linear-gradient(VAR(--Foo),red,blue)}" "var(--Foo)";
  (* CSS Syntax 3 sec. 8.3 recognises [@charset] only as the exact byte
     sequence, so the encoding name is not a keyword to fold. *)
  (match of_string ~strict:true "@charset \"utf-8\";.a{color:red}" with
  | Ok _ -> Alcotest.fail "@charset accepted a lower-case encoding name"
  | Error _ -> ());
  match of_string ~strict:true "@charset \"UTF-8\";.a{color:red}" with
  | Ok _ -> ()
  | Error err ->
      Alcotest.failf "@charset rejected the exact encoding name: %s"
        (Cascade.Error.to_string err)

let unknown_at_rule_reaches_output () =
  let parse input =
    match of_string ~strict:false input with
    | Ok parsed -> parsed.stylesheet
    | Error err ->
        Alcotest.failf "lenient parse rejected %S: %s" input
          (Cascade.Error.to_string err)
  in
  let keeps name input expected =
    Alcotest.(check string)
      (name ^ " (serialize)") expected
      (to_string ~minify:true (parse input));
    Alcotest.(check string)
      (name ^ " (optimize)") expected
      (parse input |> optimize |> to_string ~minify:true)
  in
  keeps "a statement-form at-rule reaches the output" "@foo bar;\na{color:red}"
    "@foo bar;a{color:red}";
  keeps "a block-form at-rule takes its contents with it"
    "@future x{a{color:red}}\nb{color:red}"
    "@future x{a{color:red}}b{color:red}";
  keeps "an unknown at-rule nested in @media survives"
    "@media screen{@foo bar;d{color:red}}"
    "@media screen{@foo bar;d{color:red}}";
  (* tw hands cascade [--input-css] written in this vocabulary. *)
  keeps "the Tailwind v4 authoring vocabulary survives"
    "@theme{--c:red}\n@utility btn{color:red}\n.x{color:red}"
    "@theme{--c:red}@utility btn{color:red}.x{color:red}";
  (* An at-rule alone in the stylesheet is the whole output, not nothing. *)
  keeps "an at-rule that is the entire stylesheet is the entire output"
    "@foo bar;" "@foo bar;";
  (* Control: a recognised at-rule is unaffected. *)
  keeps "a recognised at-rule is unaffected" "@media screen{a{color:red}}"
    "@media screen{a{color:red}}";
  (* Pretty output re-parses to the same statement list as the minified one. *)
  let input = "@future x{a{color:red}}\nb{color:red}" in
  Alcotest.(check string)
    "pretty output re-parses to the minified output"
    "@future x{a{color:red}}b{color:red}"
    (to_string ~minify:true (parse (to_string (parse input))))

(* Reading an at-rule name as a keyword decides what a miscased name reaches: a
   name cascade has a grammar for is that grammar, and only a name it has none
   for stays the [Unknown_at_rule] above. *)
let spec_at_rule_name_case_insensitive () =
  let parse input =
    match of_string ~strict:false input with
    | Ok parsed -> parsed.stylesheet
    | Error err ->
        Alcotest.failf "lenient parse rejected %S: %s" input
          (Cascade.Error.to_string err)
  in
  let render input = to_string ~minify:true (parse input) in
  let optimized input = to_string ~minify:true (optimize (parse input)) in
  let rejects name input =
    match of_string ~strict:true input with
    | Ok _ -> Alcotest.failf "%s: strict parse accepted %S" name input
    | Error _ -> ()
  in
  (* A name cascade has no grammar for is the keyword of nothing, so CSS Syntax
     3 sec. 5.4.2 consumes it and it reaches the output as it was written. *)
  Alcotest.(check string)
    "an unknown at-rule keeps the case it was written in"
    "@Foo bar;a{color:red}"
    (render "@Foo bar;\na{color:red}");
  Alcotest.(check string)
    "an unknown at-rule nested in a block keeps its case"
    "@layer x{@Bar baz;a{color:red}}"
    (render "@layer x{@Bar baz;a{color:red}}");
  (* CSS Syntax 3 sec. 8.3 matches [@charset] and the quote after it as an exact
     byte sequence, so no other spelling of it is the charset rule. *)
  Alcotest.(check string)
    "@CHARSET is a name, not the charset rule" "@CHARSET \"UTF-8\";a{color:red}"
    (render "@CHARSET \"UTF-8\";a{color:red}");
  (* Read as [@media], the block is the node its lower-case spelling is, so the
     optimizer treats the two as one. *)
  let merged =
    optimized "@media screen{a{color:red}}@media screen{b{color:blue}}"
  in
  Alcotest.(check bool)
    (String.concat ""
       [ "the lower-case control merges into one block, got "; merged ])
    true
    (not (Astring.String.is_infix ~affix:"}@media" merged));
  Alcotest.(check string)
    "@MEDIA merges with the @media beside it" merged
    (optimized "@media screen{a{color:red}}@MEDIA screen{b{color:blue}}");
  (* The grammar comes with its rejections: a prelude [@media] has no reading
     for is rejected under every spelling of the name. *)
  rejects "@media with a malformed prelude" "@media (bad{a{color:red}}";
  rejects "@MEDIA with a malformed prelude" "@MEDIA (bad{a{color:red}}"

let suite =
  ( "css_parsing",
    [
      Alcotest.test_case "public parse recovery edges" `Quick public_parse_edges;
      Alcotest.test_case "public bad-string prelude edges" `Quick
        public_bad_string_prelude_edges;
      Alcotest.test_case "public bad-string declaration edges" `Quick
        public_bad_string_declaration_edges;
      Alcotest.test_case "spec unknown at-rule reaches the output" `Quick
        unknown_at_rule_reaches_output;
      Alcotest.test_case "spec section 4.1 keyword case is insensitive" `Quick
        spec_keyword_case_insensitive;
      Alcotest.test_case "spec section 4.2 author ident case is sensitive"
        `Quick spec_author_ident_case_sensitive;
      Alcotest.test_case "spec section 4.1 at-rule name case is insensitive"
        `Quick spec_at_rule_name_case_insensitive;
    ] )
