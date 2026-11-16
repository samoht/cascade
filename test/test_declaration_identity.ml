(** Declaration tests: one node per value: equality, hashing and spellings. *)

open Alcotest
open Cascade
open Css.Declaration
open Css_test_helpers
open Declaration_helpers

(* CSS Values 4 sec. 10.7.2 makes NaN a keyword of the <number> grammar that
   resolves at parse time, and sec. 10.13 serialises every NaN-valued
   calculation as calc(NaN): CSS has one NaN and one spelling for it, so two
   declarations that spell it are one declaration. The cached [Declaration.hash]
   already answers that way, and [Declaration.same_minified] merges rules on
   [hash a = hash b && equal_declaration a b], so the two have to agree or a
   merge is lost. *)
let optimized_declarations css =
  Css.stylesheet css |> Css.optimize |> Css.statements
  |> List.concat_map (function
    | Css.Stylesheet.Rule r -> r.Css.Stylesheet.declarations
    | _ -> [])

let sole_declaration css =
  match optimized_declarations css with
  | [ d ] -> d
  | l ->
      Alcotest.failf "%s: expected one declaration, got %d" css (List.length l)

let minified css =
  Css.to_string ~minify:true (Css.optimize (Css.stylesheet css))

(* CSS Variables 1 sec. 3 makes a shorthand carrying a [var()] a
   pending-substitution value: it is syntax-checked only after substitution, and
   which slot the substituted tokens fill is not known before then. So a slot
   holding its initial is not spare there, and dropping it changes what the
   declaration means. [list-style: disc outside var(--x)] with [--x: circle]
   substitutes to [disc outside circle], two [<list-style-type>] values, which
   is invalid at computed-value time and leaves the longhands unset; dropping
   the initials leaves [list-style: var(--x)], which substitutes to the valid
   [circle] and sets the type. Every component the author wrote has to survive
   once a [var()] is in the value. The order may change: sec. 2 of CSS Lists 3
   spells the shorthand with [||], so the components commute. *)
let var_shorthand_keeps_every_slot () =
  let case (css, parts) =
    let out = minified css in
    List.iter
      (fun part ->
        Alcotest.(check bool)
          (String.concat "" [ css; " keeps "; part ])
          true
          (Astring.String.is_infix ~affix:part out))
      parts
  in
  List.iter case
    [
      ("a{list-style:disc outside var(--x)}", [ "disc"; "outside"; "var(--x)" ]);
      ( "a{list-style:url(marker.png) outside var(--x)}",
        [ "url(marker.png)"; "outside"; "var(--x)" ] );
      (* [text-decoration] reaches the same drop through its own normaliser:
         [solid] is [text-decoration-style]'s initial, and [--x: dotted] makes
         the substituted value two styles rather than one. *)
      ("a{text-decoration:solid var(--x)}", [ "solid"; "var(--x)" ]);
      ( "a{text-decoration:underline solid var(--x)}",
        [ "underline"; "solid"; "var(--x)" ] );
    ];
  (* A box shorthand reaches the same conclusion by counting sides rather than
     slots, so it needs the same guard. CSS Values 5 sec. 3 substitutes a
     [var()] at computed-value time and lets it expand to several tokens, so two
     sides that read alike in the source need not be alike after substitution:
     with [--w: 1px 2px 3px] the pair below is six values and invalid where the
     fold would leave three and valid. The plain case folds and pins that the
     fold is there to be prevented. *)
  Alcotest.(check string)
    "a repeated plain side folds" "a{border-width:1px}"
    (minified "a{border-width:1px 1px}");
  Alcotest.(check string)
    "a repeated substituted side does not" "a{border-width:var(--w) var(--w)}"
    (minified "a{border-width:var(--w) var(--w)}")

(* A [<length>] reaches [flex-basis] through the property's own mirror of the
   length variants, and that mirror's normaliser folds only a zero and a
   [calc()]. So [10.0px] stayed the dimension the reader built while [10px] was
   the folded [Px 10.], the printer spelled both [10px], and the rules they sat
   in could not merge until a second pass re-read that text. One [--minify] has
   to reach the stable answer: CSS Values 4 sec. 6.7.2 gives the two spellings
   one serialisation, so they are one value and one node. *)
(* The same defect in the properties whose values hold a length the property's
   own normaliser never reaches. [10.0px] and [10px] are one value under CSS
   Values 4 sec. 6.7.2 and print alike once the sheet is optimised, so the rules
   holding them have to merge in that pass rather than the one after. *)
(* CSS Values 4 sec. 6.7.2 serialises a dimension's unit in lowercase, and the
   reader folds a canonically spelled number onto the unit constructor, which
   prints that way. An authored number the printer respells kept the unit as the
   author cased it instead, so one input was minified two ways: [10PX] came back
   [10px] and [10.0PX] came back [10PX], and the two never merged. *)
let uppercase_units_minify_lowercase () =
  List.iter
    (fun (css, minified_css) ->
      Alcotest.(check string) css minified_css (minified css))
    [
      ("a{width:10.0PX}", "a{width:10px}");
      ("a{width:10.0Q}", "a{width:10q}");
      ("a{width:10.0VMIN}", "a{width:10vmin}");
      ("a{width:10.0PX}b{width:10PX}", "a,b{width:10px}");
      ("a{width:10.0PX}b{width:10px}", "a,b{width:10px}");
      (* The two values holding their own mirror of the length constructors
         decide the unit's case themselves, so they need the same answer. *)
      ("a{line-height:10.0PX}", "a{line-height:10px}");
      ("a{line-height:10.0PX}b{line-height:10PX}", "a,b{line-height:10px}");
      ("a{flex-basis:10.0PX}b{flex-basis:10PX}", "a,b{flex-basis:10px}");
    ]

(* The printer is public without the optimizer, so a value holding its own
   [Dimension] arm has to lowercase the unit there rather than lean on a
   normaliser having folded the dimension away first. *)
let printed_units_are_lowercase () =
  List.iter
    (fun (css, minified_css) ->
      Alcotest.(check string)
        css minified_css
        (Css.to_string ~minify:true (Css.stylesheet css)))
    [
      ("a{width:10.0PX}", "a{width:10px}");
      ("a{line-height:10.0PX}", "a{line-height:10px}");
      ("a{border-width:10.0PX}", "a{border-width:10px}");
    ]

(* CSS Values 4 sec. 6.7.2 gives a string one serialisation, and [--minify]
   writes that one, so the quote the author picked is a round-trip detail rather
   than part of the value. It was kept as a node of its own, so [content:'x']
   and [content:"x"] reached one minified text through two nodes and the rules
   holding them could not merge until a second pass re-read that text. *)
let quoted_content_is_one_node () =
  List.iter
    (fun (css, minified_css) ->
      Alcotest.(check string) css minified_css (minified css))
    [
      ("a{content:'x'}b{content:\"x\"}", "a,b{content:\"x\"}");
      ("a{content:'x' 'y'}b{content:\"x\" \"y\"}", "a,b{content:\"x\" \"y\"}");
      (* A quote the serialisation has to escape is still one answer, so the two
         spellings of it are still one node. *)
      ("a{content:'a\"b'}b{content:\"a\\\"b\"}", "a,b{content:\"a\\\"b\"}");
    ]

(* CSS Animations 1 (ED) sec. 4.9: what an [animation] whose every slot holds
   its initial declares is the eight initials, which is what [animation:none]
   declares, and the printer writes [none] for both. The two stayed separate
   nodes, so the same declaration optimised two ways: the reset-only longhands
   the shorthand covers were dropped after one spelling and kept after the
   other, which made the result depend on whether the sheet had been through
   [fmt] on the way. *)
let all_initial_animation_is_none () =
  List.iter
    (fun (css, minified_css) ->
      Alcotest.(check string) css minified_css (minified css))
    [
      ("a{animation:normal}b{animation:none}", "a,b{animation:none}");
      ("a{animation:none;animation-range:normal}", "a{animation:none}");
      ("a{animation:none;animation-timeline:auto}", "a{animation:none}");
      (* A slot that is not the initial keeps the shorthand it was written
         in. *)
      ("a{animation:reverse}", "a{animation:reverse}");
      ( "a{animation:none;animation-range:1px}",
        "a{animation:none;animation-range:1px}" );
    ]

let unfolded_lengths_are_one_value () =
  let case (property, merged) =
    let css =
      Pp.to_string
        (fun ctx () ->
          Pp.string ctx "a{";
          Pp.string ctx property;
          Pp.string ctx ":10.0px}b{";
          Pp.string ctx property;
          Pp.string ctx ":10px}")
        ()
    in
    Alcotest.(check string) property merged (minified css)
  in
  List.iter case
    [
      ("column-width", "a,b{column-width:10px}");
      ("initial-letter-wrap", "a,b{initial-letter-wrap:10px}");
      ("size", "a,b{size:10px}");
      ("background-size", "a,b{background-size:10px}");
      ("mask-size", "a,b{-webkit-mask-size:10px;mask-size:10px}");
    ]

let flex_basis_spellings_are_one_value () =
  let a = sole_declaration ".a{flex-basis:10.0px}" in
  let b = sole_declaration ".b{flex-basis:10px}" in
  Alcotest.(check string)
    "the authored spelling folds" "flex-basis:10px"
    (Css.Declaration.to_string ~minify:true a);
  Alcotest.(check int)
    "hash reads the two as one value" (Css.Declaration.hash a)
    (Css.Declaration.hash b);
  Alcotest.(check bool)
    "the two spellings are equal" true
    (Css.Declaration.equal_declaration a b);
  Alcotest.(check string)
    "one pass merges the rules" "a,b{flex-basis:10px}"
    (minified "a{flex-basis:10.0px}b{flex-basis:10px}")

let nan_declaration_is_one_value () =
  (* Parsed apart so the two are distinct heap blocks: a physical-equality
     short-circuit must not stand in for the answer. *)
  let a = sole_declaration ".a{opacity:calc(infinity - infinity)}" in
  let b = sole_declaration ".b{opacity:calc(infinity - infinity)}" in
  Alcotest.(check string)
    "the fold spells calc(NaN)" "opacity:calc(NaN)"
    (Css.Declaration.to_string ~minify:true a);
  Alcotest.(check int)
    "hash reads the two as one value" (Css.Declaration.hash a)
    (Css.Declaration.hash b);
  Alcotest.(check bool)
    "a NaN declaration equals itself" true
    (Css.Declaration.equal_declaration a a);
  Alcotest.(check bool)
    "two NaN declarations are equal" true
    (Css.Declaration.equal_declaration a b);
  (* An ordinary float still answers both ways. *)
  let half = sole_declaration ".c{opacity:.5}" in
  let half' = sole_declaration ".d{opacity:.5}" in
  let other = sole_declaration ".e{opacity:.6}" in
  Alcotest.(check bool)
    "equal floats are equal" true
    (Css.Declaration.equal_declaration half half');
  Alcotest.(check bool)
    "different floats are not" false
    (Css.Declaration.equal_declaration half other);
  (* What the disagreement costs: the NaN pair is left unmerged while the
     ordinary-float control merges. *)
  Alcotest.(check string)
    "the NaN pair merges" ".a,.b{opacity:calc(NaN)}"
    (minified
       ".a{opacity:calc(infinity - infinity)}.b{opacity:calc(infinity - \
        infinity)}");
  Alcotest.(check string)
    "the float control merges" ".c,.d{opacity:.5}"
    (minified ".c{opacity:.5}.d{opacity:.5}")

(* CSS Values 4 sec. 10.7.2 gives NaN one spelling and one resolution: it is a
   keyword of the <number> grammar that resolves at parse time, so a NaN reaches
   the AST as that keyword rather than as a float a calculation happened to land
   on. Sec. 10.13 serialises every NaN-valued calculation the same way,
   calc(NaN) for a <number> and calc(NaN * 1px) once the value carries a unit.
   One value, one node: however the source spelled the NaN, the declaration that
   holds it is the same declaration, so it merges and it hashes the same.

   infinity and -infinity are separate constants of the same sec. 10.7.2 list,
   each with its own serialisation, so they stay three distinct values. *)
let same_text_same_hash label a b =
  let text = Css.Declaration.to_string ~minify:true a in
  Alcotest.(check string)
    (label ^ ": one minified text")
    text
    (Css.Declaration.to_string ~minify:true b);
  Alcotest.(check int)
    (label ^ ": one hash") (Css.Declaration.hash a) (Css.Declaration.hash b);
  Alcotest.(check bool)
    (label ^ ": one declaration")
    true
    (Css.Declaration.equal_declaration a b)

let distinct_value label a b =
  Alcotest.(check bool)
    (label ^ ": two texts") false
    (String.equal
       (Css.Declaration.to_string ~minify:true a)
       (Css.Declaration.to_string ~minify:true b));
  Alcotest.(check bool)
    (label ^ ": two declarations")
    false
    (Css.Declaration.equal_declaration a b)

let aspect_ratio_has_one_node () =
  let parsed = ok (Css.Declaration.of_string "aspect-ratio:16/9") in
  same_text_same_hash "Ratio constructor vs parsed"
    (Css.aspect_ratio (Css.Ratio (16., 9.)))
    parsed;
  same_text_same_hash "ratio helper vs parsed"
    (Css.aspect_ratio (Css.ratio 16. 9.))
    parsed;
  same_text_same_hash "Auto_ratio constructor vs parsed"
    (Css.aspect_ratio (Css.Auto_ratio (16., 9.)))
    (ok (Css.Declaration.of_string "aspect-ratio:auto 16/9"))

let caret_auto_has_one_node () =
  same_text_same_hash "caret Auto constructor vs parsed"
    (Css.caret (Css.Auto : Css.caret))
    (ok (Css.Declaration.of_string "caret:auto"))

let gradient_var kind constructor =
  let var =
    Css.Values.read_var Css.Properties.read_gradient_stop
      (Cursor.of_string "var(--stops)")
  in
  same_text_same_hash
    (kind ^ " var constructor vs parsed")
    (Css.background_image (constructor var))
    (Css_test_helpers.ok
       (Css.Declaration.of_string
          ("background-image:" ^ kind ^ "(var(--stops))")))

let radial_gradient_var_has_one_node () =
  gradient_var "radial-gradient" (fun var -> Css.Radial_gradient_var var)

let conic_gradient_var_has_one_node () =
  gradient_var "conic-gradient" (fun var -> Css.Conic_gradient_var var)

let nan_has_one_node () =
  (* The keyword and the calculation that lands on NaN are the same value. *)
  let keyword = sole_declaration ".a{opacity:calc(NaN)}" in
  let computed = sole_declaration ".b{opacity:calc(infinity - infinity)}" in
  Alcotest.(check string)
    "the keyword spells calc(NaN)" "opacity:calc(NaN)"
    (Css.Declaration.to_string ~minify:true keyword);
  same_text_same_hash "keyword vs computed" keyword computed;
  Alcotest.(check string)
    "the two spellings merge" ".a,.b{opacity:calc(NaN)}"
    (minified ".a{opacity:calc(NaN)}.b{opacity:calc(infinity - infinity)}");
  (* Spellings a calculation keeps rather than folds stay one value each. *)
  Alcotest.(check string)
    "calc(0/0) merges" ".a,.b{opacity:calc(0/0)}"
    (minified ".a{opacity:calc(0/0)}.b{opacity:calc(0/0)}");
  Alcotest.(check string)
    "calc(asin(2)) merges" ".a,.b{opacity:calc(asin(2))}"
    (minified ".a{opacity:calc(asin(2))}.b{opacity:calc(asin(2))}");
  Alcotest.(check string)
    "a NaN length merges" ".a,.b{width:calc(NaN*1px)}"
    (minified ".a{width:calc(NaN*1px)}.b{width:calc(NaN*1px)}");
  Alcotest.(check string)
    "a NaN duration merges" ".a,.b{transition-duration:calc(NaN*1s)}"
    (minified
       ".a{transition-duration:calc(NaN*1s)}.b{transition-duration:calc(NaN*1s)}");
  Alcotest.(check string)
    "a NaN length expression merges"
    ".a,.b{width:calc(infinity*1px - infinity*1px)}"
    (minified
       ".a{width:calc(infinity*1px - infinity*1px)}.b{width:calc(infinity*1px \
        - infinity*1px)}");
  (* The other two degenerate constants keep their own values. *)
  let inf = sole_declaration ".c{opacity:calc(infinity)}" in
  let inf' = sole_declaration ".d{opacity:calc(infinity)}" in
  let neg_inf = sole_declaration ".e{opacity:calc(-infinity)}" in
  same_text_same_hash "infinity vs infinity" inf inf';
  distinct_value "infinity vs -infinity" inf neg_inf;
  distinct_value "infinity vs NaN" inf keyword;
  distinct_value "-infinity vs NaN" neg_inf keyword;
  Alcotest.(check string)
    "infinity merges with itself" ".c,.d{opacity:calc(infinity)}"
    (minified ".c{opacity:calc(infinity)}.d{opacity:calc(infinity)}");
  Alcotest.(check string)
    "the three constants stay apart"
    ".a{opacity:calc(NaN)}.c{opacity:calc(infinity)}.e{opacity:calc(-infinity)}"
    (minified
       ".a{opacity:calc(NaN)}.c{opacity:calc(infinity)}.e{opacity:calc(-infinity)}")

(* CSS Color 4 sec. 5.2 reads the hexadecimal digits ASCII case-insensitively
   and expands [#RGB] to [#RRGGBB] by duplicating each digit, so [#FFF], [#fff]
   and [#ffffff] name one colour. The source spelling is a pretty-printing
   detail, not part of the value: once optimisation has canonicalised it away
   the three are one declaration, they hash alike, and the rules holding them
   merge. *)
let hex_spellings_have_one_node () =
  let short_upper = sole_declaration ".a{color:#FFF}" in
  let short_lower = sole_declaration ".b{color:#fff}" in
  let long = sole_declaration ".c{color:#ffffff}" in
  Alcotest.(check string)
    "the fold spells #fff" "color:#fff"
    (Css.Declaration.to_string ~minify:true short_upper);
  same_text_same_hash "#FFF vs #fff" short_upper short_lower;
  same_text_same_hash "#fff vs #ffffff" short_lower long;
  Alcotest.(check string)
    "the three spellings merge" ".a,.b,.c{color:#fff}"
    (minified ".a{color:#FFF}.b{color:#fff}.c{color:#ffffff}");
  (* A colour with no shorter named form takes the same route. *)
  let mixed = sole_declaration ".a{color:#AbC}" in
  let expanded = sole_declaration ".b{color:#aabbcc}" in
  same_text_same_hash "#AbC vs #aabbcc" mixed expanded;
  Alcotest.(check string)
    "a mixed-case spelling merges" ".a,.b{color:#abc}"
    (minified ".a{color:#AbC}.b{color:#aabbcc}");
  (* Sec. 5.2 also gives the fourth digit pair as alpha, and sec. 4.2 drops a
     fully opaque one, so [#FFFF] is the same value again. *)
  same_text_same_hash "#FFFF vs #fff"
    (sole_declaration ".a{color:#FFFF}")
    short_lower;
  (* The control that already merged: a hex whose colour has a shorter name
     folds across notations. *)
  Alcotest.(check string)
    "hex and name merge" ".a,.b,.c{color:red}"
    (minified ".a{color:#FF0000}.b{color:#f00}.c{color:red}");
  (* Different colours stay different values. *)
  distinct_value "#fff vs #eee" short_lower (sole_declaration ".d{color:#EEE}");
  distinct_value "#fff vs #fff8" short_lower
    (sole_declaration ".e{color:#FFF8}")

(* CSS Syntax 3 (ED) sec. 4.3.12 builds a <number-token>'s value from an
   optional sign, a digit run, an optional fraction and an optional exponent, so
   [1px], [1.0px], [+1px], [01px] and [1e0px] are one length, as [1e3px] and
   [1000px] are. The AST keeps the authored spelling for the unminified
   round-trip only; once optimisation has canonicalised it away the spellings
   are one declaration, they hash alike, and the rules holding them merge in the
   single pass the optimizer's fixpoint promises. *)
let number_spellings_have_one_node () =
  let canonical = sole_declaration ".a{width:1px}" in
  List.iter
    (fun spelling ->
      let d =
        sole_declaration (String.concat "" [ ".b{width:"; spelling; "}" ])
      in
      Alcotest.(check string)
        (spelling ^ ": the fold spells 1px")
        "width:1px"
        (Css.Declaration.to_string ~minify:true d);
      same_text_same_hash (spelling ^ " vs 1px") d canonical)
    [ "1.0px"; "1.00px"; "+1px"; "01px"; "1e0px"; "1E0px" ];
  (* An exponent the printer does not keep names the number it stands for. *)
  same_text_same_hash "1e3px vs 1000px"
    (sole_declaration ".a{width:1e3px}")
    (sole_declaration ".b{width:1000px}");
  Alcotest.(check string)
    "the spellings merge in one pass" ".a,.b{width:1px}"
    (minified ".a{width:1.0px}.b{width:1px}");
  Alcotest.(check string)
    "an exponent merges in one pass" ".a,.b{width:1000px}"
    (minified ".a{width:1e3px}.b{width:1000px}");
  (* The same node feeds the box contraction: a side spelled differently held
     the shorthand open until a second pass re-read it. *)
  Alcotest.(check string)
    "the box contracts in one pass" ".a{padding:1px}"
    (minified ".a{padding:1px;padding-bottom:1E0px}")

(* What the fold above must not reach. A unit or a type carries a value the
   number alone does not, and CSS Variables 1 sec. 4.1 forbids normalizing a
   custom property at all, so its authored spelling stays a value of its own. *)
let number_spellings_keep_their_value () =
  let px = sole_declaration ".a{width:1px}" in
  distinct_value "1px vs 1pt" px (sole_declaration ".b{width:1pt}");
  distinct_value "1px vs 1%" px (sole_declaration ".c{width:1%}");
  distinct_value "1px vs 2px" px (sole_declaration ".d{width:2px}");
  (* Sec. 6 of CSS Values 4 drops a zero length's unit, so these two already
     were one declaration. *)
  same_text_same_hash "0 vs 0px"
    (sole_declaration ".a{width:0}")
    (sole_declaration ".b{width:0px}");
  (* CSS Color 4 sec. 5.2: the hex and the name are one colour, and already
     were. *)
  same_text_same_hash "red vs #f00"
    (sole_declaration ".a{color:red}")
    (sole_declaration ".b{color:#f00}");
  (* A custom property holds the token stream the author wrote, so its two
     spellings stay two declarations even where the printer gives them one
     text. *)
  let custom = sole_declaration ".a{--x:1.0px}" in
  let custom' = sole_declaration ".b{--x:1px}" in
  Alcotest.(check bool)
    "a custom property is not folded" false
    (Css.Declaration.equal_declaration custom custom')

(* Cascade keeps no raw-token sidecar, so a printed declaration is rebuilt from
   its typed values alone: reading the print back has to land on the node that
   printed it, not merely on the same text. *)
let reads_back input =
  let d = ok (Css.Declaration.of_string input) in
  let printed = Css.Declaration.to_string ~minify:true d in
  Alcotest.(check bool)
    (input ^ ": the print reads back as the same node")
    true
    (Css.Declaration.equal_declaration d
       (ok (Css.Declaration.of_string printed)))

(* Four families whose grammar takes more component values than the reader took.
   Each block pins the multi-value forms the grammar allows, the single-value
   forms that already worked, and the neighbouring grammar that must not widen
   with them. *)
let multi_value_grammars () =
  (* CSS Box 4 (ED) sec. 3.2 gives [margin] the value [<'margin-top'>{1,4}] and
     sec. 3.1 gives [margin-top] the value [<length-percentage> | auto], so
     [auto] is a component of the box: it may stand in any of the four slots and
     beside any length, at any count. It is not a whole-value keyword. *)
  check_declaration ~roundtrip:true ~expected:"margin:auto 2px"
    "margin: auto 2px";
  check_declaration ~roundtrip:true ~expected:"margin:auto auto auto 2px"
    "margin: auto auto auto 2px";
  check_declaration ~roundtrip:true ~expected:"margin:2px auto auto"
    "margin: 2px auto auto";
  reads_back "margin: auto 2px";
  reads_back "margin: auto auto auto 2px";
  (* Working before the widening, so a fix must not move them. *)
  check_declaration ~expected:"margin:auto" "margin: auto";
  check_declaration ~expected:"margin:0 auto" "margin: 0 auto";
  check_declaration ~expected:"margin:10px auto" "margin: 10px auto";
  check_declaration ~expected:"margin:0 auto 20px" "margin: 0 auto 20px";
  check_declaration ~expected:"margin:1px 2px 3px 4px" "margin: 1px 2px 3px 4px";
  check_declaration ~expected:"margin-inline:0 auto" "margin-inline: 0 auto";
  (* A fifth component is outside {1,4}, and the CSS-wide keywords stay
     whole-value (CSS Cascade 5 sec. 6). *)
  neg_cursor read_declaration "margin: 1px 2px 3px 4px 5px";
  neg_cursor read_declaration "margin: inherit 1px";
  (* Sec. 4.1 gives [padding] the same {1,4} box over [<length-percentage
     [0,inf]>], which has no [auto], so widening the margin box must leave this
     one alone. *)
  neg_cursor read_declaration "padding: 0 auto";
  neg_cursor read_declaration "padding: auto";

  (* CSS Backgrounds 3 (ED) sec. 3.2, "Line Patterns: the border-style
     properties": [border-style] is [<line-style>{1,4}], the box shorthand over
     the four side styles, exactly as sec. 3.1 gives [border-color] and sec. 3.3
     gives [border-width] theirs. *)
  check_declaration ~roundtrip:true ~expected:"border-style:double dashed"
    "border-style: double dashed";
  check_declaration ~roundtrip:true ~expected:"border-style:solid none"
    "border-style: solid none";
  check_declaration ~roundtrip:true ~expected:"border-style:none none solid"
    "border-style: none none solid";
  check_declaration ~roundtrip:true
    ~expected:"border-style:none dashed none solid"
    "border-style: none dashed none solid";
  reads_back "border-style: double dashed";
  reads_back "border-style: none dashed none solid";
  check_declaration ~expected:"border-style:solid" "border-style: solid";
  check_declaration ~expected:"border-style:none" "border-style: none";
  (* The box fills the sides by position, so a list repeating what those rules
     already supply is the longer spelling of the same declaration and
     canonicalises to the shortest, as [border-color]'s does. pp holds the
     authored node; the fold is a normalize step. *)
  check_declaration ~expected:"border-style:dashed dashed dashed dashed"
    ~optimized:"border-style:dashed" "border-style: dashed dashed dashed dashed";
  check_declaration ~expected:"border-style:dashed none dashed"
    ~optimized:"border-style:dashed none" "border-style: dashed none dashed";
  neg_cursor read_declaration "border-style: solid solid solid solid solid";
  neg_cursor read_declaration "border-style: solid 1px";

  (* CSS Logical 1 (ED) sec. 4.5.2, "Flow-Relative Border Styles": the
     [border-block-style] and [border-inline-style] shorthands are
     [<'border-top-style'>{1,2}], the first value the start edge and the second
     the end edge, as sec. 4.5.1 gives the widths and sec. 4.5.3 the colours. *)
  check_declaration ~roundtrip:true ~expected:"border-block-style:dashed double"
    "border-block-style: dashed double";
  check_declaration ~roundtrip:true
    ~expected:"border-inline-style:dashed double"
    "border-inline-style: dashed double";
  check_declaration ~roundtrip:true ~expected:"border-inline-style:solid none"
    "border-inline-style: solid none";
  reads_back "border-block-style: dashed double";
  reads_back "border-inline-style: dashed double";
  check_declaration ~expected:"border-block-style:solid"
    "border-block-style: solid";
  check_declaration ~expected:"border-inline-style:dashed"
    "border-inline-style: dashed";
  (* A third value is outside {1,2}; the sibling widths already stop there. *)
  neg_cursor read_declaration "border-inline-style: solid dashed dotted";
  neg_cursor read_declaration "border-block-style: solid dashed dotted";

  (* CSS Backgrounds 3 (ED) sec. 4.1, "Curve Radii: the border-radius
     properties": each corner longhand is [<length-percentage [0,inf]>{1,2}],
     "The first value is the horizontal radius, the second the vertical radius."
     The 11 March 2024 CR draft numbers both sections the same way. *)
  check_declaration ~roundtrip:true ~expected:"border-top-left-radius:1px 5px"
    "border-top-left-radius: 1px 5px";
  check_declaration ~roundtrip:true
    ~expected:"border-bottom-right-radius:10%20%"
    "border-bottom-right-radius: 10% 20%";
  check_declaration ~roundtrip:true
    ~expected:"border-start-start-radius:1px 5px"
    "border-start-start-radius: 1px 5px";
  reads_back "border-top-left-radius: 1px 5px";
  reads_back "border-bottom-right-radius: 10% 20%";
  check_declaration ~expected:"border-top-left-radius:1px"
    "border-top-left-radius: 1px";
  check_declaration ~expected:"border-top-left-radius:50%"
    "border-top-left-radius: 50%";
  (* One value already sets both radii, so a vertical radius equal to the
     horizontal one says what omitting it says. *)
  check_declaration ~expected:"border-top-left-radius:1px 1px"
    ~optimized:"border-top-left-radius:1px" "border-top-left-radius: 1px 1px";
  (* The shorthand already spelled both axes across the slash, and keeps to
     it. *)
  check_declaration ~expected:"border-radius:10%/20%" "border-radius: 10% / 20%";
  (* [0,inf] bars a negative radius, and a third value is outside {1,2}. *)
  neg_cursor read_declaration "border-top-left-radius: -1px";
  neg_cursor read_declaration "border-top-left-radius: 1px 2px 3px"

(* ignore-test: a calc() sum spans every length property, not one. *)
let test_calc_sum_shortest_spelling () =
  (* CSS Values 4 (ED) sec. 10.13 Serialization sorts a sum's children before
     serialising them, so the order the author wrote carries no meaning and
     every order is the same value. Minify takes the shortest of them: a
     leading negative term spends a byte on its own sign and another on the [+]
     that joins the next, where a leading positive term spends neither. *)
  (* [held] is the input itself: pp serialises what it was given, and picking
     between two spellings of one value is the optimizer's job. *)
  let shortest ~into input =
    decl_optimizes ~prop:"width" ~held:input ~into input
  in
  shortest ~into:"calc(100vw - 10px)" "calc(-10px + 100vw)";
  shortest ~into:"calc(100vw - 10px)" "calc(100vw - 10px)";
  shortest ~into:"calc(50vmin - 2em)" "calc(-2em + 50vmin)";
  shortest ~into:"calc(3px - 2em)" "calc(-2em + 3px)";
  (* The join reads the same whichever unit leads: a negative term is [- 2em],
     never [+ -2em]. *)
  shortest ~into:"calc(3px - 2em)" "calc(3px - 2em)";
  shortest ~into:"calc(3em - 2px)" "calc(3em - 2px)";
  (* With no positive term to lead, the authored order stands: every spelling is
     the same length. *)
  shortest ~into:"calc(-1px - 2em)" "calc(-1px - 2em)";
  shortest ~into:"calc(-2em - 1px)" "calc(-2em - 1px)";
  (* A percentage leads what it is mixed with, and still only when positive. *)
  shortest ~into:"calc(10% - 5vw)" "calc(-5vw + 10%)";
  shortest ~into:"calc(5vw - 10%)" "calc(-10% + 5vw)"

(* CSS Syntax 3 (ED) sec. 4.2 makes [-] an ident code point and sec. 4.3.4 has a
   dimension's unit consume an ident sequence, so [0deg-1] is the single
   dimension [0deg-1] and the separator in front of a signed number is mandatory
   after an angle. Only a number ends where one may follow it unseparated: sec.
   4.3.12 stops consuming a number at a [-] no [e] or [.] precedes. A [)] ends
   its block and separates from anything. *)
let dimension_before_signed_number () =
  minify_reads_back "rotate axis after a zero angle"
    ~expected:"a{rotate:0deg -1 0 0}" "a { rotate: -1 0 0 0deg }";
  minify_reads_back "rotate axis after an angle"
    ~expected:"a{rotate:45deg 1-1-1}" "a { rotate: 1 -1 -1 45deg }";
  minify_reads_back "rotate axis with an unsigned first component"
    ~expected:"a{rotate:45deg 1 2-3}" "a { rotate: 1 2 -3 45deg }";
  (* A [var()] angle ends on [)], which needs no separator after it. Only the
     printed text is pinned: the reader takes the angle-first spelling of this
     value as an opaque stream, so the emission does not carry the axis node
     back either way. *)
  check_declaration ~expected:"rotate:var(--a)1-1-1" "rotate:1 -1 -1 var(--a)";
  minify_reads_back "scale number pair" ~expected:"a{scale:-1-2}"
    "a { scale: -1 -2 }";
  minify_reads_back "translate lengths" ~expected:"a{translate:1px -2px}"
    "a { translate: 1px -2px }";
  minify_reads_back "box-shadow lengths"
    ~expected:"a{box-shadow:1px -2px 3px -4px red}"
    "a { box-shadow: 1px -2px 3px -4px red }";
  minify_reads_back "translate3d arguments"
    ~expected:"a{transform:translate3d(1px,-2px,-3px)}"
    "a { transform: translate3d(1px, -2px, -3px) }";
  minify_reads_back "rotate3d arguments"
    ~expected:"a{transform:rotate3d(1,-1,-1,45deg)}"
    "a { transform: rotate3d(1, -1, -1, 45deg) }";
  minify_reads_back "margin lengths" ~expected:"a{margin:-1px -2px}"
    "a { margin: -1px -2px }"

(* CSS Syntax 3 (ED) sec. 4.3.3 ends a percentage token at its [%] and sec.
   4.3.5 ends a block at its [)], so neither can absorb what follows. CSS Color
   5 sec. 4.1 gives each channel of a relative colour one component value, and
   grammars are matched against tokens, so [20%g] carries the two channels [20%]
   and [g] exactly as [20% g] does. The separator is droppable and the reader
   owes the printer that reading. *)
let relative_colour_channel_separator () =
  minify_reads_back "percentage then keyword"
    ~expected:"a{color:rgb(from #639 20%g b/alpha)}"
    "a { color: rgb(from #639 20% g b / alpha) }";
  minify_reads_back "keyword then percentage"
    ~expected:"a{color:rgb(from #639 r 20%b/alpha)}"
    "a { color: rgb(from #639 r 20% b / alpha) }";
  minify_reads_back "percentage then number"
    ~expected:"a{color:rgb(from #639 r 20%10)}"
    "a { color: rgb(from #639 r 20% 10) }";
  minify_reads_back "leading percentage then numbers"
    ~expected:"a{color:rgb(from #639 0%10 10)}"
    "a { color: rgb(from #639 0% 10 10) }";
  minify_reads_back "math function then number"
    ~expected:"a{color:rgb(from #639 r calc(g * 2)10)}"
    "a { color: rgb(from #639 r calc(g * 2) 10) }"

(* The same channel list is the same value in every relative colour function,
   whichever of the two spellings the author wrote. *)
let relative_colour_spellings_agree () =
  sheets_agree "rgb" "a{color:rgb(from #639 20% g b)}"
    "a{color:rgb(from #639 20%g b)}";
  sheets_agree "hsl" "a{color:hsl(from #639 20% s l)}"
    "a{color:hsl(from #639 20%s l)}";
  sheets_agree "hwb" "a{color:hwb(from #639 20% w b)}"
    "a{color:hwb(from #639 20%w b)}";
  sheets_agree "lab" "a{color:lab(from #639 20% a b)}"
    "a{color:lab(from #639 20%a b)}";
  sheets_agree "lch" "a{color:lch(from #639 20% c h)}"
    "a{color:lch(from #639 20%c h)}";
  sheets_agree "oklab" "a{color:oklab(from #639 20% a b)}"
    "a{color:oklab(from #639 20%a b)}";
  sheets_agree "oklch" "a{color:oklch(from #639 20% c h)}"
    "a{color:oklch(from #639 20%c h)}";
  sheets_agree "color" "a{color:color(from #639 srgb 20% g b)}"
    "a{color:color(from #639 srgb 20%g b)}";
  sheets_agree "alpha slash" "a{color:rgb(from #639 r g b / alpha)}"
    "a{color:rgb(from #639 r g b/alpha)}"

let suite =
  ( "declaration_identity",
    [
      test_case "calc sum shortest spelling" `Quick
        test_calc_sum_shortest_spelling;
      test_case "aspect-ratio has one node" `Quick aspect_ratio_has_one_node;
      test_case "caret auto has one node" `Quick caret_auto_has_one_node;
      test_case "radial gradient var has one node" `Quick
        radial_gradient_var_has_one_node;
      test_case "conic gradient var has one node" `Quick
        conic_gradient_var_has_one_node;
      test_case "NaN is one declared value" `Quick nan_declaration_is_one_value;
      test_case "flex-basis spellings are one value" `Quick
        flex_basis_spellings_are_one_value;
      test_case "unfolded lengths are one value" `Quick
        unfolded_lengths_are_one_value;
      test_case "uppercase units minify lowercase" `Quick
        uppercase_units_minify_lowercase;
      test_case "printed units are lowercase" `Quick printed_units_are_lowercase;
      test_case "a var shorthand keeps every slot" `Quick
        var_shorthand_keeps_every_slot;
      test_case "quoted content is one node" `Quick quoted_content_is_one_node;
      test_case "all-initial animation is none" `Quick
        all_initial_animation_is_none;
      test_case "NaN has one node" `Quick nan_has_one_node;
      test_case "hex spellings have one node" `Quick hex_spellings_have_one_node;
      test_case "number spellings have one node" `Quick
        number_spellings_have_one_node;
      test_case "number spellings keep their value" `Quick
        number_spellings_keep_their_value;
      test_case "multi-value grammars" `Quick multi_value_grammars;
      test_case "dimension before a signed number" `Quick
        dimension_before_signed_number;
      test_case "relative colour channel separator" `Quick
        relative_colour_channel_separator;
      test_case "relative colour spellings agree" `Quick
        relative_colour_spellings_agree;
    ] )
