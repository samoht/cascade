type row = {
  at_rule : string;
  descriptor : string;
  positives : string list;
  negatives : string list;
  why : string;
}

let row at_rule descriptor why positives negatives =
  { at_rule; descriptor; positives; negatives; why }

(* Sections are CSS Fonts 4 (ED) unless the row says otherwise. Values a browser
   and the specification disagree about are deliberately absent: this is the
   grammar, and where a browser has not caught up the harness reads the support
   dataset rather than a vector written to match it. *)
let rows =
  [
    row "font-face" "font-family"
      "sec. 4.2 <font-family-name> = <string> | <custom-ident>+"
      [ "Brand"; "\"Brand Sans\""; "Brand Sans"; "'Brand'"; "B2" ]
      [
        "serif";
        "default";
        (* CSS Values 4 sec. 4.2 reserves [default], so no <custom-ident> of a
           sequence may be one. *)
        "none default";
        "Brand!";
        "Red/Black";
        "\"Brand";
      ];
    row "font-face" "src" "sec. 4.3.1 <font-src-list>"
      [
        "url(brand.woff2)";
        "url(brand.woff2) format(\"woff2\")";
        "url(brand.woff2) format(woff2)";
        "local(Brand)";
        "local(\"Brand\")";
        "local(Brand), url(brand.woff2)";
        "url(brand.woff2) format(\"woff2\") tech(variations)";
      ]
      [ "nonsense"; "format(\"woff2\")"; "url(brand.woff2) format()" ];
    row "font-face" "font-style"
      "sec. 4.4 auto | normal | italic | left | right | oblique [<angle \
       [-90deg,90deg]>{1,2}]?"
      [
        "auto";
        "normal";
        "italic";
        "oblique";
        "oblique 10deg";
        "oblique 0deg 10deg";
      ]
      [ "normal italic"; "italic oblique"; "bold"; "oblique 10px" ];
    row "font-face" "font-weight"
      "sec. 4.4 auto | <font-weight-absolute>{1,2}, over normal | bold | \
       <number [1,1000]>"
      [ "auto"; "normal"; "bold"; "400"; "1"; "1000"; "100 900"; "bold 400" ]
      [ "lighter"; "bolder"; "400 lighter"; "normal bold italic" ];
    row "font-face" "font-stretch"
      "sec. 4.4 auto | <'font-width'>{1,2}, under the sec. 2.3.1 legacy name"
      [ "auto"; "normal"; "condensed"; "75%"; "75% 125%"; "normal condensed" ]
      [ "condensed 75% 100%"; "75px"; "wider" ];
    row "font-face" "font-display"
      "sec. 4.7 auto | block | swap | fallback | optional"
      [ "auto"; "block"; "swap"; "fallback"; "optional" ]
      [ "maybe"; "swap block"; "0" ];
    row "font-face" "unicode-range" "sec. 4.5 <unicode-range-token>#"
      [ "U+0-7F"; "U+25-FF"; "U+1F600-1F64F"; "U+???"; "U+0-7F, U+100" ]
      [ "red"; "0-7F"; "U+0-7F U+100" ];
    row "font-face" "font-feature-settings"
      "sec. 4.6 normal | <feature-tag-value>#"
      [
        "normal";
        "\"kern\"";
        "\"kern\" 1";
        "\"kern\" on";
        "\"kern\" off";
        "\"kern\" 1, \"liga\" 0";
      ]
      [ "kern"; "\"kern\" bogus"; "\"kern\" 1 2" ];
    row "font-face" "font-variation-settings"
      "sec. 4.6 normal | [<string> <number>]#"
      [ "normal"; "\"wght\" 650"; "\"wght\" 650, \"wdth\" 100" ]
      [
        "wght 650";
        "\"wght\" bold";
        "650 \"wght\"";
        (* The pair is [<string> <number>]: a tag with no number is half a
           value, and [on]/[off] belong to font-feature-settings. *)
        "\"wght\"";
        "\"text\"";
        "\"liga\" off";
      ];
    row "font-face" "size-adjust"
      "CSS Fonts 5 (ED) sec. 4.10 <percentage [0,inf]>"
      [ "0%"; "92%"; "100%"; "300%" ]
      [ "-1%"; "100"; "normal" ];
    row "font-face" "ascent-override" "sec. 4.9 normal | <percentage [0,inf]>"
      [ "normal"; "0%"; "90%" ] [ "-1%"; "90"; "auto" ];
    row "font-face" "descent-override" "sec. 4.9 normal | <percentage [0,inf]>"
      [ "normal"; "0%"; "25%" ] [ "-1%"; "25"; "auto" ];
    row "font-face" "line-gap-override" "sec. 4.9 normal | <percentage [0,inf]>"
      [ "normal"; "0%"; "10%" ] [ "-1%"; "10"; "auto" ];
    (* CSS Counter Styles 3 (ED). <symbol> = <string> | <image> | <custom-ident>
       (sec. 3.2). *)
    row "counter-style" "system"
      "sec. 3.1 cyclic | numeric | alphabetic | symbolic | additive | [fixed \
       <integer>?] | [extends <counter-style-name>]"
      [
        "cyclic";
        "numeric";
        "alphabetic";
        "symbolic";
        "additive";
        "fixed";
        "fixed 3";
        "extends decimal";
      ]
      [ "bogus"; "fixed extends"; "extends" ];
    row "counter-style" "symbols" "sec. 3.2 <symbol>+"
      [ "\"a\""; "\"a\" \"b\""; "a"; "url(a.png)" ]
      [ "1"; "\"a\", \"b\"" ];
    row "counter-style" "additive-symbols"
      "sec. 3.3 [<integer [0,inf]> && <symbol>]#"
      [ "3 \"a\""; "\"a\" 3"; "3 \"a\", 1 \"b\"" ]
      [ "-1 \"a\""; "\"a\""; "3" ];
    row "counter-style" "negative" "sec. 3.4 <symbol> <symbol>?"
      [ "\"-\""; "\"(\" \")\""; "a" ]
      [ "1"; "\"a\" \"b\" \"c\"" ];
    row "counter-style" "prefix" "sec. 3.4 <symbol>" [ "\"(\""; "a" ]
      [ "1"; "\"a\" \"b\"" ];
    row "counter-style" "suffix" "sec. 3.4 <symbol>" [ "\".\""; "a" ]
      [ "1"; "\"a\" \"b\"" ];
    row "counter-style" "range" "sec. 3.5 [[<integer> | infinite]{2}]# | auto"
      [ "auto"; "1 5"; "infinite 5"; "1 infinite"; "1 5, 8 10" ]
      [ "1"; "5 1 3"; "bogus" ];
    row "counter-style" "pad" "sec. 3.6 <integer [0,inf]> && <symbol>"
      [ "3 \"0\""; "\"0\" 3"; "0 \"0\"" ]
      [ "-1 \"0\""; "3"; "\"0\"" ];
    row "counter-style" "fallback" "sec. 3.7 <counter-style-name>"
      [ "decimal"; "my-style" ] [ "\"decimal\""; "1" ];
    row "counter-style" "speak-as"
      "sec. 3.8 auto | bullets | numbers | words | spell-out | \
       <counter-style-name>"
      [ "auto"; "bullets"; "numbers"; "words"; "spell-out"; "decimal" ]
      (* An undefined name is still a <counter-style-name>: sec. 3.7 makes it a
         <custom-ident>, and whether a style by that name exists is resolved
         when the counter is rendered rather than when the rule is read. *)
      [ "1"; "\"decimal\""; "none" ];
    (* CSS Properties and Values API 1 (ED). *)
    row "property" "syntax" "sec. 3.1 <string>"
      [ "\"<color>\""; "\"*\""; "\"<length>\""; "\"<length># \"" ]
      [ "<color>"; "1" ];
    row "property" "inherits" "sec. 3.2 true | false" [ "true"; "false" ]
      [ "yes"; "1" ];
    row "property" "initial-value" "sec. 3.3 <declaration-value>?"
      [ "red"; "0"; "10px" ] [];
  ]

let descriptors = List.map (fun r -> r.descriptor) rows

let row_for ~at_rule descriptor =
  List.find_opt
    (fun r ->
      String.equal r.at_rule at_rule && String.equal r.descriptor descriptor)
    rows
