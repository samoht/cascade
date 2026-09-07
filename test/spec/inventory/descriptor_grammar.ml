type row = {
  descriptor : string;
  positives : string list;
  negatives : string list;
  why : string;
}

let row descriptor why positives negatives =
  { descriptor; positives; negatives; why }

(* Sections are CSS Fonts 4 (ED) unless the row says otherwise. Values a browser
   and the specification disagree about are deliberately absent: this is the
   grammar, and where a browser has not caught up the harness reads the support
   dataset rather than a vector written to match it. *)
let rows =
  [
    row "font-family" "sec. 4.2 <font-family-name> = <string> | <custom-ident>+"
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
    row "src" "sec. 4.3.1 <font-src-list>"
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
    row "font-style"
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
    row "font-weight"
      "sec. 4.4 auto | <font-weight-absolute>{1,2}, over normal | bold | \
       <number [1,1000]>"
      [ "auto"; "normal"; "bold"; "400"; "1"; "1000"; "100 900"; "bold 400" ]
      [ "lighter"; "bolder"; "400 lighter"; "normal bold italic" ];
    row "font-stretch"
      "sec. 4.4 auto | <'font-width'>{1,2}, under the sec. 2.3.1 legacy name"
      [ "auto"; "normal"; "condensed"; "75%"; "75% 125%"; "normal condensed" ]
      [ "condensed 75% 100%"; "75px"; "wider" ];
    row "font-display" "sec. 4.7 auto | block | swap | fallback | optional"
      [ "auto"; "block"; "swap"; "fallback"; "optional" ]
      [ "maybe"; "swap block"; "0" ];
    row "unicode-range" "sec. 4.5 <unicode-range-token>#"
      [ "U+0-7F"; "U+25-FF"; "U+1F600-1F64F"; "U+???"; "U+0-7F, U+100" ]
      [ "red"; "0-7F"; "U+0-7F U+100" ];
    row "font-feature-settings" "sec. 4.6 normal | <feature-tag-value>#"
      [
        "normal";
        "\"kern\"";
        "\"kern\" 1";
        "\"kern\" on";
        "\"kern\" off";
        "\"kern\" 1, \"liga\" 0";
      ]
      [ "kern"; "\"kern\" bogus"; "\"kern\" 1 2" ];
    row "font-variation-settings" "sec. 4.6 normal | [<string> <number>]#"
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
    row "size-adjust" "CSS Fonts 5 (ED) sec. 4.10 <percentage [0,inf]>"
      [ "0%"; "92%"; "100%"; "300%" ]
      [ "-1%"; "100"; "normal" ];
    row "ascent-override" "sec. 4.9 normal | <percentage [0,inf]>"
      [ "normal"; "0%"; "90%" ] [ "-1%"; "90"; "auto" ];
    row "descent-override" "sec. 4.9 normal | <percentage [0,inf]>"
      [ "normal"; "0%"; "25%" ] [ "-1%"; "25"; "auto" ];
    row "line-gap-override" "sec. 4.9 normal | <percentage [0,inf]>"
      [ "normal"; "0%"; "10%" ] [ "-1%"; "10"; "auto" ];
  ]

let descriptors = List.map (fun r -> r.descriptor) rows

let row_for descriptor =
  List.find_opt (fun r -> String.equal r.descriptor descriptor) rows
