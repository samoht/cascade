open Cascade
open Css.Font_face

let test_string_of_metric_override () =
  Alcotest.(check string) "normal" "normal" (string_of_metric_override Normal);
  Alcotest.(check string)
    "percent" "110%"
    (string_of_metric_override (Percent 110.))

let test_size_adjust () =
  let s = string_of_size_adjust (Pct 90.) in
  Alcotest.(check string) "size-adjust 90%" "90%" s

let test_src () =
  let s = string_of_src [ Local "Arial" ] in
  Alcotest.(check bool) "local src" true (String.length s > 0);
  Alcotest.(check string)
    "url source with format and tech"
    "url(fonts/color.woff2) format(\"woff2\") tech(color-COLRv1)"
    (string_of_src_entry
       (Url
          {
            url = "fonts/color.woff2";
            format = Some "woff2";
            tech = Some "color-COLRv1";
          }));
  Alcotest.(check string)
    "src list preserves order"
    "local(\"Brand\"), url(brand.woff2) format(\"woff2\")"
    (string_of_src
       [
         Local "Brand";
         Url { url = "brand.woff2"; format = Some "woff2"; tech = None };
       ]);
  Alcotest.(check string)
    "url with tech" "url(font.woff2) tech(variations)"
    (string_of_src
       [ Url { url = "font.woff2"; format = None; tech = Some "variations" } ])

let expect_src_rejected input =
  try
    let src = src_of_string input in
    Alcotest.failf "invalid font-face src parsed: %S -> %S" input
      (string_of_src src)
  with
  | Error.Parse_error _ | Reader.Parse_error _ | Invalid_argument _ | Failure _
  ->
    ()

let expect_metric_rejected input =
  try
    let metric = metric_override_of_string input in
    Alcotest.failf "invalid font metric parsed: %S -> %S" input
      (string_of_metric_override metric)
  with
  | Error.Parse_error _ | Reader.Parse_error _ | Invalid_argument _ | Failure _
  ->
    ()

let expect_size_adjust_rejected input =
  try
    let size_adjust = size_adjust_of_string input in
    Alcotest.failf "invalid font size-adjust parsed: %S -> %s" input
      (string_of_size_adjust size_adjust)
  with
  | Error.Parse_error _ | Reader.Parse_error _ | Invalid_argument _ | Failure _
  ->
    ()

let accepted_invalid_cases label parse render inputs =
  List.filter_map
    (fun input ->
      try
        Fmt.kstr
          (fun s -> Some s)
          "%s: %S -> %S" label input
          (parse input |> render)
      with
      | Error.Parse_error _ | Reader.Parse_error _ | Invalid_argument _
      | Failure _
      ->
        None)
    inputs

let expect_rejected_cases label parse render inputs =
  let accepted = accepted_invalid_cases label parse render inputs in
  match accepted with
  | [] -> ()
  | _ ->
      Alcotest.failf "accepted invalid cases:\n%s" (String.concat "\n" accepted)

let test_spec_src_parser_vectors () =
  let check_src name input expected =
    let parsed = src_of_string input in
    Alcotest.(check bool) name true (equal_src parsed expected);
    Alcotest.(check bool)
      (name ^ " serialization idempotent")
      true
      (equal_src (src_of_string (string_of_src parsed)) parsed)
  in
  check_src "url format tech"
    "url(\"color.woff2\") format(\"woff2\") tech(color-COLRv1)"
    [
      Url
        {
          url = "color.woff2";
          format = Some "woff2";
          tech = Some "color-COLRv1";
        };
    ];
  check_src "local and url list"
    "local(\"Brand\"), url(\"brand.woff2\") format(\"woff2\")"
    [
      Local "Brand";
      Url { url = "brand.woff2"; format = Some "woff2"; tech = None };
    ];
  check_src "source with format collection"
    "url(\"brand.otf\") format(\"opentype\")"
    [ Url { url = "brand.otf"; format = Some "opentype"; tech = None } ];
  check_src "source with tech collection"
    "url(\"variations.woff2\") tech(variations)"
    [
      Url { url = "variations.woff2"; format = None; tech = Some "variations" };
    ]

let test_spec_src_invalid_vectors () =
  List.iter expect_src_rejected
    [
      "format(\"woff2\")";
      "tech(variations)";
      "local()";
      "url(\"font.woff2\") format()";
      "url(\"font.woff2\") tech()";
    ]

let test_spec_metric_parsing_edges () =
  Alcotest.(check string)
    "normal parses" "normal"
    (metric_override_of_string " normal " |> string_of_metric_override);
  Alcotest.(check string)
    "percentage parses" "92.5%"
    (metric_override_of_string "92.5%" |> string_of_metric_override);
  Alcotest.(check string)
    "size adjust percentage" "87.5%"
    (size_adjust_of_string "87.5%" |> string_of_size_adjust)

let test_spec_metric_negative_vectors () =
  List.iter expect_metric_rejected [ "-1%"; "auto"; "100"; "calc(1%)" ];
  Alcotest.(check string)
    "over 100 metric override still parses" "120%"
    (metric_override_of_string "120%" |> string_of_metric_override);
  List.iter expect_size_adjust_rejected
    [ "-10%"; "normal"; "auto"; "100"; "calc(100%)" ];
  Alcotest.(check string)
    "zero size adjust parses" "0%"
    (size_adjust_of_string "0%" |> string_of_size_adjust)

let spec_fontface_source_edges () =
  let check_normalized name input expected =
    Alcotest.(check string) name expected (src_of_string input |> string_of_src)
  in
  check_normalized "tech variations" "url(\"variable.woff2\") tech(variations)"
    "url(variable.woff2) tech(variations)";
  check_normalized "tech palettes" "url(\"color.woff2\") tech(palettes)"
    "url(color.woff2) tech(palettes)";
  check_normalized "tech incremental" "url(\"font.woff2\") tech(incremental)"
    "url(font.woff2) tech(incremental)";
  check_normalized "format collection"
    "url(\"collection.ttc\") format(\"collection\")"
    "url(collection.ttc) format(\"collection\")";
  check_normalized "multiple urls with local fallback"
    "local(\"Brand\"), url(\"brand.woff2\") format(\"woff2\"), \
     url(\"brand.otf\") format(\"opentype\")"
    "local(\"Brand\"), url(brand.woff2) format(\"woff2\"), url(brand.otf) \
     format(\"opentype\")";
  check_normalized "raw unknown source function"
    "url(\"font.woff2\") format(\"woff2\") tech(color-COLRv1)"
    "url(font.woff2) format(\"woff2\") tech(color-COLRv1)"

let spec_fontface_metric_edges () =
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string)
        input expected
        (metric_override_of_string input |> string_of_metric_override))
    [
      ("0%", "0%");
      ("100%", "100%");
      ("normal", "normal");
      (" 87.25% ", "87.25%");
      ("999%", "999%");
    ];
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string)
        input expected
        (size_adjust_of_string input |> string_of_size_adjust))
    [
      ("0%", "0%");
      ("100%", "100%");
      ("125.5%", "125.5%");
      (" 87.25% ", "87.25%");
    ]

let spec_fontface_src_minify_edges () =
  let check_all cases =
    let mismatches =
      List.filter_map
        (fun (name, input, expected) ->
          let actual = input |> src_of_string |> string_of_src ~minify:true in
          if String.equal actual expected then None
          else
            Fmt.kstr
              (fun s -> Some s)
              "%s\n  input:    %S\n  expected: %S\n  actual:   %S" name input
              expected actual)
        cases
    in
    match mismatches with
    | [] -> ()
    | _ ->
        Alcotest.failf "font-face src minify oracle mismatches:\n%s"
          (String.concat "\n" mismatches)
  in
  check_all
    [
      ("local ident unquotes under minify", "local(\"Brand\")", "local(Brand)");
      ( "local underscore ident unquotes under minify",
        "local(\"Brand_2\")",
        "local(Brand_2)" );
      ( "local css-wide keyword stays quoted",
        "local(\"inherit\")",
        "local(\"inherit\")" );
      ( "local leading digit stays quoted",
        "local(\"2Brand\")",
        "local(\"2Brand\")" );
      ( "local family with space stays quoted",
        "local(\"Brand Sans\")",
        "local(\"Brand Sans\")" );
      ( "bare url with spaces stays quoted",
        "url(\"fonts/brand v1.woff2\")",
        "url(\"fonts/brand v1.woff2\")" );
      ( "url with fragment stays bare",
        "url(\"brand.woff2#iefix\")",
        "url(brand.woff2#iefix)" );
      ( "known format keyword lowercases and unquotes",
        "url(\"brand.woff2\") format(\"WOFF2\")",
        "url(brand.woff2)format(woff2)" );
      ( "collection format keyword unquotes",
        "url(\"brand.ttc\") format(\"collection\")",
        "url(brand.ttc)format(collection)" );
      ( "unknown format string stays quoted",
        "url(\"brand.font\") format(\"my-format\")",
        "url(brand.font)format(\"my-format\")" );
      (* CSS Fonts 4 sec. 4.3.1 lists [format("truetype-variations")] as
         equivalent to [format(truetype) tech(variations)], not to a
         [<font-format>] keyword, so the string form has no bare spelling and
         stays quoted. *)
      ( "variations back-compat format string stays quoted",
        "url(\"brand.ttf\") format(\"truetype-variations\")",
        "url(brand.ttf)format(\"truetype-variations\")" );
      ( "svg format keyword unquotes",
        "url(\"brand.svg\") format(\"svg\")",
        "url(brand.svg)format(svg)" );
      ( "embedded-opentype format keyword unquotes",
        "url(\"brand.eot\") format(\"embedded-opentype\")",
        "url(brand.eot)format(embedded-opentype)" );
      ( "tech before format serializes in canonical modifier order",
        "url(\"color.woff2\") tech(color-COLRv1) format(\"woff2\")",
        "url(color.woff2)format(woff2)tech(color-COLRv1)" );
      ( "whitespace-only source separator is accepted",
        "local(\"Brand\") url(\"brand.woff2\")",
        "local(Brand),url(brand.woff2)" );
      ( "var source fallback survives",
        "var(--font-src, url(\"fallback.woff2\") format(\"woff2\"))",
        "var(--font-src,url(fallback.woff2)format(woff2))" );
    ]

let spec_fontface_src_invalid_edges () =
  expect_rejected_cases "font-face src" src_of_string string_of_src
    [
      "";
      "url()";
      "url(\"\")";
      "url(\"font.woff2\"),";
      "url(\"font.woff2\") garbage";
      "url(\"font.woff2\") format(\"woff2\") garbage";
      "url(\"font.woff2\") format(\"woff2\") format(\"opentype\")";
      "url(\"font.woff2\") tech(variations) tech(color-COLRv1)";
    ]

let spec_fontface_metric_numeric_edges () =
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string)
        input expected
        (metric_override_of_string input |> string_of_metric_override))
    [ ("+10%", "10%"); (".5%", "0.5%"); ("1e2%", "100%"); ("0e0%", "0%") ];
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string)
        input expected
        (size_adjust_of_string input |> string_of_size_adjust))
    [ ("+10%", "10%"); (".5%", "0.5%"); ("1e2%", "100%"); ("0e0%", "0%") ];
  let accepted =
    accepted_invalid_cases "font metric override" metric_override_of_string
      string_of_metric_override [ ""; "%"; "+%"; "NaN%" ]
    @ accepted_invalid_cases "font size-adjust" size_adjust_of_string
        string_of_size_adjust [ ""; "%"; "+%"; "NaN%" ]
  in
  match accepted with
  | [] -> ()
  | _ ->
      Alcotest.failf "accepted invalid font metric cases:\n%s"
        (String.concat "\n" accepted)

(* CSS Custom Properties 1 sec. 3 substitutes var() in properties only, and
   @font-face descriptors are not properties: no descriptor grammar accepts one
   (CSS Fonts 4 sec. 4.1), so a browser drops the declaration holding it.
   cascade parks the reference in the typed value and substitutes it at build
   time instead, and every descriptor has somewhere to park one.
   [spec_fontface_var_descriptor_kept] covers a reference standing for a whole
   descriptor value; below are the shapes it does not reach, a reference
   standing for one endpoint of a two-valued range or for one entry of a
   comma-separated list. Each is kept through the parse with no warning, and
   ~strict:true accepts it. *)
let spec_fontface_var_descriptor_edges () =
  let source declaration =
    String.concat ""
      [ "@font-face{font-family:Brand;src:url(font.woff2);"; declaration; "}" ]
  in
  let mismatches declaration =
    let input = source declaration in
    match Css.of_string input with
    | Error _ -> [ Fmt.str "%s: lenient parse rejected %S" declaration input ]
    | Ok { Css.stylesheet; warnings; _ } -> (
        let printed = Css.to_string ~minify:true stylesheet |> String.trim in
        (if String.equal printed input then []
         else
           [ Fmt.str "%s: printed %S, expected %S" declaration printed input ])
        @ (if warnings = [] then []
           else
             [
               Fmt.str "%s: %d parse warnings, expected none" declaration
                 (List.length warnings);
             ])
        @
        match Css.of_string ~strict:true input with
        | Ok _ -> []
        | Error _ ->
            [ Fmt.str "%s: strict parse rejected %S" declaration input ])
  in
  match
    List.concat_map mismatches
      [
        "font-weight:var(--b) 900";
        "font-weight:100 var(--b)";
        "font-style:oblique var(--b) 20deg";
        "font-style:oblique 10deg var(--b)";
        "font-stretch:var(--b) 200%";
        "font-stretch:50% var(--b)";
        "src:local(Other),var(--b)";
        "unicode-range:U+0-7F,var(--b)";
      ]
  with
  | [] -> ()
  | mismatches ->
      Alcotest.failf "@font-face descriptors dropping var():\n%s"
        (String.concat "\n" mismatches)

(* The whole descriptor set, one reference standing for one whole value: each
   keeps it through the parse, with no warning and no strict error, so
   [Css.inline_vars] can substitute it at build time. Keeping it is only useful
   because the value is typed; an unresolved one still prints as the [var()] it
   was. *)
let spec_fontface_var_descriptor_kept () =
  let source descriptor =
    Fmt.str "@font-face{font-family:Brand;src:url(font.woff2);%s:var(--b)}"
      descriptor
  in
  let mismatches descriptor =
    let input = source descriptor in
    match Css.of_string input with
    | Error _ -> [ Fmt.str "%s: lenient parse rejected %S" descriptor input ]
    | Ok { Css.stylesheet; warnings; _ } -> (
        let printed = Css.to_string ~minify:true stylesheet |> String.trim in
        (if String.equal printed input then []
         else [ Fmt.str "%s: printed %S, expected %S" descriptor printed input ])
        @ (if warnings = [] then []
           else
             [
               Fmt.str "%s: %d parse warnings, expected none" descriptor
                 (List.length warnings);
             ])
        @
        match Css.of_string ~strict:true input with
        | Ok _ -> []
        | Error _ -> [ Fmt.str "%s: strict parse rejected %S" descriptor input ]
        )
  in
  match
    List.concat_map mismatches
      [
        "font-family";
        "src";
        "unicode-range";
        "font-style";
        "font-weight";
        "font-stretch";
        "font-display";
        "font-variant";
        "font-feature-settings";
        "font-variation-settings";
        "ascent-override";
        "descent-override";
        "line-gap-override";
        "font-tech";
        "size-adjust";
      ]
  with
  | [] -> ()
  | mismatches ->
      Alcotest.failf "@font-face descriptors dropping var():\n%s"
        (String.concat "\n" mismatches)

(* CSS Fonts 4 sec. 4.2: the [font-family] descriptor *defines* the name used in
   all font family matching and hides any same-named installed family, so a
   rewrite here is not a spelling choice but a redefinition. Sec. 5.1 matches
   family names caselessly and nothing more: [open-sans] stays [open-sans] on
   both the defining and the referencing side. *)
let spec_fontface_family_descriptor_verbatim () =
  let check (name, input, expected) =
    match Css.of_string ~strict:false input with
    | Error _ -> Fmt.kstr (fun s -> Some s) "%s: parse rejected %S" name input
    | Ok { Css.stylesheet; _ } ->
        let actual = Css.to_string ~minify:true stylesheet |> String.trim in
        if String.equal actual expected then None
        else
          Fmt.kstr
            (fun s -> Some s)
            "%s\n  input:    %S\n  expected: %S\n  actual:   %S" name input
            expected actual
  in
  match
    List.filter_map check
      [
        ( "descriptor keeps the authored family name",
          "@font-face{font-family:open-sans;src:url(a.woff2)}",
          "@font-face{font-family:open-sans;src:url(a.woff2)}" );
        ( "descriptor keeps the authored case",
          "@font-face{font-family:inter;src:url(a.woff2)}",
          "@font-face{font-family:inter;src:url(a.woff2)}" );
        ( "reference keeps the authored family name",
          ".a{font-family:open-sans,sans-serif}",
          ".a{font-family:open-sans,sans-serif}" );
        ( "font shorthand keeps the authored family name",
          ".a{font:12px/1.5 open-sans}",
          ".a{font:12px/1.5 open-sans}" );
        ( "@font-feature-values keeps the authored family name",
          "@font-feature-values open-sans{@styleset{x:1}}",
          "@font-feature-values open-sans{@styleset{x:1}}" );
      ]
  with
  | [] -> ()
  | mismatches ->
      Alcotest.failf "@font-face family name rewrites:\n%s"
        (String.concat "\n" mismatches)

(* The [font-family] descriptor, every reference to it, and
   [@font-feature-values] share one family-name printer, so one bad ident
   spelling breaks all three at once. CSS Syntax 3 sec. 4.3.9: a name starting
   with a digit, or with [-] followed by a digit, or consisting of a lone [-],
   does not start an ident sequence, so it has no [<custom-ident>] spelling and
   CSS Fonts 4 sec. 2.1.1 leaves only the [<string>] one. Each output is re-read
   in strict mode: what the printer emits names the same family. *)
let check_family_name_minify (name, input, expected) =
  let fail fmt = Fmt.kstr (fun s -> Some s) fmt in
  match Css.of_string ~strict:false input with
  | Error _ -> fail "%s: parse rejected %S" name input
  | Ok { Css.stylesheet; _ } -> (
      let actual = Css.to_string ~minify:true stylesheet |> String.trim in
      if not (String.equal actual expected) then
        fail "%s\n  input:    %S\n  expected: %S\n  actual:   %S" name input
          expected actual
      else
        match Css.of_string ~strict:true actual with
        | Error e ->
            fail "%s: output does not read back: %S\n  %s" name actual
              (Error.to_string e)
        | Ok { Css.stylesheet; _ } ->
            let again = Css.to_string ~minify:true stylesheet |> String.trim in
            if String.equal again actual then None
            else
              fail "%s: not a fixpoint\n  once:  %S\n  twice: %S" name actual
                again)

let spec_family_name_ident_start () =
  match
    List.filter_map check_family_name_minify
      [
        ( "a name starting with a digit stays quoted in the descriptor",
          "@font-face{font-family:\"2Brand\";src:url(a.woff2)}",
          "@font-face{font-family:\"2Brand\";src:url(a.woff2)}" );
        ( "a name starting with a digit stays quoted in a reference",
          ".a{font-family:\"2Brand\",sans-serif}",
          ".a{font-family:\"2Brand\",sans-serif}" );
        ( "a name starting with a digit stays quoted in the font shorthand",
          ".a{font:12px/1.5 \"2Brand\"}",
          ".a{font:12px/1.5 \"2Brand\"}" );
        ( "a name starting with a digit stays quoted in @font-feature-values",
          "@font-feature-values \"2Brand\"{@styleset{x:1}}",
          "@font-feature-values \"2Brand\"{@styleset{x:1}}" );
        ( "hyphen then digit stays quoted",
          ".a{font-family:\"-2x\"}",
          ".a{font-family:\"-2x\"}" );
        ( "a lone hyphen stays quoted",
          ".a{font-family:\"-\"}",
          ".a{font-family:\"-\"}" );
        ( "a lone hyphen keeps a multi-word name quoted",
          ".a{font-family:\"Brand -\"}",
          ".a{font-family:\"Brand -\"}" );
        ( "a double hyphen starts an ident sequence and unquotes",
          ".a{font-family:\"--brand\"}",
          ".a{font-family:--brand}" );
      ]
  with
  | [] -> ()
  | mismatches ->
      Alcotest.failf "family names spelled as invalid idents:\n%s"
        (String.concat "\n" mismatches)

(* CSS Fonts 4 sec. 2.1.1: in an unquoted [<font-family-name>], "any identifier
   which could be misinterpreted as a pre-defined keyword in the font-family
   value definition, or the CSS-wide keywords, is not allowed", and CSS Values 4
   sec. 4.2 reserves [default] on top. The exclusion is stated per identifier,
   so it reaches every word of a [<custom-ident>+] sequence and such a name has
   no unquoted spelling at all. All four routes into the family-name printer -
   the property, the [font] shorthand, the [@font-face] descriptor and
   [@font-feature-values] - are checked, and each output is re-read in strict
   mode and re-printed. *)
let spec_family_name_reserved_words () =
  match
    List.filter_map check_family_name_minify
      [
        ( "a CSS-wide keyword as the first word keeps the quotes",
          ".a{font-family:\"inherit test\"}",
          ".a{font-family:\"inherit test\"}" );
        ( "a CSS-wide keyword as the last word keeps the quotes",
          ".a{font-family:\"test inherit\"}",
          ".a{font-family:\"test inherit\"}" );
        ( "a CSS-wide keyword in the middle keeps the quotes",
          ".a{font-family:\"a unset b\"}",
          ".a{font-family:\"a unset b\"}" );
        ( "revert-layer keeps the quotes",
          ".a{font-family:\"revert-layer x\",sans-serif}",
          ".a{font-family:\"revert-layer x\",sans-serif}" );
        ( "the reserved default keeps the quotes",
          ".a{font-family:\"default x\"}",
          ".a{font-family:\"default x\"}" );
        ( "a keyword word is excluded in every ASCII case permutation",
          ".a{font-family:\"Foo INITIAL\"}",
          ".a{font-family:\"Foo INITIAL\"}" );
        ( "a generic family name as a sequence word keeps the quotes",
          ".a{font-family:\"Foo serif\",serif}",
          ".a{font-family:\"Foo serif\",serif}" );
        ( "a generic family name as the first word keeps the quotes",
          ".a{font-family:\"monospace Foo\"}",
          ".a{font-family:\"monospace Foo\"}" );
        ( "a reserved word keeps the quotes in the font shorthand",
          ".a{font:12px/1.5 \"inherit test\"}",
          ".a{font:12px/1.5 \"inherit test\"}" );
        ( "a reserved word keeps the quotes in the @font-face descriptor",
          "@font-face{font-family:\"revert serif\";src:url(a.woff2)}",
          "@font-face{font-family:\"revert serif\";src:url(a.woff2)}" );
        ( "a reserved word keeps the quotes in @font-feature-values",
          "@font-feature-values \"default x\"{@styleset{x:1}}",
          "@font-feature-values \"default x\"{@styleset{x:1}}" );
        ( "a bare emoji is no generic in the grammar and still unquotes",
          ".a{font-family:\"Noto Color Emoji\"}",
          ".a{font-family:Noto Color Emoji}" );
        ( "a word that merely contains a keyword still unquotes",
          ".a{font-family:\"inherited serifs\"}",
          ".a{font-family:inherited serifs}" );
        ( "an ordinary multi-word name still unquotes",
          ".a{font-family:\"Times New Roman\",serif}",
          ".a{font-family:Times New Roman,serif}" );
      ]
  with
  | [] -> ()
  | mismatches ->
      Alcotest.failf "family names spelled with a reserved word:\n%s"
        (String.concat "\n" mismatches)

(* [src] has one value type and one reader, so the [@font-face] descriptor and
   an [src:] declaration are two routes into one printer and must spell the same
   value the same way. CSS Fonts 4 sec. 4.3: [<font-src> = <url>
   [format(<font-format>)]? [tech(<font-tech>#)]? | local(<font-family-name>)],
   with [<font-family-name> = <string> | <custom-ident>+] (CSS Fonts 4 sec.
   2.1.1) and [<font-format>] a [<string>] or one of the seven format keywords.
   Each of the two notations a [<url>], a family name and a format spells is
   valid, so minify picks the shorter one and pretty keeps the authored one.
   Every route is checked minified and pretty, and each output is re-read in
   strict mode: what one route emits, the other reads back. *)
let check_src_printer_routes (name, value, minified, pretty) =
  let fail fmt = Fmt.kstr (fun s -> Some s) fmt in
  let route label ~minify input expected =
    match Css.of_string ~strict:false input with
    | Error e ->
        fail "%s / %s: parse rejected %S\n  %s" name label input
          (Error.to_string e)
    | Ok { Css.stylesheet; _ } -> (
        let actual = Css.to_string ~minify stylesheet |> String.trim in
        if not (String.equal actual expected) then
          fail "%s / %s\n  input:    %S\n  expected: %S\n  actual:   %S" name
            label input expected actual
        else
          match Css.of_string ~strict:true actual with
          | Error e ->
              fail "%s / %s: output does not read back: %S\n  %s" name label
                actual (Error.to_string e)
          | Ok _ -> None)
  in
  let descriptor v =
    String.concat "" [ "@font-face{font-family:F;src:"; v; "}" ]
  in
  let declaration v = String.concat "" [ ".a{src:"; v; "}" ] in
  List.filter_map Fun.id
    [
      route "@font-face descriptor, minified" ~minify:true (descriptor value)
        (descriptor minified);
      route "src declaration, minified" ~minify:true (declaration value)
        (declaration minified);
      route "@font-face descriptor, pretty" ~minify:false (descriptor value)
        (String.concat ""
           [ "@font-face {\n  font-family: F;\n  src: "; pretty; ";\n}" ]);
      route "src declaration, pretty" ~minify:false (declaration value)
        (String.concat "" [ ".a {\n  src: "; pretty; ";\n}" ]);
    ]

let spec_src_printer_routes () =
  match
    List.concat_map check_src_printer_routes
      [
        (* CSS Values 4 sec. 3.4: [url(<string>)] and the bare [<url-token>]
           spell the same URL, so the quotes go under minify unless the body
           holds a character the bare form cannot carry. *)
        ( "a quoted url drops its quotes under minify",
          "url(\"a.woff2\")",
          "url(a.woff2)",
          "url(\"a.woff2\")" );
        ( "a single-quoted url drops its quotes under minify",
          "url('a.woff2')",
          "url(a.woff2)",
          "url('a.woff2')" );
        ("a bare url stays bare", "url(a.woff2)", "url(a.woff2)", "url(a.woff2)");
        ( "a url holding a space keeps its quotes",
          "url(\"a b.woff2\")",
          "url(\"a b.woff2\")",
          "url(\"a b.woff2\")" );
        (* CSS Fonts 4 sec. 2.1.1: a [<font-family-name>] is a [<string>] or a
           [<custom-ident>+], so an ident name drops its quotes under minify. *)
        ( "a local ident name drops its quotes under minify",
          "local(\"Arial\")",
          "local(Arial)",
          "local(\"Arial\")" );
        ( "a bare local ident name reads as the same name",
          "local(Arial)",
          "local(Arial)",
          "local(\"Arial\")" );
        ( "a local name with an underscore drops its quotes",
          "local(\"Brand_2\")",
          "local(Brand_2)",
          "local(\"Brand_2\")" );
        ( "a local name holding a space keeps its quotes",
          "local(\"Noto Sans\")",
          "local(\"Noto Sans\")",
          "local(\"Noto Sans\")" );
        (* CSS Values 4 sec. 3.3: the CSS-wide keywords and the reserved
           [default] are not valid [<custom-ident>]s, in every ASCII case
           permutation, so those names have only the [<string>] spelling. *)
        ( "a local CSS-wide keyword name keeps its quotes",
          "local(\"inherit\")",
          "local(\"inherit\")",
          "local(\"inherit\")" );
        ( "the reserved default keeps its quotes in local()",
          "local(\"default\")",
          "local(\"default\")",
          "local(\"default\")" );
        ( "the reserved default is excluded in every ASCII case permutation",
          "local(\"DEFAULT\")",
          "local(\"DEFAULT\")",
          "local(\"DEFAULT\")" );
        (* CSS Syntax 3 sec. 4.3.9: a name starting with a digit does not start
           an ident sequence, so it has no unquoted spelling. *)
        ( "a local name starting with a digit keeps its quotes",
          "local(\"2Cool\")",
          "local(\"2Cool\")",
          "local(\"2Cool\")" );
        (* CSS Fonts 4 sec. 4.3: [<font-format>] is a [<string>] or one of
           [collection], [embedded-opentype], [opentype], [svg], [truetype],
           [woff] and [woff2]; only those seven have a bare spelling. *)
        ( "a known format keyword drops its quotes under minify",
          "url(\"a.woff2\") format(\"woff2\")",
          "url(a.woff2)format(woff2)",
          "url(\"a.woff2\") format(\"woff2\")" );
        ( "a format string that is no keyword keeps its quotes",
          "url(a.woff2) format(\"weird thing\")",
          "url(a.woff2)format(\"weird thing\")",
          "url(a.woff2) format(\"weird thing\")" );
        ( "format and tech both follow the url",
          "url(a.woff2) format(\"truetype\") tech(variations)",
          "url(a.woff2)format(truetype)tech(variations)",
          "url(a.woff2) format(\"truetype\") tech(variations)" );
        ( "a source list keeps its order and separator",
          "local(\"Brand\"),url(\"b.woff2\") format(\"woff2\")",
          "local(Brand),url(b.woff2)format(woff2)",
          "local(\"Brand\"), url(\"b.woff2\") format(\"woff2\")" );
      ]
  with
  | [] -> ()
  | mismatches ->
      Alcotest.failf "src printer routes disagree with the spec oracle:\n%s"
        (String.concat "\n" mismatches)

let suite =
  let open Alcotest in
  ( "font_face",
    [
      test_case "metric_override to_string" `Quick
        test_string_of_metric_override;
      test_case "size_adjust" `Quick test_size_adjust;
      test_case "src" `Quick test_src;
      test_case "spec src parser vectors" `Quick test_spec_src_parser_vectors;
      test_case "spec src invalid vectors" `Quick test_spec_src_invalid_vectors;
      test_case "spec metric parsing edges" `Quick
        test_spec_metric_parsing_edges;
      test_case "spec metric negative vectors" `Quick
        test_spec_metric_negative_vectors;
      test_case "spec font-face level 4/5 source edges" `Quick
        spec_fontface_source_edges;
      test_case "spec font-face metric descriptor edges" `Quick
        spec_fontface_metric_edges;
      test_case "spec font-face src minify edges" `Quick
        spec_fontface_src_minify_edges;
      test_case "spec font-face src invalid edges" `Quick
        spec_fontface_src_invalid_edges;
      test_case "spec font-face metric numeric edges" `Quick
        spec_fontface_metric_numeric_edges;
      test_case "spec font-face var() descriptor kept" `Quick
        spec_fontface_var_descriptor_kept;
      test_case "spec font-face var() descriptor edges" `Quick
        spec_fontface_var_descriptor_edges;
      test_case "spec font-face family descriptor verbatim" `Quick
        spec_fontface_family_descriptor_verbatim;
      test_case "spec family name ident start" `Quick
        spec_family_name_ident_start;
      test_case "spec family name reserved words" `Quick
        spec_family_name_reserved_words;
      test_case "spec src printer routes" `Quick spec_src_printer_routes;
    ] )
