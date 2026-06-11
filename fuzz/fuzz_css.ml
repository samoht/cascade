(** Fuzz tests for the public Css module surface. *)

open Cascade
open Alcobar

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte_at buf i mod List.length xs)

let cssish buf =
  let alphabet =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 \
     -_#@.%(),;:![]{}\\\"'/*\n\
     \t"
  in
  let n = String.length alphabet in
  String.map (fun c -> alphabet.[Char.code c mod n]) buf

let ident buf i =
  pick [ "card"; "button"; "title"; "panel"; "theme"; "item" ] buf i

let declaration_text buf i =
  pick
    [
      "color:red";
      "color:var(--fg,#000)";
      "display:grid";
      "margin:1px 2px 3px 4px";
      "width:calc(100% - 2rem)";
      "--fg:oklch(50% .2 30)";
      "font:italic 700 1rem/1.5 \"Brand\",sans-serif";
    ]
    buf i

let rule_text buf i =
  "." ^ ident buf i ^ "{"
  ^ declaration_text buf (i + 1)
  ^ ";"
  ^ declaration_text buf (i + 2)
  ^ "}"

let valid_snippet_text buf i =
  pick
    [
      rule_text buf i;
      "@media (width >= 30em){" ^ rule_text buf (i + 1) ^ "}";
      "@supports (display:grid){" ^ rule_text buf (i + 1) ^ "}";
      "@container card (inline-size > 30em){" ^ rule_text buf (i + 1) ^ "}";
      "@layer " ^ ident buf i ^ "{" ^ rule_text buf (i + 1) ^ "}";
      "@scope (.card) to (.footer){" ^ rule_text buf (i + 1) ^ "}";
      "@font-face{font-family:Brand;src:url(brand.woff2);unicode-range:U+4??}";
      "@property --" ^ ident buf i
      ^ "{syntax:\"<length>\";inherits:false;initial-value:1px}";
      "@keyframes fade{from{opacity:0}to{opacity:1}}";
      "@import url(theme.css) layer(theme);@namespace svg \
       url(http://www.w3.org/2000/svg);svg|a{color:red}";
    ]
    buf i

let invalid_snippet_text buf i =
  pick
    [
      ".bad:not(){color:red}";
      ".x{color:rgb(255 0);color:red}";
      ".x{width:calc(1px + );color:red}";
      "@media screen{@import url(inner.css);}";
      "@supports selector(){.x{color:red}}";
      "@font-face{src:url(brand.woff2)}";
      "@import url(theme.css){.x{color:red}}";
    ]
    buf i

let css_like_text buf =
  let count = 1 + (byte_at buf 0 mod 3) in
  let rec loop n i acc =
    if n = 0 then String.concat "" (List.rev acc)
    else
      let snippet =
        if byte_at buf i mod 4 = 0 then invalid_snippet_text buf i
        else valid_snippet_text buf i
      in
      loop (n - 1) (i + 7) (snippet :: acc)
  in
  loop count 1 []

let generated_stylesheet buf =
  let selector name = Css.Selector.class_ name in
  let color i =
    Css.color (Css.hex (pick [ "#ff0000"; "#00ff00"; "#0000ff" ] buf i))
  in
  Css.v
    [
      Css.rule ~selector:(selector "card") [ color 0 ];
      Css.media
        ~condition:(Css.Media.of_string "(width >= 40em)")
        [
          Css.supports
            ~condition:(Css.Supports.property "display" "grid")
            [
              Css.container
                ~condition:(Css.Container.of_string "(inline-size > 30em)")
                [ Css.rule ~selector:(selector "inside") [ color 4 ] ];
            ];
        ];
      Css.layer
        ~name:(pick [ "base"; "theme"; "components" ] buf 8)
        [ Css.rule ~selector:(selector "layered") [ color 12 ] ];
    ]

let generated_api_stylesheet buf =
  let selector name = Css.Selector.class_ name in
  let prop i =
    Css.custom_property
      ("--fuzz-" ^ string_of_int i ^ "-" ^ string_of_int (byte_at buf i))
      (pick [ "0"; "1rem"; "red"; "var(--fallback)" ] buf (i + 1))
  in
  Css.v
    [
      Css.layer ~name:"theme"
        [
          Css.rule ~selector:(selector "root") [ prop 0 ];
          Css.media
            ~condition:(Css.Media.of_string "(prefers-color-scheme: dark)")
            [ Css.rule ~selector:(selector "dark") [ prop 2 ] ];
        ];
      Css.layer ~name:"utilities"
        [ Css.rule ~selector:(selector "utility") [ prop 4 ] ];
      Css.rule ~selector:(selector "card")
        ~nested:
          [
            Css.media
              ~condition:(Css.Media.of_string "(width >= 40em)")
              [
                Css.rule ~selector:(selector "wide")
                  [ Css.color (Css.hex "#0000ff") ];
              ];
            Css.declarations [ Css.background_color (Css.hex "#ffffff") ];
          ]
        [ Css.color (Css.hex "#ff0000") ];
      Css.starting_style
        [
          Css.rule ~selector:(selector "entry")
            [
              Css.opacity
                (Css.Opacity_number
                   ((float_of_int (byte_at buf 6) +. 1.) /. 256.));
            ];
        ];
    ]

let minified ss = Css.to_string ~minify:true ss |> String.trim

let recovered_css label input =
  match Css.of_string ~strict:false input with
  | Ok parsed -> parsed
  | Error err ->
      failf "%s did not recover leniently: %s" label
        (Cascade.Error.to_string err)

let assert_strict_lenient_contract label input =
  let lenient = recovered_css label input in
  match Css.of_string ~strict:true input with
  | Ok strict_result ->
      if lenient.warnings <> [] then
        failf "%s: strict accepted but lenient warned: %S" label input;
      let strict_output = minified strict_result.Css.stylesheet in
      let lenient_output = minified lenient.stylesheet in
      if strict_output <> lenient_output then
        failf "%s: strict/lenient serialization diverged: %S -> %S / %S" label
          input strict_output lenient_output
  | Error _ ->
      ignore (minified lenient.stylesheet : string);
      if lenient.warnings = [] then
        failf "%s: strict rejected but lenient recovered silently: %S" label
          input

let test_parse_crash_safety buf =
  ignore (Css.of_string ~strict:false (cssish buf));
  ignore (Css.of_string_exn ~strict:false (cssish buf));
  let css = css_like_text buf in
  ignore (Css.of_string ~strict:false css);
  ignore (Css.of_string_exn ~strict:false css)

let test_parse_always_recovers_bytes buf =
  let parsed = recovered_css "parse always recovers bytes" buf in
  ignore (minified parsed.stylesheet : string)

let test_strict_lenient_cssish_contract buf =
  assert_strict_lenient_contract "cssish public parse contract" (cssish buf)

let test_strict_lenient_structured_contract buf =
  assert_strict_lenient_contract "structured public parse contract"
    (css_like_text buf)

let test_generated_public_roundtrip buf =
  let sheet = generated_stylesheet buf in
  let once = minified sheet in
  match Css.of_string ~strict:false once with
  | Error err ->
      failf "public generated stylesheet did not parse: %s"
        (Cascade.Error.to_string err)
  | Ok parsed ->
      let twice = minified parsed.stylesheet in
      if once <> twice then
        failf "public generated stylesheet changed: %S -> %S" once twice

let test_parse_partial_stringify_reparse buf =
  let parsed = recovered_css "partial stringify reparse" (css_like_text buf) in
  if parsed.Css.warnings = [] then
    let serialized = minified parsed.stylesheet in
    match Css.of_string ~strict:false serialized with
    | Ok _ -> ()
    | Error err ->
        failf "Css.of_string ~strict:false output did not reparse strictly: %s"
          (Cascade.Error.to_string err)

let test_map_preserves_rules buf =
  let sheet = generated_stylesheet buf in
  let before = List.length (Css.rule_statements sheet) in
  let mapped =
    Css.map
      (fun sel _decls ->
        Css.rule ~selector:sel [ Css.color (Css.hex "#000000") ])
      (Css.statements sheet)
    |> Css.v
  in
  let after = List.length (Css.rule_statements mapped) in
  if before <> after then
    failf "Css.map changed rule count: %d -> %d" before after

let test_public_sort_idempotent buf =
  let cmp (sel1, _) (sel2, _) =
    String.compare (Css.Selector.to_string sel1) (Css.Selector.to_string sel2)
  in
  let sheet = generated_stylesheet buf in
  let once = Css.sort cmp (Css.statements sheet) |> Css.v |> minified in
  let twice =
    match Css.of_string ~strict:false once with
    | Error err -> fail (Cascade.Error.to_string err)
    | Ok parsed ->
        Css.sort cmp (Css.statements parsed.stylesheet) |> Css.v |> minified
  in
  if once <> twice then
    failf "Css.sort changed after reparse: %S -> %S" once twice

let test_public_optimize_idempotent buf =
  let sheet = generated_stylesheet buf in
  let once = Css.to_string ~minify:true sheet in
  match Css.of_string ~strict:false once with
  | Error err -> fail (Cascade.Error.to_string err)
  | Ok parsed ->
      let twice = Css.to_string ~minify:true parsed.stylesheet in
      if once <> twice then
        failf "public optimize output changed after reparse: %S -> %S" once
          twice

let test_public_fold_count buf =
  let sheet = generated_api_stylesheet buf in
  let count =
    Css.fold
      (fun n stmt -> match Css.as_rule stmt with Some _ -> n + 1 | None -> n)
      0 sheet
  in
  if count <> 6 then failf "Css.fold visited %d rules instead of 6" count

let test_custom_props_scope buf =
  let sheet = generated_api_stylesheet buf in
  let all_props = Css.custom_props sheet in
  let theme_props = Css.custom_props ~layer:"theme" sheet in
  let util_props = Css.custom_props ~layer:"utilities" sheet in
  if List.length all_props < 3 then
    failf "Css.custom_props lost properties: %S" (String.concat "," all_props);
  List.iter
    (fun name ->
      if List.mem name util_props then
        failf "theme property leaked into utilities: %S" name)
    theme_props;
  List.iter
    (fun name ->
      if List.mem name theme_props then
        failf "utility property leaked into theme: %S" name)
    util_props

let test_public_theme_guard buf =
  let var_name = "fuzz-" ^ string_of_int (byte_at buf 0) in
  let guarded =
    Css.theme_guarded ~var_name
      (Css.color (Css.hex (pick [ "#ff0000"; "#00ff00" ] buf 1)))
  in
  let sheet =
    Css.v
      [
        Css.rule
          ~selector:(Css.Selector.class_ "card")
          [ guarded; Css.background_color (Css.hex "#ffffff") ];
      ]
  in
  let hidden =
    sheet
    |> Css.resolve_theme ~theme:Css.Pp.String_set.empty
    |> Css.to_string ~minify:true
  in
  let shown =
    let theme = Css.Pp.String_set.add var_name Css.Pp.String_set.empty in
    sheet |> Css.resolve_theme ~theme |> Css.to_string ~minify:true
  in
  if
    String.contains hidden '#' && Astring.String.is_infix ~affix:"ff0000" hidden
  then failf "theme guard emitted without theme: %S" hidden;
  if not (Astring.String.is_infix ~affix:"color:" shown) then
    failf "theme guard omitted with theme: %S" shown

let test_public_property_shape buf =
  let name = "--fuzz-" ^ string_of_int (byte_at buf 0) in
  let sheet =
    Css.property ~name Css.Length
      ~initial_value:(Css.Px (float_of_int (byte_at buf 1)))
      ()
  in
  match Css.statements sheet with
  | [ stmt ] -> (
      match Css.as_property stmt with
      | Some (Css.Property_info info) ->
          if info.name <> name then
            failf "@property name changed: %S -> %S" name info.name
      | None -> fail "Css.property did not create @property statement")
  | _ -> fail "Css.property did not create one statement"

let test_css2_legacy_minified_vectors buf =
  let input =
    pick
      [
        "body { margin: 0; color: black }";
        "@charset \"UTF-8\";";
        "@import 'legacy.css';";
        "@media print { body { color: black } }";
        "@page :left { margin-left: 4cm; margin-right: 3cm }";
        "html > body p + p { text-indent: 1em }";
        "a:link { color: blue } a:visited { color: purple }";
        "li:first-child { list-style-type: none }";
        "h1:first-letter { color: red }";
        "p::first-line { color: blue }";
        "q:before { content: open-quote }";
        "q::after { content: close-quote }";
        "div { page-break-before: always }";
        "div { page-break-after: avoid }";
        "div { page-break-inside: avoid }";
        "table > caption + colgroup col { visibility: collapse }";
        "ol li { list-style: decimal inside }";
        "pre { white-space: pre; tab-size: 4 }";
        "img { float: left; clear: both; vertical-align: middle }";
        "@media print { h1 { page-break-before: always } }";
        "@page chapter:right { margin: 2cm; size: A4 }";
      ]
      buf 0
  in
  match Css.of_string ~strict:false input with
  | Error _ -> ()
  | Ok parsed -> (
      let minified = Css.to_string ~minify:true parsed.stylesheet in
      match Css.of_string ~strict:false minified with
      | Ok _ -> ()
      | Error err ->
          failf "CSS2 legacy minified output rejected: %S (%s)" minified
            (Cascade.Error.to_string err))

let test_css2_legacy_invalid_vectors buf =
  let input =
    pick
      [
        "@charset 'UTF-8';";
        "@page :unknown { margin: 1cm }";
        "div { page-break-before: always avoid }";
        "div { page-break-inside: left }";
        "h1::first-line::before { color: red }";
        "a + { color: red }";
        "table > > td { color: red }";
        "@page { @top-center { display: block } }";
        "ol { list-style-position: center }";
        "p { vertical-align: left right }";
        "q { content: open-quote none }";
      ]
      buf 0
  in
  assert_strict_lenient_contract "CSS2 legacy invalid vector" input

let suite =
  ( "css",
    [
      test_case "parse crash safety" [ bytes ] test_parse_crash_safety;
      test_case "parse always recovers bytes" [ bytes ]
        test_parse_always_recovers_bytes;
      test_case "strict/lenient cssish contract" [ bytes ]
        test_strict_lenient_cssish_contract;
      test_case "strict/lenient structured contract" [ bytes ]
        test_strict_lenient_structured_contract;
      test_case "generated public roundtrip" [ bytes ]
        test_generated_public_roundtrip;
      test_case "parse partial stringify reparse" [ bytes ]
        test_parse_partial_stringify_reparse;
      test_case "public map preserves rule count" [ bytes ]
        test_map_preserves_rules;
      test_case "public sort idempotent" [ bytes ] test_public_sort_idempotent;
      test_case "public optimize idempotent" [ bytes ]
        test_public_optimize_idempotent;
      test_case "public fold counts nested rules" [ bytes ]
        test_public_fold_count;
      test_case "public custom_props layer scope" [ bytes ]
        test_custom_props_scope;
      test_case "public theme guard rendering" [ bytes ] test_public_theme_guard;
      test_case "public property shape" [ bytes ] test_public_property_shape;
      test_case "CSS2 legacy minified vectors" [ bytes ]
        test_css2_legacy_minified_vectors;
      test_case "CSS2 legacy invalid vectors rejected" [ bytes ]
        test_css2_legacy_invalid_vectors;
    ] )
