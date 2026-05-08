open Cascade
open Css.Font_face

let test_string_of_metric_override () =
  Alcotest.(check string) "normal" "normal" (string_of_metric_override Normal);
  Alcotest.(check string)
    "percent" "110%"
    (string_of_metric_override (Percent 110.))

let test_size_adjust () =
  let s = string_of_size_adjust 90. in
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
  with Reader.Parse_error _ | Invalid_argument _ | Failure _ -> ()

let expect_metric_rejected input =
  try
    let metric = metric_override_of_string input in
    Alcotest.failf "invalid font metric parsed: %S -> %S" input
      (string_of_metric_override metric)
  with Reader.Parse_error _ | Invalid_argument _ | Failure _ -> ()

let expect_size_adjust_rejected input =
  try
    let size_adjust = size_adjust_of_string input in
    Alcotest.failf "invalid font size-adjust parsed: %S -> %g" input size_adjust
  with Reader.Parse_error _ | Invalid_argument _ | Failure _ -> ()

let test_spec_src_parser_vectors () =
  let check_src name input expected =
    let parsed = src_of_string input in
    Alcotest.(check bool) name true (parsed = expected);
    Alcotest.(check bool)
      (name ^ " serialization idempotent")
      true
      (src_of_string (string_of_src parsed) = parsed)
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
  Alcotest.(check (float 0.0001))
    "size adjust percentage" 87.5
    (size_adjust_of_string "87.5%")

let test_spec_metric_negative_vectors () =
  List.iter expect_metric_rejected [ "-1%"; "auto"; "100"; "calc(1%)" ];
  Alcotest.(check string)
    "over 100 metric override still parses" "120%"
    (metric_override_of_string "120%" |> string_of_metric_override);
  List.iter expect_size_adjust_rejected
    [ "-10%"; "normal"; "auto"; "100"; "calc(100%)" ];
  Alcotest.(check (float 0.0001))
    "zero size adjust parses" 0.
    (size_adjust_of_string "0%")

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
      Alcotest.(check (float 0.0001))
        input expected
        (size_adjust_of_string input))
    [ ("0%", 0.); ("100%", 100.); ("125.5%", 125.5); (" 87.25% ", 87.25) ]

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
    ] )
