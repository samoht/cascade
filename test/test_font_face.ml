open Cascade
open Css.Font_face

let test_metric_override_to_string () =
  Alcotest.(check string) "normal" "normal" (metric_override_to_string Normal);
  Alcotest.(check string)
    "percent" "110%"
    (metric_override_to_string (Percent 110.))

let test_size_adjust () =
  let s = size_adjust_to_string 90. in
  Alcotest.(check string) "size-adjust 90%" "90%" s

let test_src () =
  let s = src_to_string [ Local "Arial" ] in
  Alcotest.(check bool) "local src" true (String.length s > 0);
  Alcotest.(check string)
    "url source with format and tech"
    "url(\"fonts/color.woff2\") format(\"woff2\") tech(color-COLRv1)"
    (src_entry_to_string
       (Url
          {
            url = "fonts/color.woff2";
            format = Some "woff2";
            tech = Some "color-COLRv1";
          }));
  Alcotest.(check string)
    "src list preserves order"
    "local(\"Brand\"), url(\"brand.woff2\") format(\"woff2\")"
    (src_to_string
       [
         Local "Brand";
         Url { url = "brand.woff2"; format = Some "woff2"; tech = None };
       ]);
  Alcotest.(check string)
    "raw source escape hatch" "url(font.woff2) tech(variations)"
    (src_to_string [ Raw "url(font.woff2) tech(variations)" ])

let test_spec_src_parser_vectors () =
  let check_raw name input =
    Alcotest.(check string) name input (src_of_string input |> src_to_string)
  in
  check_raw "url format tech raw preservation"
    "url(\"color.woff2\") format(\"woff2\") tech(color-COLRv1)";
  check_raw "local and url list raw preservation"
    "local(\"Brand\"), url(\"brand.woff2\") format(\"woff2\")";
  check_raw "source with format collection"
    "url(\"brand.otf\") format(\"opentype\")";
  check_raw "source with tech collection"
    "url(\"variations.woff2\") tech(variations)"

let test_spec_metric_parsing_edges () =
  Alcotest.(check string)
    "normal parses" "normal"
    (metric_override_of_string " normal " |> metric_override_to_string);
  Alcotest.(check string)
    "percentage parses" "92.5%"
    (metric_override_of_string "92.5%" |> metric_override_to_string);
  Alcotest.(check string)
    "invalid metric falls back to normal" "normal"
    (metric_override_of_string "auto" |> metric_override_to_string);
  Alcotest.(check (float 0.0001))
    "size adjust percentage" 87.5
    (size_adjust_of_string "87.5%");
  Alcotest.(check (float 0.0001))
    "invalid size adjust fallback" 100.
    (size_adjust_of_string "normal")

let test_spec_metric_negative_vectors () =
  Alcotest.(check string)
    "negative metric override is invalid" "normal"
    (metric_override_of_string "-1%" |> metric_override_to_string);
  Alcotest.(check string)
    "over 100 metric override still parses" "120%"
    (metric_override_of_string "120%" |> metric_override_to_string);
  Alcotest.(check (float 0.0001))
    "negative size adjust is invalid" 100.
    (size_adjust_of_string "-10%");
  Alcotest.(check (float 0.0001))
    "zero size adjust parses" 0.
    (size_adjust_of_string "0%")

let spec_fontface_source_edges () =
  let check_raw name input =
    Alcotest.(check string) name input (src_of_string input |> src_to_string)
  in
  check_raw "tech variations" "url(\"variable.woff2\") tech(variations)";
  check_raw "tech palettes" "url(\"color.woff2\") tech(palettes)";
  check_raw "tech incremental" "url(\"font.woff2\") tech(incremental)";
  check_raw "format collection" "url(\"collection.ttc\") format(\"collection\")";
  check_raw "multiple urls with local fallback"
    "local(\"Brand\"), url(\"brand.woff2\") format(\"woff2\"), \
     url(\"brand.otf\") format(\"opentype\")";
  check_raw "raw unknown source function"
    "url(\"font.woff2\") format(\"woff2\") tech(color-COLRv1)"

let spec_fontface_metric_edges () =
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string)
        input expected
        (metric_override_of_string input |> metric_override_to_string))
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
        test_metric_override_to_string;
      test_case "size_adjust" `Quick test_size_adjust;
      test_case "src" `Quick test_src;
      test_case "spec src parser vectors" `Quick test_spec_src_parser_vectors;
      test_case "spec metric parsing edges" `Quick
        test_spec_metric_parsing_edges;
      test_case "spec metric negative vectors" `Quick
        test_spec_metric_negative_vectors;
      test_case "spec font-face level 4/5 source edges" `Quick
        spec_fontface_source_edges;
      test_case "spec font-face metric descriptor edges" `Quick
        spec_fontface_metric_edges;
    ] )
