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

let suite =
  let open Alcotest in
  ( "font_face",
    [
      test_case "metric_override to_string" `Quick
        test_metric_override_to_string;
      test_case "size_adjust" `Quick test_size_adjust;
      test_case "src" `Quick test_src;
      test_case "spec metric parsing edges" `Quick
        test_spec_metric_parsing_edges;
    ] )
