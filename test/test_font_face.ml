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
  Alcotest.(check bool) "local src" true (String.length s > 0)

let suite =
  let open Alcotest in
  ( "font_face",
    [
      test_case "metric_override to_string" `Quick
        test_metric_override_to_string;
      test_case "size_adjust" `Quick test_size_adjust;
      test_case "src" `Quick test_src;
    ] )
