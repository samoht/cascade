(** Tests for CSS pretty-printing module *)

open Css

let test_minified () =
  let result = Pp.to_string ~minify:true Pp.string "test" in
  Alcotest.(check string) "minified output" "test" result

let test_pretty () =
  let result = Pp.to_string ~minify:false Pp.string "test" in
  Alcotest.(check string) "pretty output" "test" result

let test_indent () =
  let result = Pp.to_string ~minify:false (Pp.indent Pp.string) "test" in
  Alcotest.(check string) "indented output" "  test" result

let test_block () =
  let formatter = Pp.braces Pp.string in
  let result = Pp.to_string ~minify:true formatter "content" in
  Alcotest.(check string) "block formatting" "{content}" result

let test_list () =
  let formatter = Pp.list ~sep:Pp.comma Pp.string in
  let result = Pp.to_string ~minify:true formatter [ "a"; "b"; "c" ] in
  Alcotest.(check string) "list formatting" "a,b,c" result

let test_float () =
  let result = Pp.to_string ~minify:true Pp.float 3.14159 in
  Alcotest.(check bool) "float formatting" true (String.length result > 0)

let test_float_leading_zero () =
  let pretty = Pp.to_string Pp.float 0.5 in
  Alcotest.(check string) "pretty keeps leading zero" "0.5" pretty;
  let minified = Pp.to_string ~minify:true Pp.float 0.5 in
  Alcotest.(check string) "minified drops leading zero" ".5" minified

let test_float_negative_leading_zero () =
  let pretty = Pp.to_string Pp.float (-0.5) in
  Alcotest.(check string) "pretty negative keeps zero" "-0.5" pretty;
  let minified = Pp.to_string ~minify:true Pp.float (-0.5) in
  Alcotest.(check string) "minified negative drops zero" "-.5" minified

let test_float_zero_and_nan_inf () =
  let zero = Pp.to_string Pp.float 0.0 in
  Alcotest.(check string) "zero formats as 0" "0" zero;
  let neg_zero = Pp.to_string Pp.float (-0.0) in
  Alcotest.(check string) "-0 formats as 0" "0" neg_zero;
  let pinf = Pp.to_string Pp.float infinity in
  Alcotest.(check string) "+inf clamps" "3.40282e38" pinf;
  let ninf = Pp.to_string Pp.float neg_infinity in
  Alcotest.(check string) "-inf clamps" "-3.40282e38" ninf;
  let nanv = Pp.to_string Pp.float nan in
  Alcotest.(check string) "NaN becomes 0" "0" nanv

let test_float_rounding_and_trim () =
  let r_up = Pp.to_string (Pp.float_n 3) 1.23456 in
  Alcotest.(check string) "round up" "1.235" r_up;
  let r_down = Pp.to_string (Pp.float_n 3) 1.23444 in
  Alcotest.(check string) "round down" "1.234" r_down;
  let trim = Pp.to_string (Pp.float_n 4) 1.2300 in
  Alcotest.(check string) "trim trailing zeros" "1.23" trim;
  let min_drop = Pp.to_string ~minify:true (Pp.float_n 3) 0.25 in
  Alcotest.(check string) "minified leading zero drop with n" ".25" min_drop

let test_cond () =
  let formatter = Pp.cond (fun ctx -> not (Pp.minified ctx)) Pp.string Pp.nop in
  let minified = Pp.to_string ~minify:true formatter "test" in
  let pretty = Pp.to_string ~minify:false formatter "test" in
  Alcotest.(check string) "minified conditional" "" minified;
  Alcotest.(check string) "pretty conditional" "test" pretty

let test_space_if_pretty () =
  let minified = Pp.to_string ~minify:true Pp.space_if_pretty () in
  let pretty = Pp.to_string ~minify:false Pp.space_if_pretty () in
  Alcotest.(check string) "minified space" "" minified;
  Alcotest.(check string) "pretty space" " " pretty

let tests =
  [
    ("minified output", `Quick, test_minified);
    ("pretty output", `Quick, test_pretty);
    ("indentation", `Quick, test_indent);
    ("block formatting", `Quick, test_block);
    ("list formatting", `Quick, test_list);
    ("float formatting", `Quick, test_float);
    ("float leading zero", `Quick, test_float_leading_zero);
    ("float negative leading zero", `Quick, test_float_negative_leading_zero);
    ("float zero/nan/inf", `Quick, test_float_zero_and_nan_inf);
    ("float rounding and trim", `Quick, test_float_rounding_and_trim);
    ("conditional output", `Quick, test_cond);
    ("space_if_pretty", `Quick, test_space_if_pretty);
  ]

let suite = [ ("pp", tests) ]
