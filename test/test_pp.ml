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
    ("conditional output", `Quick, test_cond);
    ("space_if_pretty", `Quick, test_space_if_pretty);
  ]

let suite = [ ("pp", tests) ]
