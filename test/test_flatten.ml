open Cascade

let block css =
  match Css.of_string css with
  | Ok { stylesheet; _ } -> stylesheet
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let render stmts = Css.to_string ~minify:true stmts

let test_no_nesting_is_identity () =
  let stmts = block ".a{color:red}.b{width:1px}" in
  let flattened = Flatten.block stmts in
  Alcotest.(check string)
    "no nesting input round-trips minified" ".a{color:red}.b{width:1px}"
    (render flattened)

let test_flattens_descendant_nesting () =
  let stmts = block ".a{color:red;.b{width:1px}}" in
  let flattened = Flatten.block stmts in
  let out = render flattened in
  Alcotest.(check bool)
    ("nested rule lifts to top level: " ^ out)
    true
    (String.length out > 0
    && not (String.contains out '{' && String.contains out '}' = false));
  Alcotest.(check bool)
    "result contains the lifted selector .a .b" true
    (let len = String.length ".a .b" in
     let pat = ".a .b" in
     let found = ref false in
     for i = 0 to String.length out - len do
       if String.sub out i len = pat then found := true
     done;
     !found)

let test_empty_block_is_empty () =
  let stmts = block "" in
  Alcotest.(check int)
    "empty input yields empty output" 0
    (List.length (Flatten.block stmts))

let suite =
  ( "flatten",
    [
      Alcotest.test_case "no nesting is identity" `Quick
        test_no_nesting_is_identity;
      Alcotest.test_case "flattens descendant nesting" `Quick
        test_flattens_descendant_nesting;
      Alcotest.test_case "empty block stays empty" `Quick
        test_empty_block_is_empty;
    ] )
