open Cascade

let block css =
  match Css.of_string css with
  | Ok { stylesheet; _ } -> stylesheet
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let render stmts = Css.to_string ~minify:true stmts
let id stmts = stmts

let test_merge_layers_combines_same_name () =
  let stmts = block "@layer a{.x{color:red}}@layer a{.y{color:blue}}" in
  let merged = Block.merge_consecutive_layers ~optimize_merged_block:id stmts in
  let out = render merged in
  Alcotest.(check string)
    "two same-name @layer blocks merge into one"
    "@layer a{.x{color:red}.y{color:blue}}" out

let test_merge_layers_keeps_distinct_names () =
  let stmts = block "@layer a{.x{color:red}}@layer b{.y{color:blue}}" in
  let merged = Block.merge_consecutive_layers ~optimize_merged_block:id stmts in
  let out = render merged in
  Alcotest.(check string)
    "different-name @layer blocks stay separate"
    "@layer a{.x{color:red}}@layer b{.y{color:blue}}" out

let test_merge_media_combines_same_condition () =
  let stmts =
    block
      "@media (min-width:1px){.x{color:red}}@media \
       (min-width:1px){.y{color:blue}}"
  in
  let merged = Block.merge_consecutive_media ~optimize_merged_block:id stmts in
  let out = render merged in
  Alcotest.(check bool)
    ("two same-cond @media blocks merge: " ^ out)
    true
    (let needle = ".y" in
     let len = String.length needle in
     let found = ref false in
     for i = 0 to String.length out - len do
       if String.sub out i len = needle then found := true
     done;
     !found
     &&
     (* both bodies in one @media *)
     let media_count = ref 0 in
     String.iter (fun c -> if c = '@' then incr media_count) out;
     !media_count = 1)

let test_drop_empty_rules () =
  let stmts = block ".a{}.b{color:red}.c{}" in
  let out = render (Block.drop_empty_rules stmts) in
  Alcotest.(check string) "empty rules removed" ".b{color:red}" out

let test_is_layer_empty () =
  let stmts = block "@layer a{}" in
  match stmts with
  | [ Stylesheet.Layer (_, body) ] ->
      Alcotest.(check bool) "empty layer body" true (Block.is_layer_empty body)
  | _ -> Alcotest.fail "expected one layer statement"

let suite =
  ( "block",
    [
      Alcotest.test_case "merge same-name @layer" `Quick
        test_merge_layers_combines_same_name;
      Alcotest.test_case "keep distinct-name @layer" `Quick
        test_merge_layers_keeps_distinct_names;
      Alcotest.test_case "merge same-condition @media" `Quick
        test_merge_media_combines_same_condition;
      Alcotest.test_case "drop empty rules" `Quick test_drop_empty_rules;
      Alcotest.test_case "is_layer_empty" `Quick test_is_layer_empty;
    ] )
