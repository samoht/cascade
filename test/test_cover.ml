open Cascade

let decl css = Declaration.of_string css

let single_selector css =
  match Css.of_string css with
  | Ok { stylesheet; _ } -> (
      match
        List.find_map
          (function Stylesheet.Rule r -> Some r | _ -> None)
          stylesheet
      with
      | Some r -> r.selector
      | None -> Alcotest.fail "no rule")
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let sel css = Fmt.kstr single_selector "%s{x:1}" css

let test_empty_table_covers_nothing () =
  let t = Cover.v () in
  let s = sel ".a" in
  Alcotest.(check bool)
    "empty table reports no coverage" false
    (Cover.covered (Cover.written t s) (decl "color:red"))

let test_add_then_covered () =
  let t = Cover.v () in
  let s = sel ".a" in
  Cover.record t s (Cover.written t s) [ decl "color:red" ];
  Alcotest.(check bool)
    "same property reported covered" true
    (Cover.covered (Cover.written t s) (decl "color:blue"));
  Alcotest.(check bool)
    "different property not covered" false
    (Cover.covered (Cover.written t s) (decl "width:1px"))

let test_per_selector_isolation () =
  let t = Cover.v () in
  let a = sel ".a" in
  let b = sel ".b" in
  Cover.record t a (Cover.written t a) [ decl "color:red" ];
  Alcotest.(check bool)
    "coverage is selector-scoped" false
    (Cover.covered (Cover.written t b) (decl "color:blue"))

let test_importance_partitioning () =
  let t = Cover.v () in
  let s = sel ".a" in
  Cover.record t s (Cover.written t s) [ decl "color:red" ];
  Alcotest.(check bool)
    "normal does not cover important" false
    (Cover.covered (Cover.written t s) (decl "color:blue!important"));
  Cover.record t s (Cover.written t s) [ decl "color:red!important" ];
  Alcotest.(check bool)
    "important covers important" true
    (Cover.covered (Cover.written t s) (decl "color:blue!important"))

(* A rule asks about one selector and every declaration it carries, so the
   coverage those declarations are tested against is read once and answers for
   all of them. That is what makes the read a value rather than a view onto the
   table: what the rule records afterwards does not reach back into it. *)
let test_read_answers_for_the_whole_rule () =
  let t = Cover.v () in
  let s = sel ".a" in
  let before = Cover.written t s in
  Cover.record t s before [ decl "color:red" ];
  Alcotest.(check bool)
    "the read answers as of the rule, not as of the table" false
    (Cover.covered before (decl "color:blue"));
  Alcotest.(check bool)
    "and the next read sees the store" true
    (Cover.covered (Cover.written t s) (decl "color:blue"))

(* [record] extends the read it was given, so a rule's declarations reach the
   table in one store exactly as they would one store at a time. *)
let test_one_store_agrees_with_one_per_declaration () =
  let together = Cover.v () and apart = Cover.v () in
  let s = sel ".a" in
  let decls = [ decl "color:red"; decl "width:1px!important"; decl "top:0" ] in
  Cover.record together s (Cover.written together s) decls;
  List.iter (fun d -> Cover.record apart s (Cover.written apart s) [ d ]) decls;
  List.iter
    (fun (probe, expected) ->
      let d = decl probe in
      Alcotest.(check bool)
        (Fmt.str "one store on %s" probe)
        expected
        (Cover.covered (Cover.written together s) d);
      Alcotest.(check bool)
        (Fmt.str "one store per declaration on %s" probe)
        expected
        (Cover.covered (Cover.written apart s) d))
    [
      ("color:blue", true);
      ("width:2px", true);
      ("width:2px!important", true);
      ("top:1px", true);
      ("left:0", false);
      ("color:blue!important", false);
    ]

let suite =
  ( "cover",
    [
      Alcotest.test_case "empty table covers nothing" `Quick
        test_empty_table_covers_nothing;
      Alcotest.test_case "covered after add" `Quick test_add_then_covered;
      Alcotest.test_case "selector isolation" `Quick test_per_selector_isolation;
      Alcotest.test_case "importance partitioning" `Quick
        test_importance_partitioning;
      Alcotest.test_case "read answers for the whole rule" `Quick
        test_read_answers_for_the_whole_rule;
      Alcotest.test_case "one store agrees with one per declaration" `Quick
        test_one_store_agrees_with_one_per_declaration;
    ] )
