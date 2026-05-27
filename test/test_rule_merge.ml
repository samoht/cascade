(** Rule_merge module tests. *)

open Cascade

let rules_of css =
  match Css.of_string ~strict:false css with
  | Ok { stylesheet; _ } ->
      List.filter_map
        (function Stylesheet.Rule r -> Some r | _ -> None)
        (Css.statements stylesheet)
  | Error _ -> []

let render rs =
  String.concat ""
    (List.map (fun r -> Pp.to_string ~minify:true Stylesheet.pp_rule r) rs)

let combine css = render (Rule_merge.combine_identical (rules_of css))

let check name css expected =
  Alcotest.(check string) name expected (combine css)

let pair () =
  check "two identical merge" ".a{color:red}.b{color:red}" ".a,.b{color:red}"

let different () =
  check "different blocks untouched" ".a{color:red}.b{color:blue}"
    ".a{color:red}.b{color:blue}"

let run3 () =
  check "run of three" ".a{color:red}.b{color:red}.c{color:red}"
    ".a,.b,.c{color:red}"

let non_adjacent () =
  (* .a and .c are identical but not adjacent: not merged by this pass *)
  check "non-adjacent not merged" ".a{color:red}.b{color:blue}.c{color:red}"
    ".a{color:red}.b{color:blue}.c{color:red}"

let two_runs () =
  check "two separate runs"
    ".a{color:red}.b{color:red}.c{color:blue}.d{color:blue}"
    ".a,.b{color:red}.c,.d{color:blue}"

let suite =
  ( "rule_merge",
    [
      Alcotest.test_case "pair" `Quick pair;
      Alcotest.test_case "different" `Quick different;
      Alcotest.test_case "run of three" `Quick run3;
      Alcotest.test_case "non-adjacent" `Quick non_adjacent;
      Alcotest.test_case "two runs" `Quick two_runs;
    ] )
