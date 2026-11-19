open Cascade

let rules css =
  match Css.of_string css with
  | Ok { stylesheet; _ } ->
      List.filter_map
        (function Stylesheet.Rule r -> Some r | _ -> None)
        stylesheet
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let cost css = Transfer_size.rules Size.default (rules css)

(* The model's constants, in thousandths of a byte, applied by hand: a token
   costs its bytes plus one separator. [.a] and [.b] are new (2420 + 61 * 3),
   [color:red] is new once (2420 + 61 * 10) and then one declaration match (1320
   + 5 * 10). *)
let test_repeated_declaration_is_a_match () =
  Alcotest.(check int)
    "a declaration printed before costs a match" 9606
    (cost ".a{color:red}.b{color:red}")

(* [top:0] follows [color:red] in both rules, so its second copy extends the
   match [color:red] started and costs its per-byte part alone (5 * 6). *)
let test_run_extends_the_match () =
  Alcotest.(check int)
    "the declaration after a match extends it" 12422
    (cost ".a{color:red;top:0}.b{color:red;top:0}")

(* Written in the other order, [top:0] does not follow its earlier copy's
   predecessor, so it starts a match of its own (1320 + 5 * 6). *)
let test_reordered_run_starts_a_match () =
  Alcotest.(check int)
    "a declaration out of order starts its own match" 13742
    (cost ".a{color:red;top:0}.b{top:0;color:red}")

let suite =
  ( "transfer_size",
    [
      Alcotest.test_case "repeated declaration is a match" `Quick
        test_repeated_declaration_is_a_match;
      Alcotest.test_case "run extends the match" `Quick
        test_run_extends_the_match;
      Alcotest.test_case "reordered run starts a match" `Quick
        test_reordered_run_starts_a_match;
    ] )
