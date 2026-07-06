open Cascade

let statements css =
  match Css.of_string ~strict:false css with
  | Ok { stylesheet; _ } -> Css.statements stylesheet
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let render stmts = Pp.to_string ~minify:true Stylesheet.pp_stylesheet stmts
let canonical css = statements css |> Rule_order.canonicalize |> render

let sorts_independent_rules_into_content_order () =
  Alcotest.(check string)
    "independent rules sort by serialized content" ".a{margin:0}.b{color:red}"
    (canonical ".b{color:red}.a{margin:0}")

let keeps_conflicting_rules_in_source_order () =
  Alcotest.(check string)
    "same selector and property remains ordered" ".a{color:red}.a{color:blue}"
    (canonical ".a{color:red}.a{color:blue}")

let custom_property_position_converges () =
  (* A custom-property definition and a [var()] user are cascade-independent, so
     both source orderings reach one canonical form. *)
  let want = ".x{filter:blur(var(--b))}:root{--b:1}" in
  Alcotest.(check string)
    "definition after the user" want
    (canonical ".x{filter:blur(var(--b))}:root{--b:1}");
  Alcotest.(check string)
    "definition before the user" want
    (canonical ":root{--b:1}.x{filter:blur(var(--b))}")

let suite =
  ( "rule_order",
    [
      Alcotest.test_case "sorts independent rules" `Quick
        sorts_independent_rules_into_content_order;
      Alcotest.test_case "keeps conflicting order" `Quick
        keeps_conflicting_rules_in_source_order;
      Alcotest.test_case "custom-property position converges" `Quick
        custom_property_position_converges;
    ] )
