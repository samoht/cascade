(** Invariants of the generated {!Cascade.Baseline} greenfield lists, so a
    broken regeneration (empty, everything, or the wrong side of the Baseline
    cut) is caught rather than silently changing which [@supports] guards are
    dropped. *)

open Cascade

let listed l name = Alcotest.(check bool) name true (List.mem name l)
let absent l name = Alcotest.(check bool) name false (List.mem name l)

let test_greenfield_present () =
  List.iter
    (listed Baseline.greenfield_properties)
    [ "anchor-name"; "view-transition-name"; "field-sizing"; "scroll-timeline" ];
  List.iter
    (listed Baseline.greenfield_value_functions)
    [ "anchor"; "anchor-size"; "calc-size" ]

let test_baseline_absent () =
  List.iter
    (absent Baseline.greenfield_properties)
    [ "display"; "color"; "width"; "margin"; "padding"; "opacity" ]

let test_well_formed () =
  let check name l =
    Alcotest.(check bool) (name ^ " non-empty") true (l <> []);
    Alcotest.(check bool)
      (name ^ " entries lowercase and non-empty")
      true
      (List.for_all (fun s -> s <> "" && s = String.lowercase_ascii s) l);
    Alcotest.(check int)
      (name ^ " no duplicates") (List.length l)
      (List.length (List.sort_uniq String.compare l))
  in
  check "greenfield_properties" Baseline.greenfield_properties;
  check "greenfield_value_functions" Baseline.greenfield_value_functions

let suite =
  ( "baseline",
    [
      Alcotest.test_case "not-yet-Baseline features are listed" `Quick
        test_greenfield_present;
      Alcotest.test_case "Baseline features are not listed" `Quick
        test_baseline_absent;
      Alcotest.test_case "lists are well-formed" `Quick test_well_formed;
    ] )
