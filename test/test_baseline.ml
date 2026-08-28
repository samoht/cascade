(** Invariants of the generated {!Cascade.Baseline} greenfield list, so a broken
    regeneration (empty, everything, or the wrong side of the Baseline cut) is
    caught rather than silently changing which vendor prefixes are dropped. *)

open Cascade

let listed l name = Alcotest.(check bool) name true (List.mem name l)
let absent l name = Alcotest.(check bool) name false (List.mem name l)

let test_greenfield_present () =
  List.iter
    (listed Baseline.greenfield_properties)
    [ "anchor-name"; "view-transition-name"; "field-sizing"; "scroll-timeline" ]

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
  check "greenfield_properties" Baseline.greenfield_properties

let suite =
  ( "baseline",
    [
      Alcotest.test_case "not-yet-Baseline properties are listed" `Quick
        test_greenfield_present;
      Alcotest.test_case "Baseline properties are not listed" `Quick
        test_baseline_absent;
      Alcotest.test_case "the list is well-formed" `Quick test_well_formed;
    ] )
