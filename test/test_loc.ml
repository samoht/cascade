(** Loc module tests. *)

open Cascade

let to_string l = Loc.to_string l

let v () =
  Alcotest.(check string)
    "v" "[3-7]"
    (to_string (Loc.v ~start_pos:3 ~end_pos:7))

let dummy () = Alcotest.(check string) "dummy" "[0-0]" (to_string Loc.dummy)

let union_disjoint () =
  let a = Loc.v ~start_pos:3 ~end_pos:5 in
  let b = Loc.v ~start_pos:10 ~end_pos:12 in
  Alcotest.(check string) "disjoint" "[3-12]" (to_string (Loc.union a b))

let union_overlapping () =
  let a = Loc.v ~start_pos:3 ~end_pos:8 in
  let b = Loc.v ~start_pos:5 ~end_pos:10 in
  Alcotest.(check string) "overlap" "[3-10]" (to_string (Loc.union a b))

let union_with_dummy () =
  let a = Loc.v ~start_pos:3 ~end_pos:8 in
  Alcotest.(check string)
    "dummy left absorbs" "[0-8]"
    (to_string (Loc.union Loc.dummy a))

let suite =
  ( "loc",
    [
      Alcotest.test_case "v" `Quick v;
      Alcotest.test_case "dummy" `Quick dummy;
      Alcotest.test_case "union disjoint" `Quick union_disjoint;
      Alcotest.test_case "union overlapping" `Quick union_overlapping;
      Alcotest.test_case "union with dummy" `Quick union_with_dummy;
    ] )
