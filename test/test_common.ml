(** Common helper tests. *)

open Cascade
module L = Common.List

let ref_int = Alcotest.testable (fun p r -> Fmt.int p !r) ( == )

let check_same_list name expected actual =
  Alcotest.(check (list ref_int)) name expected actual

let test_same_uses_physical_elements () =
  let a = ref 1 and b = ref 2 in
  Alcotest.(check bool) "same elements" true (L.same [ a; b ] [ a; b ]);
  Alcotest.(check bool)
    "structurally equal refs are not same" false
    (L.same [ a ] [ ref 1 ]);
  Alcotest.(check bool) "different lengths" false (L.same [ a ] [ a; b ])

let test_preserve_returns_original_list_when_elements_same () =
  let a = ref 1 and b = ref 2 and c = ref 3 in
  let before = [ a; b ] in
  let same_elements = [ a; b ] in
  let changed = [ a; c ] in
  Alcotest.(check bool)
    "same physical elements preserve list identity" true
    (L.preserve before same_elements == before);
  Alcotest.(check bool)
    "changed element returns replacement list" true
    (L.preserve before changed == changed)

let test_map_preserve_keeps_noop_identity () =
  let a = ref 1 and b = ref 2 and c = ref 3 in
  let xs = [ a; b; c ] in
  Alcotest.(check bool)
    "identity map keeps original spine" true
    (L.map_preserve Fun.id xs == xs);
  let mapped = L.map_preserve (fun x -> if x == b then ref !x else x) xs in
  Alcotest.(check bool) "changed map allocates" false (mapped == xs);
  Alcotest.(check int) "length preserved" 3 (L.length mapped);
  match mapped with
  | [ a'; b'; c' ] ->
      Alcotest.(check bool) "prefix element reused" true (a' == a);
      Alcotest.(check bool) "changed element replaced" false (b' == b);
      Alcotest.(check bool) "suffix element reused" true (c' == c)
  | _ -> Alcotest.fail "expected three mapped elements"

let test_filter_preserve_keeps_noop_identity () =
  let a = ref 1 and b = ref 2 and c = ref 3 in
  let xs = [ a; b; c ] in
  Alcotest.(check bool)
    "all-kept filter keeps original spine" true
    (L.filter_preserve (fun _ -> true) xs == xs);
  let filtered = L.filter_preserve (fun x -> x != b) xs in
  Alcotest.(check bool) "drop allocates" false (filtered == xs);
  check_same_list "drop middle" [ a; c ] filtered;
  Alcotest.(check (list ref_int))
    "drop all" []
    (L.filter_preserve (fun _ -> false) xs)

let test_filter_map_preserve_keeps_noop_identity () =
  let a = ref 1 and b = ref 2 and c = ref 3 in
  let xs = [ a; b; c ] in
  Alcotest.(check bool)
    "all-kept filter_map keeps original spine" true
    (L.filter_map_preserve Option.some xs == xs);
  let removed =
    L.filter_map_preserve (fun x -> if x == b then None else Some x) xs
  in
  check_same_list "remove middle" [ a; c ] removed;
  let replaced =
    L.filter_map_preserve (fun x -> if x == b then Some (ref !x) else Some x) xs
  in
  Alcotest.(check bool) "replacement allocates" false (replaced == xs);
  Alcotest.(check int) "replacement length" 3 (L.length replaced)

let suite =
  ( "common",
    [
      Alcotest.test_case "same uses physical elements" `Quick
        test_same_uses_physical_elements;
      Alcotest.test_case "preserve returns original list when elements same"
        `Quick test_preserve_returns_original_list_when_elements_same;
      Alcotest.test_case "map_preserve keeps no-op identity" `Quick
        test_map_preserve_keeps_noop_identity;
      Alcotest.test_case "filter_preserve keeps no-op identity" `Quick
        test_filter_preserve_keeps_noop_identity;
      Alcotest.test_case "filter_map_preserve keeps no-op identity" `Quick
        test_filter_map_preserve_keeps_noop_identity;
    ] )
