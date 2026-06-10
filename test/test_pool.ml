(** Pool module tests. *)

open Cascade
module P = Pool

let mk name : Stylesheet.rule =
  {
    selector = Selector.class_ name;
    declarations = [];
    nested = [];
    merge_key = None;
  }

let sel (r : Stylesheet.rule) = Selector.to_string ~minify:true r.selector
let names p = List.map sel (P.to_rules p)

let union_selectors (a : Stylesheet.rule) (b : Stylesheet.rule) :
    Stylesheet.rule =
  { a with selector = Selector.list [ a.selector; b.selector ] }

let roundtrip () =
  let p = P.of_rules [ mk "a"; mk "b"; mk "c" ] in
  Alcotest.(check (list string)) "order" [ ".a"; ".b"; ".c" ] (names p);
  Alcotest.(check int) "length" 3 (P.length p)

let nodes_in_order () =
  let p = P.of_rules [ mk "a"; mk "b" ] in
  match P.nodes p with
  | [ na; nb ] ->
      Alcotest.(check string) "first" ".a" (sel (P.rule na));
      Alcotest.(check string) "second" ".b" (sel (P.rule nb));
      Alcotest.(check bool) "a before b" true (P.before na nb)
  | _ -> Alcotest.fail "expected 2 nodes"

let combine_unions_and_drops () =
  let p = P.of_rules [ mk "a"; mk "b"; mk "c" ] in
  match P.nodes p with
  | [ na; nb; nc ] ->
      let s = P.combine p na nb union_selectors in
      (* surviving node carries the merged rule, [b] is gone, order kept *)
      Alcotest.(check string) "merged selector" ".a,.b" (sel (P.rule s));
      Alcotest.(check (list string)) "live rules" [ ".a,.b"; ".c" ] (names p);
      Alcotest.(check int) "length" 2 (P.length p);
      Alcotest.(check bool) "survivor before c" true (P.before s nc)
  | _ -> Alcotest.fail "expected 3 nodes"

let insert_after_places () =
  let p = P.of_rules [ mk "a"; mk "c" ] in
  match P.nodes p with
  | [ na; _ ] ->
      let nb = P.insert_after p na (mk "b") in
      Alcotest.(check (list string)) "inserted" [ ".a"; ".b"; ".c" ] (names p);
      Alcotest.(check string) "new node rule" ".b" (sel (P.rule nb))
  | _ -> Alcotest.fail "expected 2 nodes"

let remove_node () =
  let p = P.of_rules [ mk "a"; mk "b"; mk "c" ] in
  match P.nodes p with
  | [ _; nb; _ ] ->
      P.remove p nb;
      Alcotest.(check (list string)) "removed middle" [ ".a"; ".c" ] (names p)
  | _ -> Alcotest.fail "expected 3 nodes"

let set_rule () =
  let p = P.of_rules [ mk "a" ] in
  match P.nodes p with
  | [ na ] ->
      P.set na (mk "z");
      Alcotest.(check (list string)) "in place" [ ".z" ] (names p)
  | _ -> Alcotest.fail "expected 1 node"

let suite =
  ( "pool",
    [
      Alcotest.test_case "roundtrip" `Quick roundtrip;
      Alcotest.test_case "nodes in order" `Quick nodes_in_order;
      Alcotest.test_case "combine unions and drops" `Quick
        combine_unions_and_drops;
      Alcotest.test_case "insert_after places" `Quick insert_after_places;
      Alcotest.test_case "remove node" `Quick remove_node;
      Alcotest.test_case "set rule" `Quick set_rule;
    ] )
