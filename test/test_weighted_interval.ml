open Cascade

let selected_values schedule =
  let weight, intervals = Weighted_interval.select schedule in
  (weight, List.map (fun interval -> interval.Weighted_interval.value) intervals)

let test_selects_disjoint_optimum () =
  let schedule = Weighted_interval.v ~length:6 in
  Weighted_interval.add schedule ~start:0 ~stop:5 ~weight:6 "whole";
  Weighted_interval.add schedule ~start:0 ~stop:2 ~weight:5 "left";
  Weighted_interval.add schedule ~start:3 ~stop:5 ~weight:5 "right";
  Alcotest.(check (pair int (list string)))
    "two disjoint intervals beat one broad interval"
    (10, [ "left"; "right" ])
    (selected_values schedule)

let test_rejects_overlap () =
  let schedule = Weighted_interval.v ~length:6 in
  Weighted_interval.add schedule ~start:0 ~stop:3 ~weight:7 "first";
  Weighted_interval.add schedule ~start:2 ~stop:5 ~weight:7 "overlap";
  Weighted_interval.add schedule ~start:4 ~stop:5 ~weight:3 "tail";
  Alcotest.(check (pair int (list string)))
    "overlapping intervals are not both selected"
    (10, [ "first"; "tail" ])
    (selected_values schedule)

let test_ignores_non_positive_weight () =
  let schedule = Weighted_interval.v ~length:2 in
  Weighted_interval.add schedule ~start:0 ~stop:0 ~weight:0 "zero";
  Weighted_interval.add schedule ~start:1 ~stop:1 ~weight:(-1) "negative";
  Alcotest.(check (pair int (list string)))
    "non-positive weights cannot improve the optimum" (0, [])
    (selected_values schedule)

let test_validates_ranges () =
  let schedule = Weighted_interval.v ~length:2 in
  Alcotest.check_raises "negative length"
    (Invalid_argument "Weighted_interval.v: negative length") (fun () ->
      ignore (Weighted_interval.v ~length:(-1)));
  Alcotest.check_raises "negative start"
    (Invalid_argument "Weighted_interval.add: negative start") (fun () ->
      Weighted_interval.add schedule ~start:(-1) ~stop:0 ~weight:1 ());
  Alcotest.check_raises "reversed range"
    (Invalid_argument "Weighted_interval.add: stop before start") (fun () ->
      Weighted_interval.add schedule ~start:1 ~stop:0 ~weight:1 ());
  Alcotest.check_raises "stop out of range"
    (Invalid_argument "Weighted_interval.add: stop out of range") (fun () ->
      Weighted_interval.add schedule ~start:0 ~stop:2 ~weight:1 ())

let suite =
  ( "weighted_interval",
    [
      Alcotest.test_case "selects disjoint optimum" `Quick
        test_selects_disjoint_optimum;
      Alcotest.test_case "rejects overlap" `Quick test_rejects_overlap;
      Alcotest.test_case "ignores non-positive weight" `Quick
        test_ignores_non_positive_weight;
      Alcotest.test_case "validates ranges" `Quick test_validates_ranges;
    ] )
