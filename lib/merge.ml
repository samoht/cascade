let parsed_by_none = Selector_targets.parsed_by_none

(* Everything [compatible] reads off a selector: [Uniform d] when the targets
   [d] drop each of its branches, [Apart] when they drop different branches or
   [reach] has no answer for one, so that it lists with nothing. *)
type compatibility = Uniform of Selector_targets.dropped | Apart

let compatibility sel =
  match Selector_targets.dropped_by sel with
  | Some d -> Uniform d
  | None -> Apart

let listable sel =
  match compatibility sel with Uniform _ -> true | Apart -> false

let equal_compatibility a b =
  match (a, b) with
  | Uniform a, Uniform b -> Selector_targets.equal_dropped a b
  | (Uniform _ | Apart), _ -> false

let compatible sel1 sel2 =
  equal_compatibility (compatibility sel1) (compatibility sel2)

let key sel =
  match Selector.as_list sel with
  | Some xs -> List.sort compare xs
  | None -> [ sel ]

let selector_list = function
  | [ s ] -> s
  | sels ->
      let flatten = function Selector.List xs -> xs | s -> [ s ] in
      Selector.List (List.concat_map flatten sels)

(* [compatible] is an equivalence, so a group holds only compatible pairs
   exactly when every member is compatible with the first: the group remembers
   that one fact and a candidate joins by reading its own once. *)
type run = Empty | Run of Selector_targets.dropped | Broken

let empty_run = Empty

let extend_run run sel =
  match (run, compatibility sel) with
  | Broken, _ | _, Apart -> Broken
  | Empty, Uniform d -> Run d
  | Run d, Uniform e ->
      if Selector_targets.equal_dropped d e then run else Broken

let run_compatible = function Broken -> false | Empty | Run _ -> true

let all_compatible sels =
  let rec loop run = function
    | [] -> true
    | sel :: rest ->
        let run = extend_run run sel in
        run_compatible run && loop run rest
  in
  loop empty_run sels

let rec declaration_lists_equal ~same d1 d2 =
  match (d1, d2) with
  | [], [] -> true
  | a :: rest_a, b :: rest_b ->
      same a b && declaration_lists_equal ~same rest_a rest_b
  | _ -> false

let declarations_equal ~same d1 d2 =
  d1 == d2 || declaration_lists_equal ~same d1 d2
