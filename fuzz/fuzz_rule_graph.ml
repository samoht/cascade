(** Fuzz tests for the rule-conflict index.

    {!Rule_graph.conflicts_after} reads only the rules sharing a bucket with the
    asking rule, and has to answer what a scan of every later rule with
    {!Rule_graph.facts_conflict} answers. {!Rule_graph.of_rules} links each rule
    only to the candidates its index offers, through compact runs, and has to
    order every pair a scan finds in conflict. The runs mix selectors that tie
    and do not tie on specificity, that overlap and that cannot, and
    declarations that share a slot through a shorthand, through [all], through a
    property the model cannot place, or not at all. *)

open Cascade
open Alcobar

let byte buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = xs.(byte buf i mod Array.length xs)

let selectors =
  [|
    ".a";
    ".b";
    ".a.b";
    "#x";
    "div";
    "span";
    ".a .b";
    ".a,#x";
    "div,.b";
    ".a:hover";
    "p::before";
    "*";
    ":is(.a,#x)";
    "input:not(.b)";
  |]

let declarations =
  [|
    "color:red";
    "color:blue";
    "color:red!important";
    "margin:0";
    "margin-top:1px";
    "padding:0";
    "border:1px solid";
    "border-top-color:red";
    "all:unset";
    "--v:1";
    "--v:2";
    "-x-thing:1";
    "display:none";
    "display:block";
  |]

let rule buf i =
  let n = 1 + (byte buf i mod 3) in
  let decls = List.init n (fun k -> pick declarations buf (i + 1 + k)) in
  let css = pick selectors buf (i + 4) ^ "{" ^ String.concat ";" decls ^ "}" in
  match Css.of_string ~strict:false css with
  | Ok { stylesheet; _ } -> (
      match Css.statements stylesheet with
      | [ Stylesheet.Rule r ] -> Some r
      | _ -> None)
  | Error _ -> None

(* A run of rules at non-decreasing positions, ties included, as the index
   requires, then one asking rule and a position to ask after. *)
let test_index_agrees_with_scan buf =
  let count = byte buf 0 mod 24 in
  let run =
    List.init count (fun k -> (k / 2, rule buf (5 * (k + 1))))
    |> List.filter_map (fun (at, r) ->
        Option.map (fun r -> (at, Rule_graph.facts r)) r)
  in
  match rule buf 1 with
  | None -> ()
  | Some asking ->
      let asking = Rule_graph.facts asking in
      let index = Rule_graph.facts_index () in
      List.iter (fun (at, f) -> Rule_graph.index_facts index ~at f) run;
      for after = -1 to count / 2 do
        let scan =
          List.exists
            (fun (at, f) -> at > after && Rule_graph.facts_conflict asking f)
            run
        in
        if Rule_graph.conflicts_after index ~after asking <> scan then
          failf "conflicts_after ~after:%d answers %b, a scan answers %b" after
            (not scan) scan
      done

(* Rules of the shape the compact runs hold, one declaration through a simple
   subject, mixed with rules of other shapes. *)
let run_rule buf i =
  let selector =
    pick [| ".a"; ".b"; ".c"; "div"; "#x"; ".a.b"; ".a .b" |] buf i
  in
  let decls =
    match byte buf (i + 1) mod 4 with
    | 0 -> "color:red;margin:0"
    | k ->
        pick
          [| "color:red"; "color:blue"; "color:green"; "margin-top:0" |]
          buf (i + k)
  in
  match Css.of_string ~strict:false (selector ^ "{" ^ decls ^ "}") with
  | Ok { stylesheet; _ } -> (
      match Css.statements stylesheet with
      | [ Stylesheet.Rule r ] -> Some r
      | _ -> None)
  | Error _ -> None

(* Every pair of rules in conflict, by a test of the pair alone, is ordered by
   the graph in source order. Without the pins a shared selector adds, a pair
   the compact runs drop has no other path to fall back on. *)
let test_graph_orders_every_conflict buf =
  let count = byte buf 0 mod 16 in
  let rules =
    List.filter_map
      (fun k -> run_rule buf (3 * (k + 1)))
      (List.init count Fun.id)
  in
  let g =
    Rule_graph.of_rules ~pin_shared_branches:(byte buf 1 mod 2 = 0) rules
  in
  let n = Rule_graph.node_count g in
  for j = 0 to n - 1 do
    for i = 0 to j - 1 do
      let i' = Rule_graph.Node_id.of_int_exn i in
      let j' = Rule_graph.Node_id.of_int_exn j in
      if Rule_graph.conflict g i' j' && not (Rule_graph.precedes g i' j') then
        failf "rules %d and %d conflict but the graph does not order them" i j
    done
  done

let suite =
  ( "rule_graph",
    [
      test_case "conflicts_after agrees with a scan" [ bytes ]
        test_index_agrees_with_scan;
      test_case "the graph orders every conflicting pair" [ bytes ]
        test_graph_orders_every_conflict;
    ] )
