(** Fuzz tests for the rule-conflict index.

    {!Rule_graph.conflicts_after} reads only the rules sharing a bucket with the
    asking rule, and has to answer what a scan of every later rule with
    {!Rule_graph.facts_conflict} answers. The runs mix selectors that tie and do
    not tie on specificity, that overlap and that cannot, and declarations that
    share a slot through a shorthand, through [all], through a property the
    model cannot place, or not at all. *)

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

let suite =
  ( "rule_graph",
    [
      test_case "conflicts_after agrees with a scan" [ bytes ]
        test_index_agrees_with_scan;
    ] )
