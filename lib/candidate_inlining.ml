open Stylesheet
open Stdlib
open Rule_rewrite
open Candidate_kind

module String_table = Common.Table.Make (struct
  type t = string

  let equal = String.equal
  let hash s = Common.hash_string s land max_int
end)

let selector_branch_key = Rule_graph.selector_branch_key
let selector_branches (rule : rule) = Edge.selectors rule.selector

let single_selector_branch_key (rule : rule) : string option =
  match selector_branches rule with
  | [ selector ] -> Option.Some (selector_branch_key selector)
  | _ -> Option.None

let selector_branch_size = Size.selector

let remaining_selector_branches ~removed_key keyed =
  List.filter_map
    (fun (key, selector) ->
      if String.equal key removed_key then Option.None else Option.Some selector)
    keyed

let rule_for_selector_branches rule selectors : rule option =
  match selectors with
  | [] -> Option.None
  | _ ->
      Option.Some
        {
          rule with
          selector = Merge.selector_list selectors;
          nested = [];
          merge_key = Option.None;
        }

let selector_inline_candidate ~finalize g ~receiver_id ~group_id
    ~remaining_selectors =
  let receiver = node_rule g receiver_id in
  let group = node_rule g group_id in
  let receiver =
    {
      receiver with
      declarations =
        Lift.append_unique_decls receiver.declarations group.declarations;
      nested = [];
      merge_key = Option.None;
    }
  in
  let produce =
    match rule_for_selector_branches group remaining_selectors with
    | Option.None -> [ receiver ]
    | Option.Some group -> [ receiver; group ]
  in
  rewrite_candidate ~kind:Selector_branch_inline ~finalize g
    ~consume:[ receiver_id; group_id ] ~produce

type inline_group = {
  group : Rule_graph.node_id;
  branches : (string * Selector.t list) list;
  mutable pairs : (Rule_graph.node_id * offer) list;  (** by receiver *)
}

type t = {
  receivers : Rule_graph.node_id list String_table.t;
  by_branch : inline_group list String_table.t;
  inline_groups : inline_group Int_table.t;
}

let receiver_branch (rule : rule) =
  if rule_eligible rule then single_selector_branch_key rule else Option.None

(* The branch keys [rule] can hand to a receiver, each once, with the selector
   list left behind: a branch longer than the declarations it would carry. *)
let inline_branches g (rule : rule) =
  if not (rule_eligible rule && rule.declarations <> []) then []
  else
    match selector_branches rule with
    | [] | [ _ ] -> []
    | selectors ->
        let inline_cost = decls_inline_cost g rule.declarations in
        let keyed =
          List.map
            (fun selector -> (selector_branch_key selector, selector))
            selectors
        in
        List.fold_left
          (fun acc (key, selector) ->
            if selector_branch_size selector + 1 > inline_cost then
              if List.exists (fun (other, _) -> String.equal other key) acc then
                acc
              else
                (key, remaining_selector_branches ~removed_key:key keyed) :: acc
            else acc)
          [] keyed
        |> List.rev

let pair_up core group remaining receiver =
  if Rule_graph.precedes (graph (view core)) receiver group.group then
    match
      selector_inline_candidate ~finalize:(finalize core) (view core)
        ~receiver_id:receiver ~group_id:group.group
        ~remaining_selectors:remaining
    with
    | Option.None -> ()
    | Option.Some candidate ->
        group.pairs <- (receiver, offer core candidate) :: group.pairs

let table_list table key =
  Option.value ~default:[] (String_table.find_opt table key)

let set_table_list table key = function
  | [] -> String_table.remove table key
  | values -> String_table.replace table key values

let remove core inlining id =
  let rule = node_rule (view core) id in
  (match receiver_branch rule with
  | Option.None -> ()
  | Option.Some key ->
      set_table_list inlining.receivers key
        (without id (table_list inlining.receivers key));
      List.iter
        (fun group ->
          group.pairs <-
            List.filter
              (fun (receiver, offer) ->
                if same_id receiver id then begin
                  withdraw offer;
                  false
                end
                else true)
              group.pairs)
        (table_list inlining.by_branch key));
  let index = Rule_graph.Node_id.to_int id in
  match Int_table.find_opt inlining.inline_groups index with
  | Option.None -> ()
  | Option.Some group ->
      List.iter (fun (_, offer) -> withdraw offer) group.pairs;
      List.iter
        (fun (key, _) ->
          set_table_list inlining.by_branch key
            (List.filter
               (fun other -> other != group)
               (table_list inlining.by_branch key)))
        group.branches;
      Int_table.remove inlining.inline_groups index

let add core inlining id =
  let rule = node_rule (view core) id in
  (match receiver_branch rule with
  | Option.None -> ()
  | Option.Some key ->
      String_table.push inlining.receivers key id;
      List.iter
        (fun group ->
          let remaining =
            snd
              (List.find
                 (fun (other, _) -> String.equal other key)
                 group.branches)
          in
          pair_up core group remaining id)
        (table_list inlining.by_branch key));
  match inline_branches (view core) rule with
  | [] -> ()
  | branches ->
      let group = { group = id; branches; pairs = [] } in
      Int_table.replace inlining.inline_groups
        (Rule_graph.Node_id.to_int id)
        group;
      List.iter
        (fun (key, remaining) ->
          String_table.push inlining.by_branch key group;
          List.iter
            (pair_up core group remaining)
            (table_list inlining.receivers key))
        branches

let v size =
  {
    receivers = String_table.create size;
    by_branch = String_table.create 8;
    inline_groups = Int_table.create 8;
  }

let reorder _ _ _ = ()
let settle _ _ _ = ()

let offers inlining acc =
  Int_table.fold
    (fun _ group acc ->
      List.fold_left (fun acc (_, offer) -> offer :: acc) acc group.pairs)
    inlining.inline_groups acc
