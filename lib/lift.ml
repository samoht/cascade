(* Whether declarations can be lifted out of rules into a shared one. *)

open Stylesheet

let same_decl = Declaration.same_minified
let decl_facts = Commute.facts
let decl_facts_conflict = Commute.conflict
let fact_decl = Commute.decl

let unique_decls decls =
  let rec loop seen acc = function
    | [] -> List.rev acc
    | decl :: rest ->
        if List.exists (same_decl decl) seen then loop seen acc rest
        else loop (decl :: seen) (decl :: acc) rest
  in
  loop [] [] decls

let contains_decl (rule : rule) decl =
  List.exists (same_decl decl) rule.declarations

let common_exact_decls (rules : rule list) =
  match rules with
  | [] -> []
  | first :: rest ->
      unique_decls first.declarations
      |> List.filter (fun decl ->
          List.for_all (fun r -> contains_decl r decl) rest)

let remove_common common decls =
  List.filter
    (fun decl -> not (List.exists (fun common -> same_decl common decl) common))
    decls

let append_unique_decls base extras =
  List.fold_left
    (fun acc decl ->
      if List.exists (same_decl decl) acc then acc else acc @ [ decl ])
    base extras

let exact_leftovers common rules =
  List.filter_map
    (fun (r : rule) ->
      match remove_common common r.declarations with
      | [] -> Option.None
      | declarations ->
          Option.Some
            { r with declarations; nested = []; merge_key = Option.None })
    rules

let decl_fact_mem fact facts =
  List.exists (fun other -> same_decl (fact_decl fact) (fact_decl other)) facts

let unsafe_lift_over_leftover_facts group_facts decls =
  let decls = decl_facts decls in
  let rec loop prior_leftovers = function
    | [] -> false
    | fact :: rest ->
        if decl_fact_mem fact group_facts then
          List.exists
            (fun prior -> decl_facts_conflict fact prior)
            prior_leftovers
          || loop prior_leftovers rest
        else loop (fact :: prior_leftovers) rest
  in
  loop [] decls

let unsafe_lift_in_any_member group_decls rules =
  let group_facts = decl_facts group_decls in
  List.exists
    (fun (rule : rule) ->
      unsafe_lift_over_leftover_facts group_facts rule.declarations)
    rules

let decl_mem decl decls = List.exists (same_decl decl) decls

let group_orders_before group_decls left right =
  let rec loop = function
    | [] -> false
    | decl :: rest ->
        if same_decl decl left then true
        else if same_decl decl right then false
        else loop rest
  in
  loop group_decls

let unsafe_group_order_in_member group_decls (rule : rule) =
  let grouped =
    decl_facts rule.declarations
    |> List.filter (fun fact -> decl_mem (fact_decl fact) group_decls)
  in
  let rec loop prior = function
    | [] -> false
    | fact :: rest ->
        List.exists
          (fun prior ->
            group_orders_before group_decls (fact_decl fact) (fact_decl prior)
            && decl_facts_conflict fact prior)
          prior
        || loop (fact :: prior) rest
  in
  loop [] grouped

let unsafe_group_order group_decls rules =
  List.exists (unsafe_group_order_in_member group_decls) rules

let declarations_conflict_with_group_facts group_facts declarations =
  let declarations = decl_facts declarations in
  List.exists
    (fun decl ->
      List.exists (fun group -> decl_facts_conflict decl group) group_facts)
    declarations

let unsafe_cross_member_leftover group_decls rules =
  let group_facts = decl_facts group_decls in
  List.exists
    (fun (rule : rule) ->
      let leftovers = remove_common group_decls rule.declarations in
      declarations_conflict_with_group_facts group_facts leftovers
      && List.exists
           (fun (other : rule) ->
             other != rule
             && Rule_graph.selectors_conflict rule.selector other.selector)
           rules)
    rules

(* Whether a member's leftover conflicts with the group depends on that member
   alone, so it is asked once, and only the members it holds for are asked
   whether they conflict with a later one. *)
let unsafe_lift_over_prior_member ~conflict group_decls rules_with_ids =
  let group_facts = decl_facts group_decls in
  let rec loop prior = function
    | [] -> false
    | (id, (rule : rule)) :: rest ->
        List.exists (fun prior_id -> conflict prior_id id) prior
        ||
        let prior =
          if
            remove_common group_decls rule.declarations
            |> declarations_conflict_with_group_facts group_facts
          then id :: prior
          else prior
        in
        loop prior rest
  in
  loop [] rules_with_ids

let decl_facts_shorthand_order_conflict grouped external_decl =
  decl_facts_conflict grouped external_decl
  && not
       (Declaration.same_property (fact_decl grouped) (fact_decl external_decl))

let declarations_cross_order grouped external_decls =
  let grouped = decl_facts grouped in
  let external_decls = decl_facts external_decls in
  List.exists
    (fun grouped ->
      List.exists
        (fun external_decl ->
          decl_facts_shorthand_order_conflict grouped external_decl)
        external_decls)
    grouped
