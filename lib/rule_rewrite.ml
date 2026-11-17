(** Local DAG rewrite candidates and byte scoring. *)

(* The size memo is probed a few hundred thousand times on a large stylesheet,
   and a generic [Hashtbl] keyed by a rule compares rule subtrees structurally
   on every probe. *)
module Rule_tbl = Hashtbl.Make (struct
  type t = Stylesheet.rule

  let equal = ( = )
  let hash = Stylesheet.rule_hash
end)

type kind =
  | Identical_body
  | Same_selector
  | Exact_shared_declarations
  | Selector_branch_inline
  | Default_factoring

let equal_kind (a : kind) b = a = b

type candidate = {
  generation : int;
  kind : kind;
  consume : Rule_graph.node_id list;
  produce : Stylesheet.rule list;
  saving : int;
}

(* A produced rule's size depends on the rule alone, not on the graph it is
   scored against, so one cache serves a whole scheduler run. A consumed node's
   size is already held by the graph. *)
type size_cache = int Rule_tbl.t

let size_cache () = Rule_tbl.create 256

let produced_rule_size cache rule =
  match Rule_tbl.find_opt cache rule with
  | Option.Some size -> size
  | Option.None ->
      let size = Size.rule rule in
      Rule_tbl.replace cache rule size;
      size

let consumed_size graph ids =
  List.fold_left (fun acc id -> acc + Rule_graph.node_size graph id) 0 ids

let produced_size cache rules =
  List.fold_left (fun acc rule -> acc + produced_rule_size cache rule) 0 rules

let finalize_produce (finalize : Stylesheet.rule -> Stylesheet.rule)
    (produce : Stylesheet.rule list) : Stylesheet.rule list =
  List.filter_map
    (fun rule ->
      let rule = finalize rule in
      if rule.Stylesheet.declarations = [] then Option.None
      else Option.Some rule)
    produce

let v ?size_cache:cache ~kind ~finalize graph ~consume
    ~(produce : Stylesheet.rule list) =
  let produce = finalize_produce finalize produce in
  match produce with
  | [] -> Option.None
  | _ ->
      let cache =
        match cache with
        | Option.Some cache -> cache
        | Option.None -> size_cache ()
      in
      let before_size = consumed_size graph consume in
      let after_size = produced_size cache produce in
      let saving = before_size - after_size in
      if saving <= 0 then Option.None
      else
        Option.Some
          {
            generation = Rule_graph.generation graph;
            kind;
            consume;
            produce;
            saving;
          }
