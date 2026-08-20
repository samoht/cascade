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

type size_cache = {
  graph : Rule_graph.t;
  node_sizes : (Rule_graph.node_id, int) Hashtbl.t;
  rule_sizes : int Rule_tbl.t;
}

let size_cache graph =
  { graph; node_sizes = Hashtbl.create 256; rule_sizes = Rule_tbl.create 256 }

let node_size cache id =
  match Hashtbl.find_opt cache.node_sizes id with
  | Option.Some size -> size
  | Option.None ->
      let size = Rule_graph.node_size cache.graph id in
      Hashtbl.replace cache.node_sizes id size;
      size

let produced_rule_size cache rule =
  match Rule_tbl.find_opt cache.rule_sizes rule with
  | Option.Some size -> size
  | Option.None ->
      let size = Size.rule rule in
      Rule_tbl.replace cache.rule_sizes rule size;
      size

let consumed_size cache ids =
  List.fold_left (fun acc id -> acc + node_size cache id) 0 ids

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
        | Option.None -> size_cache graph
      in
      let before_size = consumed_size cache consume in
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
