(** Local DAG rewrite candidates and byte scoring. *)

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

let consumed_size graph ids =
  List.fold_left (fun acc id -> acc + Rule_graph.node_size graph id) 0 ids

let finalize_produce (finalize : Stylesheet.rule -> Stylesheet.rule)
    (produce : Stylesheet.rule list) : Stylesheet.rule list =
  List.filter_map
    (fun rule ->
      let rule = finalize rule in
      if rule.Stylesheet.declarations = [] then Option.None
      else Option.Some rule)
    produce

let of_finalized ~kind graph ~consume ~produce ~size =
  match produce with
  | [] -> Option.None
  | _ ->
      let saving = consumed_size graph consume - size in
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

let v ~kind ~finalize graph ~consume ~(produce : Stylesheet.rule list) =
  let produce = finalize_produce finalize produce in
  of_finalized ~kind graph ~consume ~produce ~size:(Size.rules produce)
