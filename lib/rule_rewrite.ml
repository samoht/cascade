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

(* Under the transfer objective the consumed rules are read in the order they
   print, as a compressor reads them. *)
let saving objective graph ~consume ~produce ~size =
  match objective with
  | `Raw -> consumed_size graph consume - size
  | `Transfer ->
      let consumed =
        List.sort (Rule_graph.compare_position graph) consume
        |> List.map (Rule_graph.node_rule graph)
      in
      let print = Rule_graph.size_print graph in
      Transfer_size.rules print consumed - Transfer_size.rules print produce

(* A produced rule writes what its body holds, nested rules included (CSS
   Nesting 1 sec. 3), so only a rule with neither declarations nor nested
   statements is dropped. *)
let finalize_produce (finalize : Stylesheet.rule -> Stylesheet.rule)
    (produce : Stylesheet.rule list) : Stylesheet.rule list =
  List.filter_map
    (fun rule ->
      let rule = finalize rule in
      if rule.Stylesheet.declarations = [] && rule.nested = [] then Option.None
      else Option.Some rule)
    produce

let of_finalized ~objective ~kind graph ~consume ~produce ~size =
  match produce with
  | [] -> Option.None
  | _ ->
      let saving = saving objective graph ~consume ~produce ~size in
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

let v ~objective ~kind ~finalize graph ~consume
    ~(produce : Stylesheet.rule list) =
  let produce = finalize_produce finalize produce in
  of_finalized ~objective ~kind graph ~consume ~produce
    ~size:(Size.rules (Rule_graph.size_print graph) produce)

let bytes objective saving =
  match objective with
  | `Raw -> saving
  | `Transfer -> saving / Transfer_size.per_byte
