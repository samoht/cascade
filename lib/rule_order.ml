(** Canonical cascade-safe rule ordering: project each run of reorderable rules
    through {!Rule_graph} to its canonical linear extension. *)

open Stylesheet

(* A rule is reorderable only when it is a plain style rule with no nested
   block; a rule that carries nesting is a barrier whose own body is
   canonicalised separately. Custom-property declarations are reorderable too:
   the dependency graph keys each custom property by name, so two writers of the
   same property on overlapping selectors keep their order, while a [var()]
   reader - which writes its own property, not the one it reads - moves freely
   past the definition. At-rules and everything else are barriers that split a
   run. *)
let reorderable = function Rule r -> r.nested = [] | _ -> false
let rule_of = function Rule r -> r | _ -> invalid_arg "Rule_order.rule_of"

let is_identity order =
  let id = ref true in
  Array.iteri
    (fun i v -> if i <> Rule_graph.Node_id.to_int v then id := false)
    order;
  !id

(* Reorder one maximal run of reorderable rules into a canonical linear
   extension of its dependency graph, breaking ties among independent rules by
   serialized rule content so two source orderings converge to one form, while
   preserving physical identity when that order already matches the input.
   [parent], when set, is the enclosing nesting context: the graph expands each
   rule's relative selector against it so overlap is computed on the effective
   selector. *)
let sort_run ?parent changed (run : statement list) : statement list =
  let arr = Array.of_list run in
  let n = Array.length arr in
  if n < 2 then run
  else
    let g = Rule_graph.of_rules ?parent (List.map rule_of run) in
    let keys =
      Array.map (fun s -> Pp.to_string ~minify:true pp_rule (rule_of s)) arr
    in
    let ranked = Array.init n (fun i -> i) in
    Array.sort
      (fun a b ->
        match String.compare keys.(a) keys.(b) with
        | 0 -> Int.compare a b
        | c -> c)
      ranked;
    let rank = Array.make n 0 in
    Array.iteri (fun pos i -> rank.(i) <- pos) ranked;
    let order =
      Rule_graph.canonical_order_by g (fun node ->
          rank.(Rule_graph.Node_id.to_int node))
    in
    if is_identity order then run
    else begin
      changed := true;
      Array.to_list
        (Array.map (fun i -> arr.(Rule_graph.Node_id.to_int i)) order)
    end

(* Canonicalise every cascade context. At-rule block bodies inherit the current
   nesting [parent]; a style rule's nested body is canonicalised under the
   rule's own (expanded) selector as the new parent, so nested relative
   selectors are compared on their effective form. *)
let rec recurse ~(parent : Selector.t option) changed (stmt : statement) :
    statement =
  let here b = canonicalize_block ~parent changed b in
  match stmt with
  | Rule r ->
      let child_parent =
        match parent with
        | None -> r.selector
        | Some p -> Nest.combine p r.selector
      in
      let nested =
        canonicalize_block ~parent:(Some child_parent) changed r.nested
      in
      if nested == r.nested then stmt else Rule { r with nested }
  | Layer (n, b) -> Layer (n, here b)
  | Media (m, b) -> Media (m, here b)
  | Container (n, c, b) -> Container (n, c, here b)
  | Supports (s, b) -> Supports (s, here b)
  | Moz_document (c, b) -> Moz_document (c, here b)
  | When (c, b) -> When (c, here b)
  | Else (c, b) -> Else (c, here b)
  | Starting_style b -> Starting_style (here b)
  | Origin (o, b) -> Origin (o, here b)
  | Scope (s, e, b) -> Scope (s, e, here b)
  | other -> other

and canonicalize_block ~parent changed (stmts : statement list) : statement list
    =
  let rec go = function
    | [] -> []
    | stmt :: rest when reorderable stmt ->
        let rec take acc = function
          | s :: r when reorderable s -> take (s :: acc) r
          | r -> (List.rev acc, r)
        in
        let run, rest = take [ stmt ] rest in
        sort_run ?parent changed run @ go rest
    | stmt :: rest -> recurse ~parent changed stmt :: go rest
  in
  go stmts

let canonicalize (stmts : statement list) : statement list =
  let changed = ref false in
  let result =
    canonicalize_block ~parent:(None : Selector.t option) changed stmts
  in
  if !changed then result else stmts
