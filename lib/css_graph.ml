type t = {
  n_rules : int;
  n_decls : int;
  decl_byte_cost : int array;
  rule_decls : int array array;
      (** [rule_decls.(r)] is the sorted-ascending column indices that rule [r]
          declares. *)
  decl_rules : int array array;
      (** [decl_rules.(c)] is the sorted-ascending row indices that contain
          column [c]. The inverted index for fast biclique candidate generation.
      *)
  rule_selector_size : int array;
      (** Minified byte size of each rule's selector, used to compute the
          biclique replacement cost. *)
}

type factoring = { rules : int array; decls : int array; saving : int }

let n_rules g = g.n_rules
let n_decls g = g.n_decls

let n_edges g =
  Array.fold_left (fun acc cols -> acc + Array.length cols) 0 g.rule_decls

let build (rules : Stylesheet.rule list) : t =
  let rules_arr = Array.of_list rules in
  let n_rules = Array.length rules_arr in
  (* Intern each distinct declaration to a small integer id via its cached
     structural hash. Hash collisions are rare; we tolerate them as
     conservatively-wrong column merges (a graph with two structurally different
     decls in the same column lets greedy_cover propose an unsound factoring, so
     we re-check decl equality during cover construction). *)
  let decl_id : (int, int) Hashtbl.t = Hashtbl.create 1024 in
  let decl_value : (int, Declaration.declaration) Hashtbl.t =
    Hashtbl.create 1024
  in
  let next_id = ref 0 in
  let intern d =
    let h = Declaration.hash d in
    match Hashtbl.find_opt decl_id h with
    | Some id -> id
    | None ->
        let id = !next_id in
        Hashtbl.add decl_id h id;
        Hashtbl.add decl_value id d;
        incr next_id;
        id
  in
  let rule_decls =
    Array.map
      (fun (r : Stylesheet.rule) ->
        let ids = List.map intern r.Stylesheet_intf.declarations in
        let arr = Array.of_list ids in
        Array.sort compare arr;
        arr)
      rules_arr
  in
  let n_decls = !next_id in
  let decl_byte_cost = Array.make n_decls 0 in
  for c = 0 to n_decls - 1 do
    let d = Hashtbl.find decl_value c in
    decl_byte_cost.(c) <- Pp.size ~minify:true Declaration.pp_declaration d
  done;
  (* Build the inverted index. *)
  let decl_rows = Array.make n_decls [] in
  Array.iteri
    (fun r cols ->
      Array.iter (fun c -> decl_rows.(c) <- r :: decl_rows.(c)) cols)
    rule_decls;
  let decl_rules =
    Array.map
      (fun rows ->
        let arr = Array.of_list rows in
        Array.sort compare arr;
        arr)
      decl_rows
  in
  let rule_selector_size =
    Array.map
      (fun (r : Stylesheet.rule) ->
        Pp.size ~minify:true Selector.pp r.Stylesheet_intf.selector)
      rules_arr
  in
  {
    n_rules;
    n_decls;
    decl_byte_cost;
    rule_decls;
    decl_rules;
    rule_selector_size;
  }

(* Sum the byte cost of a sorted decl array. *)
let cols_cost g cols =
  Array.fold_left (fun acc c -> acc + g.decl_byte_cost.(c)) 0 cols

(* Sum the selector sizes of a sorted rule array. *)
let rows_selector_cost g rows =
  Array.fold_left (fun acc r -> acc + g.rule_selector_size.(r)) 0 rows

(* Saving from replacing the biclique (rows, decls) with a single shared rule.
   The original payload is [|rows| * (decls_size + len(decls))] inline costs;
   the replacement is one rule whose selector is the comma-separated row
   selectors and whose body is the shared declarations. Each original rule loses
   [decls_size + len(decls)] declaration bytes; the new rule adds
   [sum(selector_size_rows) + (|rows| - 1) + 2 + decls_size + (len(decls) -
   1)]. *)
let biclique_saving g rows decls =
  let n_rows = Array.length rows in
  let n_decls = Array.length decls in
  if n_rows < 2 || n_decls < 1 then 0
  else
    let decls_size = cols_cost g decls in
    let decls_inline = decls_size + n_decls in
    let payload_removed = n_rows * decls_inline in
    let new_rule_size =
      rows_selector_cost g rows + (n_rows - 1) + 2 + decls_size
      + max 0 (n_decls - 1)
    in
    payload_removed - new_rule_size

(* Intersect a list of sorted int arrays. *)
let inter_arrays arrays =
  match arrays with
  | [] -> [||]
  | first :: rest ->
      let acc = ref (Array.to_list first) in
      List.iter
        (fun arr ->
          let arr_list = Array.to_list arr in
          let rec merge a b =
            match (a, b) with
            | [], _ | _, [] -> []
            | x :: xs, y :: ys ->
                if x = y then x :: merge xs ys
                else if x < y then merge xs b
                else merge a ys
          in
          acc := merge !acc arr_list)
        rest;
      Array.of_list !acc

(* Sorted set of decls shared by all the given rules. *)
let common_decls g rows =
  inter_arrays (Array.to_list (Array.map (fun r -> g.rule_decls.(r)) rows))

(* Greedy biclique cover. For each declaration column with multiple rows,
   propose [(rule_set, {col})] as a candidate biclique. Extend each candidate by
   adding any column shared by all selected rows (intersection), keeping the
   saving positive. Pick the best candidate, mark its rows consumed, and
   repeat. *)
let greedy_cover (g : t) : factoring list =
  let consumed = Array.make g.n_rules false in
  let collected = ref [] in
  let try_extend rows decls =
    let common = common_decls g rows in
    if Array.length common <= Array.length decls then (rows, decls)
    else (rows, common)
  in
  let rec loop () =
    (* Find the best biclique anchored at any unconsumed column. *)
    let best = ref None in
    for c = 0 to g.n_decls - 1 do
      let rows = g.decl_rules.(c) in
      let live =
        Array.of_list
          (List.filter (fun r -> not consumed.(r)) (Array.to_list rows))
      in
      if Array.length live >= 2 then begin
        let rows, decls = try_extend live [| c |] in
        let saving = biclique_saving g rows decls in
        if saving > 0 then
          match !best with
          | None -> best := Some { rules = rows; decls; saving }
          | Some prev when saving > prev.saving ->
              best := Some { rules = rows; decls; saving }
          | _ -> ()
      end
    done;
    match !best with
    | None -> ()
    | Some f ->
        Array.iter (fun r -> consumed.(r) <- true) f.rules;
        collected := f :: !collected;
        loop ()
  in
  loop ();
  List.sort (fun a b -> compare a.rules.(0) b.rules.(0)) !collected
