let small_declaration_threshold = 1_000
let useful_gain_units = 2_048
let useful_gain_ratio_ppm = 140_000

type t = {
  source_units : int;
  declaration_count : int;
  identical_body_gain : int;
  shared_declaration_gain : int;
}

module Int_set = Hashtbl.Make (Int)

module Body_set = Hashtbl.Make (struct
  type t = int list

  let equal = List.equal Int.equal
  let hash = List.fold_left (fun acc h -> (acc * 31) + h) 0
end)

(* Source size and both gains are tracked in approximate minified bytes so a few
   repeated long declarations register their real weight, not just a count. A
   declaration counts as shared, and a body as identical, once its hash has been
   seen earlier in the segment. *)
let gains print rules =
  let n = List.length rules in
  let seen = Int_set.create n and bodies = Body_set.create n in
  let identical = ref 0 and shared = ref 0 in
  List.iter
    (fun (rule : Stylesheet.rule) ->
      let decls = rule.declarations in
      List.iter
        (fun decl ->
          let hash = Declaration.hash decl in
          if Int_set.mem seen hash then shared := !shared + Size.decl print decl
          else Int_set.add seen hash ())
        decls;
      match decls with
      | [] -> ()
      | _ ->
          let key = List.map Declaration.hash decls in
          if Body_set.mem bodies key then
            identical := !identical + Size.decls print decls
          else Body_set.add bodies key ())
    rules;
  (!identical, !shared)

(* Only a segment past [small_declaration_threshold] is ever scored, so a
   smaller one is counted and its gains are left at zero. *)
let summarize print rules =
  let declaration_count = ref 0 and source_units = ref 0 in
  List.iter
    (fun (rule : Stylesheet.rule) ->
      let decls = rule.declarations in
      declaration_count := !declaration_count + List.length decls;
      source_units := !source_units + 8 + Size.decls print decls)
    rules;
  let identical_body_gain, shared_declaration_gain =
    if !declaration_count <= small_declaration_threshold then (0, 0)
    else gains print rules
  in
  {
    source_units = !source_units;
    declaration_count = !declaration_count;
    identical_body_gain;
    shared_declaration_gain;
  }

let declaration_count t = t.declaration_count
let source_units t = t.source_units

(* Both gains are approximate minified bytes. Identical bodies merge with no
   per-use overhead, so they count in full; a shared declaration factored into a
   grouping rule realizes only a fraction of its duplicate bytes once the new
   selector list is paid for, so discount it. *)
let estimated_gain t = t.identical_body_gain + (t.shared_declaration_gain / 4)

let useful t =
  t.declaration_count <= small_declaration_threshold
  ||
  let gain = estimated_gain t in
  gain >= useful_gain_units
  && gain * 1_000_000 >= t.source_units * useful_gain_ratio_ppm
