let small_declaration_threshold = 1_000
let useful_gain_units = 2_048
let useful_gain_ratio_ppm = 140_000

(* Source size and both gains are tracked in approximate minified bytes so a few
   repeated long declarations register their real weight, not just a count. *)
let decl_size = Pp.size ~minify:true Declaration.pp

type t = {
  mutable source_units : int;
  mutable rule_count : int;
  mutable declaration_count : int;
  mutable identical_body_gain : int;
  mutable shared_declaration_gain : int;
}

type state = {
  summary : t;
  body_groups : (int list, int) Hashtbl.t;
  declaration_counts : (int, int) Hashtbl.t;
}

let v () =
  {
    summary =
      {
        source_units = 0;
        rule_count = 0;
        declaration_count = 0;
        identical_body_gain = 0;
        shared_declaration_gain = 0;
      };
    body_groups = Hashtbl.create 256;
    declaration_counts = Hashtbl.create 1024;
  }

let record_declaration state decl size =
  let hash = Declaration.hash decl in
  let seen =
    match Hashtbl.find_opt state.declaration_counts hash with
    | Some n -> n
    | None -> 0
  in
  if seen > 0 then
    state.summary.shared_declaration_gain <-
      state.summary.shared_declaration_gain + size;
  Hashtbl.replace state.declaration_counts hash (seen + 1)

let record_identical_body state ~body_size decls =
  let key = List.map Declaration.hash decls in
  match Hashtbl.find_opt state.body_groups key with
  | Some count ->
      state.summary.identical_body_gain <-
        state.summary.identical_body_gain + body_size;
      Hashtbl.replace state.body_groups key (count + 1)
  | None -> Hashtbl.add state.body_groups key 1

let record_rule state (rule : Stylesheet.rule) =
  let decls = rule.Stylesheet_intf.declarations in
  let sizes = List.map decl_size decls in
  let decl_count = List.length decls in
  let body_size = List.fold_left ( + ) 0 sizes in
  state.summary.rule_count <- state.summary.rule_count + 1;
  state.summary.source_units <- state.summary.source_units + 8 + body_size;
  state.summary.declaration_count <-
    state.summary.declaration_count + decl_count;
  List.iter2 (record_declaration state) decls sizes;
  if decl_count > 0 then record_identical_body state ~body_size decls

let summarize rules =
  let state = v () in
  List.iter (record_rule state) rules;
  state.summary

let declaration_count t = t.declaration_count
let source_units t = t.source_units

(* Both gains are approximate minified bytes. Identical bodies merge with no
   per-use overhead, so they count in full; a shared declaration factored into a
   grouping rule realizes only a fraction of its duplicate bytes once the new
   selector list is paid for, so discount it. *)
let estimated_gain t = t.identical_body_gain + (t.shared_declaration_gain / 4)

(* The gain is a raw-byte proxy, and the transfer gate in [Factor] later judges
   the same factoring on compressed size, so a segment admitted here can still
   be thrown away. Measured over the 30-file satcss corpus: 44 fixpoints run, 4
   reverted, and those 4 account for 0.20s of the 1.01s spent factoring.

   Scoring compressed size here instead is not available: the gate compares the
   factored rendering against the unfactored one, and producing the factored
   rendering is the work this gate exists to avoid. What the reverts cost is
   bounded another way, by [Factor.near_reverted], which remembers a large
   reverted segment's shape and suppresses the ones that resemble it. *)
let useful t =
  t.declaration_count <= small_declaration_threshold
  ||
  let gain = estimated_gain t in
  gain >= useful_gain_units
  && gain * 1_000_000 >= t.source_units * useful_gain_ratio_ppm
