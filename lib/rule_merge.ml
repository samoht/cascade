module P = Rule_pool

let same_declarations (a : Stylesheet.rule) (b : Stylesheet.rule) =
  a.declarations = b.declarations

(* Union two selectors into a flat comma list (no nesting), so a run of merges
   yields [a, b, c] rather than [[a, b], c]. *)
let union_selectors (a : Stylesheet.rule) (b : Stylesheet.rule) :
    Stylesheet.rule =
  let parts (s : Selector.t) =
    match Selector.as_list s with Some xs -> xs | None -> [ s ]
  in
  { a with selector = Selector.list (parts a.selector @ parts b.selector) }

let combine_identical rules =
  let pool = P.of_rules rules in
  (* Sweep left to right; while the focus and its successor share a declaration
     block, fold the successor in and re-examine the focus against its new
     successor. Each fold removes one rule, so the sweep is linear. *)
  let rec sweep = function
    | None -> ()
    | Some n -> (
        match P.next n with
        | Some m when same_declarations (P.rule n) (P.rule m) ->
            ignore (P.combine pool n m union_selectors);
            sweep (Some n)
        | _ -> sweep (P.next n))
  in
  (match P.nodes pool with [] -> () | first :: _ -> sweep (Some first));
  P.to_rules pool

let property_name = Declaration.property_name

(* [d] is safe to hoist out of [r] when [r] declares it and no other declaration
   in [r] sets the same property: then [d]'s position within [r] is immaterial,
   so moving it into a shared rule ahead of [r] cannot reorder a property. *)
let hoistable (r : Stylesheet.rule) (d : Declaration.declaration) =
  let p = property_name d in
  let same_prop, has_d =
    List.fold_left
      (fun (cnt, seen) d' ->
        ( (if String.equal (property_name d') p then cnt + 1 else cnt),
          seen || d' = d ))
      (0, false) r.declarations
  in
  has_d && same_prop = 1

let without (r : Stylesheet.rule) d : Stylesheet.rule =
  { r with declarations = List.filter (fun d' -> not (d' = d)) r.declarations }

let rule_size (r : Stylesheet.rule) = Pp.size ~minify:true Stylesheet.pp_rule r

(* Maximal run of consecutive nodes from [n] whose rule can hoist [d]. *)
let run_sharing n d =
  let rec walk acc node =
    match node with
    | Some m when hoistable (P.rule m) d -> walk (m :: acc) (P.next m)
    | _ -> List.rev acc
  in
  walk [] n

(* The best (highest-saving) hoist available at node [n]: try each declaration
   [n] can hoist, take the longest run, keep the one that saves the most. *)
let best_hoist_at n =
  let r = P.rule n in
  List.fold_left
    (fun best d ->
      if not (hoistable r d) then best
      else
        let run = run_sharing (Some n) d in
        if List.length run < 2 then best
        else
          let shared : Stylesheet.rule =
            {
              selector =
                Selector.list
                  (List.map (fun m -> (P.rule m).Stylesheet_intf.selector) run);
              declarations = [ d ];
              nested = [];
              merge_key = None;
            }
          in
          let before =
            List.fold_left (fun a m -> a + rule_size (P.rule m)) 0 run
          in
          let after =
            rule_size shared
            + List.fold_left
                (fun a m -> a + rule_size (without (P.rule m) d))
                0 run
          in
          let savings = before - after in
          match best with
          | Some (_, _, s) when s >= savings -> best
          | _ -> Some (d, run, savings))
    None r.declarations

(* Greedy: repeatedly apply the best hoist found anywhere, until none saves. *)
let factor_common rules =
  let pool = P.of_rules rules in
  let apply d run =
    let shared : Stylesheet.rule =
      {
        selector =
          Selector.list
            (List.map (fun m -> (P.rule m).Stylesheet_intf.selector) run);
        declarations = [ d ];
        nested = [];
        merge_key = None;
      }
    in
    ignore (P.insert_before pool (List.hd run) shared);
    List.iter
      (fun m ->
        let r = without (P.rule m) d in
        if r.declarations = [] then P.remove pool m else P.set m r)
      run
  in
  let rec loop () =
    let best =
      List.fold_left
        (fun best n ->
          match best_hoist_at n with
          | Some (_, _, s) as here when s > 0 -> (
              match best with Some (_, _, bs) when bs >= s -> best | _ -> here)
          | _ -> best)
        None (P.nodes pool)
    in
    match best with
    | Some (d, run, _) ->
        apply d run;
        loop ()
    | None -> ()
  in
  loop ();
  P.to_rules pool
