(** Graph invariants for the cascade-dependency rule ordering. *)

open Cascade

let parse s =
  match Css.of_string ~strict:false s with
  | Ok p -> p.Css.stylesheet
  | Error _ -> Alcotest.failf "parse failed: %s" s

let optimize_str s = parse s |> Css.optimize |> Css.to_string ~minify:true

let graph_project_str s =
  parse s
  |> List.filter_map (function Stylesheet.Rule r -> Some r | _ -> None)
  |> Rule_graph.of_rules |> Rule_graph.to_rules
  |> List.map (fun r -> Stylesheet.Rule r)
  |> Css.to_string ~minify:true

(* The optimizer's output must be a fixed point: reparsing and optimizing it
   again changes nothing. Guards the in-loop canonicalisation against passes
   that would only converge on a second run. *)
let idempotent_after_reparse () =
  let cases =
    [
      ".a{color:red}.x{margin:0}.b{color:red}";
      ".nav{display:flex}.nav a{color:red}.nav a:hover{margin:0}";
      ".foo{margin:5px}.baz{padding:10px}.bar{margin:5px}";
      "@media(min-width:1px){.a{color:red}.b{margin:0}}.c{top:0}";
      ".c2,.c5{transition:opacity .25s \
       linear}.c2{border:1px}.c4,.c6{margin:0}.c4{padding:0;top:0;color:red}.c5{top:0}.c6{top:0;left:0}";
    ]
  in
  List.iter
    (fun s ->
      let once = optimize_str s in
      let twice = optimize_str once in
      Alcotest.(check string) ("fixed point: " ^ s) once twice)
    cases

(* The graph projection is source-stable: independent nodes keep first
   appearance order, and real cascade dependencies still pin source order. *)
let graph_projection_keeps_source_order () =
  let pairs =
    [
      ( ".a{color:red}.x{margin:0}.b{padding:0}",
        ".b{padding:0}.x{margin:0}.a{color:red}" );
      ("#a{color:red}#b{color:blue}", "#b{color:blue}#a{color:red}");
      ( ".x{margin:0}.a{color:red}.y{padding:0}",
        ".a{color:red}.y{padding:0}.x{margin:0}" );
    ]
  in
  List.iter
    (fun (p, q) ->
      Alcotest.(check bool)
        ("neutral permutations keep textual source order: " ^ p)
        false
        (String.equal (graph_project_str p) (graph_project_str q)))
    pairs

(* A conflicting pair - same element, same property - is order-significant:
   reordering it changes the cascade, so the two inputs must not collapse to the
   same output. *)
let conflicting_pair_is_pinned () =
  let a = optimize_str ".a{color:red}.a{color:blue}" in
  let b = optimize_str ".a{color:blue}.a{color:red}" in
  Alcotest.(check bool)
    "conflicting order is observable" false (String.equal a b)

(* --- Rule_graph.try_rewrite transactions --- *)

let rules_of s =
  parse s
  |> List.filter_map (function Stylesheet.Rule r -> Some r | _ -> None)

let nid i = Rule_graph.Node_id.of_int_exn i
let nids ids = List.map nid ids

let int_order order =
  order |> Array.to_list |> List.map Rule_graph.Node_id.to_int

let graph_to_string g =
  Rule_graph.to_rules g
  |> List.map (fun r -> Stylesheet.Rule r)
  |> Css.to_string ~minify:true

(* A factoring whose intervening context does not conflict commits, and the
   produced grouped/leftover nodes appear in the emitted graph. *)
let try_rewrite_safe_factoring () =
  let g =
    Rule_graph.of_rules
      (rules_of ".a{color:red;width:1px}.b{color:red;height:1px}")
  in
  let produce =
    rules_of ".a,.b{color:red}"
    @ rules_of ".a{width:1px}" @ rules_of ".b{height:1px}"
  in
  match Rule_graph.try_rewrite g ~consume:(nids [ 0; 1 ]) ~produce with
  | None -> Alcotest.fail "safe factoring should commit"
  | Some g' ->
      let out = graph_to_string g' in
      Alcotest.(check bool)
        ("grouped node emitted: " ^ out)
        true
        (Astring.String.is_infix ~affix:".a,.b{color:red}" out)

(* A node consumed by a committed rewrite is dead; reusing it is rejected. *)
let try_rewrite_rejects_stale () =
  let g = Rule_graph.of_rules (rules_of ".a{color:red}.b{color:blue}") in
  match
    Rule_graph.try_rewrite g
      ~consume:(nids [ 0; 1 ])
      ~produce:(rules_of ".a,.b{color:red}")
  with
  | None -> Alcotest.fail "first rewrite should commit"
  | Some g' -> (
      match
        Rule_graph.try_rewrite g' ~consume:(nids [ 0 ])
          ~produce:(rules_of ".c{top:0}")
      with
      | None -> ()
      | Some _ -> Alcotest.fail "stale consumed node should be rejected")

(* Grouping two same-property rules across a conflicting intervening rule would
   reverse the cascade for an element matching the middle one; the rewrite must
   be rejected because the inherited edges form a cycle. *)
let try_rewrite_rejects_unsafe_cross () =
  let g =
    Rule_graph.of_rules (rules_of ".a{color:red}.k{color:green}.b{color:blue}")
  in
  match
    Rule_graph.try_rewrite g
      ~consume:(nids [ 0; 2 ])
      ~produce:(rules_of ".a,.b{color:red}")
  with
  | None -> ()
  | Some _ ->
      Alcotest.fail "merging across a conflicting rule should be rejected"

let try_rewrite_preserves_residual_source_slots () =
  let g =
    Rule_graph.of_rules
      (rules_of
         ".form-input{display:block;color:red}.marker{z-index:1}.form-select{display:block;color:blue}")
  in
  let produce =
    rules_of
      ".form-input,.form-select{display:block}.form-input{color:red}.form-select{color:blue}"
  in
  match Rule_graph.try_rewrite g ~consume:(nids [ 0; 2 ]) ~produce with
  | None -> Alcotest.fail "safe forms-style factoring should commit"
  | Some g' ->
      Alcotest.(check string)
        "residual selector keeps its original source slot"
        ".form-input,.form-select{display:block}.form-input{color:red}.marker{z-index:1}.form-select{color:blue}"
        (graph_to_string g')

(* --- conflict predicate: the bidimensional + specificity edge --- *)

let g_of s = Rule_graph.of_rules (rules_of s)

(* Same element, same property, tied specificity: order is observable. *)
let conflict_same_property_tie () =
  let g = g_of ".a{color:red}.a{color:blue}" in
  Alcotest.(check bool)
    "same-selector same-property pair conflicts" true
    (Rule_graph.conflict g (nid 0) (nid 1))

(* A strictly-higher-specificity competitor wins regardless of order, so it does
   not constrain order: no edge. *)
let conflict_higher_specificity_no_edge () =
  let g = g_of ".a{color:red}.a.b{color:blue}" in
  Alcotest.(check bool)
    "higher-specificity competitor is not order-constrained" false
    (Rule_graph.conflict g (nid 0) (nid 1))

(* Disjoint properties never interact in the cascade. Distinct selectors that
   can co-occur ([.a.b]) isolate the property dimension from the same-selector
   branch pin. *)
let conflict_disjoint_property () =
  let g = g_of ".a{color:red}.b{margin:0}" in
  Alcotest.(check bool)
    "disjoint properties never conflict" false
    (Rule_graph.conflict g (nid 0) (nid 1))

(* Two distinct mandatory IDs can never match one element. *)
let conflict_disjoint_selectors () =
  let g = g_of "#a{color:red}#b{color:blue}" in
  Alcotest.(check bool)
    "distinct-id selectors never overlap" false
    (Rule_graph.conflict g (nid 0) (nid 1))

(* Shorthand footprints can intersect even when neither declaration covers the
   other directionally: [border-top] and [border-color] both write
   [border-top-color]. *)
let conflict_intersecting_shorthands () =
  let g = g_of ".a{border-top:1px solid red}.a{border-color:blue}" in
  Alcotest.(check bool)
    "intersecting shorthand footprints conflict" true
    (Rule_graph.conflict g (nid 0) (nid 1))

let conflict_intersecting_shorthands_across_cooccurring_classes () =
  let g = g_of ".b{border-top-color:red}.a{border-color:green}" in
  Alcotest.(check bool)
    "different class selectors can overlap on one element" true
    (Rule_graph.conflict g (nid 0) (nid 1))

let conflict_logical_border_components () =
  let cases =
    [
      ( ".a{border-block-start:1px solid red}.a{border-block-start-width:2px}",
        "border-block-start intersects its width longhand" );
      ( ".a{border-inline:1px solid red}.a{border-inline-start-color:blue}",
        "border-inline intersects inline-start color" );
      ( ".a{border-inline-start:1px solid red}.a{border-inline-style:dashed}",
        "border-inline-start intersects inline style" );
      ( ".a{border-block:1px solid red}.a{border-block-style:dashed}",
        "border-block intersects block style" );
    ]
  in
  List.iter
    (fun (source, label) ->
      let g = g_of source in
      Alcotest.(check bool) label true (Rule_graph.conflict g (nid 0) (nid 1)))
    cases

let conflict_flex_and_transition_components () =
  let cases =
    [
      (".a{flex:1 1 auto}.a{flex-basis:0}", "flex intersects flex-basis");
      ( ".a{flex-flow:row wrap}.a{flex-direction:column}",
        "flex-flow intersects flex-direction" );
      ( ".a{transition:opacity 1s}.a{transition-duration:2s}",
        "transition intersects transition-duration" );
      ( ".a{transition:opacity 1s}.a{transition-behavior:allow-discrete}",
        "transition intersects transition-behavior" );
    ]
  in
  List.iter
    (fun (source, label) ->
      let g = g_of source in
      Alcotest.(check bool) label true (Rule_graph.conflict g (nid 0) (nid 1)))
    cases

(* --- canonical_order: topological projection --- *)

let canonical_order_pins_conflict () =
  let g = g_of ".a{color:red}.a{color:blue}" in
  Alcotest.(check (list int))
    "conflicting pair keeps source order" [ 0; 1 ]
    (int_order (Rule_graph.canonical_order g))

let canonical_order_keeps_disjoint_source_order () =
  let g = g_of "#b{color:red}#a{color:blue}" in
  Alcotest.(check (list int))
    "disjoint nodes emit first source appearance first" [ 0; 1 ]
    (int_order (Rule_graph.canonical_order g))

(* --- try_rewrite transaction validation --- *)

let try_rewrite_rejects_empty_consume () =
  let g = g_of ".a{color:red}.b{color:blue}" in
  match
    Rule_graph.try_rewrite g ~consume:[] ~produce:(rules_of ".c{top:0}")
  with
  | None -> ()
  | Some _ -> Alcotest.fail "empty consume must be rejected"

let try_rewrite_rejects_duplicate_consume () =
  let g = g_of ".a{color:red}.b{color:red}" in
  match
    Rule_graph.try_rewrite g
      ~consume:(nids [ 0; 0 ])
      ~produce:(rules_of ".a,.b{color:red}")
  with
  | None -> ()
  | Some _ -> Alcotest.fail "duplicate consume ids must be rejected"

let try_rewrite_bumps_generation () =
  let g = g_of ".a{color:red}.b{color:red}" in
  let g0 = Rule_graph.generation g in
  match
    Rule_graph.try_rewrite g
      ~consume:(nids [ 0; 1 ])
      ~produce:(rules_of ".a,.b{color:red}")
  with
  | None -> Alcotest.fail "identical-body group should commit"
  | Some g' ->
      Alcotest.(check bool)
        "generation increments on commit" true
        (Rule_graph.generation g' > g0);
      Alcotest.(check bool)
        "consumed nodes become dead" true
        ((not (Rule_graph.is_live g' (nid 0)))
        && not (Rule_graph.is_live g' (nid 1)))

(* --- declaration_body_key: identical-body bucketing --- *)

let declaration_body_key_buckets () =
  let g = g_of ".a{color:red;margin:0}.b{color:red;margin:0}.c{color:blue}" in
  Alcotest.(check bool)
    "identical bodies share a key" true
    (Rule_graph.declaration_body_key g (nid 0)
    = Rule_graph.declaration_body_key g (nid 1));
  Alcotest.(check bool)
    "different bodies differ" false
    (Rule_graph.declaration_body_key g (nid 0)
    = Rule_graph.declaration_body_key g (nid 2))

(* --- allocation / complexity guards --- *)

let measure f =
  Gc.full_major ();
  let w0 = Gc.minor_words () in
  let r = f () in
  ignore (Sys.opaque_identity r);
  Gc.minor_words () -. w0

(* [n] rules on the same selector and property form a dense (all-pairs) conflict
   graph - the worst case for edge construction and topological ordering. *)
let dense_conflict n =
  List.init n (fun i ->
      match Fmt.kstr rules_of ".x{color:rgb(%d,0,0)}" (i mod 200) with
      | [ r ] -> r
      | _ -> assert false)

(* Building the graph and projecting it back is at worst quadratic in the rule
   count (the all-pairs edge set). Doubling N must keep allocation well under
   the cubic 8x, catching any accidental super-quadratic regression. *)
let graph_build_is_subcubic () =
  let work n () =
    Rule_graph.of_rules (dense_conflict n) |> Rule_graph.canonical_order
  in
  let a1 = measure (work 60) in
  let a2 = measure (work 120) in
  Alcotest.(check bool)
    (Fmt.str "alloc %.0f -> %.0f (%.1fx for 2x N)" a1 a2 (a2 /. a1))
    true
    (a1 = 0. || a2 < a1 *. 6.)

(* A single transaction rebuilds graph state once: its allocation grows at most
   linearly with the live node count, never with the number of edges. *)
let try_rewrite_is_subquadratic () =
  let disjoint n =
    List.init n (fun i ->
        match Fmt.kstr rules_of ".c%d{color:red}" i with
        | [ r ] -> r
        | _ -> assert false)
  in
  let one_rewrite n () =
    let g = Rule_graph.of_rules (disjoint n) in
    Rule_graph.try_rewrite g
      ~consume:(nids [ 0; 1 ])
      ~produce:(rules_of ".c0,.c1{color:red}")
  in
  let a1 = measure (one_rewrite 60) in
  let a2 = measure (one_rewrite 120) in
  Alcotest.(check bool)
    (Fmt.str "alloc %.0f -> %.0f (%.1fx for 2x N)" a1 a2 (a2 /. a1))
    true
    (a1 = 0. || a2 < a1 *. 6.)

let suite =
  ( "rule_graph",
    [
      Alcotest.test_case "optimize output is idempotent after reparse" `Quick
        idempotent_after_reparse;
      Alcotest.test_case "graph projection keeps source order" `Quick
        graph_projection_keeps_source_order;
      Alcotest.test_case "conflicting pair keeps source order" `Quick
        conflicting_pair_is_pinned;
      Alcotest.test_case "try_rewrite commits a safe factoring" `Quick
        try_rewrite_safe_factoring;
      Alcotest.test_case "try_rewrite rejects a stale consumed node" `Quick
        try_rewrite_rejects_stale;
      Alcotest.test_case "try_rewrite rejects an unsafe cross-merge" `Quick
        try_rewrite_rejects_unsafe_cross;
      Alcotest.test_case "try_rewrite preserves residual source slots" `Quick
        try_rewrite_preserves_residual_source_slots;
      Alcotest.test_case "conflict: same-property tied pair" `Quick
        conflict_same_property_tie;
      Alcotest.test_case "conflict: higher specificity is not an edge" `Quick
        conflict_higher_specificity_no_edge;
      Alcotest.test_case "conflict: disjoint properties" `Quick
        conflict_disjoint_property;
      Alcotest.test_case "conflict: disjoint selectors" `Quick
        conflict_disjoint_selectors;
      Alcotest.test_case "conflict: intersecting shorthand footprints" `Quick
        conflict_intersecting_shorthands;
      Alcotest.test_case "conflict: shorthand across co-occurring classes"
        `Quick conflict_intersecting_shorthands_across_cooccurring_classes;
      Alcotest.test_case "conflict: logical border components" `Quick
        conflict_logical_border_components;
      Alcotest.test_case "conflict: flex and transition components" `Quick
        conflict_flex_and_transition_components;
      Alcotest.test_case "canonical_order pins a conflicting pair" `Quick
        canonical_order_pins_conflict;
      Alcotest.test_case "canonical_order keeps disjoint source order" `Quick
        canonical_order_keeps_disjoint_source_order;
      Alcotest.test_case "try_rewrite rejects empty consume" `Quick
        try_rewrite_rejects_empty_consume;
      Alcotest.test_case "try_rewrite rejects duplicate consume" `Quick
        try_rewrite_rejects_duplicate_consume;
      Alcotest.test_case "try_rewrite bumps generation and kills nodes" `Quick
        try_rewrite_bumps_generation;
      Alcotest.test_case "declaration_body_key buckets identical bodies" `Quick
        declaration_body_key_buckets;
      Alcotest.test_case "graph build/project is sub-cubic" `Quick
        graph_build_is_subcubic;
      Alcotest.test_case "try_rewrite is sub-quadratic" `Quick
        try_rewrite_is_subquadratic;
    ] )
