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

(* A consumed node can be the middle of a compact transitive source-order chain.
   It remains a zero-output relay, so the surviving endpoints cannot be reversed
   even when their rank requests it. *)
let rewrite_keeps_constraints_through_dead_relays () =
  let g =
    Rule_graph.of_rules (rules_of ".a{color:red}.b{color:green}.c{color:blue}")
  in
  match
    Rule_graph.try_rewrite g ~consume:(nids [ 1 ])
      ~produce:(rules_of ".b{margin:0}")
  with
  | None -> Alcotest.fail "disjoint replacement should commit"
  | Some g' ->
      let order =
        Rule_graph.canonical_order_by g' (fun id ->
            -Rule_graph.Node_id.to_int id)
        |> int_order
      in
      let rec position id index = function
        | [] -> max_int
        | node :: rest ->
            if Int.equal id node then index else position id (index + 1) rest
      in
      Alcotest.(check bool)
        "live endpoints retain their source constraint" true
        (position 0 0 order < position 2 0 order)

(* A later rewrite can add a back edge around a consumed node in a compact
   chain. Cycle detection must traverse the dead relay, not just live nodes. *)
let rewrite_rejects_cycle_through_dead_relay () =
  let g =
    Rule_graph.of_rules
      (rules_of
         ".early{color:black}.left{color:red}.dead{color:green}.right{color:blue}.late{color:white}")
  in
  match
    Rule_graph.try_rewrite g ~consume:(nids [ 2 ])
      ~produce:(rules_of ".dead{margin:0}")
  with
  | None -> Alcotest.fail "disjoint replacement should commit"
  | Some g' -> (
      match
        Rule_graph.try_rewrite g'
          ~consume:(nids [ 0; 4 ])
          ~produce:
            (rules_of ".late,.shared{color:white}.early,.shared{color:black}")
      with
      | None -> ()
      | Some _ -> Alcotest.fail "cycle through dead relay should be rejected")

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
      match
        Fmt.kstr rules_of ".c%d{color:rgb(%d,%d,0)}" i (i mod 256) (i / 256)
      with
      | [ r ] -> r
      | _ -> assert false)

(* Every pair can target the same element and writes a distinct value to the
   same property. Their required source order is a transitive chain, so graph
   construction and projection should remain linear rather than materialising
   the quadratic transitive closure. *)
let dense_graph_build_is_subquadratic () =
  let work n () =
    Rule_graph.of_rules (dense_conflict n) |> Rule_graph.canonical_order
  in
  let a1 = measure (work 400) in
  let a2 = measure (work 800) in
  Alcotest.(check bool)
    (Fmt.str "alloc %.0f -> %.0f (%.1fx for 2x N)" a1 a2 (a2 /. a1))
    true
    (a1 = 0. || a2 < a1 *. 2.5)

(* Two rules whose selectors can never tie on specificity are never
   order-constrained, whatever they write, so padding a run with rules at a
   second specificity adds no dependency to discover: twice the rules may cost
   about twice as much, not four times. Every rule below writes the same
   declaration, so neither run has an edge to build and what is left is the
   pairwise candidate enumeration. *)
let second_specificity_costs_no_pair () =
  let sheet_of rules = rules_of (String.concat "" rules) in
  let n = 300 in
  let one_class = sheet_of (List.init n (Fmt.str ".c%d{color:red}")) in
  let two_classes =
    sheet_of (List.init n (fun i -> Fmt.str ".d%d.e%d{color:red}" i i))
  in
  let padded = one_class @ two_classes in
  let a1 = measure (fun () -> Rule_graph.of_rules one_class) in
  let a2 = measure (fun () -> Rule_graph.of_rules padded) in
  Alcotest.(check bool)
    (Fmt.str "alloc %.0f -> %.0f (%.1fx for 2x N at a second specificity)" a1 a2
       (a2 /. a1))
    true
    (a1 = 0. || a2 < a1 *. 2.5)

(* An independent oracle for {!Rule_graph.precedes}: a plain walk of the edges
   the graph exposes, reusing nothing between questions. Dead nodes are walked
   through, since a compact chain can carry a live-to-live constraint across a
   node a rewrite consumed. *)
let reaches g source target =
  let seen = Array.make (max (Rule_graph.node_count g) 1) false in
  let rec walk = function
    | [] -> false
    | i :: rest ->
        let i = Rule_graph.Node_id.to_int i in
        if i = target then true
        else if seen.(i) then walk rest
        else begin
          seen.(i) <- true;
          walk (Rule_graph.successors g (nid i) @ rest)
        end
  in
  source <> target && walk (Rule_graph.successors g (nid source))

let check_reachability label g =
  let count = Rule_graph.node_count g in
  for i = 0 to count - 1 do
    for j = 0 to count - 1 do
      let expected =
        reaches g i j
        && Rule_graph.is_live g (nid i)
        && Rule_graph.is_live g (nid j)
      in
      Alcotest.(check bool)
        (Fmt.str "%s: precedes %d %d" label i j)
        expected
        (Rule_graph.precedes g (nid i) (nid j))
    done
  done

(* [precedes] answers a reachability question over edges that never change once
   a graph is published, so however it is answered it must agree with a walk of
   those edges on every ordered pair - including pairs joined only through a
   node a rewrite consumed, and pairs a compact chain relates without a direct
   edge. This pins the answer itself, not the rule order it feeds. *)
let precedes_agrees_with_a_walk () =
  List.iter
    (fun (label, css) -> check_reachability label (g_of css))
    [
      ("chain", ".a{color:red}.a{color:blue}.a{color:green}.a{color:teal}");
      ( "forks",
        ".a{color:red}.b{margin:0}.a{color:blue}.b{margin:1px}.a,.b{top:0}" );
      ( "mixed specificity",
        "#i{color:red}.a{color:blue}#i{color:teal}.a.c{color:teal}.a{color:lime}"
      );
      ("disjoint", "#a{color:red}#b{color:blue}#c{color:teal}");
    ];
  (* A committed rewrite leaves its inputs behind as dead relays, so the walk
     has to cross them. *)
  let g =
    g_of ".early{color:black}.mid{color:red}.mid{color:blue}.late{color:white}"
  in
  match
    Rule_graph.try_rewrite g
      ~consume:(nids [ 1; 2 ])
      ~produce:(rules_of ".mid{color:red}.mid{color:blue}")
  with
  | None -> Alcotest.fail "replacement should commit"
  | Some g' -> check_reachability "after rewrite" g'

(* Orienting a factoring asks which of two nodes has to come first, once per
   ordered pair of source references, so a run of N rules is asked about N^2
   pairs. The edges are settled the moment the graph is published, so all of
   those answers follow from one pass over them: the nodes expanded to answer
   them must stay bounded by the node count, not grow with the pairs asked
   about. Every rule below writes a distinct value for one property on one
   element, the shape that makes the run one long chain. *)
let reachability_settles_once () =
  let expansions n =
    let g = Rule_graph.of_rules (dense_conflict n) in
    let count = Rule_graph.node_count g in
    let ask i j = ignore (Sys.opaque_identity (Rule_graph.precedes g i j)) in
    (* Deepest source first, then shallowest: asking the deepest first leaves
       every later question facing a cone an earlier one already settled, which
       only stays cheap if a settled node is read rather than descended again.
       Asking the shallowest first hides that. *)
    for i = count - 1 downto 0 do
      for j = 0 to count - 1 do
        ask (nid i) (nid j)
      done
    done;
    for i = 0 to count - 1 do
      for j = 0 to count - 1 do
        ask (nid i) (nid j)
      done
    done;
    (count, Rule_graph.reachability_expansions g)
  in
  List.iter
    (fun n ->
      let count, spent = expansions n in
      Alcotest.(check bool)
        (Fmt.str "%d nodes, %d pairs asked: %d nodes expanded" count
           (2 * count * count)
           spent)
        true (spent <= count))
    [ 200; 400 ]

(* Rules that all write the same declaration have no order to discover: an
   identical pair is its own winner wherever the two sit. The run sits at one
   specificity, where partitioning candidates by specificity separates nothing,
   so the pairs are only ruled out by what each rule writes. Doubling N doubles
   the rules to index and adds no dependency to find, so it may cost about twice
   as much, not four times. *)
let identical_declaration_costs_no_pair () =
  let sheet n =
    rules_of (String.concat "" (List.init n (Fmt.str ".c%d{color:red}")))
  in
  let small = sheet 400 in
  let large = sheet 800 in
  let a1 = measure (fun () -> Rule_graph.of_rules small) in
  let a2 = measure (fun () -> Rule_graph.of_rules large) in
  Alcotest.(check bool)
    (Fmt.str "alloc %.0f -> %.0f (%.1fx for 2x N at one specificity)" a1 a2
       (a2 /. a1))
    true
    (a1 = 0. || a2 < a1 *. 2.5)

(* Distinct declarations sharing a property still have no dependency when the
   selectors are provably disjoint. Mandatory IDs and element names are cheap
   exclusive selector facts, so doubling either run should cost about twice as
   much rather than enumerating every declaration pair at one specificity. *)
let disjoint_selector_facts_cost_no_pair () =
  let check label selector =
    let sheet n =
      List.init n (fun i ->
          Fmt.str "%s{color:rgb(%d,%d,0)}" (selector i) (i mod 256) (i / 256))
      |> String.concat "" |> rules_of
    in
    let small = sheet 400 in
    let large = sheet 800 in
    let a1 = measure (fun () -> Rule_graph.of_rules small) in
    let a2 = measure (fun () -> Rule_graph.of_rules large) in
    Alcotest.(check bool)
      (Fmt.str "%s alloc %.0f -> %.0f (%.1fx for 2x N)" label a1 a2 (a2 /. a1))
      true
      (a1 = 0. || a2 < a1 *. 2.5)
  in
  check "mandatory IDs" (Fmt.str "#i%d");
  check "element names" (Fmt.str "e%d")

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

(* Every rule here writes one selector and a block of custom properties no other
   rule names, so the same-selector merge order compares all N(N-1)/2 pairs and
   not one of them short-circuits on a conflict. Whether two blocks commute is a
   question about the slots each writes, answered by reading each block once
   rather than once per declaration facing it, so the pair loop costs no
   allocation of its own: doubling N may cost about twice as much, never the
   quadratic four times.

   Both runs stay above the node count at which the enumerator stops offering
   its wider candidate kinds, so what separates them is the pair loop alone. *)
let same_selector_commute_is_subquadratic () =
  let block i =
    List.init 20 (fun k -> Fmt.str "--p%d-%d:%d" i k ((i * 20) + k))
    |> String.concat ";"
  in
  let sheet n =
    List.init n (fun i -> Fmt.str ".q{%s}" (block i))
    |> String.concat "" |> rules_of
  in
  let candidates graph () =
    Rule_candidate.enumerate ~ctx:Ctx.fragment ~finalize:Fun.id graph
  in
  let a1 = measure (candidates (Rule_graph.of_rules (sheet 150))) in
  let a2 = measure (candidates (Rule_graph.of_rules (sheet 300))) in
  Alcotest.(check bool)
    (Fmt.str "alloc %.0f -> %.0f (%.1fx for 2x N)" a1 a2 (a2 /. a1))
    true
    (a1 = 0. || a2 < a1 *. 3.)

(* Selector compatibility is not transitive, so a run of same-body rules groups
   only when EVERY pair in it agrees. [.y] pairs with both of the others and
   they do not pair with each other, so the three never share a selector list
   and the enumerator settles for the compatible subset. *)
let grouping_respects_non_transitive_compatibility () =
  let newer = ".x:user-valid{color:red}" in
  let plain = ".y{color:red}" in
  let guarded = ":is(:where(.group):hover .z){color:red}" in
  Alcotest.(check string)
    "newer groups with plain" ".x:user-valid,.y{color:red}"
    (optimize_str (newer ^ plain));
  Alcotest.(check string)
    "plain groups with guarded" ".y,:is(:where(.group):hover .z){color:red}"
    (optimize_str (plain ^ guarded));
  Alcotest.(check string)
    "newer never groups with guarded"
    ".x:user-valid{color:red}:is(:where(.group):hover .z){color:red}"
    (optimize_str (newer ^ guarded));
  Alcotest.(check string)
    "all three settle for the compatible subset"
    ".x:user-valid{color:red}.y,:is(:where(.group):hover .z){color:red}"
    (optimize_str (newer ^ plain ^ guarded))

(* Every rule here carries the same body and a selector no other rule shares, so
   the whole run lands in one identical-body bucket and grouping it asks about
   all N(N-1)/2 pairs. Whether two selectors can share a selector list is
   settled by two predicates read off each of them alone, so reading each
   selector once is enough and the pair loop costs no allocation of its own:
   doubling N may cost about twice as much, never the quadratic four times.

   The [:where()] nodes hold no group or peer marker, so neither predicate stops
   early and each pair pays a full walk of both selectors. Both runs stay above
   the 128 nodes at which the enumerator drops its wider candidate kinds, so
   what separates them is the pair loop alone. *)
let identical_body_grouping_is_subquadratic () =
  let sheet n =
    List.init n
      (Fmt.str ":where(.a,.b):where(.c,.d):where(.e,.f) .w%d{color:red}")
    |> String.concat "" |> rules_of
  in
  let candidates graph () =
    Rule_candidate.enumerate ~ctx:Ctx.fragment ~finalize:Fun.id graph
  in
  let a1 = measure (candidates (Rule_graph.of_rules (sheet 200))) in
  let a2 = measure (candidates (Rule_graph.of_rules (sheet 400))) in
  Alcotest.(check bool)
    (Fmt.str "alloc %.0f -> %.0f (%.1fx for 2x N)" a1 a2 (a2 /. a1))
    true
    (a1 = 0. || a2 < a1 *. 2.6)

(* Every rule here sits on one selector and carries a block of custom properties
   no other rule names, half of them in a nested block, so deciding whether the
   run can merge asks about all N(N-1)/2 pairs and not one short-circuits.
   Whether a nested body commutes with a later declaration block is a question
   about the slots each writes, and the relation reads the same from either
   side: indexing the nested body once per rule and reading each block once for
   the whole run leaves the pair loop no allocation of its own, so doubling N
   may cost about twice as much, never the quadratic four times.

   Both runs stay above the 128 nodes at which the enumerator drops its wider
   candidate kinds, so what separates them is the pair loop alone. *)
let nested_merge_safety_is_subquadratic () =
  let block prefix i =
    List.init 20 (fun k -> Fmt.str "--%s%d-%d:%d" prefix i k ((i * 20) + k))
    |> String.concat ";"
  in
  let sheet n =
    List.init n (fun i ->
        Fmt.str ".q{%s;&:hover{%s}}" (block "o" i) (block "n" i))
    |> String.concat "" |> rules_of
  in
  let candidates graph () =
    Rule_candidate.enumerate ~ctx:Ctx.fragment ~finalize:Fun.id graph
  in
  let a1 = measure (candidates (Rule_graph.of_rules (sheet 200))) in
  let a2 = measure (candidates (Rule_graph.of_rules (sheet 400))) in
  Alcotest.(check bool)
    (Fmt.str "alloc %.0f -> %.0f (%.1fx for 2x N)" a1 a2 (a2 /. a1))
    true
    (a1 = 0. || a2 < a1 *. 2.6)

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
      Alcotest.test_case "rewrite constraints cross dead relays" `Quick
        rewrite_keeps_constraints_through_dead_relays;
      Alcotest.test_case "rewrite rejects cycle through a dead relay" `Quick
        rewrite_rejects_cycle_through_dead_relay;
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
      Alcotest.test_case "precedes agrees with a walk of the edges" `Quick
        precedes_agrees_with_a_walk;
      Alcotest.test_case "reachability settles once per node" `Quick
        reachability_settles_once;
      Alcotest.test_case "dense graph build/project is sub-quadratic" `Quick
        dense_graph_build_is_subquadratic;
      Alcotest.test_case "try_rewrite is sub-quadratic" `Quick
        try_rewrite_is_subquadratic;
      Alcotest.test_case "a second specificity costs no pair" `Quick
        second_specificity_costs_no_pair;
      Alcotest.test_case "an identical declaration costs no pair" `Quick
        identical_declaration_costs_no_pair;
      Alcotest.test_case "disjoint selector facts cost no pair" `Quick
        disjoint_selector_facts_cost_no_pair;
      Alcotest.test_case "same-selector commute is sub-quadratic" `Quick
        same_selector_commute_is_subquadratic;
      Alcotest.test_case "grouping respects non-transitive compatibility" `Quick
        grouping_respects_non_transitive_compatibility;
      Alcotest.test_case "identical-body grouping is sub-quadratic" `Quick
        identical_body_grouping_is_subquadratic;
      Alcotest.test_case "nested-merge safety is sub-quadratic" `Quick
        nested_merge_safety_is_subquadratic;
    ] )
