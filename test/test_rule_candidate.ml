open Cascade
open Css_test_helpers

let rules css =
  match Css.of_string ~strict:false css with
  | Ok { stylesheet; _ } ->
      List.filter_map
        (function Stylesheet.Rule r -> Some r | _ -> None)
        (Css.statements stylesheet)
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let render rules =
  String.concat ""
    (List.map (Pp.to_string ~minify:true Stylesheet.pp_rule) rules)

let candidates css =
  css |> rules |> Rule_graph.of_rules
  |> Rule_candidate.enumerate ~ctx:Ctx.fragment ~finalize:Fun.id

let candidates_with_ctx ctx css =
  css |> rules |> Rule_graph.of_rules
  |> Rule_candidate.enumerate ~ctx ~finalize:Fun.id

let large_normal_css prefix =
  let fillers =
    List.init 129 (fun i -> Fmt.str ".f%d{left:%dpx;top:%dpx}" i i i)
    |> String.concat ""
  in
  prefix ^ fillers

let has_candidate ~kind ~produce candidates =
  List.exists
    (fun candidate ->
      Rule_rewrite.equal_kind candidate.Rule_rewrite.kind kind
      && String.equal produce (render candidate.produce))
    candidates

let default_factoring_uses_first_value_with_overrides () =
  let candidates =
    candidates
      ".a{display:flex;margin:1px}.b{display:flex;margin:2px}.c{display:flex;margin:3px}"
  in
  Alcotest.(check bool)
    "default factoring candidate keeps first value and emits overrides" true
    (has_candidate ~kind:Default_factoring
       ~produce:".a,.b,.c{display:flex;margin:1px}.b{margin:2px}.c{margin:3px}"
       candidates)

let large_normal_mode_keeps_local_exact_factoring () =
  let candidates =
    candidates
      (large_normal_css ".a{display:flex;color:red}.b{display:flex;color:blue}")
  in
  Alcotest.(check bool)
    "large normal mode keeps local exact shared-declaration candidates" true
    (has_candidate ~kind:Exact_shared_declarations
       ~produce:".a,.b{display:flex}.a{color:red}.b{color:blue}" candidates)

let large_normal_mode_keeps_local_default_factoring () =
  let candidates =
    candidates
      (large_normal_css
         ".a{display:flex;margin:1px}.b{display:flex;margin:2px}.c{display:flex;margin:3px}")
  in
  Alcotest.(check bool)
    "large normal mode keeps local default-value candidates" true
    (has_candidate ~kind:Default_factoring
       ~produce:".a,.b,.c{display:flex;margin:1px}.b{margin:2px}.c{margin:3px}"
       candidates)

let exact_shared_declarations_prune_long_selector () =
  let candidates =
    candidates
      "#f{top:0;position:absolute;width:100%}.b{padding:16px;position:absolute;z-index:1}.b-arrow{position:absolute}.very-long-closebtn-selector{height:21px;position:absolute;width:21px}"
  in
  Alcotest.(check bool)
    "long selector is not pulled into the shared position:absolute group" false
    (List.exists
       (fun candidate ->
         candidate.Rule_rewrite.kind = Exact_shared_declarations
         && Astring.String.is_infix ~affix:".very-long-closebtn-selector"
              (render candidate.produce))
       candidates)

let same_selector_candidate_orders_commuting_blocks_canonically () =
  let candidates = candidates ".x{margin:0}.x{color:red}" in
  Alcotest.(check bool)
    "same-selector candidate canonicalizes commuting declaration blocks" true
    (has_candidate ~kind:Same_selector ~produce:".x{color:red;margin:0}"
       candidates)

let selector_list_extension_keeps_strict_alternative () =
  let css = ".a,.b{color:red}.c{color:red}.d{color:red}" in
  let strict = candidates css in
  Alcotest.(check bool)
    "plain context ignores the existing selector list" true
    (has_candidate ~kind:Identical_body ~produce:".c,.d{color:red}" strict);
  Alcotest.(check bool)
    "plain context does not extend the existing selector list" false
    (has_candidate ~kind:Identical_body ~produce:".a,.b,.c,.d{color:red}" strict);
  let extended =
    candidates_with_ctx (Ctx.with_extend_lists true Ctx.fragment) css
  in
  Alcotest.(check bool)
    "extended context still exposes the strict alternative" true
    (has_candidate ~kind:Identical_body ~produce:".c,.d{color:red}" extended);
  Alcotest.(check bool)
    "extended context exposes the locally better selector-list extension" true
    (has_candidate ~kind:Identical_body ~produce:".a,.b,.c,.d{color:red}"
       extended)

let exact_factoring_keeps_later_shorthand_overlap () =
  let candidates =
    candidates
      ".c{border-color:green}.a.c{border-top:3px solid red;border-color:green}"
  in
  Alcotest.(check bool)
    "border-color must not be lifted before an earlier border-top leftover"
    false
    (has_candidate ~kind:Exact_shared_declarations
       ~produce:".a.c,.c{border-color:green}.a.c{border-top:3px solid red}"
       candidates)

let exact_factoring_keeps_prior_member_leftover_overlap () =
  let candidates =
    candidates
      ".b{border-color:green;border-top-color:red}.a{border-color:green}.a.b.c{border-color:green}"
  in
  Alcotest.(check bool)
    "border-color must not be lifted away from a prior overlapping leftover"
    false
    (has_candidate ~kind:Exact_shared_declarations
       ~produce:".a,.a.b.c,.b{border-color:green}.b{border-top-color:red}"
       candidates)

let default_factoring_keeps_prior_member_leftover_overlap () =
  let candidates =
    candidates
      ".b{border-color:green;border-top-color:red}.a{border-color:green;margin-top:7px}.a.b.c{border-color:green}"
  in
  Alcotest.(check bool)
    "default group must not lift a later default before a prior overlap" false
    (has_candidate ~kind:Default_factoring
       ~produce:
         ".a,.a.b.c,.b{border-color:green}.b{border-top-color:red}.a{margin-top:7px}"
       candidates)

(* Whether a run of same-selector rules can merge is a question about the pairs
   a merge reorders: each later rule's declarations move ahead of every nested
   block an earlier rule carries. A pair counts when it writes a common cascade
   slot at the same weight with a different value, so two nested blocks that
   write one slot from two different rules must be read as a multiset - the
   answer for a later block is not settled by whichever of them was seen first.
   Each case below puts two nested writers on one slot and a third rule behind
   them. *)
let nested_merge_reads_a_slot_as_a_multiset () =
  let case name expected css =
    Alcotest.(check bool)
      name expected
      (Commute.nested_merge_is_safe ~stats:(Stats.v ()) (rules css))
  in
  case "a later nested writer still blocks a matching earlier one" false
    ".q{top:0;&:hover{color:red}}.q{left:0;&:hover{color:blue}}.q{color:red}";
  case "two nested writers that agree with the later block merge" true
    ".q{top:0;&:hover{color:red}}.q{left:0;&:hover{color:red}}.q{color:red}";
  case "an earlier nested writer still blocks a matching later one" false
    ".q{top:0;&:hover{--x:1}}.q{left:0;&:hover{--y:1}}.q{--x:2}";
  case "a weight of its own keeps a nested writer out of the slot" true
    ".q{top:0;&:hover{color:red!important}}.q{left:0;&:hover{color:red}}.q{color:red}";
  case "and does not excuse the writer at the block's weight" false
    ".q{top:0;&:hover{color:red!important}}.q{left:0;&:hover{color:blue}}.q{color:red}";
  case "a shorthand behind a longhand on one slot blocks the merge" false
    ".q{top:0;&:hover{margin-top:1px}}.q{left:0;&:hover{margin:0}}.q{margin-top:1px}";
  case "a slot neither of them writes is free" true
    ".q{top:0;&:hover{margin-top:1px}}.q{left:0;&:hover{margin:0}}.q{padding:1rem}";
  case "two values of one custom property block the merge" false
    ".q{top:0;&:hover{--x:1}}.q{left:0;&:hover{--x:2}}.q{--x:1}";
  case "one value of it does not" true
    ".q{top:0;&:hover{--x:1}}.q{left:0;&:hover{--x:1}}.q{--x:1}";
  case "nor does a custom property of another name" true
    ".q{top:0;&:hover{--x:1}}.q{left:0;&:hover{--x:2}}.q{--y:1}";
  case "a custom property at another weight writes another slot" true
    ".q{top:0;&:hover{--x:1!important}}.q{left:0;&:hover{--x:2}}.q{--x:1!important}"

(* Two runs of the same length carrying the same declarations and the same
   nested declarations, differing only in where the nested blocks sit: [spread]
   hangs one off every rule, [front] hangs all of them off the first. Deciding
   the run asks, of each rule's declarations, whether they commute with every
   nested block written before them - one question about the union of those
   blocks, not one question per block - so each block is read once whichever run
   it belongs to and the two cost the same. Asking it per (block, later block)
   pair leaves [front] alone, since it has one block, and makes [spread] cost a
   probe per pair.

   The probes are counted, not timed: a probe allocates nothing, so allocation
   cannot tell the two ways apart, and a time ratio moves with whatever else the
   host runs. Each block after the first faces the nested bodies ahead of it
   once, so both runs make [n - 1] probes; asking per pair makes [spread] make
   [n (n - 1) / 2]. *)
let nested_merge_safety_reads_each_block_once () =
  let n = 200 in
  let spread =
    List.init n (fun i -> Fmt.str ".q{--a%d:%d;&:hover{--n%d:%d}}" i i i i)
    |> String.concat "" |> rules
  in
  let front =
    let nested =
      List.init n (fun i -> Fmt.str "--n%d:%d" i i) |> String.concat ";"
    in
    Fmt.str ".q{--a0:0;&:hover{%s}}" nested
    ^ (List.init (n - 1) (fun i -> Fmt.str ".q{--a%d:%d}" (i + 1) (i + 1))
      |> String.concat "")
    |> rules
  in
  let probes rules =
    let stats = Stats.v () in
    let safe = Commute.nested_merge_is_safe ~stats rules in
    Alcotest.(check bool)
      "neither run writes a slot twice, so it merges" true safe;
    (Stats.snapshot stats).counters.nested_merge_probes
  in
  Alcotest.(check int)
    "nested blocks spread over the run" (n - 1) (probes spread);
  Alcotest.(check int)
    "nested blocks all on the first rule" (n - 1) (probes front)

(* --- allocation / complexity guards --- *)

(* [members] rules that write the same [props] custom properties, each differing
   on one of them, is the densest shape the default-value search sees: every
   property is common to every member, so each is a candidate default and each
   member is a candidate override. *)
let dense_default_sheet ~members ~props =
  List.init members (fun m ->
      let decls =
        List.init props (fun p ->
            if p = m mod props then Fmt.str "--p%d:%dpx" p (p + 1)
            else Fmt.str "--p%d:0px" p)
        |> String.concat ";"
      in
      Fmt.str ".s%d{%s}" m decls)
  |> String.concat ""

(* Deciding a default group reads each member's body once per entry to find the
   declaration that carries that entry's property. The number of entries and the
   length of a body both grow with the shared property count, and so does the
   number of groups examined, so a per-declaration allocation inside that lookup
   makes the whole search cubic in [props]. Building only the lists the search
   keeps leaves it quadratic: doubling [props] must stay well below the cubic
   8x. *)
let default_factoring_lookup_is_subcubic () =
  let work props () =
    dense_default_sheet ~members:8 ~props
    |> rules |> Rule_graph.of_rules
    |> Rule_candidate.enumerate ~ctx:Ctx.fragment ~finalize:Fun.id
  in
  let a1 = measure (work 16) in
  let a2 = measure (work 32) in
  Alcotest.(check bool)
    (Fmt.str "alloc %.0f -> %.0f (%.1fx for 2x shared properties)" a1 a2
       (a2 /. a1))
    true
    (a1 = 0. || a2 < a1 *. 5.5)

(* [pairs] pairs of neighbouring rules, each pair sharing one custom property
   and each member carrying one of its own. Every pair is an exact-factoring
   group whose two members sit next to each other in source order, so the span
   between the group's first and last rule holds nothing, and the whole sheet
   grows without ever putting a rule inside one of those spans. *)
let adjacent_shared_pairs ~pairs =
  List.init pairs (fun i ->
      Fmt.str ".a%d{--s%d:1;--x%d:2}.b%d{--s%d:1;--y%d:3}" i i i i i i)
  |> String.concat ""

(* Before committing a factored group the search asks whether any rule outside
   it sits between the group's first and last member and would change meaning by
   being crossed. Only a rule inside that origin span can answer yes, and a
   group of two neighbours has none, so every node is rejected on an integer
   comparison. Listing the live nodes to scan them, and capturing the group's
   ids in a closure at each one, spends that scan's whole allocation on nodes it
   walks straight past - and both the number of groups and the number of nodes
   grow with the sheet, so the waste is quadratic in it. Scanning without
   building the list leaves the rejection allocation-free: quadrupling the sheet
   must stay near the linear 4x, well below the quadratic 16x. *)
let external_conflict_scan_is_subquadratic () =
  let work pairs () =
    adjacent_shared_pairs ~pairs
    |> rules |> Rule_graph.of_rules
    |> Rule_candidate.enumerate ~ctx:Ctx.fragment ~finalize:Fun.id
  in
  let a1 = measure (work 64) in
  let a2 = measure (work 256) in
  Alcotest.(check bool)
    (Fmt.str "alloc %.0f -> %.0f (%.1fx for 4x rules)" a1 a2 (a2 /. a1))
    true
    (a1 = 0. || a2 < a1 *. 5.)

(* The first member provides the only default. Every later member writes an
   extra declaration that the provider does not, and its selector costs more
   than the provider declaration it shares, so pruning must discard it. The
   group can therefore never keep two members. A same-sized group whose members
   all repeat the provider's declaration is the control: those members can
   survive and the full candidate checks still have to run. Rejecting the doomed
   group before constructing entries and leftovers keeps its allocation well
   below the control. *)
let default_factoring_rejects_a_doomed_group_early () =
  let doomed =
    List.init 8 (fun i ->
        if i = 0 then ".p{color:red}"
        else
          Fmt.str
            ".selector-that-cannot-pay-for-the-group-%d{color:blue;--own-%d:1}"
            i i)
    |> String.concat "" |> rules |> Rule_graph.of_rules
  in
  let survivable =
    List.init 8 (fun i -> Fmt.str ".s%d{color:red}" i)
    |> String.concat "" |> rules |> Rule_graph.of_rules
  in
  let work graph () =
    Rule_candidate.enumerate ~ctx:Ctx.fragment ~finalize:Fun.id graph
  in
  let rejected = measure (work doomed) in
  let control = measure (work survivable) in
  Alcotest.(check bool)
    (Fmt.str "doomed group %.0f words, survivable control %.0f" rejected control)
    true
    (control = 0. || rejected < control *. 0.4)

(* Each run of four neighbours writes one property with differing values and
   each run of five shares one declaration, so both factoring kinds find groups
   all along it. Past 128 nodes the search spends one budget of subsets on the
   earliest buckets, so a rewrite there moves what the later ones are given. *)
let factoring_run n =
  List.init n (fun i ->
      Fmt.str ".c%d{--p%d:%d;--q%d:1;color:rgb(%d,%d,0)}" i (i / 4) (i mod 3)
        (i / 5) (i mod 256) (i / 256))
  |> String.concat ""

(* Identical bodies, repeated selectors and long selector branches a receiver
   can take, so every candidate kind is offered. *)
let mixed_run n =
  List.init n (fun i ->
      match i mod 5 with
      | 0 -> Fmt.str ".m%d{margin:0;padding:%dpx}" (i / 10) (i mod 7)
      | 1 -> Fmt.str ".same{color:red;--s%d:1}" (i mod 3)
      | 2 -> Fmt.str ".r%d{top:0}" (i mod 4)
      | 3 ->
          Fmt.str ".r%d,.a-long-selector-branch-%d .x{top:0;left:%dpx}"
            (i mod 4) i (i mod 3)
      | _ -> ".t{display:flex;align-items:center}")
  |> String.concat ""

let kind_name = function
  | Rule_rewrite.Identical_body -> "identical-body"
  | Same_selector -> "same-selector"
  | Exact_shared_declarations -> "exact-shared"
  | Selector_branch_inline -> "selector-inline"
  | Default_factoring -> "default-factoring"

(* A candidate set as a sorted list: which candidates a graph offers, not the
   order they were found in or the graph that first offered them. *)
let observed (candidates : Rule_rewrite.candidate list) =
  List.map
    (fun (c : Rule_rewrite.candidate) ->
      Fmt.str "%s|%a|%d|%s" (kind_name c.kind)
        Fmt.(list ~sep:comma int)
        (List.map Rule_graph.Node_id.to_int c.consume)
        c.saving (render c.produce))
    candidates
  |> List.sort String.compare

(* The largest saving that commits, as the scheduler would take it. *)
let best_commit g candidates =
  List.sort
    (fun (a : Rule_rewrite.candidate) (b : Rule_rewrite.candidate) ->
      Int.compare b.saving a.saving)
    candidates
  |> List.find_map (fun (c : Rule_rewrite.candidate) ->
      match Rule_graph.rewrite g ~consume:c.consume ~produce:c.produce with
      | Ok g' -> Some (c, g')
      | Error _ -> None)

(* A rewrite carries the candidate set to the new graph without enumerating it
   again. What it carries must be what a full enumeration of the new graph
   offers, for every kind, whether it is refreshed after every commit of a
   greedy run or after several. *)
let a_refresh_leaves_what_a_full_enumeration_offers () =
  let check ?(commits_at_least = 3) ~ctx ~every label css =
    let g = Rule_graph.of_rules (rules css) in
    let state = Rule_candidate.start ~ctx ~finalize:Fun.id g in
    ignore (Rule_candidate.refresh state);
    let rec loop step g candidates =
      match best_commit g candidates with
      | None -> step
      | Some (c, g') ->
          Rule_candidate.rewrite state g' ~consume:c.consume;
          let step = step + 1 in
          if step mod every <> 0 then
            let live = Rule_graph.live_nodes g' in
            loop step g'
              (List.filter
                 (fun (other : Rule_rewrite.candidate) ->
                   List.for_all
                     (fun id ->
                       List.exists
                         (fun live -> Rule_graph.Node_id.compare live id = 0)
                         live)
                     other.consume)
                 candidates)
          else begin
            ignore (Rule_candidate.refresh state);
            let candidates = Rule_candidate.candidates state in
            Alcotest.(check (list string))
              (Fmt.str "%s: candidates after commit %d" label step)
              (observed (Rule_candidate.enumerate ~ctx ~finalize:Fun.id g'))
              (observed candidates);
            loop step g' candidates
          end
    in
    let commits = loop 0 g (Rule_candidate.candidates state) in
    Alcotest.(check bool)
      (Fmt.str "%s: %d commits" label commits)
      true
      (commits >= commits_at_least)
  in
  let fragment = Ctx.fragment
  and aggressive = Ctx.v ~aggressive:true `Fragment in
  check ~ctx:fragment ~every:1 "indexed" (factoring_run 200);
  check ~ctx:fragment ~every:3 "indexed, refreshed every third commit"
    (factoring_run 200);
  check ~ctx:fragment ~every:1 "exhaustive" (factoring_run 60);
  check ~ctx:fragment ~every:1 "mixed indexed" (mixed_run 180);
  check ~ctx:fragment ~every:1 "mixed exhaustive" (mixed_run 50);
  check ~ctx:aggressive ~every:1 "aggressive" (mixed_run 60);
  (* The first commit merges the [#d] rules, which puts the member that
     overrides [display] ahead of the provider and drops that entry from the
     default group. *)
  check ~commits_at_least:1 ~ctx:fragment ~every:1 "an order the rewrite forces"
    "#d{display:flex;color:red!important}#d{padding-left:4px}#d{display:flex;color:blue}.e.f{display:block;color:blue}.e.f{color:red;color:red!important}";
  (* The second commit produces a [.cccccccc] rule inside the span of a group it
     crosses. *)
  check ~commits_at_least:2 ~ctx:fragment ~every:1 "a crossing a rewrite adds"
    "a:hover{padding-left:4px}.g>.h{color:green;padding:0}.cccccccc{display:block;padding-left:4px}.a{margin:1px;padding:0}.cccccccc{padding-left:4px;color:blue}";
  (* The second commit consumes the only rule a group would have crossed. *)
  check ~commits_at_least:2 ~ctx:fragment ~every:1
    "a crossing a rewrite removes"
    ".e.f{display:block;margin:0}p{margin:2px;font-weight:700}#d{display:flex;margin-top:3px}p{display:flex;margin:0}p{margin-top:3px;margin:2px}"

(* A candidate set read on the printed rules alone: each consumed rule by where
   it prints, so the candidates of two graphs that print the same rules compare
   whatever node ids they gave them. *)
let printed_observed g (candidates : Rule_rewrite.candidate list) =
  let ranks = Array.make (Rule_graph.node_count g) (-1) in
  Array.iteri
    (fun rank id -> ranks.(Rule_graph.Node_id.to_int id) <- rank)
    (Rule_graph.canonical_order g);
  List.map
    (fun (c : Rule_rewrite.candidate) ->
      Fmt.str "%s|%a|%d|%s" (kind_name c.kind)
        Fmt.(list ~sep:comma int)
        (List.sort Int.compare
           (List.map (fun id -> ranks.(Rule_graph.Node_id.to_int id)) c.consume))
        c.saving (render c.produce))
    candidates
  |> List.sort String.compare

let printed_rules g =
  Array.to_list
    (Array.map (Rule_graph.node_rule g) (Rule_graph.canonical_order g))

(* Pairs of identical bodies, which merge, so a run of [2 * n] rules ends with
   [n]. *)
let shrinking_run n =
  List.init n (fun i -> Fmt.str ".a%d{width:%dpx}.b%d{width:%dpx}" i i i i)
  |> String.concat ""

(* The candidates are a function of the rules a graph prints: after each commit
   of a greedy run, the run's own graph offers exactly what a graph built afresh
   from the rules it prints offers, though its rules print at other source
   positions and fewer of them print than it started with. *)
let a_rewritten_graph_offers_what_a_fresh_one_does () =
  let check ?(commits_at_least = 3) ~ctx label css =
    let g = Rule_graph.of_rules (rules css) in
    let state = Rule_candidate.start ~ctx ~finalize:Fun.id g in
    ignore (Rule_candidate.refresh state);
    let rec loop step g =
      match best_commit g (Rule_candidate.candidates state) with
      | None -> step
      | Some (c, g') ->
          Rule_candidate.rewrite state g' ~consume:c.consume;
          ignore (Rule_candidate.refresh state);
          let step = step + 1 in
          let fresh = Rule_graph.of_rules (printed_rules g') in
          Alcotest.(check (list string))
            (Fmt.str "%s: candidates after commit %d" label step)
            (printed_observed fresh
               (Rule_candidate.enumerate ~ctx ~finalize:Fun.id fresh))
            (printed_observed g' (Rule_candidate.candidates state));
          loop step g'
    in
    let commits = loop 0 g in
    Alcotest.(check bool)
      (Fmt.str "%s: %d commits" label commits)
      true
      (commits >= commits_at_least)
  in
  check ~ctx:Ctx.fragment "a run that shrinks past 128 rules"
    (shrinking_run 70 ^ factoring_run 20);
  check ~ctx:Ctx.fragment "mixed" (mixed_run 180)

(* And it costs what the rewrite changed: one commit leaves most buckets and
   most of the groups they reach as they were, so carrying the set allocates a
   fraction of what enumerating the new graph does. *)
let a_refresh_costs_what_the_rewrite_changed () =
  let g = Rule_graph.of_rules (rules (factoring_run 200)) in
  let ctx = Ctx.fragment in
  let state = Rule_candidate.start ~ctx ~finalize:Fun.id g in
  ignore (Rule_candidate.refresh state);
  match best_commit g (Rule_candidate.candidates state) with
  | None -> Alcotest.fail "one of the candidates should commit"
  | Some (c, g') ->
      let carried =
        measure (fun () ->
            Rule_candidate.rewrite state g' ~consume:c.consume;
            Rule_candidate.refresh state)
      in
      let fresh =
        measure (fun () -> Rule_candidate.enumerate ~ctx ~finalize:Fun.id g')
      in
      Alcotest.(check bool)
        (Fmt.str "carried %.0f words, fresh %.0f" carried fresh)
        true
        (fresh = 0. || carried < fresh *. 0.2)

(* Whether a group would cross a rule printed between its members reads the
   rule's selector branches, which the graph holds for every rule, and the
   group's own, read once per scan. So walking past a rule reads no selector:
   the group of [.a] and [.b] below walks past 100 rules between them, and costs
   under a hundred words for each over the same rules printed after both. *)
let a_scan_reads_no_selector_again () =
  let n = 100 in
  let fillers =
    String.concat ""
      (List.init n (fun i -> Fmt.str ".f%d .g%d>a:hover{top:%dpx}" i i i))
  in
  let first = ".a{color:red;margin:0}" and last = ".b{color:red;margin:0}" in
  let cost css =
    let g = Rule_graph.of_rules (rules css) in
    measure (fun () ->
        Rule_candidate.refresh
          (Rule_candidate.start ~ctx:Ctx.fragment ~finalize:Fun.id g))
  in
  let across = cost (first ^ fillers ^ last) in
  let beside = cost (first ^ last ^ fillers) in
  Alcotest.(check bool)
    (Fmt.str "across %.0f words, beside %.0f" across beside)
    true
    (across -. beside < float n *. 100.)

let suite =
  ( "rule_candidate",
    [
      Alcotest.test_case "default factoring uses first value with overrides"
        `Quick default_factoring_uses_first_value_with_overrides;
      Alcotest.test_case "large normal mode keeps local exact factoring" `Quick
        large_normal_mode_keeps_local_exact_factoring;
      Alcotest.test_case "large normal mode keeps local default factoring"
        `Quick large_normal_mode_keeps_local_default_factoring;
      Alcotest.test_case "exact shared declarations prune long selector" `Quick
        exact_shared_declarations_prune_long_selector;
      Alcotest.test_case
        "same selector candidate orders commuting blocks canonically" `Quick
        same_selector_candidate_orders_commuting_blocks_canonically;
      Alcotest.test_case "selector-list extension keeps strict alternative"
        `Quick selector_list_extension_keeps_strict_alternative;
      Alcotest.test_case "exact factoring keeps later shorthand overlap" `Quick
        exact_factoring_keeps_later_shorthand_overlap;
      Alcotest.test_case "exact factoring keeps prior member leftover overlap"
        `Quick exact_factoring_keeps_prior_member_leftover_overlap;
      Alcotest.test_case "default factoring keeps prior member leftover overlap"
        `Quick default_factoring_keeps_prior_member_leftover_overlap;
      Alcotest.test_case "nested merge reads a slot as a multiset" `Quick
        nested_merge_reads_a_slot_as_a_multiset;
      Alcotest.test_case "nested-merge safety reads each block once" `Quick
        nested_merge_safety_reads_each_block_once;
      Alcotest.test_case "default factoring lookup is subcubic" `Quick
        default_factoring_lookup_is_subcubic;
      Alcotest.test_case "external-conflict scan is subquadratic" `Quick
        external_conflict_scan_is_subquadratic;
      Alcotest.test_case "default factoring rejects a doomed group early" `Quick
        default_factoring_rejects_a_doomed_group_early;
      Alcotest.test_case "a refresh leaves what a full enumeration offers"
        `Quick a_refresh_leaves_what_a_full_enumeration_offers;
      Alcotest.test_case "a rewritten graph offers what a fresh one does" `Quick
        a_rewritten_graph_offers_what_a_fresh_one_does;
      Alcotest.test_case "a refresh costs what the rewrite changed" `Quick
        a_refresh_costs_what_the_rewrite_changed;
      Alcotest.test_case "a scan reads no selector again" `Quick
        a_scan_reads_no_selector_again;
    ] )
