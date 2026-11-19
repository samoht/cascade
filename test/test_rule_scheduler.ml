open Cascade

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

let optimize_rules css =
  rules css |> Rule_graph.of_rules
  |> Rule_scheduler.run ~ctx:Ctx.fragment ~finalize:Fun.id
  |> fun { Rule_scheduler.graph = g; _ } ->
  Array.to_list
    (Array.map (Rule_graph.node_rule g) (Rule_graph.canonical_order g))
  |> render

(* Cascade-equivalence oracle for the safety tests: resolve every element the
   class alphabet can describe against the input and against the optimized
   output, and require an identical computed style. This judges soundness from
   the cascade itself, independent of how (or whether) a rule fired. *)
let parse css =
  match Css.of_string ~strict:false css with
  | Ok { stylesheet; _ } -> stylesheet
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

type node = { classes : string list }

module Node = struct
  type t = node

  let equal = ( = )
  let name _ = None
  let id _ = None
  let classes t = t.classes
  let attribute _ _ = None
  let parent _ = None
  let children _ = []
  let text_children _ = []
end

module R = Resolve.Make (Node)

let rec subsets = function
  | [] -> [ [] ]
  | x :: xs ->
      let rest = subsets xs in
      rest @ List.map (fun tail -> x :: tail) rest

let canon d = Pp.to_string ~minify:true Declaration.pp d

let computed sheet classes =
  R.resolve sheet { classes } |> List.map canon |> List.sort String.compare

let assert_cascade_preserved ~universe css =
  let before = parse css and after = parse (optimize_rules css) in
  List.iter
    (fun classes ->
      Alcotest.(check (list string))
        ("computed style for ." ^ String.concat "." classes)
        (computed before classes) (computed after classes))
    (subsets universe)

(* --- one positive case per rule kind --- *)

let merges_identical_body () =
  Alcotest.(check string)
    "same-body rules group under a combined selector list"
    ".a,.b{color:red;margin:0}"
    (optimize_rules ".a{color:red;margin:0}.b{color:red;margin:0}")

let merges_same_selector_canonically () =
  Alcotest.(check string)
    "same-selector rules concatenate, commuting blocks ordered canonically"
    ".x{color:red;margin:0}"
    (optimize_rules ".x{margin:0}.x{color:red}")

let inlines_long_selector_branch () =
  (* The [.aaaaaaaa] branch costs more than inlining [left:0] into its own rule,
     so the branch is pulled out and the residual group keeps only [.b]. *)
  Alcotest.(check string)
    "long selector branch is inlined into its receiver"
    ".aaaaaaaa{top:0;left:0}.b{left:0}"
    (optimize_rules ".aaaaaaaa{top:0}.b,.aaaaaaaa{left:0}")

(* --- each kind must refuse a merge that would reorder a real conflict --- *)

let refuses_identical_body_across_conflict () =
  (* [.a] and [.c] share a body but a conflicting [.b] sits between them;
     merging would reorder the cascade for [.a.b] / [.b.c]. *)
  assert_cascade_preserved ~universe:[ "a"; "b"; "c" ]
    ".a{color:red}.b{color:green}.c{color:red}"

let refuses_same_selector_across_conflict () =
  (* Two [.x] rules cannot fuse across a [.y] that ties them on [color]. *)
  assert_cascade_preserved ~universe:[ "x"; "y" ]
    ".x{color:red}.y{color:green}.x{color:blue}"

let refuses_default_factoring_across_override () =
  (* The default-value group would hoist [z-index:1] (the first value) and leave
     [.c] re-asserting the default, but [.b] overrides it in between on
     overlapping selectors, so the hoist is unsafe and must not happen. *)
  assert_cascade_preserved ~universe:[ "a"; "b"; "c" ]
    ".a{z-index:1}.b{z-index:2}.c{z-index:1}"

let factors_shared_declarations () =
  Alcotest.(check string)
    "shared exact declaration is factored"
    ".a,.b{display:flex;color:red}.b{color:blue}"
    (optimize_rules ".a{display:flex;color:red}.b{display:flex;color:blue}")

let factors_default_value_group () =
  Alcotest.(check string)
    "common property uses first cascade value and leaves overrides"
    ".a,.b,.c{display:flex;margin:1px}.b{margin:2px}.c{margin:3px}"
    (optimize_rules
       ".a{display:flex;margin:1px}.b{display:flex;margin:2px}.c{display:flex;margin:3px}")

(* Neighbours sharing a declaration and writing one property with differing
   values, so both factoring kinds commit all along the run. *)
let factoring_run n =
  List.init n (fun i ->
      Fmt.str ".c%d{--p%d:%d;--q%d:1;color:rgb(%d,0,0)}" i (i / 4) (i mod 3)
        (i / 5) i)
  |> String.concat "" |> rules |> Rule_graph.of_rules

(* A settled run is a fixed point of its search: every candidate of the graph it
   returns, under the bounds the run started with, is refused there, so running
   the scheduler again on that graph commits nothing. Both runs stay on one side
   of the 128 nodes that set those bounds. *)
let a_settled_run_is_a_fixed_point () =
  let ctx = Ctx.fragment and finalize = Fun.id in
  List.iter
    (fun (label, graph) ->
      let nodes = Rule_graph.node_count graph in
      let { Rule_scheduler.graph = settled_graph; settled; _ } =
        Rule_scheduler.run ~ctx ~finalize graph
      in
      Alcotest.(check bool) (label ^ ": settled") true settled;
      Alcotest.(check bool)
        (label ^ ": it rewrote") true
        (Rule_graph.generation settled_graph > 0);
      Alcotest.(check bool)
        (label ^ ": on the bounds' side it started on")
        true
        (nodes <= 128 = (Rule_graph.node_count settled_graph <= 128));
      Alcotest.(check (list int))
        (label ^ ": candidates the settled graph accepts")
        []
        (Rule_candidate.enumerate ~nodes ~ctx ~finalize settled_graph
        |> List.filter_map (fun (c : Rule_rewrite.candidate) ->
            match
              Rule_graph.rewrite settled_graph ~consume:c.consume
                ~produce:c.produce
            with
            | Ok _ -> Some c.saving
            | Error _ -> None));
      let again = Rule_scheduler.run ~ctx ~finalize settled_graph in
      Alcotest.(check bool) (label ^ ": settled again") true again.settled;
      Alcotest.(check int)
        (label ^ ": running again commits nothing")
        (Rule_graph.generation settled_graph)
        (Rule_graph.generation again.graph))
    [ ("small run", factoring_run 40); ("large run", factoring_run 200) ]

(* Rules the run printed, in that order. *)
let printed graph =
  Array.to_list
    (Array.map (Rule_graph.node_rule graph) (Rule_graph.canonical_order graph))

(* [.x] and [.y] share [color:red], 141 source positions apart, with pairs of
   identical bodies between them. A search reads the members of a bucket in the
   order they print, wherever that is, so the run's own graph searches the two
   together, as a graph built afresh from what it prints does. *)
let spanned_run =
  let pairs =
    List.init 70 (fun i -> Fmt.str ".a%d{width:%dpx}.b%d{width:%dpx}" i i i i)
  in
  String.concat ""
    ((".x{color:red;margin:0}" :: pairs) @ [ ".y{color:red;padding:0}" ])
  |> rules |> Rule_graph.of_rules

(* A run that says [afresh] is a fixed point of a graph built afresh from what
   it prints: a run over that graph commits nothing. One that does not say so is
   one where that run commits. *)
let afresh_is_a_fresh_fixed_point () =
  let ctx = Ctx.fragment and finalize = Fun.id in
  List.iter
    (fun (label, graph, expected) ->
      let { Rule_scheduler.graph = settled_graph; settled; afresh } =
        Rule_scheduler.run ~ctx ~finalize graph
      in
      Alcotest.(check bool) (label ^ ": settled") true settled;
      Alcotest.(check bool) (label ^ ": afresh") expected afresh;
      let again =
        Rule_scheduler.run ~ctx ~finalize
          (Rule_graph.of_rules (printed settled_graph))
      in
      Alcotest.(check bool)
        (label ^ ": a run afresh commits")
        (not afresh)
        (Rule_graph.generation again.graph > 0);
      if not afresh then
        Alcotest.(check bool)
          (label ^ ": the run afresh says afresh")
          true again.afresh)
    [
      ("small run", factoring_run 40, true);
      ("large run", factoring_run 200, true);
      ("spanned run", spanned_run, true);
    ]

(* A rule a rewrite consumed stays in the graph as a relay. Here [p.k] and
   [span.m] can match no element together, and only [.x], which wrote [color]
   for both, stood between them; once it is consumed nothing live orders them.
   Merging the identical bodies of [p.c] and [span.d] then has to print ahead of
   [p.k] and after [span.m], as the source does: through the relay that closes a
   loop, but a graph built afresh from the rules the graph prints has none, and
   the run merges them there. *)
let merges_past_a_relay_cycle () =
  let graph =
    rules
      "p.c{color:green}p.k{color:red}p.x,span.x{color:#000}span.m{color:blue}span.d{color:green}"
    |> Rule_graph.of_rules
  in
  match
    Rule_graph.try_rewrite graph
      ~consume:[ Rule_graph.Node_id.of_int_exn 2 ]
      ~produce:(rules "p.x,span.x{margin:0}")
  with
  | None -> Alcotest.fail "a disjoint replacement commits"
  | Some graph ->
      let { Rule_scheduler.graph; _ } =
        Rule_scheduler.run ~ctx:Ctx.fragment ~finalize:Fun.id graph
      in
      let texts =
        List.map (Pp.to_string ~minify:true Stylesheet.pp_rule) (printed graph)
      in
      let has s text =
        let n = String.length s in
        let rec at i =
          i + n <= String.length text
          && (String.equal (String.sub text i n) s || at (i + 1))
        in
        at 0
      in
      let index s =
        let rec find i = function
          | [] ->
              Alcotest.failf "no printed rule holds %s in %s" s
                (render (printed graph))
          | text :: rest -> if has s text then i else find (i + 1) rest
        in
        find 0 texts
      in
      Alcotest.(check int)
        "the two bodies print once" 1
        (List.length (List.filter (has "color:green") texts));
      Alcotest.(check bool)
        "the merge prints after span.m" true
        (index "span.m" < index "color:green");
      Alcotest.(check bool)
        "and before p.k" true
        (index "color:green" < index "p.k")

let suite =
  ( "rule_scheduler",
    [
      Alcotest.test_case "merges past a cycle through a relay" `Quick
        merges_past_a_relay_cycle;
      Alcotest.test_case "a settled run is a fixed point" `Quick
        a_settled_run_is_a_fixed_point;
      Alcotest.test_case "a run afresh is a fixed point of a fresh graph" `Quick
        afresh_is_a_fresh_fixed_point;
      Alcotest.test_case "merges identical bodies" `Quick merges_identical_body;
      Alcotest.test_case "merges same selector canonically" `Quick
        merges_same_selector_canonically;
      Alcotest.test_case "inlines a long selector branch" `Quick
        inlines_long_selector_branch;
      Alcotest.test_case "refuses identical-body merge across a conflict" `Quick
        refuses_identical_body_across_conflict;
      Alcotest.test_case "refuses same-selector merge across a conflict" `Quick
        refuses_same_selector_across_conflict;
      Alcotest.test_case "refuses default factoring across an override" `Quick
        refuses_default_factoring_across_override;
      Alcotest.test_case "factors shared declarations" `Quick
        factors_shared_declarations;
      Alcotest.test_case "factors default-value group" `Quick
        factors_default_value_group;
    ] )
