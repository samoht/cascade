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
      (Rule_candidate.nested_merge_is_safe (rules css))
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

   The comparison is between the two runs, never against a wall-clock budget:
   [Sys.time] is this process's own CPU, alcotest runs its cases one after
   another, and the two measurements therefore sit under the same load. *)
let nested_merge_safety_reads_each_block_once () =
  let n = 2000 and reps = 100 in
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
  Alcotest.(check bool)
    "neither run writes a slot twice, so both merge" true
    (Rule_candidate.nested_merge_is_safe spread
    && Rule_candidate.nested_merge_is_safe front);
  let cost rules =
    Gc.full_major ();
    let t0 = Sys.time () in
    for _ = 1 to reps do
      ignore (Sys.opaque_identity (Rule_candidate.nested_merge_is_safe rules))
    done;
    Sys.time () -. t0
  in
  let a = cost spread in
  let b = cost front in
  Alcotest.(check bool)
    (Fmt.str
       "%d nested blocks over %d rules costs %.3fs, all on one %.3fs (%.1fx)" n
       n a b (a /. b))
    true
    (b = 0. || a < b *. 3.)

(* --- allocation / complexity guards --- *)

let measure f =
  Gc.full_major ();
  let w0 = Gc.minor_words () in
  let r = f () in
  ignore (Sys.opaque_identity r);
  Gc.minor_words () -. w0

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
    ] )
