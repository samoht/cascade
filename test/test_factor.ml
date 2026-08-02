open Cascade

let rules css =
  match Css.of_string css with
  | Ok { stylesheet; _ } ->
      List.filter_map
        (function Stylesheet.Rule r -> Some r | _ -> None)
        (Css.statements stylesheet)
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let render rules =
  String.concat ""
    (List.map (Pp.to_string ~minify:true Stylesheet.pp_rule) rules)

let optimize_str css =
  match Css.of_string css with
  | Ok { stylesheet; _ } ->
      Css.optimize stylesheet |> Css.to_string ~minify:true
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

(* A selector whose box shorthand is split across rules (its base plus a corner
   longhand, as Tailwind emits for [.form-select]) must optimise to the same
   output as the already consolidated form the optimiser itself produces:
   consolidation runs before factoring, so a partial group cannot absorb the
   shared base before the selector's own rules are merged. *)
let test_split_shorthand_confluence () =
  let split =
    ".i{color:red;margin:0;padding:5px}.s{color:red;margin:0;padding:5px}.s{background-repeat:no-repeat;padding-right:8px}.t{color:red;margin:0;padding:5px}"
  in
  let consolidated =
    ".i{color:red;margin:0;padding:5px}.s{color:red;margin:0;padding:5px;background-repeat:no-repeat;padding-right:8px}.t{color:red;margin:0;padding:5px}"
  in
  Alcotest.(check string)
    "split and consolidated shorthands optimise identically"
    (optimize_str consolidated)
    (optimize_str split)

let test_run_reaches_fixpoint_with_finalizer () =
  let optimized =
    Factor.run ~ctx:Ctx.fragment
      ~finalize:(Rule.finalize ~ctx:Ctx.fragment)
      (rules
         ".a{display:flex;margin-top:1px;margin-right:1px;margin-bottom:1px;margin-left:1px}.b{display:flex;margin-top:2px;margin-right:2px;margin-bottom:2px;margin-left:2px}.c{display:flex;margin-top:3px;margin-right:3px;margin-bottom:3px;margin-left:3px}")
  in
  Alcotest.(check string)
    "run factors and finalizes leftovers"
    ".a,.b,.c{display:flex;margin:1px}.b{margin:2px}.c{margin:3px}"
    (render optimized)

let fixpoints_run stats = (Stats.snapshot stats).counters.factor_fixpoints_run

let test_cache_reuses_identical_rule_run () =
  let stats = Stats.v () in
  let ctx = Ctx.v ~stats `Fragment in
  let cache = Factor.cache () in
  let input =
    rules
      ".a{display:flex;margin:1px}.b{display:flex;margin:2px}.c{display:flex;margin:3px}"
  in
  let finalize = Rule.finalize ~ctx in
  let first = Factor.run ~cache ~ctx ~finalize input in
  let fixpoints = fixpoints_run stats in
  ignore (Factor.run ~cache ~ctx ~finalize input);
  Alcotest.(check int)
    "cached run does not start another fixpoint" fixpoints (fixpoints_run stats);
  Alcotest.(check string)
    "first run still optimizes"
    ".a,.b,.c{display:flex;margin:1px}.b{margin:2px}.c{margin:3px}"
    (render first)

let read_example name =
  let candidates = [ "examples/" ^ name; "test/examples/" ^ name ] in
  match List.find_opt Sys.file_exists candidates with
  | Some path ->
      let ic = open_in path in
      Fun.protect
        ~finally:(fun () -> close_in ic)
        (fun () -> really_input_string ic (in_channel_length ic))
  | None -> Alcotest.failf "fixture %s not found (tried examples/)" name

(* Factoring must not depend on unrelated content: two sheets that differ only
   in whether an unrelated pair of rules is written grouped or split must
   optimise identically. The transfer-objective gate scores a segment by its
   estimated compressed size, which a distant grouping shifts by a byte or two;
   reverting on that noise once flipped a far-away, otherwise raw-smaller
   factoring (the prose list-item rules here) depending only on the shadow
   spelling. The fixture holds the shared prose; the two spellings of the
   unrelated [.shadow]/[.shadow-sm] pair are appended here. *)
let test_factoring_stable_under_unrelated_grouping () =
  let prose = read_example "prose_lg_prefix.css" in
  let shadow =
    "{--tw-shadow:0 1px 3px 0 var(--tw-shadow-color,#0000001a),0 1px 2px -1px \
     var(--tw-shadow-color,#0000001a);box-shadow:var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)}"
  in
  let separate = prose ^ ".shadow" ^ shadow ^ ".shadow-sm" ^ shadow ^ "}" in
  let grouped = prose ^ ".shadow,.shadow-sm" ^ shadow ^ "}" in
  Alcotest.(check string)
    "unrelated grouping does not change factoring" (optimize_str separate)
    (optimize_str grouped)

let suite =
  ( "factor",
    [
      Alcotest.test_case "run reaches fixpoint with finalizer" `Quick
        test_run_reaches_fixpoint_with_finalizer;
      Alcotest.test_case "cache reuses identical rule run" `Quick
        test_cache_reuses_identical_rule_run;
      Alcotest.test_case "split shorthand optimises like consolidated" `Quick
        test_split_shorthand_confluence;
      Alcotest.test_case "factoring stable under unrelated grouping" `Quick
        test_factoring_stable_under_unrelated_grouping;
    ] )
