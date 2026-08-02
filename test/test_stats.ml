open Cascade
module S = Stats

let sheet css =
  match Css.of_string css with
  | Ok { stylesheet; _ } -> stylesheet
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let rules css =
  List.filter_map
    (function Stylesheet.Rule r -> Some r | _ -> None)
    (Css.statements (sheet css))

(* Three rules sharing [display:flex]: enough for the preflight to rate the
   factoring worth running, so a run over them reports a fixpoint. *)
let factorable =
  ".a{display:flex;margin:1px}.b{display:flex;margin:2px}.c{display:flex;margin:3px}"

(* Counters reported for one factoring run over [css]. [nest] runs from the
   finalizer the scheduler applies to each produced rule, so whatever it does
   happens in the middle of this run. *)
let run_counters ?(nest = fun () -> ()) css =
  let stats = S.v () in
  let ctx = Ctx.v ~stats `Fragment in
  let finalize r =
    nest ();
    Rule.finalize ~ctx r
  in
  ignore (Factor.run ~ctx ~finalize (rules css));
  (S.snapshot stats).counters

let optimize_counters css =
  let stats = S.v () in
  ignore (Css.optimize ~stats (sheet css));
  (S.snapshot stats).counters

(* An optimization started while another is running has its own work to count.
   Neither run may see the other's numbers, whether they interleave through a
   callback the outer run makes (here) or across threads. *)
let test_nested_run_keeps_its_counts_to_itself () =
  let alone = (run_counters factorable).factor_fixpoints_run in
  let fired = ref false in
  let nest () =
    if not !fired then begin
      fired := true;
      ignore (Css.optimize (sheet factorable))
    end
  in
  let nested = (run_counters ~nest factorable).factor_fixpoints_run in
  Alcotest.(check int)
    "an optimization nested in the finalizer leaves the outer count alone" alone
    nested

(* What a run hands back describes that run. A later optimization - the
   caller's, or one started by a library the caller passed the report to -
   cannot rewrite it. *)
let test_report_survives_a_later_run () =
  let report = optimize_counters factorable in
  let fixpoints = report.factor_fixpoints_run in
  ignore (optimize_counters ".a{color:red}");
  Alcotest.(check int)
    "the first run's report still describes the first run" fixpoints
    report.factor_fixpoints_run

(* A recorder starts empty, and every optimizer run gets a fresh one. *)
let test_fresh_recorder_is_empty () =
  let report = S.snapshot (S.v ()) in
  Alcotest.(check int) "iterations" 0 report.counters.iterations;
  Alcotest.(check int)
    "factor_fixpoints_run" 0 report.counters.factor_fixpoints_run;
  Alcotest.(check int)
    "factor_fixpoints_skipped" 0 report.counters.factor_fixpoints_skipped;
  Alcotest.(check int)
    "factor_preflight_gain" 0 report.counters.factor_preflight_gain;
  Alcotest.(check int) "factor_bytes_saved" 0 report.counters.factor_bytes_saved;
  Alcotest.(check int)
    "factor_transfer_reverts" 0 report.counters.factor_transfer_reverts;
  Alcotest.(check int) "iteration stats" 0 (List.length report.iteration_stats);
  Alcotest.(check bool) "profile off by default" false (S.profile (S.v ()));
  Alcotest.(check bool)
    "profile on when asked" true
    (S.profile (S.v ~profile:true ()))

(* Savings are the running total for one fixpoint iteration, so they only count
   up and start again at each iteration. *)
let test_savings_accumulate_per_iteration () =
  let stats = S.v () in
  S.add_saving stats 10;
  S.add_saving stats 0;
  S.add_saving stats (-5);
  S.add_saving stats 7;
  Alcotest.(check int) "only positive savings accumulate" 17 (S.saving stats);
  S.reset_saving stats;
  Alcotest.(check int) "saving reset" 0 (S.saving stats)

let test_record_iteration_updates_history_and_counters () =
  let stats = S.v () in
  S.record_iteration stats ~fixpoint:2 ~local_iteration:5 ~before_rules:17
    ~before_bytes:300 ~after_rules:13 ~after_bytes:240 ~bytes_saved:60
    ~active_passes:4 ~changed_passes:3 ~elapsed:0.125;
  let report = S.snapshot stats in
  Alcotest.(check int)
    "factor bytes saved counter updated" 60 report.counters.factor_bytes_saved;
  Alcotest.(check int) "iteration counted" 1 report.counters.iterations;
  match report.iteration_stats with
  | [ s ] ->
      Alcotest.(check int) "fixpoint" 2 s.fixpoint;
      Alcotest.(check int) "global iteration" 1 s.iteration;
      Alcotest.(check int) "local iteration" 5 s.local_iteration;
      Alcotest.(check int) "before rules" 17 s.before_rules;
      Alcotest.(check int) "after rules" 13 s.after_rules;
      Alcotest.(check int) "before bytes" 300 s.before_bytes;
      Alcotest.(check int) "after bytes" 240 s.after_bytes;
      Alcotest.(check int) "bytes saved" 60 s.bytes_saved;
      Alcotest.(check int) "active passes" 4 s.active_passes;
      Alcotest.(check int) "changed passes" 3 s.changed_passes;
      Alcotest.(check (float 0.0001)) "elapsed" 0.125 s.elapsed
  | stats ->
      Alcotest.failf "expected one iteration stat, got %d" (List.length stats)

let test_fixpoints_are_numbered_within_the_run () =
  let stats = S.v () in
  Alcotest.(check int) "first fixpoint" 1 (S.start_fixpoint stats);
  Alcotest.(check int) "second fixpoint" 2 (S.start_fixpoint stats);
  S.skip_fixpoint stats;
  S.revert_fixpoint stats;
  S.add_preflight_gain stats 40;
  S.add_preflight_gain stats 2;
  let report = S.snapshot stats in
  Alcotest.(check int) "fixpoints run" 2 report.counters.factor_fixpoints_run;
  Alcotest.(check int)
    "fixpoints skipped" 1 report.counters.factor_fixpoints_skipped;
  Alcotest.(check int)
    "transfer reverts" 1 report.counters.factor_transfer_reverts;
  Alcotest.(check int) "preflight gain" 42 report.counters.factor_preflight_gain;
  Alcotest.(check int)
    "a fresh recorder numbers from one again" 1
    (S.start_fixpoint (S.v ()))

let suite =
  ( "stats",
    [
      Alcotest.test_case "nested run keeps its counts to itself" `Quick
        test_nested_run_keeps_its_counts_to_itself;
      Alcotest.test_case "report survives a later run" `Quick
        test_report_survives_a_later_run;
      Alcotest.test_case "fresh recorder is empty" `Quick
        test_fresh_recorder_is_empty;
      Alcotest.test_case "savings accumulate per iteration" `Quick
        test_savings_accumulate_per_iteration;
      Alcotest.test_case "record iteration updates history and counters" `Quick
        test_record_iteration_updates_history_and_counters;
      Alcotest.test_case "fixpoints are numbered within the run" `Quick
        test_fixpoints_are_numbered_within_the_run;
    ] )
