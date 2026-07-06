open Cascade
module S = Stats

let check_zeroed_counters label =
  Alcotest.(check int) (label ^ " iterations") 0 S.counters.iterations;
  Alcotest.(check int)
    (label ^ " factor_fixpoints_run")
    0 S.counters.factor_fixpoints_run;
  Alcotest.(check int) (label ^ " marginal_stops") 0 S.counters.marginal_stops;
  Alcotest.(check int)
    (label ^ " factor_fixpoints_skipped")
    0 S.counters.factor_fixpoints_skipped;
  Alcotest.(check int)
    (label ^ " factor_preflight_gain")
    0 S.counters.factor_preflight_gain;
  Alcotest.(check int)
    (label ^ " factor_bytes_saved")
    0 S.counters.factor_bytes_saved

let test_pass_bucket_reuse_and_reset () =
  S.reset ();
  let first = S.pass "merge" in
  first.time <- 1.25;
  first.calls <- 3;
  first.changes <- 2;
  first.rules_in <- 17;
  first.rules_out <- 11;
  let second = S.pass "merge" in
  Alcotest.(check bool) "same pass bucket reused" true (first == second);
  Alcotest.(check int) "one pass bucket" 1 (Hashtbl.length S.pass_times);
  S.reset ();
  Alcotest.(check int) "pass buckets cleared" 0 (Hashtbl.length S.pass_times);
  let fresh = S.pass "merge" in
  Alcotest.(check bool) "new bucket after reset" false (first == fresh);
  Alcotest.(check (float 0.0001)) "fresh time" 0.0 fresh.time;
  Alcotest.(check int) "fresh calls" 0 fresh.calls;
  Alcotest.(check int) "fresh changes" 0 fresh.changes;
  Alcotest.(check int) "fresh rules_in" 0 fresh.rules_in;
  Alcotest.(check int) "fresh rules_out" 0 fresh.rules_out

let test_profile_flag_and_savings () =
  S.reset ();
  Alcotest.(check bool) "profile starts disabled" false (S.profile ());
  S.set_profile true;
  Alcotest.(check bool) "profile enabled" true (S.profile ());
  S.add_saving 10;
  S.add_saving 0;
  S.add_saving (-5);
  S.add_saving 7;
  Alcotest.(check int) "only positive savings accumulate" 17 (S.saving ());
  S.reset_saving ();
  Alcotest.(check int) "saving reset" 0 (S.saving ());
  S.reset ();
  Alcotest.(check bool) "reset keeps profile flag" true (S.profile ());
  S.set_profile false;
  Alcotest.(check bool) "profile disabled" false (S.profile ())

let test_record_iteration_updates_history_and_counters () =
  S.reset ();
  S.counters.iterations <- 42;
  S.record_iteration ~fixpoint:2 ~local_iteration:5 ~before_rules:17
    ~before_bytes:300 ~after_rules:13 ~after_bytes:240 ~bytes_saved:60
    ~active_passes:4 ~changed_passes:3 ~elapsed:0.125;
  Alcotest.(check int)
    "factor bytes saved counter updated" 60 S.counters.factor_bytes_saved;
  match S.iteration_stats () with
  | [ s ] ->
      Alcotest.(check int) "fixpoint" 2 s.fixpoint;
      Alcotest.(check int) "global iteration snapshot" 42 s.iteration;
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

let test_reset_clears_mutable_state () =
  S.reset ();
  ignore (S.pass "merge");
  S.add_saving 9;
  S.counters.iterations <- 1;
  S.counters.factor_fixpoints_run <- 2;
  S.counters.marginal_stops <- 3;
  S.counters.factor_fixpoints_skipped <- 6;
  S.counters.factor_preflight_gain <- 7;
  S.counters.factor_bytes_saved <- 8;
  S.record_iteration ~fixpoint:1 ~local_iteration:1 ~before_rules:1
    ~before_bytes:1 ~after_rules:1 ~after_bytes:1 ~bytes_saved:1
    ~active_passes:1 ~changed_passes:1 ~elapsed:0.0;
  S.reset ();
  Alcotest.(check int) "saving cleared" 0 (S.saving ());
  Alcotest.(check int) "pass_times cleared" 0 (Hashtbl.length S.pass_times);
  Alcotest.(check int)
    "iteration stats cleared" 0
    (List.length (S.iteration_stats ()));
  check_zeroed_counters "reset"

let suite =
  ( "stats",
    [
      Alcotest.test_case "pass bucket reuse and reset" `Quick
        test_pass_bucket_reuse_and_reset;
      Alcotest.test_case "profile flag and savings" `Quick
        test_profile_flag_and_savings;
      Alcotest.test_case "record iteration updates history and counters" `Quick
        test_record_iteration_updates_history_and_counters;
      Alcotest.test_case "reset clears mutable state" `Quick
        test_reset_clears_mutable_state;
    ] )
