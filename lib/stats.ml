type iteration_stat = {
  fixpoint : int;
  iteration : int;
  local_iteration : int;
  before_rules : int;
  after_rules : int;
  before_bytes : int;
  after_bytes : int;
  bytes_saved : int;
  active_passes : int;
  changed_passes : int;
  elapsed : float;
}

type counters = {
  iterations : int;
  factor_fixpoints_run : int;
  factor_fixpoints_skipped : int;
  factor_preflight_gain : int;
  factor_bytes_saved : int;
  factor_transfer_reverts : int;
}

type snapshot = { counters : counters; iteration_stats : iteration_stat list }

type t = {
  profile : bool;
  mutable counters : counters;
  mutable iteration_stats_rev : iteration_stat list;
  mutable saving : int;
}

let no_counters =
  {
    iterations = 0;
    factor_fixpoints_run = 0;
    factor_fixpoints_skipped = 0;
    factor_preflight_gain = 0;
    factor_bytes_saved = 0;
    factor_transfer_reverts = 0;
  }

let v ?(profile = false) () =
  { profile; counters = no_counters; iteration_stats_rev = []; saving = 0 }

let profile t = t.profile

let snapshot t =
  { counters = t.counters; iteration_stats = t.iteration_stats_rev }

let add_saving t saving = if saving > 0 then t.saving <- t.saving + saving
let reset_saving t = t.saving <- 0
let saving t = t.saving

let start_fixpoint t =
  let run = t.counters.factor_fixpoints_run + 1 in
  t.counters <- { t.counters with factor_fixpoints_run = run };
  run

let skip_fixpoint t =
  t.counters <-
    {
      t.counters with
      factor_fixpoints_skipped = t.counters.factor_fixpoints_skipped + 1;
    }

let revert_fixpoint t =
  t.counters <-
    {
      t.counters with
      factor_transfer_reverts = t.counters.factor_transfer_reverts + 1;
    }

let add_preflight_gain t gain =
  t.counters <-
    {
      t.counters with
      factor_preflight_gain = t.counters.factor_preflight_gain + gain;
    }

let record_iteration t ~fixpoint ~local_iteration ~before_rules ~before_bytes
    ~after_rules ~after_bytes ~bytes_saved ~active_passes ~changed_passes
    ~elapsed =
  let iteration = t.counters.iterations + 1 in
  t.counters <-
    {
      t.counters with
      iterations = iteration;
      factor_bytes_saved = t.counters.factor_bytes_saved + bytes_saved;
    };
  t.iteration_stats_rev <-
    {
      fixpoint;
      iteration;
      local_iteration;
      before_rules;
      after_rules;
      before_bytes;
      after_bytes;
      bytes_saved;
      active_passes;
      changed_passes;
      elapsed;
    }
    :: t.iteration_stats_rev
