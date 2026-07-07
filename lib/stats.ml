type pass_stat = {
  mutable time : float;
  mutable calls : int;
  mutable changes : int;
  mutable rules_in : int;
  mutable rules_out : int;
}

let pass_times : (string, pass_stat) Hashtbl.t = Hashtbl.create 16
let profile_enabled = ref false
let set_profile enabled = profile_enabled := enabled
let profile () = !profile_enabled
let factor_saving = ref 0

let add_saving saving =
  if saving > 0 then factor_saving := !factor_saving + saving

let reset_saving () = factor_saving := 0
let saving () = !factor_saving

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

let iteration_stats_rev = ref []
let iteration_stats () = !iteration_stats_rev

let pass name =
  match Hashtbl.find_opt pass_times name with
  | Some s -> s
  | None ->
      let s =
        { time = 0.0; calls = 0; changes = 0; rules_in = 0; rules_out = 0 }
      in
      Hashtbl.add pass_times name s;
      s

type counters = {
  mutable iterations : int;
  mutable factor_fixpoints_run : int;
  mutable marginal_stops : int;
  mutable factor_fixpoints_skipped : int;
  mutable factor_preflight_gain : int;
  mutable factor_bytes_saved : int;
  mutable factor_transfer_reverts : int;
}

let counters =
  {
    iterations = 0;
    factor_fixpoints_run = 0;
    marginal_stops = 0;
    factor_fixpoints_skipped = 0;
    factor_preflight_gain = 0;
    factor_bytes_saved = 0;
    factor_transfer_reverts = 0;
  }

let record_iteration ~fixpoint ~local_iteration ~before_rules ~before_bytes
    ~after_rules ~after_bytes ~bytes_saved ~active_passes ~changed_passes
    ~elapsed =
  counters.factor_bytes_saved <- counters.factor_bytes_saved + bytes_saved;
  iteration_stats_rev :=
    {
      fixpoint;
      iteration = counters.iterations;
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
    :: !iteration_stats_rev

let reset () =
  Hashtbl.reset pass_times;
  iteration_stats_rev := [];
  factor_saving := 0;
  counters.iterations <- 0;
  counters.factor_fixpoints_run <- 0;
  counters.marginal_stops <- 0;
  counters.factor_fixpoints_skipped <- 0;
  counters.factor_preflight_gain <- 0;
  counters.factor_bytes_saved <- 0;
  counters.factor_transfer_reverts <- 0
