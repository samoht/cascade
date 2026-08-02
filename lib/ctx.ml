type scope = [ `Fragment | `Stylesheet ]
type objective = [ `Raw | `Transfer ]

type t = {
  scope : scope;
  registered : string -> bool;
  lossless : bool;
  aggressive : bool;
  regroup : bool;
      (** Whether rules may be regrouped: shared declarations factored into a
          selector list, nesting synthesised from adjacent rules. Both depend on
          how the input happened to order its rules, so a canonical projection
          turns them off to stay confluent. *)
  extend_lists : bool;
  closed_world : bool;
  objective : objective;
  enforce_spec : bool;
      (** Drop the evergreen-browser target. Off by default: cascade may strip a
          vendor-prefixed declaration whose unprefixed twin modern browsers
          support. On: keep every prefix (spec-literal, maximal compatibility).
      *)
  stats : Stats.t;
      (** Profiling recorder for the run this context belongs to. Carried here
          because every pass that counts anything already takes a context, and a
          context is built per run, so two runs cannot share a recorder. *)
}

let fragment =
  {
    scope = `Fragment;
    registered = (fun _ -> false);
    lossless = false;
    aggressive = false;
    regroup = true;
    extend_lists = false;
    closed_world = false;
    objective = `Transfer;
    enforce_spec = false;
    stats = Stats.v ();
  }

let of_scope ?(lossless = false) ?(aggressive = false) ?(regroup = true)
    ?(extend_lists = false) ?(closed_world = false) ?(objective = `Transfer)
    ?(enforce_spec = false) ?stats scope =
  let stats = match stats with Some stats -> stats | None -> Stats.v () in
  let scope = match scope with Some scope -> scope | None -> fragment.scope in
  {
    fragment with
    scope;
    lossless;
    aggressive;
    regroup;
    extend_lists;
    closed_world;
    objective;
    enforce_spec;
    stats;
  }

let v ?(lossless = false) ?(aggressive = false) ?(regroup = true)
    ?(extend_lists = false) ?(closed_world = false) ?(objective = `Transfer)
    ?(enforce_spec = false) ?(registered = fun _ -> false) ?stats scope =
  let stats = match stats with Some stats -> stats | None -> Stats.v () in
  {
    scope;
    registered;
    lossless;
    aggressive;
    regroup;
    extend_lists;
    closed_world;
    objective;
    enforce_spec;
    stats;
  }

let scope t = t.scope
let registered t = t.registered
let lossless t = t.lossless
let aggressive t = t.aggressive
let regroup t = t.regroup
let extend_lists t = t.extend_lists
let closed_world t = t.closed_world
let objective t = t.objective
let enforce_spec t = t.enforce_spec
let stats t = t.stats
let with_extend_lists extend_lists t = { t with extend_lists }

let pp_scope ctx = function
  | `Fragment -> Pp.string ctx "fragment"
  | `Stylesheet -> Pp.string ctx "stylesheet"

let pp ctx t =
  Pp.string ctx "{scope=";
  pp_scope ctx t.scope;
  Pp.string ctx ";lossless=";
  Pp.string ctx (string_of_bool t.lossless);
  Pp.string ctx ";aggressive=";
  Pp.string ctx (string_of_bool t.aggressive);
  Pp.string ctx ";extend_lists=";
  Pp.string ctx (string_of_bool t.extend_lists);
  Pp.string ctx ";closed_world=";
  Pp.string ctx (string_of_bool t.closed_world);
  Pp.string ctx ";objective=";
  Pp.string ctx
    (match t.objective with `Raw -> "raw" | `Transfer -> "transfer");
  Pp.string ctx ";enforce_spec=";
  Pp.string ctx (string_of_bool t.enforce_spec);
  Pp.string ctx "}"
