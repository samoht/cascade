type scope = [ `Fragment | `Stylesheet ]

type t = {
  scope : scope;
  registered : string -> bool;
  lossless : bool;
  aggressive : bool;
  extend_lists : bool;
  closed_world : bool;
}

let fragment =
  {
    scope = `Fragment;
    registered = (fun _ -> false);
    lossless = false;
    aggressive = false;
    extend_lists = false;
    closed_world = false;
  }

let of_scope ?(lossless = false) ?(aggressive = false) ?(extend_lists = false)
    ?(closed_world = false) = function
  | Some scope ->
      { fragment with scope; lossless; aggressive; extend_lists; closed_world }
  | None -> { fragment with lossless; aggressive; extend_lists; closed_world }

let v ?(lossless = false) ?(aggressive = false) ?(extend_lists = false)
    ?(closed_world = false) ?(registered = fun _ -> false) scope =
  { scope; registered; lossless; aggressive; extend_lists; closed_world }

let scope t = t.scope
let registered t = t.registered
let lossless t = t.lossless
let aggressive t = t.aggressive
let extend_lists t = t.extend_lists
let closed_world t = t.closed_world
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
  Pp.string ctx "}"
