type scope = [ `Fragment | `Stylesheet ]

type t = {
  scope : scope;
  registered : string -> bool;
  lossless : bool;
  aggressive : bool;
}

let fragment =
  {
    scope = `Fragment;
    registered = (fun _ -> false);
    lossless = false;
    aggressive = false;
  }

let of_scope ?(lossless = false) ?(aggressive = false) = function
  | Some scope -> { fragment with scope; lossless; aggressive }
  | None -> { fragment with lossless; aggressive }

let v ?(lossless = false) ?(aggressive = false) ?(registered = fun _ -> false)
    scope =
  { scope; registered; lossless; aggressive }

let scope t = t.scope
let registered t = t.registered
let lossless t = t.lossless
let aggressive t = t.aggressive

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
  Pp.string ctx "}"
