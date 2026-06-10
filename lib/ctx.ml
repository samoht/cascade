type scope = [ `Fragment | `Stylesheet ]
type t = { scope : scope; registered : string -> bool; lossless : bool }

let fragment =
  { scope = `Fragment; registered = (fun _ -> false); lossless = false }

let of_scope ?(lossless = false) = function
  | Some scope -> { fragment with scope; lossless }
  | None -> { fragment with lossless }

let v ?(lossless = false) ?(registered = fun _ -> false) scope =
  { scope; registered; lossless }

let scope t = t.scope
let registered t = t.registered
let lossless t = t.lossless

let pp_scope ctx = function
  | `Fragment -> Pp.string ctx "fragment"
  | `Stylesheet -> Pp.string ctx "stylesheet"

let pp ctx t =
  Pp.string ctx "{scope=";
  pp_scope ctx t.scope;
  Pp.string ctx ";lossless=";
  Pp.string ctx (string_of_bool t.lossless);
  Pp.string ctx "}"
