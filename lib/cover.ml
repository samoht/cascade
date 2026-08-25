module Props = Set.Make (struct
  type t = Declaration.prop_key

  let compare = Stdlib.compare
end)

(* Keyed on the selector itself, so it carries the same requirement as the
   shadowing table next door: the stdlib hash reads a fixed count of nodes, and
   rules written under one deep prefix would all land in one bucket. *)
module Selector_tbl = Hashtbl.Make (struct
  type t = Selector.t

  let equal = Selector.equal
  let hash = Selector.hash
end)

type entry = Props.t * Props.t
type t = entry Selector_tbl.t

let v () = Selector_tbl.create 256
let empty = (Props.empty, Props.empty)

let get t selector =
  Option.value ~default:empty (Selector_tbl.find_opt t selector)

let covered t selector decl =
  let normal, important = get t selector in
  let prop = Declaration.property_key decl in
  if Declaration.is_important decl then Props.mem prop important
  else Props.mem prop normal || Props.mem prop important

let add t selector decl =
  let normal, important = get t selector in
  let prop = Declaration.property_key decl in
  let cover =
    if Declaration.is_important decl then (normal, Props.add prop important)
    else (Props.add prop normal, important)
  in
  Selector_tbl.replace t selector cover
