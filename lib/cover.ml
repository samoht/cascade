module Props = Set.Make (struct
  type t = Declaration.prop_key

  let compare = Stdlib.compare
end)

module Selector_tbl = Hashtbl.Make (struct
  type t = Selector.t

  let equal = ( = )
  let hash = Hashtbl.hash
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
