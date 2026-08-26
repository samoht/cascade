(* Keyed on the property identity, ordered by its typed tag: the set is probed
   once per declaration of every rule, so each [mem] and [add] would otherwise
   spend O(log n) [caml_compare] calls walking a GADT-derived key. *)
module Props = Set.Make (struct
  type t = Declaration.prop_key

  let compare = Declaration.compare_prop_key
end)

(* Keyed on the selector itself, so it carries the same requirement as the
   shadowing table next door: the stdlib hash reads a fixed count of nodes, and
   rules written under one deep prefix would all land in one bucket. *)
module Selector_tbl = Hashtbl.Make (struct
  type t = Selector.t

  let equal = Selector.equal
  let hash = Selector.hash
end)

(* The normal and the important half of what a selector writes later. A rule
   asks about one selector and every declaration it carries, so the entry is
   fetched once and stored once rather than probed per declaration. *)
type written = Props.t * Props.t
type t = written Selector_tbl.t

let v () = Selector_tbl.create 256
let empty = (Props.empty, Props.empty)

let written t selector =
  Option.value ~default:empty (Selector_tbl.find_opt t selector)

let covered (normal, important) decl =
  let prop = Declaration.property_key decl in
  if Declaration.is_important decl then Props.mem prop important
  else Props.mem prop normal || Props.mem prop important

let add (normal, important) decls =
  let rec fold normal important = function
    | [] -> (normal, important)
    | decl :: rest ->
        let prop = Declaration.property_key decl in
        if Declaration.is_important decl then
          fold normal (Props.add prop important) rest
        else fold (Props.add prop normal) important rest
  in
  fold normal important decls

let record t selector written decls =
  (* The two halves stay apart until the store, so a rule builds one entry
     rather than one per declaration. *)
  Selector_tbl.replace t selector (add written decls)
