(* Keyed on the property identity, ordered by its typed tag: the map is probed
   once per declaration of every rule, so each [find] and [add] would otherwise
   spend O(log n) [caml_compare] calls walking a GADT-derived key. *)
module Props = Map.Make (struct
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

(* The normal and the important half of what a selector writes later, each the
   later writes of a property. A rule asks about one selector and every
   declaration it carries, so the entry is fetched once and stored once rather
   than probed per declaration. *)
type written = Declaration.t list Props.t * Declaration.t list Props.t
type t = written Selector_tbl.t

let v () = Selector_tbl.create 256
let empty = (Props.empty, Props.empty)

let written t selector =
  Option.value ~default:empty (Selector_tbl.find_opt t selector)

(* A later write shadows [decl] only where every browser at or above the floors
   that reads [decl] reads it too; otherwise [decl] is the fallback a floor
   paints. A later [revert-rule] rolls back to what the earlier rules write (CSS
   Cascade 5 sec. 7.3.6), so it shadows nothing. The question is asked only of a
   property written again, so a run that shadows nothing never reads a value. *)
let covered ~floors (normal, important) decl =
  let prop = Declaration.property_key decl in
  let read_later half =
    match Props.find_opt prop half with
    | None -> false
    | Some laters ->
        List.exists
          (fun later ->
            (not (Declaration.rolls_back_rule later))
            && not (Declaration.fallback_for ~floors ~later decl))
          laters
  in
  if Declaration.is_important decl then read_later important
  else read_later normal || read_later important

let add (normal, important) decls =
  let push prop decl half =
    Props.update prop
      (function None -> Some [ decl ] | Some laters -> Some (decl :: laters))
      half
  in
  let rec fold normal important = function
    | [] -> (normal, important)
    | decl :: rest ->
        let prop = Declaration.property_key decl in
        if Declaration.is_important decl then
          fold normal (push prop decl important) rest
        else fold (push prop decl normal) important rest
  in
  fold normal important decls

let record t selector written decls =
  (* The two halves stay apart until the store, so a rule builds one entry
     rather than one per declaration. *)
  Selector_tbl.replace t selector (add written decls)
