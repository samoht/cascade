open Declaration
open Common

let preserve_list = List.preserve
let scope = Ctx.scope

(* Walk the rule for a family whose shorthand always absorbs exactly three
   longhands, absorbing every run [try_compose] accepts. *)
let compose_fixed3_via_index idx ~try_compose =
  let n = Rule_index.length idx in
  let i = ref 0 in
  while !i + 2 < n do
    match try_compose idx !i with
    | None -> incr i
    | Some shorthand ->
        if
          Rule_index.absorb idx ~at:!i
            ~absorbed:[ !i; !i + 1; !i + 2 ]
            ~shorthand
        then i := !i + 3
        else incr i
  done

(* Same walk for a family whose run length varies: [try_compose] reports the
   shorthand together with the number of positions it consumes. An earlier
   composer may already own a position, so absorbed slots are skipped. *)
let compose_run_via_index idx ~try_compose =
  let n = Rule_index.length idx in
  let i = ref 0 in
  while !i < n do
    if Rule_index.is_absorbed idx !i then incr i
    else
      match try_compose idx !i with
      | None -> incr i
      | Some (shorthand, k) ->
          let absorbed = List.init k (fun j -> !i + j) in
          if Rule_index.absorb idx ~at:!i ~absorbed ~shorthand then i := !i + k
          else incr i
  done

(* Collect the contiguous run of one family's longhands starting at [i], as
   (decl, part) pairs, with its length. *)
let take_run_at idx ~part_of i =
  let n = Rule_index.length idx in
  let rec aux j acc =
    if j >= n then (List.rev acc, j - i)
    else if Rule_index.is_absorbed idx j then (List.rev acc, j - i)
    else
      let d = Rule_index.decl_at idx j in
      match part_of d with
      | Some f -> aux (j + 1) ((d, f) :: acc)
      | None -> (List.rev acc, j - i)
  in
  aux i []

(* A slot a declaration after the run writes again is one the shorthand's reset
   cannot reach: the later write is what the element ends up with, whatever the
   reset put there. Unlike [*_overwritten_slots] this counts a write back to the
   slot initial, which answers the reset just as well. *)
let rec slots_written_after ~slots_of idx ~from =
  let n = Rule_index.length idx in
  if from >= n then 0
  else
    let rest = slots_written_after ~slots_of idx ~from:(from + 1) in
    if Rule_index.is_absorbed idx from then rest
    else slots_of (Rule_index.decl_at idx from) lor rest

let same_importance = function
  | [] -> true
  | first :: rest ->
      let important = is_important first in
      List.for_all (fun d -> is_important d = important) rest

(* The property a name spells, when the reader types one.
   [Properties.read_any_property] is the table [pp_property] inverts, so a name
   resolves to the very constructor the typed matches below test. *)
let property_of_name name =
  let t = Cursor.of_string name in
  match
    let property = Properties.read_any_property t in
    Cursor.expect_eof t;
    property
  with
  | property -> Some property
  | exception Cursor.Parse_error _ -> None

(* [border] / [border-<edge>] disambiguate width/style/color by type, so a
   [var()] (or other runtime substitution) in a longhand cannot be safely folded
   into the shorthand: the substituted tokens might re-assign to a different
   component, and one bad substitution invalidates the whole shorthand rather
   than the single longhand. Positional same-type shorthands (padding, ...) are
   exempt. *)
let has_runtime_substitution d = Variables.vars_of_declarations [ d ] <> []
