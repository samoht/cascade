(* A property's typed identity and importance, which a default factoring groups
   declarations by. *)

type t = { prop : Declaration.prop_key; important : bool; hash : int }

let of_declaration decl =
  let prop = Declaration.property_key decl in
  let important = Declaration.is_important decl in
  {
    prop;
    important;
    hash =
      Common.mix_int
        (Declaration.hash_prop_key prop)
        (if important then 1 else 0);
  }

let equal left right =
  Bool.equal left.important right.important
  && Declaration.equal_prop_key left.prop right.prop

let hash key = key.hash

let pp ctx { prop = Declaration.Key property; important; _ } =
  Properties.pp_property ctx property;
  if important then Pp.string ctx "!important"

(* [equal (of_declaration decl) key] without building [decl]'s key: a factoring
   asks it of every declaration of every member of every candidate group. *)
let has key decl =
  Bool.equal (Declaration.is_important decl) key.important
  && Declaration.equal_prop_key (Declaration.property_key decl) key.prop

let has_any keys decl = List.exists (fun key -> has key decl) keys

let single key decls =
  let rec loop found = function
    | [] -> found
    | decl :: rest ->
        if has key decl then
          match found with
          | Option.None -> loop (Option.Some decl) rest
          | Option.Some _ -> Option.None
        else loop found rest
  in
  loop Option.None decls

let of_declarations decls =
  List.fold_left
    (fun keys decl ->
      let key = of_declaration decl in
      if List.exists (equal key) keys then keys else key :: keys)
    [] decls

let rec written key = function
  | [] -> false
  | decl :: rest -> has key decl || written key rest

let remove keys decls = List.filter (fun decl -> not (has_any keys decl)) decls
