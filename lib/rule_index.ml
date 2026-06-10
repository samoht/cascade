open Declaration

type slot = Live | Absorbed | Shorthand of declaration

type t = {
  decls : declaration array;
  slots : slot array;
  by_prop : (Declaration.prop_key, int list) Hashtbl.t;
}

let prop_key_of d =
  match d with
  | Declaration { property; _ } -> Declaration.Key property
  | Theme_guarded _ as d -> Declaration.property_key d

let build decls =
  let decls = Array.of_list decls in
  let n = Array.length decls in
  let slots = Array.make n Live in
  let by_prop : (Declaration.prop_key, int list) Hashtbl.t =
    Hashtbl.create (max 16 n)
  in
  (* Walk back-to-front so the per-property positions list ends up in cascade
     order without a final reverse. *)
  for i = n - 1 downto 0 do
    let k = prop_key_of decls.(i) in
    let prev = try Hashtbl.find by_prop k with Not_found -> [] in
    Hashtbl.replace by_prop k (i :: prev)
  done;
  { decls; slots; by_prop }

let length t = Array.length t.decls
let decl_at t i = t.decls.(i)

let positions (type a) t (p : a Properties.property) =
  try Hashtbl.find t.by_prop (Declaration.Key p) with Not_found -> []

let is_absorbed t i =
  match t.slots.(i) with Live | Shorthand _ -> false | Absorbed -> true

let absorb t ~at ~absorbed ~shorthand =
  List.iter
    (fun i ->
      match t.slots.(i) with
      | Live -> t.slots.(i) <- Absorbed
      | Absorbed | Shorthand _ ->
          failwith "Rule_index.absorb: position already consumed")
    absorbed;
  match t.slots.(at) with
  | Absorbed ->
      (* [at] is typically the earliest absorbed position, which was just marked
         Absorbed above; promote it to carry the synthesized shorthand. *)
      t.slots.(at) <- Shorthand shorthand
  | Live -> t.slots.(at) <- Shorthand shorthand
  | Shorthand _ -> failwith "Rule_index.absorb: shorthand slot already taken"

let to_list t =
  let n = Array.length t.decls in
  let rec loop i acc =
    if i < 0 then acc
    else
      let acc =
        match t.slots.(i) with
        | Live -> t.decls.(i) :: acc
        | Absorbed -> acc
        | Shorthand d -> d :: acc
      in
      loop (i - 1) acc
  in
  loop (n - 1) []
