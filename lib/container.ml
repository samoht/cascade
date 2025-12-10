(** CSS container query condition types *)

type t =
  | Min_width_rem of float
  | Min_width_px of int
  | Named of string * t
  | Raw of string

let rec to_string = function
  | Min_width_rem rem -> Printf.sprintf "(min-width:%grem)" rem
  | Min_width_px px -> Printf.sprintf "(min-width:%dpx)" px
  | Named (name, cond) -> name ^ " " ^ to_string cond
  | Raw s -> s

let rec compare t1 t2 =
  match (t1, t2) with
  | Min_width_rem r1, Min_width_rem r2 -> Float.compare r1 r2
  | Min_width_px p1, Min_width_px p2 -> Int.compare p1 p2
  | Named (n1, c1), Named (n2, c2) ->
      let name_cmp = String.compare n1 n2 in
      if name_cmp <> 0 then name_cmp else compare c1 c2
  | Raw s1, Raw s2 -> String.compare s1 s2
  (* Order: Min_width_rem < Min_width_px < Named < Raw *)
  | Min_width_rem _, _ -> -1
  | _, Min_width_rem _ -> 1
  | Min_width_px _, _ -> -1
  | _, Min_width_px _ -> 1
  | Named _, Raw _ -> -1
  | Raw _, Named _ -> 1

type kind = Kind_min_width | Kind_other

let rec kind = function
  | Min_width_rem _ | Min_width_px _ -> Kind_min_width
  | Named (_, cond) -> kind cond
  | Raw _ -> Kind_other
