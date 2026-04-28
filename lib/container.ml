(** CSS container query condition types *)

type t =
  | Min_width_rem of float
  | Min_width_px of int
  | Named of string * t
  | Custom of Media.t

(* Format float without trailing period (24. -> 24, 24.5 -> 24.5) *)
let format_rem f =
  let s = string_of_float f in
  if String.ends_with ~suffix:"." s then String.sub s 0 (String.length s - 1)
  else s

let rec to_string = function
  | Min_width_rem rem -> "(min-width:" ^ format_rem rem ^ "rem)"
  | Min_width_px px -> "(min-width:" ^ Int.to_string px ^ "px)"
  | Named (name, cond) -> name ^ " " ^ to_string cond
  | Custom cond -> Media.to_string cond

let pp = to_string

let rec compare t1 t2 =
  match (t1, t2) with
  | Min_width_rem r1, Min_width_rem r2 -> Float.compare r1 r2
  | Min_width_px p1, Min_width_px p2 -> Int.compare p1 p2
  | Named (n1, c1), Named (n2, c2) ->
      let name_cmp = String.compare n1 n2 in
      if name_cmp <> 0 then name_cmp else compare c1 c2
  | Custom c1, Custom c2 -> Media.compare c1 c2
  (* Order: Min_width_rem < Min_width_px < Named < Custom *)
  | Min_width_rem _, _ -> -1
  | _, Min_width_rem _ -> 1
  | Min_width_px _, _ -> -1
  | _, Min_width_px _ -> 1
  | Named _, Custom _ -> -1
  | Custom _, Named _ -> 1

type kind = Kind_min_width | Kind_other

let rec kind = function
  | Min_width_rem _ | Min_width_px _ -> Kind_min_width
  | Named (_, cond) -> kind cond
  | Custom _ -> Kind_other

let of_string s =
  match Media.of_string s with
  | Media.Min_width_rem rem -> Min_width_rem rem
  | Media.Min_width px when Float.is_integer px ->
      Min_width_px (int_of_float px)
  | media -> Custom media
