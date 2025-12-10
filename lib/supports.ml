(** Structured [\@supports] conditions for type-safe feature query construction.
*)

type t =
  | Property of string * string  (** [(property: value)] feature test *)
  | Selector of Selector.t  (** [selector(sel)] selector test *)
  | Not of t  (** [not (condition)] negation *)
  | And of t * t  (** [(cond1) and (cond2)] conjunction *)
  | Or of t * t  (** [(cond1) or (cond2)] disjunction *)
  | Raw of string  (** Escape hatch for unparsed conditions *)

let rec to_string = function
  | Property (prop, value) -> Printf.sprintf "(%s:%s)" prop value
  | Selector sel -> Printf.sprintf "selector(%s)" (Selector.to_string sel)
  | Not cond -> Printf.sprintf "not %s" (to_string cond)
  | And (a, b) -> Printf.sprintf "%s and %s" (to_string a) (to_string b)
  | Or (a, b) -> Printf.sprintf "%s or %s" (to_string a) (to_string b)
  | Raw s -> s

let rec compare t1 t2 =
  match (t1, t2) with
  | Property (p1, v1), Property (p2, v2) ->
      let c = String.compare p1 p2 in
      if c <> 0 then c else String.compare v1 v2
  | Selector s1, Selector s2 ->
      String.compare (Selector.to_string s1) (Selector.to_string s2)
  | Not a, Not b -> compare a b
  | And (a1, b1), And (a2, b2) ->
      let c = compare a1 a2 in
      if c <> 0 then c else compare b1 b2
  | Or (a1, b1), Or (a2, b2) ->
      let c = compare a1 a2 in
      if c <> 0 then c else compare b1 b2
  | Raw s1, Raw s2 -> String.compare s1 s2
  (* Order: Property < Selector < Not < And < Or < Raw *)
  | Property _, _ -> -1
  | _, Property _ -> 1
  | Selector _, _ -> -1
  | _, Selector _ -> 1
  | Not _, _ -> -1
  | _, Not _ -> 1
  | And _, _ -> -1
  | _, And _ -> 1
  | Or _, Raw _ -> -1
  | Raw _, Or _ -> 1

let equal a b = compare a b = 0
