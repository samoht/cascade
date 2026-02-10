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
  | Property (prop, value) -> "(" ^ prop ^ ": " ^ value ^ ")"
  | Selector sel -> "selector(" ^ Selector.to_string sel ^ ")"
  | Not cond -> "not " ^ to_string cond
  | And (a, b) -> to_string a ^ " and " ^ to_string b
  | Or (a, b) -> to_string a ^ " or " ^ to_string b
  | Raw s -> s

let rec pp ctx = function
  | Property (prop, value) ->
      Pp.char ctx '(';
      Pp.string ctx prop;
      Pp.char ctx ':';
      Pp.space_if_pretty ctx ();
      Pp.string ctx value;
      Pp.char ctx ')'
  | Selector sel ->
      Pp.string ctx "selector(";
      Pp.string ctx (Selector.to_string sel);
      Pp.char ctx ')'
  | Not cond ->
      Pp.string ctx "not ";
      pp ctx cond
  | And (a, b) ->
      pp ctx a;
      Pp.string ctx " and ";
      pp ctx b
  | Or (a, b) ->
      pp ctx a;
      Pp.string ctx " or ";
      pp ctx b
  | Raw s ->
      (* For Raw strings, adjust spacing based on minify mode. Replace ": " with
         ":" when minifying. *)
      let s =
        if Pp.minified ctx then (
          (* Remove space after colon in property declarations *)
          let buf = Buffer.create (String.length s) in
          let i = ref 0 in
          while !i < String.length s do
            let c = s.[!i] in
            if c = ':' && !i + 1 < String.length s && s.[!i + 1] = ' ' then (
              Buffer.add_char buf ':';
              incr i (* skip the space *))
            else Buffer.add_char buf c;
            incr i
          done;
          Buffer.contents buf)
        else s
      in
      Pp.string ctx s

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
