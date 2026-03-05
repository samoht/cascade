(** Structured [\@supports] conditions for type-safe feature query construction.
*)

type t =
  | Property of string * string  (** [(property: value)] feature test *)
  | Selector of Selector.t  (** [selector(sel)] selector test *)
  | Not of t  (** [not (condition)] negation *)
  | And of t * t  (** [(cond1) and (cond2)] conjunction *)
  | Or of t * t  (** [(cond1) or (cond2)] disjunction *)

let rec to_string = function
  | Property (prop, value) -> "(" ^ prop ^ ": " ^ value ^ ")"
  | Selector sel -> "selector(" ^ Selector.to_string sel ^ ")"
  | Not cond -> "(not " ^ to_string cond ^ ")"
  | And (a, b) -> to_string a ^ " and " ^ to_string b
  | Or (a, b) ->
      (* Tailwind quirk: double space before "or" after a Not condition *)
      let sep = match a with Not _ -> "  or " | _ -> " or " in
      to_string a ^ sep ^ to_string b

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
      (* Tailwind quirk: in minified mode, wraps the inner condition in extra
         parens: (not ((...))). In non-minified mode, single parens: (not
         (...)) *)
      Pp.string ctx "(not ";
      if Pp.minified ctx then Pp.char ctx '(';
      pp ctx cond;
      if Pp.minified ctx then Pp.char ctx ')';
      Pp.char ctx ')'
  | And (a, b) ->
      pp ctx a;
      Pp.string ctx " and ";
      pp ctx b
  | Or (a, b) ->
      pp ctx a;
      (* Tailwind quirk: in non-minified mode, double space before "or" after a
         Not condition. In minified mode, single space. *)
      if (not (Pp.minified ctx)) && match a with Not _ -> true | _ -> false
      then Pp.char ctx ' ';
      Pp.string ctx " or ";
      pp ctx b

(* ===== Parser for @supports conditions ===== *)

(** Skip whitespace in string starting at position [i]. *)
let skip_ws s i =
  let len = String.length s in
  let j = ref i in
  while !j < len && (s.[!j] = ' ' || s.[!j] = '\t' || s.[!j] = '\n') do
    incr j
  done;
  !j

(** Find the matching closing paren for an opening paren at position [i].
    Handles nested parens. *)
let find_matching_paren s i =
  let len = String.length s in
  let depth = ref 1 in
  let j = ref (i + 1) in
  while !j < len && !depth > 0 do
    if s.[!j] = '(' then incr depth else if s.[!j] = ')' then decr depth;
    if !depth > 0 then incr j
  done;
  if !depth = 0 then !j else failwith "Unmatched paren in @supports condition"

(** Check if string [s] starting at position [i] matches keyword [kw]
    (case-insensitive) followed by whitespace. *)
let looking_at_keyword s i kw =
  let kw_len = String.length kw in
  let s_len = String.length s in
  if i + kw_len > s_len then false
  else
    let matches = ref true in
    for k = 0 to kw_len - 1 do
      if Char.lowercase_ascii s.[i + k] <> Char.lowercase_ascii kw.[k] then
        matches := false
    done;
    !matches
    && (i + kw_len >= s_len
       || s.[i + kw_len] = ' '
       || s.[i + kw_len] = '('
       || s.[i + kw_len] = '\t')

(** Parse a single @supports condition atom (property test or nested group).
    Returns (condition, next_position). *)
let rec parse_atom s i =
  let i = skip_ws s i in
  if i >= String.length s then failwith "Unexpected end of @supports condition"
  else if looking_at_keyword s i "not" then
    let j = skip_ws s (i + 3) in
    let cond, k = parse_atom s j in
    (Not cond, k)
  else if s.[i] = '(' then
    (* Could be a property test or a grouped condition *)
    let close = find_matching_paren s i in
    let inner = String.sub s (i + 1) (close - i - 1) in
    let inner_trimmed = String.trim inner in
    (* Check if inner content is a "not/and/or" expression or a property test *)
    if looking_at_keyword inner_trimmed 0 "not" then
      let cond, _ = parse_atom inner_trimmed 0 in
      (cond, close + 1)
    else
      match String.index_opt inner_trimmed ':' with
      | Some colon_pos ->
          (* Check for nested parens before the colon - if so, it might be a
             grouped expression *)
          let before_colon =
            String.sub inner_trimmed 0 colon_pos |> String.trim
          in
          if String.contains before_colon '(' then
            (* Grouped expression like ((cond) and (cond)) *)
            let cond, _ = parse_expr inner_trimmed 0 in
            (cond, close + 1)
          else
            let prop = before_colon in
            let value =
              String.sub inner_trimmed (colon_pos + 1)
                (String.length inner_trimmed - colon_pos - 1)
              |> String.trim
            in
            (Property (prop, value), close + 1)
      | None ->
          (* No colon - must be a grouped expression *)
          let cond, _ = parse_expr inner_trimmed 0 in
          (cond, close + 1)
  else
    failwith
      ("Unexpected character in @supports condition at position "
     ^ string_of_int i ^ ": "
      ^ String.make 1 s.[i])

(** Parse a full @supports expression with and/or operators. *)
and parse_expr s i =
  let left, j = parse_atom s i in
  let j = skip_ws s j in
  if j >= String.length s then (left, j)
  else if looking_at_keyword s j "and" then
    let right, k = parse_atom s (j + 3) in
    let result = And (left, right) in
    chain_ops s k result
  else if looking_at_keyword s j "or" then
    let right, k = parse_atom s (j + 2) in
    let result = Or (left, right) in
    chain_ops s k result
  else (left, j)

and chain_ops s i acc =
  let i = skip_ws s i in
  if i >= String.length s then (acc, i)
  else if looking_at_keyword s i "and" then
    let right, j = parse_atom s (i + 3) in
    chain_ops s j (And (acc, right))
  else if looking_at_keyword s i "or" then
    let right, j = parse_atom s (i + 2) in
    chain_ops s j (Or (acc, right))
  else (acc, i)

let of_string s =
  let s = String.trim s in
  let cond, _ = parse_expr s 0 in
  cond

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
  (* Order: Property < Selector < Not < And < Or *)
  | Property _, _ -> -1
  | _, Property _ -> 1
  | Selector _, _ -> -1
  | _, Selector _ -> 1
  | Not _, _ -> -1
  | _, Not _ -> 1
  | And _, _ -> -1
  | _, And _ -> 1

let equal a b = compare a b = 0
