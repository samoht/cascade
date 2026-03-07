(** Structured [@supports] conditions for type-safe feature query construction.

    Implements the grammar from CSS Conditional Rules Level 3/4/5:
    {v
    <supports-condition> = not <supports-in-parens>
                         | <supports-in-parens> [ and <supports-in-parens> ]*
                         | <supports-in-parens> [ or <supports-in-parens> ]*

    <supports-in-parens> = ( <supports-condition> )
                         | <supports-feature>
                         | <general-enclosed>

    <supports-feature> = <supports-decl>
                       | <supports-selector-fn>
                       | <supports-font-tech-fn>
                       | <supports-font-format-fn>

    <supports-decl> = ( <declaration> )
    <supports-selector-fn> = selector( <complex-selector> )
    <supports-font-tech-fn> = font-tech( <font-tech> )
    <supports-font-format-fn> = font-format( <font-format> )
    <general-enclosed> = <function-token> <any-value>? )
                       | ( <any-value> )
    v} *)

type t =
  | Property of string * string
      (** [(property: value)] declaration feature test *)
  | Func of string * string
      (** [name(args)] function test (selector, font-format, font-tech, var,
          etc.) *)
  | Not of t  (** [not (condition)] negation *)
  | And of t * t  (** [(cond1) and (cond2)] conjunction *)
  | Or of t * t  (** [(cond1) or (cond2)] disjunction *)

(* Selector is subsumed by Func("selector", ...) for simplicity — the selector
   content is stored as raw string to avoid css identifier validation on
   arbitrary selectors like "A > B". *)

(* ===== Pretty printing ===== *)

let rec to_string = function
  | Property (prop, value) -> "(" ^ prop ^ ": " ^ value ^ ")"
  | Func (name, args) -> name ^ "(" ^ args ^ ")"
  | Not cond -> "(not " ^ to_string cond ^ ")"
  | And (a, b) -> to_string a ^ " and " ^ to_string b
  | Or (a, b) ->
      (* Wrap And branches in parens when inside Or *)
      let wrap = function
        | And _ as x -> "(" ^ to_string x ^ ")"
        | x -> to_string x
      in
      let sep = match a with Not _ -> "  or " | _ -> " or " in
      wrap a ^ sep ^ wrap b

let rec pp_aux ~in_and ctx = function
  | Property (prop, value) ->
      Pp.char ctx '(';
      Pp.string ctx prop;
      Pp.char ctx ':';
      Pp.space_if_pretty ctx ();
      Pp.string ctx value;
      Pp.char ctx ')'
  | Func (name, args) ->
      Pp.string ctx name;
      Pp.char ctx '(';
      Pp.string ctx args;
      Pp.char ctx ')'
  | Not cond ->
      (* Tailwind/LightningCSS quirk: in minified mode, Not wraps its child in
         extra parens — but only when Not is NOT nested inside And. Goal: match
         tailwindcss output exactly. *)
      let extra_parens = Pp.minified ctx && not in_and in
      Pp.string ctx "(not ";
      if extra_parens then Pp.char ctx '(';
      pp_aux ~in_and ctx cond;
      if extra_parens then Pp.char ctx ')';
      Pp.char ctx ')'
  | And (a, b) ->
      pp_aux ~in_and:true ctx a;
      Pp.string ctx " and ";
      pp_aux ~in_and:true ctx b
  | Or (a, b) ->
      (* Wrap And branches in parens when inside Or, to match tailwindcss output
         exactly. LightningCSS quirk: in minified mode, the left branch's first
         Property operand gets extra grouping parens. *)
      let pp_or_branch ~is_left ctx = function
        | And (a1, b1) ->
            Pp.char ctx '(';
            (* LightningCSS quirk: first Property in left And gets extra parens
               in minified mode. Goal: match tailwindcss exactly. *)
            (match a1 with
            | Property _ when Pp.minified ctx && is_left ->
                Pp.char ctx '(';
                pp_aux ~in_and:true ctx a1;
                Pp.char ctx ')'
            | _ -> pp_aux ~in_and:true ctx a1);
            Pp.string ctx " and ";
            pp_aux ~in_and:true ctx b1;
            Pp.char ctx ')'
        | x -> pp_aux ~in_and:false ctx x
      in
      pp_or_branch ~is_left:true ctx a;
      (* Tailwind quirk: double space before "or" after Not in non-minified
         mode. Goal: match tailwindcss output exactly. *)
      if (not (Pp.minified ctx)) && match a with Not _ -> true | _ -> false
      then Pp.char ctx ' ';
      Pp.string ctx " or ";
      pp_or_branch ~is_left:false ctx b

let pp ctx t = pp_aux ~in_and:false ctx t

(* ===== Scanner ===== *)

type scanner = { s : string; mutable pos : int }

let peek sc = if sc.pos < String.length sc.s then Some sc.s.[sc.pos] else None
let advance sc = sc.pos <- sc.pos + 1
let at_end sc = sc.pos >= String.length sc.s

let skip_ws sc =
  while
    (not (at_end sc))
    &&
    let c = sc.s.[sc.pos] in
    c = ' ' || c = '\t' || c = '\n'
  do
    advance sc
  done

(** Check if scanner is looking at [kw] (case-insensitive) followed by a
    non-identifier character or end-of-input. *)
let looking_at sc kw =
  let kw_len = String.length kw in
  let s_len = String.length sc.s in
  if sc.pos + kw_len > s_len then false
  else
    let ok = ref true in
    for k = 0 to kw_len - 1 do
      if Char.lowercase_ascii sc.s.[sc.pos + k] <> Char.lowercase_ascii kw.[k]
      then ok := false
    done;
    !ok
    && (sc.pos + kw_len >= s_len
       ||
       let c = sc.s.[sc.pos + kw_len] in
       c = ' ' || c = '(' || c = '\t')

(** Read balanced parenthesised content. Assumes '(' already consumed; reads
    through matching ')'. Returns inner content. *)
let read_balanced sc =
  let buf = Buffer.create 32 in
  let depth = ref 1 in
  while !depth > 0 do
    match peek sc with
    | None -> failwith "Unmatched parenthesis in @supports condition"
    | Some '(' ->
        incr depth;
        Buffer.add_char buf '(';
        advance sc
    | Some ')' ->
        decr depth;
        if !depth > 0 then Buffer.add_char buf ')';
        advance sc
    | Some c ->
        Buffer.add_char buf c;
        advance sc
  done;
  Buffer.contents buf

(** Read an identifier: [-a-zA-Z0-9_]+ *)
let read_ident sc =
  let start = sc.pos in
  while
    (not (at_end sc))
    &&
    let c = sc.s.[sc.pos] in
    (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9')
    || c = '-' || c = '_'
  do
    advance sc
  done;
  if sc.pos = start then "" else String.sub sc.s start (sc.pos - start)

(** Find the first ':' at parenthesis depth 0 in [s]. Returns [Some pos] if
    found. This distinguishes property tests [(prop: value)] from grouped
    conditions containing function calls with colons. *)
let top_level_colon s =
  let len = String.length s in
  let depth = ref 0 in
  let result = ref None in
  let i = ref 0 in
  while !i < len && !result = None do
    (match s.[!i] with
    | '(' -> incr depth
    | ')' -> decr depth
    | ':' when !depth = 0 -> result := Some !i
    | _ -> ());
    incr i
  done;
  !result

(* ===== Recursive descent parser following the CSS spec grammar ===== *)

(** Parse <supports-in-parens>:
    - ( <supports-condition> )
    - ( <declaration> ) → Property
    - <function-token> <any> ) → Func / selector *)
let rec parse_supports_in_parens sc =
  skip_ws sc;
  if at_end sc then failwith "Unexpected end of @supports condition";
  match peek sc with
  | Some '(' -> parse_paren_content sc
  | _ -> parse_function sc

(** Parse parenthesised content: could be property test or grouped condition. *)
and parse_paren_content sc =
  advance sc;
  (* consume '(' *)
  let content = read_balanced sc in
  let trimmed = String.trim content in
  if String.length trimmed = 0 then failwith "Empty parentheses in @supports";
  (* Try <supports-condition>: starts with "not" *)
  if looking_at_sub trimmed "not" then
    let sub = { s = trimmed; pos = 0 } in
    parse_supports_condition sub
  else
    match top_level_colon trimmed with
    | Some colon_pos ->
        (* <supports-decl>: property: value *)
        let prop = String.sub trimmed 0 colon_pos |> String.trim in
        let value =
          String.sub trimmed (colon_pos + 1)
            (String.length trimmed - colon_pos - 1)
          |> String.trim
        in
        Property (prop, value)
    | None ->
        (* No colon → grouped <supports-condition> *)
        let sub = { s = trimmed; pos = 0 } in
        parse_supports_condition sub

(** Parse a bare function: name( args ) → Func *)
and parse_function sc =
  let name = read_ident sc in
  if name = "" then
    failwith
      (String.concat ""
         [
           "Expected identifier at position ";
           string_of_int sc.pos;
           " in @supports";
         ]);
  skip_ws sc;
  match peek sc with
  | Some '(' ->
      advance sc;
      let args = read_balanced sc in
      Func (name, String.trim args)
  | _ ->
      failwith
        (String.concat ""
           [
             "Expected '(' after '";
             name;
             "' at position ";
             string_of_int sc.pos;
             " in @supports";
           ])

(** Parse <supports-condition>:
    - not <supports-in-parens>
    - <supports-in-parens> [ and <supports-in-parens> ]*
    - <supports-in-parens> [ or <supports-in-parens> ]* *)
and parse_supports_condition sc =
  skip_ws sc;
  if looking_at sc "not" then (
    sc.pos <- sc.pos + 3;
    let cond = parse_supports_in_parens sc in
    Not cond)
  else
    let left = parse_supports_in_parens sc in
    chain sc left

and chain sc acc =
  skip_ws sc;
  if at_end sc then acc
  else if looking_at sc "and" then (
    sc.pos <- sc.pos + 3;
    let right = parse_supports_in_parens sc in
    chain sc (And (acc, right)))
  else if looking_at sc "or" then (
    sc.pos <- sc.pos + 2;
    let right = parse_supports_in_parens sc in
    chain sc (Or (acc, right)))
  else acc

(** Check if a substring starts with keyword [kw] (for sub-parsing). *)
and looking_at_sub s kw =
  let kw_len = String.length kw in
  let s_len = String.length s in
  if kw_len > s_len then false
  else
    let ok = ref true in
    for k = 0 to kw_len - 1 do
      if Char.lowercase_ascii s.[k] <> Char.lowercase_ascii kw.[k] then
        ok := false
    done;
    !ok
    && (kw_len >= s_len
       ||
       let c = s.[kw_len] in
       c = ' ' || c = '(' || c = '\t')

let of_string s =
  let sc = { s = String.trim s; pos = 0 } in
  let cond = parse_supports_condition sc in
  skip_ws sc;
  if not (at_end sc) then
    failwith
      (String.concat ""
         [
           "Trailing content at position ";
           string_of_int sc.pos;
           " in @supports: ";
           String.sub sc.s sc.pos (String.length sc.s - sc.pos);
         ]);
  cond

(* ===== Comparison ===== *)

let rec compare t1 t2 =
  match (t1, t2) with
  | Property (p1, v1), Property (p2, v2) ->
      let c = String.compare p1 p2 in
      if c <> 0 then c else String.compare v1 v2
  | Func (n1, a1), Func (n2, a2) ->
      let c = String.compare n1 n2 in
      if c <> 0 then c else String.compare a1 a2
  | Not a, Not b -> compare a b
  | And (a1, b1), And (a2, b2) ->
      let c = compare a1 a2 in
      if c <> 0 then c else compare b1 b2
  | Or (a1, b1), Or (a2, b2) ->
      let c = compare a1 a2 in
      if c <> 0 then c else compare b1 b2
  (* Order: Property < Func < Not < And < Or *)
  | Property _, _ -> -1
  | _, Property _ -> 1
  | Func _, _ -> -1
  | _, Func _ -> 1
  | Not _, _ -> -1
  | _, Not _ -> 1
  | And _, _ -> -1
  | _, And _ -> 1

let equal a b = compare a b = 0
