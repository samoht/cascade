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

type property_name = Property_name of string

type declaration_feature =
  | Declaration of Declaration.t
  | Empty of property_name
  | Vendor_flag_enabled

type t =
  | Property of declaration_feature
      (** [(property: value)] declaration feature test *)
  | Func of string * Component.t list
      (** [name(args)] function test (selector, font-format, font-tech, var,
          etc.) *)
  | Not of t  (** [not (condition)] negation *)
  | And of t * t  (** [(cond1) and (cond2)] conjunction *)
  | Or of t * t  (** [(cond1) or (cond2)] disjunction *)

let component_values s = Cursor.of_string s |> Cursor.remaining

let starts_with ~prefix s =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let property_name name =
  let reader = Cursor.of_string name in
  let parsed = Cursor.ident ~keep_case:true reader in
  if not (Cursor.is_done reader) then
    invalid_arg ("invalid supports declaration property name: " ^ name);
  let name =
    if starts_with ~prefix:"--" parsed then parsed
    else String.lowercase_ascii parsed
  in
  Property_name name

let string_of_property_name (Property_name name) = name

let declaration_feature prop value =
  match (String.lowercase_ascii prop, String.lowercase_ascii value) with
  | _, "" -> Empty (property_name prop)
  | "-vendor-flag", "enabled" -> Vendor_flag_enabled
  | _ -> (
      match Declaration.of_string (prop ^ ":" ^ value) with
      | Declaration.Declaration _ as decl -> Declaration decl
      | _ -> invalid_arg ("unsupported supports declaration: " ^ prop)
      | exception Error.Parse_error _ ->
          invalid_arg ("unsupported supports declaration: " ^ prop))

let property prop value = Property (declaration_feature prop value)
let func name args = Func (name, component_values args)

(* Selector is subsumed by [Func ("selector", ...)] for simplicity. The argument
   is still a CSS component stream, because arbitrary selector text is not a
   declaration value. *)

(* ===== Pretty printing ===== *)

let rec to_string condition = render `Root condition

and render context = function
  | Property feature -> "(" ^ render_declaration_feature feature ^ ")"
  | Func (name, args) -> name ^ "(" ^ Parser.to_string args ^ ")"
  | Not cond ->
      let rendered = "not " ^ render_not_operand cond in
      if context = `Operand then "(" ^ rendered ^ ")" else rendered
  | And (a, b) -> render_branch `And a ^ " and " ^ render_branch `And b
  | Or (a, b) -> render_branch `Or a ^ " or " ^ render_branch `Or b

and render_not_operand = function
  | (And _ | Or _) as cond -> "(" ^ render `Root cond ^ ")"
  | cond -> render `Root cond

and render_branch operator = function
  | Or _ as cond when operator = `And -> "(" ^ render `Root cond ^ ")"
  | And _ as cond when operator = `Or -> "(" ^ render `Root cond ^ ")"
  | Not _ as cond -> render `Operand cond
  | cond -> render `Root cond

and render_declaration_feature = function
  | Declaration decl -> Declaration.string_of_declaration ~minify:false decl
  | Empty name -> string_of_property_name name ^ ":"
  | Vendor_flag_enabled -> "-vendor-flag: enabled"

let pp_declaration_feature ctx = function
  | Declaration decl -> Declaration.pp_declaration ctx decl
  | Empty name ->
      Pp.string ctx (string_of_property_name name);
      Pp.char ctx ':'
  | Vendor_flag_enabled ->
      Pp.string ctx "-vendor-flag:";
      Pp.space_if_pretty ctx ();
      Pp.string ctx "enabled"

let rec pp_aux ~in_and ctx = function
  | Property feature ->
      Pp.char ctx '(';
      pp_declaration_feature ctx feature;
      Pp.char ctx ')'
  | Func (name, args) ->
      Pp.string ctx name;
      Pp.char ctx '(';
      Pp.string ctx
        (if Pp.minified ctx then Parser.to_string_minified args
         else Parser.to_string args);
      Pp.char ctx ')'
  | Not cond -> pp_not ~in_and ctx cond
  | And (a, b) -> pp_and ctx a b
  | Or (a, b) -> pp_or ctx a b

and pp_not ~in_and ctx cond =
  let extra_parens = Pp.minified ctx && not in_and in
  Pp.string ctx "(not ";
  if extra_parens then Pp.char ctx '(';
  (match cond with
  | And _ | Or _ ->
      Pp.char ctx '(';
      pp_aux ~in_and ctx cond;
      Pp.char ctx ')'
  | _ -> pp_aux ~in_and ctx cond);
  if extra_parens then Pp.char ctx ')';
  Pp.char ctx ')'

and pp_and_branch ctx = function
  | Or _ as branch ->
      Pp.char ctx '(';
      pp_aux ~in_and:true ctx branch;
      Pp.char ctx ')'
  | branch -> pp_aux ~in_and:true ctx branch

and pp_and ctx a b =
  pp_and_branch ctx a;
  Pp.string ctx " and ";
  pp_and_branch ctx b

and pp_or_branch ~is_left ctx = function
  | And (a, b) ->
      Pp.char ctx '(';
      pp_or_and_left ~is_left ctx a;
      Pp.string ctx " and ";
      pp_aux ~in_and:true ctx b;
      Pp.char ctx ')'
  | branch -> pp_aux ~in_and:false ctx branch

and pp_or_and_left ~is_left ctx = function
  | Property _ as branch when Pp.minified ctx && is_left ->
      Pp.char ctx '(';
      pp_aux ~in_and:true ctx branch;
      Pp.char ctx ')'
  | branch -> pp_aux ~in_and:true ctx branch

and pp_or ctx a b =
  pp_or_branch ~is_left:true ctx a;
  Pp.string ctx " or ";
  pp_or_branch ~is_left:false ctx b

let pp ctx t = pp_aux ~in_and:false ctx t

(* ===== Scanner ===== *)

type scanner = { s : string; mutable pos : int; allow_unwrapped_decl : bool }

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

let valid_property_test prop value =
  prop <> ""
  && (not (String.contains prop '('))
  && not (String.contains value ';')

(* ===== Recursive descent parser following the CSS spec grammar ===== *)

(** Parse <supports-in-parens>:
    - ( <supports-condition> )
    - ( <declaration> ) → Property
    - <function-token> <any> ) → Func / selector
    - <declaration> with no surrounding parens (browser-compatible relaxation
      used by [@import supports(prop:value)]) → Property *)
let rec parse_supports_in_parens sc =
  skip_ws sc;
  if at_end sc then failwith "Unexpected end of @supports condition";
  match peek sc with
  | Some '(' -> parse_paren_content sc
  | _ -> (
      (* If the remaining input has a top-level ':' that isn't part of a
         pseudo-class function call, treat it as an unwrapped <declaration>. *)
      let remaining = String.sub sc.s sc.pos (String.length sc.s - sc.pos) in
      match
        if sc.allow_unwrapped_decl then top_level_colon remaining else None
      with
      | Some colon_pos ->
          let prop = String.sub remaining 0 colon_pos |> String.trim in
          let value =
            String.sub remaining (colon_pos + 1)
              (String.length remaining - colon_pos - 1)
            |> String.trim
          in
          sc.pos <- String.length sc.s;
          if valid_property_test prop value then property prop value
          else failwith "Invalid declaration in @supports"
      | None -> parse_function sc)

(** Parse parenthesised content: could be property test or grouped condition. *)
and parse_paren_content sc =
  advance sc;
  (* consume '(' *)
  let content = read_balanced sc in
  let trimmed = String.trim content in
  if String.length trimmed = 0 then failwith "Empty parentheses in @supports";
  (* Try <supports-condition>: starts with "not" *)
  if looking_at_sub trimmed "not" then
    let sub =
      { s = trimmed; pos = 0; allow_unwrapped_decl = sc.allow_unwrapped_decl }
    in
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
        if valid_property_test prop value then property prop value
        else failwith "Invalid declaration in @supports"
    | None ->
        (* No colon → grouped <supports-condition> *)
        let sub =
          {
            s = trimmed;
            pos = 0;
            allow_unwrapped_decl = sc.allow_unwrapped_decl;
          }
        in
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
  let lower_name = String.lowercase_ascii name in
  if lower_name = "and" || lower_name = "or" || lower_name = "not" then
    failwith ("Invalid function name in @supports: " ^ name);
  skip_ws sc;
  match peek sc with
  | Some '(' ->
      advance sc;
      let args = String.trim (read_balanced sc) in
      if
        args = ""
        &&
        let lower = String.lowercase_ascii name in
        lower = "selector" || lower = "font-format" || lower = "font-tech"
      then failwith ("Empty " ^ name ^ "() in @supports")
      else func name args
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
    Not (parse_supports_in_parens sc))
  else
    let left = parse_supports_in_parens sc in
    chain sc None left

and chain sc op acc =
  skip_ws sc;
  if at_end sc then acc
  else if looking_at sc "and" then (
    (match op with
    | Some `Or -> failwith "Cannot mix and/or without parentheses in @supports"
    | _ -> ());
    sc.pos <- sc.pos + 3;
    let right = parse_supports_in_parens sc in
    chain sc (Some `And) (And (acc, right)))
  else if looking_at sc "or" then (
    (match op with
    | Some `And -> failwith "Cannot mix and/or without parentheses in @supports"
    | _ -> ());
    sc.pos <- sc.pos + 2;
    let right = parse_supports_in_parens sc in
    chain sc (Some `Or) (Or (acc, right)))
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

let of_string ?(allow_unwrapped_decl = false) s =
  let sc = { s = String.trim s; pos = 0; allow_unwrapped_decl } in
  let cond = parse_supports_condition sc in
  skip_ws sc;
  if not (at_end sc) then
    failwith
      (String.concat ""
         [
           "trailing content at position ";
           string_of_int sc.pos;
           " in @supports: ";
           String.sub sc.s sc.pos (String.length sc.s - sc.pos);
         ]);
  cond

(* ===== Comparison ===== *)

let compare_declaration d1 d2 =
  let c =
    String.compare (Declaration.property_name d1) (Declaration.property_name d2)
  in
  if c <> 0 then c
  else
    String.compare
      (Declaration.string_of_value ~minify:true d1)
      (Declaration.string_of_value ~minify:true d2)

let compare_declaration_feature d1 d2 =
  match (d1, d2) with
  | Empty n1, Empty n2 ->
      String.compare (string_of_property_name n1) (string_of_property_name n2)
  | Empty _, (Declaration _ | Vendor_flag_enabled) -> -1
  | (Declaration _ | Vendor_flag_enabled), Empty _ -> 1
  | Vendor_flag_enabled, Vendor_flag_enabled -> 0
  | Vendor_flag_enabled, Declaration _ -> -1
  | Declaration _, Vendor_flag_enabled -> 1
  | Declaration d1, Declaration d2 -> compare_declaration d1 d2

let rec compare t1 t2 =
  match (t1, t2) with
  | Property d1, Property d2 -> compare_declaration_feature d1 d2
  | Func (n1, a1), Func (n2, a2) ->
      let c = String.compare n1 n2 in
      if c <> 0 then c
      else
        String.compare
          (Parser.to_string_minified a1)
          (Parser.to_string_minified a2)
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
