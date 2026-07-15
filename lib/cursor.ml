open Common

type t = {
  mutable cvs : Component.t list;
  source : string option;
  warnings : Error.t list ref;
  recover : bool;
  meta : Loc.meta_level;
  eof_loc : Loc.t option;
      (* Where to point errors raised at end of this cursor. For a cursor over a
         nested block body this is the block's closing delimiter position, not
         the end of the whole input. *)
  depth : int;
      (* Function-call nesting depth, bumped by [call] each time it descends
         into a function's arguments. Bounds recursion ([calc(calc(...))],
         [:is(:is(...))]) so [of_string] over untrusted CSS cannot be driven
         into a stack overflow or super-linear validation by deeply nested
         input. *)
}

(* CSS Syntax has no nesting limit, but real authored CSS nests functions only a
   handful deep; browsers cap too. Generous enough never to reject real input,
   small enough that the per-level selector validation walks stay linear. *)
let max_nesting_depth = 100

let of_components ?source ?(recover = false) ?(meta = Loc.default_meta_level)
    ?eof_loc cvs =
  { cvs; source; warnings = ref []; recover; meta; eof_loc; depth = 0 }

let sub ?eof_loc t cvs =
  {
    cvs;
    source = t.source;
    warnings = t.warnings;
    recover = t.recover;
    meta = t.meta;
    eof_loc = (match eof_loc with Some _ -> eof_loc | None -> t.eof_loc);
    depth = t.depth;
  }

let push_warning t e = t.warnings := e :: !(t.warnings)
let recover t = t.recover
let meta t = t.meta
let source t = t.source

let drain_warnings t =
  let ws = List.rev !(t.warnings) in
  t.warnings := [];
  ws

let lex_to_cv_list parser =
  let rec loop acc =
    let cv = Parser.next parser in
    match cv with
    | Component.Preserved { kind = Token.Eof; _ } -> List.rev acc
    | _ -> loop (cv :: acc)
  in
  loop []

(* [source] must be the post-preprocessing buffer (CSS Syntax section 3.3),
   which is what the lexer indexes against. Using the caller's raw string would
   desync [Loc.offset] from [Loc.snippet] when the input contains BOM, NUL, CR,
   FF, or CRLF. *)
let of_string ?(meta = Loc.default_meta_level) s =
  let reader = Reader.of_string s in
  let parser = Parser.of_reader reader in
  {
    cvs = lex_to_cv_list parser;
    source = Some (Reader.source reader);
    warnings = ref [];
    recover = false;
    meta;
    eof_loc = None;
    depth = 0;
  }

let of_reader ?(meta = Loc.default_meta_level) r =
  let parser = Parser.of_reader r in
  {
    cvs = lex_to_cv_list parser;
    source = Some (Reader.source r);
    warnings = ref [];
    recover = false;
    meta;
    eof_loc = None;
    depth = 0;
  }

let is_ws_cv : Component.t -> bool = function
  | Preserved { kind = Token.Whitespace; _ } -> true
  | _ -> false

let rec drop_ws t =
  match t.cvs with
  | hd :: rest when is_ws_cv hd ->
      t.cvs <- rest;
      drop_ws t
  | _ -> ()

let ws = drop_ws

let peek t =
  drop_ws t;
  match t.cvs with [] -> None | hd :: _ -> Some hd

let next t =
  drop_ws t;
  match t.cvs with
  | [] -> None
  | hd :: rest ->
      t.cvs <- rest;
      Some hd

let skip t = ignore (next t : Component.t option)

let is_done t =
  drop_ws t;
  t.cvs = []

let position t =
  drop_ws t;
  match t.cvs with
  | [] -> (
      (* Prefer the explicit EOF location (block closer, function ')', etc.)
         over the whole-input end; fall back to the source length only when
         neither is set. *)
      match t.eof_loc with
      | Some loc -> loc
      | None -> (
          match t.source with
          | None -> Loc.dummy
          | Some source ->
              let pos = String.length source in
              Loc.v ~start_pos:pos ~end_pos:pos))
  | hd :: _ -> Component.source_loc hd

let remaining t = t.cvs

let string_of_components ?(trim = false) cvs =
  let s = Parser.string_of_components cvs in
  if trim then String.trim s else s

let string_of_remaining ?(trim = false) t =
  string_of_components ~trim (remaining t)

let consume_remaining_as_string ?(trim = false) t =
  let cvs = remaining t in
  t.cvs <- [];
  string_of_components ~trim cvs

let peek_raw t = match t.cvs with [] -> None | hd :: _ -> Some hd

type head_shape =
  [ `Eof
  | `Semicolon
  | `Colon
  | `Comma
  | `Bang
  | `Curly_block
  | `Paren_block
  | `Square_block
  | `Ident
  | `Func
  | `Other ]

let peek_head_shape t : head_shape =
  drop_ws t;
  match t.cvs with
  | [] -> `Eof
  | Component.Preserved { kind; _ } :: _ -> (
      match kind with
      | Token.Semicolon -> `Semicolon
      | Token.Colon -> `Colon
      | Token.Comma -> `Comma
      | Token.Delim "!" -> `Bang
      | Token.Ident _ -> `Ident
      | _ -> `Other)
  | Component.Block { node = { opening; _ }; _ } :: _ -> (
      match opening with
      | Token.Curly -> `Curly_block
      | Token.Paren -> `Paren_block
      | Token.Square -> `Square_block)
  | Component.Func _ :: _ -> `Func

let next_raw t =
  match t.cvs with
  | [] -> None
  | hd :: rest ->
      t.cvs <- rest;
      Some hd

let skip_ws t =
  let started = t.cvs in
  drop_ws t;
  t.cvs != started

type snapshot = Component.t list

let save t = t.cvs
let restore t s = t.cvs <- s

(** {1 Errors} *)

exception Parse_error = Error.Parse_error

let sort = Sort.Component

let raise_ t kind loc =
  let source = match (t.meta, t.source) with `Full, s -> s | _ -> None in
  Error.fail (Error.v ?source ~loc ~sort kind)

let err ?got t msg =
  let loc = position t in
  match got with
  | Some g ->
      raise_ t
        (Error.Bad_value { property = ""; reason = msg ^ ": got " ^ g })
        loc
  | None -> raise_ t (Error.Bad_value { property = ""; reason = msg }) loc

let err_invalid t msg = err t ("invalid: " ^ msg)
let err_eof t = raise_ t (Error.Unterminated sort) (position t)
let err_expected t what = err t ("expected " ^ what)

let err_expected_but_eof t what =
  raise_ t (Error.Missing_token what) (position t)

let err_unexpected t = err t "unexpected token"
let with_context _t label f = Error.with_context label f

let atomic t f =
  let snap = save t in
  try f ()
  with Parse_error _ as e ->
    restore t snap;
    raise e

let lookahead p t =
  let snap = save t in
  let v = p t in
  restore t snap;
  v

(* Typed reader for a [<basic-shape>] / math call with a verbatim fallback:
   snapshot the cursor, run the typed reader, and on [Parse_error] restore +
   skip + return the captured call for the caller to wrap in an [Invalid]
   arm. *)
let try_typed_call (typed : t -> 'a) (t : t) : ('a, Component.t) result =
  let snap = save t in
  match peek t with
  | Some (Component.Func _ as comp) -> (
      match typed t with
      | value -> Ok value
      | exception Parse_error _ ->
          restore t snap;
          skip t;
          Error comp)
  | _ -> Ok (typed t)

(** {1 Token-shape helpers - option variants} *)

(* Inspect the head [Component.Preserved] directly to avoid the [Some hd] option
   allocation [peek] would do on each call - this is the workhorse for
   ident/number/percentage/delim_opt etc. *)
let take_token_if (f : Token.kind -> 'a option) t : 'a option =
  drop_ws t;
  match t.cvs with
  | Component.Preserved tok :: _ -> (
      match f tok.kind with
      | Some _ as r ->
          let _ = next t in
          r
      | None -> None)
  | _ -> None

let ident_opt t =
  take_token_if (function Token.Ident s -> Some s | _ -> None) t

let number_opt t =
  take_token_if
    (function Token.Number_tok { value; _ } -> Some value | _ -> None)
    t

let integer_opt t =
  take_token_if
    (function
      | Token.Number_tok { value; number_flag = Token.Integer; _ } ->
          Some (int_of_float value)
      | _ -> None)
    t

let percentage_opt t =
  take_token_if
    (function Token.Percentage { value; _ } -> Some value | _ -> None)
    t

let dimension_opt t =
  take_token_if
    (function
      | Token.Dimension { number = { value; _ }; unit_ } -> Some (value, unit_)
      | _ -> None)
    t

let hash_opt t =
  take_token_if (function Token.Hash { value; _ } -> Some value | _ -> None) t

let is_hex_string s =
  let rec loop i =
    if i = String.length s then i > 0
    else
      match s.[i] with
      | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> loop (i + 1)
      | _ -> false
  in
  loop 0

let string_opt t =
  take_token_if
    (function Token.String { value; _ } -> Some value | _ -> None)
    t

let string_with_quote_opt t =
  take_token_if
    (function
      | Token.String { value; quote; _ } -> Some (value, quote) | _ -> None)
    t

let source_slice t loc =
  match t.source with
  | None -> None
  | Some source ->
      let len = String.length source in
      if
        loc.Loc.start_pos < 0
        || loc.end_pos < loc.start_pos
        || loc.end_pos > len
      then None
      else Some (String.sub source loc.start_pos (loc.end_pos - loc.start_pos))

let string_repr_with_quote_opt t =
  match peek t with
  | Some
      (Component.Preserved
         ({ kind = Token.String { value; quote; _ }; loc } : Token.t)) ->
      skip t;
      Some (value, quote, source_slice t loc)
  | _ -> None

let url_opt t = take_token_if (function Token.Url s -> Some s | _ -> None) t

let ascii_delim = function
  | s -> if String.length s = 1 then Some s.[0] else None

let delim_opt t =
  take_token_if (function Token.Delim s -> ascii_delim s | _ -> None) t

(* Predicate helpers that inspect the head component directly without going
   through [peek], so they do not allocate a [Some hd] option per call. *)
let peek_delim t =
  drop_ws t;
  match t.cvs with
  | Component.Preserved { kind = Token.Delim s; _ } :: _ -> ascii_delim s
  | _ -> None

let peek_comma t =
  drop_ws t;
  match t.cvs with
  | Component.Preserved { kind = Token.Comma; _ } :: _ -> true
  | _ -> false

let peek_semicolon t =
  drop_ws t;
  match t.cvs with
  | Component.Preserved { kind = Token.Semicolon; _ } :: _ -> true
  | _ -> false

let peek_colon t =
  drop_ws t;
  match t.cvs with
  | Component.Preserved { kind = Token.Colon; _ } :: _ -> true
  | _ -> false

let peek_ident t =
  match peek t with
  | Some (Component.Preserved { kind = Token.Ident s; _ }) -> Some s
  | _ -> None

let peek_hash t =
  match peek t with
  | Some (Component.Preserved { kind = Token.Hash { value; _ }; _ }) ->
      Some value
  | _ -> None

let peek_at_keyword t =
  match peek t with
  | Some (Component.Preserved { kind = Token.At_keyword s; _ }) -> Some s
  | _ -> None

let peek_block t =
  match peek t with
  | Some (Component.Block b) -> Some b.node.opening
  | _ -> None

let at_keyword_opt t =
  match peek t with
  | Some (Component.Preserved { kind = Token.At_keyword s; _ }) ->
      skip t;
      Some s
  | _ -> None

let expect_at_keyword name t =
  match at_keyword_opt t with
  | Some s when s = name -> ()
  | _ -> err_expected t ("@" ^ name)

let drain_until_block t =
  (* Used for at-rule / qualified-rule preludes: drain until the curly block
     body, not any block (selectors can contain [Square] blocks for attribute
     matchers). Whitespace is preserved so the selector parser can recognise the
     descendant combinator. *)
  let rec loop acc =
    match peek_raw t with
    | None -> List.rev acc
    | Some (Component.Block { node = { opening = Token.Curly; _ }; _ }) ->
        List.rev acc
    | Some (Component.Preserved { kind = Token.Semicolon; _ }) -> List.rev acc
    | Some cv ->
        ignore (next_raw t : Component.t option);
        loop (cv :: acc)
  in
  loop []

let drain_until_block_as_string ?(trim = false) t =
  string_of_components ~trim (drain_until_block t)

let drain_until_raw stop t =
  let rec loop acc =
    match peek_raw t with
    | None -> List.rev acc
    | Some cv when stop cv -> List.rev acc
    | Some cv ->
        ignore (next_raw t : Component.t option);
        loop (cv :: acc)
  in
  loop []

let is_semicolon_cv = function
  | Component.Preserved { kind = Token.Semicolon; _ } -> true
  | _ -> false

let is_bang_cv = function
  | Component.Preserved { kind = Token.Delim "!"; _ } -> true
  | _ -> false

let consume_until_semicolon ?(trim = false) t =
  string_of_components ~trim (drain_until_raw is_semicolon_cv t)

let consume_to_decl_end ?(trim = false) t =
  string_of_components ~trim
    (drain_until_raw (fun cv -> is_semicolon_cv cv || is_bang_cv cv) t)

let drain_to_decl_end t =
  drain_until_raw (fun cv -> is_semicolon_cv cv || is_bang_cv cv) t

let is_slash_cv = function
  | Component.Preserved { kind = Token.Delim "/"; _ } -> true
  | _ -> false

let consume_to_slash_or_semicolon ?(trim = false) t =
  string_of_components ~trim
    (drain_until_raw (fun cv -> is_slash_cv cv || is_semicolon_cv cv) t)

(** {1 Token-shape helpers - raising variants} *)

let ident ?keep_case:_ t =
  match ident_opt t with Some s -> s | None -> err_expected t "identifier"

let number ?(allow_negative = true) t =
  match number_opt t with
  | Some n ->
      if (not allow_negative) && n < 0.0 then err_invalid t "negative number"
      else n
  | None -> err_expected t "number"

let int t =
  match integer_opt t with Some n -> n | None -> err_expected t "integer"

let hex t =
  let hex_string_opt =
    take_token_if
      (function
        | Token.Hash { value; _ } when is_hex_string value -> Some value
        | Token.Ident s when is_hex_string s -> Some s
        | Token.Number_tok n when is_hex_string n.repr -> Some n.repr
        | Token.Dimension { number; unit_ }
          when is_hex_string (number.repr ^ unit_) ->
            Some (number.repr ^ unit_)
        | _ -> None)
      t
  in
  match hex_string_opt with
  | Some s -> (
      try int_of_string ("0x" ^ s)
      with Failure _ -> err_invalid t ("hex: " ^ s))
  | None -> err_expected t "hex token"

let string ?(trim = false) t =
  match string_opt t with
  | Some s -> if trim then String.trim s else s
  | None -> err_expected t "string"

(* Zero-width loc pointing at the block/function closing delimiter. Used as the
   [eof_loc] for sub-cursors so errors raised at end-of-block point at the
   closer, not at the end of the whole input. *)
let closer_loc (node_loc : Loc.t) =
  Loc.v ~start_pos:node_loc.end_pos ~end_pos:node_loc.end_pos

let func_sub (fn : Component.func Component.node) t =
  sub ~eof_loc:(closer_loc fn.loc) t fn.node.arguments

let url_from_func t (fn : Component.func Component.node) =
  if not fn.node.terminated then err_expected t "terminated url";
  skip t;
  let inner = func_sub fn t in
  match string_opt inner with
  | Some s ->
      ws inner;
      if is_done inner then s else err_expected t "url argument"
  | None ->
      let components = remaining inner in
      let raw = Parser.to_string_minified components in
      if raw = "" then err_expected t "url argument" else raw

let url t =
  match peek t with
  | Some (Component.Preserved { kind = Token.Url ""; loc }) ->
      if loc.end_pos - loc.start_pos < 5 then err_expected t "url argument";
      skip t;
      ""
  | Some (Component.Preserved { kind = Token.Url s; _ }) ->
      skip t;
      s
  | Some (Component.Func ({ node = { name = "url"; _ }; _ } as fn)) ->
      url_from_func t fn
  | _ -> err_expected t "url"

let pct ?(clamp = false) t =
  match percentage_opt t with
  | Some v -> if clamp then Float.min 100.0 (Float.max 0.0 v) else v
  | None -> err_expected t "percentage"

let number_with_unit t =
  match peek t with
  | Some (Component.Preserved { kind = Token.Dimension { number; unit_ }; _ })
    ->
      skip t;
      (number.value, Some unit_)
  | Some (Component.Preserved { kind = Token.Percentage { value; _ }; _ }) ->
      skip t;
      (value, Some "%")
  | Some (Component.Preserved { kind = Token.Number_tok { value; _ }; _ }) ->
      skip t;
      (value, None)
  | _ -> err_expected t "number with unit"

let number_repr_with_unit t =
  match peek t with
  | Some (Component.Preserved { kind = Token.Dimension { number; unit_ }; _ })
    ->
      skip t;
      (number.value, number.repr, Some unit_)
  | Some (Component.Preserved { kind = Token.Percentage number; _ }) ->
      skip t;
      (number.value, number.repr, Some "%")
  | Some (Component.Preserved { kind = Token.Number_tok number; _ }) ->
      skip t;
      (number.value, number.repr, None)
  | _ -> err_expected t "number with unit"

let bool t =
  match ident t with
  | "true" -> true
  | "false" -> false
  | s -> err_invalid t ("boolean: " ^ s)

(** {1 Delim helpers} *)

let bool_token (k : Token.kind) t =
  drop_ws t;
  match t.cvs with
  | Component.Preserved tok :: _ when tok.kind = k ->
      let _ = next t in
      true
  | _ -> false

let colon t = bool_token Token.Colon t
let semicolon t = bool_token Token.Semicolon t
let comma_opt t = bool_token Token.Comma t
let comma t = if not (comma_opt t) then err_expected t "','"

let slash_opt t =
  match peek t with
  | Some (Component.Preserved { kind = Token.Delim "/"; _ }) ->
      skip t;
      true
  | _ -> false

let slash t = if not (slash_opt t) then err_expected t "'/'"

let consume_if c t =
  match peek t with
  | Some (Component.Preserved { kind = Token.Ident s; _ })
    when String.length s = 1 && s.[0] = c ->
      skip t;
      true
  | Some (Component.Preserved { kind = Token.Colon; _ }) when c = ':' ->
      skip t;
      true
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) when c = ';' ->
      skip t;
      true
  | Some (Component.Preserved { kind = Token.Comma; _ }) when c = ',' ->
      skip t;
      true
  | Some (Component.Preserved { kind = Token.Delim d; _ })
    when d = String.make 1 c ->
      skip t;
      true
  | _ -> false

let try_kind k t =
  match peek t with
  | Some (Component.Preserved tok) when tok.kind = k ->
      let _ = next t in
      true
  | _ -> false

let looking_at_ident name t =
  match peek t with
  | Some (Component.Preserved { kind = Token.Ident s; _ }) -> s = name
  | _ -> false

let looking_at_func name t =
  drop_ws t;
  match t.cvs with
  | Component.Func { node = { name = n; _ }; _ } :: _ -> n = name
  | _ -> false

let looking_at_calc t =
  looking_at_func "calc" t || looking_at_func "-webkit-calc" t

let looking_at t s =
  (* [s] can be an ident ("auto"), a function prefix ("var("), or a short string
     starting with a delim ("--", ")"). *)
  let len = String.length s in
  if len = 0 then true
  else if s.[len - 1] = '(' then looking_at_func (String.sub s 0 (len - 1)) t
  else if looking_at_ident s t then true
  else
    match peek t with
    | Some (Component.Preserved { kind = Token.Ident ident; _ }) ->
        String.starts_with ~prefix:s ident
    | Some (Component.Preserved { kind = Token.At_keyword name; _ }) ->
        String.starts_with ~prefix:s ("@" ^ name)
    | Some (Component.Preserved { kind = Token.Hash { value; _ }; _ }) ->
        String.starts_with ~prefix:s ("#" ^ value)
    | Some (Component.Preserved { kind = Token.Url _; _ }) ->
        String.starts_with ~prefix:s "url("
    | _ -> (
        match peek_raw t with
        | Some (Component.Preserved { kind = Token.Colon; _ }) ->
            len = 1 && s.[0] = ':'
        | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
            len = 1 && s.[0] = ';'
        | Some (Component.Preserved { kind = Token.Comma; _ }) ->
            len = 1 && s.[0] = ','
        | Some (Component.Preserved { kind = Token.Delim c; _ }) -> s = c
        | _ -> false)

let try_kind_pair k1 k2 t =
  let snap = save t in
  match peek t with
  | Some (Component.Preserved tok) when tok.kind = k1 -> (
      let _ = next t in
      match peek_raw t with
      | Some (Component.Preserved tok2) when tok2.kind = k2 ->
          let _ = next_raw t in
          true
      | _ ->
          restore t snap;
          false)
  | _ -> false

(** {1 Expectations} *)

let expect c t =
  if not (consume_if c t) then
    err_expected t (String.concat "" [ "'"; String.make 1 c; "'" ])

let expect_string name t =
  match ident_opt t with Some s when s = name -> () | _ -> err_expected t name

let expect_eof t = if not (is_done t) then err t "unexpected token"

(** {1 Group / function helpers} *)

let take_block_if pred t : Component.block Component.node option =
  match peek t with
  | Some (Component.Block b) when pred b.node.opening ->
      let _ = next t in
      Some b
  | _ -> None

let parens f t =
  match take_block_if (fun b -> b = Token.Paren) t with
  | Some b -> f (sub ~eof_loc:(closer_loc b.loc) t b.node.value)
  | None -> err_expected t "'('"

let brackets f t =
  match take_block_if (fun b -> b = Token.Square) t with
  | Some b -> f (sub ~eof_loc:(closer_loc b.loc) t b.node.value)
  | None -> err_expected t "'['"

let braces f t =
  match take_block_if (fun b -> b = Token.Curly) t with
  | Some b -> f (sub ~eof_loc:(closer_loc b.loc) t b.node.value)
  | None -> err_expected t "'{'"

let function_call name f t =
  match peek t with
  | Some (Component.Func fn) when fn.node.name = name ->
      let _ = next t in
      Some (f (sub ~eof_loc:(closer_loc fn.loc) t fn.node.arguments))
  | _ -> None

let any_function_call f t =
  match peek t with
  | Some (Component.Func fn) ->
      let _ = next t in
      Some
        (f fn.node.name (sub ~eof_loc:(closer_loc fn.loc) t fn.node.arguments))
  | _ -> None

let call name t f =
  match peek t with
  | Some (Component.Func fn)
    when String.lowercase_ascii_preserve fn.node.name
         = String.lowercase_ascii_preserve name ->
      let _ = next t in
      let arg = sub ~eof_loc:(closer_loc fn.loc) t fn.node.arguments in
      let arg = { arg with depth = t.depth + 1 } in
      if arg.depth > max_nesting_depth then err arg "nesting too deep";
      f arg
  | _ -> err_expected t (name ^ "(")

(** {1 Enums} *)

let try_enum table t =
  match peek t with
  | Some (Component.Preserved { kind = Token.Ident s; _ }) -> (
      match List.assoc_opt s table with
      | Some v ->
          let _ = next t in
          Some v
      | None -> None)
  | _ -> None

let enum ?default label table t =
  ws t;
  match peek t with
  | Some (Component.Preserved { kind = Token.Ident s; _ }) -> (
      (* CSS idents are case-insensitive (Syntax section 3.3). *)
      match List.assoc_opt (String.lowercase_ascii_preserve s) table with
      | Some v ->
          let _ = next t in
          v
      | None -> (
          match default with
          | Some f -> f t
          | None -> err t ("unknown " ^ label ^ ": " ^ s)))
  | _ -> ( match default with Some f -> f t | None -> err_expected t label)

let enum_calls ?default table t =
  ws t;
  match peek t with
  | Some (Component.Func { node = { name; _ }; _ }) -> (
      match List.assoc_opt (String.lowercase_ascii_preserve name) table with
      | Some f -> f t
      | None -> (
          match default with
          | Some f -> f t
          | None -> err t ("unknown function: " ^ name)))
  | _ -> (
      match default with
      | Some f -> f t
      | None -> err_expected t "function call")

let enum_or_var ?default label idents ~var t =
  ws t;
  match peek t with
  | Some (Component.Func { node = { name; _ }; _ })
    when String.lowercase_ascii_preserve name = "var" ->
      var t
  | _ -> enum ?default label idents t

let enum_or_calls ?default label idents ?(calls = []) t =
  ws t;
  match peek t with
  | Some (Component.Preserved { kind = Token.Ident s; _ }) -> (
      (* CSS idents are case-insensitive (Syntax section 3.3). *)
      match List.assoc_opt (String.lowercase_ascii_preserve s) idents with
      | Some v ->
          let _ = next t in
          v
      | None -> (
          match default with
          | Some f -> f t
          | None -> err t ("unknown " ^ label ^ ": " ^ s)))
  | Some (Component.Func { node = { name; _ }; _ }) -> (
      match List.assoc_opt (String.lowercase_ascii_preserve name) calls with
      | Some f -> f t
      | None -> (
          match default with
          | Some f -> f t
          | None -> err t ("unknown " ^ label ^ " function: " ^ name)))
  | _ -> ( match default with Some f -> f t | None -> err_expected t label)

(** {1 Higher-order combinators} *)

let option p t =
  let snap = save t in
  match p t with
  | v -> Some v
  | exception Parse_error _ ->
      restore t snap;
      None

let rec one_of ps t =
  match ps with
  | [] -> err_expected t "one of"
  | p :: rest -> (
      let snap = save t in
      match p t with
      | v -> v
      | exception Parse_error _ ->
          restore t snap;
          one_of rest t)

let rec many p t =
  match option p t with
  | None -> ([], None)
  | Some v ->
      let rest, err = many p t in
      (v :: rest, err)

let pair ?sep p1 p2 t =
  let a = p1 t in
  (match sep with None -> () | Some s -> s t);
  let b = p2 t in
  (a, b)

let triple ?sep p1 p2 p3 t =
  let a = p1 t in
  (match sep with None -> () | Some s -> s t);
  let b = p2 t in
  (match sep with None -> () | Some s -> s t);
  let c = p3 t in
  (a, b, c)

let fold_many p ~init ~f t =
  let rec loop acc =
    match option p t with None -> (acc, None) | Some v -> loop (f acc v)
  in
  loop init

let list_consume_separator sep t =
  match sep with
  | None -> true
  | Some s -> (
      let snap = save t in
      match s t with
      | () -> true
      | exception Parse_error _ ->
          restore t snap;
          false)

type 'a collect_step = Done of 'a list | Continue of 'a list * int

let list_collect_step sep item t acc n max =
  if n >= max then Done (List.rev acc)
  else
    let snap = save t in
    match option item t with
    | None -> Done (List.rev acc)
    | Some v ->
        if t.cvs == snap then err t "list item consumed no input";
        let acc = v :: acc in
        if n + 1 >= max then Done (List.rev acc)
        else if list_consume_separator sep t then Continue (acc, n + 1)
        else Done (List.rev acc)

let rec list_collect sep item t acc n max =
  match list_collect_step sep item t acc n max with
  | Done items -> items
  | Continue (acc, n) -> list_collect sep item t acc n max

let list ?sep ?(at_least = 0) ?at_most item t =
  let max = Option.value at_most ~default:max_int in
  let items = list_collect sep item t [] 0 max in
  let len = List.length items in
  if len < at_least then
    err_expected t
      (String.concat ""
         [
           "at least ";
           string_of_int at_least;
           " items (got ";
           string_of_int len;
           ")";
         ])
  else items

let try_parse_err p t =
  let snap = save t in
  match p t with
  | v -> Ok v
  | exception Parse_error e ->
      restore t snap;
      Error (Error.to_string e)

let try_parse_full_err p t =
  let snap = save t in
  match p t with
  | v ->
      if is_done t then Ok v
      else (
        restore t snap;
        Error "trailing tokens")
  | exception Parse_error e ->
      restore t snap;
      Error (Error.to_string e)
