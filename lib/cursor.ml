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

let push_warning t ~recovery e =
  t.warnings := Error.with_recovery recovery e :: !(t.warnings)

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

(* [source] must be the post-preprocessing buffer (CSS Syntax 3 (ED) sec. 3.3),
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

let consume_remaining t =
  let cvs = t.cvs in
  t.cvs <- [];
  cvs

let consume_remaining_as_string ?(trim = false) t =
  string_of_components ~trim (consume_remaining t)

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

let component_head_shape : Component.t -> head_shape = function
  | Component.Preserved { kind; _ } -> (
      match kind with
      | Token.Semicolon -> `Semicolon
      | Token.Colon -> `Colon
      | Token.Comma -> `Comma
      | Token.Delim "!" -> `Bang
      | Token.Ident _ -> `Ident
      | _ -> `Other)
  | Component.Block { node = { opening; _ }; _ } -> (
      match opening with
      | Token.Curly -> `Curly_block
      | Token.Paren -> `Paren_block
      | Token.Square -> `Square_block)
  | Component.Func _ -> `Func

let peek_head_shape t : head_shape =
  drop_ws t;
  match t.cvs with [] -> `Eof | cv :: _ -> component_head_shape cv

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

(* The source range covered by the components consumed since [snap]. A recovery
   point holds the snapshot from before the item it gave up on, so this names
   the whole construct it then skipped rather than the point the read failed at.
   Whitespace at either end sits outside the construct and is left out. *)
let consumed_span t snap =
  let rec span acc cvs =
    if cvs == t.cvs then acc
    else
      match cvs with
      | [] -> None
      | hd :: tl when is_ws_cv hd -> span acc tl
      | hd :: tl ->
          let loc = Component.source_loc hd in
          let acc =
            match acc with None -> Some loc | Some l -> Some (Loc.union l loc)
          in
          span acc tl
  in
  span None snap

let dropped_since t snap construct =
  Error.Recovery.dropped ?source:t.source ?loc:(consumed_span t snap) construct

(** {1 Errors} *)

exception Parse_error = Error.Parse_error

let sort = Sort.Component

let raise_sort t sort kind loc =
  let source = match (t.meta, t.source) with `Full, s -> s | _ -> None in
  Error.fail (Error.v ?source ~loc ~sort kind)

let raise_ t kind loc = raise_sort t sort kind loc

(* [loc] is for a reader that consumed the offending token before deciding it
   was bad: [position t] has moved on to whatever follows by then, and at the
   end of a value that is the terminator. Such a reader passes the span it
   read. *)
let err ?loc ?got t msg =
  let loc = match loc with Some loc -> loc | None -> position t in
  match got with
  | Some g ->
      raise_ t
        (Error.Bad_value { property = ""; reason = msg ^ ": got " ^ g })
        loc
  | None -> raise_ t (Error.Bad_value { property = ""; reason = msg }) loc

let err_invalid ?loc t msg = err ?loc t ("invalid: " ^ msg)
let err_eof t = raise_ t (Error.Unterminated sort) (position t)
let err_expected ?loc t what = err ?loc t ("expected " ^ what)

let err_expected_but_eof t what =
  raise_ t (Error.Missing_token what) (position t)

let err_unexpected t = err t "unexpected token"

let condition_error t ~at_rule reason =
  let source = match (t.meta, t.source) with `Full, s -> s | _ -> None in
  Error.v ?source ~loc:(position t) ~sort:Sort.At_rule
    (Error.Bad_condition { at_rule; reason })

let err_condition t ~at_rule reason =
  Error.fail (condition_error t ~at_rule reason)

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
    (function Token.Number_tok number -> Token.integer_opt number | _ -> None)
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
  | Some s when String.lowercase_ascii_preserve s = name -> ()
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

let consume_until_semicolon ?(trim = false) t =
  string_of_components ~trim (drain_until_raw is_semicolon_cv t)

let rec skip_past_semicolon t =
  match next_raw t with
  | None -> ()
  | Some cv -> if not (is_semicolon_cv cv) then skip_past_semicolon t

(* CSS Syntax 3 (ED) sec. 5.5.6 reads a declaration's value with
   [<semicolon-token>] as the stop token, then removes a trailing [!]
   [important] pair from that value and sets the declaration's important flag
   instead. Both are therefore the declaration consumer's to read, never the
   property grammar's, and [head_shape] is what says which is which. *)
let ends_declaration_value cv =
  match component_head_shape cv with `Semicolon | `Bang -> true | _ -> false

let consume_to_decl_end ?(trim = false) t =
  string_of_components ~trim (drain_until_raw ends_declaration_value t)

let drain_to_decl_end t = drain_until_raw ends_declaration_value t

let decl_value_loc t =
  match
    List.filter (fun cv -> not (is_ws_cv cv)) (lookahead drain_to_decl_end t)
  with
  | [] -> position t
  | first :: rest ->
      List.fold_left
        (fun acc cv -> Loc.union acc (Component.source_loc cv))
        (Component.source_loc first)
        rest

let declaration_value t =
  let cvs = drain_to_decl_end t in
  let eof_loc = Option.map Component.source_loc (peek_raw t) in
  sub ?eof_loc t cvs

let is_slash_cv = function
  | Component.Preserved { kind = Token.Delim "/"; _ } -> true
  | _ -> false

let consume_to_slash_or_semicolon ?(trim = false) t =
  string_of_components ~trim
    (drain_until_raw (fun cv -> is_slash_cv cv || is_semicolon_cv cv) t)

(** {1 Token-shape helpers - raising variants} *)

(* CSS Values 4 sec. 4.1 reads a keyword ASCII case-insensitively; sec. 4.2
   keeps an author-defined identifier case-sensitive. [keep_case] is which of
   the two the caller is reading. *)
let ident ?(keep_case = true) t =
  match ident_opt t with
  | Some s -> if keep_case then s else String.lowercase_ascii_preserve s
  | None -> err_expected t "identifier"

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
  (* CSS Syntax 3 (ED) sec. 4.3.6 ends a url token at EOF as it does at [)], the
     missing closer a parse error and nothing more, so the shorter span of
     [url(] is still a url token holding the empty string. *)
  | Some (Component.Preserved { kind = Token.Url s; _ }) ->
      skip t;
      s
  | Some (Component.Func ({ node = { name; _ }; _ } as fn))
    when String.lowercase_ascii_preserve name = "url" ->
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
  | Component.Preserved tok :: _ when Token.equal_kind tok.kind k ->
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
  | Some (Component.Preserved tok) when Token.equal_kind tok.kind k ->
      let _ = next t in
      true
  | _ -> false

let looking_at_ident name t =
  match peek t with
  | Some (Component.Preserved { kind = Token.Ident s; _ }) ->
      String.lowercase_ascii_preserve s = name
  | _ -> false

let try_ident name t =
  if looking_at_ident name t then
    let _ = next t in
    true
  else false

let looking_at_func name t =
  drop_ws t;
  match t.cvs with
  | Component.Func { node = { name = n; _ }; _ } :: _ ->
      String.lowercase_ascii_preserve n = name
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
        String.starts_with ~prefix:s (String.lowercase_ascii_preserve ident)
    | Some (Component.Preserved { kind = Token.At_keyword name; _ }) ->
        String.starts_with ~prefix:s
          (String.concat "" [ "@"; String.lowercase_ascii_preserve name ])
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
  | Some (Component.Preserved tok) when Token.equal_kind tok.kind k1 -> (
      let _ = next t in
      match peek_raw t with
      | Some (Component.Preserved tok2) when Token.equal_kind tok2.kind k2 ->
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
  let loc = position t in
  match ident_opt t with
  | Some s when String.lowercase_ascii_preserve s = name -> ()
  | _ -> err_expected ~loc t name

let expect_eof t = if not (is_done t) then err t "unexpected token"

(** {1 Group / function helpers} *)

let take_block_if pred t : Component.block Component.node option =
  match peek t with
  | Some (Component.Block b) when pred b.node.opening ->
      let _ = next t in
      Some b
  | _ -> None

(* A block's grammar ends at its closer, so a reader that stops part-way through
   the contents has met an invalid value, not a shorter one it may answer with:
   CSS Syntax 3 (ED) sec. 5.4.1 matches a grammar against the whole
   component-value list the block holds, or returns failure. [expect_eof] skips
   leading whitespace itself. *)
let whole_block f inner =
  let v = f inner in
  expect_eof inner;
  v

let parens f t =
  match take_block_if (fun b -> b = Token.Paren) t with
  | Some b -> whole_block f (sub ~eof_loc:(closer_loc b.loc) t b.node.value)
  | None -> err_expected t "'('"

let brackets f t =
  match take_block_if (fun b -> b = Token.Square) t with
  | Some b -> whole_block f (sub ~eof_loc:(closer_loc b.loc) t b.node.value)
  | None -> err_expected t "'['"

let braces f t =
  match take_block_if (fun b -> b = Token.Curly) t with
  | Some b -> whole_block f (sub ~eof_loc:(closer_loc b.loc) t b.node.value)
  | None -> err_expected t "'{'"

let function_call name f t =
  match peek t with
  | Some (Component.Func fn)
    when String.lowercase_ascii_preserve fn.node.name = name ->
      let _ = next t in
      Some
        (whole_block f (sub ~eof_loc:(closer_loc fn.loc) t fn.node.arguments))
  | _ -> None

let any_function_call f t =
  match peek t with
  | Some (Component.Func fn) ->
      let _ = next t in
      let inner = sub ~eof_loc:(closer_loc fn.loc) t fn.node.arguments in
      Some (whole_block (f fn.node.name) inner)
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
      whole_block f arg
  | _ -> err_expected t (name ^ "(")

(** {1 Enums} *)

let try_enum table t =
  match peek t with
  | Some (Component.Preserved { kind = Token.Ident s; _ }) -> (
      match List.assoc_opt (String.lowercase_ascii_preserve s) table with
      | Some v ->
          let _ = next t in
          Some v
      | None -> None)
  | _ -> None

let enum ?default label table t =
  ws t;
  match peek t with
  | Some (Component.Preserved { kind = Token.Ident s; _ }) -> (
      (* CSS idents are case-insensitive (CSS Values 4 section 4.1). *)
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

(* CSS Cascade 5 sec. 6 gives the CSS-wide keywords the whole declaration value,
   and a [var()] standing for all of it reads the same way. A reader that owns
   the whole value therefore takes the [var] branch only when the reference
   spans that value: one with components after it is an operand of the grammar
   [default] reads, so the value goes there instead. *)
let enum_or_whole_value_var ?default label idents ~var t =
  ws t;
  match peek t with
  | Some (Component.Func { node = { name; _ }; _ })
    when String.lowercase_ascii_preserve name = "var" -> (
      let snap = save t in
      let read_default () =
        restore t snap;
        enum ?default label idents t
      in
      match var t with
      | v ->
          ws t;
          if is_done t then v else read_default ()
      | exception Parse_error _ -> read_default ())
  | _ -> enum ?default label idents t

let enum_or_calls ?default label idents ?(calls = []) t =
  ws t;
  match peek t with
  | Some (Component.Preserved { kind = Token.Ident s; _ }) -> (
      (* CSS idents are case-insensitive (CSS Values 4 section 4.1). *)
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

(* The alternatives are anonymous readers, so an exhausted list has no forms to
   name: report that none matched rather than open a list and leave it empty. *)
let rec one_of ps t =
  match ps with
  | [] -> err t "no accepted form"
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
  atomic t (fun () ->
      let a = p1 t in
      (match sep with None -> () | Some s -> s t);
      let b = p2 t in
      (a, b))

let triple ?sep p1 p2 p3 t =
  atomic t (fun () ->
      let a = p1 t in
      (match sep with None -> () | Some s -> s t);
      let b = p2 t in
      (match sep with None -> () | Some s -> s t);
      let c = p3 t in
      (a, b, c))

let fold_many p ~init ~f t =
  let rec loop acc =
    match option p t with None -> (acc, None) | Some v -> loop (f acc v)
  in
  loop init

(* One wording for an [~at_least] shortfall, shared with [Reader.list]. *)
let at_least_shortfall ~at_least ~got =
  String.concat ""
    [
      "at least ";
      string_of_int at_least;
      " items (got ";
      string_of_int got;
      ")";
    ]

type 'a collect_step = Done of 'a list | Continue of 'a list * int

(* A separator only commits once the item after it parses: with [n > 0], [snap]
   covers both, so a failing [sep] or a failing item after a successful [sep]
   restores to before the separator instead of leaving it consumed. CSS Values 4
   sec. 5.7.3 makes a [#] list's trailing comma invalid, and this is the one
   place that comma is read. *)
let list_collect_step sep item t acc n max =
  if n >= max then Done (List.rev acc)
  else
    let snap = save t in
    let sep_ok =
      n = 0
      ||
      match sep with
      | None -> true
      | Some s -> (
          match s t with () -> true | exception Parse_error _ -> false)
    in
    if not sep_ok then (
      restore t snap;
      Done (List.rev acc))
    else
      let item_snap = save t in
      match option item t with
      | None ->
          restore t snap;
          Done (List.rev acc)
      | Some v ->
          if t.cvs == item_snap then err t "list item consumed no input";
          Continue (v :: acc, n + 1)

let rec list_collect sep item t acc n max =
  match list_collect_step sep item t acc n max with
  | Done items -> items
  | Continue (acc, n) -> list_collect sep item t acc n max

let list ?sep ?(at_least = 1) ?at_most item t =
  let max = Option.value at_most ~default:max_int in
  let items = list_collect sep item t [] 0 max in
  let len = List.length items in
  if len < at_least then err_expected t (at_least_shortfall ~at_least ~got:len)
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
