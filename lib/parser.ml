(** Stage 3 stream: Token.t -> Component.t.

    Ports the "consume a ..." algorithms from
    https://www.w3.org/TR/css-syntax-3/#parser-algorithms onto a {!Lexer.t}
    token stream, producing the IR defined in {!Component}. *)

open Component

type t = { lexer : Lexer.t; mutable lookback : Component.t option }

let of_lexer lexer = { lexer; lookback = None }
let of_reader r = of_lexer (Lexer.of_reader r)
let of_string s = of_reader (Reader.of_string s)

(** {1 section 5.3 algorithms operating on a {!Lexer.t}} *)

(* Consume a component value. The opening token [tok] has already been read. *)
let rec consume_component_value_from lexer tok : Component.t =
  match tok.Token.kind with
  | Token.Open bracket ->
      let block = consume_simple_block lexer bracket ~start_loc:tok.loc in
      Block block
  | Token.Function name ->
      let func = consume_function lexer ~name ~start_loc:tok.loc in
      Func func
  | _ -> Preserved tok

and consume_component_value lexer : Component.t =
  let tok = Lexer.next lexer in
  consume_component_value_from lexer tok

and consume_simple_block lexer opening ~start_loc :
    Component.block Component.node =
  let rec loop acc =
    let tok = Lexer.next lexer in
    match tok.Token.kind with
    | Token.Eof ->
        let loc = Loc.union start_loc tok.loc in
        { node = { opening; value = List.rev acc }; loc }
    | Token.Close b when b = opening ->
        let loc = Loc.union start_loc tok.loc in
        { node = { opening; value = List.rev acc }; loc }
    | _ ->
        let cv = consume_component_value_from lexer tok in
        loop (cv :: acc)
  in
  loop []

and consume_function lexer ~name ~start_loc : Component.func Component.node =
  let rec loop acc =
    let tok = Lexer.next lexer in
    match tok.Token.kind with
    | Token.Eof | Token.Close Paren ->
        let loc = Loc.union start_loc tok.loc in
        { node = { name; arguments = List.rev acc }; loc }
    | _ ->
        let cv = consume_component_value_from lexer tok in
        loop (cv :: acc)
  in
  loop []

(** {1 Stream API (uniform with Reader/Lexer)} *)

let next t =
  match t.lookback with
  | Some cv ->
      t.lookback <- None;
      cv
  | None -> consume_component_value t.lexer

let peek t =
  match t.lookback with
  | Some cv -> cv
  | None ->
      let cv = consume_component_value t.lexer in
      t.lookback <- Some cv;
      cv

let reconsume t cv =
  assert (t.lookback = None);
  t.lookback <- Some cv

(** {1 Reserialization} *)

let hex_digit n =
  if n < 10 then Char.chr (n + Char.code '0')
  else Char.chr (n - 10 + Char.code 'A')

(* Hex-escape a control byte as "\HH " per CSS Syntax section 9.1. *)
let add_hex_escape buf c =
  let code = Char.code c in
  Buffer.add_char buf '\\';
  if code >= 0x10 then Buffer.add_char buf (hex_digit (code lsr 4));
  Buffer.add_char buf (hex_digit (code land 0xF));
  Buffer.add_char buf ' '

let is_ident_continue_ascii c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c = '-' || c = '_'

let add_hex_escape_cp buf cp =
  Buffer.add_char buf '\\';
  let rec emit n acc =
    if n = 0 && acc = [] then Buffer.add_char buf '0'
    else if n = 0 then List.iter (Buffer.add_char buf) acc
    else emit (n / 16) (hex_digit (n mod 16) :: acc)
  in
  emit cp [];
  Buffer.add_char buf ' '

let escape_ident s =
  let n = String.length s in
  let buf = Buffer.create n in
  let starts_with_digit = n > 0 && s.[0] >= '0' && s.[0] <= '9' in
  let starts_dash_digit =
    n >= 2 && s.[0] = '-' && s.[1] >= '0' && s.[1] <= '9'
  in
  let folder () i = function
    | `Uchar u ->
        let cp = Uchar.to_int u in
        if (i = 0 && starts_with_digit) || (i = 1 && starts_dash_digit) then
          add_hex_escape_cp buf cp
        else if cp < 0x20 || cp = 0x7F then add_hex_escape_cp buf cp
        else if cp < 0x80 then
          if is_ident_continue_ascii (Char.chr cp) then
            Buffer.add_char buf (Char.chr cp)
          else (
            Buffer.add_char buf '\\';
            Buffer.add_char buf (Char.chr cp))
        else if Lexer.is_non_ascii_ident_cp cp then Uutf.Buffer.add_utf_8 buf u
        else add_hex_escape_cp buf cp
    | `Malformed bs -> Buffer.add_string buf bs
  in
  Uutf.String.fold_utf_8 folder () s;
  Buffer.contents buf

let escape_string ~quote s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf quote;
  String.iter
    (fun c ->
      let code = Char.code c in
      if c = quote || c = '\\' then (
        Buffer.add_char buf '\\';
        Buffer.add_char buf c)
      else if code < 0x20 || code = 0x7F then add_hex_escape buf c
      else Buffer.add_char buf c)
    s;
  Buffer.add_char buf quote;
  Buffer.contents buf

let token_kind_to_string : Token.kind -> string = function
  | Token.Ident s -> escape_ident s
  | Token.Function s -> escape_ident s ^ "("
  | Token.At_keyword s -> "@" ^ escape_ident s
  | Token.Hash { value; _ } -> "#" ^ escape_ident value
  | Token.String { value; quote } -> escape_string ~quote value
  | Token.Bad_string -> ""
  | Token.Url s ->
      let buf = Buffer.create (String.length s + 5) in
      Buffer.add_string buf "url(";
      String.iter
        (fun c ->
          let code = Char.code c in
          if code < 0x20 || code = 0x7F then add_hex_escape buf c
          else if
            c = '"' || c = '\'' || c = '(' || c = ')' || c = '\\' || c = ' '
          then (
            Buffer.add_char buf '\\';
            Buffer.add_char buf c)
          else Buffer.add_char buf c)
        s;
      Buffer.add_char buf ')';
      Buffer.contents buf
  | Token.Bad_url -> ""
  | Token.Delim "\\" -> "\\\n"
  | Token.Delim s -> s
  | Token.Number_tok { repr; _ } -> repr
  | Token.Percentage { repr; _ } -> repr ^ "%"
  | Token.Dimension { number; unit_ } -> number.repr ^ escape_ident unit_
  | Token.Whitespace -> " "
  | Token.Unicode_range { start_value; end_value } ->
      let hex_digits = "0123456789ABCDEF" in
      let to_hex n =
        let buf = Buffer.create 6 in
        let rec emit n =
          if n = 0 then ()
          else (
            emit (n / 16);
            Buffer.add_char buf hex_digits.[n mod 16])
        in
        if n = 0 then Buffer.add_char buf '0' else emit n;
        Buffer.contents buf
      in
      if start_value = end_value then
        String.concat "" [ "U+"; to_hex start_value ]
      else String.concat "" [ "U+"; to_hex start_value; "-"; to_hex end_value ]
  | Token.Cdo -> "<!--"
  | Token.Cdc -> "-->"
  | Token.Colon -> ":"
  | Token.Semicolon -> ";"
  | Token.Comma -> ","
  | Token.Open Square -> "["
  | Token.Close Square -> "]"
  | Token.Open Paren -> "("
  | Token.Close Paren -> ")"
  | Token.Open Curly -> "{"
  | Token.Close Curly -> "}"
  | Token.Eof -> ""

let opening_char : Token.bracket -> char = function
  | Curly -> '{'
  | Paren -> '('
  | Square -> '['

let closing_char : Token.bracket -> char = function
  | Curly -> '}'
  | Paren -> ')'
  | Square -> ']'

let is_backslash_delim = function
  | Preserved { kind = Token.Delim "\\"; _ } -> true
  | _ -> false

let is_whitespace = function
  | Preserved { kind = Token.Whitespace; _ } -> true
  | _ -> false

let rec cv_to_buffer buf : Component.t -> unit = function
  | Preserved t -> Buffer.add_string buf (token_kind_to_string t.kind)
  | Block { node = { opening; value }; _ } ->
      Buffer.add_char buf (opening_char opening);
      cvs_to_buffer buf value;
      Buffer.add_char buf (closing_char opening)
  | Func { node = { name; arguments }; _ } ->
      Buffer.add_string buf (escape_ident name);
      Buffer.add_char buf '(';
      cvs_to_buffer buf arguments;
      Buffer.add_char buf ')'

(* The serialised [Delim "\\"] is "\\\n", which already supplies a separator;
   eat the next whitespace so [Delim "\\"; Whitespace] round-trips cleanly. *)
and cvs_to_buffer buf cvs =
  let rec loop prev = function
    | [] -> ()
    | cv :: rest
      when is_whitespace cv
           && match prev with Some p -> is_backslash_delim p | None -> false ->
        loop prev rest
    | cv :: rest ->
        cv_to_buffer buf cv;
        loop (Some cv) rest
  in
  loop None cvs

let to_string cvs =
  let buf = Buffer.create 64 in
  cvs_to_buffer buf cvs;
  Buffer.contents buf

(* CSS Syntax Level 3 section 9.1: when serialising adjacent tokens, the
   serialiser must keep them lexically separate. Two predicates are needed
   because the relevant property is what byte the previous token *ends* with and
   what byte the next token *starts* with:

   - [word_like_end p]: [p] ends with a code point that could continue an
   ident-like or numeric token (so an adjacent ident-continue or [-] would merge
   in). - [word_like_start n]: [n] starts with a code point that an ident-like
   or numeric token could absorb on its left.

   {!Func} components begin with [ident(] (word-like at the start) but end with
   [)] (self-delimiting). {!Block} is self-delimiting at both ends.
   Self-delimiting tokens never need separation from a neighbour. *)
let word_like_end : Component.t -> bool = function
  | Preserved
      {
        kind =
          ( Whitespace | Open _ | Close _ | Colon | Semicolon | Comma | Cdo
          | Cdc | Bad_string | Bad_url | Eof
          (* These delim characters are self-delimiting at the end, so a
             trailing [<delim>] never merges with what follows. *)
          | Delim
              ( "!" | "*" | "/" | ">" | "?" | "|" | "&" | "^" | "$" | "=" | "%"
              | "~" | "(" | ")" | "[" | "]" | "{" | "}" ) );
        _;
      } ->
      false
  | Preserved _ -> true
  | Func _ -> false
  | Block _ -> false

let word_like_start : Component.t -> bool = function
  | Preserved
      {
        kind =
          ( Whitespace | Close _ | Colon | Semicolon | Comma | Cdo | Cdc
          | Bad_string | Bad_url | Eof
          | Delim
              ( "!" | "*" | "/" | ">" | "?" | "|" | "&" | "^" | "$" | "=" | "~"
              | "(" | ")" | "[" | "]" | "{" | "}" ) );
        _;
      } ->
      false
  | Preserved { kind = Open Square | Open Curly; _ } -> false
  | Preserved { kind = Open Paren; _ } -> true
  | Preserved _ -> true
  | Func _ -> true
  | Block { node = { opening = Paren; _ }; _ } -> true
  | Block _ -> false

let rec cv_to_buffer_min buf = function
  | Preserved t -> Buffer.add_string buf (token_kind_to_string t.kind)
  | Block { node = { opening; value }; _ } ->
      Buffer.add_char buf (opening_char opening);
      cvs_to_buffer_min buf value;
      Buffer.add_char buf (closing_char opening)
  | Func { node = { name; arguments }; _ } ->
      Buffer.add_string buf (escape_ident name);
      Buffer.add_char buf '(';
      cvs_to_buffer_min buf arguments;
      Buffer.add_char buf ')'

and cvs_to_buffer_min buf cvs =
  let rec loop prev = function
    | [] -> ()
    | Component.Preserved { kind = Token.Whitespace; _ } :: rest ->
        let rec skip_ws = function
          | Component.Preserved { kind = Token.Whitespace; _ } :: r -> skip_ws r
          | other -> other
        in
        let rest' = skip_ws rest in
        (match rest' with
        | next :: _
          when (match prev with
                 | Some p -> word_like_end p && not (is_backslash_delim p)
                 | None -> false)
               && word_like_start next ->
            Buffer.add_char buf ' '
        | _ -> ());
        loop prev rest'
    | cv :: rest ->
        cv_to_buffer_min buf cv;
        loop (Some cv) rest
  in
  loop None cvs

let to_string_minified cvs =
  let buf = Buffer.create 64 in
  cvs_to_buffer_min buf cvs;
  Buffer.contents buf

(** {1 Rule / declaration consumers (section 5.3)} *)

(* Push a warning, attaching a source snippet from the lexer's reader when [meta
   = `Full] so section 5.3 recovery warnings carry the same context as raised
   Cursor errors. Lower meta levels skip the snippet allocation. *)
let warn ~meta lexer (warnings : Error.t list ref) (e : Error.t) =
  let e =
    match meta with
    | `Full ->
        let source = Lexer.source lexer in
        let snippet = Loc.snippet source e.loc in
        Error.v ~snippet ~loc:e.loc ~sort:e.sort e.kind
    | `None | `Locs -> e
  in
  warnings := e :: !warnings

(* CSS Syntax Level 3 section 5.5.2. [nested = true] also terminates on a stray
   ['}'] (the spec's "outermost block ended") so block-contents callers can
   recover instead of swallowing the closing delimiter. *)
let consume_at_rule ?(nested = false) lexer ~name ~start_loc : Component.at_rule
    =
  let close prelude end_loc block =
    let loc = Loc.union start_loc end_loc in
    { node = { name; prelude = List.rev prelude; block }; loc }
  in
  let rec loop prelude =
    let tok = Lexer.peek lexer in
    match tok.Token.kind with
    | Token.Semicolon | Token.Eof ->
        let _ = Lexer.next lexer in
        close prelude tok.loc None
    | Token.Close Curly when nested -> close prelude tok.loc None
    | Token.Open Curly ->
        let _ = Lexer.next lexer in
        let block = consume_simple_block lexer Curly ~start_loc:tok.loc in
        close prelude block.loc (Some block)
    | _ ->
        let _ = Lexer.next lexer in
        let cv = consume_component_value_from lexer tok in
        loop (cv :: prelude)
  in
  loop []

(* CSS Syntax Level 3 section 5.5.3. [nested = true] makes a stray ['}'] or a
   top-level ';' before any block end the rule attempt with [None]. The
   custom-property-shaped guard discards a rule whose first two non-whitespace
   prelude items are an ident starting with [--] followed by ':'. *)
let consume_qualified_rule ?(nested = false) ~meta lexer ~start_loc ~warnings :
    Component.qualified_rule option =
  let is_custom_property_shape prelude =
    let rec drop_ws = function
      | Component.Preserved { kind = Token.Whitespace; _ } :: rest ->
          drop_ws rest
      | other -> other
    in
    match drop_ws (List.rev prelude) with
    | Component.Preserved { kind = Token.Ident name; _ } :: rest
      when String.length name >= 2 && name.[0] = '-' && name.[1] = '-' -> (
        match drop_ws rest with
        | Component.Preserved { kind = Token.Colon; _ } :: _ -> true
        | _ -> false)
    | _ -> false
  in
  let rec loop prelude =
    let tok = Lexer.peek lexer in
    match tok.Token.kind with
    | Token.Eof ->
        let _ = Lexer.next lexer in
        let loc = Loc.union start_loc tok.loc in
        warn ~meta lexer warnings (Error.unterminated loc Sort.Qualified_rule);
        None
    | Token.Semicolon when nested ->
        let _ = Lexer.next lexer in
        None
    | Token.Close Curly when nested -> None
    | Token.Open Curly ->
        let _ = Lexer.next lexer in
        let block = consume_simple_block lexer Curly ~start_loc:tok.loc in
        if nested && is_custom_property_shape prelude then None
        else
          let loc = Loc.union start_loc block.loc in
          Some { node = { prelude = List.rev prelude; block }; loc }
    | _ ->
        let _ = Lexer.next lexer in
        let cv = consume_component_value_from lexer tok in
        loop (cv :: prelude)
  in
  loop []

let consume_list_of_rules ~meta lexer ~top_level ~warnings : Component.rule list
    =
  let rec loop acc =
    let tok = Lexer.next lexer in
    match tok.Token.kind with
    | Token.Eof -> List.rev acc
    | Token.Whitespace -> loop acc
    | (Token.Cdo | Token.Cdc) when top_level -> loop acc
    | Token.Cdo | Token.Cdc -> (
        Lexer.reconsume lexer tok;
        match
          consume_qualified_rule ~meta lexer ~start_loc:tok.loc ~warnings
        with
        | Some qr -> loop (Qualified qr :: acc)
        | None -> loop acc)
    | Token.At_keyword name ->
        let ar = consume_at_rule lexer ~name ~start_loc:tok.loc in
        loop (At ar :: acc)
    | _ -> (
        Lexer.reconsume lexer tok;
        match
          consume_qualified_rule ~meta lexer ~start_loc:tok.loc ~warnings
        with
        | Some qr -> loop (Qualified qr :: acc)
        | None -> loop acc)
  in
  loop []

(* 5.3.7 Parse a declaration from a buffered component-value list. *)
let parse_declaration_from_buffer ~meta lexer ~name ~name_loc ~warnings cvs :
    Component.declaration option =
  let is_ws_cv = function
    | Preserved { kind = Token.Whitespace; _ } -> true
    | _ -> false
  in
  let is_curly_block = function
    | Block { node = { opening = Token.Curly; _ }; _ } -> true
    | _ -> false
  in
  let rec skip_leading_ws = function
    | hd :: rest when is_ws_cv hd -> skip_leading_ws rest
    | other -> other
  in
  let is_custom = String.length name >= 2 && name.[0] = '-' && name.[1] = '-' in
  (* CSS Syntax section 5.5.6: a non-custom property may contain a top-level {}
     block only if that block is the entire (non-whitespace) value. A custom
     property may contain a {} block, but only as the FIRST non-whitespace
     component value -- a block appearing mid-value makes the declaration
     invalid. *)
  let value_has_invalid_block value =
    let trimmed =
      skip_leading_ws value |> List.rev |> skip_leading_ws |> List.rev
    in
    let has_block = List.exists is_curly_block trimmed in
    if not has_block then false
    else
      match trimmed with
      | first :: _ when is_curly_block first ->
          if is_custom then false
          else
            (* Non-custom: also require nothing after the leading block. *)
            List.length trimmed > 1
      | _ -> true
  in
  match skip_leading_ws cvs with
  | Preserved { kind = Token.Colon; _ } :: rest ->
      let value0 = skip_leading_ws rest in
      let rec rtrim = function
        | [] -> []
        | lst -> (
            let rev = List.rev lst in
            match rev with
            | hd :: rest' when is_ws_cv hd -> rtrim (List.rev rest')
            | _ -> lst)
      in
      let value1 = rtrim value0 in
      let value, important =
        let rev = List.rev value1 in
        match rev with
        | Preserved { kind = Token.Ident s; _ } :: rest
          when String.lowercase_ascii s = "important" -> (
            let rest = skip_leading_ws rest in
            match rest with
            | Preserved { kind = Token.Delim "!"; _ } :: rest ->
                (rtrim (List.rev rest), true)
            | _ -> (value1, false))
        | _ -> (value1, false)
      in
      if value_has_invalid_block value then (
        warn ~meta lexer warnings
          (Error.unexpected_token name_loc ~sort:Sort.Declaration
             (Token.Open Token.Curly));
        None)
      else
        let loc =
          List.fold_left
            (fun l cv -> Loc.union l (Component.source_loc cv))
            name_loc value
        in
        Some { node = { name; value; important }; loc }
  | _ ->
      warn ~meta lexer warnings
        (Error.missing_token name_loc ~sort:Sort.Declaration "':'");
      None

(* Buffer component values until the terminating ';' or EOF (CSS Syntax section
   5.4.6 declaration body). Shared by the list, single-declaration and
   block-contents entry points. *)
let consume_declaration_body lexer =
  let rec loop acc =
    let t = Lexer.next lexer in
    match t.Token.kind with
    | Token.Semicolon | Token.Eof -> List.rev acc
    | _ -> loop (consume_component_value_from lexer t :: acc)
  in
  loop []

let consume_list_of_declarations ~meta lexer ~warnings :
    [ `Decl of Component.declaration | `At of Component.at_rule ] list =
  let rec loop acc =
    let tok = Lexer.next lexer in
    match tok.Token.kind with
    | Token.Eof -> List.rev acc
    | Token.Whitespace | Token.Semicolon -> loop acc
    | Token.At_keyword name ->
        let ar = consume_at_rule lexer ~name ~start_loc:tok.loc in
        loop (`At ar :: acc)
    | Token.Ident name -> (
        let body = consume_declaration_body lexer in
        match
          parse_declaration_from_buffer ~meta lexer ~name ~name_loc:tok.loc
            ~warnings body
        with
        | Some d -> loop (`Decl d :: acc)
        | None -> loop acc)
    | _ ->
        warn ~meta lexer warnings
          (Error.unexpected_token tok.loc ~sort:Sort.Declaration tok.kind);
        let rec skip () =
          let t = Lexer.next lexer in
          match t.Token.kind with
          | Token.Semicolon | Token.Eof -> ()
          | _ ->
              let _ = consume_component_value_from lexer t in
              skip ()
        in
        let _ = consume_component_value_from lexer tok in
        skip ();
        loop acc
  in
  loop []

(** {1 Entry points (section 5.4)} *)

type 'a output = { value : 'a; warnings : Error.t list }

type block_item =
  [ `Decls of Component.declaration list | `Rule of Component.rule ]

type grammar = Component.t list -> bool

let with_warnings f =
  let warnings = ref [] in
  let value = f ~warnings in
  { value; warnings = List.rev !warnings }

let parse_stylesheet ?(meta = Loc.default_meta_level) r =
  with_warnings (fun ~warnings ->
      let lexer = Lexer.of_reader r in
      consume_list_of_rules ~meta lexer ~top_level:true ~warnings)

let parse_stylesheet_contents = parse_stylesheet

(* CSS Syntax Level 3 section 5.4.5: a block's contents is a mix of declarations
   and nested rules. Consecutive declarations are grouped into a single [`Decls]
   item so callers can re-emit them as a contiguous run. *)
let consume_block_contents ~meta lexer ~warnings : block_item list =
  let pending = ref [] in
  let result = ref [] in
  let flush () =
    match !pending with
    | [] -> ()
    | ds ->
        result := `Decls (List.rev ds) :: !result;
        pending := []
  in
  let rec loop () =
    let tok = Lexer.next lexer in
    match tok.Token.kind with
    | Token.Eof | Token.Close Curly ->
        flush ();
        List.rev !result
    | Token.Whitespace | Token.Semicolon -> loop ()
    | Token.At_keyword name ->
        flush ();
        let ar = consume_at_rule ~nested:true lexer ~name ~start_loc:tok.loc in
        result := `Rule (Component.At ar) :: !result;
        loop ()
    | Token.Ident name -> (
        let body = consume_declaration_body lexer in
        match
          parse_declaration_from_buffer ~meta lexer ~name ~name_loc:tok.loc
            ~warnings body
        with
        | Some d ->
            pending := d :: !pending;
            loop ()
        | None -> loop ())
    | _ ->
        flush ();
        Lexer.reconsume lexer tok;
        (match
           consume_qualified_rule ~nested:true ~meta lexer ~start_loc:tok.loc
             ~warnings
         with
        | Some qr -> result := `Rule (Component.Qualified qr) :: !result
        | None -> ());
        loop ()
  in
  loop ()

let parse_block_contents ?(meta = Loc.default_meta_level) r :
    block_item list output =
  with_warnings (fun ~warnings ->
      let lexer = Lexer.of_reader r in
      consume_block_contents ~meta lexer ~warnings)

(* CSS Syntax Level 3 section 5.4.6 "Parse a rule": skip surrounding whitespace,
   consume one rule, require EOF, no extra rules or stray tokens afterwards. *)
let parse_rule ?(meta = Loc.default_meta_level) r =
  with_warnings (fun ~warnings ->
      let lexer = Lexer.of_reader r in
      let rec skip_ws () =
        match (Lexer.peek lexer).Token.kind with
        | Token.Whitespace ->
            let _ = Lexer.next lexer in
            skip_ws ()
        | _ -> ()
      in
      skip_ws ();
      let rule =
        match (Lexer.peek lexer).Token.kind with
        | Token.Eof -> None
        | Token.At_keyword name ->
            let tok = Lexer.next lexer in
            Some (Component.At (consume_at_rule lexer ~name ~start_loc:tok.loc))
        | _ -> (
            let start_loc = (Lexer.peek lexer).Token.loc in
            match consume_qualified_rule ~meta lexer ~start_loc ~warnings with
            | Some qr -> Some (Component.Qualified qr)
            | None -> None)
      in
      match rule with
      | None -> None
      | Some _ as r' ->
          skip_ws ();
          if (Lexer.peek lexer).Token.kind = Token.Eof then r' else None)

(* CSS Syntax Level 3 section 5.4.7 "Parse a declaration": skip leading
   whitespace, require an ident, consume exactly one declaration, ignore
   anything after the terminating ';' or EOF. The first non-whitespace token
   must be the declaration name -- a stray ':' or [@x] is a syntax error. *)
let parse_declaration ?(meta = Loc.default_meta_level) r =
  with_warnings (fun ~warnings ->
      let lexer = Lexer.of_reader r in
      let rec skip_ws () =
        match (Lexer.peek lexer).Token.kind with
        | Token.Whitespace ->
            let _ = Lexer.next lexer in
            skip_ws ()
        | _ -> ()
      in
      skip_ws ();
      match (Lexer.peek lexer).Token.kind with
      | Token.Ident name ->
          let tok = Lexer.next lexer in
          let body = consume_declaration_body lexer in
          parse_declaration_from_buffer ~meta lexer ~name ~name_loc:tok.loc
            ~warnings body
      | _ -> None)

let parse_list_of_declarations ?(meta = Loc.default_meta_level) r =
  with_warnings (fun ~warnings ->
      let lexer = Lexer.of_reader r in
      consume_list_of_declarations ~meta lexer ~warnings)

let parse_list_of_rules ?(meta = Loc.default_meta_level) r =
  with_warnings (fun ~warnings ->
      let lexer = Lexer.of_reader r in
      consume_list_of_rules ~meta lexer ~top_level:false ~warnings)

let parse_list_of_component_values r =
  with_warnings (fun ~warnings:_ ->
      let p = of_reader r in
      let rec loop acc =
        match next p with
        | Preserved { kind = Token.Eof; _ } -> List.rev acc
        | cv -> loop (cv :: acc)
      in
      loop [])

let parse_component_value r =
  with_warnings (fun ~warnings:_ ->
      let p = of_reader r in
      let rec next_non_ws () =
        match next p with
        | Preserved { kind = Token.Whitespace; _ } -> next_non_ws ()
        | cv -> cv
      in
      let first = next_non_ws () in
      match first with
      | Preserved { kind = Token.Eof; _ } -> None
      | _ ->
          let rec rest_is_ws_then_eof () =
            match next p with
            | Preserved { kind = Token.Eof; _ } -> true
            | Preserved { kind = Token.Whitespace; _ } -> rest_is_ws_then_eof ()
            | _ -> false
          in
          if rest_is_ws_then_eof () then Some first else None)

let split_comma_groups cvs =
  let rec split current groups = function
    | [] ->
        if current = [] && groups = [] then []
        else if current = [] then List.rev groups
        else List.rev (List.rev current :: groups)
    | [ Preserved { kind = Token.Comma; _ } ] ->
        List.rev (List.rev current :: groups)
    | Preserved { kind = Token.Comma; _ } :: rest ->
        split [] (List.rev current :: groups) rest
    | cv :: rest -> split (cv :: current) groups rest
  in
  split [] [] cvs

let parse_comma_separated_list_of_component_values r =
  let out = parse_list_of_component_values r in
  { out with value = split_comma_groups out.value }

let trim_component_value_whitespace cvs =
  let is_ws = function
    | Preserved { kind = Token.Whitespace; _ } -> true
    | _ -> false
  in
  let rec drop_leading = function
    | cv :: rest when is_ws cv -> drop_leading rest
    | rest -> rest
  in
  cvs |> drop_leading |> List.rev |> drop_leading |> List.rev

let component_values_are_whitespace_only cvs =
  List.for_all
    (function Preserved { kind = Token.Whitespace; _ } -> true | _ -> false)
    cvs

let parse_according_to_grammar r grammar =
  let out = parse_list_of_component_values r in
  let value = trim_component_value_whitespace out.value in
  if grammar value then { out with value = Some value }
  else { out with value = None }

let parse_comma_separated_list_according_to_grammar r grammar =
  let raw = parse_list_of_component_values r in
  if component_values_are_whitespace_only raw.value then { raw with value = [] }
  else
    let out = { raw with value = split_comma_groups raw.value } in
    let match_group group =
      let group = trim_component_value_whitespace group in
      if grammar group then Some group else None
    in
    { out with value = List.map match_group out.value }

let rec arbitrary_value_tokens_ok ~allow_top_level_semicolon_bang ~top_level =
  List.for_all (fun cv ->
      match cv with
      | Component.Preserved
          { kind = Token.Bad_string | Token.Bad_url | Token.Close _; _ } ->
          false
      | Component.Preserved { kind = Token.Semicolon; _ }
        when top_level && not allow_top_level_semicolon_bang ->
          false
      | Component.Preserved { kind = Token.Delim "!"; _ }
        when top_level && not allow_top_level_semicolon_bang ->
          false
      | Component.Block { node = { value; _ }; _ }
      | Component.Func { node = { arguments = value; _ }; _ } ->
          arbitrary_value_tokens_ok ~allow_top_level_semicolon_bang
            ~top_level:false value
      | Component.Preserved _ -> true)

let parse_arbitrary_value r ~allow_top_level_semicolon_bang =
  let out = parse_list_of_component_values r in
  let value = trim_component_value_whitespace out.value in
  if
    value <> []
    && arbitrary_value_tokens_ok ~allow_top_level_semicolon_bang ~top_level:true
         value
  then { out with value = Some value }
  else { out with value = None }

let parse_declaration_value r =
  parse_arbitrary_value r ~allow_top_level_semicolon_bang:false

let parse_any_value r =
  parse_arbitrary_value r ~allow_top_level_semicolon_bang:true
