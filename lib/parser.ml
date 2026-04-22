(** Stage 3 stream: Token.t -> Component.t.

    Ports the "consume a ..." algorithms from
    https://www.w3.org/TR/css-syntax-3/#parser-algorithms onto a {!Lexer.t}
    token stream, producing the IR defined in {!Component}. *)

open Component

type t = { lexer : Lexer.t; mutable lookback : Component.t option }

let of_lexer lexer = { lexer; lookback = None }
let of_reader r = of_lexer (Lexer.of_reader r)
let of_string s = of_reader (Reader.of_string s)

(** {1 §5.3 algorithms operating on a {!Lexer.t}} *)

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

let token_kind_to_string : Token.kind -> string = function
  | Token.Ident s -> s
  | Token.Function s -> s ^ "("
  | Token.At_keyword s -> "@" ^ s
  | Token.Hash { value; _ } -> "#" ^ value
  | Token.String s ->
      let buf = Buffer.create (String.length s + 2) in
      Buffer.add_char buf '"';
      String.iter
        (fun c ->
          (match c with '"' | '\\' -> Buffer.add_char buf '\\' | _ -> ());
          Buffer.add_char buf c)
        s;
      Buffer.add_char buf '"';
      Buffer.contents buf
  | Token.Bad_string -> ""
  | Token.Url s -> "url(" ^ s ^ ")"
  | Token.Bad_url -> ""
  | Token.Delim c -> String.make 1 c
  | Token.Number_tok { repr; _ } -> repr
  | Token.Percentage { repr; _ } -> repr ^ "%"
  | Token.Dimension { number; unit_ } -> number.repr ^ unit_
  | Token.Whitespace -> " "
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

let rec cv_to_buffer buf : Component.t -> unit = function
  | Preserved t -> Buffer.add_string buf (token_kind_to_string t.kind)
  | Block { node = { opening; value }; _ } ->
      Buffer.add_char buf (opening_char opening);
      List.iter (cv_to_buffer buf) value;
      Buffer.add_char buf (closing_char opening)
  | Func { node = { name; arguments }; _ } ->
      Buffer.add_string buf name;
      Buffer.add_char buf '(';
      List.iter (cv_to_buffer buf) arguments;
      Buffer.add_char buf ')'

let to_string cvs =
  let buf = Buffer.create 64 in
  List.iter (cv_to_buffer buf) cvs;
  Buffer.contents buf

(** {1 Rule / declaration consumers (section 5.3)} *)

let warn (warnings : Error.t list ref) e = warnings := e :: !warnings

let consume_at_rule lexer ~name ~start_loc : Component.at_rule =
  let rec loop prelude =
    let tok = Lexer.next lexer in
    match tok.Token.kind with
    | Token.Semicolon ->
        let loc = Loc.union start_loc tok.loc in
        { node = { name; prelude = List.rev prelude; block = None }; loc }
    | Token.Eof ->
        let loc = Loc.union start_loc tok.loc in
        { node = { name; prelude = List.rev prelude; block = None }; loc }
    | Token.Open Curly ->
        let block = consume_simple_block lexer Curly ~start_loc:tok.loc in
        let loc = Loc.union start_loc block.loc in
        { node = { name; prelude = List.rev prelude; block = Some block }; loc }
    | _ ->
        let cv = consume_component_value_from lexer tok in
        loop (cv :: prelude)
  in
  loop []

let consume_qualified_rule lexer ~start_loc ~warnings :
    Component.qualified_rule option =
  let rec loop prelude =
    let tok = Lexer.next lexer in
    match tok.Token.kind with
    | Token.Eof ->
        let loc = Loc.union start_loc tok.loc in
        warn warnings (Error.unterminated loc Sort.Qualified_rule);
        None
    | Token.Open Curly ->
        let block = consume_simple_block lexer Curly ~start_loc:tok.loc in
        let loc = Loc.union start_loc block.loc in
        Some { node = { prelude = List.rev prelude; block }; loc }
    | _ ->
        let cv = consume_component_value_from lexer tok in
        loop (cv :: prelude)
  in
  loop []

let consume_list_of_rules lexer ~top_level ~warnings : Component.rule list =
  let rec loop acc =
    let tok = Lexer.next lexer in
    match tok.Token.kind with
    | Token.Eof -> List.rev acc
    | Token.Whitespace -> loop acc
    | (Token.Cdo | Token.Cdc) when top_level -> loop acc
    | Token.Cdo | Token.Cdc -> (
        Lexer.reconsume lexer tok;
        match consume_qualified_rule lexer ~start_loc:tok.loc ~warnings with
        | Some qr -> loop (Qualified qr :: acc)
        | None -> loop acc)
    | Token.At_keyword name ->
        let ar = consume_at_rule lexer ~name ~start_loc:tok.loc in
        loop (At ar :: acc)
    | _ -> (
        Lexer.reconsume lexer tok;
        match consume_qualified_rule lexer ~start_loc:tok.loc ~warnings with
        | Some qr -> loop (Qualified qr :: acc)
        | None -> loop acc)
  in
  loop []

(* 5.3.7 Parse a declaration from a buffered component-value list. *)
let parse_declaration_from_buffer ~name ~name_loc ~warnings cvs :
    Component.declaration option =
  let is_ws_cv = function
    | Preserved { kind = Token.Whitespace; _ } -> true
    | _ -> false
  in
  let rec skip_leading_ws = function
    | hd :: rest when is_ws_cv hd -> skip_leading_ws rest
    | other -> other
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
            | Preserved { kind = Token.Delim '!'; _ } :: rest ->
                (rtrim (List.rev rest), true)
            | _ -> (value1, false))
        | _ -> (value1, false)
      in
      let loc =
        List.fold_left
          (fun l cv -> Loc.union l (Component.source_loc cv))
          name_loc value
      in
      Some { node = { name; value; important }; loc }
  | _ ->
      warn warnings (Error.missing_token name_loc ~sort:Sort.Declaration "':'");
      None

let consume_list_of_declarations lexer ~warnings :
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
        let rec buffer acc =
          let t = Lexer.next lexer in
          match t.Token.kind with
          | Token.Semicolon | Token.Eof -> List.rev acc
          | _ ->
              let cv = consume_component_value_from lexer t in
              buffer (cv :: acc)
        in
        let body = buffer [] in
        match
          parse_declaration_from_buffer ~name ~name_loc:tok.loc ~warnings body
        with
        | Some d -> loop (`Decl d :: acc)
        | None -> loop acc)
    | _ ->
        warn warnings
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

let with_warnings f =
  let warnings = ref [] in
  let value = f ~warnings in
  { value; warnings = List.rev !warnings }

let parse_stylesheet r =
  with_warnings (fun ~warnings ->
      let lexer = Lexer.of_reader r in
      consume_list_of_rules lexer ~top_level:true ~warnings)

let parse_list_of_declarations r =
  with_warnings (fun ~warnings ->
      let lexer = Lexer.of_reader r in
      consume_list_of_declarations lexer ~warnings)

let parse_list_of_rules r =
  with_warnings (fun ~warnings ->
      let lexer = Lexer.of_reader r in
      consume_list_of_rules lexer ~top_level:false ~warnings)
