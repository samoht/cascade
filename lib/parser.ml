(** Stage 3 stream: Token.t -> Component.t.

    Ports the "consume a ..." algorithms from
    https://www.w3.org/TR/css-syntax-3/#parser-algorithms onto a {!Lexer.t}
    token stream, producing the IR defined in {!Component}. Downstream typed-AST
    validators consume lists of {!Component.t}. *)

open Component

type t = { lexer : Lexer.t; mutable lookback : Component.t option }

let of_lexer lexer = { lexer; lookback = None }
let of_reader r = of_lexer (Lexer.of_reader r)
let of_string s = of_reader (Reader.of_string s)

(** {1 §5.3 algorithms operating on a {!Lexer.t}} *)

let rec consume_component_value lexer : Component.t =
  match Lexer.next lexer with
  | Token.Open bracket -> Block (consume_simple_block lexer bracket)
  | Token.Function name -> Func (consume_function lexer ~name)
  | t -> Preserved t

and consume_simple_block lexer opening : Component.block =
  let ending = Token.Close opening in
  let rec loop acc =
    match Lexer.next lexer with
    | Token.Eof -> { opening; value = List.rev acc }
    | t when t = ending -> { opening; value = List.rev acc }
    | t ->
        Lexer.reconsume lexer t;
        let cv = consume_component_value lexer in
        loop (cv :: acc)
  in
  loop []

and consume_function lexer ~name : Component.func =
  let rec loop acc =
    match Lexer.next lexer with
    | Token.Eof | Token.Close Paren -> { name; arguments = List.rev acc }
    | t ->
        Lexer.reconsume lexer t;
        let cv = consume_component_value lexer in
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

let token_to_string : Token.t -> string = function
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
  | Preserved t -> Buffer.add_string buf (token_to_string t)
  | Block { opening; value } ->
      Buffer.add_char buf (opening_char opening);
      List.iter (cv_to_buffer buf) value;
      Buffer.add_char buf (closing_char opening)
  | Func { name; arguments } ->
      Buffer.add_string buf name;
      Buffer.add_char buf '(';
      List.iter (cv_to_buffer buf) arguments;
      Buffer.add_char buf ')'

let to_string cvs =
  let buf = Buffer.create 64 in
  List.iter (cv_to_buffer buf) cvs;
  Buffer.contents buf

(** {1 Rule / declaration consumers (section 5.3)} *)

(* section 5.3.3 Consume an at-rule. *)
let consume_at_rule lexer ~name : Component.at_rule =
  let rec loop prelude =
    match Lexer.next lexer with
    | Token.Semicolon -> { name; prelude = List.rev prelude; block = None }
    | Token.Eof -> { name; prelude = List.rev prelude; block = None }
    | Token.Open Curly ->
        let block = consume_simple_block lexer Curly in
        { name; prelude = List.rev prelude; block = Some block }
    | t ->
        Lexer.reconsume lexer t;
        let cv = consume_component_value lexer in
        loop (cv :: prelude)
  in
  loop []

(* section 5.3.4 Consume a qualified rule. *)
let consume_qualified_rule lexer : Component.qualified_rule option =
  let rec loop prelude =
    match Lexer.next lexer with
    | Token.Eof -> None
    | Token.Open Curly ->
        let block = consume_simple_block lexer Curly in
        Some Component.{ prelude = List.rev prelude; block }
    | t ->
        Lexer.reconsume lexer t;
        let cv = consume_component_value lexer in
        loop (cv :: prelude)
  in
  loop []

(* section 5.3.2 Consume a list of rules. *)
let consume_list_of_rules lexer ~top_level : Component.rule list =
  let rec loop acc =
    match Lexer.next lexer with
    | Token.Eof -> List.rev acc
    | Token.Whitespace -> loop acc
    | (Token.Cdo | Token.Cdc) when top_level -> loop acc
    | (Token.Cdo | Token.Cdc) as t -> (
        Lexer.reconsume lexer t;
        match consume_qualified_rule lexer with
        | Some qr -> loop (Qualified qr :: acc)
        | None -> loop acc)
    | Token.At_keyword name ->
        let ar = consume_at_rule lexer ~name in
        loop (At ar :: acc)
    | t -> (
        Lexer.reconsume lexer t;
        match consume_qualified_rule lexer with
        | Some qr -> loop (Qualified qr :: acc)
        | None -> loop acc)
  in
  loop []

(* 5.3.7 Parse a declaration from a buffered component-value list. *)
let parse_declaration_from_buffer ~name cvs : Component.declaration option =
  let rec skip_leading_ws = function
    | Preserved Token.Whitespace :: rest -> skip_leading_ws rest
    | other -> other
  in
  match skip_leading_ws cvs with
  | Preserved Token.Colon :: rest ->
      let value0 = skip_leading_ws rest in
      let rec rtrim = function
        | [] -> []
        | lst -> (
            let rev = List.rev lst in
            match rev with
            | Preserved Token.Whitespace :: rest' -> rtrim (List.rev rest')
            | _ -> lst)
      in
      let value1 = rtrim value0 in
      let value, important =
        let rev = List.rev value1 in
        match rev with
        | Preserved (Token.Ident s) :: rest
          when String.lowercase_ascii s = "important" -> (
            let rest = skip_leading_ws rest in
            match rest with
            | Preserved (Token.Delim '!') :: rest ->
                (rtrim (List.rev rest), true)
            | _ -> (value1, false))
        | _ -> (value1, false)
      in
      Some { name; value; important }
  | _ -> None

(* section 5.3.6 Consume a list of declarations. *)
let consume_list_of_declarations lexer :
    [ `Decl of Component.declaration | `At of Component.at_rule ] list =
  let rec loop acc =
    match Lexer.next lexer with
    | Token.Eof -> List.rev acc
    | Token.Whitespace | Token.Semicolon -> loop acc
    | Token.At_keyword name ->
        let ar = consume_at_rule lexer ~name in
        loop (`At ar :: acc)
    | Token.Ident name -> (
        let rec buffer acc =
          match Lexer.next lexer with
          | Token.Semicolon | Token.Eof -> List.rev acc
          | t ->
              Lexer.reconsume lexer t;
              let cv = consume_component_value lexer in
              buffer (cv :: acc)
        in
        let body = buffer [] in
        match parse_declaration_from_buffer ~name body with
        | Some d -> loop (`Decl d :: acc)
        | None -> loop acc)
    | t ->
        Lexer.reconsume lexer t;
        let rec skip () =
          match Lexer.next lexer with
          | Token.Semicolon | Token.Eof -> ()
          | t ->
              Lexer.reconsume lexer t;
              let _ = consume_component_value lexer in
              skip ()
        in
        skip ();
        loop acc
  in
  loop []

(** {1 Entry points (section 5.4)} *)

let parse_stylesheet r =
  let lexer = Lexer.of_reader r in
  consume_list_of_rules lexer ~top_level:true

let parse_list_of_declarations r =
  let lexer = Lexer.of_reader r in
  consume_list_of_declarations lexer

let parse_list_of_rules r =
  let lexer = Lexer.of_reader r in
  consume_list_of_rules lexer ~top_level:false
