(** CSS Syntax Module Level 3 section 5: parser algorithms.

    Ports the "consume a ..." algorithms from
    https://www.w3.org/TR/css-syntax-3/#parser-algorithms onto a token stream
    sourced from {!Token.next}. Produces the generic intermediate representation
    (component values, simple blocks, functions, rules, declarations) defined in
    section 5.1; higher-level typed parsing runs afterwards as validation on
    those component values. *)

(** {1 Intermediate representation (section 5.1)} *)

type component_value =
  | Preserved of Token.t
      (** Any token except function/[{]/[(]/[[]: passed through. *)
  | Block of simple_block
  | Func of function_cv

and simple_block = { opening : Token.bracket; value : component_value list }
(** A balanced [{...\}], [(...)] or [[...]] group. *)

and function_cv = { name : string; arguments : component_value list }
(** A [name(...)] group, where the arguments are a list of component values. *)

type at_rule = {
  name : string;
  prelude : component_value list;
  block : simple_block option;
}
(** An at-rule: name, prelude (component values between [@name] and the block or
    terminating [;]), and optional block. *)

type qualified_rule = { prelude : component_value list; block : simple_block }
(** A qualified rule (style rule): prelude (typically a selector list) followed
    by a block. *)

type rule = Qualified of qualified_rule | At of at_rule

type declaration = {
  name : string;
  value : component_value list;
  important : bool;
}
(** A declaration extracted by section 5.3.7. [value] has trailing whitespace
    and the [!important] marker stripped. *)

(** {1 Token stream with one-token pushback} *)

type stream = { reader : Reader.t; mutable lookback : Token.t option }

let of_reader reader = { reader; lookback = None }

(** [next s] consumes the next token, honouring any pushed-back token. *)
let next s =
  match s.lookback with
  | Some t ->
      s.lookback <- None;
      t
  | None -> Token.next s.reader

(** [reconsume s t] pushes [t] back so the next call to {!next} returns it. At
    most one token can be pushed back. *)
let reconsume s t =
  assert (s.lookback = None);
  s.lookback <- Some t

(** {1 Reserialization} *)

(* Turn a token back into its source form. *)
let token_to_string : Token.t -> string = function
  | Token.Ident s -> s
  | Token.Function s -> s ^ "("
  | Token.At_keyword s -> "@" ^ s
  | Token.Hash { value; _ } -> "#" ^ value
  | Token.String s ->
      (* Double-quote and escape embedded double-quotes and backslashes. *)
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

let rec cv_to_buffer buf = function
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

(** [to_string cvs] reconstitutes the source text for a component-value list.
    Whitespace tokens are serialized as a single space; the output is not
    byte-identical to the input but is parse-equivalent. *)
let to_string cvs =
  let buf = Buffer.create 64 in
  List.iter (cv_to_buffer buf) cvs;
  Buffer.contents buf

(** {1 Parser algorithms (section 5.3)} *)

(* Forward-declared so the recursive algorithms can call each other. *)

let rec consume_component_value s =
  match next s with
  | Token.Open bracket -> Block (consume_simple_block s bracket)
  | Token.Function name -> Func (consume_function s ~name)
  | t -> Preserved t

(* section 5.3.9 Consume a simple block. The opening token has been consumed; we
   scan until the matching close. *)
and consume_simple_block s opening =
  let ending = Token.Close opening in
  let rec loop acc =
    match next s with
    | Token.Eof ->
        (* Parse error; return what we have. *)
        { opening; value = List.rev acc }
    | t when t = ending -> { opening; value = List.rev acc }
    | t ->
        reconsume s t;
        let cv = consume_component_value s in
        loop (cv :: acc)
  in
  loop []

(* section 5.3.10 Consume a function. The <function-token> has been consumed;
   its name is passed in. Scan until <)-token>. *)
and consume_function s ~name =
  let rec loop acc =
    match next s with
    | Token.Eof | Token.Close Paren -> { name; arguments = List.rev acc }
    | t ->
        reconsume s t;
        let cv = consume_component_value s in
        loop (cv :: acc)
  in
  loop []

(* section 5.3.3 Consume an at-rule. Assumes the <at-keyword-token> has been
   consumed; its name is passed in. *)
let consume_at_rule s ~name =
  let rec loop prelude =
    match next s with
    | Token.Semicolon -> { name; prelude = List.rev prelude; block = None }
    | Token.Eof ->
        (* Parse error. *)
        { name; prelude = List.rev prelude; block = None }
    | Token.Open Curly ->
        let block = consume_simple_block s Curly in
        { name; prelude = List.rev prelude; block = Some block }
    | t ->
        reconsume s t;
        let cv = consume_component_value s in
        loop (cv :: prelude)
  in
  loop []

(* section 5.3.4 Consume a qualified rule. *)
let consume_qualified_rule s =
  let rec loop prelude =
    match next s with
    | Token.Eof -> None (* Parse error: drop the rule. *)
    | Token.Open Curly ->
        let block = consume_simple_block s Curly in
        Some { prelude = List.rev prelude; block }
    | t ->
        reconsume s t;
        let cv = consume_component_value s in
        loop (cv :: prelude)
  in
  loop []

(* section 5.3.2 Consume a list of rules. [top_level] controls CDO/CDC handling:
   at the stylesheet's top level, CDO and CDC are skipped; inside nested rule
   lists (e.g. @media bodies) they reconsume into qualified rules. *)
let consume_list_of_rules s ~top_level =
  let rec loop acc =
    match next s with
    | Token.Eof -> List.rev acc
    | Token.Whitespace -> loop acc
    | (Token.Cdo | Token.Cdc) when top_level -> loop acc
    | (Token.Cdo | Token.Cdc) as t -> (
        reconsume s t;
        match consume_qualified_rule s with
        | Some qr -> loop (Qualified qr :: acc)
        | None -> loop acc)
    | Token.At_keyword name ->
        let ar = consume_at_rule s ~name in
        loop (At ar :: acc)
    | t -> (
        reconsume s t;
        match consume_qualified_rule s with
        | Some qr -> loop (Qualified qr :: acc)
        | None -> loop acc)
  in
  loop []

(* Given a buffered list of component values (and the ident that started the
   declaration), parse it as a declaration per section 5.3.7. Returns None if
   the buffer doesn't form a valid declaration header. *)
let parse_declaration_from_buffer ~name cvs =
  (* cvs is in order. Skip leading whitespace. *)
  let rec skip_leading_ws = function
    | Preserved Token.Whitespace :: rest -> skip_leading_ws rest
    | other -> other
  in
  let after_name = skip_leading_ws cvs in
  match after_name with
  | Preserved Token.Colon :: rest ->
      let value0 = skip_leading_ws rest in
      (* Trim trailing whitespace. *)
      let rec rtrim = function
        | [] -> []
        | lst -> (
            let rev = List.rev lst in
            match rev with
            | Preserved Token.Whitespace :: rest -> rtrim (List.rev rest)
            | _ -> lst)
      in
      let value1 = rtrim value0 in
      (* Detect and strip trailing "!important". *)
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

(* section 5.3.6 Consume a list of declarations. Invalid tokens are skipped to
   the next <semicolon-token> or <EOF-token>. *)
let consume_list_of_declarations s =
  let rec loop acc =
    match next s with
    | Token.Eof -> List.rev acc
    | Token.Whitespace | Token.Semicolon -> loop acc
    | Token.At_keyword name ->
        let ar = consume_at_rule s ~name in
        loop (`At ar :: acc)
    | Token.Ident name -> (
        (* Buffer tokens up to the next ; or EOF, then parse as a
           declaration. *)
        let rec buffer acc =
          match next s with
          | Token.Semicolon | Token.Eof -> List.rev acc
          | t ->
              reconsume s t;
              let cv = consume_component_value s in
              buffer (cv :: acc)
        in
        let body = buffer [] in
        match parse_declaration_from_buffer ~name body with
        | Some d -> loop (`Decl d :: acc)
        | None -> loop acc)
    | t ->
        (* Parse error: skip to next ; or EOF. *)
        reconsume s t;
        let rec skip () =
          match next s with
          | Token.Semicolon | Token.Eof -> ()
          | t ->
              reconsume s t;
              let _ = consume_component_value s in
              skip ()
        in
        skip ();
        loop acc
  in
  loop []

(** {1 Entry points (section 5.4)} *)

(** [parse_stylesheet r] parses an entire stylesheet from [r] per section 5.4.3.
    Equivalent to [consume_list_of_rules ~top_level:true]. *)
let parse_stylesheet r =
  let s = of_reader r in
  consume_list_of_rules s ~top_level:true

(** [parse_list_of_declarations r] parses the contents of a declaration block
    per section 5.4.8. The input should be the body (without the surrounding
    braces). *)
let parse_list_of_declarations r =
  let s = of_reader r in
  consume_list_of_declarations s

(** [parse_list_of_rules r] is section 5.4.4; used for nested rule bodies where
    CDO/CDC should NOT be discarded. *)
let parse_list_of_rules r =
  let s = of_reader r in
  consume_list_of_rules s ~top_level:false
