(** Stage 3 stream: Token.t -> Component.t.

    Ports the "consume a ..." algorithms from
    https://www.w3.org/TR/css-syntax-3/#parser-algorithms onto a {!Lexer.t}
    token stream, producing the IR defined in {!Component}. *)

open Component
open Syntax

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

and consume_simple_block lexer opening ~start_loc :
    Component.block Component.node =
  let rec loop acc =
    let tok = Lexer.next lexer in
    match tok.Token.kind with
    | Token.Eof ->
        let loc = Loc.union start_loc tok.loc in
        { node = { opening; value = List.rev acc; closed = false }; loc }
    | Token.Close b when b = opening ->
        let loc = Loc.union start_loc tok.loc in
        { node = { opening; value = List.rev acc; closed = true }; loc }
    | _ ->
        let cv = consume_component_value_from lexer tok in
        loop (cv :: acc)
  in
  loop []

and consume_function lexer ~name ~start_loc : Component.func Component.node =
  let rec loop acc =
    let tok = Lexer.next lexer in
    match tok.Token.kind with
    | Token.Eof ->
        let loc = Loc.union start_loc tok.loc in
        { node = { name; arguments = List.rev acc; terminated = false }; loc }
    | Token.Close Paren ->
        let loc = Loc.union start_loc tok.loc in
        { node = { name; arguments = List.rev acc; terminated = true }; loc }
    | _ ->
        let cv = consume_component_value_from lexer tok in
        loop (cv :: acc)
  in
  loop []

let consume_component_value lexer : Component.t =
  let tok = Lexer.next lexer in
  consume_component_value_from lexer tok

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

let add_hex_escape_cp buf cp =
  Buffer.add_char buf '\\';
  let rec emit n acc =
    if n = 0 && acc = [] then Buffer.add_char buf '0'
    else if n = 0 then List.iter (Buffer.add_char buf) acc
    else emit (n / 16) (hex_digit (n mod 16) :: acc)
  in
  emit cp [];
  Buffer.add_char buf ' '

(* Emit an ASCII (cp < 0x80) code point into [buf]: keep ident-continue
   characters verbatim, otherwise backslash-quote the single byte. *)
let escape_ident_emit_ascii buf cp =
  if is_ascii_ident_continue (Char.chr cp) then
    Buffer.add_char buf (Char.chr cp)
  else (
    Buffer.add_char buf '\\';
    Buffer.add_char buf (Char.chr cp))

(* Emit one code point of an ident-like name, honouring the leading-digit
   restriction recorded in [needs_leading_escape]. *)
let escape_ident_emit_cp buf ~needs_leading_escape u =
  let cp = Uchar.to_int u in
  if needs_leading_escape then add_hex_escape_cp buf cp
  else if cp < 0x20 || cp = 0x7F then add_hex_escape_cp buf cp
  else if cp < 0x80 then escape_ident_emit_ascii buf cp
  else if Lexer.is_non_ascii_ident_cp cp then Uutf.Buffer.add_utf_8 buf u
  else add_hex_escape_cp buf cp

let escape_ident_starts s n =
  let starts_with_digit = n > 0 && s.[0] >= '0' && s.[0] <= '9' in
  let starts_dash_digit =
    n >= 2 && s.[0] = '-' && s.[1] >= '0' && s.[1] <= '9'
  in
  (starts_with_digit, starts_dash_digit)

let escape_ident_needs_leading (starts_with_digit, starts_dash_digit) i =
  (i = 0 && starts_with_digit) || (i = 1 && starts_dash_digit)

let escape_ident_emit_item buf starts () i = function
  | `Uchar u ->
      let needs_leading_escape = escape_ident_needs_leading starts i in
      escape_ident_emit_cp buf ~needs_leading_escape u
  | `Malformed bs ->
      (* Malformed UTF-8 bytes (e.g., a lone continuation byte the lexer's
         [consume_escape] dropped into an ident) can't be re-tokenized as the
         same ident. Hex-escape each byte so the serialized form round-trips. *)
      String.iter (fun c -> add_hex_escape buf c) bs

(* An all-ASCII ident (start byte in ident-start, rest in ident-continue)
   serialises to itself byte-for-byte, so [escape_ident]'s buffer + Uutf walk
   would allocate nothing useful. *)
let escape_ident_needs_no_escape s n =
  if n = 0 then true
  else if not (Syntax.is_ascii_ident_start s.[0]) then false
  else
    let rec loop i =
      if i >= n then true
      else if Syntax.is_ascii_ident_continue s.[i] then loop (i + 1)
      else false
    in
    loop 1

let escape_ident s =
  let n = String.length s in
  if n = 1 && s.[0] = '-' then "\\-"
  else if escape_ident_needs_no_escape s n then s
  else
    let buf = Buffer.create n in
    let starts = escape_ident_starts s n in
    Uutf.String.fold_utf_8 (escape_ident_emit_item buf starts) () s;
    Buffer.contents buf

let escape_name s =
  let n = String.length s in
  let buf = Buffer.create n in
  let folder () _ = function
    | `Uchar u -> escape_ident_emit_cp buf ~needs_leading_escape:false u
    | `Malformed bs -> Buffer.add_string buf bs
  in
  Uutf.String.fold_utf_8 folder () s;
  Buffer.contents buf

let escape_string ~quote ~terminated s =
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
  if terminated then Buffer.add_char buf quote;
  Buffer.contents buf

let string_of_token_kind : Token.kind -> string = function
  | Token.Ident s -> escape_ident s
  | Token.Function s -> escape_ident s ^ "("
  | Token.At_keyword s -> "@" ^ escape_ident s
  | Token.Hash { value; _ } -> "#" ^ escape_name value
  | Token.String { value; quote = _; terminated } ->
      (* Normalize quoting to double-quote (the original quote is kept on the
         token only for quote-sensitive lookups like @charset). CSS Syntax
         §4.3.5 recovers an unterminated string; the [terminated] flag is
         preserved so one round-trips, emitting without its closing quote. *)
      escape_string ~quote:'"' ~terminated value
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
  | Token.Bad_url -> "url(a b)"
  | Token.Delim "\\" -> "\\\n"
  | Token.Delim s -> s
  | Token.Number_tok { repr; _ } -> repr
  | Token.Percentage { repr; _ } -> repr ^ "%"
  | Token.Dimension { number; unit_ } ->
      (* CSS Syntax §9.1 ambiguous-dimension rule: a unit of [e]/[E] then a
         (signed) digit would re-read as scientific notation, so hex-escape the
         leading letter to keep it out of the number's exponent. *)
      let unit_serialized =
        let len = String.length unit_ in
        let next_is_digit i = i < len && unit_.[i] >= '0' && unit_.[i] <= '9' in
        let is_sign c = c = '+' || c = '-' in
        if
          len >= 2
          && (unit_.[0] = 'e' || unit_.[0] = 'E')
          && (next_is_digit 1
             || (len >= 3 && is_sign unit_.[1] && next_is_digit 2))
        then (
          let buf = Buffer.create (len + 4) in
          add_hex_escape buf unit_.[0];
          Buffer.add_string buf (escape_ident (String.sub unit_ 1 (len - 1)));
          Buffer.contents buf)
        else escape_ident unit_
      in
      number.repr ^ unit_serialized
  | Token.Whitespace -> " "
  | Token.Unicode_range { start_value; end_value; _ } ->
      let buf = Buffer.create 16 in
      Buffer.add_string buf "U+";
      let rec add_hex n acc =
        if n = 0 && acc = [] then Buffer.add_char buf '0'
        else if n = 0 then List.iter (Buffer.add_char buf) acc
        else add_hex (n / 16) (hex_digit (n mod 16) :: acc)
      in
      add_hex start_value [];
      if end_value <> start_value then (
        Buffer.add_char buf '-';
        add_hex end_value []);
      Buffer.contents buf
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

let is_signed_number_repr repr =
  String.length repr > 0 && (repr.[0] = '-' || repr.[0] = '+')

let signed_number_pair prev next =
  match (prev, next) with
  | ( Component.Preserved { kind = Token.Number_tok _; _ },
      Component.Preserved { kind = Token.Number_tok { repr; _ }; _ } ) ->
      is_signed_number_repr repr
  | _ -> false

let normal_pair_needs_token_boundary prev next =
  match (prev, next) with
  | _ when signed_number_pair prev next -> false
  | ( Component.Preserved
        {
          kind =
            ( Token.Ident _ | Token.At_keyword _ | Token.Hash _
            | Token.Dimension _ );
          _;
        },
      ( Component.Preserved
          {
            kind =
              ( Token.Ident _ | Token.Function _ | Token.Number_tok _
              | Token.Percentage _ | Token.Dimension _ );
            _;
          }
      | Component.Func _
      | Component.Block { node = { opening = Token.Paren; _ }; _ } ) ) ->
      true
  | ( Component.Preserved { kind = Token.Number_tok _; _ },
      Component.Preserved
        {
          kind =
            ( Token.Ident _ | Token.Function _ | Token.Number_tok _
            | Token.Percentage _ | Token.Dimension _ );
          _;
        } ) ->
      true
  | ( Component.Preserved { kind = Token.Delim ("-" | "#" | "@"); _ },
      ( Component.Preserved { kind = Token.Ident _ | Token.Function _; _ }
      | Component.Func _ ) ) ->
      true
  | ( Component.Preserved { kind = Token.Delim ("+" | "-"); _ },
      Component.Preserved
        {
          kind = Token.Number_tok _ | Token.Percentage _ | Token.Dimension _;
          _;
        } ) ->
      true
  | ( Component.Preserved { kind = Token.Delim "."; _ },
      Component.Preserved
        {
          kind =
            ( Token.Number_tok { repr; _ }
            | Token.Percentage { repr; _ }
            | Token.Dimension { number = { repr; _ }; _ } );
          _;
        } ) ->
      repr <> "" && repr.[0] >= '0' && repr.[0] <= '9'
  | ( Component.Preserved { kind = Token.Hash _; _ },
      Component.Preserved { kind = Token.Delim "-"; _ } ) ->
      true
  | _ -> false

let rec cv_to_buffer buf : Component.t -> unit = function
  | Preserved t -> Buffer.add_string buf (string_of_token_kind t.kind)
  | Block { node = { opening; value; _ }; _ } ->
      Buffer.add_char buf (opening_char opening);
      cvs_to_buffer buf value;
      Buffer.add_char buf (closing_char opening)
  | Func { node = { name; arguments; _ }; _ } ->
      (* Always emit the closing [)]: section 5.4.6 still produces the function
         token on EOF, so the round-trip should match the lexer, not the
         truncated bytes. [terminated] is left for typed validators to
         reject. *)
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
        (match prev with
        | Some p when normal_pair_needs_token_boundary p cv ->
            Buffer.add_char buf ' '
        | _ -> ());
        cv_to_buffer buf cv;
        loop (Some cv) rest
  in
  loop None cvs

let string_of_components cvs =
  let buf = Buffer.create 64 in
  cvs_to_buffer buf cvs;
  Buffer.contents buf

(* CSS Syntax 3 section 9.1: adjacent tokens must stay lexically separate.
   [word_like_end p]/[word_like_start n] test the byte [p] ends with / [n]
   starts with, since a merge depends on both. {!Func} is word-like at the start
   ([ident(]) but self-delimiting at the end ([)]); {!Block} is self-delimiting
   both ends and never needs separation. *)
let word_like_end : Component.t -> bool = function
  | Preserved
      {
        kind =
          ( Whitespace | Open _ | Close _ | Colon | Semicolon | Comma | Cdo
          | Cdc | Bad_string | Bad_url | Eof
          (* These delim characters are self-delimiting at the end, so a
             trailing [<delim>] never merges with what follows. *)
          | Hash _ | Percentage _
          | Delim
              ( "!" | "*" | "/" | ">" | "?" | "|" | "&" | "^" | "$" | "=" | "%"
              | "~" | "(" | ")" | "[" | "]" | "{" | "}" ) );
        _;
      } ->
      false
  | Preserved _ -> true
  (* CSS Color 4 sec. 11.1 relative colour needs whitespace between a [var()]
     (or other function) [<color>] arg and the following channel ident:
     [oklab(from var(--c) l a b)] tokenises fine as [var(--c)l] but spec-strict
     parsers expect the separator. So [Func]/Paren [Block] count as
     word-like-end to keep the [Func] + [Ident] boundary. *)
  | Func _ -> true
  | Block { node = { opening = Paren; _ }; _ } -> true
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

(* CSS Syntax section 9 fixed-pair separations: certain delim pairs would form a
   multi-char token (comment, CDO) when emitted adjacently, even though neither
   token is word-like. Force a separator for those. *)
let pair_forms_multichar_token prev next =
  match (prev, next) with
  | ( Component.Preserved { kind = Token.Delim "/"; _ },
      Component.Preserved { kind = Token.Delim "*"; _ } )
  | ( Component.Preserved { kind = Token.Delim "*"; _ },
      Component.Preserved { kind = Token.Delim "/"; _ } )
  | ( Component.Preserved { kind = Token.Delim "<"; _ },
      Component.Preserved { kind = Token.Delim "!"; _ } ) ->
      true
  | _ -> false

let pair_prefers_component_separator prev next =
  match (prev, next) with
  | ( Component.Preserved { kind = Token.Percentage _; _ },
      Component.Preserved
        {
          kind = Token.Number_tok _ | Token.Percentage _ | Token.Dimension _;
          _;
        } ) ->
      true
  | _ -> false

let pair_needs_token_boundary prev next =
  match (prev, next) with
  | _ when signed_number_pair prev next -> false
  | _ when pair_forms_multichar_token prev next -> true
  | _ when pair_prefers_component_separator prev next -> true
  | ( Component.Preserved
        {
          kind =
            ( Token.Ident _ | Token.At_keyword _ | Token.Hash _
            | Token.Dimension _ );
          _;
        },
      ( Component.Preserved
          {
            kind =
              ( Token.Ident _ | Token.Function _ | Token.Number_tok _
              | Token.Percentage _ | Token.Dimension _ );
            _;
          }
      | Component.Func _
      | Component.Block { node = { opening = Token.Paren; _ }; _ } ) ) ->
      true
  | ( Component.Preserved { kind = Token.Number_tok _; _ },
      Component.Preserved
        {
          kind =
            ( Token.Ident _ | Token.Function _ | Token.Number_tok _
            | Token.Percentage _ | Token.Dimension _ );
          _;
        } ) ->
      true
  | ( Component.Preserved { kind = Token.Delim ("-" | "#" | "@"); _ },
      ( Component.Preserved { kind = Token.Ident _ | Token.Function _; _ }
      | Component.Func _ ) ) ->
      true
  | ( Component.Preserved { kind = Token.Delim ("+" | "-"); _ },
      Component.Preserved
        {
          kind = Token.Number_tok _ | Token.Percentage _ | Token.Dimension _;
          _;
        } ) ->
      true
  | ( Component.Preserved { kind = Token.Delim "."; _ },
      Component.Preserved
        {
          kind =
            ( Token.Number_tok { repr; _ }
            | Token.Percentage { repr; _ }
            | Token.Dimension { number = { repr; _ }; _ } );
          _;
        } ) ->
      repr <> "" && repr.[0] >= '0' && repr.[0] <= '9'
  (* A hash token absorbs trailing name code points; a following [-] (which is a
     name code point) would extend it on re-tokenization. *)
  | ( Component.Preserved { kind = Token.Hash _; _ },
      Component.Preserved { kind = Token.Delim "-"; _ } ) ->
      true
  | _ -> false

let rec cv_to_buffer_min buf = function
  | Preserved t -> Buffer.add_string buf (string_of_token_kind t.kind)
  | Block { node = { opening; value; _ }; _ } ->
      Buffer.add_char buf (opening_char opening);
      cvs_to_buffer_min buf value;
      Buffer.add_char buf (closing_char opening)
  | Func { node = { name; arguments; _ }; _ } ->
      Buffer.add_string buf (escape_ident name);
      Buffer.add_char buf '(';
      cvs_to_buffer_min buf arguments;
      Buffer.add_char buf ')'

and cvs_to_buffer_min buf cvs =
  let rec drop_ws = function
    | cv :: rest when is_whitespace cv -> drop_ws rest
    | other -> other
  in
  let needs_separator prev next =
    match prev with
    | None -> false
    | Some p ->
        pair_forms_multichar_token p next
        || (not (signed_number_pair p next))
           && word_like_end p
           && (not (is_backslash_delim p))
           && word_like_start next
  in
  let rec loop prev separated = function
    | [] -> ()
    | cv :: rest when is_whitespace cv ->
        let rest' = drop_ws rest in
        let separated' =
          match rest' with
          | next :: _
            when needs_separator prev next
                 || Option.fold ~none:false
                      ~some:(fun p -> pair_prefers_component_separator p next)
                      prev ->
              Buffer.add_char buf ' ';
              true
          | _ -> separated
        in
        loop prev separated' rest'
    | cv :: rest ->
        (match prev with
        | Some p when (not separated) && pair_needs_token_boundary p cv ->
            Buffer.add_char buf ' '
        | _ -> ());
        cv_to_buffer_min buf cv;
        loop (Some cv) false rest
  in
  loop None false cvs

let to_string_minified cvs =
  if cvs <> [] && List.for_all is_whitespace cvs then " "
  else
    let buf = Buffer.create 64 in
    cvs_to_buffer_min buf cvs;
    Buffer.contents buf

let to_string_custom cvs =
  let buf = Buffer.create 64 in
  cvs_to_buffer buf cvs;
  Buffer.contents buf

(* Custom-property values are opaque token streams (CSS Custom Properties 1), so
   [to_string_custom] keeps authored whitespace. This minified rendering is for
   canonical output only: collapse optional whitespace in blocks and function
   args while preserving token boundaries. *)
let url_string_can_unquote s =
  not
    (String.exists
       (fun c ->
         c = ' ' || c = ')' || c = '"' || c = '\'' || c = '(' || c = '\\')
       s)

(* If [args] is a single [<string-token>] argument we can fold it into the
   bare-URL form [url(X)] when X has no special characters - per CSS Values L4
   §3.4 the two notations are equivalent and the bare form is shorter. *)
let url_args_as_bare_string args =
  let stripped =
    List.filter
      (function
        | Component.Preserved { kind = Token.Whitespace; _ } -> false
        | _ -> true)
      args
  in
  match stripped with
  | [ Component.Preserved { kind = Token.String { value; _ }; _ } ]
    when url_string_can_unquote value ->
      Some value
  | _ -> None

let rec drop_whitespace_components = function
  | cv :: rest when is_whitespace cv -> drop_whitespace_components rest
  | other -> other

let custom_min_is_math_delim = function
  | Component.Preserved { kind = Token.Delim ("*" | "/"); _ } -> true
  | _ -> false

let custom_min_is_bang = function
  | Component.Preserved { kind = Token.Delim "!"; _ } -> true
  | _ -> false

let custom_min_is_important = function
  | Component.Preserved { kind = Token.Ident s; _ }
    when String.lowercase_ascii s = "important" ->
      true
  | _ -> false

let custom_min_after_bang rest =
  match rest with _ :: more -> drop_whitespace_components more | [] -> []

let custom_min_bang_boundary prev next rest =
  match (prev, next) with
  | _, Component.Preserved { kind = Token.Delim "!"; _ } -> (
      match custom_min_after_bang rest with
      | head :: _ -> custom_min_is_important head
      | [] -> false)
  | Some p, _ when custom_min_is_bang p && custom_min_is_important next -> true
  | _ -> false

let custom_min_word_boundary p next =
  (not (signed_number_pair p next))
  && word_like_end p
  && (not (is_backslash_delim p))
  && word_like_start next

(* Whitespace in a custom-property value is part of the stream a var()
   substitution receives, so a separator around [*] and [/] is collapsed to one
   space, never deleted: [16 / 9] and [16/9] are distinct streams. *)
let custom_min_needs_separator prev next rest =
  match prev with
  | None -> false
  | Some p ->
      pair_forms_multichar_token p next
      || custom_min_is_math_delim p
      || custom_min_is_math_delim next
      || custom_min_bang_boundary prev next rest
      || custom_min_word_boundary p next

let custom_min_ws_separator buf prev separated rest =
  match rest with
  | next :: _ when custom_min_needs_separator prev next rest ->
      Buffer.add_char buf ' ';
      true
  | _ -> separated

let custom_min_item_separator buf prev separated cv =
  match prev with
  | Some p when (not separated) && pair_needs_token_boundary p cv ->
      Buffer.add_char buf ' '
  | _ -> ()

(* Idents are ASCII-case-insensitive per CSS Syntax 3 sec. 3, but the parser
   keeps source case so selectors stay case-sensitive. In a custom-property
   value only these keywords have a canonical lower-case spelling, so fold them
   ([--c:currentColor] == [--c:currentcolor]). Kept conservative: an
   unrecognised ident (e.g. a tw class name in a var body) passes through
   unchanged. *)
let case_insensitive_value_idents =
  let s = Hashtbl.create 16 in
  List.iter
    (fun k -> Hashtbl.add s k ())
    [
      "currentcolor";
      "transparent";
      "inherit";
      "initial";
      "unset";
      "revert";
      "revert-layer";
    ];
  s

let fold_value_ident s =
  let lower = String.lowercase_ascii s in
  if Hashtbl.mem case_insensitive_value_idents lower then lower else s

let string_of_custom_value_token ~fold_ident : Token.kind -> string = function
  | Token.Ident s -> escape_ident (fold_ident s)
  | other -> string_of_token_kind other

let rec cv_to_buffer_custom_min ~fold_ident buf : Component.t -> unit = function
  | Preserved t ->
      Buffer.add_string buf (string_of_custom_value_token ~fold_ident t.kind)
  | Block { node = { opening; value; _ }; _ } ->
      Buffer.add_char buf (opening_char opening);
      cvs_to_buffer_min_custom ~fold_ident buf value;
      Buffer.add_char buf (closing_char opening)
  | Func { node = { name; arguments; _ }; _ }
    when String.lowercase_ascii name = "url" -> (
      match url_args_as_bare_string arguments with
      | Some s ->
          Buffer.add_string buf "url(";
          Buffer.add_string buf s;
          Buffer.add_char buf ')'
      | None ->
          Buffer.add_string buf (escape_ident name);
          Buffer.add_char buf '(';
          cvs_to_buffer_min_custom ~fold_ident buf arguments;
          Buffer.add_char buf ')')
  | Func { node = { name; arguments; _ }; _ } ->
      Buffer.add_string buf (escape_ident name);
      Buffer.add_char buf '(';
      cvs_to_buffer_min_custom ~fold_ident buf arguments;
      Buffer.add_char buf ')'

(* Drops optional whitespace between sibling tokens (like [cvs_to_buffer_min])
   but routes children through [cv_to_buffer_custom_min] so nested function and
   block contents use the custom-property minifier recursively. *)
and cvs_to_buffer_min_custom ~fold_ident buf cvs =
  let rec loop prev separated = function
    | [] -> ()
    | cv :: rest when is_whitespace cv ->
        let rest' = drop_whitespace_components rest in
        let separated' = custom_min_ws_separator buf prev separated rest' in
        loop prev separated' rest'
    | cv :: rest ->
        custom_min_item_separator buf prev separated cv;
        cv_to_buffer_custom_min ~fold_ident buf cv;
        loop (Some cv) false rest
  in
  loop None false cvs

let to_string_custom_minified ?(fold_ident = fold_value_ident) cvs =
  if cvs <> [] && List.for_all is_whitespace cvs then " "
  else
    let buf = Buffer.create 64 in
    cvs_to_buffer_min_custom ~fold_ident buf cvs;
    Buffer.contents buf

(** {1 Rule / declaration consumers (section 5.3)} *)

(* Drop a run of whitespace tokens from [lexer]. Used by the entry-point parsers
   that need to honour the spec's "skip surrounding whitespace" steps without
   leaking the loop body. *)
let rec skip_whitespace_tokens lexer =
  match (Lexer.peek lexer).Token.kind with
  | Token.Whitespace ->
      let _ = Lexer.next lexer in
      skip_whitespace_tokens lexer
  | _ -> ()

(* Push a warning, attaching a source snippet from the lexer's reader when [meta
   = `Full] so section 5.3 recovery warnings carry the same context as raised
   Cursor errors. Lower meta levels skip the snippet allocation. *)
let warn ~meta lexer (warnings : Error.t list ref) (e : Error.t) =
  let e =
    match meta with
    | `Full ->
        let source = Lexer.source lexer in
        Error.v ~source ~loc:e.loc ~sort:e.sort e.kind
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
        if is_custom_property_shape prelude then None
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

(* Skip leading whitespace components from a buffered component-value list. *)
let rec drop_leading_ws = function
  | hd :: rest when is_whitespace hd -> drop_leading_ws rest
  | other -> other

(* Trim whitespace components from both ends of a buffered list. *)
let trim_ws cvs = drop_leading_ws cvs |> List.rev |> drop_leading_ws |> List.rev

let is_curly_block = function
  | Block { node = { opening = Token.Curly; _ }; _ } -> true
  | _ -> false

let has_bad_token value =
  let rec walk = function
    | Preserved { kind = Token.Bad_string | Token.Bad_url; _ } -> true
    | Block { node = { value; _ }; _ } -> List.exists walk value
    | Func { node = { arguments; _ }; _ } -> List.exists walk arguments
    | _ -> false
  in
  List.exists walk value

let value_has_invalid_block ~is_custom value =
  let trimmed = trim_ws value in
  let blocks = List.filter is_curly_block trimmed in
  match blocks with
  | [] -> false
  | _ :: _ :: _ -> true
  | [ block ] ->
      let rec split before = function
        | [] -> (List.rev before, [])
        | hd :: rest when hd == block -> (List.rev before, rest)
        | hd :: rest -> split (hd :: before) rest
      in
      let before, after = split [] trimmed in
      let non_ws = List.filter (fun cv -> not (is_whitespace cv)) in
      let before_has = non_ws before <> [] in
      let after_has = non_ws after <> [] in
      if is_custom then before_has && after_has else before_has || after_has

(* 5.3.7 Parse a declaration from a buffered component-value list. *)
let declaration_of_buffer ~meta lexer ~name ~name_loc ~warnings cvs :
    Component.declaration option =
  let is_custom = String.length name >= 2 && name.[0] = '-' && name.[1] = '-' in
  match drop_leading_ws cvs with
  | Preserved { kind = Token.Colon; _ } :: rest ->
      let value1 = trim_ws rest in
      let value, important =
        match List.rev value1 with
        | Preserved { kind = Token.Ident s; _ } :: rest
          when String.lowercase_ascii s = "important" -> (
            match drop_leading_ws rest with
            | Preserved { kind = Token.Delim "!"; _ } :: rest ->
                (trim_ws (List.rev rest), true)
            | _ -> (value1, false))
        | _ -> (value1, false)
      in
      if value_has_invalid_block ~is_custom value || has_bad_token value then (
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

let skip_bad_declaration lexer tok =
  let rec skip () =
    let t = Lexer.next lexer in
    match t.Token.kind with
    | Token.Semicolon | Token.Eof -> ()
    | _ ->
        let _ = consume_component_value_from lexer t in
        skip ()
  in
  let _ = consume_component_value_from lexer tok in
  skip ()

let consume_decl_from_ident ~meta lexer ~warnings ~name ~name_loc =
  let body = consume_declaration_body lexer in
  match declaration_of_buffer ~meta lexer ~name ~name_loc ~warnings body with
  | Some d -> Some (`Decl d)
  | None -> None

let consume_decl_list_item ~meta lexer ~warnings tok =
  match tok.Token.kind with
  | Token.Eof -> `Done
  | Token.Whitespace | Token.Semicolon | Token.Close Curly -> `Skip
  | Token.At_keyword name ->
      let ar = consume_at_rule lexer ~name ~start_loc:tok.loc in
      `Item (`At ar)
  | Token.Ident name -> (
      match
        consume_decl_from_ident ~meta lexer ~warnings ~name ~name_loc:tok.loc
      with
      | Some item -> `Item item
      | None -> `Skip)
  | _ ->
      warn ~meta lexer warnings
        (Error.unexpected_token tok.loc ~sort:Sort.Declaration tok.kind);
      skip_bad_declaration lexer tok;
      `Skip

let consume_list_of_declarations ~meta lexer ~warnings :
    [ `Decl of Component.declaration | `At of Component.at_rule ] list =
  let rec loop acc =
    let tok = Lexer.next lexer in
    match consume_decl_list_item ~meta lexer ~warnings tok with
    | `Done -> List.rev acc
    | `Skip -> loop acc
    | `Item item -> loop (item :: acc)
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

let stylesheet ?(meta = Loc.default_meta_level) r =
  with_warnings (fun ~warnings ->
      let lexer = Lexer.of_reader r in
      consume_list_of_rules ~meta lexer ~top_level:true ~warnings)

let stylesheet_contents = stylesheet

(* CSS Syntax Level 3 section 5.4.5: a block's contents is a mix of declarations
   and nested rules. Consecutive declarations are grouped into a single [`Decls]
   item so callers can re-emit them as a contiguous run. *)

(* Try to consume a nested qualified rule at the current [tok] position. On
   success, push it as a [`Rule] onto [result]; on failure, leave [result]
   unchanged. The caller is expected to have already reconsumed [tok]. *)
let try_consume_nested_qualified ~meta lexer ~start_loc ~warnings ~result =
  match
    consume_qualified_rule ~nested:true ~meta lexer ~start_loc ~warnings
  with
  | Some qr -> result := `Rule (Component.Qualified qr) :: !result
  | None -> ()

(* Section 5.5.5: an [Ident] in a block's contents may begin either a
   declaration or a nested qualified rule. Try declaration first; if it fails,
   rewind and parse a qualified rule. *)
let consume_block_ident ~meta lexer ~name ~tok ~warnings ~pending ~result ~flush
    =
  Lexer.save lexer;
  let body = consume_declaration_body lexer in
  let warnings_snapshot = !warnings in
  match
    declaration_of_buffer ~meta lexer ~name ~name_loc:tok.Token.loc ~warnings
      body
  with
  | Some d ->
      Lexer.commit lexer;
      pending := d :: !pending
  | None ->
      warnings := warnings_snapshot;
      Lexer.restore lexer;
      Lexer.reconsume lexer tok;
      flush ();
      try_consume_nested_qualified ~meta lexer ~start_loc:tok.loc ~warnings
        ~result

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
    | Token.Ident name ->
        consume_block_ident ~meta lexer ~name ~tok ~warnings ~pending ~result
          ~flush;
        loop ()
    | _ ->
        flush ();
        Lexer.reconsume lexer tok;
        try_consume_nested_qualified ~meta lexer ~start_loc:tok.loc ~warnings
          ~result;
        loop ()
  in
  loop ()

let block_contents ?(meta = Loc.default_meta_level) r : block_item list output =
  with_warnings (fun ~warnings ->
      let lexer = Lexer.of_reader r in
      consume_block_contents ~meta lexer ~warnings)

(* CSS Syntax Level 3 section 5.4.6 "Parse a rule": skip surrounding whitespace,
   consume one rule, require EOF, no extra rules or stray tokens afterwards. *)
let rule ?(meta = Loc.default_meta_level) r =
  with_warnings (fun ~warnings ->
      let lexer = Lexer.of_reader r in
      skip_whitespace_tokens lexer;
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
          skip_whitespace_tokens lexer;
          if (Lexer.peek lexer).Token.kind = Token.Eof then r' else None)

(* CSS Syntax Level 3 section 5.4.7 "Parse a declaration": skip leading
   whitespace, require an ident, consume exactly one declaration, ignore
   anything after the terminating ';' or EOF. The first non-whitespace token
   must be the declaration name -- a stray ':' or [@x] is a syntax error. *)
let declaration ?(meta = Loc.default_meta_level) r =
  with_warnings (fun ~warnings ->
      let lexer = Lexer.of_reader r in
      skip_whitespace_tokens lexer;
      match (Lexer.peek lexer).Token.kind with
      | Token.Ident name ->
          let tok = Lexer.next lexer in
          let body = consume_declaration_body lexer in
          declaration_of_buffer ~meta lexer ~name ~name_loc:tok.loc ~warnings
            body
      | _ -> None)

let list_of_declarations ?(meta = Loc.default_meta_level) r =
  with_warnings (fun ~warnings ->
      let lexer = Lexer.of_reader r in
      consume_list_of_declarations ~meta lexer ~warnings)

let list_of_rules ?(meta = Loc.default_meta_level) r =
  with_warnings (fun ~warnings ->
      let lexer = Lexer.of_reader r in
      consume_list_of_rules ~meta lexer ~top_level:false ~warnings)

let list_of_component_values r =
  with_warnings (fun ~warnings:_ ->
      let p = of_reader r in
      let rec loop acc =
        match next p with
        | Preserved { kind = Token.Eof; _ } -> List.rev acc
        | cv -> loop (cv :: acc)
      in
      loop [])

let rec next_non_ws p =
  match next p with
  | Preserved { kind = Token.Whitespace; _ } -> next_non_ws p
  | cv -> cv

let rec rest_is_ws_then_eof p =
  match next p with
  | Preserved { kind = Token.Eof; _ } -> true
  | Preserved { kind = Token.Whitespace; _ } -> rest_is_ws_then_eof p
  | _ -> false

let component_value r =
  with_warnings (fun ~warnings:_ ->
      let p = of_reader r in
      let first = next_non_ws p in
      match first with
      | Preserved { kind = Token.Eof; _ } -> None
      | _ -> if rest_is_ws_then_eof p then Some first else None)

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

let csv_component_values r =
  let out = list_of_component_values r in
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

let matches_grammar r grammar =
  let out = list_of_component_values r in
  let value = trim_component_value_whitespace out.value in
  if grammar value then { out with value = Some value }
  else { out with value = None }

let csv_by_grammar r grammar =
  let raw = list_of_component_values r in
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

let arbitrary_value r ~allow_top_level_semicolon_bang =
  let out = list_of_component_values r in
  let value = trim_component_value_whitespace out.value in
  if
    value <> []
    && arbitrary_value_tokens_ok ~allow_top_level_semicolon_bang ~top_level:true
         value
  then { out with value = Some value }
  else { out with value = None }

let declaration_value r =
  arbitrary_value r ~allow_top_level_semicolon_bang:false

let any_value r = arbitrary_value r ~allow_top_level_semicolon_bang:true
