(** Stage 3 stream: Token.t -> Component.t.

    Ports the "consume a ..." algorithms from
    https://www.w3.org/TR/css-syntax-3/#parser-algorithms onto a {!Lexer.t}
    token stream, producing the IR defined in {!Component}. *)

open Component
open Syntax

type t = { lexer : Lexer.t; mutable lookback : Component.t option }

let of_lexer lexer = { lexer; lookback = None }
let of_reader ?unicode_ranges r = of_lexer (Lexer.of_reader ?unicode_ranges r)
let of_string ?unicode_ranges s = of_reader ?unicode_ranges (Reader.of_string s)

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
    | Token.Close b when Token.equal_bracket b opening ->
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

(* Hex-escape a control byte as "\HH " per CSS Syntax 3 (ED) sec. 9. *)
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

(* Emit the code point [s] holds at byte [i], honouring the leading-digit
   restriction recorded in [needs_leading_escape]. One that needs no escape is
   copied straight out of [s] rather than encoded again. *)
let escape_ident_emit_cp buf ~needs_leading_escape s i u =
  let cp = Uchar.to_int u in
  if needs_leading_escape then add_hex_escape_cp buf cp
  else if cp < 0x20 || cp = 0x7F then add_hex_escape_cp buf cp
  else if cp < 0x80 then escape_ident_emit_ascii buf cp
  else if Lexer.spec_non_ascii_ident_cp cp then
    Buffer.add_substring buf s i (Uchar.utf_8_byte_length u)
  else add_hex_escape_cp buf cp

(* Sec. 4.3.9 opens no ident on [-] then a digit, so the digit is escaped even
   though [-] is ident-start and the digit is ident-continue. *)
let escape_ident_starts_dash_digit s n =
  n >= 2 && s.[0] = '-' && s.[1] >= '0' && s.[1] <= '9'

let escape_ident_starts s n =
  let starts_with_digit = n > 0 && s.[0] >= '0' && s.[0] <= '9' in
  (starts_with_digit, escape_ident_starts_dash_digit s n)

let escape_ident_needs_leading (starts_with_digit, starts_dash_digit) i =
  (i = 0 && starts_with_digit) || (i = 1 && starts_dash_digit)

let escape_ident_emit_item buf starts s () i = function
  | Common.String.Scalar u ->
      let needs_leading_escape = escape_ident_needs_leading starts i in
      escape_ident_emit_cp buf ~needs_leading_escape s i u
  | Common.String.Malformed len ->
      (* Malformed UTF-8 bytes (e.g., a lone continuation byte the lexer's
         [consume_escape] dropped into an ident) can't be re-tokenized as the
         same ident. Hex-escape each byte so the serialized form round-trips. *)
      for j = i to i + len - 1 do
        add_hex_escape buf s.[j]
      done

(* [s] and [n] are parameters rather than free variables of an inner loop, which
   would cost a closure on every ident escaped. *)
let rec ascii_ident_continue_from s n i =
  i >= n
  || Syntax.is_ascii_ident_continue s.[i]
     && ascii_ident_continue_from s n (i + 1)

(* An ident serialises to itself byte-for-byte, so the buffer + walk below would
   allocate nothing useful. The empty name is not an ident and has nothing to
   escape either. *)
let escape_ident_needs_no_escape s n = n = 0 || Syntax.is_ident s

let escape_ident s =
  let n = String.length s in
  if n = 1 && s.[0] = '-' then "\\-"
  else if escape_ident_needs_no_escape s n then s
  else
    let buf = Buffer.create n in
    let starts = escape_ident_starts s n in
    Common.String.utf8_fold (escape_ident_emit_item buf starts s) () s;
    Buffer.contents buf

(* Every code point of an all-ASCII name serialises to itself, so the buffer +
   walk below would allocate nothing useful. *)
let escape_name_needs_no_escape s n = ascii_ident_continue_from s n 0

let escape_name s =
  let n = String.length s in
  if escape_name_needs_no_escape s n then s
  else
    let buf = Buffer.create n in
    let folder () i = function
      | Common.String.Scalar u ->
          escape_ident_emit_cp buf ~needs_leading_escape:false s i u
      | Common.String.Malformed len -> Buffer.add_substring buf s i len
    in
    Common.String.utf8_fold folder () s;
    Buffer.contents buf

let add_escaped_string buf ~quote ~terminated s =
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
  if terminated then Buffer.add_char buf quote

let add_escaped_url buf s =
  Buffer.add_string buf "url(";
  String.iter
    (fun c ->
      let code = Char.code c in
      if code < 0x20 || code = 0x7F then add_hex_escape buf c
      else if c = '"' || c = '\'' || c = '(' || c = ')' || c = '\\' || c = ' '
      then (
        Buffer.add_char buf '\\';
        Buffer.add_char buf c)
      else Buffer.add_char buf c)
    s;
  Buffer.add_char buf ')'

let digit_at s i = i < String.length s && s.[i] >= '0' && s.[i] <= '9'
let is_sign c = c = '+' || c = '-'

(* CSS Syntax 3 (ED) sec. 9 ambiguous-dimension rule: a unit of [e]/[E] then a
   (signed) digit would re-read as scientific notation, so the leading letter is
   hex-escaped to keep it out of the number's exponent. [digit_at] and [is_sign]
   are top-level so this test costs no closure per dimension printed. *)
let unit_starts_exponent unit_ =
  let len = String.length unit_ in
  len >= 2
  && (unit_.[0] = 'e' || unit_.[0] = 'E')
  && (digit_at unit_ 1 || (len >= 3 && is_sign unit_.[1] && digit_at unit_ 2))

let add_dimension_unit buf unit_ =
  if unit_starts_exponent unit_ then (
    add_hex_escape buf unit_.[0];
    Buffer.add_string buf
      (escape_ident (String.sub unit_ 1 (String.length unit_ - 1))))
  else Buffer.add_string buf (escape_ident unit_)

let add_unicode_range buf ~start_value ~end_value =
  Buffer.add_string buf "U+";
  let rec add_hex n acc =
    if n = 0 && acc = [] then Buffer.add_char buf '0'
    else if n = 0 then List.iter (Buffer.add_char buf) acc
    else add_hex (n / 16) (hex_digit (n mod 16) :: acc)
  in
  add_hex start_value [];
  if end_value <> start_value then (
    Buffer.add_char buf '-';
    add_hex end_value [])

(* Every token appends straight to the caller's buffer. All three callers are
   themselves buffer writers, so returning a [string] would allocate one per
   token whose text is assembled rather than read straight off the token. *)
let add_token_kind buf : Token.kind -> unit = function
  | Token.Ident s -> Buffer.add_string buf (escape_ident s)
  | Token.Function s ->
      Buffer.add_string buf (escape_ident s);
      Buffer.add_char buf '('
  | Token.At_keyword s ->
      Buffer.add_char buf '@';
      Buffer.add_string buf (escape_ident s)
  | Token.Hash { value; _ } ->
      Buffer.add_char buf '#';
      Buffer.add_string buf (escape_name value)
  | Token.String { value; quote = _; terminated } ->
      (* Normalize quoting to double-quote (the original quote is kept on the
         token only for quote-sensitive lookups like @charset). CSS Syntax 3
         (ED) sec. 4.3.5 recovers an unterminated string; the [terminated] flag
         is preserved so one round-trips, emitting without its closing quote. *)
      add_escaped_string buf ~quote:'"' ~terminated value
  | Token.Bad_string ->
      (* A bad string keeps no text, so serialize the shortest source that
         re-tokenizes as one, the way [Bad_url] serializes to [url(a b)]. CSS
         Syntax 3 (ED) sec. 4.3.5 makes one only from a newline inside a string
         and reconsumes that newline, so the quote needs the newline after it
         and the token is always followed by whitespace -- which is what the
         reconsumed newline lexes as, keeping the component count stable. *)
      Buffer.add_string buf "\"\n"
  | Token.Url s -> add_escaped_url buf s
  | Token.Bad_url -> Buffer.add_string buf "url(a b)"
  | Token.Delim "\\" -> Buffer.add_string buf "\\\n"
  | Token.Delim s -> Buffer.add_string buf s
  | Token.Number_tok { repr; _ } -> Buffer.add_string buf repr
  | Token.Percentage { repr; _ } ->
      Buffer.add_string buf repr;
      Buffer.add_char buf '%'
  | Token.Dimension { number; unit_ } ->
      Buffer.add_string buf number.repr;
      add_dimension_unit buf unit_
  | Token.Whitespace -> Buffer.add_char buf ' '
  | Token.Unicode_range { start_value; end_value; _ } ->
      add_unicode_range buf ~start_value ~end_value
  | Token.Cdo -> Buffer.add_string buf "<!--"
  | Token.Cdc -> Buffer.add_string buf "-->"
  | Token.Colon -> Buffer.add_char buf ':'
  | Token.Semicolon -> Buffer.add_char buf ';'
  | Token.Comma -> Buffer.add_char buf ','
  | Token.Open Square -> Buffer.add_char buf '['
  | Token.Close Square -> Buffer.add_char buf ']'
  | Token.Open Paren -> Buffer.add_char buf '('
  | Token.Close Paren -> Buffer.add_char buf ')'
  | Token.Open Curly -> Buffer.add_char buf '{'
  | Token.Close Curly -> Buffer.add_char buf '}'
  | Token.Eof -> ()

let is_signed_number_repr repr =
  String.length repr > 0 && (repr.[0] = '-' || repr.[0] = '+')

(* A numeric token's value, rather than its source representation, determines
   its meaning. Use the typed printer's compact spelling when it is shorter and
   reparses to the same float; the equality guard keeps high-precision authored
   values exact when the printer's decimal bound would round them. *)
let minified_number_repr ?(preserve_sign = false)
    ({ value; repr; _ } : Token.number) =
  let compact = Pp.string_of_float ~drop_leading_zero:true value in
  if preserve_sign && is_signed_number_repr repr then repr
  else if
    String.length compact < String.length repr
    &&
    match float_of_string_opt compact with
    | Some parsed -> Float.equal value parsed
    | None -> false
  then compact
  else repr

let add_minified_token_kind ?(preserve_numeric_sign = false) buf :
    Token.kind -> unit = function
  | Token.Number_tok number ->
      Buffer.add_string buf
        (minified_number_repr ~preserve_sign:preserve_numeric_sign number)
  | Token.Percentage number ->
      Buffer.add_string buf
        (minified_number_repr ~preserve_sign:preserve_numeric_sign number);
      Buffer.add_char buf '%'
  | Token.Dimension { number; unit_ } ->
      Buffer.add_string buf
        (minified_number_repr ~preserve_sign:preserve_numeric_sign number);
      add_dimension_unit buf unit_
  | kind -> add_token_kind buf kind

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

let numeric_repr ~minify number =
  if minify then minified_number_repr number else number.Token.repr

let signed_number_pair ~minify prev next =
  match (prev, next) with
  | ( Component.Preserved { kind = Token.Number_tok _; _ },
      Component.Preserved { kind = Token.Number_tok number; _ } ) ->
      is_signed_number_repr (numeric_repr ~minify number)
  | _ -> false

(* The sign a numeric token's serialisation opens with, per the [+]/[-] of the
   CSS Syntax 3 (ED) sec. 4.3.12 number grammar. *)
let numeric_leading_sign ~minify = function
  | Component.Preserved
      {
        kind =
          ( Token.Number_tok number
          | Token.Percentage number
          | Token.Dimension { number; _ } );
        _;
      }
    when is_signed_number_repr (numeric_repr ~minify number) ->
      Some (numeric_repr ~minify number).[0]
  | _ -> None

(* A [+] is no ident code point (CSS Syntax 3 (ED) sec. 4.2) and continues no
   number in front of it (sec. 4.3.12), so a plus-signed numeric re-lexes as its
   own token after every token the arms below would otherwise separate. A [-] is
   an ident code point and [--] starts an ident sequence (sec. 4.3.9), so a
   minus-signed one only stands apart after a number or a [+]. Emitting a
   separator for either would hand the next reader a token sequence the source
   never held. *)
let signed_numeric_self_separates ~minify prev next =
  match numeric_leading_sign ~minify next with
  | None -> false
  | Some '+' -> true
  | Some _ -> (
      match prev with
      | Component.Preserved { kind = Token.Number_tok _ | Token.Delim "+"; _ }
        ->
          true
      | _ -> false)

let normal_pair_needs_token_boundary prev next =
  match (prev, next) with
  | _ when signed_numeric_self_separates ~minify:false prev next -> false
  (* Sec. 4.3.4 turns [ident(] into a function token. An at-keyword, a hash and
     a dimension all end their name at the [(], so the block stays apart. *)
  | ( Component.Preserved { kind = Token.Ident _; _ },
      Component.Block { node = { opening = Token.Paren; _ }; _ } ) ->
      true
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
      | Component.Func _ ) ) ->
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
            ( Token.Number_tok number
            | Token.Percentage number
            | Token.Dimension { number; _ } );
          _;
        } ) ->
      let repr = numeric_repr ~minify:false number in
      repr <> "" && repr.[0] >= '0' && repr.[0] <= '9'
  | ( Component.Preserved { kind = Token.Hash _; _ },
      Component.Preserved { kind = Token.Delim "-"; _ } ) ->
      true
  | _ -> false

let rec cv_to_buffer buf : Component.t -> unit = function
  | Preserved t -> add_token_kind buf t.kind
  | Block { node = { opening; value; _ }; _ } ->
      Buffer.add_char buf (opening_char opening);
      cvs_to_buffer buf value;
      Buffer.add_char buf (closing_char opening)
  | Func { node = { name; arguments; _ }; _ } ->
      (* Always emit the closing [)]: section 5.5.10 still produces the function
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

(* CSS Syntax 3 (ED) section 9.1: adjacent tokens must stay lexically separate.
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
          (* Self-delimiting at the end: [%] closes its percentage token and
             these delims close themselves, so nothing that follows merges into
             them. Same rule the typed printer applies as [Pp.token_sp]. *)
          | Hash _ | Percentage _
          | Delim
              ( "!" | "*" | "/" | ">" | "?" | "|" | "&" | "^" | "$" | "=" | "%"
              | "~" | "(" | ")" | "[" | "]" | "{" | "}" ) );
        _;
      } ->
      false
  | Preserved _ -> true
  (* CSS Color 5 sec. 4.2 relative colour needs whitespace between a [var()] (or
     other function) [<color>] arg and the following channel ident: [oklab(from
     var(--c) l a b)] tokenises fine as [var(--c)l] but spec-strict parsers
     expect the separator. So [Func]/Paren [Block] count as word-like-end to
     keep the [Func] + [Ident] boundary. *)
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

(* CSS Syntax 3 (ED) sec. 9 fixed-pair separations: certain delim pairs would
   form a multi-char token (comment, CDO) when emitted adjacently, even though
   neither token is word-like. Force a separator for those. *)
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

let pair_needs_token_boundary ~minify_numeric prev next =
  match (prev, next) with
  | _ when signed_numeric_self_separates ~minify:minify_numeric prev next ->
      false
  | _ when pair_forms_multichar_token prev next -> true
  (* Sec. 4.3.4 turns [ident(] into a function token. An at-keyword, a hash and
     a dimension all end their name at the [(], so the block stays apart. *)
  | ( Component.Preserved { kind = Token.Ident _; _ },
      Component.Block { node = { opening = Token.Paren; _ }; _ } ) ->
      true
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
      | Component.Func _ ) ) ->
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
            ( Token.Number_tok number
            | Token.Percentage number
            | Token.Dimension { number; _ } );
          _;
        } ) ->
      let repr = numeric_repr ~minify:minify_numeric number in
      repr <> "" && repr.[0] >= '0' && repr.[0] <= '9'
  (* A hash token absorbs trailing name code points; a following [-] (which is a
     name code point) would extend it on re-tokenization. *)
  | ( Component.Preserved { kind = Token.Hash _; _ },
      Component.Preserved { kind = Token.Delim "-"; _ } ) ->
      true
  | _ -> false

let preserve_minified_numeric_sign ~in_math ~keep_authored_whitespace
    ~after_whitespace prev cv =
  match prev with
  | None -> false
  | Some p ->
      Option.is_some (numeric_leading_sign ~minify:false cv)
      && (in_math
         || (not (keep_authored_whitespace && after_whitespace))
            && signed_numeric_self_separates ~minify:false p cv
            && pair_needs_token_boundary ~minify_numeric:true p cv)

(* CSS Values 4 (ED) sec. 10.8 "Syntax" requires whitespace on both sides of the
   [+] and [-] operators of a math function, and allows [*] and [/] without any.
   Sec. 10 names the whole set: [calc()], the comparison functions, the
   stepped-value, trigonometric, exponential and sign-related families. One
   table serves every caller - a second copy drifts, and the passes over a
   custom-property stream then disagree about the same token. *)
let is_math_function name =
  match String.lowercase_ascii name with
  | "calc" | "min" | "max" | "clamp" | "round" | "mod" | "rem" | "sin" | "cos"
  | "tan" | "asin" | "acos" | "atan" | "atan2" | "pow" | "sqrt" | "hypot"
  | "log" | "exp" | "abs" | "sign" ->
      true
  | _ -> false

let is_plus_or_minus_delim = function
  | Component.Preserved { kind = Token.Delim ("+" | "-"); _ } -> true
  | _ -> false

(* Dropping a required separator gives a stream the browser rejects, so a
   minified serialiser keeps it even though neither neighbour is word-like.
   [in_math] enters at a math function's arguments and carries through a
   grouping paren, itself a math operand; a nested function has its own
   grammar. *)
let math_sign_boundary ~in_math prev next =
  in_math && (is_plus_or_minus_delim prev || is_plus_or_minus_delim next)

let block_in_math ~in_math : Token.bracket -> bool = function
  | Token.Paren -> in_math
  | Token.Square | Token.Curly -> false

let minified_stream_needs_separator ~minify_numbers ~in_math prev next =
  match prev with
  | None -> false
  | Some p ->
      let preserve_numeric_sign =
        minify_numbers
        && preserve_minified_numeric_sign ~in_math
             ~keep_authored_whitespace:false ~after_whitespace:true prev next
      in
      pair_forms_multichar_token p next
      || math_sign_boundary ~in_math p next
      || (not
            (signed_number_pair
               ~minify:(minify_numbers && not preserve_numeric_sign)
               p next))
         && word_like_end p
         && (not (is_backslash_delim p))
         && word_like_start next

let rec cv_to_buffer_min ~minify_numbers ~in_math ~preserve_numeric_sign buf =
  function
  | Preserved t ->
      if minify_numbers then
        add_minified_token_kind ~preserve_numeric_sign buf t.kind
      else add_token_kind buf t.kind
  | Block { node = { opening; value; _ }; _ } ->
      Buffer.add_char buf (opening_char opening);
      cvs_to_buffer_min ~minify_numbers
        ~in_math:(block_in_math ~in_math opening)
        buf value;
      Buffer.add_char buf (closing_char opening)
  | Func { node = { name; arguments; _ }; _ } ->
      Buffer.add_string buf (escape_ident name);
      Buffer.add_char buf '(';
      cvs_to_buffer_min ~minify_numbers ~in_math:(is_math_function name) buf
        arguments;
      Buffer.add_char buf ')'

and cvs_to_buffer_min ~minify_numbers ~in_math buf cvs =
  let rec drop_ws = function
    | cv :: rest when is_whitespace cv -> drop_ws rest
    | other -> other
  in
  let rec loop prev separated after_whitespace = function
    | [] -> ()
    | cv :: rest when is_whitespace cv ->
        let rest' = drop_ws rest in
        let separated' =
          match rest' with
          | next :: _
            when minified_stream_needs_separator ~minify_numbers ~in_math prev
                   next ->
              Buffer.add_char buf ' ';
              true
          | _ -> separated
        in
        loop prev separated' true rest'
    | cv :: rest ->
        let preserve_numeric_sign =
          minify_numbers
          && preserve_minified_numeric_sign ~in_math
               ~keep_authored_whitespace:false ~after_whitespace prev cv
        in
        (match prev with
        | Some p
          when (not separated)
               && (not preserve_numeric_sign)
               && pair_needs_token_boundary ~minify_numeric:minify_numbers p cv
          ->
            Buffer.add_char buf ' '
        | _ -> ());
        cv_to_buffer_min ~minify_numbers ~in_math ~preserve_numeric_sign buf cv;
        loop (Some cv) false false rest
  in
  loop None false false cvs

let to_string_minified_with ~minify_numbers cvs =
  if cvs <> [] && List.for_all is_whitespace cvs then " "
  else
    let buf = Buffer.create 64 in
    cvs_to_buffer_min ~minify_numbers ~in_math:false buf cvs;
    Buffer.contents buf

let to_string_minified cvs = to_string_minified_with ~minify_numbers:false cvs

let to_string_minified_numbers cvs =
  to_string_minified_with ~minify_numbers:true cvs

let url_string_can_unquote s =
  not
    (String.exists
       (fun c ->
         c = ' ' || c = ')' || c = '"' || c = '\'' || c = '(' || c = '\\')
       s)

(* If [args] is a single [<string-token>] argument we can fold it into the
   bare-URL form [url(X)] when X has no special characters - per CSS Values L4
   sec. 4.5 the two notations are equivalent and the bare form is shorter. *)
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

let custom_min_word_boundary ~minify p next =
  (not (signed_number_pair ~minify p next))
  && word_like_end p
  && (not (is_backslash_delim p))
  && word_like_start next

(* Whitespace in a custom-property value is part of the stream a var()
   substitution receives, so a separator around [*] and [/] is collapsed to one
   space, never deleted: [16 / 9] and [16/9] are distinct streams. Around a
   math-function [+] or [-] the space is required outright. *)
let custom_min_needs_separator ~in_math prev next rest =
  match prev with
  | None -> false
  | Some p ->
      let minify_numeric =
        (not in_math)
        || Option.is_none (numeric_leading_sign ~minify:false next)
      in
      pair_forms_multichar_token p next
      || custom_min_is_math_delim p
      || custom_min_is_math_delim next
      || math_sign_boundary ~in_math p next
      || custom_min_bang_boundary prev next rest
      || custom_min_word_boundary ~minify:minify_numeric p next

let custom_min_ws_separator ~in_math buf prev separated rest =
  match rest with
  | next :: _ when custom_min_needs_separator ~in_math prev next rest ->
      Buffer.add_char buf ' ';
      true
  | _ -> separated

let custom_min_item_separator ~preserve_numeric_sign buf prev separated cv =
  match prev with
  | Some p
    when (not separated)
         && (not preserve_numeric_sign)
         && pair_needs_token_boundary ~minify_numeric:true p cv ->
      Buffer.add_char buf ' '
  | _ -> ()

(* Idents are ASCII-case-insensitive per CSS Values 4 sec. 4.1, but the parser
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

let add_custom_value_token ~fold_ident ~preserve_numeric_sign buf :
    Token.kind -> unit = function
  | Token.Ident s -> Buffer.add_string buf (escape_ident (fold_ident s))
  | other -> add_minified_token_kind ~preserve_numeric_sign buf other

let rec cv_to_buffer_custom_min ~fold_ident ~in_math ~preserve_numeric_sign buf
    : Component.t -> unit = function
  | Preserved t ->
      add_custom_value_token ~fold_ident ~preserve_numeric_sign buf t.kind
  | Block { node = { opening; value; _ }; _ } ->
      Buffer.add_char buf (opening_char opening);
      cvs_to_buffer_min_custom ~fold_ident
        ~in_math:(block_in_math ~in_math opening)
        buf value;
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
          cvs_to_buffer_min_custom ~fold_ident ~in_math:false buf arguments;
          Buffer.add_char buf ')')
  | Func { node = { name; arguments; _ }; _ } ->
      Buffer.add_string buf (escape_ident name);
      Buffer.add_char buf '(';
      cvs_to_buffer_min_custom ~fold_ident ~in_math:(is_math_function name) buf
        arguments;
      Buffer.add_char buf ')'

(* Drops optional whitespace between sibling tokens (like [cvs_to_buffer_min])
   but routes children through [cv_to_buffer_custom_min] so nested function and
   block contents use the custom-property minifier recursively. *)
and cvs_to_buffer_min_custom ~fold_ident ~in_math buf cvs =
  let rec loop prev separated after_whitespace = function
    | [] -> ()
    | cv :: rest when is_whitespace cv ->
        let rest' = drop_whitespace_components rest in
        let separated' =
          custom_min_ws_separator ~in_math buf prev separated rest'
        in
        loop prev separated' true rest'
    | cv :: rest ->
        let preserve_numeric_sign =
          preserve_minified_numeric_sign ~in_math ~keep_authored_whitespace:true
            ~after_whitespace prev cv
        in
        custom_min_item_separator ~preserve_numeric_sign buf prev separated cv;
        cv_to_buffer_custom_min ~fold_ident ~in_math ~preserve_numeric_sign buf
          cv;
        loop (Some cv) false false rest
  in
  loop None false false cvs

(* Custom-property values are opaque token streams (CSS Custom Properties 1), so
   [string_of_components] keeps every optional whitespace token. This minified
   rendering is for canonical output only: collapse optional whitespace in
   blocks and function args while preserving token boundaries. *)
let to_string_custom_minified ?(fold_ident = fold_value_ident) cvs =
  if cvs <> [] && List.for_all is_whitespace cvs then " "
  else
    let buf = Buffer.create 64 in
    cvs_to_buffer_min_custom ~fold_ident ~in_math:false buf cvs;
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
   Cursor errors. Lower meta levels skip the snippet allocation. [recovery] is
   what this site did with the construct the error is about. *)
let warn ~meta lexer (warnings : Error.t list ref) ~recovery (e : Error.t) =
  let e =
    match meta with
    | `Full ->
        let source = Lexer.source lexer in
        Error.v ~source ~loc:e.loc ~sort:e.sort e.kind
    | `None | `Locs -> e
  in
  warnings := Error.with_recovery recovery e :: !warnings

(* Sections 5.5.9 and 5.5.10 auto-close a simple block and a function at EOF,
   which every rule-level caller relies on, and the CR snapshot marks both of
   those EOF branches a parse error. Report the repair so [Css.of_string] keeps
   its promise: an unclosed block otherwise swallows the rest of the input in
   silence. A function left open at EOF always sits inside such a block, so the
   enclosing block accounts for it and the rule is reported once. *)
let warn_unclosed ~meta lexer warnings (block : Component.block Component.node)
    =
  if not block.node.closed then
    warn ~meta lexer warnings ~recovery:Error.Recovery.Recovered
      (Error.unterminated block.loc Sort.Block)

(* CSS Syntax 3 (ED) sec. 5.5.2. [nested = true] also terminates on a stray
   ['}'] (the spec's "outermost block ended") so block-contents callers can
   recover instead of swallowing the closing delimiter. *)
let consume_at_rule ?(nested = false) ~meta lexer ~name ~start_loc ~warnings :
    Component.at_rule =
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
        warn_unclosed ~meta lexer warnings block;
        close prelude block.loc (Some block)
    | _ ->
        let _ = Lexer.next lexer in
        let cv = consume_component_value_from lexer tok in
        loop (cv :: prelude)
  in
  loop []

(* A prelude whose first two non-whitespace items are an ident starting with
   [--] followed by ':' reads as a declaration rather than a selector. The
   prelude is held reversed, as [consume_qualified_rule] accumulates it. *)
let is_custom_property_shape prelude =
  let rec drop_ws = function
    | Component.Preserved { kind = Token.Whitespace; _ } :: rest -> drop_ws rest
    | other -> other
  in
  match drop_ws (List.rev prelude) with
  | Component.Preserved { kind = Token.Ident name; _ } :: rest
    when Custom_property_name.has_prefix name -> (
      match drop_ws rest with
      | Component.Preserved { kind = Token.Colon; _ } :: _ -> true
      | _ -> false)
  | _ -> false

(* CSS Syntax 3 (ED) sec. 5.5.3. [nested = true] makes a stray ['}'] or a
   top-level ';' before any block end the rule attempt with [None]; the spec
   groups those two with the EOF branch as parse errors, so they are reported
   the same way. A custom-property-shaped prelude discards the rule, warning
   that it read as a declaration. *)
let consume_qualified_rule ?(nested = false) ~meta lexer ~start_loc ~warnings :
    Component.qualified_rule option =
  let drop end_loc =
    (* [start_loc] is the rule's first token and [end_loc] the one that ended
       the attempt, so the union spans the rule the sheet loses. *)
    let loc = Loc.union start_loc end_loc in
    warn ~meta lexer warnings
      ~recovery:Error.Recovery.(dropped ~source:(Lexer.source lexer) ~loc Rule)
      (Error.unterminated loc Sort.Qualified_rule);
    None
  in
  let rec loop prelude =
    let tok = Lexer.peek lexer in
    match tok.Token.kind with
    | Token.Eof ->
        let _ = Lexer.next lexer in
        drop tok.loc
    | Token.Semicolon when nested ->
        let _ = Lexer.next lexer in
        drop tok.loc
    | Token.Close Curly when nested -> drop tok.loc
    | Token.Open Curly ->
        let _ = Lexer.next lexer in
        let block = consume_simple_block lexer Curly ~start_loc:tok.loc in
        warn_unclosed ~meta lexer warnings block;
        let loc = Loc.union start_loc block.loc in
        if is_custom_property_shape prelude then (
          (* Dropping the rule is what the spec asks for, but it still has to be
             reported: [Css.of_string] warns for every rule it drops. *)
          warn ~meta lexer warnings
            ~recovery:Error.Recovery.(dropped Rule)
            (Error.sort_mismatch loc ~sort:Sort.Qualified_rule
               ~expected:Sort.Selector ~found:Sort.Declaration);
          None)
        else Some { node = { prelude = List.rev prelude; block }; loc }
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
        let ar =
          consume_at_rule ~meta lexer ~name ~start_loc:tok.loc ~warnings
        in
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
  let is_custom = Custom_property_name.has_prefix name in
  (* [name_loc] opens the declaration and [cvs] is its body up to the [;], so
     the union spans what the sheet loses when the pair is refused. *)
  let dropped () =
    let loc =
      List.fold_left
        (fun l cv -> Loc.union l (Component.source_loc cv))
        name_loc (trim_ws cvs)
    in
    Error.Recovery.dropped ~source:(Lexer.source lexer) ~loc
      Error.Recovery.Declaration
  in
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
        warn ~meta lexer warnings ~recovery:(dropped ())
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
      warn ~meta lexer warnings ~recovery:(dropped ())
        (Error.missing_token name_loc ~sort:Sort.Declaration "':'");
      None

(* Buffer component values until the terminating ';' or EOF (CSS Syntax 3 (ED)
   sec. 5.5.6 declaration body). Shared by the list, single-declaration and
   block-contents entry points.

   Section 5.5.7 also stops on a '}' when [nested] is true, the flag section
   5.5.5 passes to "consume a declaration". A balanced '}' is eaten by
   [consume_component_value_from] along with its opening brace, so one reaching
   this loop closes an enclosing block: hand it back rather than swallow it and
   the rest of the block with it. *)
let consume_declaration_body lexer =
  let rec loop acc =
    let t = Lexer.next lexer in
    match t.Token.kind with
    | Token.Semicolon | Token.Eof -> List.rev acc
    | Token.Close Curly ->
        Lexer.reconsume lexer t;
        List.rev acc
    | _ -> loop (consume_component_value_from lexer t :: acc)
  in
  loop []

(* Answers where the skip stopped, so the caller can report the whole
   declaration it threw away and not just the token it started on. *)
let skip_bad_declaration lexer tok =
  let rec skip last =
    let t = Lexer.next lexer in
    match t.Token.kind with
    | Token.Semicolon -> t.loc
    | Token.Eof -> last
    | _ -> skip (Component.source_loc (consume_component_value_from lexer t))
  in
  skip (Component.source_loc (consume_component_value_from lexer tok))

let consume_decl_from_ident ~meta lexer ~warnings ~name ~name_loc =
  (* CSS Syntax 3 (ED) sec. 5.5.11: the value of a unicode-range descriptor is
     the one place in the language the tokenizer is asked for unicode ranges. *)
  let body =
    if String.equal (String.lowercase_ascii name) "unicode-range" then
      Lexer.with_unicode_ranges lexer (fun () -> consume_declaration_body lexer)
    else consume_declaration_body lexer
  in
  match declaration_of_buffer ~meta lexer ~name ~name_loc ~warnings body with
  | Some d -> Some (`Decl d)
  | None -> None

let consume_decl_list_item ~meta lexer ~warnings tok =
  match tok.Token.kind with
  | Token.Eof -> `Done
  | Token.Whitespace | Token.Semicolon | Token.Close Curly -> `Skip
  | Token.At_keyword name ->
      let ar = consume_at_rule ~meta lexer ~name ~start_loc:tok.loc ~warnings in
      `Item (`At ar)
  | Token.Ident name -> (
      match
        consume_decl_from_ident ~meta lexer ~warnings ~name ~name_loc:tok.loc
      with
      | Some item -> `Item item
      | None -> `Skip)
  | _ ->
      let end_loc = skip_bad_declaration lexer tok in
      let loc = Loc.union tok.loc end_loc in
      warn ~meta lexer warnings
        ~recovery:
          Error.Recovery.(dropped ~source:(Lexer.source lexer) ~loc Declaration)
        (Error.unexpected_token tok.loc ~sort:Sort.Declaration tok.kind);
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

let stylesheet ?(meta = Loc.default_meta_level) ?on_comment r =
  with_warnings (fun ~warnings ->
      let lexer = Lexer.of_reader ?on_comment r in
      consume_list_of_rules ~meta lexer ~top_level:true ~warnings)

let stylesheet_contents ?meta r = stylesheet ?meta r

(* CSS Syntax 3 (ED) sec. 5.4.5: a block's contents is a mix of declarations and
   nested rules. Consecutive declarations are grouped into a single [`Decls]
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
        let ar =
          consume_at_rule ~nested:true ~meta lexer ~name ~start_loc:tok.loc
            ~warnings
        in
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

(* CSS Syntax 3 (ED) sec. 5.4.6 "Parse a rule": skip surrounding whitespace,
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
            Some
              (Component.At
                 (consume_at_rule ~meta lexer ~name ~start_loc:tok.loc ~warnings))
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

(* CSS Syntax 3 (ED) sec. 5.4.7 "Parse a declaration": skip leading whitespace,
   require an ident, consume exactly one declaration, ignore anything after the
   terminating ';' or EOF. The first non-whitespace token must be the
   declaration name -- a stray ':' or [@x] is a syntax error. *)
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
