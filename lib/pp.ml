module String_set = Set.Make (String)

type counter = { mutable count : int; mutable last : char }
(** A byte sink that records only the running length and the last emitted byte.
    It lets [size] measure output with no [Buffer] (hence no allocation): the
    last byte is all the emission-time lookback under minify needs, and the
    backward newline scan in [column] only runs in pretty mode. *)

type out = Buffer of Buffer.t | Counter of counter

type ctx = {
  minify : bool;
  level : int;  (** current nesting depth *)
  indent : int option;
      (** indent width per nesting level. [None] disables per-level indentation
          even when not minifying. *)
  out : out;
  inline : bool;
  in_function : bool;
  in_calc : bool;
  in_feature_query : bool;
      (** Set while serialising the value of an [@supports (property: value)]
          feature test. The value is a capability predicate for that exact
          syntax, so lossy rewrites (e.g. static colour folding) must be
          suppressed there. *)
  lossless : bool;
      (** Set under [--minify --lossless]: suppress colour-channel rounding and
          other colour approximations while keeping exact serialisation
          shortenings. *)
  enforce_spec : bool;
      (** Set under [--minify --enforce-spec]: emit the shortest spec-canonical
          serialisation but without evergreen-target facts, so target-dependent
          shortenings (e.g. the oklch/lch chroma number -> percentage swap) are
          suppressed. *)
}

type 'a t = ctx -> 'a -> unit

let emit_string out s =
  match out with
  | Buffer b -> Buffer.add_string b s
  | Counter c ->
      let n = String.length s in
      if n > 0 then (
        c.count <- c.count + n;
        c.last <- s.[n - 1])

let emit_char out ch =
  match out with
  | Buffer b -> Buffer.add_char b ch
  | Counter c ->
      c.count <- c.count + 1;
      c.last <- ch

let out_length = function Buffer b -> Buffer.length b | Counter c -> c.count

(* Only the backward newline scan in [column] reads positions other than the
   last byte, and that runs in pretty mode; under minify (the only mode
   [Counter] serves) there are no newlines, so the counter answers for the last
   byte and yields [\000] elsewhere. *)
let out_nth out i =
  match out with
  | Buffer b -> Buffer.nth b i
  | Counter c -> if i = c.count - 1 then c.last else '\000'

(* [resolve_indent ~minify indent]: under [minify] there is no indentation;
   otherwise pick the explicit value or the default 2-space indent. *)
let resolve_indent ~minify = function
  | Some _ as i -> i
  | None -> if minify then None else Some 2

let v ?(minify = false) ?indent ?(inline = false) ?(lossless = false)
    ?(enforce_spec = false) out =
  {
    minify;
    level = 0;
    indent = resolve_indent ~minify indent;
    out;
    inline;
    in_function = false;
    in_calc = false;
    in_feature_query = false;
    lossless;
    enforce_spec;
  }

let ctx ?minify ?indent ?inline ?lossless ?enforce_spec buf =
  v ?minify ?indent ?inline ?lossless ?enforce_spec (Buffer buf)

let to_buffer ?minify ?indent ?inline ?lossless ?enforce_spec buf pp a =
  let ctx = ctx ?minify ?indent ?inline ?lossless ?enforce_spec buf in
  pp ctx a

let to_string ?minify ?indent ?inline ?lossless ?enforce_spec pp a =
  let buf = Buffer.create 64 in
  to_buffer ?minify ?indent ?inline ?lossless ?enforce_spec buf pp a;
  Buffer.contents buf

(* Byte length of [pp a] with no allocation: the counter sink records only the
   running length and last byte, so there is no [Buffer] and no result
   string. *)
let size ?minify ?indent ?inline ?lossless ?enforce_spec pp a =
  let counter = { count = 0; last = '\000' } in
  let ctx =
    v ?minify ?indent ?inline ?lossless ?enforce_spec (Counter counter)
  in
  pp ctx a;
  counter.count

let nop _ _ = ()
let string ctx s = emit_string ctx.out s
let char ctx c = emit_char ctx.out c

let quoted ctx s =
  char ctx '"';
  string ctx s;
  char ctx '"'

(* The last byte emitted so far, for token-boundary spacing decisions. *)
let last_char ctx =
  let len = out_length ctx.out in
  if len = 0 then None else Some (out_nth ctx.out (len - 1))

let is_hex_digit = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

(* CSS Syntax 3 sec. 4.3.7: a hex escape consumes up to 6 hex digits and one
   trailing whitespace. The space terminator is only needed when the following
   character would otherwise be eaten by the escape - a hex digit or a
   whitespace. Omit it everywhere else for the shortest spelling. *)
let hex_escape_byte ctx ~next c =
  let code = Char.code c in
  let hex_digits = "0123456789abcdef" in
  char ctx '\\';
  if code >= 0x10 then char ctx hex_digits.[code lsr 4];
  char ctx hex_digits.[code land 0xF];
  match next with
  | Some (' ' | '\t' | '\n' | '\r' | '\012') -> char ctx ' '
  | Some c when is_hex_digit c -> char ctx ' '
  | _ -> ()

(* Output a string literal with CSS section 9.2 escaping: backslash for the
   delimiter and for '\', hex escapes for control bytes (U+0000..U+001F, U+007F)
   so the serialized form parses back to the same string. *)
let quoted_string ?(quote = '"') ctx s =
  char ctx quote;
  let len = String.length s in
  String.iteri
    (fun i c ->
      if c = quote then (
        char ctx '\\';
        char ctx c)
      else
        match c with
        | '\\' -> string ctx "\\\\"
        | '\x00' .. '\x1F' | '\x7F' ->
            let next = if i + 1 < len then Some s.[i + 1] else None in
            hex_escape_byte ctx ~next c
        | c -> char ctx c)
    s;
  char ctx quote

let sp ctx () = if not ctx.minify then char ctx ' '
let cut ctx () = if not ctx.minify then string ctx "\n" else ()

let nest n pp ctx a =
  let new_ctx = { ctx with level = ctx.level + n } in
  pp new_ctx a

let indent pp ctx a =
  (* Output [level * indent] spaces when an indent width is set. *)
  (match ctx.indent with
  | Some w when w > 0 -> string ctx (String.make (w * ctx.level) ' ')
  | _ -> ());
  pp ctx a

let ( ++ ) pp1 pp2 ctx a =
  pp1 ctx a;
  pp2 ctx a

let pair ?sep pp1 pp2 ctx (a, b) =
  pp1 ctx a;
  (match sep with Some s -> s ctx () | None -> ());
  pp2 ctx b

let triple ?sep pp1 pp2 pp3 ctx (a, b, c) =
  pp1 ctx a;
  (match sep with Some s -> s ctx () | None -> ());
  pp2 ctx b;
  (match sep with Some s -> s ctx () | None -> ());
  pp3 ctx c

let list ?sep pp ctx l =
  match l with
  | [] -> ()
  | [ x ] -> pp ctx x
  | h :: t ->
      pp ctx h;
      List.iter
        (fun x ->
          (match sep with Some s -> s ctx () | None -> ());
          pp ctx x)
        t

(* CSS Syntax 3 sec. 4 token-boundary separator: under minify, drop the space
   when the previous token ends with [)] or [%] - both close cleanly so the
   following ident/number cannot be re-tokenised into a single token. Falls back
   to a regular space in pretty mode. *)
let token_sp ctx () =
  if not ctx.minify then char ctx ' '
  else
    let len = out_length ctx.out in
    if len = 0 then ()
    else
      match out_nth ctx.out (len - 1) with ')' | '%' -> () | _ -> char ctx ' '

let column ctx =
  let len = out_length ctx.out in
  let rec find_newline i =
    if i < 0 then len
    else if out_nth ctx.out i = '\n' then len - i - 1
    else find_newline (i - 1)
  in
  find_newline (len - 1)

let list_wrap_append ~threshold ~wrap_sep ctx s =
  char ctx ',';
  if column ctx + 1 + String.length s > threshold then (
    char ctx '\n';
    string ctx wrap_sep)
  else char ctx ' ';
  string ctx s

let list_wrap ?(threshold = 80) ~sep ~wrap_indent pp ctx l =
  if ctx.minify then list ~sep pp ctx l
  else
    let measure_item x =
      let tmp = Buffer.create 64 in
      let tmp_ctx = { ctx with out = Buffer tmp } in
      pp tmp_ctx x;
      Buffer.contents tmp
    in
    let wrap_sep = String.make wrap_indent ' ' in
    match l with
    | [] -> ()
    | [ x ] -> pp ctx x
    | h :: t ->
        pp ctx h;
        ignore sep;
        List.iter
          (fun x -> list_wrap_append ~threshold ~wrap_sep ctx (measure_item x))
          t

let option ?(none = nop) pp ctx = function
  | None -> none ctx ()
  | Some x -> pp ctx x

let surround ~left ~right pp ctx a =
  left ctx ();
  pp ctx a;
  right ctx ()

(* Number formatting to be implemented according to the plan *)
let format_decimal ?(drop_leading_zero = false) s max_decimals is_neg =
  let len = String.length s in
  let s =
    if len < max_decimals then String.make (max_decimals - len) '0' ^ s else s
  in
  let len = String.length s in
  let point_pos = len - max_decimals in
  let int_part = if point_pos <= 0 then "0" else String.sub s 0 point_pos in
  let frac_part =
    if point_pos >= len then "" else String.sub s point_pos (len - point_pos)
  in

  (* Trim trailing zeros from fractional part *)
  let rec trim_zeros s =
    if String.ends_with ~suffix:"0" s then
      trim_zeros (String.sub s 0 (String.length s - 1))
    else s
  in
  let frac_part = trim_zeros frac_part in

  let final_str =
    if frac_part = "" then int_part
    else if drop_leading_zero && int_part = "0" then "." ^ frac_part
    else int_part ^ "." ^ frac_part
  in
  if is_neg then "-" ^ final_str else final_str

let trim_decimal_suffix s =
  if String.ends_with ~suffix:".0" s then String.sub s 0 (String.length s - 2)
  else if String.ends_with ~suffix:"." s then
    String.sub s 0 (String.length s - 1)
  else s

let strip_exponent_plus s =
  (* Remove redundant '+' in exponent notation: "3.40282e+38" -> "3.40282e38" *)
  match String.index_opt s 'e' with
  | Some i when i + 1 < String.length s && s.[i + 1] = '+' ->
      String.sub s 0 (i + 1) ^ String.sub s (i + 2) (String.length s - i - 2)
  | _ -> s

let format_integer is_neg abs_f f =
  if abs_f <= float_of_int max_int then
    let s = string_of_int (int_of_float abs_f) in
    if is_neg then "-" ^ s else s
  else
    (* Very large integer - use string_of_float and clean it up *)
    trim_decimal_suffix (string_of_float f) |> strip_exponent_plus

let format_decimal_value ~drop_leading_zero max_decimals is_neg abs_f =
  let scale = 10.0 ** float_of_int max_decimals in
  let scaled = floor ((abs_f *. scale) +. 0.5) in
  (* Extend the cap past the leading zeros only for a magnitude that would
     otherwise round to "0", so [0.000000001em] keeps its digits while a coarse
     [max_decimals] and colour-channel precision stay untouched. *)
  let max_decimals, scaled =
    if scaled <> 0.0 then (max_decimals, scaled)
    else
      let leading_zeros = -1 - int_of_float (floor (log10 abs_f)) in
      let d = max_decimals + min 24 (max 0 leading_zeros) in
      (d, floor ((abs_f *. (10.0 ** float_of_int d)) +. 0.5))
  in
  if scaled > float_of_int max_int then
    (* [format_decimal] splices the decimal point into what it takes for a pure
       digit string, but a scaled magnitude past [max_int] only stringifies
       through exponent notation. Print the magnitude itself, the way
       [format_integer] does over the same range. *)
    let s =
      trim_decimal_suffix (string_of_float abs_f) |> strip_exponent_plus
    in
    if is_neg then "-" ^ s else s
  else
    let s = string_of_int (int_of_float scaled) in
    format_decimal ~drop_leading_zero s max_decimals is_neg

let string_of_float ?(drop_leading_zero = false) ?(max_decimals = 8) f =
  (* Handle special cases first *)
  match classify_float f with
  | FP_zero -> "0"
  | FP_nan -> "NaN"
  | FP_infinite when f > 0.0 -> "3.40282e38"
  | FP_infinite -> "-3.40282e38"
  | FP_normal | FP_subnormal ->
      let is_neg = f < 0.0 in
      let abs_f = if is_neg then -.f else f in
      (* Check if this is an integer or needs decimal handling *)
      if abs_f = floor abs_f then format_integer is_neg abs_f f
      else format_decimal_value ~drop_leading_zero max_decimals is_neg abs_f

let round_sig n f =
  if f = 0.0 then 0.0
  else
    let d =
      Float.of_int (int_of_float (Float.ceil (Float.log10 (Float.abs f))))
    in
    let factor = 10.0 ** (Float.of_int n -. d) in
    Float.round (f *. factor) /. factor

(* Emit the decimal digits of [i] straight into the sink. [string_of_int] routes
   through C [snprintf] (slow) and allocates a string even when the sink only
   counts bytes; the optimizer measures declarations O(n^2) times, so this is
   hot. Recurse in the non-positive domain so [min_int] cannot overflow. *)
let int ctx i =
  if i < 0 then char ctx '-';
  let rec go n =
    if n <= -10 then go (n / 10);
    char ctx (Char.unsafe_chr (Char.code '0' - (n mod 10)))
  in
  go (if i > 0 then -i else i)

(* An integer-valued float prints as that integer (see [format_integer]); take
   the allocation-free [int] path instead of building a string via
   [string_of_float]. The bound mirrors [format_integer]'s own guard. *)
let float ctx f =
  if Float.is_integer f && Float.abs f <= float_of_int max_int then
    int ctx (int_of_float f)
  else string ctx (string_of_float ~drop_leading_zero:true f)

let float_compact = float

let float_n n ctx f =
  if Float.is_integer f && Float.abs f <= float_of_int max_int then
    int ctx (int_of_float f)
  else string ctx (string_of_float ~drop_leading_zero:true ~max_decimals:n f)

let hex ctx i =
  let hex_digit n =
    match n with
    | 10 -> 'A'
    | 11 -> 'B'
    | 12 -> 'C'
    | 13 -> 'D'
    | 14 -> 'E'
    | 15 -> 'F'
    | n -> char_of_int (n + int_of_char '0')
  in
  let rec to_hex n =
    if n = 0 then "0"
    else if n < 16 then String.make 1 (hex_digit n)
    else to_hex (n / 16) ^ to_hex (n mod 16)
  in
  string ctx (to_hex i)

let unit ctx f suffix =
  float ctx f;
  string ctx suffix

let pct ctx f =
  (* CSS Values 4 sec. 6.5 only allows the unit to drop on a zero [<length>]; a
     zero [<percentage>] keeps the [%] (otherwise [opacity:0] vs [opacity:0%]
     are no longer equivalent, and dimension/percentage-typed grammars reject a
     bare [0]). The coefficient prints in full, like [float]: rounding it
     changes the value (a repeating fraction such as [33.333333%] from [w-1/3]
     would lose digits), so any precision reduction belongs in the optimizer,
     not here. *)
  float ctx f;
  string ctx "%"

let sep ctx s =
  string ctx s;
  if not ctx.minify then char ctx ' '

let comma ctx () = sep ctx ","
let semicolon ctx () = char ctx ';'
let slash ctx () = char ctx '/'
let space ctx () = char ctx ' '
let block_open ctx () = char ctx '{'
let block_close ctx () = char ctx '}'
let minified ctx = ctx.minify
let in_feature_query ctx = ctx.in_feature_query
let enter_feature_query ctx = { ctx with in_feature_query = true }
let cond p a b ctx x = if p ctx then a ctx x else b ctx x
let space_if_pretty = sp

(* Operator character with conditional spacing *)
let op_char ctx c =
  space_if_pretty ctx ();
  char ctx c;
  space_if_pretty ctx ()

let braces pp =
  let open_ ctx () =
    block_open ctx ();
    if not ctx.minify then cut ctx ()
  in
  let close_ ctx () =
    if not ctx.minify then (
      cut ctx ();
      indent nop ctx ());
    block_close ctx ()
  in
  surround ~left:open_ ~right:close_ (nest 1 (indent pp))

let semicolon_cut ctx () =
  semicolon ctx ();
  cut ctx ()

(* [braces] indents the body's first line and emits the cuts around the braces
   itself, so the body prints the first item bare, re-indents every item after a
   separator's cut, and adds no leading or trailing cut. *)
let braced_items ?sep ?(trailing = nop) pp_item ctx items =
  braces
    (fun ctx () ->
      match items with
      | [] -> ()
      | first :: rest ->
          pp_item ctx first;
          List.iter
            (fun item ->
              (match sep with Some s -> s ctx () | None -> ());
              indent pp_item ctx item)
            rest;
          trailing ctx ())
    ctx ()

let braced_list ?sep pp_item ctx items = braced_items ?sep pp_item ctx items
let semicolon_if_pretty ctx () = if not ctx.minify then semicolon ctx ()

let braced_semicolon_list pp_item =
  braced_items ~sep:semicolon_cut ~trailing:semicolon_if_pretty pp_item

let call name pp_args ctx args =
  string ctx name;
  char ctx '(';
  pp_args ctx args;
  char ctx ')'

let call_list name pp_item = call name (list ~sep:comma pp_item)
let call_2 name pp_a pp_b = call name (pair ~sep:comma pp_a pp_b)
let call_3 name pp_a pp_b pp_c = call name (triple ~sep:comma pp_a pp_b pp_c)

let url ctx s =
  string ctx "url(";
  (* Only quote if the URL contains special characters *)
  let needs_quotes =
    String.exists
      (fun c ->
        c = ' ' || c = ')' || c = '"' || c = '\'' || c = '(' || c = '\\')
      s
  in
  if needs_quotes then quoted_string ctx s else string ctx s;
  string ctx ")"
