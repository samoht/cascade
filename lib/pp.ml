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
  in_style_rule : bool;
      (** Set while serialising a style rule's contents, where CSS nesting
          applies and a statement-form at-rule is not accepted. *)
  lossless : bool;
      (** Set under [--minify --lossless]: suppress colour-channel rounding and
          other colour approximations while keeping exact serialisation
          shortenings. *)
  enforce_spec : bool;
  exact_numbers : bool;
      (** Set while printing a factor that an unresolved factor scales: a number
          prints the shortest digits of its double. *)
  rename_custom_property : string -> string;
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
    ?(enforce_spec = false) ?(rename_custom_property = Fun.id) out =
  {
    minify;
    level = 0;
    indent = resolve_indent ~minify indent;
    out;
    inline;
    in_style_rule = false;
    lossless;
    enforce_spec;
    exact_numbers = false;
    rename_custom_property;
  }

let ctx ?minify ?indent ?inline ?lossless ?enforce_spec ?rename_custom_property
    buf =
  v ?minify ?indent ?inline ?lossless ?enforce_spec ?rename_custom_property
    (Buffer buf)

let to_buffer ?minify ?indent ?inline ?lossless ?enforce_spec
    ?rename_custom_property buf pp a =
  let ctx =
    ctx ?minify ?indent ?inline ?lossless ?enforce_spec ?rename_custom_property
      buf
  in
  pp ctx a

let to_string ?minify ?indent ?inline ?lossless ?enforce_spec
    ?rename_custom_property pp a =
  let buf = Buffer.create 64 in
  to_buffer ?minify ?indent ?inline ?lossless ?enforce_spec
    ?rename_custom_property buf pp a;
  Buffer.contents buf

let render ctx pp a =
  to_string ~minify:ctx.minify ?indent:ctx.indent ~inline:ctx.inline
    ~lossless:ctx.lossless ~enforce_spec:ctx.enforce_spec
    ~rename_custom_property:ctx.rename_custom_property pp a

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

(* The last byte emitted so far, for token-boundary spacing decisions. *)
let last_char ctx =
  let len = out_length ctx.out in
  if len = 0 then None else Some (out_nth ctx.out (len - 1))

let is_hex_digit = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

(* CSS Syntax 3 (ED) sec. 4.3.7: a hex escape consumes up to 6 hex digits and
   one trailing whitespace. The space terminator is only needed when the
   following character would otherwise be eaten by the escape - a hex digit or a
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

(* Output a string literal with CSSOM 1 sec. 2.1 escaping: backslash for the
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

(* CSS Syntax 3 (ED) sec. 4 token-boundary separator: under minify, drop the
   space when the previous token ends with [)] or [%] - both close cleanly so
   the following ident/number cannot be re-tokenised into a single token. Falls
   back to a regular space in pretty mode. *)
let token_sp ctx () =
  if not ctx.minify then char ctx ' '
  else
    let len = out_length ctx.out in
    if len = 0 then ()
    else
      match out_nth ctx.out (len - 1) with ')' | '%' -> () | _ -> char ctx ' '

(* CSS Syntax 3 (ED) sec. 4: a <percentage-token> ends at its [%], so the
   separator after one carries no information and elides under minify. Narrower
   than [token_sp], which also elides after [)]: a function is word-like at its
   end, and the custom-value serialiser keeps that separator, so eliding it here
   would make a constructed value disagree with its own reparse. Reads the
   emitted byte rather than the node, so a spelling that does not end on [%]
   ([calc(NaN*1%)]) stays spaced. *)
let pct_sp ctx () =
  if not ctx.minify then char ctx ' '
  else
    let len = out_length ctx.out in
    if len = 0 then char ctx ' '
    else match out_nth ctx.out (len - 1) with '%' -> () | _ -> char ctx ' '

let column ctx =
  let len = out_length ctx.out in
  let rec find_newline i =
    if i < 0 then len
    else if out_nth ctx.out i = '\n' then len - i - 1
    else find_newline (i - 1)
  in
  find_newline (len - 1)

let truncate_out out len =
  match out with
  | Buffer b -> Buffer.truncate b len
  | Counter c -> c.count <- len

let is_layout_whitespace = function
  | ' ' | '\t' | '\n' | '\r' | '\012' -> true
  | _ -> false

let trim_separator_layout ctx start =
  let rec find i =
    if i >= start && is_layout_whitespace (out_nth ctx.out i) then find (i - 1)
    else i + 1
  in
  truncate_out ctx.out (find (out_length ctx.out - 1))

let list_wrap_append ~threshold ~sep ~wrap_sep ctx s =
  let separator_start = out_length ctx.out in
  sep ctx ();
  if column ctx + String.length s > threshold then (
    (* A pretty separator may end in layout space (for example [, ]). Replace
       that layout with the wrapped layout while preserving the separator
       itself. *)
    trim_separator_layout ctx separator_start;
    char ctx '\n';
    string ctx wrap_sep)
  else ();
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
        List.iter
          (fun x ->
            list_wrap_append ~threshold ~sep ~wrap_sep ctx (measure_item x))
          t

let option ?(none = nop) pp ctx = function
  | None -> none ctx ()
  | Some x -> pp ctx x

let surround ~left ~right pp ctx a =
  left ctx ();
  pp ctx a;
  right ctx ()

(* The digit at [i] of [s] left-padded with [pad] zeros. *)
let padded s pad i = if i < pad then '0' else s.[i - pad]

let rec trim_trailing_zeros s pad ~point i =
  if i > point && padded s pad (i - 1) = '0' then
    trim_trailing_zeros s pad ~point (i - 1)
  else i

let add_digits b s pad lo hi =
  for i = lo to hi - 1 do
    Buffer.add_char b (padded s pad i)
  done

(* [s] is the digit string of a magnitude scaled by [10^max_decimals]. Every
   non-integral number printed goes through here, so the digits are read in
   place, zero-padded on the left to [max_decimals], and written once. *)
let format_decimal ?(drop_leading_zero = false) s max_decimals is_neg =
  let pad = max 0 (max_decimals - String.length s) in
  let len = String.length s + pad in
  let point = len - max_decimals in
  let frac_end = trim_trailing_zeros s pad ~point len in
  let has_frac = frac_end > point in
  let int_is_zero = point = 0 || (point = 1 && padded s pad 0 = '0') in
  let b = Buffer.create (len + 3) in
  if is_neg then Buffer.add_char b '-';
  if not (has_frac && drop_leading_zero && int_is_zero) then
    if point = 0 then Buffer.add_char b '0' else add_digits b s pad 0 point;
  if has_frac then (
    Buffer.add_char b '.';
    add_digits b s pad point frac_end);
  Buffer.contents b

(* Every digit the number printers write is decided below in OCaml, from the
   bits of the double. The C library's formatting is never asked for a digit, so
   the bytes are the same whichever runtime rounds an exact half which way. *)

(* [pow10.(k)] is [10^k], exact up to [10^22]. *)
let pow10 =
  let a = Array.make 23 1.0 in
  for k = 1 to 22 do
    a.(k) <- a.(k - 1) *. 10.0
  done;
  a

(* A natural number as base-[limb] limbs, least significant first. *)
let limb = 1_000_000_000

(* Multiply the first [len] limbs of [a] by [k <= 5^13] in place, so that no
   intermediate passes [max_int]; the new length. *)
let mul_limbs a len k =
  let len = ref len and carry = ref 0 in
  for i = 0 to !len - 1 do
    let v = (a.(i) * k) + !carry in
    a.(i) <- v mod limb;
    carry := v / limb
  done;
  while !carry > 0 do
    a.(!len) <- !carry mod limb;
    carry := !carry / limb;
    incr len
  done;
  !len

let rec mul_pow a len base n =
  if n = 0 then len
  else
    let step = min n 13 in
    let k = ref 1 in
    for _ = 1 to step do
      k := !k * base
    done;
    mul_pow a (mul_limbs a len !k) base (n - step)

let digits_of_limbs a len =
  let b = Bytes.create (9 * len) in
  for i = 0 to len - 1 do
    let v = ref a.(i) and last = (9 * (len - i)) - 1 in
    for j = 0 to 8 do
      Bytes.set b (last - j) (Char.unsafe_chr (Char.code '0' + (!v mod 10)));
      v := !v / 10
    done
  done;
  let rec first i = if Bytes.get b i = '0' then first (i + 1) else i in
  let i = first 0 in
  Bytes.sub_string b i (Bytes.length b - i)

(* A positive finite [x] as [(m, b)], [x = m * 2^b] with [m < 2^53]. *)
let binary x =
  let bits = Int64.bits_of_float x in
  let biased = Int64.to_int (Int64.shift_right_logical bits 52) in
  let m = Int64.to_int (Int64.logand bits 0xF_FFFF_FFFF_FFFFL) in
  if biased = 0 then (m, -1074) else (m lor (1 lsl 52), biased - 1075)

(* [m * 2^b], for [0 < m < 2^55], as [(s, e)]: [s * 10^e] with [s] a digit
   string and no leading zero. It is an integer for [b >= 0], and [m * 5^-b *
   10^b] below; the longest, [m * 5^1076], has 769 digits, 86 limbs. *)
let exact_digits m b =
  let a = Array.make 90 0 in
  a.(0) <- m mod limb;
  a.(1) <- m / limb;
  let len = if a.(1) = 0 then 1 else 2 in
  let len = if b >= 0 then mul_pow a len 2 b else mul_pow a len 5 (-b) in
  (digits_of_limbs a len, min b 0)

(* The digit string [s] plus one. *)
let succ_digits s =
  let b = Bytes.of_string s in
  let rec carry i =
    if i < 0 then true
    else if Bytes.get b i = '9' then (
      Bytes.set b i '0';
      carry (i - 1))
    else (
      Bytes.set b i (Char.chr (Char.code (Bytes.get b i) + 1));
      false)
  in
  if carry (String.length s - 1) then
    String.concat "" [ "1"; Bytes.to_string b ]
  else Bytes.to_string b

type tie = Even | Away

(* The digit string [s] read as an integer, divided by [10^(length s - k)] and
   rounded to nearest, a half going to the even neighbour or away from zero by
   [tie]: [s] cut to its first [k] digits. *)
let round_digits ~tie s k =
  let n = String.length s in
  if k >= n then String.concat "" [ s; String.make (k - n) '0' ]
  else if k < 0 then "0"
  else
    let rec nonzero i = i < n && (s.[i] <> '0' || nonzero (i + 1)) in
    let odd i = i >= 0 && (Char.code s.[i] - Char.code '0') land 1 = 1 in
    let up =
      match s.[k] with
      | '6' .. '9' -> true
      | '5' -> (
          nonzero (k + 1) || match tie with Away -> true | Even -> odd (k - 1))
      | _ -> false
    in
    if k = 0 then if up then "1" else "0"
    else if up then succ_digits (String.sub s 0 k)
    else String.sub s 0 k

(* Two positive [(s, e)] values in order. *)
let compare_decimal (s1, e1) (s2, e2) =
  let n1 = String.length s1 and n2 = String.length s2 in
  let c = Int.compare (n1 + e1) (n2 + e2) in
  if c <> 0 then c
  else
    let digit s n i = if i < n then s.[i] else '0' in
    let rec go i =
      if i = max n1 n2 then 0
      else
        let c = Char.compare (digit s1 n1 i) (digit s2 n2 i) in
        if c <> 0 then c else go (i + 1)
    in
    go 0

(* The shortest decimal that reads back as the positive finite [x], as [(s, e)]:
   the fewest significant digits whose value lies within [x]'s rounding
   interval, the midpoints to its neighbouring doubles included when [x]'s
   significand is even, and of two such the nearer to [x]. It is the number an
   author wrote whenever that number has 15 significant digits or fewer. *)
let shortest_digits x =
  let m, b = binary x in
  let s, e = exact_digits m b in
  let high = exact_digits ((2 * m) + 1) (b - 1) in
  let low =
    if m = 1 lsl 52 && b > -1074 then exact_digits ((4 * m) - 1) (b - 2)
    else exact_digits ((2 * m) - 1) (b - 1)
  in
  let inside c =
    let lo = compare_decimal low c and hi = compare_decimal c high in
    if m land 1 = 0 then lo <= 0 && hi <= 0 else lo < 0 && hi < 0
  in
  let n = String.length s in
  let rec go p =
    if p >= n then (s, e)
    else
      let scale = e + n - p in
      let down = String.sub s 0 p in
      let near = round_digits ~tie:Even s p in
      let far = if near = down then succ_digits down else down in
      if inside (near, scale) then (near, scale)
      else if inside (far, scale) then (far, scale)
      else go (p + 1)
  in
  go 1

(* [s * 10^e], [s] a digit string with no leading zero, in exponent notation,
   trailing zeros dropped. CSS Syntax 3 sec. 4.3.13 reads [1e30] and [4e-33]
   without a [+] or a padded exponent. *)
let format_exponent is_neg s e =
  let rec sig_end i = if i > 1 && s.[i - 1] = '0' then sig_end (i - 1) else i in
  let n = sig_end (String.length s) in
  let b = Buffer.create (n + 8) in
  if is_neg then Buffer.add_char b '-';
  Buffer.add_char b s.[0];
  if n > 1 then (
    Buffer.add_char b '.';
    Buffer.add_substring b s 1 (n - 1));
  Buffer.add_char b 'e';
  Buffer.add_string b (string_of_int (String.length s - 1 + e));
  Buffer.contents b

(* An integral [abs_f] prints every digit below [2^62], and its shortest decimal
   in exponent notation from there, where it has no [int]. *)
let format_integer is_neg abs_f =
  match Common.int_of_float_opt abs_f with
  | Some i -> string_of_int (if is_neg then -i else i)
  | None ->
      let s, e = shortest_digits abs_f in
      format_exponent is_neg s e

(* [abs_f] at [d] decimal places: its shortest decimal, rounded with a half
   going away from zero. One that rounds to [0] keeps [max 1 d] significant
   digits instead, positional while the rounded value has at most 24 leading
   zeros and in exponent notation past that, so that its print reads back as a
   value printed the same way. A shortest decimal whose scaled magnitude reaches
   [2^62] has fewer than [d] places, so the places keep every digit it has. *)
let format_shortest ~drop_leading_zero d is_neg abs_f =
  let s, e = shortest_digits abs_f in
  let scaled d = round_digits ~tie:Away s (String.length s + e + d) in
  match scaled d with
  | "0" ->
      let d = max 1 d - (String.length s + e) in
      let r = scaled d in
      if d - String.length r > 24 then format_exponent is_neg r (-d)
      else format_decimal ~drop_leading_zero r d is_neg
  | r -> format_decimal ~drop_leading_zero r d is_neg

(* The hot path. [p], [abs_f] times an exact [10^d], is off the shortest decimal
   times [10^d] by one and a half of its ulps at most, and below [2^52] an ulp
   of [p] is at most [p * 2^-52]. A fractional part of [p] further than [p *
   2^-51] from [0.5] therefore lies on the same side of the half as the shortest
   decimal's, and fixes the rounding. A near half, a product past [2^52], a [d]
   past the exact powers and a value that rounds to [0] take the shortest
   digits. *)
let format_decimal_value ~drop_leading_zero d is_neg abs_f =
  let p = if d >= 0 && d <= 22 then abs_f *. pow10.(d) else Float.infinity in
  let f = Float.floor p in
  let frac = p -. f in
  if
    p < 0x1p52
    && Float.abs (frac -. 0.5) > p *. 0x1p-51
    && (f > 0.0 || frac > 0.5)
  then
    let n = int_of_float f + if frac > 0.5 then 1 else 0 in
    format_decimal ~drop_leading_zero (string_of_int n) d is_neg
  else format_shortest ~drop_leading_zero d is_neg abs_f

let string_of_float ?(drop_leading_zero = false) ?(max_decimals = 8) f =
  match classify_float f with
  | FP_zero -> "0"
  (* Raw float formatting, not a CSS value sink: [Context] round-trips a float
     through this and back with [float_of_string]. The CSS spelling of a NaN is
     [nan_value]'s. *)
  | FP_nan -> "NaN"
  | FP_infinite when f > 0.0 -> "3.40282e38"
  | FP_infinite -> "-3.40282e38"
  | FP_normal | FP_subnormal ->
      let is_neg = f < 0.0 in
      let abs_f = if is_neg then -.f else f in
      if abs_f = floor abs_f then format_integer is_neg abs_f
      else format_decimal_value ~drop_leading_zero max_decimals is_neg abs_f

let printed f =
  if Float.is_finite f then float_of_string (string_of_float f) else f

let string_of_float_exact f =
  let max_decimals =
    if Float.is_finite f && not (Float.equal (printed f) f) then
      -snd (shortest_digits (Float.abs f))
    else 8
  in
  string_of_float ~drop_leading_zero:true ~max_decimals f

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
   [string_of_float]. One with no [int] takes [format_integer]'s fallback. *)
(* CSS Values 4 sec. 10.7.2: [NaN] is a keyword of the math-function grammar and
   exists nowhere else, so a NaN-valued number has no literal spelling. Sec.
   10.13 serialises it as [calc(NaN)], with [* 1<unit>] appended once the value
   carries a unit. A bare [NaN] token is not CSS: browsers drop [width: NaNpx]
   and [opacity: NaN], and so does cascade's own reader. *)
let nan_value ctx suffix =
  string ctx "calc(NaN";
  if suffix <> "" then (
    string ctx (if ctx.minify then "*1" else " * 1");
    string ctx suffix);
  char ctx ')'

let float ctx f =
  if Float.is_nan f then nan_value ctx ""
  else
    match Common.int_of_float_opt f with
    | Some i when Float.is_integer f -> int ctx i
    | _ ->
        string ctx
          (if ctx.exact_numbers then string_of_float_exact f
           else string_of_float ~drop_leading_zero:true f)

let exact ctx =
  if ctx.exact_numbers then ctx else { ctx with exact_numbers = true }

let exact_numbers ctx = ctx.exact_numbers

let float_n n ctx f =
  if Float.is_nan f then nan_value ctx ""
  else
    match Common.int_of_float_opt f with
    | Some i when Float.is_integer f -> int ctx i
    | _ ->
        string ctx (string_of_float ~drop_leading_zero:true ~max_decimals:n f)

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
  let rec go n =
    if n < 16 then char ctx (hex_digit n)
    else (
      go (n / 16);
      go (n mod 16))
  in
  go i

let unit ctx f suffix =
  if Float.is_nan f then nan_value ctx suffix
  else (
    float ctx f;
    string ctx suffix)

let pct ctx f =
  (* CSS Values 4 sec. 6 only allows the unit to drop on a zero [<length>]; a
     zero [<percentage>] keeps the [%] (otherwise [opacity:0] vs [opacity:0%]
     are no longer equivalent, and dimension/percentage-typed grammars reject a
     bare [0]). The coefficient prints in full, like [float]: rounding it
     changes the value (a repeating fraction such as [33.333333%] from [w-1/3]
     would lose digits), so any precision reduction belongs in the optimizer,
     not here. *)
  if Float.is_nan f then nan_value ctx "%"
  else (
    float ctx f;
    string ctx "%")

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
let in_style_rule ctx = ctx.in_style_rule
let enter_style_rule ctx = { ctx with in_style_rule = true }
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

(* CSSOM 1 sec. 2.1 serializes a URL as [url(] and the serialized string; the
   bare body is the shorter spelling of the same url wherever it can hold it. *)
let url ctx s =
  string ctx "url(";
  if Common.String.url_needs_quotes s then quoted_string ctx s else string ctx s;
  string ctx ")"

(* CSS Syntax 3 (ED) sec. 4.3.6: inside a url token a space, a quote, a
   parenthesis and a backslash end or break the token unless escaped, and a
   non-printable code point is a parse error unless escaped. *)
let url_token ctx s =
  string ctx "url(";
  let len = String.length s in
  String.iteri
    (fun i c ->
      match c with
      | ' ' | '"' | '\'' | '(' | ')' | '\\' ->
          char ctx '\\';
          char ctx c
      | '\x00' .. '\x1F' | '\x7F' ->
          let next = if i + 1 < len then Some s.[i + 1] else None in
          hex_escape_byte ctx ~next c
      | c -> char ctx c)
    s;
  char ctx ')'

let rename_custom_property ctx name = ctx.rename_custom_property name
