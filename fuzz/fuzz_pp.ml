(** Fuzz tests for [Pp]'s number printer: its digits follow the rule
    [Pp.string_of_float] states, worked out here from the bits of the double
    with no C library formatting, so they cannot depend on the platform. *)

open Cascade
open Alcobar

(* A decimal digit string times 2 or 5. *)
let times k s =
  let n = String.length s in
  let b = Bytes.create (n + 1) in
  let carry = ref 0 in
  for i = n - 1 downto 0 do
    let v = ((Char.code s.[i] - 48) * k) + !carry in
    Bytes.set b (i + 1) (Char.chr (48 + (v mod 10)));
    carry := v / 10
  done;
  if !carry = 0 then Bytes.sub_string b 1 n
  else (
    Bytes.set b 0 (Char.chr (48 + !carry));
    Bytes.to_string b)

(* [m * 2^b] as [(s, scale)], the value being [s / 10^scale]: [m] doubled or
   halved once per binary exponent step, a halving being a product by 5 and one
   more decimal place. It shares no code and no method with the printer. *)
let exact_of m b =
  let s = ref (string_of_int m) in
  if b >= 0 then (
    for _ = 1 to b do
      s := times 2 !s
    done;
    (!s, 0))
  else (
    for _ = 1 to -b do
      s := times 5 !s
    done;
    (!s, -b))

let binary x =
  let bits = Int64.bits_of_float x in
  let biased = Int64.to_int (Int64.shift_right_logical bits 52) land 0x7ff in
  let frac = Int64.to_int (Int64.logand bits 0xF_FFFF_FFFF_FFFFL) in
  if biased = 0 then (frac, -1074) else (frac lor (1 lsl 52), biased - 1075)

let strip_leading s =
  let n = String.length s in
  let rec go i = if i < n - 1 && s.[i] = '0' then go (i + 1) else i in
  let i = go 0 in
  String.sub s i (n - i)

(* A number as [(digits, e)] with value [digits * 10^e], no leading or trailing
   zero in [digits]; zero is [("", 0)]. *)
let canonical digits e =
  let digits = strip_leading digits in
  if digits = "0" then ("", 0)
  else
    let n = String.length digits in
    let rec last i = if digits.[i] = '0' then last (i - 1) else i in
    let l = last (n - 1) in
    (String.sub digits 0 (l + 1), e + (n - 1 - l))

(* Two positive canonical numbers in order. *)
let compare_value (d1, e1) (d2, e2) =
  let c = compare (String.length d1 + e1) (String.length d2 + e2) in
  if c <> 0 then c
  else
    let n = max (String.length d1) (String.length d2) in
    let pad d = String.concat "" [ d; String.make (n - String.length d) '0' ] in
    compare (pad d1) (pad d2)

(* [s] plus one, as decimal strings. *)
let succ s =
  let b = Bytes.of_string (String.concat "" [ "0"; s ]) in
  let rec go i =
    if Bytes.get b i = '9' then (
      Bytes.set b i '0';
      go (i - 1))
    else Bytes.set b i (Char.chr (Char.code (Bytes.get b i) + 1))
  in
  go (Bytes.length b - 1);
  strip_leading (Bytes.to_string b)

(* [s / 10^drop] rounded to nearest, a half going to even or away from zero, as
   a decimal string. *)
let round_ref ~away s drop =
  if drop <= 0 then String.concat "" [ s; String.make (-drop) '0' ]
  else
    let n = String.length s in
    let padded =
      if drop >= n then String.concat "" [ String.make (drop - n + 1) '0'; s ]
      else s
    in
    let n = String.length padded in
    let head = String.sub padded 0 (n - drop) in
    let tail = String.sub padded (n - drop) drop in
    let half = String.concat "" [ "5"; String.make (drop - 1) '0' ] in
    let c = compare tail half in
    let odd = (Char.code head.[String.length head - 1] - 48) land 1 = 1 in
    strip_leading
      (if c > 0 || (c = 0 && (away || odd)) then succ head else head)

(* The shortest decimal that reads back as the positive finite [x], as [(s,
   scale)]: for each length in turn, the decimals of that length just below and
   just above [x], the first one inside [x]'s rounding interval (its ends
   included when the significand is even), the nearer if both are. *)
let shortest x =
  let m, b = binary x in
  let s, scale = exact_of m b in
  let value (s, scale) = canonical s (-scale) in
  let high = value (exact_of ((2 * m) + 1) (b - 1)) in
  let low =
    value
      (if m = 1 lsl 52 && b > -1074 then exact_of ((4 * m) - 1) (b - 2)
       else exact_of ((2 * m) - 1) (b - 1))
  in
  let inside c =
    let v = value c in
    let lo = compare_value low v and hi = compare_value v high in
    if m land 1 = 0 then lo <= 0 && hi <= 0 else lo < 0 && hi < 0
  in
  let n = String.length s in
  let rec go p =
    let drop = n - p in
    if drop <= 0 then (s, scale)
    else
      let down = String.sub s 0 p in
      let up = succ down in
      let near = round_ref ~away:false s drop in
      let far = if near = down then up else down in
      if inside (near, scale - drop) then (near, scale - drop)
      else if inside (far, scale - drop) then (far, scale - drop)
      else go (p + 1)
  in
  go 1

(* What the printer writes, read back as an exact decimal. *)
let read_printed p =
  let neg = String.length p > 0 && p.[0] = '-' in
  let p = if neg then String.sub p 1 (String.length p - 1) else p in
  let mantissa, exp =
    match String.index_opt p 'e' with
    | None -> (p, 0)
    | Some i ->
        ( String.sub p 0 i,
          int_of_string (String.sub p (i + 1) (String.length p - i - 1)) )
  in
  let digits, frac =
    match String.index_opt mantissa '.' with
    | None -> (mantissa, 0)
    | Some i ->
        ( String.concat ""
            [
              String.sub mantissa 0 i;
              String.sub mantissa (i + 1) (String.length mantissa - i - 1);
            ],
          String.length mantissa - i - 1 )
  in
  (neg, canonical (if digits = "" then "0" else digits) (exp - frac))

(* A decimal integer string against [2^62]. *)
let reaches_2_62 s =
  let limit = "4611686018427387904" in
  String.length s > String.length limit
  || (String.length s = String.length limit && s >= limit)

(* The value [Pp.string_of_float ~max_decimals:d] documents for [x]: decimal
   places round the shortest decimal with a half going away from zero, and the
   12 significant digits round the exact value with ties to even. *)
let expected d x =
  if x = 0.0 then (false, ("", 0))
  else
    let neg = x < 0.0 in
    let a = Float.abs x in
    let significant n =
      let m, b = binary a in
      let s, scale = exact_of m b in
      let point = String.length s - 1 - scale in
      let keep = n - 1 - point in
      canonical (round_ref ~away:false s (scale - keep)) (-keep)
    in
    let value =
      if Float.is_integer a then
        if a < 0x1p62 then
          let m, b = binary a in
          let s, scale = exact_of m b in
          canonical s (-scale)
        else significant 12
      else
        let s, scale = shortest a in
        let at keep =
          let r = round_ref ~away:true s (scale - keep) in
          if reaches_2_62 r then significant 12 else canonical r (-keep)
        in
        if round_ref ~away:true s (scale - d) <> "0" then at d
        else
          let leading_zeros = scale - String.length s in
          if leading_zeros > 24 then significant 12
          else at (max 1 d + leading_zeros)
    in
    (neg, value)

let pp_value ppf (neg, (digits, e)) =
  Fmt.pf ppf "%s%se%d" (if neg then "-" else "") digits e

let check_rounding d x =
  if Float.is_finite x then (
    let printed = Pp.string_of_float ~max_decimals:d x in
    let got = read_printed printed and want = expected d x in
    if got <> want then
      failf "string_of_float ~max_decimals:%d %h printed %S (%a), want %a" d x
        printed pp_value got pp_value want;
    if d = 8 then
      let minified = Pp.to_string ~minify:true Pp.float x in
      if read_printed minified <> want then
        failf "Pp.float %h printed %S, want %a" x minified pp_value want)

(* Any bit pattern; a significand at a binary exponent the decimal places reach;
   a small numerator over a power of two, which lands on exact halves; and a
   short decimal over a power of ten, which lands on the halves an author
   writes, such as [0.8765] at three places. *)
let double =
  choose
    [
      map [ int64 ] Int64.float_of_bits;
      map
        [ range 0x20_0000_0000_0000; range 130; bool ]
        (fun m e neg ->
          let x = Float.ldexp (Float.of_int m) (e - 90) in
          if neg then -.x else x);
      map
        [ range 100_000; range ~min:1 20; bool ]
        (fun k j neg ->
          let x = Float.ldexp (Float.of_int k) (-j) in
          if neg then -.x else x);
      map
        [ range 1_000_000; range 10; bool ]
        (fun k j neg ->
          let x =
            Float.of_int ((10 * k) + 5) /. (10.0 ** Float.of_int (j + 1))
          in
          if neg then -.x else x);
    ]

let suite =
  ( "pp",
    [
      test_case "string_of_float rounds by its stated rule"
        [ range 13; double ]
        check_rounding;
    ] )
