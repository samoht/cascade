(** Fuzz tests for the DEFLATE size estimate.

    {!Gzip_size.estimate} is a greedy LZ77 parse over hash chains, costed with
    RFC 1951's extra-bit tables and the order-0 entropy of the literals. The
    reference below is that parse written plainly: every chain candidate is
    measured byte by byte and the longest, first found, wins. The estimate has
    to answer what it answers, on input as repetitive as CSS and as noisy as
    arbitrary bytes. *)

open Cascade
open Alcobar

let window = 32768
let min_match = 3
let max_match = 258
let hash_mask = (1 lsl 15) - 1
let max_chain = 64

let length_extra_bits len =
  if len <= 10 then 0
  else if len <= 18 then 1
  else if len <= 34 then 2
  else if len <= 66 then 3
  else if len <= 130 then 4
  else if len <= 257 then 5
  else 0

let distance_extra_bits dist =
  let rec log2 n acc = if n <= 1 then acc else log2 (n lsr 1) (acc + 1) in
  if dist <= 4 then 0 else max 0 (log2 (dist - 1) 0 - 1)

let hash3 s i =
  (Char.code s.[i] lsl 10)
  lxor (Char.code s.[i + 1] lsl 5)
  lxor Char.code s.[i + 2]
  land hash_mask

(* Every chain candidate of position [i] measured byte by byte; the longest,
   first found among equals, as [(length, distance)]. *)
let longest s ~head ~prev i =
  let n = String.length s in
  let best = ref (0, 0) in
  let j = ref head.(hash3 s i) and chain = ref max_chain in
  while !j >= 0 && !chain > 0 && i - !j <= window do
    let limit = min max_match (n - i) in
    let len = ref 0 in
    while !len < limit && s.[i + !len] = s.[!j + !len] do
      incr len
    done;
    if !len > fst !best then best := (!len, i - !j);
    decr chain;
    j := prev.(!j)
  done;
  !best

let entropy freq literals =
  if literals = 0 then 0.
  else
    let total = float_of_int literals in
    Array.fold_left
      (fun acc f ->
        if f = 0 then acc
        else
          let f = float_of_int f in
          acc +. (f *. (log (total /. f) /. log 2.)))
      0. freq

let reference s =
  let n = String.length s in
  let freq = Array.make 256 0 and literals = ref 0 and bits = ref 0 in
  let head = Array.make (hash_mask + 1) (-1) and prev = Array.make n (-1) in
  let insert i =
    let h = hash3 s i in
    prev.(i) <- head.(h);
    head.(h) <- i
  in
  let literal i =
    freq.(Char.code s.[i]) <- freq.(Char.code s.[i]) + 1;
    incr literals;
    insert i
  in
  let hash_limit = n - min_match + 1 in
  let i = ref 0 in
  while !i < hash_limit do
    match longest s ~head ~prev !i with
    | len, dist when len >= min_match ->
        bits := !bits + 12 + length_extra_bits len + distance_extra_bits dist;
        for k = !i to min (!i + len) hash_limit - 1 do
          insert k
        done;
        i := !i + len
    | _ ->
        literal !i;
        incr i
  done;
  for k = !i to n - 1 do
    freq.(Char.code s.[k]) <- freq.(Char.code s.[k]) + 1;
    incr literals
  done;
  int_of_float (ceil ((float_of_int !bits +. entropy freq !literals) /. 8.))
  + 24

let fragments =
  [|
    ".a{color:red}";
    ".b{margin:0}";
    "color:red;";
    "@media print{";
    "}";
    "aaaaaaaaaaaaaaaa";
    "0123456789abcdef";
  |]

(* Fragments drawn by the input, some repeated many times over so matches reach
   the 258-byte cap and the 32 KiB window, with raw input bytes between. *)
let sheet buf =
  let b = Buffer.create 4096 in
  String.iteri
    (fun i c ->
      let k = Char.code c in
      if k < 64 then Buffer.add_char b c
      else if k < 72 then
        for _ = 0 to (k - 64) * 40 do
          Buffer.add_string b fragments.(i mod Array.length fragments)
        done
      else Buffer.add_string b fragments.(k mod Array.length fragments))
    buf;
  Buffer.contents b

let test_estimate_agrees_with_reference buf =
  let s = sheet buf in
  let estimate = Gzip_size.estimate s and expected = reference s in
  if estimate <> expected then
    failf "estimate %d, reference parse %d, on %d bytes" estimate expected
      (String.length s)

let suite =
  ( "gzip_size",
    [
      test_case "estimate agrees with a plain greedy parse" [ bytes ]
        test_estimate_agrees_with_reference;
    ] )
