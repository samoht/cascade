(* Estimated DEFLATE (gzip) output size.

   Greedy LZ77 parse -- zlib-style hash chains, 32 KiB window, match lengths
   3..258 -- costed with RFC 1951's extra-bit tables and an order-0 entropy
   estimate for literal bytes. A size oracle, not a compressor: absolute values
   are approximate, differences between candidate spellings of the same
   stylesheet are what it is for. *)

let window = 32768
let min_match = 3
let max_match = 258
let hash_mask = (1 lsl 15) - 1
let max_chain = 64

(* RFC 1951 sec. 3.2.5: extra bits of the length code for a match length. *)
let length_extra_bits len =
  if len <= 10 then 0
  else if len <= 18 then 1
  else if len <= 34 then 2
  else if len <= 66 then 3
  else if len <= 130 then 4
  else if len <= 257 then 5
  else 0

(* RFC 1951 sec. 3.2.5: extra bits of the distance code for a distance. *)
let distance_extra_bits dist =
  let rec log2 n acc = if n <= 1 then acc else log2 (n lsr 1) (acc + 1) in
  if dist <= 4 then 0 else max 0 (log2 (dist - 1) 0 - 1)

(* Typical dynamic-Huffman code lengths for length and distance symbols; a fixed
   per-symbol cost is enough when only deltas matter. *)
let length_symbol_bits = 7
let distance_symbol_bits = 5

(* Block header, code-length tree, end-of-block and gzip wrapper, in bytes.
   Constant across candidates; kept so absolute values stay plausible. *)
let overhead = 24

let hash3 s i =
  (Char.code (String.unsafe_get s i) lsl 10)
  lxor (Char.code (String.unsafe_get s (i + 1)) lsl 5)
  lxor Char.code (String.unsafe_get s (i + 2))
  land hash_mask

let match_length s i j n =
  let limit = min max_match (n - i) in
  let k = ref 0 in
  while
    !k < limit && String.unsafe_get s (i + !k) = String.unsafe_get s (j + !k)
  do
    incr k
  done;
  !k

let match_bits_of ~len ~dist =
  length_symbol_bits + length_extra_bits len + distance_symbol_bits
  + distance_extra_bits dist

(* Greedy LZ77 parse of [s]: count each literal byte in [lit_freq] via [literal]
   and accumulate match token costs into [match_bits]. *)
let parse s ~literal ~match_bits =
  let n = String.length s in
  let i = ref 0 in
  if n >= min_match then begin
    let head = Array.make (hash_mask + 1) (-1) in
    (* [prev] is indexed by absolute position, so chains strictly decrease and
       need no aliasing guard; the window bound cuts them instead. *)
    let prev = Array.make n (-1) in
    let insert i =
      let h = hash3 s i in
      prev.(i) <- head.(h);
      head.(h) <- i
    in
    let hash_limit = n - min_match + 1 in
    while !i < hash_limit do
      let best_len = ref 0 and best_dist = ref 0 in
      let j = ref head.(hash3 s !i) and chain = ref max_chain in
      while !j >= 0 && !chain > 0 && !i - !j <= window do
        let len = match_length s !i !j n in
        if len > !best_len then begin
          best_len := len;
          best_dist := !i - !j
        end;
        decr chain;
        j := prev.(!j)
      done;
      if !best_len >= min_match then begin
        match_bits :=
          !match_bits + match_bits_of ~len:!best_len ~dist:!best_dist;
        let stop = min (!i + !best_len) hash_limit in
        let next = !i + !best_len in
        while !i < stop do
          insert !i;
          incr i
        done;
        i := next
      end
      else begin
        literal !i;
        insert !i;
        incr i
      end
    done
  end;
  while !i < n do
    literal !i;
    incr i
  done

(* Order-0 entropy of the emitted literals, in bits. *)
let literal_bits lit_freq lit_count =
  if lit_count = 0 then 0.
  else begin
    let total = float_of_int lit_count in
    let bits = ref 0. in
    Array.iter
      (fun f ->
        if f > 0 then
          let f = float_of_int f in
          bits := !bits +. (f *. (log (total /. f) /. log 2.)))
      lit_freq;
    !bits
  end

let estimate s =
  let lit_freq = Array.make 256 0 in
  let lit_count = ref 0 in
  let match_bits = ref 0 in
  let literal i =
    let c = Char.code (String.unsafe_get s i) in
    lit_freq.(c) <- lit_freq.(c) + 1;
    incr lit_count
  in
  parse s ~literal ~match_bits;
  let bits = float_of_int !match_bits +. literal_bits lit_freq !lit_count in
  int_of_float (ceil (bits /. 8.)) + overhead
