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
  if dist <= 4 then 0 else Int.max 0 (log2 (dist - 1) 0 - 1)

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

(* The length of the common run at [j] and [i], [j < i], up to [limit]. Eight
   bytes at a time while they agree, then byte by byte. *)
let match_length s i j limit =
  let k = ref 0 in
  while
    !k + 8 <= limit
    && Int64.equal
         (String.get_int64_le s (i + !k))
         (String.get_int64_le s (j + !k))
  do
    k := !k + 8
  done;
  while
    !k < limit
    && Char.equal (String.unsafe_get s (i + !k)) (String.unsafe_get s (j + !k))
  do
    incr k
  done;
  !k

let match_bits_of ~len ~dist =
  length_symbol_bits + length_extra_bits len + distance_symbol_bits
  + distance_extra_bits dist

(* The hash chains over [s]: [head] holds the latest position of each hash and
   [prev] the one before each position with the same hash. [prev] is indexed by
   absolute position, so chains strictly decrease and need no aliasing guard;
   the window bound cuts them instead. [best_len] and [best_dist] are the match
   [longest_match] last found. *)
type chains = {
  s : string;
  head : int array;
  prev : int array;
  mutable best_len : int;
  mutable best_dist : int;
}

let insert c i =
  let h = hash3 c.s i in
  c.prev.(i) <- c.head.(h);
  c.head.(h) <- i

(* The longest match for position [i] along its chain, the first found among
   equals. A candidate whose byte at the best length so far differs from [i]'s
   cannot be longer, so it is skipped unmeasured, and the walk stops once a
   match reaches [limit], which none can pass. *)
let longest_match c i limit =
  c.best_len <- 0;
  c.best_dist <- 0;
  let j = ref c.head.(hash3 c.s i) and chain = ref max_chain in
  while !j >= 0 && !chain > 0 && i - !j <= window && c.best_len < limit do
    let best = c.best_len in
    if
      Char.equal
        (String.unsafe_get c.s (!j + best))
        (String.unsafe_get c.s (i + best))
    then begin
      let len = match_length c.s i !j limit in
      if len > best then begin
        c.best_len <- len;
        c.best_dist <- i - !j
      end
    end;
    decr chain;
    j := c.prev.(!j)
  done

(* Greedy LZ77 parse of [s]: count each literal byte via [literal] and
   accumulate match token costs into [match_bits]. *)
let parse s ~literal ~match_bits =
  let n = String.length s in
  let i = ref 0 in
  if n >= min_match then begin
    let c =
      {
        s;
        head = Array.make (hash_mask + 1) (-1);
        prev = Array.make n (-1);
        best_len = 0;
        best_dist = 0;
      }
    in
    let hash_limit = n - min_match + 1 in
    while !i < hash_limit do
      longest_match c !i (Int.min max_match (n - !i));
      if c.best_len >= min_match then begin
        match_bits :=
          !match_bits + match_bits_of ~len:c.best_len ~dist:c.best_dist;
        let stop = Int.min (!i + c.best_len) hash_limit in
        let next = !i + c.best_len in
        while !i < stop do
          insert c !i;
          incr i
        done;
        i := next
      end
      else begin
        literal !i;
        insert c !i;
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
