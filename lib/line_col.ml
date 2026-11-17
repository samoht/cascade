(* CSS Syntax 3 (ED) sec. 3.3 turns CR, FF and the CRLF pair into one LF before
   tokenizing, and a column is one element of the decoded stream: a code point,
   or one U+FFFD per maximal ill-formed subpart (sec. 3.2). A source the reader
   already preprocessed holds only LF; counting the other breaks as well gives
   the same answer on a raw string.

   A walk is the state after decoding the elements before byte [i]. [after_cr]
   holds the LF of a CRLF pair back from opening a second line. *)
type walk = {
  mutable i : int;
  mutable line : int;
  mutable col : int;
  mutable after_cr : bool;
}

let start () = { i = 0; line = 1; col = 1; after_cr = false }

let break w =
  w.line <- w.line + 1;
  w.col <- 1

(* Decode [source] from [w.i] up to the first element boundary at or after
   [until], inside the window that ends at [stop]. A sequence [stop] cuts short
   is one ill-formed element. Every byte below 0x80 is an element of its own, so
   only the others go through the decoder. *)
let advance source w ~until ~stop =
  while w.i < until do
    let i = w.i in
    match String.unsafe_get source i with
    | '\n' ->
        if w.after_cr then w.after_cr <- false else break w;
        w.i <- i + 1
    | '\x0C' ->
        break w;
        w.after_cr <- false;
        w.i <- i + 1
    | '\r' ->
        break w;
        w.after_cr <- true;
        w.i <- i + 1
    | c ->
        let n =
          if Char.code c < 0x80 then 1
          else Uchar.utf_decode_length (String.get_utf_8_uchar source i)
        in
        w.col <- w.col + 1;
        w.after_cr <- false;
        w.i <- min stop (i + n)
  done

let clamp source pos = max 0 (min pos (String.length source))

(* Mark [k] is the walk at the first element boundary at or after byte [k *
   stride], three slots wide: the boundary, the line, and the column doubled
   with [after_cr] in the low bit. An element is at most four bytes, so every
   multiple of the stride up to the length has a mark of its own, and [marks]
   holds marks [1 .. length / stride]; mark [0] is the start of the source. The
   marks are the only state, written once and the same however many readers race
   to build them, so lookups share [t] freely. *)
type t = { source : string; mutable marks : int array option }

let stride = 1024
let v source = { source; marks = None }
let source t = t.source
let count probes n = match probes with Some p -> p := !p + n | None -> ()

let build ?probes t =
  let len = String.length t.source in
  let count_marks = len / stride in
  let marks = Array.make (3 * count_marks) 0 in
  let w = start () in
  for k = 1 to count_marks do
    advance t.source w ~until:(k * stride) ~stop:len;
    let slot = 3 * (k - 1) in
    marks.(slot) <- w.i;
    marks.(slot + 1) <- w.line;
    marks.(slot + 2) <- (2 * w.col) + Bool.to_int w.after_cr
  done;
  count probes w.i;
  t.marks <- Some marks;
  marks

(* The walk at the last mark at or before [stop]. The mark of the stride [stop]
   falls in can sit up to three bytes past it. *)
let resume marks stop =
  let k = stop / stride in
  let k = if k > 0 && marks.(3 * (k - 1)) > stop then k - 1 else k in
  if k = 0 then start ()
  else
    let slot = 3 * (k - 1) in
    let packed = marks.(slot + 2) in
    {
      i = marks.(slot);
      line = marks.(slot + 1);
      col = packed / 2;
      after_cr = packed land 1 = 1;
    }

let locate ?probes t pos =
  let stop = clamp t.source pos in
  let marks = match t.marks with Some m -> m | None -> build ?probes t in
  let w = resume marks stop in
  count probes (stop - w.i);
  advance t.source w ~until:stop ~stop;
  (w.line, w.col)
