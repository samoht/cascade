(** Simple CSS parser implementation. *)

(* [from, upto) of the rewrite stands for [source_from, source_upto) of the
   original, where preprocessing changed the length. *)
type shift = { from : int; upto : int; source_from : int; source_upto : int }

type t = {
  input : string;
  len : int;
  shifts : shift array;
  (* Restrict non-ASCII identifiers to the CSS Syntax 3 (ED) sec. 4.2 range list
     rather than accepting any code point >= U+0080. Rides on the reader so
     every [_at] predicate reaches it without threading a parameter through the
     tokenizer. *)
  enforce_spec : bool;
  mutable pos : int;
}

(* Pretty-printer for the parser state *)
let pp (ctx : Pp.ctx) (t : t) =
  Pp.string ctx "<reader pos=";
  Pp.string ctx (string_of_int t.pos);
  Pp.string ctx ">"

(** {1 Creation} *)

(* CSS Syntax 3 (ED) sec. 3.3 "Preprocessing the input stream": - Strip a
   leading U+FEFF BYTE ORDER MARK. - Replace any U+0000 NULL or surrogate code
   point with U+FFFD REPLACEMENT. (We operate post-UTF-8-decode; surrogates
   don't occur in valid UTF-8 so the NUL byte is the practical concern.) -
   Replace U+000D CARRIAGE RETURN, U+000C FORM FEED, and U+000D U+000A CRLF
   pairs with a single U+000A LINE FEED. *)
let has_bom input =
  let len = String.length input in
  len >= 3 && input.[0] = '\xEF' && input.[1] = '\xBB' && input.[2] = '\xBF'

let rec needs_preprocess input len i =
  i < len
  &&
  match input.[i] with
  | '\x00' | '\r' | '\x0C' -> true
  | _ -> needs_preprocess input len (i + 1)

let copy_preprocessed input len start =
  let buf = Buffer.create len in
  let shifts =
    ref
      (if start = 0 then []
       else [ { from = 0; upto = 0; source_from = 0; source_upto = start } ])
  in
  let replace ~source_from ~source_upto text =
    let from = Buffer.length buf in
    Buffer.add_string buf text;
    shifts :=
      { from; upto = Buffer.length buf; source_from; source_upto } :: !shifts
  in
  let i = ref start in
  while !i < len do
    let c = input.[!i] in
    (match c with
    | '\x00' -> replace ~source_from:!i ~source_upto:(!i + 1) "\xEF\xBF\xBD"
    | '\r' when !i + 1 < len && input.[!i + 1] = '\n' ->
        replace ~source_from:!i ~source_upto:(!i + 2) "\n";
        incr i
    | '\r' | '\x0C' -> Buffer.add_char buf '\n'
    | _ -> Buffer.add_char buf c);
    incr i
  done;
  (Buffer.contents buf, Array.of_list (List.rev !shifts))

let preprocess input =
  let len = String.length input in
  let bom = has_bom input in
  let start = if bom then 3 else 0 in
  if (not bom) && not (needs_preprocess input len 0) then (input, [||])
  else copy_preprocessed input len start

let of_string ?(enforce_spec = false) input =
  let input, shifts = preprocess input in
  { input; len = String.length input; shifts; enforce_spec; pos = 0 }

type origin = { map : shift array; length : int }

let origin t = { map = t.shifts; length = t.len }

let original_offset { map; length } i =
  if i < 0 || i > length then
    invalid_arg
      (String.concat ""
         [
           "Reader.original_offset: ";
           string_of_int i;
           " is outside [0-";
           string_of_int length;
           "]";
         ]);
  (* The last shift starting at or before [i], or -1. *)
  let rec last lo hi =
    if hi - lo <= 1 then lo
    else
      let mid = (lo + hi) / 2 in
      if map.(mid).from <= i then last mid hi else last lo mid
  in
  let k = last (-1) (Array.length map) in
  if k < 0 then i
  else
    let s = map.(k) in
    if i < s.upto then s.source_from else s.source_upto + (i - s.upto)

let source t = t.input
let enforce_spec t = t.enforce_spec
let is_done t = t.pos >= t.len

(* Decode the UTF-8 code point at [t.pos + offset] to [Some (cp, byte_length)],
   [None] at EOF or on a malformed sequence. The decoder rejects
   overlong/surrogate/out-of-range sequences per the Unicode spec, and reading
   only the element the position opens keeps bad bytes out of the code point
   that follows them (and out of an ident-like unit token). An ASCII fast path
   comes first: the byte is its own code point and needs no decoding. *)
let peek_utf8_at t offset =
  if offset < 0 || offset >= t.len - t.pos then None
  else
    let p = t.pos + offset in
    let b = Char.code (String.unsafe_get t.input p) in
    if b < 0x80 then Some (b, 1)
    else
      let len = min 4 (t.len - p) in
      match Common.String.utf8_decode ~pos:p ~len t.input with
      | Some (Common.String.Scalar u) ->
          Some (Uchar.to_int u, Uchar.utf_8_byte_length u)
      | Some (Common.String.Malformed _) | None -> None

let peek_utf8 t = peek_utf8_at t 0

let skip_utf8 t =
  match peek_utf8 t with
  | None -> if t.pos < t.len then t.pos <- t.pos + 1 (* advance past bad byte *)
  | Some (_, n) -> t.pos <- t.pos + n

let context_window ?(before = 40) ?(after = 40) t =
  let pos = t.pos in
  (* [before] and [after] are target radiuses, not caps: a boundary that falls
     inside a code point moves outward to the lead byte, widening the window by
     up to three bytes a side. A window is a diagnostic, so keeping the sequence
     whole outranks keeping the byte budget. *)
  let start_pos =
    Common.String.utf8_lead_before t.input (max 0 (pos - before))
  in
  let end_pos =
    Common.String.utf8_lead_after t.input (min t.len (pos + after))
  in
  let context = String.sub t.input start_pos (end_pos - start_pos) in
  let marker_pos =
    Common.String.utf8_length ~pos:start_pos ~len:(pos - start_pos) t.input
  in
  (context, marker_pos)

(** {1 Looking Ahead} *)

let peek t = if t.pos >= t.len then None else Some t.input.[t.pos]

let peek_at t offset =
  let p = t.pos + offset in
  if p < 0 || p >= t.len then None else Some (String.unsafe_get t.input p)

let peek_byte t =
  if t.pos >= t.len then -1 else Char.code (String.unsafe_get t.input t.pos)

let peek_byte_at t offset =
  let p = t.pos + offset in
  if p < 0 || p >= t.len then -1 else Char.code (String.unsafe_get t.input p)

let peek_string t n =
  let n = min n (t.len - t.pos) in
  String.sub t.input t.pos n

let looking_at t s =
  let slen = String.length s in
  if t.pos + slen > t.len then false
  else
    (* Iterative byte compare so [looking_at] (called once per [next_token]
       through [skip_comment_run]) does not allocate a closure for the inner
       loop. *)
    let i = ref 0 in
    let ok = ref true in
    while !ok && !i < slen do
      if String.unsafe_get t.input (t.pos + !i) <> String.unsafe_get s !i then
        ok := false
      else incr i
    done;
    !ok

(** {1 Reading Characters} *)

let skip t =
  if t.pos >= t.len then invalid_arg "Reader.skip: end of input";
  t.pos <- t.pos + 1

(** Get current position in input *)
let position t = t.pos

(** Get context window around current position for better error messages *)
(* Removed duplicate context_window function - already defined above *)
