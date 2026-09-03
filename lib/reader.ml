(** Simple CSS parser implementation. *)

type t = {
  input : string;
  len : int;
  (* Restrict non-ASCII identifiers to the CSS Syntax 3 (ED) sec. 4.2 range list
     rather than accepting any code point >= U+0080. Rides on the reader so
     every [_at] predicate reaches it without threading a parameter through the
     tokenizer. *)
  enforce_spec : bool;
  mutable pos : int;
  mutable call_stack : string list; (* Stack of parsing contexts for debugging *)
}

type parse_error = {
  message : string;
  got : string option;
  position : int;
  filename : string;
  line : int;
  col : int;
  context_window : string;
  marker_pos : int;
  callstack : string list;
}
(** Parse error information with structured details. *)

exception Parse_error of parse_error

(* Locate a snippet-relative character offset as a line and a column. *)
let marker_in_lines lines marker_pos =
  let rec find line remaining = function
    | [] -> (0, 0)
    | [ last ] ->
        let len = Common.String.utf8_length last in
        (line, min remaining len)
    | current :: rest ->
        let len = Common.String.utf8_length current in
        if remaining <= len then (line, remaining)
        else find (line + 1) (remaining - len - 1) rest
  in
  find 0 (max 0 marker_pos) lines

(** Pretty print parse error with debugging information *)
let pp_parse_error (err : parse_error) =
  let buf = Buffer.create 256 in
  Buffer.add_string buf err.message;
  Buffer.add_string buf " at ";
  Buffer.add_string buf err.filename;
  Buffer.add_char buf ':';
  Buffer.add_string buf (string_of_int err.line);
  Buffer.add_char buf ':';
  Buffer.add_string buf (string_of_int err.col);
  (match err.callstack with
  | [] -> ()
  | callstack ->
      Buffer.add_string buf "\n    [stack: ";
      Buffer.add_string buf (String.concat " -> " callstack);
      Buffer.add_char buf ']');
  if err.context_window <> "" then begin
    let lines = String.split_on_char '\n' err.context_window in
    let marker_line, marker_pos = marker_in_lines lines err.marker_pos in
    List.iteri
      (fun i line ->
        Buffer.add_char buf '\n';
        Buffer.add_string buf line;
        if i = marker_line then begin
          Buffer.add_char buf '\n';
          Buffer.add_string buf (String.make marker_pos ' ');
          Buffer.add_char buf '^'
        end)
      lines
  end;
  Buffer.contents buf

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
  let fffd = "\xEF\xBF\xBD" in
  let i = ref start in
  while !i < len do
    let c = input.[!i] in
    (match c with
    | '\x00' -> Buffer.add_string buf fffd
    | '\r' ->
        Buffer.add_char buf '\n';
        if !i + 1 < len && input.[!i + 1] = '\n' then incr i
    | '\x0C' -> Buffer.add_char buf '\n'
    | _ -> Buffer.add_char buf c);
    incr i
  done;
  Buffer.contents buf

let preprocess input =
  let len = String.length input in
  let bom = has_bom input in
  let start = if bom then 3 else 0 in
  if (not bom) && not (needs_preprocess input len 0) then input
  else copy_preprocessed input len start

let of_string ?(enforce_spec = false) input =
  let input = preprocess input in
  { input; len = String.length input; enforce_spec; pos = 0; call_stack = [] }

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

(** {1 Call Stack Management} *)

let push_context t context = t.call_stack <- context :: t.call_stack

let pop_context t =
  match t.call_stack with
  | [] -> () (* No context to pop *)
  | _ :: rest -> t.call_stack <- rest

let with_context t context f =
  push_context t context;
  match f () with
  | result ->
      pop_context t;
      result
  | exception exn ->
      pop_context t;
      raise exn

let callstack t = List.rev t.call_stack

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

(** Error helpers *)
let err ?got t expected =
  let context, marker_pos = context_window t in
  (* Scan forward from the start of the input, so a newline ends the line it
     sits on and the byte after it opens the next one at column 1. A column
     counts decoded elements, like the caret. *)
  let line, col =
    Common.String.utf8_fold ~len:t.pos
      (fun (line, col) _ decoded ->
        match decoded with
        | Common.String.Scalar u when Uchar.to_int u = 0x0A -> (line + 1, 1)
        | Common.String.Scalar _ | Common.String.Malformed _ -> (line, col + 1))
      (1, 1) t.input
  in
  raise
    (Parse_error
       {
         message = expected;
         got;
         position = t.pos;
         (* A reader is built from a string, so it has no name of its own; a
            caller that has one stamps it with [with_filename]. *)
         filename = "<CSS input>";
         line;
         col;
         context_window = context;
         marker_pos;
         callstack = callstack t;
       })

let err_eof t = err t "unexpected end of input"
let err_expected t what = err t ("expected " ^ what)

let err_expected_but_eof t what =
  err t ("Expected " ^ what ^ " but reached end of input")

(** {1 Error Utilities} *)
let err_invalid t what = err t ("invalid " ^ what)

let with_filename error filename = { error with filename }

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
  if t.pos >= t.len then err_eof t;
  t.pos <- t.pos + 1

(** Get current position in input *)
let position t = t.pos

(** Get context window around current position for better error messages *)
(* Removed duplicate context_window function - already defined above *)
