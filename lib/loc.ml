(** Source locations: byte-offset ranges in the CSS Syntax-preprocessed input.

    {!Reader.of_string} strips a leading UTF-8 BOM, replaces NUL with U+FFFD,
    and maps CR, FF and CRLF to LF before the lexer records these offsets.

    Every {!Token.t} and {!Component.t} carries one, letting downstream
    validators report errors with precise positions and recover ranges for
    editor integrations. *)

type meta_level = [ `None | `Locs | `Full ]

let default_meta_level : meta_level = `Full

type t = { start_pos : int; end_pos : int }

let v ~start_pos ~end_pos = { start_pos; end_pos }
let dummy = { start_pos = 0; end_pos = 0 }

let union a b =
  {
    start_pos = Int.min a.start_pos b.start_pos;
    end_pos = Int.max a.end_pos b.end_pos;
  }

let pp : t Pp.t =
 fun ctx { start_pos; end_pos } ->
  Pp.char ctx '[';
  Pp.string ctx (string_of_int start_pos);
  Pp.char ctx '-';
  Pp.string ctx (string_of_int end_pos);
  Pp.char ctx ']'

let to_string t = Pp.to_string pp t

module Path = struct
  type step = Mem of string | Nth of int | Label of string
  type t = step list

  let empty = []
  let push step t = t @ [ step ]

  let rec last = function
    | [] -> None
    | [ step ] -> Some step
    | _ :: rest -> last rest

  let to_list t = t
  let of_labels labels = List.map (fun label -> Label label) labels

  let string_of_step = function
    | Mem s -> s
    | Nth n -> String.concat "" [ "["; string_of_int n; "]" ]
    | Label s -> s

  let to_labels t = List.map string_of_step t
  let pp : t Pp.t = fun ctx t -> Pp.string ctx (String.concat "/" (to_labels t))
end

module Context = struct
  type snippet = { text : string; marker_pos : int; marker_len : int }

  type nonrec t = {
    path : Path.t;
    loc : t;
    sort : Sort.t;
    snippet : snippet option;
  }

  let push step t = { t with path = Path.push step t.path }
end

(* A caret column is one Unicode scalar value. That is not what a terminal
   draws: a combining mark takes a column of its own here and none there, and a
   wide CJK glyph takes one here and two there. Counting graphemes or east Asian
   widths instead needs segmentation and width tables cascade does not carry,
   and scalars are exact for the ASCII and Latin text CSS is written in. *)
let scalars = Common.String.utf8_length

(* CSS Syntax 3 (ED) sec. 3.3 ends a line at LF, CR or FF. *)
let line_break = function '\n' | '\r' | '\x0c' -> true | _ -> false

let snippet ?(window = 40) source loc =
  let len = String.length source in
  let pos = max 0 (min len loc.start_pos) in
  let end_pos = max pos (min len loc.end_pos) in
  (* [window] is a target radius, not a cap: a boundary that falls inside a code
     point moves outward to the lead byte, widening the snippet by up to three
     bytes a side. A snippet is a diagnostic, so keeping the sequence whole
     outranks keeping the byte budget. Inside the window the snippet stops at
     the line breaks around [pos], since the caret goes under one line. *)
  let low = Common.String.utf8_lead_before source (max 0 (pos - window)) in
  let high =
    Common.String.utf8_lead_after source (min len (end_pos + window))
  in
  let rec back i =
    if i > low && not (line_break source.[i - 1]) then back (i - 1) else i
  in
  let rec forward i =
    if i < high && not (line_break source.[i]) then forward (i + 1) else i
  in
  let start_pos = back pos and stop_pos = forward pos in
  let end_pos = min end_pos stop_pos in
  let text = String.sub source start_pos (stop_pos - start_pos) in
  let before = pos - start_pos and marked = end_pos - pos in
  let marker_pos = scalars ~pos:0 ~len:before text in
  (* An empty span still gets one caret, past the end of the line when that is
     where it points. *)
  let marker_len = max 1 (scalars ~pos:before ~len:marked text) in
  { Context.text; marker_pos; marker_len }
