(** Source locations: byte-offset ranges in the original input.

    Every {!Token.t} and {!Component.t} carries one, letting downstream
    validators report errors with precise positions and recover ranges for
    editor integrations. *)

type meta_level = [ `None | `Locs | `Full ]

let default_meta_level : meta_level = `Full

type t = { start_pos : int; end_pos : int }

let v ~start_pos ~end_pos = { start_pos; end_pos }
let dummy = { start_pos = 0; end_pos = 0 }

let union a b =
  { start_pos = min a.start_pos b.start_pos; end_pos = max a.end_pos b.end_pos }

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
    | Nth n -> "[" ^ string_of_int n ^ "]"
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

let snippet ?(window = 40) source loc =
  let len = String.length source in
  let pos = max 0 (min len loc.start_pos) in
  let end_pos = max pos (min len loc.end_pos) in
  (* [window] is a target radius, not a cap: a boundary that falls inside a code
     point moves outward to the lead byte, widening the snippet by up to three
     bytes a side. A snippet is a diagnostic, so keeping the sequence whole
     outranks keeping the byte budget. *)
  let start_pos =
    Common.String.utf8_lead_before source (max 0 (pos - window))
  in
  let stop_pos =
    Common.String.utf8_lead_after source (min len (end_pos + window))
  in
  let text = String.sub source start_pos (stop_pos - start_pos) in
  let before = pos - start_pos and marked = end_pos - pos in
  let marker_pos = scalars ~pos:0 ~len:before text in
  let marked_chars = scalars ~pos:before ~len:marked text in
  let after_chars =
    scalars ~pos:(before + marked) ~len:(stop_pos - end_pos) text
  in
  {
    Context.text;
    marker_pos;
    marker_len = min (max 1 marked_chars) (marked_chars + after_chars);
  }
