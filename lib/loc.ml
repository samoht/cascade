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

let snippet ?(window = 40) source loc =
  let len = String.length source in
  let pos = max 0 (min len loc.start_pos) in
  let end_pos = max pos (min len loc.end_pos) in
  let start_pos = max 0 (pos - window) in
  let stop_pos = min len (end_pos + window) in
  let text = String.sub source start_pos (stop_pos - start_pos) in
  let marker_pos = pos - start_pos in
  let marker_len = max 1 (end_pos - pos) in
  let marker_len = min marker_len (String.length text - marker_pos) in
  { Context.text; marker_pos; marker_len }
