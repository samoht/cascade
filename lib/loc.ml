(** Source locations: byte-offset ranges in the original input.

    Every {!Token.t} and {!Component.t} carries one, letting downstream
    validators report errors with precise positions and recover ranges for
    editor integrations. *)

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
