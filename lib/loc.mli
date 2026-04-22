(** Source locations: byte-offset ranges in the original input.

    Every {!Token.t} and {!Component.t} carries one. *)

type t = { start_pos : int; end_pos : int }

val v : start_pos:int -> end_pos:int -> t
(** [v ~start_pos ~end_pos] is a location spanning [\[start_pos, end_pos)]. *)

val dummy : t
(** [dummy] is a location of zero width at position 0. For test data and
    synthetic values that don't come from a real input. *)

val union : t -> t -> t
(** [union a b] is the smallest location covering both [a] and [b]. *)

val pp : t Pp.t
(** [pp] formats as [[start-end]]. *)

val to_string : t -> string
