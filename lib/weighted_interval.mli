(** Weighted interval scheduling over dense integer positions.

    Intervals are inclusive ranges [[start, stop]] with positive weights. The
    scheduler selects a maximum-weight subset with no overlapping ranges. *)

type 'a interval = private { start : int; stop : int; weight : int; value : 'a }
type 'a t

val v : length:int -> 'a t
(** [v ~length] creates a schedule over positions [0] to [length - 1]. [length]
    must be non-negative. *)

val add : 'a t -> start:int -> stop:int -> weight:int -> 'a -> unit
(** [add t ~start ~stop ~weight value] adds one candidate interval. Non-positive
    weights are ignored because they cannot improve the optimum. Raises
    [Invalid_argument] if the range is outside [t] or [start > stop]. *)

val select : 'a t -> int * 'a interval list
(** [select t] returns [(weight, intervals)] for an optimal non-overlapping
    schedule. Returned intervals are in ascending source order. *)
