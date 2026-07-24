(** Keyframe position types for type-safe [\@keyframes] construction. *)

(** A single keyframe position. *)
type position =
  | From  (** [from] or [0%] *)
  | To  (** [to] or [100%] *)
  | Percent of float  (** Percentage like [50%] *)
  | Timeline_range of string * float
      (** Scroll-Driven Animations 1 sec. 8.1
          [<timeline-range-name> <percentage>] selector such as [entry 0%]. *)

val string_of_position : position -> string
(** [string_of_position pos] renders a position as CSS string. *)

(** A keyframe selector (one or more positions). *)
type selector = Positions of position list

type t = selector
(** A keyframe selector. *)

val pp : t Pp.t
(** [pp] renders a keyframe selector. *)

val string_of_selector : selector -> string
(** [string_of_selector sel] renders a selector as CSS string. *)

val to_string : t -> string
(** [to_string sel] renders a selector as CSS source text. *)

val position_compare : position -> position -> int
(** [position_compare a b] compares two positions for sorting. *)

val position_of_string : string -> position option
(** [position_of_string s] parses a position string like "from", "to", "50%". *)

val selector_of_string : string -> selector
(** [selector_of_string s] parses a selector string. Raises [Invalid_argument]
    if parsing fails. *)

val selector_equal : selector -> selector -> bool
(** [selector_equal a b] checks if two selectors are equal. *)
