(** Keyframe position types for type-safe [\@keyframes] construction. *)

(** A single keyframe position. *)
type position =
  | From  (** [from] or [0%] *)
  | To  (** [to] or [100%] *)
  | Percent of float  (** Percentage like [50%] *)

val position_to_string : position -> string
(** [position_to_string pos] renders a position as CSS string. *)

(** A keyframe selector (one or more positions, or raw string). *)
type selector = Positions of position list | Raw of string

val selector_to_string : selector -> string
(** [selector_to_string sel] renders a selector as CSS string. *)

val position_compare : position -> position -> int
(** [position_compare a b] compares two positions for sorting. *)

val position_of_string : string -> position option
(** [position_of_string s] parses a position string like "from", "to", "50%". *)

val selector_of_string : string -> selector
(** [selector_of_string s] parses a selector string. Always succeeds - returns
    [Raw s] if parsing fails. *)

val selector_equal : selector -> selector -> bool
(** [selector_equal a b] checks if two selectors are equal. *)
