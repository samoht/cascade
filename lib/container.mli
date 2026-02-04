(** CSS container query condition types *)

(** Container query condition type *)
type t =
  | Min_width_rem of float
      (** Container min-width in rem: [@container (min-width:Xrem)] *)
  | Min_width_px of int
      (** Container min-width in pixels: [@container (min-width:Xpx)] *)
  | Named of string * t
      (** Named container with condition: [@container name (condition)] *)
  | Raw of string  (** Escape hatch for complex/unknown conditions *)

val to_string : t -> string
(** [to_string t] converts a container condition to its CSS string
    representation. *)

val compare : t -> t -> int
(** [compare t1 t2] compares two container conditions. *)

(** {1 Container Condition Classification} *)

type kind =
  | Kind_min_width  (** min-width based query *)
  | Kind_other  (** Other/unknown condition *)

val kind : t -> kind
(** [kind t] returns the classification of a container condition. *)
