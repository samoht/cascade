(** CSS container query condition types *)

(** Container query condition type *)
type t =
  | Min_width_rem of float
      (** Container min-width in rem: [@container (min-width:Xrem)] *)
  | Min_width_px of int
      (** Container min-width in pixels: [@container (min-width:Xpx)] *)
  | Named of string * t
      (** Named container with condition: [@container name (condition)] *)
  | Style of string * string option
      (** Style query: [style(--flag)] or [style(property: value)]. *)
  | Scroll_state of string * string
      (** Scroll-state query: [scroll-state(stuck: top)]. *)
  | And of t * t  (** [(A) and (B)] *)
  | Or of t * t  (** [(A) or (B)] *)
  | Not of t  (** [not (A)] *)
  | Feature_query of Media.t
      (** Container size/range feature query, e.g. [(inline-size: 640px)] or
          [(inline-size > 30em)]. *)
  | Custom of Media.t  (** Structured condition beyond the typed shorthands. *)

val to_string : t -> string
(** [to_string t] converts a container condition to its CSS string
    representation. *)

val pp : t -> string
(** [pp t] returns a string representation of a container condition. *)

val of_string : string -> t
(** [of_string s] parses a container condition. Raises [Failure] for malformed
    conditions. *)

val feature : string -> Media.value -> t
(** [feature name value] is the typed container feature query constructed via
    {!Media.feature}. *)

val style : ?value:string -> string -> t
(** [style ?value prop] is a [style()] query: [Style (prop, value)]. With no
    [value] it matches the boolean form [style(--flag)]; with a value it matches
    [style(prop: value)]. *)

val scroll_state : string -> string -> t
(** [scroll_state prop value] is [Scroll_state (prop, value)], matching
    [scroll-state(prop: value)]. *)

val compare : t -> t -> int
(** [compare t1 t2] compares two container conditions. *)

(** {1 Container Condition Classification} *)

type kind =
  | Min_width  (** min-width based query *)
  | Other  (** Other/unknown condition *)

val kind : t -> kind
(** [kind t] returns the classification of a container condition. *)
