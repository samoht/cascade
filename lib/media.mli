(** Structured media conditions for type-safe media query construction. *)

(** Media condition type. Provides type safety and consistent formatting. *)
type t =
  | Min_width of float  (** Responsive breakpoint: [(min-width:Xpx)] *)
  | Max_width of float  (** Max-width query: [(max-width:Xpx)] *)
  | Not_min_width of float
      (** Negated breakpoint: [not all and (min-width:Xpx)] *)
  | Min_width_rem of float  (** Responsive breakpoint: [(min-width:Xrem)] *)
  | Not_min_width_rem of float
      (** Negated breakpoint: [not all and (min-width:Xrem)] *)
  | Min_width_length of Values.length
      (** Arbitrary length breakpoint: [(min-width:<length>)] *)
  | Not_min_width_length of Values.length
      (** Negated arbitrary length breakpoint:
          [not all and (min-width:<length>)] *)
  | Prefers_reduced_motion of [ `No_preference | `Reduce ]
  | Prefers_contrast of [ `More | `Less ]
  | Prefers_color_scheme of [ `Dark | `Light ]
  | Forced_colors of [ `Active | `None ]
  | Inverted_colors of [ `Inverted | `None ]  (** [(inverted-colors:...)] *)
  | Pointer of [ `None | `Coarse | `Fine ]  (** [(pointer:...)] *)
  | Any_pointer of [ `None | `Coarse | `Fine ]  (** [(any-pointer:...)] *)
  | Scripting of [ `None | `Initial_only | `Enabled ]  (** [(scripting:...)] *)
  | Hover  (** [(hover:hover)] *)
  | Print  (** [print] media type *)
  | Orientation of [ `Portrait | `Landscape ]  (** [(orientation:...)] *)
  | Raw of string  (** Escape hatch for complex/unknown conditions *)
  | Negated of t
      (** [not all and (condition)] or [not print] for media type negation *)

val to_string : t -> string
(** [to_string cond] renders the condition as a CSS media query string. Always
    includes spaces after colons (non-minified form). *)

val pp : t Pp.t
(** [pp] pretty-prints the condition. *)

val compare : t -> t -> int
(** [compare a b] compares conditions for sorting. Order: Hover < Other/Raw <
    Preference_accessibility < Responsive < Preference_appearance. *)

val equal : t -> t -> bool
(** [equal a b] tests structural equality. *)

(** Classification for sorting/grouping. *)
type kind =
  | Kind_hover
  | Kind_responsive of int * float
      (** (unit_order, value) — unit_order: -2=calc, -1=em, 0=px, 1=rem, 2=vh *)
  | Kind_responsive_max of int * float
  | Kind_preference_accessibility
  | Kind_preference_appearance
  | Kind_other

val kind : t -> kind
(** [kind cond] classifies a condition for grouping. *)

val kind_of_string : string -> kind
(** [kind_of_string s] classifies a raw condition string. For backward compat.
*)

val group_order : kind -> int * float
(** [group_order k] returns (group, value) for sorting. *)

val preference_order : t -> int
(** [preference_order cond] returns fine-grained order among preference
    conditions. *)
