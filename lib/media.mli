(** Structured media conditions for type-safe media query construction. *)

(** Media condition type. Provides type safety and consistent formatting. *)
type t =
  | Min_width of float  (** Responsive breakpoint: [(min-width:Xrem)] *)
  | Max_width of float  (** Max-width query: [(max-width:Xrem)] *)
  | Prefers_reduced_motion of [ `No_preference | `Reduce ]
  | Prefers_contrast of [ `More | `Less ]
  | Prefers_color_scheme of [ `Dark | `Light ]
  | Forced_colors of [ `Active | `None ]
  | Hover  (** [(hover:hover)] *)
  | Print  (** [print] media type *)
  | Raw of string  (** Escape hatch for complex/unknown conditions *)

val to_string : t -> string
(** [to_string cond] renders the condition as a CSS media query string. *)

val compare : t -> t -> int
(** [compare a b] compares conditions for sorting. Order: Hover < Other/Raw <
    Preference_accessibility < Responsive < Preference_appearance. *)

val equal : t -> t -> bool
(** [equal a b] tests structural equality. *)

(** Classification for sorting/grouping. *)
type kind =
  | Kind_hover
  | Kind_responsive of float
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
