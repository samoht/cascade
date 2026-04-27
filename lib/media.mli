(** Structured media conditions for type-safe media query construction. *)

(** Comparison operator for range-form media features. *)
type cmp = Lt | Le | Eq | Gt | Ge

(** Value carried by a media feature: length, number, ratio, resolution, ident.
*)
type value =
  | Length of Values.length
  | Integer of int
  | Number of float
  | Ratio of int * int
  | Resolution of float * string
  | Ident of string

(** Media feature query: plain ([(name: value)]), boolean ([(name)]), or range
    forms ([(name op value)], [(value op name)], [(value op name op value)]). *)
type feature =
  | Plain of string * value
  | Boolean of string
  | Range of string * cmp * value
  | Range_rev of value * cmp * string
  | Interval of value * cmp * string * cmp * value

(** Media condition: nested combinations of features with [not], [and], [or]. *)
type condition =
  | Feature of feature
  | Not of condition
  | And of condition * condition
  | Or of condition * condition

(** Media type identifier. *)
type media_type = All | Screen | Print | Other of string

(** Prefix on a media query starting with a media type. *)
type prefix = Not | Only

type query =
  | Cond of condition
  | Type of {
      prefix : prefix option;
      type_ : media_type;
      trailing : condition option;
    }
  | List of query list  (** Comma-separated media query list. *)

(** Media condition. Provides type safety and consistent formatting. *)
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
  | Custom of query
      (** Structured query for forms not covered by the typed shorthands above
          (range syntax, combined media-type+condition, etc.). *)
  | Negated of t
      (** [not all and (condition)] or [not print] for media type negation *)

val of_string : string -> t
(** [of_string s] parses a CSS media query into the typed AST, collapsing
    recognised plain features into their typed shorthands and falling back to
    {!Custom} for richer forms. Raises {!Failure} for malformed input. *)

val to_string : t -> string
(** [to_string cond] renders the condition as a CSS media query string. Includes
    spaces after colons (non-minified form). *)

val pp : t Pp.t
(** [pp] pretty-prints the condition. *)

val pp_query : query Pp.t
(** [pp_query] pretty-prints a structured query. *)

val pp_condition : condition Pp.t
(** [pp_condition] pretty-prints a media condition. *)

val pp_feature : feature Pp.t
(** [pp_feature] pretty-prints a media feature. *)

val compare : t -> t -> int
(** [compare a b] compares conditions for sorting. Order: Hover < Other <
    Preference_accessibility < Responsive < Preference_appearance. *)

val equal : t -> t -> bool
(** [equal a b] tests structural equality. *)

(** Classification for sorting/grouping. *)
type kind =
  | Kind_hover
  | Kind_responsive of int * float
      (** (unit_order, value) -- unit_order: -2=calc, -1=em, 0=px, 1=rem, 2=vh
      *)
  | Kind_responsive_max of int * float
  | Kind_preference_accessibility
  | Kind_preference_appearance
  | Kind_other

val kind : t -> kind
(** [kind cond] classifies a condition for grouping. *)

val group_order : kind -> int * float
(** [group_order k] returns (group, value) for sorting. *)

val preference_order : t -> int
(** [preference_order cond] returns fine-grained order among preference
    conditions. *)
