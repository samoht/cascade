(** CSS container query condition types *)

(** Container query condition type *)
type component_values = Values.component_values
(** Parsed CSS component values preserved inside style queries. *)

type t =
  | Min_width_rem of float
      (** Container min-width in rem: [@container (min-width:Xrem)] *)
  | Min_width_px of int
      (** Container min-width in pixels: [@container (min-width:Xpx)] *)
  | Named of string * t
      (** Named container with condition: [@container name (condition)] *)
  | Style of {
      query : style_query;
      uppercase : bool;
          (** [true] when the source spelled the function as [STYLE(...)] (case
              preserved for round-trip fidelity); [false] for the canonical
              lowercase spelling. *)
    }
      (** Style query: [style(--flag)], [style(property: value)], or a
          custom-property range query, optionally composed with [and], [or], or
          [not]. *)
  | Scroll_state of {
      query : scroll_state_query;
      uppercase : bool;
          (** [true] when the source spelled the function as
              [SCROLL-STATE(...)]. *)
    }  (** Scroll-state query: [scroll-state(stuck: top)]. *)
  | And of t * t  (** [(A) and (B)] *)
  | Or of t * t  (** [(A) or (B)] *)
  | Not of t  (** [not (A)] *)
  | Feature_query of Media.t
      (** Container size/range feature query, e.g. [(inline-size: 640px)] or
          [(inline-size > 30em)]. *)

and style_query =
  | Boolean of string
  | Declaration of { name : string; value : component_values }
  | Range of style_range
  | All of style_query * style_query
  | Any of style_query * style_query
  | Neg of style_query

and style_range = {
  lower : component_values;
  lower_op : range_operator;
  name : string;
  upper_op : range_operator;
  upper : component_values;
}

and range_operator = Lt | Lte | Gt | Gte

and scroll_state_query =
  | State of { name : string; value : string }
  | Both of scroll_state_query * scroll_state_query
  | Either of scroll_state_query * scroll_state_query
  | Negated of scroll_state_query

type kind = Min_width | Other  (** Coarse container condition category. *)

val kind : t -> kind
(** [kind t] classifies min-width-only conditions for compatibility helpers. *)

val lower_for_minify : t -> t
(** [lower_for_minify t] applies {!Media.lower_for_minify} to every nested
    feature query, leaving the dedicated [Min_width_*], style, and scroll-state
    forms untouched. *)

val to_string : ?minify:bool -> t -> string
(** [to_string t] converts a container condition to its CSS string
    representation. Typed [Min_width_*] shorthands keep their historical compact
    form; stylesheet printing uses {!to_stylesheet_string} when pretty spacing
    is required. *)

val to_stylesheet_string : ?minify:bool -> t -> string
(** [to_stylesheet_string t] converts a container condition for stylesheet
    printing. Non-minified output keeps optional whitespace in typed shorthand
    feature queries. *)

val pp : t -> string
(** [pp t] returns a string representation of a container condition. *)

val of_string : string -> t
(** [of_string s] parses a container condition. Raises [Failure] for malformed
    conditions. *)

val feature : string -> Media.value -> t
(** [feature name value] is the typed container feature query constructed via
    {!Media.val-feature}. *)

val style : ?value:string -> string -> t
(** [style ?value prop] is a [style()] query. With no [value], [prop] must be a
    custom property and the query matches the boolean form [style(--prop)]. With
    a value it matches [style(prop: value)]. Constructs the canonical lowercase
    form. *)

val scroll_state : string -> string -> t
(** [scroll_state prop value] is the canonical lowercase
    {!constructor-Scroll_state} query, matching [scroll-state(prop: value)]. *)

val compare : t -> t -> int
(** [compare t1 t2] compares two container conditions. *)
