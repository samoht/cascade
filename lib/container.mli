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

(** Coarse container condition category. *)
type kind = Min_width | Other

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

val pp : t Pp.t
(** [pp] is a composable printer for a container condition. *)

val of_string : string -> t
(** [of_string s] parses a container condition. Raises
    {!Cursor.exception-Parse_error} for a malformed condition. *)

val read : Cursor.t -> t
(** [read t] parses the container condition [t] holds, consuming nothing. A
    malformed condition raises {!Cursor.exception-Parse_error} anchored on the
    slice of the query that failed, so pass a cursor built over the components
    the prelude was lexed into ({!Cursor.val-sub}) rather than a re-lex of their
    text. *)

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

val normalize : t -> t
(** [normalize t] rewrites [t] into the spelling its equivalence class is
    compared by. The invariant runs one way: when [normalize a] and
    [normalize b] are equal, [a] and [b] select the same query containers.

    The converse is neither promised nor intended: two equivalent queries may
    normalise apart, which costs a merge and never correctness. So every rewrite
    is one of the spec's own equivalences. A [<size-feature>] is spelled like a
    media feature (CSS Conditional Rules 5 sec. 6.1), so it takes
    {!Media.normalize}: a [min-] or [max-] prefix on a range feature becomes the
    [>=] or [<=] comparison, a value-first bound becomes name-first (Media
    Queries 4 sec. 2.4.4 and sec. 2.4.3), and a descending interval becomes
    ascending. {!constructor-Min_width_rem} and {!constructor-Min_width_px} are
    cascade's compact spelling of [(min-width: V)] and enter that fold as the
    feature they spell. A [style()] or [scroll-state()] function name loses the
    case the source wrote it in (CSS Values 4 sec. 9), and a
    [<style-feature-plain>] loses the whitespace around its value (CSS Syntax 3
    sec. 5.5.6).

    Completeness is a separate property, and it stops there. Equivalences
    [normalize] leaves apart: a [style()] property name spelled in another case;
    two [style()] values that compute alike but tokenise apart, such as [red]
    against [#f00]; reassociating or reordering [and] and [or]; De Morgan pairs;
    two bounds on one feature against the written interval; a plain feature
    against its [=] comparison; and anything inside a [<general-enclosed>],
    whose text is compared verbatim. Each of those costs a merge, never
    correctness. *)

val compare : t -> t -> int
(** [compare t1 t2] compares two container conditions. Ordering only needs to be
    deterministic, so a size feature that ties on every ordinal key reaches
    {!Media.compare}'s serialised tiebreak. That makes [compare a b = 0] a
    different question from {!equal}, which reads structure: each says yes where
    the other says no. Sort with [compare], decide sameness with {!equal}. *)

val equal : t -> t -> bool
(** [equal a b] is structural equality on {!normalize}d conditions, so it is
    true only when [a] and [b] select the same query containers. It gates block
    merging and therefore never answers on serialised text: an unknown container
    feature spelled as one escaped ident is not the size feature whose
    characters it repeats, and a [<general-enclosed>] equals only the same text.
*)

val compare_scroll_state_query : scroll_state_query -> scroll_state_query -> int
(** [compare_scroll_state_query a b] totally orders scroll-state queries. *)

val equal_kind : kind -> kind -> bool
(** [equal_kind a b] tests container condition categories for equality. *)
