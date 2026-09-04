(** Structured [\@supports] conditions for type-safe feature query construction.
*)

(** Supports condition type. Provides type safety and consistent formatting. *)

type property_name

type declaration_feature =
  | Declaration of Declaration.t
      (** The declaration the feature tests. Its value is the component stream
          the author wrote, read through
          {!Declaration.parse_opaque_declaration}: a browser answers the feature
          by parsing that exact declaration, so the property's typed grammar
          never re-spells it. *)
  | Empty of property_name
  | Unsupported of property_name * string
  | Vendor_flag_enabled

type font_format =
  | Collection
  | Embedded_opentype
  | Opentype
  | Svg
  | Truetype
  | Woff
  | Woff2

type font_tech =
  | Features_opentype
  | Features_aat
  | Features_graphite
  | Color_colrv0
  | Color_colrv1
  | Color_svg
  | Color_sbix
  | Color_cbdt
  | Variations
  | Palettes
  | Incremental

val font_tech_of_string : string -> font_tech option
(** [font_tech_of_string s] is the [<font-tech>] keyword [s] spells (CSS Fonts 4
    sec. 11.1), or [None] for anything else. [s] is matched lower-cased. *)

val string_of_font_tech : font_tech -> string
(** [string_of_font_tech tech] is the canonical spelling of [tech]. *)

type function_feature =
  | Selector of Selector.t
  | Font_format of font_format
  | Font_tech of font_tech
  | At_rule of string
  | Named_feature of string
  | Env of string
  | General of string * string

type t =
  | Property of declaration_feature  (** [(property: value)] feature test *)
  | Function of function_feature
      (** Function feature test: [selector()], [font-format()], [font-tech()],
          [at-rule()], [named-feature()], [env()], or a general-enclosed
          function. *)
  | General_enclosed of string
      (** Opaque parenthesized condition, including its delimiters. *)
  | Not of t  (** [not (condition)] negation *)
  | And of t * t  (** [(cond1) and (cond2)] conjunction *)
  | Or of t * t  (** [(cond1) or (cond2)] disjunction *)

val property : string -> string -> t
(** [property name value] parses [name: value] as a structured supports
    declaration feature. [value] keeps the spelling it is given.

    @raise Failure
      if [value] is not a [<declaration-value>] (CSS Syntax 3 (ED) sec. 7.2).
      The feature writes the value between its own parentheses, so an unmatched
      closing bracket closes them and the tail becomes a second branch of the
      condition. *)

val func : string -> string -> t
(** [func name args] parses [args] as CSS component values for a supports
    function feature. *)

val to_string : t -> string
(** [to_string cond] renders the condition as a CSS [\@supports] string. *)

val pp : t Pp.t
(** [pp ctx cond] prints the condition with context-aware spacing. *)

val pp_declaration_feature : declaration_feature Pp.t
(** [pp_declaration_feature ctx feature] prints a supports declaration feature
    without the surrounding condition parentheses. *)

val read : ?allow_unwrapped_decl:bool -> Cursor.t -> t
(** [read t] parses the [\@supports] condition [t] holds. A malformed condition
    raises {!Cursor.exception-Parse_error} anchored on the slice of the
    condition that failed, so pass a cursor built over the components the
    prelude was lexed into ({!Cursor.val-sub}) rather than a re-lex of their
    text. *)

val of_string : ?allow_unwrapped_decl:bool -> string -> t
(** [of_string s] parses a [\@supports] condition string into a structured type.
    Raises {!Cursor.exception-Parse_error} if the condition cannot be parsed. *)

val compare : t -> t -> int
(** [compare a b] compares conditions for sorting. *)

val equal : t -> t -> bool
(** [equal a b] tests structural equality. *)

val simplify_under : context:t list -> t -> [ `True | `False | `Cond of t ]
(** [simplify_under ~context cond] decides [cond] against the conjunction [K] of
    the conditions enclosing it. [`True] means [K and not cond] is
    unsatisfiable, so [cond] holds wherever [K] does and its guard asks a
    question already answered; [`False] means [K and cond] is unsatisfiable, so
    the guard selects no user agent; [`Cond c] keeps the guard, narrowed to what
    [K] leaves of it when that is shorter and left as written otherwise.

    Every feature test is one propositional variable, keyed by {!compare}, and
    the decision is the truth table over them: CSS Conditional 3 sec. 6 gives
    every term a two-valued result, [<general-enclosed>] included. No support
    table is consulted, so the answer holds whatever the user agent answers. A
    chain carrying more than sixteen distinct feature tests is returned
    unchanged. *)
