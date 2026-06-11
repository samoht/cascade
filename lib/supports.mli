(** Structured [\@supports] conditions for type-safe feature query construction.
*)

(** Supports condition type. Provides type safety and consistent formatting. *)

type property_name

type declaration_feature =
  | Declaration of Declaration.t
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
  | Not of t  (** [not (condition)] negation *)
  | And of t * t  (** [(cond1) and (cond2)] conjunction *)
  | Or of t * t  (** [(cond1) or (cond2)] disjunction *)

val property : string -> string -> t
(** [property name value] parses [name: value] as a structured supports
    declaration feature. *)

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

val of_string : ?allow_unwrapped_decl:bool -> string -> t
(** [of_string s] parses a [\@supports] condition string into a structured type.
    Fails if the condition cannot be parsed. *)

val compare : t -> t -> int
(** [compare a b] compares conditions for sorting. *)

val equal : t -> t -> bool
(** [equal a b] tests structural equality. *)

val simplify_baseline : t -> [ `True | `False | `Cond of t ]
(** [simplify_baseline cond] classifies [cond] against Cascade's evergreen
    feature-support baseline (its typed property set) and simplifies it. [`True]
    / [`False] mean the whole condition is statically decided; [`Cond c] keeps
    the residual condition with known-true [and]-conjuncts and known-false
    [or]-disjuncts removed. Function features and properties Cascade does not
    model stay in the residual. *)
