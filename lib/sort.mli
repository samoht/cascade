(** Closed enum of IR node categories.

    Used in error messages and (eventually) error contexts to label which kind
    of CSS construct a parser was working on when something went wrong. Mirrors
    the IR layering: tokens (section 4) -> components (section 5.1) -> the typed
    AST. *)

type t =
  | Token  (** A section 4 lexeme. *)
  | Component
      (** A section 5.1 component value (preserved token, block, or func). *)
  | Block  (** A balanced [\{...\}], [(...)], or [[...]] group. *)
  | Function  (** A [name(...)] call. *)
  | At_rule  (** An at-rule, e.g. [@media ...]. *)
  | Qualified_rule  (** A style rule (selector + block). *)
  | Declaration  (** A [property: value] declaration. *)
  | Selector  (** A selector list. *)
  | Property_value  (** The right-hand side of a declaration. *)
  | Stylesheet  (** A whole section 5.4 stylesheet. *)

val pp : t Pp.t
(** [pp] renders the sort as a lowercase identifier, e.g. [at-rule]. *)

val to_string : t -> string
(** [to_string t] renders [t] as a lowercase identifier. *)
