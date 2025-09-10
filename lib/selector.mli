(** CSS selectors: core types and helpers. *)

include module type of Selector_intf
(** Shared selector types exposed by both implementation and interface. *)

val element : ?ns:ns -> string -> t
(** Element selector (e.g., "div"). Validates CSS identifiers; raises
    [Invalid_argument] on invalid. *)

val class_ : string -> t
(** Class selector (e.g., ".prose"). Validates CSS identifiers; raises
    [Invalid_argument] on invalid. *)

val id : string -> t
(** ID selector (e.g., "#header"). Validates CSS identifiers; raises
    [Invalid_argument] on invalid. *)

val universal : t
(** Universal selector "*" (no namespace). *)

val universal_ns : ns -> t
(** Namespaced universal selector (e.g., [*|*], [svg|*]). *)

(* Typed attribute selector helpers *)

val attribute : ?ns:ns -> ?flag:attr_flag -> string -> attribute_match -> t
(** Attribute selector. Validates CSS identifiers; raises [Invalid_argument] on
    invalid. *)

val pseudo_class : string -> t
(** Pseudo-class selector (e.g., ":hover"). Validates identifiers where
    applicable. *)

val pseudo_element : string -> t
(** Pseudo-element selector (e.g., "::before"). Validates identifiers. *)

val combine : t -> combinator -> t -> t
(** Combine selectors with a combinator. *)

val ( ++ ) : t -> t -> t
(** Combine with descendant (space). *)

val ( >> ) : t -> t -> t
(** Combine with child (>). *)

val where : t list -> t
(** [:where(...)] pseudo-class. *)

val not : t list -> t
(** [:not(...)] pseudo-class. *)

val fun_ : string -> t list -> t
(** Functional pseudo-class (e.g., [:is], [:has]). *)

val list : t list -> t
(** Comma-separated selector list. *)

val is_compound_list : t -> bool
(** True if already a list of selectors. *)

val compound : t list -> t
(** Compound selector (concatenates simple selectors). *)

val ( && ) : t -> t -> t
(** Combine two simple selectors into a compound selector. *)

val ( || ) : t -> t -> t
(** Combine with column combinator (||). *)

val pp : t Pp.t
(** Pretty-printer for selectors. *)

val to_string : ?minify:bool -> t -> string
val pp_combinator : combinator Pp.t
val pp_attribute_match : attribute_match Pp.t
val pp_ns : ns Pp.t
val pp_attr_flag : attr_flag option Pp.t

val pp_nth : nth Pp.t
(** Pretty-printer for nth expressions. *)

val read_selector_list : Reader.t -> t
(** Read a selector list without checking for end of input. Used when parsing
    selectors as part of a larger CSS structure. *)

val read : Reader.t -> t
(** Parse a CSS selector. *)

val read_opt : Reader.t -> t option
(** Parse a CSS selector; returns [None] on failure. *)

val read_combinator : Reader.t -> combinator
(** Parse a combinator. *)

val read_attribute_match : Reader.t -> attribute_match
(** Parse an attribute matcher. *)

val read_ns : Reader.t -> ns option
(** Parse an optional attribute/selector namespace. *)

val read_attr_flag : Reader.t -> attr_flag option
(** Parse an attribute selector flag ([i] or [s]). *)

val read_nth : Reader.t -> nth
(** Parse an An+B nth expression. *)

val is_ : t list -> t
(** [:is(...)] pseudo-class. *)

val has : t list -> t
(** [:has(...)] pseudo-class. *)

val nth_child : ?of_:t list -> nth -> t
(** [:nth-child] with optional [:of]. *)

val nth_last_child : ?of_:t list -> nth -> t
(** [:nth-last-child] with optional [:of]. *)

val nth_of_type : ?of_:t list -> nth -> t
(** [:nth-of-type] with optional [:of]. *)

val nth_last_of_type : ?of_:t list -> nth -> t
(** [:nth-last-of-type] with optional [:of]. *)

val pseudo_element_fun : string -> t list -> t
(** Pseudo-element with selector list payload (e.g., ::slotted). *)

val part : string list -> t
(** ::part(...) pseudo-element. *)

val slotted : t list -> t
(** ::slotted(...) pseudo-element. *)

val cue : t list -> t
(** ::cue(...) pseudo-element. *)

val cue_region : t list -> t
(** ::cue-region(...) pseudo-element. *)
