(** CSS selectors: core types and helpers. *)

include module type of Selector_intf
(** Shared selector types exposed by both implementation and interface. *)

val element : ?ns:ns -> string -> t
(** [element ?ns name] element selector (e.g., "div"). Validates CSS
    identifiers; raises [Invalid_argument] on invalid. *)

val class_ : string -> t
(** [class_ name] class selector (e.g., ".prose"). Validates identifiers; raises
    [Invalid_argument] on invalid. *)

val id : string -> t
(** [id name] ID selector (e.g., "#header"). Validates identifiers; raises
    [Invalid_argument] on invalid. *)

val universal : t
(** [universal] universal selector "*" (no namespace). *)

val universal_ns : ns -> t
(** [universal_ns ns] namespaced universal selector (e.g., [*|*], [svg|*]). *)

(* Typed attribute selector helpers *)

val attribute : ?ns:ns -> ?flag:attr_flag -> string -> attribute_match -> t
(** [attribute ?ns ?flag name match] attribute selector. Validates identifiers;
    raises [Invalid_argument] on invalid. *)

val pseudo_class : string -> t
(** [pseudo_class name] pseudo-class selector (e.g., ":hover"). Validates
    identifiers where applicable. *)

val pseudo_element : string -> t
(** [pseudo_element name] pseudo-element selector (e.g., "::before"). Validates
    identifiers. *)

val combine : t -> combinator -> t -> t
(** [combine a comb b] combines selectors with a combinator. *)

val ( ++ ) : t -> t -> t
(** [a ++ b] combines with descendant (space). *)

val ( >> ) : t -> t -> t
(** [a >> b] combines with child (>). *)

val where : t list -> t
(** [where selectors] [:where(...)] pseudo-class. *)

val not : t list -> t
(** [not selectors] [:not(...)] pseudo-class. *)

val fun_ : string -> t list -> t
(** [fun_ name args] functional pseudo-class (e.g., [:is], [:has]). *)

val list : t list -> t
(** [list selectors] comma-separated selector list. *)

val is_compound_list : t -> bool
(** [is_compound_list selector] returns [true] if already a list of selectors.
*)

val compound : t list -> t
(** [compound selectors] compound selector (concatenates simple selectors). *)

val ( && ) : t -> t -> t
(** [a && b] combines two simple selectors into a compound selector. *)

val ( || ) : t -> t -> t
(** [a || b] combines with column combinator (||). *)

val pp : t Pp.t
(** [pp] pretty-prints selectors. *)

val to_string : ?minify:bool -> t -> string
(** [to_string ?minify sel] renders a selector to a string. *)

val pp_combinator : combinator Pp.t
(** [pp_combinator] pretty-prints selector combinators. *)

val pp_attribute_match : attribute_match Pp.t
(** [pp_attribute_match] pretty-prints attribute matchers. *)

val pp_ns : ns Pp.t
(** [pp_ns] pretty-prints namespaces. *)

val pp_attr_flag : attr_flag option Pp.t
(** [pp_attr_flag] pretty-prints optional attribute flags. *)

val pp_nth : nth Pp.t
(** [pp_nth] pretty-prints nth expressions. *)

val read_selector_list : Reader.t -> t
(** [read_selector_list r] reads a selector list without checking for end of
    input. Used when parsing selectors as part of a larger CSS structure. *)

val read : Reader.t -> t
(** [read r] parses a CSS selector. *)

val read_opt : Reader.t -> t option
(** [read_opt r] parses a CSS selector; returns [None] on failure. *)

val read_combinator : Reader.t -> combinator
(** [read_combinator r] parses a combinator. *)

val read_attribute_match : Reader.t -> attribute_match
(** [read_attribute_match r] parses an attribute matcher. *)

val read_ns : Reader.t -> ns option
(** [read_ns r] parses an optional attribute/selector namespace. *)

val read_attr_flag : Reader.t -> attr_flag option
(** [read_attr_flag r] parses an attribute selector flag ([i] or [s]). *)

val read_nth : Reader.t -> nth
(** [read_nth r] parses an An+B nth expression. *)

val is_ : t list -> t
(** [is_ selectors] [:is(...)] pseudo-class. *)

val has : t list -> t
(** [has selectors] [:has(...)] pseudo-class. *)

val nth_child : ?of_:t list -> nth -> t
(** [nth_child ?of_ nth] builds [:nth-child] with optional [:of]. *)

val nth_last_child : ?of_:t list -> nth -> t
(** [nth_last_child ?of_ nth] builds [:nth-last-child] with optional [:of]. *)

val nth_of_type : ?of_:t list -> nth -> t
(** [nth_of_type ?of_ nth] builds [:nth-of-type] with optional [:of]. *)

val nth_last_of_type : ?of_:t list -> nth -> t
(** [nth_last_of_type ?of_ nth] builds [:nth-last-of-type] with optional [:of].
*)

val pseudo_element_fun : string -> t list -> t
(** [pseudo_element_fun name args] builds a functional pseudo-element (e.g.,
    ::slotted). *)

val part : string list -> t
(** [part names] builds a ::part(...) pseudo-element. *)

val slotted : t list -> t
(** [slotted selectors] builds a ::slotted(...) pseudo-element. *)

val cue : t list -> t
(** [cue selectors] builds a ::cue(...) pseudo-element. *)

val cue_region : t list -> t
(** [cue_region selectors] builds a ::cue-region(...) pseudo-element. *)
