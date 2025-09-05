type t
(** Abstract type for CSS selectors *)

type combinator =
  | Descendant  (** Space: "div p" *)
  | Child  (** >: "div > p" *)
  | Next_sibling  (** +: "div + p" *)
  | Subsequent_sibling  (** ~: "div ~ p" *)
  | Column  (** ||: column combinator "col || th" *)

type ns =
  | Any
  | None
  | Prefix of string
      (** CSS namespace qualifier for type, universal, and attribute selectors.
      *)

val element : ?ns:ns -> string -> t
(** [element name] is an element selector (e.g., "div", "p"). Validates that
    [name] is a valid CSS identifier. Raise [Invalid_argument] if [name]
    violates CSS identifier rules. *)

val class_ : string -> t
(** [class_ name] is a class selector (e.g., ".button", ".prose"). Validates
    that [name] is a valid CSS identifier. Raise [Invalid_argument] if [name]
    violates CSS identifier rules. *)

val id : string -> t
(** [id name] is an ID selector (e.g., "#header"). Validates that [name] is a
    valid CSS identifier. Raise [Invalid_argument] if [name] violates CSS
    identifier rules. *)

val universal : t
(** [universal] is the universal selector "*" (no namespace). *)

val universal_ns : ns -> t
(** [universal_ns ns] is the namespaced universal selector, e.g., [*|*] or
    [svg|*]. *)

(** Typed attribute selector values *)
type attribute_match =
  | Presence  (** [attribute] - attribute exists *)
  | Exact of string  (** [attribute="value"] - exact match *)
  | Whitespace_list of string
      (** [attribute~="value"] - whitespace-separated list *)
  | Hyphen_list of string  (** [attribute|="value"] - hyphen-separated list *)
  | Prefix of string  (** [attribute^="value"] - prefix match *)
  | Suffix of string  (** [attribute$="value"] - suffix match *)
  | Substring of string  (** [attribute*="value"] - substring match *)

type attr_flag =
  | Case_insensitive
  | Case_sensitive
      (** CSS attribute selector flags: [i] (ASCII case-insensitive), [s]
          (case-sensitive). *)

val attribute : ?ns:ns -> ?flag:attr_flag -> string -> attribute_match -> t
(** [attribute name match] is a typed attribute selector. Validates that [name]
    is a valid CSS identifier. Raise [Invalid_argument] if [name] violates CSS
    identifier rules. *)

val pseudo_class : string -> t
(** [pseudo_class name] is a pseudo-class selector (e.g., ":hover",
    ":first-child"). Validates that [name] is a valid CSS identifier. Raise
    [Invalid_argument] if [name] violates CSS identifier rules. *)

val pseudo_element : string -> t
(** [pseudo_element name] is a pseudo-element selector (e.g., "::before",
    "::marker"). Validates that [name] is a valid CSS identifier. Raise
    [Invalid_argument] if [name] violates CSS identifier rules. *)

val combine : t -> combinator -> t -> t
(** [combine a c b] is [a] combined with [b] using combinator [c]. *)

val ( ++ ) : t -> t -> t
(** [sel1 ++ sel2] is [sel1] and [sel2] combined with the descendant combinator
    (space). Equivalent to [combine sel1 Descendant sel2]. *)

val ( >> ) : t -> t -> t
(** [sel1 >> sel2] is [sel1] and [sel2] combined with the child combinator (>).
    Equivalent to [combine sel1 Child sel2]. *)

val where : t list -> t
(** [where sels] is a [:where()] functional pseudo-class. *)

val not : t list -> t
(** [not sels] is a [:not()] functional pseudo-class. *)

val fun_ : string -> t list -> t
(** [fun_ name sels] is a functional pseudo-class like [:is()], [:has()], etc.
*)

val list : t list -> t
(** [list sels] is a comma-separated selector list. *)

val is_compound_list : t -> bool
(** [is_compound_list s] is [true] if [s] is already a comma-separated list of
    selectors. *)

val compound : t list -> t
(** [compound selectors] is a compound selector made by combining multiple
    simple selectors (e.g., element + attribute: "div[class=foo]"). *)

val ( && ) : t -> t -> t
(** [sel1 && sel2] is a compound selector of two simple selectors. Equivalent to
    [compound [sel1; sel2]]. Example: [element "div" && class_ "foo"]. *)

val ( || ) : t -> t -> t
(** [sel1 || sel2] combines selectors with the CSS column combinator. *)

val pp : t Pp.t
(** Pretty printer for selectors *)

val to_string : ?minify:bool -> t -> string
val pp_combinator : combinator Pp.t
val pp_attribute_match : attribute_match Pp.t
val pp_ns : ns Pp.t
val pp_attr_flag : attr_flag option Pp.t

type nth =
  | Even
  | Odd
  | An_plus_b of int * int  (** Typed An+B notation for :nth-* selectors. *)

val pp_nth : nth Pp.t
(** Pretty-printer for nth expressions. *)

val read : Reader.t -> t
(** [read t] parses a CSS selector from the reader. *)

val read_opt : Reader.t -> t option
(** [read_opt t] parses a CSS selector and returns [None] on failure. *)

val read_combinator : Reader.t -> combinator
val read_attribute_match : Reader.t -> attribute_match
val read_ns : Reader.t -> ns option
val read_attr_flag : Reader.t -> attr_flag option
val read_nth : Reader.t -> nth

val is_ : t list -> t
(** Typed helpers for functional selectors *)

val has : t list -> t
val nth_child : ?of_:t list -> nth -> t
val nth_last_child : ?of_:t list -> nth -> t
val nth_of_type : ?of_:t list -> nth -> t
val nth_last_of_type : ?of_:t list -> nth -> t
val pseudo_element_fun : string -> t list -> t
val part : string list -> t
val slotted : t list -> t
val cue : t list -> t
val cue_region : t list -> t
