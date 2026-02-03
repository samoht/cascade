(** CSS selectors: core types and helpers. *)

include module type of Selector_intf
(** Shared selector types exposed by both implementation and interface. *)

val element : ?ns:ns -> string -> t
(** [element ?ns name] element selector (e.g., "div"). Validates CSS
    identifiers; raises [Invalid_argument] on invalid. *)

val class_ : string -> t
(** [class_ name] class selector from raw (unescaped) string. Accepts any
    serializable characters including special chars that will be escaped during
    pretty-printing. Only rejects control characters and '--' prefix. Raises
    [Invalid_argument] on invalid. *)

val id : string -> t
(** [id name] ID selector from raw (unescaped) string. Accepts any serializable
    characters including special chars that will be escaped during
    pretty-printing. Only rejects control characters and '--' prefix. Raises
    [Invalid_argument] on invalid. *)

val of_string : string -> t
(** [of_string s] parses a CSS-escaped selector string:
    - [".classname"] → class selector
    - ["#idname"] → id selector
    - ["element"] → element selector

    Unescapes both simple escapes (e.g., ["\:"]) and hex escapes (e.g.,
    ["\3A"]). Example: [of_string ".sm\\:p-4"] creates a class selector for
    "sm:p-4". *)

val universal : t
(** [universal] universal selector "*" (no namespace). *)

val attribute : ?ns:ns -> ?flag:attr_flag -> string -> attribute_match -> t
(** [attribute ?ns ?flag name match] attribute selector. Validates identifiers;
    raises [Invalid_argument] on invalid. *)

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

val list : t list -> t
(** [list selectors] comma-separated selector list. *)

val is_compound_list : t -> bool
(** [is_compound_list selector] returns [true] if already a list of selectors.
*)

val as_list : t -> t list option
(** [as_list selector] returns [Some selectors] if [selector] is a list, [None]
    otherwise. Useful for pattern matching on merged selectors. *)

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

val map : (t -> t) -> t -> t
(** [map f selector] recursively applies [f] to all selectors in the tree. The
    function [f] is applied bottom-up: first to descendants, then to the current
    node. This is useful for transforming class names throughout a complex
    selector structure. *)

val pp_combinator : combinator Pp.t
(** [pp_combinator] pretty-prints selector combinators. *)

val pp_attribute_match : attribute_match Pp.t
(** [pp_attribute_match] pretty-prints attribute matchers. *)

val pp_ns : ns Pp.t
(** [pp_ns] pretty-prints namespaces. *)

val pp_attr_flag : attr_flag option Pp.t
(** [pp_attr_flag] pretty-prints optional attribute flags. *)

val attr_value_needs_quoting : string -> bool
(** [attr_value_needs_quoting value] returns [true] if the given attribute value
    requires quoting according to CSS specifications. Values need quotes if:
    - They are empty
    - They start with a digit
    - They start with double hyphen (--)
    - They contain characters that are not valid in CSS identifiers (anything
      other than letters, digits, hyphens, underscores, or non-ASCII) *)

val pp_nth : nth Pp.t
(** [pp_nth] pretty-prints nth expressions. *)

val read_selector_list : Reader.t -> t
(** [read_selector_list r] reads a selector list without checking for end of
    input. Used when parsing selectors as part of a larger CSS structure. *)

val read : Reader.t -> t
(** [read r] parses a CSS selector. *)

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

val host : ?selectors:t list -> unit -> t
(** [host ?selectors ()] [:host] or [:host(selector)] pseudo-class. *)

(** Analysis helpers (structure-based, no string scanning) *)
val any : (t -> bool) -> t -> bool
(** [any p sel] returns [true] if any node in [sel] satisfies [p]. *)

val has_focus : t -> bool
(** Check presence of [:focus] pseudo-class in the selector. *)

val has_focus_within : t -> bool

val has_focus_visible : t -> bool
(** Check presence of focus-related pseudo-classes in the selector. *)

val exists_class : (string -> bool) -> t -> bool
(** [exists_class pred sel] returns [true] if any class node satisfies [pred].
*)

val first_class : t -> string option
(** [first_class sel] returns the first class name found along the leftmost path
    (Compound > Class, then left side of Combined, or first in List), or [None]
    if no class is found. *)

val contains_modifier_colon : t -> bool
(** [contains_modifier_colon sel] returns [true] if any class name contains a
    modifier colon (e.g., "md:...", "hover:..."). *)

val has_group_marker : t -> bool
(** [has_group_marker sel] returns [true] if selector contains [:where(.group)],
    indicating a group-* modifier like group-hover, group-focus, group-has. *)

val has_peer_marker : t -> bool
(** [has_peer_marker sel] returns [true] if selector contains [:where(.peer)],
    indicating a peer-* modifier like peer-checked, peer-focus, peer-has. *)

val modifier_prefix : t -> string option
(** [modifier_prefix sel] extracts the modifier prefix from the first class in
    the selector. Returns [Some "before:"] for ".before:absolute",
    [Some "hover:"] for ".hover:bg-blue-500", [None] for ".shadow" or
    ".shadow-sm".

    This is used by the CSS optimizer to determine if selectors can be safely
    merged while preserving cascade semantics. Selectors with different modifier
    prefixes target different elements and must remain separate. *)
