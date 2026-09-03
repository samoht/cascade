(** CSS selectors: core types and helpers. *)

include module type of Selector_intf
(** Shared selector types exposed by both implementation and interface. *)

val pp_component_values : component_values Pp.t
(** [pp_component_values] pretty-prints preserved selector component values. *)

val read_component_values : Cursor.t -> component_values
(** [read_component_values t] reads preserved selector component values. *)

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
(** [of_string s] parses a complete CSS selector, including complex and
    comma-separated selectors. CSS escapes are decoded. Raises
    {!Error.Parse_error} when [s] is malformed. *)

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

val to_buffer : ?minify:bool -> Buffer.t -> t -> unit
(** [to_buffer ?minify buf sel] renders a selector into [buf]. *)

val equal : t -> t -> bool
(** [equal a b] tests selectors for structural equality. *)

val compare : t -> t -> int
(** [compare a b] totally orders selectors structurally. *)

val hash : t -> int
(** [hash selector] returns a hash consistent with {!equal}. *)

val equal_specificity : specificity -> specificity -> bool
(** [equal_specificity a b] tests specificity triples for equality. *)

val equal_combinator : combinator -> combinator -> bool
(** [equal_combinator a b] tests selector combinators for equality. *)

val specificity : t -> specificity
(** [specificity selector] computes Selectors specificity as
    [(ids, classes, elements)]. For selector-list-like pseudos such as [:is()],
    [:not()], and [:has()], the most specific argument is used; [:where()]
    contributes zero. *)

val compare_specificity : specificity -> specificity -> int
(** [compare_specificity a b] orders specificity triples lexicographically. *)

val map : (t -> t) -> t -> t
(** [map f selector] recursively applies [f] to all selectors in the tree. The
    function [f] is applied bottom-up: first to descendants, then to the current
    node. This is useful for transforming class names throughout a complex
    selector structure. *)

val canonicalize : t -> t
(** [canonicalize selector] removes lexical-only redundancy so that distinct
    ASTs denoting the same selector become structurally equal: the implied
    universal in a compound is dropped ([*::before] -> [::before]), a one-part
    compound collapses to that part, and a whole-selector [:is(s1, s2, ...)]
    whose arguments share one specificity becomes the selector list
    [s1, s2, ...] (CSS Selectors 4 sec. 4.2). [selector] is read as a rule
    selector, so the split applies at its top level and to the members of its
    top-level list. Idempotent. *)

val pp_combinator : combinator Pp.t
(** [pp_combinator] pretty-prints selector combinators. *)

val pp_attribute_match : attribute_match Pp.t
(** [pp_attribute_match] pretty-prints attribute matchers. *)

val pp_ns : ns Pp.t
(** [pp_ns] pretty-prints namespaces. *)

val pp_attr_flag : attr_flag option Pp.t
(** [pp_attr_flag] pretty-prints optional attribute flags. *)

val pp_aria_attr : aria_attr Pp.t
(** [pp_aria_attr] pretty-prints aria attributes. *)

val read_aria_attr : Cursor.t -> aria_attr
(** [read_aria_attr t] parses an [aria_attr] from [t]. *)

val pp_attr_name : attr_name Pp.t
(** [pp_attr_name] pretty-prints attribute names. *)

val read_attr_name : Cursor.t -> attr_name
(** [read_attr_name t] parses an [attr_name] from [t]. *)

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

val read_selector_list : Cursor.t -> t
(** [read_selector_list r] reads a selector list without checking for end of
    input. Used when parsing selectors as part of a larger CSS structure. *)

val read_strict_selector_list : Cursor.t -> t
(** [read_strict_selector_list r] reads a non-forgiving selector list, rejecting
    unknown pseudo-classes and other invalid selector-list arms. *)

val read : Cursor.t -> t
(** [read r] parses a CSS selector. *)

val read_relative : Cursor.t -> t
(** [read_relative r] parses a CSS relative selector (may start with a
    combinator like [+], [>], or [~]). Used for :has() arguments. *)

val drop_redundant_nesting_prefix : t -> t
(** [drop_redundant_nesting_prefix sel] removes a redundant leading [&] from a
    nested-rule selector: [& .bar] -> [.bar], [& > .bar] -> [> .bar]. A nested
    selector is implicitly relative to the parent [&] (CSS Nesting 1 sec. 3), so
    this is shape-preserving for selectors used in a nested position. *)

val read_combinator : Cursor.t -> combinator
(** [read_combinator r] parses a combinator. *)

val read_attribute_match : Cursor.t -> attribute_match
(** [read_attribute_match r] parses an attribute matcher. *)

val read_ns : Cursor.t -> ns option
(** [read_ns r] parses an optional attribute/selector namespace. *)

val read_attr_flag : Cursor.t -> attr_flag option
(** [read_attr_flag r] parses an attribute selector flag ([i] or [s]). *)

val read_nth : Cursor.t -> nth
(** [read_nth r] parses an An+B nth expression. *)

val read_nth_selector : Cursor.t -> nth * t list option
(** [read_nth_selector r] parses an An+B expression with optional [of S]. *)

val is_ : t list -> t
(** [is_ selectors] [:is(...)] pseudo-class. *)

val has : t list -> t
(** [has selectors] [:has(...)] pseudo-class. *)

val nth_child : ?of_:t list -> nth -> t
(** [nth_child ?of_ nth] builds [:nth-child] with optional [:of]. *)

val host : ?selectors:t list -> unit -> t
(** [host ?selectors ()] [:host] or [:host(selector)] pseudo-class. *)

val any : (t -> bool) -> t -> bool
(** [any predicate selector] returns [true] if any node in [selector] satisfies
    [predicate]. Analysis helper (structure-based, no string scanning). *)

val has_focus : t -> bool
(** [has_focus sel] checks presence of [:focus] pseudo-class in the selector. *)

val has_focus_within : t -> bool
(** [has_focus_within sel] checks presence of [:focus-within] pseudo-class in
    the selector. *)

val has_focus_visible : t -> bool
(** [has_focus_visible sel] checks presence of [:focus-visible] pseudo-class in
    the selector. *)

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

val has_is_where_pattern : t -> bool
(** [has_is_where_pattern sel] returns [true] if the selector uses the
    [:is(:where(...))] pattern typical of group-* and peer-* variants. *)

val is_newer_pseudo_class : t -> bool
(** [is_newer_pseudo_class sel] returns [true] if [sel] is a newer pseudo-class
    with limited browser support ([:user-valid], [:user-invalid]). *)

val has_newer_pseudo_class : t -> bool
(** [has_newer_pseudo_class sel] returns [true] if the selector directly uses a
    newer pseudo-class (not nested inside [:is()]/[:where()] which provides
    forgiving parsing). Newer pseudo-classes should not be combined in selector
    lists with group/peer variants to preserve browser compatibility. *)

val has_combinator_after_pseudo_element : t -> bool
(** [has_combinator_after_pseudo_element sel] is [true] when [sel] puts a
    combinator after a pseudo-element, which CSS Selectors 4 sec. 3.6.5 makes
    invalid and {!of_string} refuses. Reading it off a built selector catches
    the selectors nesting composes, which join a valid parent to a valid child
    without either one being refused. [>>>] and [/deep/] do not count: no engine
    parses them, so the rule never reaches them. *)

val has_refused_simple_in_compound : t -> bool
(** [has_refused_simple_in_compound sel] is [true] when a compound in [sel]
    carries, after a pseudo-element, a simple selector that pseudo-element does
    not take: CSS Selectors 4 sec. 3.6.3 and sec. 3.6.4, the same rows
    {!of_string} applies while reading. Nesting extends a pseudo-element's
    compound out of a valid parent and a valid child, so the composed selector
    escapes the reader's check. *)

val is_pseudo_element : t -> bool
(** [is_pseudo_element sel] is [true] when [sel] is one simple selector naming a
    pseudo-element: a box other than the originating element, where an
    element-tied pseudo-class such as [:hover] only narrows which elements
    match. Every [::] form counts, including a name cascade does not recognise,
    since Selectors 4 sec. 16 shapes a pseudo-compound around a pseudo-element
    whatever its name. *)

val has_pseudo_element : t -> bool
(** [has_pseudo_element sel] returns [true] if selector contains a
    pseudo-element like ::before, ::after, ::marker, etc. *)

val matches_nothing : t -> bool
(** [matches_nothing sel] returns [true] when [sel] cannot match any element.
    The canonical case is an empty forgiving [:is()] or [:where()] (every
    argument was an invalid pseudo-class), and the predicate propagates through
    compound, combined, and relative selectors. A selector list matches nothing
    only when every entry does. *)

val has_unknown_pseudo_class : t -> bool
(** [has_unknown_pseudo_class sel] returns [true] when [sel] contains an
    [Unknown_pseudo_class] / [Unknown_pseudo_class_call] anywhere in the tree.
    Used to flag unforgiving-site unknown pseudo-classes ([.x,:future-pseudo])
    as spec deviations in strict mode. *)

val modifier_prefix : t -> string option
(** [modifier_prefix sel] extracts the modifier prefix from the first class in
    the selector. Returns [Some "before:"] for ".before:absolute",
    [Some "hover:"] for ".hover:bg-blue-500", [None] for ".shadow" or
    ".shadow-sm".

    This is used by the CSS optimizer to determine if selectors can be safely
    merged while preserving cascade semantics. Selectors with different modifier
    prefixes target different elements and must remain separate. *)
