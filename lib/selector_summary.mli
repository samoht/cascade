(** Cheap structural summary of a selector, for use by the optimizer's safety
    checks.

    Modelled on the CSS-graph framing of Hague, Lin, and Hong's "CSS
    Minification via Constraint Solving" (TOPLAS 2019): a selector's subject
    compound determines which DOM elements it matches, and two summaries are
    {b definitely disjoint} when no element can match both. The summary records
    the cheap-to-extract facts about the subject (rightmost compound) and a
    [complex] bit that flags anything the summary can't characterize. Functions
    that ask "do these two selectors overlap?" use the summary instead of
    walking the selector tree on every pairwise call. *)

type t
(** Structural summary of a selector's subject compound. *)

val of_selector : Selector.t -> t
(** [of_selector sel] computes the summary in O(size of [sel]). The summary is
    keyed on the rightmost compound (the subject element) so that, e.g., [.a .b]
    and [.c .b] summarize the same way - both target a [.b] element. *)

val may_overlap : t -> t -> bool
(** [may_overlap a b] is [true] when some DOM element might match both
    selectors. [false] is a hard guarantee that no element matches both, modulo
    invalid HTML with duplicate IDs.

    The predicate is conservative: it returns [true] whenever either summary is
    [complex], or whenever the cheap structural checks can't rule overlap out.
    Disjointness fires on:

    - different pseudo-elements ([::before] vs [::after], or one with and one
      without);
    - different element names in the subject ([div] vs [p]);
    - both subjects carry non-empty, disjoint ID sets (an element has at most
      one ID, so two selectors with distinct mandatory IDs can't match it). *)

val ids : t -> string list
(** [ids s] returns the IDs that appear in the summary's subject, in sorted
    order. Helper for tests and assertions. *)

val classes : t -> string list
(** [classes s] returns the classes in the summary's subject, sorted. *)

val element : t -> string option
(** [element s] is the element name in the subject, or [None] if the subject is
    universal / unspecified. *)

val is_complex : t -> bool
(** [is_complex s] is [true] when the summary fell back to "treat as overlapping
    with everything". Useful for tests that want to flag selectors the cheap
    check doesn't model. *)
