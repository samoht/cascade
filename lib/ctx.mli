(** Optimizer context. *)

type scope = [ `Fragment | `Stylesheet ]
(** Surrounding CSS context assumed by optimizations. *)

type objective = [ `Raw | `Transfer ]
(** Size metric optimizations are judged by: raw bytes, or estimated DEFLATE
    (gzip) transfer bytes. *)

type t
(** Shared optimizer context. One is built per optimizer run and threaded to
    every pass, so it also carries the run's profiling recorder. *)

val fragment : t
(** Default fragment context. Being a constant it carries a recorder shared by
    everything that uses it; build a context with {!v} to collect a run's
    profile. *)

val of_scope :
  ?lossless:bool ->
  ?aggressive:bool ->
  ?regroup:bool ->
  ?extend_lists:bool ->
  ?closed_world:bool ->
  ?objective:objective ->
  ?enforce_spec:bool ->
  ?targets:Support.targets ->
  ?judge:Support.targets ->
  ?stats:Stats.t ->
  scope option ->
  t
(** Build a context from an optional scope. [targets] names the floors the
    minifier serves (default {!Support.evergreen}); [judge] names the browsers
    the run judges for (default none), and [enforce_spec] drops it. *)

val v :
  ?lossless:bool ->
  ?aggressive:bool ->
  ?regroup:bool ->
  ?extend_lists:bool ->
  ?closed_world:bool ->
  ?objective:objective ->
  ?enforce_spec:bool ->
  ?targets:Support.targets ->
  ?judge:Support.targets ->
  ?registered:(string -> bool) ->
  ?resets_held:
    (rule:Declaration.declaration list ->
    Declaration.declaration list ->
    Declaration.declaration ->
    bool) ->
  ?stats:Stats.t ->
  scope ->
  t
(** Build a context explicitly. [stats] defaults to a fresh recorder, so a
    context that is not handed one still counts its run's work, into a recorder
    nobody reads. *)

val scope : t -> scope
(** Scope assumed by optimizations. *)

val registered : t -> string -> bool
(** Whether a custom property name is registered. *)

val resets_held :
  t ->
  rule:Declaration.declaration list ->
  Declaration.declaration list ->
  Declaration.declaration ->
  bool
(** [resets_held t ~rule answered shorthand] is whether [shorthand], composed in
    [rule], sets to its initial value a longhand that no declaration of
    [answered] writes (its run, and the declarations of [rule] that win over it)
    and that a declaration outside [rule], or the user-agent sheet, may hold at
    another value. The composer answers for [rule]'s own declarations. It is
    [true] when the context was built without that knowledge. *)

val lossless : t -> bool
(** Whether lossless value optimization is enabled. *)

val regroup : t -> bool
(** Whether rules may be regrouped: shared declarations factored into a selector
    list, and nesting synthesised from a run of adjacent rules. Both depend on
    how the input happened to order its rules - a rule sitting between two
    others decides whether either applies - so a canonical projection turns them
    off to stay confluent. *)

val aggressive : t -> bool
(** Whether expensive optimization passes (notably the global factoring
    fixpoint) run regardless of the preflight's byte-gain estimate. *)

val extend_lists : t -> bool
(** Whether DAG identical-body factoring may absorb candidates into an existing
    {!Selector.List} rule or treat such a rule as a multi-subselector candidate.
    Off by default for direct scheduler callers; the main optimizer enables it
    through the guarded DAG candidate selector, which also keeps the strict
    non-list alternative when that is locally better. *)

val recurse_blocks : t -> bool
(** Whether statement optimization descends into child blocks. False only for
    the seam pass over blocks whose children were already optimized. *)

val closed_world : t -> bool
(** Whether the caller knows the exact HTML the CSS will style.

    Off by default: the optimizer assumes any HTML is possible, so it never
    merges or reorders two rules when some element could match both selectors
    and get a different result. That keeps the output correct for any page.

    On: the caller promises the clashing selector combinations never appear on a
    real element, which lets the optimizer merge more. It is unsafe if such an
    element does turn up, including one a script adds at runtime. This is about
    the HTML, separate from {!type-scope}, which is about how much of the CSS
    you control. *)

val objective : t -> objective
(** Size metric optimizations are judged by. Under [`Transfer] (the default)
    every rewrite is scored by an estimate of the compressed bytes it saves: a
    selector branch or a declaration printed before costs a match of a few
    bytes, a new one costs its text. Rules with one body merge only where they
    print within 128 rules of each other, since a selector moved away from its
    neighbours stops compressing against them, which the estimate does not see.
    [`Raw] scores raw bytes and merges them at any distance, the right objective
    when the output ships uncompressed (inline [style], email) or for structural
    comparisons against raw-size oracles. *)

val enforce_spec : t -> bool
(** Whether the evergreen-browser target is dropped. Off by default: a vendor-
    prefixed declaration whose unprefixed twin is present may be stripped, since
    modern browsers understand the unprefixed form. On: keep every prefix. *)

val judge : t -> Support.targets option
(** The browsers the run judges for: a [@supports] guard they all satisfy is
    decided and a fallback they all parse past is dead. [None], the default and
    what {!enforce_spec} forces, keeps every guard and fallback for the engine
    that renders the sheet to answer. *)

val floors : t -> Support.targets
(** The floors a fallback is judged against: {!judge} when the run judges for
    browsers, the targets the minifier serves otherwise. Each stands for its
    version and every later release, so an earlier declaration is dead only when
    every browser at or above them that reads it reads the later one
    ({!Declaration.fallback_for}). They hold under {!val-enforce_spec} too,
    since they only ever keep a declaration. *)

val parses_nesting : t -> bool
(** [parses_nesting t] is whether every browser the run serves parses CSS
    Nesting, so that a nested rule may be emitted: every engine of {!floors} is
    at or past the release MDN's browser-compat-data gives for
    [css.selectors.nesting] in full (Chrome 120, Firefox 117, Safari 17.2).
    Chrome 111 and Safari 16.4, the default floors, read no nested rule, and
    Chrome 112 to 119 and Safari 16.5 to 17.1 refuse one whose selector starts
    with an identifier. Always [false] under {!val-enforce_spec}, whose
    conforming implementations include one that predates CSS Nesting. *)

val stats : t -> Stats.t
(** Profiling recorder for the run this context belongs to. *)

val with_extend_lists : bool -> t -> t
(** [with_extend_lists enabled ctx] returns [ctx] with only the list-extension
    strategy flag changed. *)

val with_recurse_blocks : bool -> t -> t
(** [with_recurse_blocks enabled ctx] returns [ctx] with only child-block
    recursion changed. *)

val pp : t Pp.t
(** Pretty-printer for debugging. *)
