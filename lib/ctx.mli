(** Optimizer context. *)

type scope = [ `Fragment | `Stylesheet ]
(** Surrounding CSS context assumed by optimizations. *)

type objective = [ `Raw | `Transfer ]
(** Size metric optimizations are judged by: raw bytes, or estimated DEFLATE
    (gzip) transfer bytes. *)

type t
(** Shared optimizer context. *)

val fragment : t
(** Default fragment context. *)

val of_scope :
  ?lossless:bool ->
  ?aggressive:bool ->
  ?extend_lists:bool ->
  ?closed_world:bool ->
  ?objective:objective ->
  ?enforce_spec:bool ->
  scope option ->
  t
(** Build a context from an optional scope. *)

val v :
  ?lossless:bool ->
  ?aggressive:bool ->
  ?extend_lists:bool ->
  ?closed_world:bool ->
  ?objective:objective ->
  ?enforce_spec:bool ->
  ?registered:(string -> bool) ->
  scope ->
  t
(** Build a context explicitly. *)

val scope : t -> scope
(** Scope assumed by optimizations. *)

val registered : t -> string -> bool
(** Whether a custom property name is registered. *)

val lossless : t -> bool
(** Whether lossless value optimization is enabled. *)

val aggressive : t -> bool
(** Whether expensive optimization passes (notably the global factoring
    fixpoint) run regardless of the preflight's byte-gain estimate. *)

val extend_lists : t -> bool
(** Whether DAG identical-body factoring may absorb candidates into an existing
    {!Selector.List} rule or treat such a rule as a multi-subselector candidate.
    Off by default for direct scheduler callers; the main optimizer enables it
    through the guarded DAG candidate selector, which also keeps the strict
    non-list alternative when that is locally better. *)

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
(** Size metric optimizations are judged by. Under [`Transfer] (the default) the
    global factoring result is kept only when it does not grow the estimated
    DEFLATE-compressed output; [`Raw] keeps every raw-byte win, the right
    objective when the output ships uncompressed (inline [style], email) or for
    structural comparisons against raw-size oracles. *)

val enforce_spec : t -> bool
(** Whether the evergreen-browser target is dropped. Off by default: a vendor-
    prefixed declaration whose unprefixed twin is present may be stripped, since
    modern browsers understand the unprefixed form. On: keep every prefix. *)

val with_extend_lists : bool -> t -> t
(** [with_extend_lists enabled ctx] returns [ctx] with only the list-extension
    strategy flag changed. *)

val pp : t Pp.t
(** Pretty-printer for debugging. *)
