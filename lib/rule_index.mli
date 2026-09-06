(** Per-rule declaration index for the shorthand composer pipeline.

    A shorthand composer looks for a contiguous run of specific longhands (e.g.
    [outline-width / -style / -color], or the 12
    [border-* per side x width/style/color] longhands) and merges them into a
    shorthand declaration. Naively each composer scans the rule's declaration
    list once, so running 14 composers per rule walks the list 14 times. The
    index answers "where in the rule does property P appear?" in O(1), so each
    composer becomes a handful of lookups + a contiguity check.

    The index is built once per rule and composers record absorbed positions
    + emitted shorthands by mutating a parallel slot array; {!to_list} flattens
      the result back into a declaration list in original cascade order. *)

type t

val build : Declaration.declaration list -> t
(** [build decls] constructs an index over [decls]. Positions are dense
    [0 .. List.length decls - 1] indices into the input. *)

val length : t -> int
(** [length t] is the number of decl positions in the index. *)

val decl_at : t -> int -> Declaration.declaration
(** [decl_at t i] returns the declaration at position [i]. *)

val positions : t -> 'a Properties.property -> int list
(** [positions t p] is the cascade-ordered list of positions at which the typed
    property [p] appears in the rule. *)

val absorb :
  t -> at:int -> absorbed:int list -> shorthand:Declaration.declaration -> bool
(** [absorb t ~at ~absorbed ~shorthand] records that [shorthand] should appear
    at position [at] (typically the earliest absorbed position) and that each
    position in [absorbed] is consumed by the shorthand. Repeated absorptions of
    the same position are a programming error. It is {!splice} with a
    single-declaration list, and refuses on the same terms. *)

val splice :
  t ->
  at:int ->
  absorbed:int list ->
  new_decls:Declaration.declaration list ->
  bool
(** [splice t ~at ~absorbed ~new_decls] generalises {!absorb}: emit each
    declaration of [new_decls] in order at position [at] and mark every position
    in [absorbed] as consumed. Suits composers that need to replace a contiguous
    run with multiple declarations (e.g. a non-important shorthand followed by a
    re-stated important longhand).

    [false], with the index left untouched, when a declaration of [new_decls]
    mixes a CSS-wide keyword with other components: CSS Cascade 5 sec. 7.3 makes
    that invalid CSS whatever family built it, so the index is the one gate
    every composer's emission passes through. The caller leaves the run as its
    longhands. *)

val is_absorbed : t -> int -> bool
(** [is_absorbed t i] is [true] when position [i] has been absorbed by a
    composer. *)

val to_list : t -> Declaration.declaration list
(** [to_list t] linearises the index back into a declaration list. Positions are
    emitted in their original cascade order; an absorbed position is replaced by
    the shorthand attached to it (when the position is the [at] site) or skipped
    (otherwise). *)
