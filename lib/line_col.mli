(** Lines and columns of byte offsets in a source text.

    As in CSS Syntax 3 (ED) sec. 3.3, CR, FF and a CRLF pair each end one line,
    whether or not the text went through the reader's preprocessing. A column
    counts one code point, or one maximal ill-formed subpart, rather than a
    byte. Lines and columns are 1-based. *)

type t
(** The type for a source and the index of its positions. The index is a line
    and column every {!stride} bytes, built by one walk of the source on the
    first {!locate} and kept for the next ones, so a value shared by everything
    that reports positions in one source walks it once. *)

val v : string -> t
(** [v source] indexes [source]. It reads nothing until the first {!locate}. *)

val source : t -> string
(** [source t] is the text [t] indexes. *)

val locate : ?probes:int ref -> t -> int -> int * int
(** [locate ?probes t pos] is the line and column of byte [pos] of [source t],
    clamped to it, read off at most {!stride} bytes past the nearest indexed
    position: its cost does not grow with [pos], with the length of the line
    [pos] is on, or with the source. A lookup allocates a constant number of
    words, the walk and its answer.

    [probes], when given, is incremented by the number of source bytes the
    lookup reads: up to the whole source on the lookup that builds the index,
    and at most {!stride} plus three on any other. *)

val stride : int
(** [stride] is the distance in bytes between two indexed positions. *)
