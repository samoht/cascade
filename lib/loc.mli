(** Source locations: byte-offset ranges in the original input.

    Every {!Token.t} and {!Component.t} carries one. *)

type meta_level = [ `None | `Locs | `Full ]
(** Metadata level collected during parsing, mirroring [ocaml-json]'s
    [type meta]:
    - [`None]: no source positions or snippets attached; fastest.
    - [`Locs]: source positions are kept on tokens / components / errors
      (already unconditional in Cascade since tokens always carry {!t}).
      Equivalent to [`None] in Cascade -- present for API symmetry with
      [ocaml-json].
    - [`Full]: also attach source-context snippets to errors and warnings, so
      {!Context.field-snippet} is populated for diagnostics. *)

val default_meta_level : meta_level
(** [default_meta_level] is [`Full]. Entry points default to this so that
    callers who don't pass [?meta] get the rich diagnostics behaviour. *)

type t = { start_pos : int; end_pos : int }

val v : start_pos:int -> end_pos:int -> t
(** [v ~start_pos ~end_pos] is a location spanning [\[start_pos, end_pos)]. *)

val dummy : t
(** [dummy] is a location of zero width at position 0. For test data and
    synthetic values that don't come from a real input. *)

val union : t -> t -> t
(** [union a b] is the smallest location covering both [a] and [b]. *)

val pp : t Pp.t
(** [pp] formats as [[start-end]]. *)

val to_string : t -> string
(** [to_string t] formats [t] as [[start-end]]. *)

module Path : sig
  type step =
    | Mem of string
    | Nth of int
    | Label of string
        (** Path step used to describe a descent into parsed structure. *)

  type t
  (** A root-to-leaf path. *)

  val empty : t
  (** [empty] is the root path. *)

  val push : step -> t -> t
  (** [push step t] appends [step] below [t]. *)

  val last : t -> step option
  (** [last t] is the leaf step of [t], if any. *)

  val to_list : t -> step list
  (** [to_list t] returns the root-to-leaf path steps. *)

  val of_labels : string list -> t
  (** [of_labels labels] builds a path from slash-separated context labels. *)

  val to_labels : t -> string list
  (** [to_labels t] renders each path step as a label. *)

  val pp : t Pp.t
  (** [pp] formats {!type-t} as slash-separated labels. *)
end

module Context : sig
  type snippet = { text : string; marker_pos : int; marker_len : int }
  (** Source text around a location plus the caret marker span. *)

  type nonrec t = {
    path : Path.t;
    loc : t;
    sort : Sort.t;
    snippet : snippet option;
  }
  (** Location, sort, path and optional source context for an error site. *)

  val push : Path.step -> t -> t
  (** [push step t] appends [step] to [t]'s path. *)
end

val snippet : ?window:int -> string -> t -> Context.snippet
(** [snippet source loc] extracts a snippet of [source] around [loc]:
    {!Context.field-text} is the byte window (default: 40 bytes on each side of
    the location), {!Context.field-marker_pos} points at [loc.start_pos] within
    {!Context.field-text}, and {!Context.field-marker_len} spans the located
    range. *)
