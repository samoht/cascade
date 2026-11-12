(** The authored-source snapshot of one parse, {!Stylesheet.Source}: the exact
    input, its preprocessed form, the recovered rules and comments with their
    ranges, and the map from preprocessed offsets back to the input. *)

type t
(** The type for source snapshots. *)

type comment = { loc : Loc.t; terminated : bool }
(** A lexer comment; [terminated] is [false] when end of input closed it. *)

type rule = {
  syntax : Component.rule;  (** The located component tree of the rule. *)
  loc : Loc.t;  (** The rule's own range in {!preprocessed}. *)
  owned_loc : Loc.t;  (** From the previous rule's end through this rule's. *)
}
(** A recovered top-level rule. *)

type position = { byte : int; line : int; column : int }
(** An input byte offset with its one-based line and Unicode-scalar column. *)

type span = { start : position; end_ : position }
(** The two boundaries of a range. *)

val v :
  original:string ->
  preprocessed:string ->
  comments:Lexer.comment list ->
  Component.rule list ->
  t
(** [v ~original ~preprocessed ~comments rules] is the snapshot of a parse of
    [original] whose preprocessed form is [preprocessed]. *)

val contents : t -> string
(** [contents t] is the caller's input, byte for byte. *)

val preprocessed : t -> string
(** [preprocessed t] is the preprocessed input every {!Loc.t} indexes. *)

val comments : t -> comment list
(** [comments t] is every lexer comment, in source order. *)

val rules : t -> rule list
(** [rules t] is every recovered top-level rule. *)

val trailing_loc : t -> Loc.t
(** [trailing_loc t] is the range after the last rule. *)

val slice : t -> Loc.t -> string
(** [slice t loc] is [loc] of {!preprocessed}. Raises [Invalid_argument] when
    [loc] is outside it. *)

val original_loc : t -> Loc.t -> Loc.t
(** [original_loc t loc] maps the boundaries of [loc] back to {!contents}. *)

val original_slice : t -> Loc.t -> string
(** [original_slice t loc] is the input bytes {!original_loc} covers. *)

val position : t -> int -> position
(** [position t offset] is the input position of the preprocessed [offset]. *)

val span : t -> Loc.t -> span
(** [span t loc] maps both boundaries of [loc] with {!position}. *)
