(** Stage 2 stream: characters -> Token.t (CSS Syntax 3 (ED) sec. 4).

    Wraps a {!Reader.t} and produces {!Token.t} values via the section 4.3
    tokenization algorithm. Exposes the uniform [next / peek / reconsume] triple
    used across parse stages.

    The input is already-decoded UTF-8 text. CSS Syntax 3 (ED) sec. 3.2
    byte-stream decoding is outside this layer. *)

type t
(** A lexer stream: a character cursor plus one-token pushback. *)

type comment = { loc : Loc.t; terminated : bool }
(** A comment consumed before tokenization. [loc] covers its opening [/*]
    through its closing [*/], or through end of input when [terminated] is
    [false]. Comments remain absent from the token stream as CSS Syntax
    requires; this record is an opt-in tooling hook. *)

val of_reader :
  ?on_comment:(comment -> unit) -> ?unicode_ranges:bool -> Reader.t -> t
(** [of_reader ?on_comment ?unicode_ranges r] wraps an existing character
    reader. [on_comment] observes each consumed comment exactly once.
    [unicode_ranges] is CSS Syntax 3 (ED) sec. 4.3.1's "unicode ranges allowed",
    defaulting to [false]: sec. 4.3.14 names its one caller, the value of a
    [unicode-range] descriptor, so everywhere else [u+a] is an ident, a delim
    and an ident. *)

val of_string :
  ?enforce_spec:bool ->
  ?on_comment:(comment -> unit) ->
  ?unicode_ranges:bool ->
  string ->
  t
(** [of_string s] builds a fresh reader from an already-decoded UTF-8 string and
    wraps it. [enforce_spec] is passed to {!Reader.of_string}; [on_comment] and
    [unicode_ranges] have the meaning documented on {!of_reader}. *)

val with_unicode_ranges : t -> (unit -> 'a) -> 'a
(** [with_unicode_ranges t f] runs [f] with CSS Syntax 3 (ED) sec. 4.3.1's
    "unicode ranges allowed" set, restoring it afterwards. Sec. 5.5.11 is the
    one caller: the value of a [unicode-range] descriptor. *)

val source : t -> string
(** [source t] is the full input string the underlying reader was built from. *)

val next : t -> Token.t
(** [next t] consumes the next token. Returns {!Token.Eof} at end of input.
    Honours any token pushed back by {!reconsume}. *)

val peek : t -> Token.t
(** [peek t] is the next token without consuming it. A subsequent {!peek} or
    {!next} returns the same token. *)

val reconsume : t -> Token.t -> unit
(** [reconsume t tok] pushes [tok] back so the next {!next} returns it. The
    pushback buffer is unbounded -- multiple [reconsume] calls stack. *)

val save : t -> unit
(** [save t] records the current position. A subsequent {!restore} replays every
    token consumed since this {!save} so the next {!next} returns the same
    sequence again. {!save}/{!restore}/{!commit} stack: nested saves are
    independent. *)

val restore : t -> unit
(** [restore t] replays the tokens consumed since the most recent {!save}. Pops
    one entry off the save stack. *)

val commit : t -> unit
(** [commit t] discards the most recent {!save} without rewinding. The replay
    log is folded into the parent save (if any) so an outer {!restore} still
    sees the consumed tokens. *)

val is_done : t -> bool
(** [is_done t] is [true] when no more tokens remain. *)

val spec_non_ascii_ident_cp : int -> bool
(** [spec_non_ascii_ident_cp cp] is the CSS Syntax 3 (ED) sec. 4.2 predicate: is
    [cp] in that section's range list of non-ASCII ident code points? Exposed
    for serialisers, which hex-escape anything outside it; an escape is read by
    every parser, so emission stays on this list even though reading accepts any
    code point [>= U+0080] unless [~enforce_spec:true]. *)
