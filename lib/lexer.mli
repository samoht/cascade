(** Stage 2 stream: characters -> Token.t (CSS Syntax section 4).

    Wraps a {!Reader.t} and produces {!Token.t} values via the section 4.3
    tokenization algorithm. Exposes the uniform [next / peek / reconsume] triple
    used across parse stages.

    The input is already-decoded UTF-8 text. CSS Syntax section 3.2 byte-stream
    decoding is outside this layer. *)

type t
(** A lexer stream: a character cursor plus one-token pushback. *)

val of_reader : Reader.t -> t
(** [of_reader r] wraps an existing character reader. *)

val of_string : string -> t
(** [of_string s] builds a fresh reader from an already-decoded UTF-8 string and
    wraps it. *)

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

val is_non_ascii_ident_cp : int -> bool
(** [is_non_ascii_ident_cp cp] is the CSS Syntax section 4.2 predicate: is [cp]
    one of the non-ASCII code points allowed inside an ident sequence? Exposed
    for serialisers that decide whether to emit a code point verbatim or
    hex-escape it. *)
