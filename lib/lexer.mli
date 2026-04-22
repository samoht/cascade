(** Stage 2 stream: characters -> Token.t (CSS Syntax section 4).

    Wraps a {!Reader.t} and produces {!Token.t} values via the section 4.3
    tokenization algorithm. Exposes the uniform [next / peek / reconsume] triple
    used across parse stages. *)

type t
(** A lexer stream: a character cursor plus one-token pushback. *)

val of_reader : Reader.t -> t
(** [of_reader r] wraps an existing character reader. *)

val of_string : string -> t
(** [of_string s] builds a fresh reader from [s] and wraps it. *)

val next : t -> Token.t
(** [next t] consumes the next token. Returns {!Token.Eof} at end of input.
    Honours any token pushed back by {!reconsume}. *)

val peek : t -> Token.t
(** [peek t] is the next token without consuming it. A subsequent {!peek} or
    {!next} returns the same token. *)

val reconsume : t -> Token.t -> unit
(** [reconsume t tok] pushes [tok] back so the next {!next} returns it. At most
    one token can be pushed back. *)

val is_done : t -> bool
(** [is_done t] is [true] when no more tokens remain. *)
