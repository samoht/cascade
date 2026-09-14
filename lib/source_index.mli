(** Offsets into CSS source, for rewriting it without reprinting it.

    A tool that edits CSS and has to give back the author's own bytes cannot go
    through {!Parser} and {!Pp}: reprinting normalises whitespace, comments and
    the spelling of every value, and a dialect built on CSS carries preludes
    such as [@import "x" theme(static) source(none)] that the grammar refuses
    outright. Such a tool splices the source instead, and the one thing it must
    not do is find the boundaries by counting braces - a [{] inside a string or
    a comment ends nothing.

    This index answers where each at-rule and function token in a source begins
    and ends, from the {!Component} values {!Parser} produced and the {!Loc.t}
    each carries. The caller then cuts the original text at offsets a parser
    supplied. *)

type block = { body : string; next : int }
(** A [\{ ... \}] or [( ... )] body, and the offset just past its close. *)

type header = { prelude : string; brace : int; block : block }
(** An [@name PRELUDE \{ ... \}] header: what stands between the at-keyword and
    the [\{], where that [\{] is, and the block it opens. *)

type statement = { prelude : string; next : int }
(** A blockless at-rule's prelude and the offset after its semicolon. When a
    closing brace terminates it, [next] points at that brace. *)

type t
(** Every at-rule and function token of one source, keyed by the offset it
    starts at. *)

val v : string -> t
(** [v css] indexes [css]. Every offset in the result is an index into this same
    string, so an edited copy needs a fresh index. *)

val call : t -> name:string -> int -> block option
(** [call t ~name i] is the arguments of [name( ... )] starting at [i], and
    [None] when no such call starts there. *)

val at_rule : t -> name:string -> int -> header option
(** [at_rule t ~name i] is the header of [@name PRELUDE \{ ... \}] starting at
    [i]. [name] carries its [@]. An at-rule with no block of its own is not one
    of these; see {!at_statement}. *)

val at_statement : t -> name:string -> int -> statement option
(** [at_statement t ~name i] is the blockless [@name PRELUDE;] starting at [i].
    [name] carries its [@]. *)

val calls : t -> name:string -> (int * block) list
(** [calls t ~name] is every [name( ... )] in the source, each with the offset
    it starts at. The order is unspecified. *)

val at_statements : t -> name:string -> (int * statement) list
(** [at_statements t ~name] is every blockless [@name PRELUDE;], the same way.
    The order is unspecified. *)
