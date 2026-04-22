(** Stage 3 stream: Token.t -> Component.t (CSS Syntax section 5).

    Ports the "consume a ..." algorithms from
    https://www.w3.org/TR/css-syntax-3/#parser-algorithms onto a {!Lexer.t}
    token stream. Produces the {!Component} IR consumed by the typed-AST
    validators. *)

type t
(** A component-value stream: a {!Lexer.t} plus one-component pushback. *)

val of_lexer : Lexer.t -> t
(** [of_lexer l] wraps an existing lexer stream. *)

val of_reader : Reader.t -> t
(** [of_reader r] builds a lexer from [r] and wraps it. *)

val of_string : string -> t
(** [of_string s] builds a fresh reader and lexer from [s]. *)

(** {1 Stream API} *)

val next : t -> Component.t
(** [next t] consumes the next component value. At end of input returns
    [Preserved Token.Eof]. Honours a component pushed back by {!reconsume}. *)

val peek : t -> Component.t
(** [peek t] is the next component value without consuming it. A subsequent
    {!peek} or {!next} returns the same value. *)

val reconsume : t -> Component.t -> unit
(** [reconsume t cv] pushes [cv] back so the next {!next} returns it. At most
    one component can be pushed back. *)

(** {1 Reserialization} *)

val to_string : Component.t list -> string
(** [to_string cvs] renders a component-value list back to source text.
    Whitespace tokens serialize to a single space; the output is
    parse-equivalent but not byte-identical. *)

(** {1 Entry points (section 5.4)} *)

val parse_stylesheet : Reader.t -> Component.rule list
(** [parse_stylesheet r] runs section 5.4.3: consume a list of rules with the
    top-level flag set. CDO and CDC are skipped. *)

val parse_list_of_rules : Reader.t -> Component.rule list
(** [parse_list_of_rules r] runs section 5.4.4. CDO/CDC are not discarded;
    suitable for nested rule bodies. *)

val parse_list_of_declarations :
  Reader.t -> [ `Decl of Component.declaration | `At of Component.at_rule ] list
(** [parse_list_of_declarations r] runs section 5.4.8. *)
