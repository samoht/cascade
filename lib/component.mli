(** Stage 3 IR: CSS Syntax section 5.1 component values and rules.

    Stage 3 of the pipeline (chars -> lexer stream -> token stream -> AST): the
    output of {!Parser}, consumed by the typed-AST validators. *)

type t = Preserved of Token.t | Block of block | Func of func
and block = { opening : Token.bracket; value : t list }
and func = { name : string; arguments : t list }

type at_rule = { name : string; prelude : t list; block : block option }
type qualified_rule = { prelude : t list; block : block }
type rule = Qualified of qualified_rule | At of at_rule
type declaration = { name : string; value : t list; important : bool }
