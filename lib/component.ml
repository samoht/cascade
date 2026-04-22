(** Stage 3 IR: CSS Syntax section 5.1 component values and rules.

    A component value is the grouped output of the §5 parser. Where a {!Token.t}
    is a flat lexer-stream atom, a {!Component.t} is either a preserved token or
    a nested group: a balanced block ({!Block}) or a named function call
    ({!Func}). The grammar-specific validators in {!Selector}, {!Values},
    {!Properties}, etc. consume lists of these. *)

type t =
  | Preserved of Token.t
      (** Any {!Token.t} except [Function], [Open _], [Close _]: passed through
          from the lexer stream. *)
  | Block of block
  | Func of func

and block = { opening : Token.bracket; value : t list }
(** A balanced [\{...\}], [(...)] or [[...]] group (section 5.1.5). *)

and func = { name : string; arguments : t list }
(** A [name(...)] call, section 5.1.4. *)

type at_rule = { name : string; prelude : t list; block : block option }
(** An at-rule (section 5.1.7): name, prelude, optional block. *)

type qualified_rule = { prelude : t list; block : block }
(** A qualified (style) rule (section 5.1.6). *)

type rule = Qualified of qualified_rule | At of at_rule

type declaration = { name : string; value : t list; important : bool }
(** A declaration extracted by section 5.3.7: name, value (with trailing
    whitespace and [!important] marker stripped), and the flag. *)
