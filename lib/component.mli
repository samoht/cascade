(** Stage 3 IR: CSS Syntax section 5.1 component values and rules.

    Stage 3 of the pipeline (chars -> lexer stream -> token stream -> AST): the
    output of {!Parser}, consumed by the typed-AST validators.

    Every node pairs its payload with the {!Loc.t} spanning the source text it
    was parsed from. The [_body] records hold the structural fields; the
    corresponding types ending in [node] are the located wrappers. *)

type 'a node = { node : 'a; loc : Loc.t }
(** A payload paired with the source range it occupies. Convention mirrors the
    ocaml-encodings skill's ['a node = 'a * Loc.Meta.t]: every IR constructor is
    a located payload. *)

type t = Preserved of Token.t | Block of block node | Func of func node
and block = { opening : Token.bracket; value : t list }
and func = { name : string; arguments : t list }

type at_rule_body = {
  name : string;
  prelude : t list;
  block : block node option;
}

type at_rule = at_rule_body node
type qualified_rule_body = { prelude : t list; block : block node }
type qualified_rule = qualified_rule_body node
type rule = Qualified of qualified_rule | At of at_rule
type declaration_body = { name : string; value : t list; important : bool }
type declaration = declaration_body node

val source_loc : t -> Loc.t
(** [source_loc cv] is the source range spanned by [cv]. *)

val rule_loc : rule -> Loc.t
(** [rule_loc r] is the source range spanned by rule [r]. *)

val pp : t Pp.t
(** [pp] renders a component value back to source-like text. *)

val to_string : t -> string
(** [to_string t] is the string rendering of {!pp}. *)
