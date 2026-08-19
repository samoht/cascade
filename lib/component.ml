(** Stage 3 IR: CSS Syntax section 5.1 component values and rules. *)

type 'a node = { node : 'a; loc : Loc.t }

type t = Preserved of Token.t | Block of block node | Func of func node

and block = {
  opening : Token.bracket;
  value : t list;
  closed : bool;
      (** [false] when the lexer reached EOF before the matching closer (CSS
          Syntax section 5.4.6 parse error). Typed validators inspect this to
          reject values that the syntax level only forgives. *)
}

and func = {
  name : string;
  arguments : t list;
  terminated : bool;
      (** [false] when the lexer reached EOF before the matching [)] (CSS Syntax
          section 5.4.6 parse error). The serializer still emits the synthetic
          [)] so reserialised output round-trips through the lexer; typed
          validators can inspect this flag to reject values that the syntax
          level only forgives. *)
}

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

let equal (a : t) b = a = b

let source_loc : t -> Loc.t = function
  | Preserved tok -> tok.Token.loc
  | Block b -> b.loc
  | Func f -> f.loc

let rule_loc : rule -> Loc.t = function Qualified r -> r.loc | At r -> r.loc

let opening_char : Token.bracket -> char = function
  | Curly -> '{'
  | Paren -> '('
  | Square -> '['

let closing_char : Token.bracket -> char = function
  | Curly -> '}'
  | Paren -> ')'
  | Square -> ']'

let rec pp : t Pp.t =
 fun ctx cv ->
  match cv with
  | Preserved tok -> Token.pp ctx tok
  | Block { node = { opening; value; _ }; _ } ->
      Pp.char ctx (opening_char opening);
      Pp.list ~sep:Pp.sp pp ctx value;
      Pp.char ctx (closing_char opening)
  | Func { node = { name; arguments; _ }; _ } ->
      Pp.string ctx name;
      Pp.char ctx '(';
      Pp.list ~sep:Pp.sp pp ctx arguments;
      Pp.char ctx ')'

let to_string t = Pp.to_string pp t
