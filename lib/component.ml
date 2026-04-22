(** Stage 3 IR: CSS Syntax section 5.1 component values and rules. *)

type 'a node = { node : 'a; loc : Loc.t }

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
  | Block { node = { opening; value }; _ } ->
      Pp.char ctx (opening_char opening);
      Pp.list ~sep:Pp.sp pp ctx value;
      Pp.char ctx (closing_char opening)
  | Func { node = { name; arguments }; _ } ->
      Pp.string ctx name;
      Pp.char ctx '(';
      Pp.list ~sep:Pp.sp pp ctx arguments;
      Pp.char ctx ')'

let to_string t = Pp.to_string pp t
