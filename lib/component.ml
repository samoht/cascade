(** Stage 3 IR: CSS Syntax 3 (ED) sec. 5.1 component values and rules. *)

type 'a node = { node : 'a; loc : Loc.t }

type t = Preserved of Token.t | Block of block node | Func of func node

and block = {
  opening : Token.bracket;
  value : t list;
  closed : bool;
      (** [false] when the lexer reached EOF before the matching closer (CSS
          Syntax 3 (ED) sec. 5.5.9 parse error). Typed validators inspect this
          to reject values that the syntax level only forgives. *)
}

and func = {
  name : string;
  arguments : t list;
  terminated : bool;
      (** [false] when the lexer reached EOF before the matching [)] (CSS Syntax
          3 (ED) sec. 5.4.6 parse error). The serializer still emits the
          synthetic [)] so reserialised output round-trips through the lexer;
          typed validators can inspect this flag to reject values that the
          syntax level only forgives. *)
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

(* A {!Loc.t} records where a node was read, not what it holds, so two nodes
   that spell the same component value are the same value however far apart the
   source wrote them. *)
let rank = function Preserved _ -> 0 | Block _ -> 1 | Func _ -> 2

let rec compare (a : t) (b : t) =
  match (a, b) with
  | Preserved t1, Preserved t2 -> Token.compare_kind t1.kind t2.kind
  | Block b1, Block b2 -> compare_block b1.node b2.node
  | Func f1, Func f2 -> compare_func f1.node f2.node
  | (Preserved _ | Block _ | Func _), _ -> Int.compare (rank a) (rank b)

and compare_block (b1 : block) (b2 : block) =
  let c = Token.compare_bracket b1.opening b2.opening in
  if c <> 0 then c
  else
    let c = Bool.compare b1.closed b2.closed in
    if c <> 0 then c else List.compare compare b1.value b2.value

and compare_func (f1 : func) (f2 : func) =
  let c = String.compare f1.name f2.name in
  if c <> 0 then c
  else
    let c = Bool.compare f1.terminated f2.terminated in
    if c <> 0 then c else List.compare compare f1.arguments f2.arguments

let equal a b = compare a b = 0

let source_loc : t -> Loc.t = function
  | Preserved tok -> tok.Token.loc
  | Block b -> b.loc
  | Func f -> f.loc

let rule_loc : rule -> Loc.t = function Qualified r -> r.loc | At r -> r.loc

let rec is_any_value components =
  List.for_all
    (function
      | Preserved { kind = Token.Bad_string | Token.Bad_url | Token.Close _; _ }
        ->
          false
      | Block { node = { value; closed; _ }; _ } -> closed && is_any_value value
      | Func { node = { arguments; terminated; _ }; _ } ->
          terminated && is_any_value arguments
      | Preserved _ -> true)
    components

let is_whitespace = function
  | Preserved { kind = Token.Whitespace; _ } -> true
  | Preserved _ | Block _ | Func _ -> false

(* A real [var()] function anywhere in the components, recursing into function
   arguments and bracketed blocks. A [var(] inside a string or a url() is an
   atomic [Preserved] token, never a [Func], so it is data, not a reference. *)
let rec has_var components =
  List.exists
    (fun (c : t) ->
      match c with
      | Func { node = { name; arguments; _ }; _ } ->
          String.lowercase_ascii name = "var" || has_var arguments
      | Block { node = { value; _ }; _ } -> has_var value
      | Preserved _ -> false)
    components

let opening_char : Token.bracket -> char = function
  | Curly -> '{'
  | Paren -> '('
  | Square -> '['

let closing_char : Token.bracket -> char = function
  | Curly -> '}'
  | Paren -> ')'
  | Square -> ']'

(* A debug dump, not source text: every node shows its own location and the CSS
   Syntax 3 (ED) sec. 5.5.9 and 5.5.10 flags [compare] separates values on.
   [Pp.space] rather than [Pp.sp] because a dump has no minified form, and
   layout whitespace would drop out and run the children together. *)
let rec pp : t Pp.t =
 fun ctx cv ->
  match cv with
  | Preserved tok -> Token.pp ctx tok
  | Block { node = { opening; value; closed }; loc } ->
      Pp.char ctx (opening_char opening);
      Pp.list ~sep:Pp.space pp ctx value;
      Pp.char ctx (closing_char opening);
      if not closed then Pp.string ctx "<unclosed>";
      Pp.char ctx '@';
      Loc.pp ctx loc
  | Func { node = { name; arguments; terminated }; loc } ->
      Pp.string ctx name;
      Pp.char ctx '(';
      Pp.list ~sep:Pp.space pp ctx arguments;
      Pp.char ctx ')';
      if not terminated then Pp.string ctx "<unterminated>";
      Pp.char ctx '@';
      Loc.pp ctx loc

let to_string t = Pp.to_string pp t
