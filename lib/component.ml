(** Stage 3 IR: CSS Syntax 3 (ED) sec. 5.1 component values and rules. *)

type 'a node = { node : 'a; loc : Loc.t }

type t =
  | Preserved of Token.t
  | Block of block node
  | Func of func node
  | Flat of Token.t list node

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
let rank = function
  | Preserved _ -> 0
  | Block _ -> 1
  | Func _ -> 2
  | Flat _ -> 3

let rec compare (a : t) (b : t) =
  match (a, b) with
  | Preserved t1, Preserved t2 -> Token.compare_kind t1.kind t2.kind
  | Block b1, Block b2 -> compare_block b1.node b2.node
  | Func f1, Func f2 -> compare_func f1.node f2.node
  | Flat r1, Flat r2 ->
      List.compare
        (fun (t1 : Token.t) (t2 : Token.t) ->
          Token.compare_kind t1.kind t2.kind)
        r1.node r2.node
  | (Preserved _ | Block _ | Func _ | Flat _), _ ->
      Int.compare (rank a) (rank b)

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
  | Flat r -> r.loc

let rule_loc : rule -> Loc.t = function Qualified r -> r.loc | At r -> r.loc

let opening_char : Token.bracket -> char = function
  | Curly -> '{'
  | Paren -> '('
  | Square -> '['

(* One pass over a flat run. The blocks it opens are a byte each in [open_], so
   the scan takes no stack whatever the run nests. A closer is matched only by
   the innermost open block, as sec. 5.5.9 closes one; any other is a stray
   token of the run. *)
type flat_scan = {
  bad_token : bool;
  stray_closer : bool;
  left_open : Token.bracket list;
}

let bracket_of_char = function
  | '{' -> Token.Curly
  | '[' -> Token.Square
  | _ -> Token.Paren

let scan_flat tokens =
  let open_ = Buffer.create 16 in
  let scan acc (tok : Token.t) =
    match tok.kind with
    | Token.Bad_string | Token.Bad_url -> { acc with bad_token = true }
    | Token.Open b ->
        Buffer.add_char open_ (opening_char b);
        acc
    | Token.Function _ ->
        Buffer.add_char open_ (opening_char Paren);
        acc
    | Token.Close b ->
        let depth = Buffer.length open_ in
        if depth > 0 && Buffer.nth open_ (depth - 1) = opening_char b then (
          Buffer.truncate open_ (depth - 1);
          acc)
        else { acc with stray_closer = true }
    | _ -> acc
  in
  let acc =
    List.fold_left scan
      { bad_token = false; stray_closer = false; left_open = [] }
      tokens
  in
  let left_open = ref [] in
  String.iter
    (fun c -> left_open := bracket_of_char c :: !left_open)
    (Buffer.contents open_);
  { acc with left_open = !left_open }

let flat_is_valid ~require_closed tokens =
  let s = scan_flat tokens in
  (not s.bad_token) && (not s.stray_closer)
  && ((not require_closed) || s.left_open = [])

let flat_left_open tokens = (scan_flat tokens).left_open

let flat_calls names tokens =
  List.exists
    (fun (tok : Token.t) ->
      match tok.kind with
      | Token.Function name ->
          List.exists (String.equal (String.lowercase_ascii name)) names
      | _ -> false)
    tokens

let flat_var_refs tokens =
  let _, refs =
    List.fold_left
      (fun (after_var, refs) (tok : Token.t) ->
        match tok.kind with
        | Token.Function name when String.lowercase_ascii name = "var" ->
            (true, refs)
        | Token.Ident n when after_var && Custom_property_name.is_valid n ->
            (false, n :: refs)
        | _ -> (after_var, refs))
      (false, []) tokens
  in
  List.rev refs

let shallow = function
  | [ Flat { node; _ } ] -> List.map (fun tok -> Preserved tok) node
  | components -> components

let rec is_any_value components =
  List.for_all
    (function
      | Preserved { kind = Token.Bad_string | Token.Bad_url | Token.Close _; _ }
        ->
          false
      | Block { node = { value; closed; _ }; _ } -> closed && is_any_value value
      | Func { node = { arguments; terminated; _ }; _ } ->
          terminated && is_any_value arguments
      | Flat { node; _ } -> flat_is_valid ~require_closed:true node
      | Preserved _ -> true)
    components

let is_whitespace = function
  | Preserved { kind = Token.Whitespace _; _ } -> true
  | Preserved _ | Block _ | Func _ | Flat _ -> false

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
      | Flat { node; _ } -> flat_calls [ "var" ] node
      | Preserved _ -> false)
    components

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
  | Flat { node; loc } ->
      Pp.string ctx "<flat";
      List.iter
        (fun tok ->
          Pp.space ctx ();
          Token.pp ctx tok)
        node;
      Pp.string ctx ">@";
      Loc.pp ctx loc

let to_string t = Pp.to_string pp t
