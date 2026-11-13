(** Stage 3 IR: CSS Syntax 3 (ED) sec. 5.1 component values and rules.

    Stage 3 of the pipeline (chars -> lexer stream -> token stream -> AST): the
    output of {!Parser}, consumed by the typed-AST validators.

    Every node pairs its payload with the {!Loc.t} spanning the source text it
    was parsed from. The [_body] records hold the structural fields; the
    corresponding types ending in [node] are the located wrappers. *)

type 'a node = { node : 'a; loc : Loc.t }
(** A payload paired with the source range it occupies. Convention mirrors the
    ocaml-encodings skill's ['a node = 'a * Loc.Meta.t]: every IR constructor is
    a located payload. *)

(** A component value of CSS Syntax 3 (ED) sec. 5.1: a preserved token, a simple
    block or a function, or the {!Flat} run standing for the contents of one
    nested past the depth the parser builds. *)
type t =
  | Preserved of Token.t
  | Block of block node
  | Func of func node
  | Flat of Token.t list node
      (** The contents of a block or function opened more than
          {!Parser.max_nesting_depth} deep, as the run of tokens the lexer read
          rather than as a tree, so that no walk over a component recurses
          deeper than the limit. A block or function nested inside is its
          opening token, its contents and the closer matching it; a closer
          matching no opener in the run stays a token, as CSS Syntax 3 (ED) sec.
          5.5.9 keeps it in a block. A [Flat] is only ever the whole of a
          {!block}'s value or a {!func}'s arguments, and prints back as its
          tokens. *)

and block = {
  opening : Token.bracket;
  value : t list;
  closed : bool;
      (** [false] when the lexer reached EOF before the matching closer (CSS
          Syntax 3 (ED) sec. 5.5.9 parse error). The serializer still emits the
          synthetic closer so reserialised output round-trips. *)
}

and func = {
  name : string;
  arguments : t list;
  terminated : bool;
      (** [false] when the lexer reached EOF before the matching [)] (CSS Syntax
          3 (ED) sec. 5.5.10 parse error). The serializer still emits the
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

val equal : t -> t -> bool
(** [equal a b] tests component values for structural equality. Source locations
    are provenance rather than value, so they are not compared. *)

val compare : t -> t -> int
(** [compare a b] totally orders component values, ignoring source locations
    just as {!equal} does. *)

val source_loc : t -> Loc.t
(** [source_loc cv] is the source range spanned by [cv]. *)

val rule_loc : rule -> Loc.t
(** [rule_loc r] is the source range spanned by rule [r]. *)

val is_whitespace : t -> bool
(** [is_whitespace cv] is [true] for a preserved whitespace token. *)

val is_any_value : t list -> bool
(** [is_any_value cvs] checks the optional [<any-value>] of a general-enclosed
    condition: no bad string, bad URL, unmatched closer, or unclosed group,
    recursively. An empty sequence is allowed. *)

val flat_is_valid : require_closed:bool -> Token.t list -> bool
(** [flat_is_valid ~require_closed tokens] is [true] when the {!Flat} run
    [tokens] holds no bad string, no bad URL and no closer matching no block the
    run opened, and, with [require_closed], leaves no block open: the checks
    {!is_any_value} makes of a tree, made without recursing. *)

val flat_left_open : Token.t list -> Token.bracket list
(** [flat_left_open tokens] are the blocks and functions the {!Flat} run
    [tokens] opens and leaves open at its end, innermost first, a function as
    {!Token.Paren}: what the input ended inside. *)

val flat_calls : string list -> Token.t list -> bool
(** [flat_calls names tokens] is [true] when the {!Flat} run [tokens] calls a
    function named in [names], which are lowercase: the names are matched ASCII
    case-insensitively, at any depth of the run. *)

val flat_var_refs : Token.t list -> string list
(** [flat_var_refs tokens] are the custom properties the {!Flat} run [tokens]
    references, in order: for each [var(] in it, the first custom-property name
    after it. *)

val shallow : t list -> t list
(** [shallow cvs] is [cvs], unless [cvs] is a {!Flat} run alone, the arguments
    of a function opened past {!Parser.max_nesting_depth}: then it is the run's
    tokens, each a {!Preserved}, so a reader of the first few arguments sees the
    tokens it would see in a tree. A block the run opens is its opening token
    there, not a {!Block}. *)

val has_var : t list -> bool
(** [has_var cvs] is [true] when a [var()] function appears anywhere in [cvs],
    including inside function arguments and bracketed blocks. A [var(] written
    inside a string or a [url()] is one atomic preserved token rather than a
    function, so it is data and does not count. *)

val pp : t Pp.t
(** [pp] renders a component value as a located debug dump, the {!Token.pp} of a
    whole tree, e.g. [rgb(<number 1>@[4-5])@[0-6]]. Every node shows its own
    {!Loc.t}, and one the lexer closed synthetically at EOF is tagged
    [<unclosed>] or [<unterminated>]. Source text comes from
    {!Parser.string_of_components} and its whitespace-policy variants. *)

val to_string : t -> string
(** [to_string t] is the string rendering of {!pp}. *)
