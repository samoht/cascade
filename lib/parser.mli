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

val string_of_components : Component.t list -> string
(** [string_of_components cvs] renders a component-value list back to source
    text. Whitespace tokens serialize to a single space; the output is
    parse-equivalent but not byte-identical. *)

val to_string_minified : Component.t list -> string
(** Like {!string_of_components} but drops whitespace that sits between two
    components where at least one side is not word-like (ident / number / etc.).
    Two word-like components still get a separating space so they don't merge
    into a single token. *)

val to_string_custom : Component.t list -> string
(** Variant of {!string_of_components} for CSS Custom Properties Level 1 token
    streams. Whitespace is preserved at the top level and inside nested blocks
    and functions. *)

val fold_value_ident : string -> string
(** [fold_value_ident s] is the canonical lower-case spelling of [s] when [s]
    names a CSS-wide keyword, [currentcolor], or [transparent]; any other ident
    keeps its source case. This is the default ident fold of
    {!to_string_custom_minified}. *)

val to_string_custom_minified :
  ?fold_ident:(string -> string) -> Component.t list -> string
(** Variant of {!string_of_components} for CSS Custom Properties Level 1 token
    streams. Optional whitespace is collapsed using the rules of
    {!to_string_minified} while preserving token boundaries. [fold_ident]
    (default {!fold_value_ident}) maps each ident token to its serialized
    spelling; callers that can recognise a wider set of case-insensitive value
    keywords pass a stronger fold. *)

val escape_ident : string -> string
(** [escape_ident s] returns [s] with non-ident-continue code points backslash-
    or hex-escaped per CSS Syntax 3 section 9.1, so that
    [Cursor.of_string (escape_ident s) |> Cursor.ident] yields [s] again. *)

(** {1 Entry points (section 5.4)} *)

type 'a output = { value : 'a; warnings : Error.t list }
(** Parser entry-point result: the produced AST plus any non-fatal warnings
    collected during error recovery. The CSS spec mandates declaration- and
    rule-level skip-on-error; the dropped material surfaces here. *)

type block_item =
  [ `Decls of Component.declaration list | `Rule of Component.rule ]
(** One item returned by section 5.4.5 / 5.5.5 block-contents parsing: either a
    contiguous declaration run or a nested rule. *)

type grammar = Component.t list -> bool
(** Minimal executable hook for section 5.4.1/5.4.2 grammar matching. The
    representation of matched grammar results is intentionally unspecified by
    CSS Syntax; Cascade returns the component-value group when this predicate
    accepts it. *)

val matches_grammar : Reader.t -> grammar -> Component.t list option output
(** [matches_grammar r grammar] runs section 5.4.1: parse a list of component
    values, then return it only if [grammar] accepts it. *)

val csv_by_grammar : Reader.t -> grammar -> Component.t list option list output
(** [csv_by_grammar r grammar] runs section 5.4.2: split into top-level comma
    groups, then match each group independently. Whitespace-only input returns
    an empty list. *)

val stylesheet : ?meta:Loc.meta_level -> Reader.t -> Component.rule list output
(** [stylesheet ?meta r] runs section 5.4.3: consume a list of rules with the
    top-level flag set. CDO and CDC are skipped. [?meta] controls snippet
    attachment on recovery warnings; see {!Loc.meta_level}. *)

val stylesheet_contents :
  ?meta:Loc.meta_level -> Reader.t -> Component.rule list output
(** [stylesheet_contents ?meta r] runs section 5.4.4. *)

val block_contents : ?meta:Loc.meta_level -> Reader.t -> block_item list output
(** [block_contents ?meta r] runs section 5.4.5. *)

val rule : ?meta:Loc.meta_level -> Reader.t -> Component.rule option output
(** [rule ?meta r] runs section 5.4.6: discard surrounding whitespace, parse
    exactly one rule, and return [None] for empty input, invalid rule input, or
    trailing component values. *)

val declaration :
  ?meta:Loc.meta_level -> Reader.t -> Component.declaration option output
(** [declaration ?meta r] runs section 5.4.7: discard surrounding whitespace,
    parse exactly one declaration, and reject at-rules or declaration lists. *)

val list_of_rules :
  ?meta:Loc.meta_level -> Reader.t -> Component.rule list output
(** [list_of_rules ?meta r] runs section 5.4.4. CDO/CDC are not discarded;
    suitable for nested rule bodies. *)

val list_of_declarations :
  ?meta:Loc.meta_level ->
  Reader.t ->
  [ `Decl of Component.declaration | `At of Component.at_rule ] list output
(** [list_of_declarations ?meta r] runs section 5.4.8. *)

val component_value : Reader.t -> Component.t option output
(** [component_value r] runs section 5.4.8: discard surrounding whitespace,
    parse exactly one component value, and return [None] for empty input or
    trailing component values. *)

val list_of_component_values : Reader.t -> Component.t list output
(** [list_of_component_values r] runs section 5.4.9. *)

val csv_component_values : Reader.t -> Component.t list list output
(** [csv_component_values r] runs section 5.4.10, splitting only on top-level
    comma tokens. *)

val declaration_value : Reader.t -> Component.t list option output
(** [declaration_value r] matches CSS Syntax section 7.2's [<declaration-value>]
    production. *)

val any_value : Reader.t -> Component.t list option output
(** [any_value r] matches CSS Syntax section 7.2's [<any-value>] production. *)
