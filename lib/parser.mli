(** Stage 3 stream: Token.t -> Component.t (CSS Syntax 3 (ED) sec. 5).

    Ports the "consume a ..." algorithms from
    https://www.w3.org/TR/css-syntax-3/#parser-algorithms onto a {!Lexer.t}
    token stream. Produces the {!Component} IR consumed by the typed-AST
    validators. *)

type t
(** A component-value stream: a {!Lexer.t} plus one-component pushback. *)

val of_lexer : Lexer.t -> t
(** [of_lexer l] wraps an existing lexer stream. *)

val of_reader : ?unicode_ranges:bool -> Reader.t -> t
(** [of_reader ?unicode_ranges r] builds a lexer from [r] and wraps it.
    [unicode_ranges] is passed to {!Lexer.of_reader}. *)

val of_string : ?unicode_ranges:bool -> string -> t
(** [of_string ?unicode_ranges s] builds a fresh reader and lexer from [s]. *)

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
    text that re-tokenizes to the same stream, as CSS Syntax 3 (ED) section 9.1
    requires. A [<whitespace-token>] is a whole run of whitespace carrying no
    text, so authored whitespace comes back as one space rather than byte for
    byte, and a space is added wherever two adjacent components would otherwise
    merge into a single token. This is the whitespace-keeping renderer for every
    stream, the opaque custom-property values of CSS Custom Properties 1
    included. *)

val to_string_minified : Component.t list -> string
(** Like {!string_of_components} but drops whitespace that sits between two
    components where at least one side is not word-like (ident / number / etc.).
    Two word-like components still get a separating space so they don't merge
    into a single token, and so does either side of a [+] or [-] operator inside
    a math function, where CSS Values 4 (ED) section 10.8 requires one. *)

val to_string_minified_numbers : Component.t list -> string
(** [to_string_minified_numbers] additionally chooses shorter exact spellings
    for numeric tokens. It is a separate operation because some opaque streams,
    such as declaration feature queries, ask another parser about the author's
    exact spelling. *)

val is_math_function : string -> bool
(** [is_math_function name] is true for a CSS Values 4 (ED) section 10 math
    function: [calc()], the comparison functions ([min()], [max()], [clamp()]),
    the stepped-value ([round()], [mod()], [rem()]), trigonometric ([sin()]
    through [atan2()]), exponential ([pow()], [sqrt()], [hypot()], [log()],
    [exp()]) and sign-related ([abs()], [sign()]) families. [name] is matched
    case insensitively, as CSS function names are. One table serves every
    caller: a second copy drifts, and the passes over a custom-property stream
    then disagree about the same token. *)

val is_plus_or_minus_delim : Component.t -> bool
(** [is_plus_or_minus_delim cv] is true for a [+] or [-] delim token. Inside a
    math function that token is the operator whose whitespace CSS Values 4 (ED)
    section 10.8 requires; a sign written as part of a number ([+2]) lexes as
    one numeric token and is not one of these. *)

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
    or hex-escaped per CSS Syntax 3 (ED) section 9.1, so that
    [Cursor.of_string (escape_ident s) |> Cursor.ident] yields [s] again. CSS
    Syntax 3 (ED) section 4.3.9 opens no ident sequence on a digit, nor on [-]
    followed by a digit, so that digit is hex-escaped even where it is an
    ident-continue code point. *)

val escape_name : string -> string
(** [escape_name s] escapes [s] as CSS Syntax 3 (ED) section 4.3.7 name code
    points, with no ident-start rule applied to the first one. It is what
    serialises a name written after a prefix the caller emits itself, such as
    the [#] of a hash or the [--] of a dashed ident; {!escape_ident} serialises
    a whole ident. *)

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

val stylesheet :
  ?meta:Loc.meta_level ->
  ?on_comment:(Lexer.comment -> unit) ->
  Reader.t ->
  Component.rule list output
(** [stylesheet ?meta ?on_comment r] runs section 5.4.3: consume a list of rules
    with the top-level flag set. CDO and CDC are skipped. [?meta] controls
    snippet attachment on recovery warnings; see {!Loc.meta_level}. [on_comment]
    is the opt-in observer documented by {!Lexer.of_reader}. *)

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
(** [list_of_declarations ?meta r] runs section 5.4.5. *)

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
(** [declaration_value r] matches CSS Syntax 3 (ED) sec. 7.2's
    [<declaration-value>] production. *)

val any_value : Reader.t -> Component.t list option output
(** [any_value r] matches CSS Syntax 3 (ED) sec. 7.2's [<any-value>] production.
*)
