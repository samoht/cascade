(** Stream cursor over [Component.t list].

    Validators (Selector, Values, Properties, ...) consume component values
    rather than raw characters. {!Cursor.t} is the moral equivalent of
    {!Reader.t} for that input: a mutable cursor with [peek] / [next] plus a
    battery of token-shape helpers ([ident], [number], [parens], ...).

    Whitespace components are leading-trimmed by every typed helper, so pattern
    matching stays simple at call sites. *)

type t

val of_components :
  ?source:string ->
  ?recover:bool ->
  ?meta:Loc.meta_level ->
  ?eof_loc:Loc.t ->
  Component.t list ->
  t
(** [of_components ?source ?recover ?meta ?eof_loc cvs] is a fresh cursor over
    [cvs]. Pass [?source] so errors raised while consuming the cursor get a
    source-context snippet attached (matching {!of_string}'s behaviour). Pass
    [~recover:true] to enable per-declaration recovery: validators that honour
    {!recover} will catch a {!exception-Parse_error} on one declaration, push it
    to {!push_warning}, and skip to the next [;] instead of propagating. [?meta]
    controls snippet construction and defaults to {!Loc.default_meta_level}.
    [?eof_loc] anchors end-of-input errors at a specific location. *)

val sub : ?eof_loc:Loc.t -> t -> Component.t list -> t
(** [sub ?eof_loc parent cvs] is a fresh cursor over [cvs] that inherits
    [parent]'s source, warnings list, recovery mode, and {!meta} level. Pass
    [?eof_loc] to anchor end-of-input errors at a specific location (e.g. the
    closing delimiter of a block); defaults to [parent]'s own [eof_loc]. *)

val func_sub : Component.func Component.node -> t -> t
(** [func_sub fn parent] is a sub-cursor over [fn]'s arguments, anchored so an
    EOF error inside the function body points at the function's closing [')']
    rather than the end of the outer input. *)

val recover : t -> bool
(** [recover t] is the recovery mode [t] was built with. Validators that support
    declaration-level recovery check this to decide whether to catch and skip on
    a {!exception-Parse_error} or let it propagate. *)

val meta : t -> Loc.meta_level
(** [meta t] is the metadata level [t] was built with. At [`Full] errors carry
    source-context snippets; lower levels skip snippet construction. *)

val source : t -> string option
(** [source t] is the preprocessed source text that produced [t], when known. *)

val push_warning : t -> recovery:Error.Recovery.t -> Error.t -> unit
(** [push_warning t ~recovery e] records [e] as a non-fatal warning on [t],
    stamped with what the recovery did to the construct [e] is about. A
    validator in recovery mode catches a {!exception-Parse_error}, pushes it
    here, skips to a recovery point, and keeps going. Drained via
    {!drain_warnings}. *)

val drain_warnings : t -> Error.t list
(** [drain_warnings t] returns and clears the warnings accumulated on [t] in
    source order. *)

val of_string : ?meta:Loc.meta_level -> ?unicode_ranges:bool -> string -> t
(** [of_string ?meta ?unicode_ranges s] lexes [s] into a {!Component.t} list and
    wraps it. The trailing [Eof] token is dropped. [unicode_ranges] is passed to
    {!Lexer.of_reader}, and only the value of a [unicode-range] descriptor sets
    it. *)

val of_reader : ?meta:Loc.meta_level -> Reader.t -> t
(** [of_reader ?meta r] consumes the rest of [r]'s input as a component stream
    and wraps it. *)

(** {1 Stream API} *)

val peek : t -> Component.t option
(** [peek t] is the next component, or [None] at end of input. Skips any leading
    whitespace components. *)

val next : t -> Component.t option
(** [next t] consumes and returns the next component (or [None] at end of
    input). Skips any leading whitespace components. *)

val skip : t -> unit
(** [skip t] consumes the next component and discards it. *)

val is_done : t -> bool
(** [is_done t] is true when only whitespace (or nothing) remains. *)

val position : t -> Loc.t
(** [position t] is the {!Loc.t} of the next component, or {!Loc.dummy} if
    {!val-is_done}. *)

val remaining : t -> Component.t list
(** [remaining t] is the un-consumed tail (still includes whitespace). *)

val string_of_components : ?trim:bool -> Component.t list -> string
(** [string_of_components ?trim cvs] serializes a component-value list. This is
    useful for at-rule preludes that must be split structurally before being
    preserved as raw CSS text. *)

val string_of_remaining : ?trim:bool -> t -> string
(** [string_of_remaining t] serializes the unconsumed tail without advancing
    [t]. *)

val consume_remaining : t -> Component.t list
(** [consume_remaining t] is {!val-remaining}, and also consumes the tail. *)

val consume_remaining_as_string : ?trim:bool -> t -> string
(** [consume_remaining_as_string t] serializes and consumes the unconsumed tail.
*)

val ws : t -> unit
(** [ws t] drops any leading whitespace components. Usually a no-op since typed
    helpers skip whitespace for you; useful when a raw {!val-peek_raw} or
    {!val-next_raw} is about to run. *)

(** {1 Whitespace-aware variants}

    [peek] / [next] above transparently skip whitespace components. For grammars
    where whitespace is significant (CSS selectors -- the descendant combinator
    {e is} whitespace), use these variants. *)

val peek_raw : t -> Component.t option
(** [peek_raw t] is the next component without skipping whitespace. *)

type head_shape =
  [ `Eof
  | `Semicolon
  | `Colon
  | `Comma
  | `Bang
  | `Curly_block
  | `Paren_block
  | `Square_block
  | `Ident
  | `Func
  | `Other ]
(** Head-shape classification of the next non-whitespace component, returned as
    a polymorphic variant constant so it carries no allocation. *)

val peek_head_shape : t -> head_shape
(** [peek_head_shape t] classifies the next non-whitespace component, without
    boxing the component in [Some _]. *)

val next_raw : t -> Component.t option
(** [next_raw t] consumes the next component without skipping whitespace. *)

val skip_ws : t -> bool
(** [skip_ws t] consumes any leading whitespace and returns whether at least one
    whitespace component was skipped. *)

(** {1 Snapshot / restore}

    Some grammars (notably CSS selectors, which must distinguish [prefix|name]
    from [prefix|=value]) need lookahead beyond a single component. Save the
    cursor with {!save}, do speculative consumption, and {!restore} if it didn't
    pan out. *)

type snapshot

val save : t -> snapshot
(** [save t] captures the current cursor position. *)

val restore : t -> snapshot -> unit
(** [restore t s] rewinds [t] to the position captured by [s]. *)

val dropped_since :
  t -> snapshot -> Error.Recovery.construct -> Error.Recovery.t
(** [dropped_since t s construct] is a dropped-[construct] recovery naming the
    source text from the first component [t] consumed since [s] to the last,
    whitespace at either end left out. A recovery point takes [s] before the
    item it gives up on and calls this once it has skipped that item, so the
    recovery names the whole construct the reader threw away rather than the
    point the read failed at. It names no text when [t] carries no source, when
    nothing but whitespace was consumed, or when [s] is no snapshot [t] advanced
    from. *)

val atomic : t -> (unit -> 'a) -> 'a
(** [atomic t f] runs [f ()] with a snapshot held; if [f] raises {!Parse_error},
    the cursor is restored before the exception propagates. *)

val lookahead : (t -> 'a) -> t -> 'a
(** [lookahead p t] runs [p t] and then restores the cursor, returning the
    result. *)

val try_typed_call : (t -> 'a) -> t -> ('a, Component.t) result
(** [try_typed_call typed t] runs the typed reader [typed] when the next
    component is a [Func]. On success, returns [Ok value]. On
    {!exception-Parse_error}, restores the cursor, skips past the function call,
    and returns [Error <captured-call>] so callers can wrap it in their type's
    [Invalid] arm. When the next component isn't a [Func] the typed reader is
    run directly (errors propagate). *)

(** {1 Errors} *)

exception Parse_error of Error.t
(** Alias for {!Error.Parse_error}. *)

val err : ?loc:Loc.t -> ?got:string -> t -> string -> 'a
(** [err ?loc ?got t msg] raises {!Parse_error} at the current position, or at
    [loc] when given. A reader that consumes the offending token before
    rejecting it passes the span it read: {!val-position} has moved on to
    whatever follows by then, which at the end of a value is the terminator. *)

val err_invalid : ?loc:Loc.t -> t -> string -> 'a
(** [err_invalid t msg] is [err t ("invalid: " ^ msg)]. *)

val err_eof : t -> 'a
(** [err_eof t] raises an "unexpected end of input" error. *)

val err_expected : ?loc:Loc.t -> t -> string -> 'a
(** [err_expected t what] raises with "expected [what]". *)

val err_expected_but_eof : t -> string -> 'a
(** [err_expected_but_eof t what] raises when end of input is reached while
    expecting [what]. *)

val err_unexpected : t -> 'a
(** [err_unexpected t] raises "unexpected token" at the current position. *)

val condition_error : t -> at_rule:string -> string -> Error.t
(** [condition_error t ~at_rule reason] is the {!Error.Bad_condition} for
    [at_rule] anchored at the current position, with [t]'s source attached. A
    reader that buffers a failure before deciding whether to recover from it
    builds the error here and raises it later. *)

val err_condition : t -> at_rule:string -> string -> 'a
(** [err_condition t ~at_rule reason] raises {!Parse_error} carrying
    {!val-condition_error}. An at-rule prelude reader raises through this so the
    caret lands on the slice of the condition that failed rather than on the
    enclosing rule. *)

val with_context : t -> string -> (unit -> 'a) -> 'a
(** [with_context t label f] annotates any {!Parse_error} raised by [f] with
    [label] in the error path. *)

(** {1 Token-shape helpers - raising variants}

    These parse and advance the cursor, or raise {!Parse_error} on mismatch. *)

val ident : ?keep_case:bool -> t -> string
(** [ident t] consumes the next ident. [keep_case] defaults to [true], the
    author's spelling, which is what an author-defined identifier is (CSS Values
    4 sec. 4.2). [~keep_case:false] lowercases it, which is what a keyword is
    (sec. 4.1). *)

val number : ?allow_negative:bool -> t -> float
(** [number t] consumes the next numeric token. *)

val int : t -> int
(** [int t] consumes the next integer token. *)

val hex : t -> int
(** [hex t] consumes a hash token and parses it as hex. *)

val string : ?trim:bool -> t -> string
(** [string t] consumes a string literal. *)

val url : t -> string
(** [url t] consumes the body of a [url(...)] call (either a [<url-token>] or a
    function call with a quoted argument). *)

val pct : ?clamp:bool -> t -> float
(** [pct t] consumes a percentage token. If [~clamp] is set, the value is
    clamped to [0..100]. *)

val number_with_unit : t -> float * string option
(** [number_with_unit t] consumes a dimension, percentage or number and returns
    the value and unit (if any). *)

val number_repr_with_unit : t -> float * string * string option
(** [number_repr_with_unit t] is like {!number_with_unit}, but also returns the
    authored numeric token representation. *)

val bool : t -> bool
(** [bool t] consumes [true] or [false]. *)

(** {1 Token-shape helpers - option variants}

    Each [foo_opt] returns [Some _] and advances when the next component
    matches; otherwise returns [None] and the cursor is unchanged. *)

val ident_opt : t -> string option
(** [ident_opt t] consumes and returns an identifier if present. *)

val number_opt : t -> float option
(** [number_opt t] consumes and returns a number token if present. *)

val integer_opt : t -> int option
(** [integer_opt t] consumes and returns an integer-valued number if present. *)

val percentage_opt : t -> float option
(** [percentage_opt t] consumes and returns a percentage value if present. *)

val dimension_opt : t -> (float * string) option
(** [dimension_opt t] consumes and returns a dimension value and unit if
    present. *)

val hash_opt : t -> string option
(** [hash_opt t] consumes and returns a hash token value if present. *)

val string_opt : t -> string option
(** [string_opt t] consumes and returns a string token if present. *)

val string_with_quote_opt : t -> (string * char) option
(** [string_with_quote_opt t] consumes and returns a string token plus its quote
    character if present. *)

val string_repr_with_quote_opt : t -> (string * char * string option) option
(** [string_repr_with_quote_opt t] is like {!string_with_quote_opt}, but also
    returns the authored string token spelling when source text is available. *)

val url_opt : t -> string option
(** [url_opt t] consumes and returns a URL token if present. *)

val delim_opt : t -> char option
(** [delim_opt t] consumes and returns a delimiter token if present. *)

val peek_delim : t -> char option
(** [peek_delim t] is the char of the next component if it is a [Delim] token,
    without advancing the cursor. *)

val peek_comma : t -> bool
(** [peek_comma t] is [true] iff the next component is a comma token. *)

val peek_semicolon : t -> bool
(** [peek_semicolon t] is [true] iff the next component is a semicolon token. *)

val peek_colon : t -> bool
(** [peek_colon t] is [true] iff the next component is a colon token. *)

val peek_ident : t -> string option
(** [peek_ident t] is [Some s] when the next component is [Ident s]. *)

val peek_hash : t -> string option
(** [peek_hash t] is [Some s] when the next component is [Hash s]. *)

val peek_at_keyword : t -> string option
(** [peek_at_keyword t] is [Some s] when the next component is [At_keyword s].
*)

val peek_block : t -> Token.bracket option
(** [peek_block t] is [Some bracket] when the next component is a balanced
    block, returning its opening bracket kind. *)

val at_keyword_opt : t -> string option
(** [at_keyword_opt t] consumes the next component if it is an [At_keyword]
    token, returning the keyword name without the leading [@]. *)

val expect_at_keyword : string -> t -> unit
(** [expect_at_keyword name t] consumes the [@name] token or raises. The name is
    a keyword, matched ASCII case-insensitively (CSS Values 4 sec. 4.1), so
    [name] is given lowercase. *)

val drain_until_block : t -> Component.t list
(** [drain_until_block t] consumes components up to (but not including) the next
    block or semicolon, returning the drained components. Used for at-rule
    preludes. *)

val drain_until_block_as_string : ?trim:bool -> t -> string
(** Like {!drain_until_block}, but serializes the drained components. *)

val consume_until_semicolon : ?trim:bool -> t -> string
(** [consume_until_semicolon t] consumes and serializes components up to, but
    not including, the next semicolon. *)

val skip_past_semicolon : t -> unit
(** [skip_past_semicolon t] consumes and discards components up to and including
    the next top-level semicolon, or up to end of input if none is left. A [{}]
    met on the way is one component value, not a stopping point, so this is the
    recovery step CSS Syntax 3 (ED) sec. 5.5.5 prescribes for a declaration that
    fails to parse. *)

val consume_to_decl_end : ?trim:bool -> t -> string
(** [consume_to_decl_end t] consumes and serializes components up to, but not
    including, the next semicolon or top-level [!] delimiter. *)

val drain_to_decl_end : t -> Component.t list
(** [drain_to_decl_end t] consumes components up to (but not including) the next
    semicolon or top-level [!] delimiter, returning the drained list without
    serialising it. *)

val decl_value_loc : t -> Loc.t
(** [decl_value_loc t] is the span the declaration value ahead covers, without
    consuming it, or {!val-position} when no value is left. A shorthand reader
    that takes the whole value before deciding it is bad reports against this,
    since the failure is the value's rather than that of the token after it. *)

val declaration_value : t -> t
(** [declaration_value t] consumes a declaration's value off [t] and is a cursor
    over it alone.

    CSS Syntax 3 (ED) sec. 5.5.6 reads the value with [<semicolon-token>] as the
    stop token, then lifts a trailing [!] [important] pair out of it into the
    declaration's important flag. A property grammar is handed what is left,
    which holds neither, so a reader whose grammar has an optional trailing
    component asks {!is_done} and nothing more: the [;] and the [!] stay on [t]
    for the declaration consumer to finish. End-of-input errors on the value
    anchor at whichever of the two stopped it. *)

val consume_to_slash_or_semicolon : ?trim:bool -> t -> string
(** [consume_to_slash_or_semicolon t] consumes and serializes components up to,
    but not including, the next top-level slash delimiter or semicolon. *)

val colon : t -> bool
(** [colon t] consumes a [':'] if next; [true] iff consumed. *)

val semicolon : t -> bool
(** [semicolon t] consumes a [';'] if next; [true] iff consumed. *)

val comma : t -> unit
(** [comma t] consumes a [','] or raises. *)

val comma_opt : t -> bool
(** [comma_opt t] consumes a [','] if next; [true] iff consumed. *)

val slash : t -> unit
(** [slash t] consumes a [/] delim or raises. *)

val slash_opt : t -> bool
(** [slash_opt t] consumes a [/] delim if present and returns whether it was
    consumed. *)

val consume_if : char -> t -> bool
(** [consume_if c t] consumes the next component if it is the delim [c]. *)

val try_kind : Token.kind -> t -> bool
(** [try_kind kind t] consumes [kind] if it is next and returns whether it
    matched. *)

val try_ident : string -> t -> bool
(** [try_ident name t] consumes the next component if it is the identifier
    [name] and returns whether it matched. The ident is a keyword, matched ASCII
    case-insensitively (CSS Values 4 sec. 4.1), so [name] is given lowercase. *)

val try_kind_pair : Token.kind -> Token.kind -> t -> bool
(** [try_kind_pair k1 k2 t] consumes [k1] followed by [k2] if both are next and
    returns whether they matched. *)

val looking_at : t -> string -> bool
(** [looking_at t s] is [true] iff the next component (after leading whitespace)
    starts with [s] - matches an ident, a function name followed by [(], or a
    delim-based prefix. Idents and at-keywords match ASCII case-insensitively
    (CSS Values 4 sec. 4.1), so [s] is given lowercase. *)

val looking_at_ident : string -> t -> bool
(** [looking_at_ident name t] is [true] if the next component is identifier
    [name], compared ASCII case-insensitively (CSS Values 4 sec. 4.1), so [name]
    is given lowercase. *)

val looking_at_func : string -> t -> bool
(** [looking_at_func name t] is [true] if the next component is function [name],
    compared ASCII case-insensitively (CSS Values 4 sec. 4.1), so [name] is
    given lowercase. *)

val looking_at_calc : t -> bool
(** [looking_at_calc t] is [true] if the next component is [calc()] or the
    legacy [-webkit-calc()] spelling. *)

(** {1 Expectations} *)

val expect : char -> t -> unit
(** [expect c t] consumes the delim [c] or raises. *)

val expect_string : string -> t -> unit
(** [expect_string s t] consumes the ident [s] or raises. The ident is a
    keyword, matched ASCII case-insensitively (CSS Values 4 sec. 4.1), so [s] is
    given lowercase. *)

val expect_eof : t -> unit
(** [expect_eof t] raises if any non-whitespace component remains. *)

(** {1 Group / function helpers} *)

val parens : (t -> 'a) -> t -> 'a
(** [parens f t] consumes a [(...)] block and calls [f] with a fresh cursor over
    its contents. Raises if the next component is not a parenthesised block, or
    if [f] leaves any of the contents unconsumed: trailing content makes the
    value invalid, not truncated (CSS Syntax 3 (ED) sec. 5.4.1). *)

val brackets : (t -> 'a) -> t -> 'a
(** [brackets t f] consumes a [[...]] block similarly. *)

val braces : (t -> 'a) -> t -> 'a
(** [braces t f] consumes a [{...}] block similarly. *)

val call : string -> t -> (t -> 'a) -> 'a
(** [call name t f] consumes a [name(...)] function call and applies [f] to a
    cursor over its arguments. Raises if no such function is next, or if [f]
    leaves any of the arguments unconsumed: trailing content makes the value
    invalid, not truncated (CSS Syntax 3 (ED) sec. 5.4.1). *)

val function_call : string -> (t -> 'a) -> t -> 'a option
(** [function_call name f t] consumes a [name(...)] call and calls [f] over its
    arguments, raising if [f] leaves any of them unconsumed, as {!call} does.
    Returns [None] without advancing if the next component is not a function
    with that name, compared ASCII case-insensitively (CSS Values 4 sec. 4.1),
    so [name] is given lowercase. *)

val any_function_call : (string -> t -> 'a) -> t -> 'a option
(** [any_function_call f t] consumes any function call and applies [f] to its
    name and argument cursor, raising if [f] leaves any argument unconsumed, as
    {!call} does. *)

(** {1 Enums} *)

val enum : ?default:(t -> 'a) -> string -> (string * 'a) list -> t -> 'a
(** [enum ?default label table t] skips leading whitespace, consumes an ident,
    and looks it up in [table]. Falls back to [default] if provided, otherwise
    raises. *)

val try_enum : (string * 'a) list -> t -> 'a option
(** [try_enum table t] is like {!enum} but returns [None] without raising. Keys
    are given lowercase. *)

val enum_calls : ?default:(t -> 'a) -> (string * (t -> 'a)) list -> t -> 'a
(** [enum_calls ?default table t] skips leading whitespace, then dispatches on
    the name of the next function call. Each parser receives the raw cursor
    (still pointing at the function) and is expected to consume it. *)

val enum_or_calls :
  ?default:(t -> 'a) ->
  string ->
  (string * 'a) list ->
  ?calls:(string * (t -> 'a)) list ->
  t ->
  'a
(** [enum_or_calls ?default label idents ?calls t] skips leading whitespace,
    first tries to match [idents] (ident token), then [calls] (function call),
    and finally falls back to [default]. Raises if none apply and no default is
    given. *)

val enum_or_var :
  ?default:(t -> 'a) -> string -> (string * 'a) list -> var:(t -> 'a) -> t -> 'a
(** [enum_or_var label idents ~var t] is {!val-enum_or_calls} specialised to the
    common case of "match a CSS keyword from [idents], or read [var(...)] via
    [var]". Removes the boilerplate of writing the [var] entry in a [calls] list
    at every typed-property reader. [var] reads whatever the caller is reading,
    which for a reader called once per shorthand slot is that slot. *)

val enum_or_whole_value_var :
  ?default:(t -> 'a) -> string -> (string * 'a) list -> var:(t -> 'a) -> t -> 'a
(** [enum_or_whole_value_var] is {!val-enum_or_var} for a reader that owns the
    whole declaration value, where [var] stands for all of it. A [var()] with
    components after it is an operand of [default]'s grammar rather than the
    value, so such a value goes to [default]. *)

(** {1 Higher-order combinators} *)

val option : (t -> 'a) -> t -> 'a option
(** [option p t] returns [Some (p t)] on success, [None] if [p] raises
    {!Parse_error} (cursor is rewound). *)

val one_of : (t -> 'a) list -> t -> 'a
(** [one_of ps t] tries each parser in order, rewinding on failure. *)

val list :
  ?sep:(t -> unit) -> ?at_least:int -> ?at_most:int -> (t -> 'a) -> t -> 'a list
(** [list ?sep ?at_least ?at_most item t] parses items separated by [sep]
    (default: no separator). Lists require at least one item unless [at_least]
    is specified explicitly. A [sep] only commits once another [item] parses
    after it, so a trailing separator with nothing following it is left
    unconsumed for the caller rather than silently dropped. Enforces cardinality
    bounds. Too few items raise ["expected at least N items (got M)"], the
    wording {!Reader.list} uses. *)

val fold_many :
  (t -> 'a) -> init:'s -> f:('s -> 'a -> 's) -> t -> 's * string option
(** Like {!many} but folds into an accumulator. Returns the final accumulator
    and the last error (if any). *)

val many : (t -> 'a) -> t -> 'a list * string option
(** [many p t] runs [p] repeatedly while it succeeds; returns the list of
    results and the last error message on failure (or [None] if the stream
    simply ran out). *)

val pair : ?sep:(t -> unit) -> (t -> 'a) -> (t -> 'b) -> t -> 'a * 'b
(** [pair ?sep a b t] parses [a] followed by [b], optionally separated by [sep].
    Atomic: a failure in [sep] or [b] rewinds the cursor to where [a] started,
    so the caller can try another alternative. *)

val triple :
  ?sep:(t -> unit) -> (t -> 'a) -> (t -> 'b) -> (t -> 'c) -> t -> 'a * 'b * 'c
(** [triple ?sep a b c t] parses [a], [b], and [c], optionally separated by
    [sep]. Atomic like {!val-pair}. *)

val try_parse_err : (t -> 'a) -> t -> ('a, string) result
(** [try_parse_err p t] returns [Ok v] on success or [Error msg] on parse
    failure (cursor rewound). *)

val try_parse_full_err : (t -> 'a) -> t -> ('a, string) result
(** [try_parse_full_err p t] is like {!try_parse_err}, but also requires [p] to
    consume the cursor fully. *)
