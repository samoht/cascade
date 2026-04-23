(** Stream cursor over [Component.t list].

    Validators (Selector, Values, Properties, ...) consume component values
    rather than raw characters. {!Cursor.t} is the moral equivalent of
    {!Reader.t} for that input: a mutable cursor with [peek] / [next] plus a
    battery of token-shape helpers ([ident], [number], [parens], ...).

    Whitespace components are leading-trimmed by every typed helper, so pattern
    matching stays simple at call sites. *)

type t

val of_components : Component.t list -> t
(** [of_components cvs] is a fresh cursor positioned at the head of [cvs]. *)

val of_string : string -> t
(** [of_string s] lexes [s] into a {!Component.t} list and wraps it. The
    trailing [Eof] token is dropped. *)

val of_reader : Reader.t -> t
(** [of_reader r] consumes the rest of [r]'s input as a component stream and
    wraps it. *)

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
    [is_done]. *)

val remaining : t -> Component.t list
(** [remaining t] is the un-consumed tail (still includes whitespace). *)

val ws : t -> unit
(** [ws t] drops any leading whitespace components. Usually a no-op since typed
    helpers skip whitespace for you; useful when a raw [peek_raw] or [next_raw]
    is about to run. *)

(** {1 Whitespace-aware variants}

    [peek] / [next] above transparently skip whitespace components. For grammars
    where whitespace is significant (CSS selectors -- the descendant combinator
    {e is} whitespace), use these variants. *)

val peek_raw : t -> Component.t option
(** [peek_raw t] is the next component without skipping whitespace. *)

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

val atomic : t -> (unit -> 'a) -> 'a
(** [atomic t f] runs [f ()] with a snapshot held; if [f] raises {!Parse_error},
    the cursor is restored before the exception propagates. *)

val lookahead : (t -> 'a) -> t -> 'a
(** [lookahead p t] runs [p t] and then restores the cursor, returning the
    result. *)

(** {1 Errors} *)

exception Parse_error of Error.t
(** Alias for {!Error.Parse_error}. *)

val err : ?got:string -> t -> string -> 'a
(** [err ?got t msg] raises {!Parse_error} at the current position. *)

val err_invalid : t -> string -> 'a
(** [err_invalid t msg] is [err t ("invalid: " ^ msg)]. *)

val err_eof : t -> 'a
(** [err_eof t] raises an "unexpected end of input" error. *)

val err_expected : t -> string -> 'a
(** [err_expected t what] raises with "expected [what]". *)

val err_expected_but_eof : t -> string -> 'a
(** [err_expected_but_eof t what] raises when end of input is reached while
    expecting [what]. *)

val err_unexpected : t -> 'a
(** [err_unexpected t] raises "unexpected token" at the current position. *)

val with_context : t -> string -> (unit -> 'a) -> 'a
(** [with_context t label f] annotates any {!Parse_error} raised by [f] with
    [label] in the error path. *)

(** {1 Token-shape helpers — raising variants}

    These parse and advance the cursor, or raise {!Parse_error} on mismatch. *)

val ident : ?keep_case:bool -> t -> string
(** [ident t] consumes the next ident. The [keep_case] flag is accepted for
    source compatibility; component idents are already case-preserved. *)

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

(** {1 Token-shape helpers — option variants}

    Each [foo_opt] returns [Some _] and advances when the next component
    matches; otherwise returns [None] and the cursor is unchanged. *)

val ident_opt : t -> string option
val number_opt : t -> float option
val integer_opt : t -> int option
val percentage_opt : t -> float option
val dimension_opt : t -> (float * string) option
val hash_opt : t -> string option
val string_opt : t -> string option
val url_opt : t -> string option
val delim_opt : t -> char option

val peek_delim : t -> char option
(** [peek_delim t] is the char of the next component if it is a [Delim] token,
    without advancing the cursor. *)

val peek_comma : t -> bool
(** [peek_comma t] is [true] iff the next component is a comma token. *)

val peek_semicolon : t -> bool
(** [peek_semicolon t] is [true] iff the next component is a semicolon token. *)

val at_keyword_opt : t -> string option
(** [at_keyword_opt t] consumes the next component if it is an [At_keyword]
    token, returning the keyword name without the leading [@]. *)

val expect_at_keyword : string -> t -> unit
(** [expect_at_keyword name t] consumes the [@name] token or raises. *)

val drain_until_block : t -> Component.t list
(** [drain_until_block t] consumes components up to (but not including) the next
    block or semicolon, returning the drained components. Used for at-rule
    preludes. *)

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
val try_kind_pair : Token.kind -> Token.kind -> t -> bool

val looking_at : t -> string -> bool
(** [looking_at t s] is [true] iff the next component (after leading whitespace)
    starts with [s] — matches an ident, a function name followed by [(], or a
    delim-based prefix. *)

val looking_at_ident : string -> t -> bool
val looking_at_func : string -> t -> bool

(** {1 Expectations} *)

val expect : char -> t -> unit
(** [expect c t] consumes the delim [c] or raises. *)

val expect_string : string -> t -> unit
(** [expect_string s t] consumes the ident [s] or raises. *)

val expect_eof : t -> unit
(** [expect_eof t] raises if any non-whitespace component remains. *)

(** {1 Group / function helpers} *)

val parens : (t -> 'a) -> t -> 'a
(** [parens f t] consumes a [(...)] block and calls [f] with a fresh cursor over
    its contents. Raises if the next component is not a parenthesised block. *)

val brackets : (t -> 'a) -> t -> 'a
(** [brackets f t] consumes a [[...]] block similarly. *)

val braces : (t -> 'a) -> t -> 'a
(** [braces f t] consumes a [{...}] block similarly. *)

val call : string -> t -> (t -> 'a) -> 'a
(** [call name t f] consumes a [name(...)] function call and applies [f] to a
    cursor over its arguments. Raises if no such function is next. *)

val function_call : string -> (t -> 'a) -> t -> 'a option
(** [function_call name f t] consumes a [name(...)] call and calls [f] over its
    arguments. Returns [None] without advancing if the next component is not a
    function with that name. *)

val any_function_call : (string -> t -> 'a) -> t -> 'a option

(** {1 Enums} *)

val enum : ?default:(t -> 'a) -> string -> (string * 'a) list -> t -> 'a
(** [enum ?default label table t] consumes an ident and looks it up in [table].
    Falls back to [default] if provided, otherwise raises. *)

val try_enum : (string * 'a) list -> t -> 'a option
(** [try_enum table t] is like {!enum} but returns [None] without raising. *)

val enum_calls : ?default:(t -> 'a) -> (string * (t -> 'a)) list -> t -> 'a
(** [enum_calls ?default table t] dispatches on the name of the next function
    call. Each parser receives the raw cursor (still pointing at the function)
    and is expected to consume it. *)

val enum_or_calls :
  ?default:(t -> 'a) ->
  string ->
  (string * 'a) list ->
  ?calls:(string * (t -> 'a)) list ->
  t ->
  'a
(** [enum_or_calls ?default label idents ?calls t] first tries to match [idents]
    (ident token), then [calls] (function call), and finally falls back to
    [default]. Raises if none apply and no default is given. *)

(** {1 Higher-order combinators} *)

val option : (t -> 'a) -> t -> 'a option
(** [option p t] returns [Some (p t)] on success, [None] if [p] raises
    {!Parse_error} (cursor is rewound). *)

val one_of : (t -> 'a) list -> t -> 'a
(** [one_of ps t] tries each parser in order, rewinding on failure. *)

val list :
  ?sep:(t -> unit) -> ?at_least:int -> ?at_most:int -> (t -> 'a) -> t -> 'a list
(** [list ?sep ?at_least ?at_most item t] parses items separated by [sep]
    (default: no separator). Enforces cardinality bounds. *)

val fold_many :
  (t -> 'a) -> init:'s -> f:('s -> 'a -> 's) -> t -> 's * string option
(** Like {!many} but folds into an accumulator. Returns the final accumulator
    and the last error (if any). *)

val many : (t -> 'a) -> t -> 'a list * string option
(** [many p t] runs [p] repeatedly while it succeeds; returns the list of
    results and the last error message on failure (or [None] if the stream
    simply ran out). *)

val pair : ?sep:(t -> unit) -> (t -> 'a) -> (t -> 'b) -> t -> 'a * 'b

val triple :
  ?sep:(t -> unit) -> (t -> 'a) -> (t -> 'b) -> (t -> 'c) -> t -> 'a * 'b * 'c

val try_parse_err : (t -> 'a) -> t -> ('a, string) result
(** [try_parse_err p t] returns [Ok v] on success or [Error msg] on parse
    failure (cursor rewound). *)
