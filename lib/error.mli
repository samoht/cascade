(** Parse errors, sealed.

    Every error is anchored to a {!Loc.t} and labelled with the {!Sort.t} of the
    IR node the parser was building when the failure occurred. The {!type-kind}
    variant is closed: every CSS-syntax-level failure that the parser can emit
    is enumerated here. *)

(** What a recovery point did with the construct an error is about. *)
module Recovery : sig
  type construct =
    | Declaration  (** A [property: value] pair. *)
    | Rule  (** A rule or an at-rule, and everything it holds. *)

  type t =
    | Dropped of { construct : construct; text : string option }
        (** The construct's text reaches neither the AST nor anything printed
            back out from it, so a caller reading the parse never sees it.
            [text] is that source text, for a caller comparing one parse's
            losses against another's. [None] leaves the loss unnamed: the
            recovery point held no span for the whole construct. *)
    | Recovered
        (** The construct survives into the output. The error reports on it
            without costing it. *)

  val equal_construct : construct -> construct -> bool
  (** [equal_construct a b] is whether [a] and [b] are the same construct. *)

  val equal : t -> t -> bool
  (** [equal a b] is whether [a] and [b] are the same recovery. Two unnamed
      losses compare equal here; a caller weighing one parse's losses against
      another's wants a [text] on both sides instead. *)

  val dropped : ?source:string -> ?loc:Loc.t -> construct -> t
  (** [dropped ~source ~loc construct] is a {!Dropped} recovery naming the text
      [loc] spans in [source]. [loc] must cover the whole construct, not the
      point the failure was reported at. Called without both arguments, or with
      a span [source] does not cover, it names no text. *)
end

type kind =
  | Sort_mismatch of { expected : Sort.t; found : Sort.t }
      (** Got a node of the wrong category, e.g. a component where a declaration
          was required. *)
  | Unexpected_token of Token.kind
      (** Hit a token that has no place in the current production. *)
  | Missing_token of string
      (** Expected a specific lexical thing, e.g. ["';'"] or ["')'"]. *)
  | Bad_selector of string
      (** Selector parser rejected the prelude; payload is a short reason. *)
  | Bad_value of { property : string; reason : string }
      (** Property value validator rejected the right-hand side. *)
  | Bad_condition of { at_rule : string; reason : string }
      (** At-rule prelude (e.g. [@supports]) rejected by its condition grammar.
      *)
  | Unknown_at_rule of string  (** At-keyword name has no registered handler. *)
  | Unterminated of Sort.t
      (** Hit EOF inside a {!Sort.t} that needed a closing delimiter. *)

type t = {
  loc : Loc.t;
  sort : Sort.t;
  path : string list;
  kind : kind;
  source : string option;
  filename : string option;
  recovery : Recovery.t;
}
(** {!field-path} is a breadcrumb trail from the outermost context down to the
    exact sub-production that failed, rendered with ["/"] separators. {!source}
    carries the raw input string for materialising a snippet on demand via
    {!snippet}. {!field-recovery} is what became of the construct the error is
    about, which the site that caught the error fills in through
    {!with_recovery}. *)

val pp_kind : kind Pp.t
(** [pp_kind] renders just the reason, e.g.
    [expected <ident> but found <delim '.'>]. *)

val pp : t Pp.t
(** [pp] renders the located, sort-tagged error, e.g.
    [expected <ident> but found <delim '.'> at [12-13] (in selector)]. *)

val to_string : t -> string
(** [to_string error] renders [error] as source text. *)

(** {1 Construction and raising}

    Two flavours, [Stdlib.failwith]-style:

    - Value constructors ([sort_mismatch], [bad_selector], ...) build an {!t}.
      Use when collecting non-fatal warnings, e.g. {!Parser.stylesheet}.
    - Raising constructors ([fail_sort_mismatch], [fail_bad_selector], ...)
      build the same error and immediately raise {!Parse_error}. Use at the
      point of failure inside a decoder. *)

exception Parse_error of t
(** Raised by parsers (and the {!Cursor} helpers) on the first unrecoverable
    failure. *)

val fail : t -> 'a
(** Raises {!Parse_error} with the given value. *)

val with_context : string -> (unit -> 'a) -> 'a
(** [with_context label f] runs [f ()] and, on a raised {!Parse_error}, prepends
    [label] to the error's context path. Compose on every descent into a named
    sub-production ([:is()], [[attr]], [nth-child], ...) to build breadcrumb
    paths like [":is()/.foo"]. *)

val v :
  ?path:Loc.Path.t ->
  ?source:string ->
  ?filename:string ->
  loc:Loc.t ->
  sort:Sort.t ->
  kind ->
  t
(** [v ~loc ~sort kind] builds an [Error.t]. *)

val with_filename : ?filename:string -> t -> t
(** [with_filename ~filename t] stamps [filename] onto [t] when [t] does not
    already carry one. Used by entry points that know the source filename to
    annotate warnings collected by inner readers. *)

val with_recovery : Recovery.t -> t -> t
(** [with_recovery recovery t] records what the reader did with the construct
    [t] is about. The raise site knows what failed; only the site that catches
    the error knows whether the construct survives, so an error carries
    {!Recovery.Recovered} until such a site stamps it. *)

val with_property : string -> t -> t
(** [with_property property t] fills in the property name of a {!Bad_value}
    error raised without one - the typed value readers reject a right-hand side
    without knowing which property they serve, so the declaration parser stamps
    it back on. A non-{!Bad_value} error, or one that already names a property,
    is returned unchanged. *)

val context : t -> Loc.Context.t
(** [context t] is the structured error context. *)

val snippet : t -> Loc.Context.snippet option
(** [snippet t] materialises the source-context snippet from
    {!type-t.field-source} and {!type-t.field-loc}. Returns [None] when no
    source was attached. *)

(** {2 Value constructors} *)

val sort_mismatch : Loc.t -> sort:Sort.t -> expected:Sort.t -> found:Sort.t -> t
(** [sort_mismatch loc ~sort ~expected ~found] flags a category mismatch. *)

val unexpected_token : Loc.t -> sort:Sort.t -> Token.kind -> t
(** [unexpected_token loc ~sort k] flags a stray token in a [sort] production.
*)

val missing_token : Loc.t -> sort:Sort.t -> string -> t
(** [missing_token loc ~sort what] flags an expected lexeme that wasn't found.
*)

val bad_selector : Loc.t -> string -> t
(** [bad_selector loc reason] flags a selector the validator rejected. *)

val bad_value : Loc.t -> property:string -> reason:string -> t
(** [bad_value loc ~property ~reason] flags a failed property value. *)

val bad_condition : Loc.t -> at_rule:string -> reason:string -> t
(** [bad_condition loc ~at_rule ~reason] flags a failed at-rule prelude. *)

val unknown_at_rule : Loc.t -> string -> t
(** [unknown_at_rule loc name] flags an [\@name] keyword with no handler. *)

val unterminated : Loc.t -> Sort.t -> t
(** [unterminated loc s] flags an EOF inside an unclosed node of sort [s]. *)

(** {2 Raising constructors}

    [fail_x args] is [fail (x args)]. *)

val fail_sort_mismatch :
  Loc.t -> sort:Sort.t -> expected:Sort.t -> found:Sort.t -> 'a
(** [fail_sort_mismatch loc ~sort ~expected ~found] raises {!sort_mismatch}. *)

val fail_unexpected_token : Loc.t -> sort:Sort.t -> Token.kind -> 'a
(** [fail_unexpected_token loc ~sort k] raises {!unexpected_token}. *)

val fail_missing_token : Loc.t -> sort:Sort.t -> string -> 'a
(** [fail_missing_token loc ~sort what] raises {!missing_token}. *)

val fail_bad_selector : Loc.t -> string -> 'a
(** [fail_bad_selector loc reason] raises {!bad_selector}. *)

val fail_bad_value : Loc.t -> property:string -> reason:string -> 'a
(** [fail_bad_value loc ~property ~reason] raises {!bad_value}. *)

val fail_bad_condition : Loc.t -> at_rule:string -> reason:string -> 'a
(** [fail_bad_condition loc ~at_rule ~reason] raises {!bad_condition}. *)

val fail_unknown_at_rule : Loc.t -> string -> 'a
(** [fail_unknown_at_rule loc name] raises {!unknown_at_rule}. *)

val fail_unterminated : Loc.t -> Sort.t -> 'a
(** [fail_unterminated loc s] raises {!unterminated}. *)
