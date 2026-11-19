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
  | Untargeted_selector of string
      (** The prelude is a selector no browser the minifier targets reads, so
          each of them drops the rule: one holding, outside every forgiving
          list, a pseudo-class none of them reads (Selectors 4 sec. 3.9), or a
          keyframe selector outside CSS Animations 1's grammar. Payload names
          it: the pseudo-class, colon and all, or [keyframe selector] and the
          prelude. *)
  | Bad_value of { property : string; reason : string }
      (** Property value validator rejected the right-hand side. *)
  | Bad_condition of { at_rule : string; reason : string }
      (** At-rule prelude (e.g. [@supports]) rejected by its condition grammar.
      *)
  | Unknown_at_rule of string  (** At-keyword name has no registered handler. *)
  | Invalid_statement of string
      (** A statement read whole that the rules around it refuse, such as an
          [@import] after a style rule; payload is the reason. *)
  | Unterminated of Sort.t
      (** Hit EOF inside a {!Sort.t} that needed a closing delimiter. *)
  | Too_deep of int
      (** A {!Sort.t} nests blocks or functions deeper than the payload, the
          most a reader goes into: {!Parser.max_nesting_depth} for what the
          parser builds, less for a grammar that recurses at each level. *)

(** Source texts and their positions. *)
module Source : sig
  type t
  (** The type for a source text and the index of its positions. The index is
      built by one walk of the text on the first {!locate} and kept, so every
      error that shares one value walks its text once in all. *)

  val v : string -> t
  (** [v text] is [text] with a fresh index. It reads nothing until the first
      {!locate}. *)

  val text : t -> string
  (** [text t] is the text [t] indexes. *)

  val locate : t -> int -> int * int
  (** [locate t pos] is the 1-based line and column of byte [pos] of {!text},
      clamped to it. As in CSS Syntax 3 (ED) sec. 3.3, CR, FF and a CRLF pair
      each end one line, and a column counts one code point, or one maximal
      ill-formed subpart, rather than a byte. Its cost does not grow with the
      text. *)
end

type t = {
  loc : Loc.t;
  sort : Sort.t;
  path : string list;
  kind : kind;
  source : Source.t option;
  filename : string option;
  recovery : Recovery.t;
}
(** {!field-path} is a breadcrumb trail from the outermost context down to the
    exact sub-production that failed, for a caller that inspects it; {!pp}
    leaves it out, since it names the reader's productions, not the source.
    {!field-source} carries the input text, for materialising a snippet on
    demand via {!snippet} and for {!position}; the errors of one parse share
    one. {!field-recovery} is what became of the construct the error is about,
    which the site that caught the error fills in through {!with_recovery}. *)

val pp_kind : kind Pp.t
(** [pp_kind] renders just the reason, e.g.
    [expected <ident> but found <delim '.'>]. *)

val pp_message : t Pp.t
(** [pp_message] renders the reason with no location, e.g.
    [bad value for color: not a color] or [unexpected <delim '.'> in selector]:
    the kinds that do not name what was being read are followed by the
    {!field-sort}. *)

val pp : t Pp.t
(** [pp] renders the error as [location: message], then the source line it is
    about, indented, with a caret line under the span, e.g.
    {v
    theme.css:2:5: bad selector: double dot
      a..b { color: red }
       ^
    v}
    The location is [source:line:column], the source being [<CSS input>] when
    the error has no {!field-filename}. An error without a {!field-source} has
    no line to give or quote, and names the source alone. *)

type severity = [ `Error | `Warning ]
(** The type for how a report ranks an error. *)

val position : t -> (int * int) option
(** [position t] is the 1-based line and column of [t]'s start in its
    {!field-source}, or [None] without one. As in CSS Syntax 3 (ED) sec. 3.3,
    CR, FF and a CRLF pair each end one line, and a column counts one code
    point, or one maximal ill-formed subpart, rather than a byte, as
    {!Source.locate} counts them. *)

val to_string : ?severity:severity -> t -> string
(** [to_string error] renders [error] as {!pp} does. With [severity] the word
    [error:] or [warning:] follows the location, the form compilers print and
    editors read: [theme.css:2:5: warning: bad selector: double dot]. *)

(** {1 Construction and raising}

    The constructors ([sort_mismatch], [bad_selector], ...) build an {!t}, which
    a reader collects as a warning, e.g. {!Parser.stylesheet}, or raises with
    {!fail} at the point of failure. *)

exception Parse_error of t
(** Raised by parsers (and the {!Cursor} helpers) on the first unrecoverable
    failure. *)

val fail : t -> 'a
(** Raises {!Parse_error} with the given value. *)

val catch : (unit -> 'a) -> ('a, t) result
(** [catch f] is [Ok (f ())], or [Error e] if [f] raises [Parse_error e]. Every
    public [of_string] returns through it, so {!Parse_error} stays inside the
    readers. *)

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
(** [v ~loc ~sort kind] builds an [Error.t]. [source] gets an index of its own;
    {!in_source} shares one. *)

val in_source : Source.t -> loc:Loc.t -> sort:Sort.t -> kind -> t
(** [in_source source ~loc ~sort kind] is [v ~loc ~sort kind] located in
    [source], whose index it shares with every other error built on [source]. *)

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

val as_selector : t -> t
(** [as_selector t] is [t] read as an error in a selector. The selector readers
    raise through the value readers' helpers, which build a {!Bad_value} with no
    property; the site that knows it read a selector turns it into a
    {!Bad_selector} with the same reason. Any other error is returned unchanged.
*)

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
