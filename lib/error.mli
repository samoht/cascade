(** Parse errors, sealed.

    Every error is anchored to a {!Loc.t} and labelled with the {!Sort.t} of the
    IR node the parser was building when the failure occurred. The {!kind}
    variant is closed: every CSS-syntax-level failure that the parser can emit
    is enumerated here. *)

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
  | Unknown_at_rule of string  (** At-keyword name has no registered handler. *)
  | Unterminated of Sort.t
      (** Hit EOF inside a {!Sort.t} that needed a closing delimiter. *)

type t = { loc : Loc.t; sort : Sort.t; kind : kind }

val pp_kind : kind Pp.t
(** [pp_kind] renders just the reason, e.g.
    [expected <ident> but found <delim '.'>]. *)

val pp : t Pp.t
(** [pp] renders the located, sort-tagged error, e.g.
    [expected <ident> but found <delim '.'> at [12-13] (in selector)]. *)

val to_string : t -> string

(** {1 Named constructors}

    Use these at call sites instead of building records by hand — they give the
    parser a vocabulary that maps one-to-one with the spec. *)

val sort_mismatch : Loc.t -> sort:Sort.t -> expected:Sort.t -> found:Sort.t -> t
(** [sort_mismatch loc ~sort ~expected ~found] flags a category mismatch, e.g.
    an [At_rule] where a [Declaration] was needed. *)

val unexpected_token : Loc.t -> sort:Sort.t -> Token.kind -> t
(** [unexpected_token loc ~sort k] flags a stray token in the production
    labelled by [sort]. *)

val missing_token : Loc.t -> sort:Sort.t -> string -> t
(** [missing_token loc ~sort what] flags an expected lexeme that wasn't found.
    [what] is its short description, e.g. ["';'"]. *)

val bad_selector : Loc.t -> string -> t
(** [bad_selector loc reason] flags a selector the validator rejected; [reason]
    is a short human-readable note. *)

val bad_value : Loc.t -> property:string -> reason:string -> t
(** [bad_value loc ~property ~reason] flags a declaration whose right-hand side
    failed validation for the given [property]. *)

val unknown_at_rule : Loc.t -> string -> t
(** [unknown_at_rule loc name] flags an [\@name] keyword with no registered
    handler. *)

val unterminated : Loc.t -> Sort.t -> t
(** [unterminated loc s] flags an EOF reached while still inside a node of sort
    [s] that needed a closing delimiter. *)
