(** Which productions the browser in front of these harnesses implements.

    A differential between cascade and a browser produces disagreements, and
    most of them are not defects: a browser that has not shipped a grammar
    rejects CSS the specifications grant, and one that ships more than they
    grant accepts CSS no specification does. Both are facts about browsers.

    This used to hold them as a table of property-value pairs with prose. It
    does not any more. {!Cascade.Support} answers "does this engine implement
    this production" from the web-features dataset, so the only thing a harness
    needs is the BCD compat key naming the production, and BCD names one from
    the property and the value. That is what this derives. A disagreement the
    dataset explains is reported as the fact it is, and one it does not explain
    is a finding.

    The consequence is that nothing here can talk a defect away. A hand-written
    entry stays true until someone rereads it; a lookup goes stale the day the
    browser ships the grammar, and the harness says so on the next run. *)

val unimplemented : string list
(** [unimplemented] is the property names Chrome implements no grammar for, so
    it accepts no vector for them and cannot answer about them. Each harness
    checks the list against its own population in both directions. *)

val unimplemented_property : string -> bool
(** [unimplemented_property name] is [true] when [name] is in {!unimplemented}.
*)

val keys_for :
  ?prefix:string -> property:string -> value:string -> unit -> string list
(** [keys_for ~property ~value ()] is the BCD compat keys that could name this
    production, most specific first.

    BCD names a keyword arm [css.properties.<property>.<value>], writing some
    values with [_] where CSS writes [-], names a multi-slot value after the
    slot that carries it, and names a value type under [css.types]. Each of
    those is a candidate, and the dataset decides which one exists. Deriving
    rather than listing is what keeps a browser gap from needing a hand-written
    entry that nothing can invalidate.

    [prefix] is the BCD namespace, [css.properties] by default; a descriptor is
    filed under [css.at-rules.font-face]. *)

(** What the support dataset says about a disagreement. *)
type explanation =
  | Not_shipped of string
      (** the key names a production this browser has not shipped, so its
          rejection says nothing about the value *)
  | Shipped_beyond_spec of string
      (** the key names a production this browser has shipped and no
          specification grants, so its acceptance says nothing about the value
      *)

val explanation_key : explanation -> string
(** [explanation_key e] is the BCD key [e] cites. *)

val explains_rejection :
  ?prefix:string ->
  chrome:Cascade.Support.version ->
  property:string ->
  value:string ->
  unit ->
  explanation option
(** [explains_rejection ~chrome ~property ~value ()] is [Some] when the dataset
    says this Chrome has not shipped a production naming that pair, so cascade
    reading a value the browser rejects is the browser being behind. *)

val explains_acceptance :
  ?prefix:string ->
  chrome:Cascade.Support.version ->
  property:string ->
  value:string ->
  unit ->
  explanation option
(** [explains_acceptance ~chrome ~property ~value ()] is [Some] when the dataset
    records a production this Chrome ships that no specification grants, so
    cascade rejecting a value the browser accepts is the browser being lenient.

    Only {!Cascade.Support.self_measured} keys answer here. The generated table
    records what browsers ship, which cannot say whether a specification grants
    it; that judgement is a measurement, and it lives in the library beside the
    specification text that decided it. *)

(** Which side of a cascade-vs-browser divergence is wrong. *)
type verdict =
  | Browser_behind
      (** A specification defines the grammar and the dataset says a target
          engine has not shipped it, so cascade is right to read it. A candidate
          to report upstream. *)
  | Cascade_wrong
      (** The dataset says every target ships it, so the browser's answer is the
          specification's and cascade's disagreement is a defect. *)
  | Needs_measurement
      (** web-features models no key for it, which is every vendor-prefixed
          property and every arm BCD gives no key. Neither side is established
          without measuring, and saying so is the honest third answer. *)

val verdict_of : Cascade.Support.targets -> string -> verdict
(** [verdict_of targets key] classifies a divergence explained by [key], from
    the generated support table. *)

val verdict_name : verdict -> string
(** [verdict_name v] names [v] for a report. *)
