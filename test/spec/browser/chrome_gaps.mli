(** What a headless Chrome cannot arbitrate, and the vectors where it and the
    specifications disagree. Shared by the browser-backed harnesses here, so a
    browser that catches up is recorded once rather than in each of them. *)

val unimplemented : string list
(** [unimplemented] is the property names Chrome implements no grammar for, so
    it accepts no vector for them and cannot answer about them. Each harness
    checks the list against its own population in both directions. *)

val unimplemented_property : string -> bool
(** [unimplemented_property name] is [true] when [name] is in {!unimplemented}.
*)

type excuse = {
  properties : string list;
  key : string option;
  value : string;
  why : string;
}
(** One property-value pair the browser and the specifications disagree about,
    with the spec text that decides it.

    [key] is the BCD compat key web-features records the production under, when
    it records one. That is what makes the entry checkable: {!Support} says
    whether Chrome has since shipped it, so the entry goes stale when the
    browser catches up rather than when a seeded generator stops drawing the
    literal. [None] is a production web-features does not model, which no lookup
    can answer and which stays a measurement. *)

val spec_ahead : excuse list
(** [spec_ahead] is grammar a specification defines and Chrome rejects, so a
    Chrome rejection says nothing about the value. *)

val lenient : excuse list
(** [lenient] is a value Chrome accepts that no specification grants, so a
    Chrome acceptance says nothing about the value. *)

val find : excuse list -> property:string -> value:string -> excuse option
(** [find table ~property ~value] is the entry of [table] covering that pair. *)

(** Which side of a cascade-vs-browser divergence is wrong. Every differential
    here produces this classification and used to discard it, so a browser-bug
    candidate was only ever noticed by a human reading the output. *)
type verdict =
  | Browser_behind
      (** A specification defines the grammar and the dataset says a target
          engine has not shipped it, so cascade is right to read it and the
          browser's answer says nothing about the value. A candidate to report
          upstream. *)
  | Cascade_wrong
      (** The dataset says every target ships it, so the browser's answer is the
          specification's and cascade's disagreement is a defect. *)
  | Needs_measurement
      (** web-features models no key for it, which is every vendor-prefixed
          property and every arm BCD gives no key. Neither side is established
          without measuring, and saying so is the honest third answer. *)

val verdict_of : Cascade.Support.targets -> excuse -> verdict
(** [verdict_of targets e] classifies the divergence [e] excuses, from its
    {!excuse.key} and the generated support table. An entry with no key is
    {!Needs_measurement} however convincing its prose is. *)

val verdict_name : verdict -> string
(** [verdict_name v] names [v] for a report. *)

val overtaken : excuse list -> excuse list
(** [overtaken table] is the entries whose {!excuse.key} names a production
    Chrome has since shipped, so the disagreement they excuse is gone and the
    entry has to go with it. An entry with no key is never overtaken: nothing
    can answer for it, which is the cost of a production web-features does not
    model. *)

type shape = {
  shape_properties : string list;
  shape_name : string;
  matches : string -> bool;
  shape_why : string;
}
(** A disagreement that is about a SHAPE of value rather than one value, for the
    cases where naming every value is not possible: the generator draws from a
    seeded stream, so a literal list is a fact about one sample rather than
    about the browser. [shape_name] names the shape for the report and for the
    staleness check. *)

val lenient_shapes : shape list
(** [lenient_shapes] is {!lenient} keyed by a predicate. *)

val spec_ahead_shapes : shape list
(** [spec_ahead_shapes] is {!spec_ahead} keyed by a predicate. *)

val shape_covering :
  shape list -> property:string -> value:string -> shape option
(** [shape_covering table ~property ~value] is the entry of [table] whose
    predicate covers that pair. *)
