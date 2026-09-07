(** Which grammar the target browsers implement.

    Cascade reads against the specifications, so it accepts grammar a browser
    has not shipped. Whether that is true of a given production is a fact about
    browsers, and this answers it from the web-features dataset
    {!Baseline.support} carries rather than from a list written by hand. A
    browser that catches up moves the answer at the next
    [scripts/gen_baseline.sh] run.

    Keys are BCD compat keys, the names web-features itself uses:
    [css.properties.text-overflow.string] for one arm of a property,
    [css.properties.zoom] for a whole one, [css.types.image.cross-fade] for a
    value type. *)

type version = int * int
(** A browser version as [(major, minor)]. *)

type targets = {
  chrome : version;
  firefox : version;
  safari : version;
  ios_safari : version;
}
(** The browser versions a run answers for. *)

val evergreen : targets
(** The default contract: Chrome 111, Firefox 128, Safari 16.4, iOS Safari 16.4.
*)

val implemented : targets -> string -> bool option
(** [implemented targets key] is [Some true] when every engine in [targets] is
    at or past the version that shipped [key], [Some false] when one of them is
    not, and [None] when the dataset says nothing about [key].

    [None] is not "unsupported": it is what web-features does not model, which
    is every vendor-prefixed property and every arm BCD gives no key. A caller
    that needs an answer for one of those has to measure it. *)

(** One rendering engine of a {!targets}. *)
type engine = Chrome | Firefox | Safari | Ios_safari

val engine_implements : engine -> version -> string -> bool option
(** [engine_implements engine version key] is whether [engine] at [version]
    implements [key], and [None] when the dataset does not model [key]. Asking
    about one engine is what a harness comparing cascade against a single
    browser needs; {!implemented} answers for a whole target set. *)

val unimplemented_by : targets -> string -> bool
(** [unimplemented_by targets key] is [true] only when the dataset says some
    engine in [targets] lacks [key]. An unknown key is [false], so this reads as
    "known to be missing" rather than "not known to be present". *)
