(** CSS features that are not yet Baseline "widely available".

    Generated from the [web-features] Baseline dataset by
    [scripts/gen_baseline.sh]; regenerate when browsers ship new features. A
    feature-query guard for one of these is load-bearing (an author writes it to
    detect exactly the feature), so {!Supports} keeps rather than unwraps it.
    The lists are conservative: web-features tracks a feature's full support, so
    a long-established property (e.g. [background-attachment]) appears when a
    recent value of it is not yet widely available, which at property
    granularity keeps the whole property's guard. *)

val greenfield_properties : string list
(** CSS property names whose owning web-features feature is not Baseline "widely
    available". *)

val greenfield_value_functions : string list
(** CSS value-function names (e.g. [anchor], [calc-size]) whose owning
    web-features feature is not Baseline "widely available". *)
