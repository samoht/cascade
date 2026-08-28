(** CSS properties that are not yet Baseline "widely available".

    Generated from the [web-features] Baseline dataset by
    [scripts/gen_baseline.sh]; regenerate when browsers ship new features. A
    vendor-prefixed declaration whose unprefixed twin names one of these keeps
    its prefix, since a maintained browser may read only the prefixed form. The
    list is conservative: web-features tracks a feature's full support, so a
    long-established property (e.g. [background-attachment]) appears when a
    recent value of it is not yet widely available. *)

val greenfield_properties : string list
(** CSS property names whose owning web-features feature is not Baseline "widely
    available". *)
