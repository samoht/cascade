(** Build-time metadata for the Cascade library.

    Exposes a {!version} string derived from [dune-build-info] (for tagged
    releases), the git short hash (for release builds), or ["dev"] during
    development. *)

val version : string
(** [version] is the current version string. Uses the dune-build-info version
    when available (tagged releases), falls back to the git short hash in
    release builds, or ["dev"] during development. *)
