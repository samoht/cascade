(** Build-time metadata for the Cascade library.

    Exposes a {!version} string taken from the [PROJECT_VERSION] environment
    variable when the build exports one (packaging builds such as bottler), from
    [dune-build-info] otherwise (tagged opam releases), or ["dev"] during
    development. *)

val version : string
(** [version] is the current version string: the [PROJECT_VERSION] environment
    variable when set at build time, the dune-build-info version when available,
    or ["dev"]. *)
