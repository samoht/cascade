val version : string
(** [version] is the current version string. Uses the dune-build-info version
    when available (tagged releases), falls back to the git short hash in
    release builds, or ["dev"] during development. *)
