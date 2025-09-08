(** CSS rendering interface types *)

(** {1 Types} *)

(** Rendering mode for CSS output *)
type mode = Variables | Inline

type config = { minify : bool; mode : mode; optimize : bool }
(** Configuration for CSS rendering *)
