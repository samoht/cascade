(** CSS rendering interface types *)

(** {1 Types} *)

type mode = Variables | Inline  (** Rendering mode for CSS output *)

type config = { minify : bool; mode : mode; optimize : bool; newline : bool }
(** Configuration for CSS rendering *)
