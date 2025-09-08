(** CSS variables interface types *)

open Values

(** {1 Types} *)

type any_var =
  | V : 'a var -> any_var
      (** Existential wrapper for CSS variables of any type *)

type mode = Variables | Inline  (** Rendering mode for CSS output *)

type config = { minify : bool; mode : mode; optimize : bool }
(** Configuration for CSS rendering *)
