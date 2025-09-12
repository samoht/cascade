(** CSS variables interface types *)

open Values

(** {1 Types} *)

type any_var =
  | V : 'a var -> any_var
      (** Existential wrapper for CSS variables of any type *)
