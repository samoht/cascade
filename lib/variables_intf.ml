(** CSS variables interface types *)

open Values

(** {1 Custom Property Syntax} *)

(** Type-safe CSS @property syntax descriptors following CSS/MDN spec *)
type 'a syntax =
  | Length : length syntax
  | Color : color syntax
  | Number : float syntax
  | Integer : int syntax
  | Percentage : percentage syntax
  | Length_percentage : length_percentage syntax
  | Angle : angle syntax
  | Time : duration syntax
  | Custom_ident : string syntax
  | String : string syntax
  | Url : string syntax
  | Image : string syntax
  | Transform_function : string syntax
  | Universal : string syntax
  | Or : 'a syntax * 'b syntax -> ('a, 'b) Either.t syntax
  | Plus : 'a syntax -> 'a list syntax
  | Hash : 'a syntax -> 'a list syntax
  | Question : 'a syntax -> 'a option syntax
  | Brackets : string -> string syntax

(** Existential wrapper for syntax of any type *)
type any_syntax = Syntax : 'a syntax -> any_syntax

(** {1 Types} *)

type any_var =
  | V : 'a var -> any_var
      (** Existential wrapper for CSS variables of any type *)
