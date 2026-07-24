(** CSS variables interface types *)

open Values
open Properties

(** {1 Custom Property Syntax} *)

(** Type-safe CSS [@property] syntax descriptors per CSS Properties and Values
    API 1 sec. 2 (https://drafts.css-houdini.org/css-properties-values-api-1/).
    The spec admits only the named [<...>] type references below, the universal
    [*], bare [<ident>] keywords, and the [+]/[#] multipliers; alternatives are
    joined by [|]. *)
type 'a syntax =
  | Length : length syntax
  | Color : color syntax
  | Number : float syntax
  | Integer : int syntax
  | Percentage : percentage syntax
  | Length_percentage : length_percentage syntax
  | Angle : angle syntax
  | Time : duration syntax
  | Resolution : string syntax
  | Custom_ident : string syntax
  | String : string syntax
  | Url : string syntax
  | Image : background_image syntax
  | Transform_function : string syntax
  | Transform_list : string syntax
  | Universal : string syntax
  | Or : 'a syntax * 'b syntax -> ('a, 'b) Either.t syntax
  | Plus : 'a syntax -> 'a list syntax
  | Hash : 'a syntax -> 'a list syntax
  | Ident_keyword : string -> unit syntax
      (** Literal ident keyword in a [@property] syntax disjunction (e.g. [auto]
          in [<length> | auto]). The keyword has no value-level data; matching
          the spelling is enough. *)

(** Existential wrapper for syntax of any type *)
type any_syntax = Syntax : 'a syntax -> any_syntax

(** {1 Types} *)

type nonrec custom_value = custom_value
(** CSS custom-property token stream. *)

type any_var =
  | V : 'a var -> any_var
      (** Existential wrapper for CSS variables of any type *)
