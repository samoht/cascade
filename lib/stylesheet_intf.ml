(** CSS stylesheet interface types *)

(** {1 Core Types} *)

(** {2 Import Rule} *)

type import_rule = {
  url : string;  (** URL or string to import *)
  layer : string option;  (** Optional layer name *)
  supports : string option;  (** Optional supports condition *)
  media : string option;  (** Optional media query *)
}
(** A CSS [@import] rule *)

(** {2 Property Rule} *)

type 'a property_rule = {
  name : string;
  syntax : 'a Variables.syntax;
  inherits : bool;
  initial_value : 'a option;
}
(** Type-safe CSS [@property] rule with typed syntax and initial value *)

(** {2 Basic Rules} *)

type rule = {
  selector : Selector.t;
  declarations : Declaration.declaration list;
  nested : statement list;
}
(** A CSS rule with a selector, declarations, and optional nested rules/at-rules
*)

(** {2 Statements and Blocks} *)

(** A CSS statement - either a rule or an at-rule *)
and statement =
  | Rule of rule
  | Charset of string  (** [@charset "UTF-8";] *)
  | Import of import_rule  (** [@import url(...) layer(...) supports(...);] *)
  | Namespace of string option * string  (** [@namespace prefix? url;] *)
  | Property : 'a property_rule -> statement  (** [@property --name { ... }] *)
  | Layer_decl of string list  (** [@layer theme, base, utilities;] *)
  | Layer of string option * block  (** [@layer name? { ... }] *)
  | Media of string * block  (** [@media (...) { ... }] *)
  | Container of string option * string * block
      (** [@container name? (...) { ... }] *)
  | Supports of string * block  (** [@supports (...) { ... }] *)
  | Starting_style of block  (** [@starting-style { ... }] *)
  | Scope of string option * string option * block
      (** [@scope (start)? to (end)? { ... }] *)
  | Keyframes of string * keyframe list  (** [@keyframes name { ... }] *)
  | Font_face of Declaration.declaration list  (** [@font-face { ... }] *)
  | Page of string option * Declaration.declaration list
      (** [@page :first { ... }] *)

and block = statement list
(** A block contains a list of statements *)

and keyframe = {
  keyframe_selector : string;  (** e.g., "0%", "from", "50%", "to" *)
  keyframe_declarations : Declaration.declaration list;
}
(** A single keyframe within [@keyframes] *)

(** {1 Stylesheet Structure} *)

type stylesheet = statement list
(** A CSS stylesheet is a list of statements *)

type t = stylesheet
(** Alias for backwards compatibility *)

(** {1 Rendering} *)

type mode = Variables | Inline  (** Rendering mode for CSS output *)

type config = { minify : bool; mode : mode; optimize : bool; newline : bool }
(** Configuration for CSS rendering *)
