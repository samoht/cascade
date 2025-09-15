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
  | Font_face of font_face_descriptor list  (** [@font-face { ... }] *)
  | Page of string option * Declaration.declaration list
      (** [@page :first { ... }] *)

and block = statement list
(** A block contains a list of statements *)

and keyframe = {
  keyframe_selector : string;  (** e.g., "0%", "from", "50%", "to" *)
  keyframe_declarations : Declaration.declaration list;
}
(** A single keyframe within [@keyframes] *)

(** Font-face descriptors per CSS Fonts spec *)
and font_face_descriptor =
  | Font_family of Properties.font_family list  (** Font family name *)
  | Src of string  (** Font source (url(), local(), etc.) - TODO: proper type *)
  | Font_style of Properties.font_style  (** normal, italic, oblique *)
  | Font_weight of Properties.font_weight  (** normal, bold, 100-900 *)
  | Font_stretch of Properties.font_stretch
      (** normal, condensed, expanded, etc. *)
  | Font_display of string
      (** auto, block, swap, fallback, optional - TODO: proper type *)
  | Unicode_range of string  (** Unicode range - TODO: proper type *)
  | Font_variant of string  (** Font variant settings - TODO: proper type *)
  | Font_feature_settings of string
      (** OpenType feature settings - TODO: proper type *)
  | Font_variation_settings of string
      (** Variable font settings - TODO: proper type *)
  | Size_adjust of string  (** Size adjustment percentage - TODO: proper type *)
  | Ascent_override of string  (** Ascent metric override - TODO: proper type *)
  | Descent_override of string
      (** Descent metric override - TODO: proper type *)
  | Line_gap_override of string
      (** Line gap metric override - TODO: proper type *)

(** {1 Stylesheet Structure} *)

type stylesheet = statement list
(** A CSS stylesheet is a list of statements *)

type t = stylesheet
(** Alias for backwards compatibility *)

(** {1 Rendering} *)

type mode = Variables | Inline  (** Rendering mode for CSS output *)

type config = { minify : bool; mode : mode; optimize : bool; newline : bool }
(** Configuration for CSS rendering *)
