(** CSS stylesheet interface *)

open Declaration
include module type of Stylesheet_intf

(** {1 Construction Functions} *)

val rule :
  selector:Selector.t -> ?nested:statement list -> declaration list -> rule
(** [rule ~selector ?nested declarations] creates a CSS rule with optional
    nested rules/at-rules. *)

val property :
  syntax:'a Variables.syntax ->
  ?initial_value:'a ->
  ?inherits:bool ->
  string ->
  statement
(** [property ~syntax ?initial_value ?inherits name] creates a [@property] rule
    with typed syntax and initial value. *)

val layer_decl : string list -> statement
(** [layer_decl names] creates a layer declaration statement. *)

val layer : ?name:string -> block -> statement
(** [layer ?name content] creates a [@layer] rule. *)

val media : condition:string -> block -> statement
(** [media ~condition content] creates a [@media] rule. *)

val media_nested : condition:string -> Declaration.declaration list -> statement
(** [media_nested ~condition declarations] creates a [@media] rule for CSS
    nesting, containing bare declarations (no selector). Used inside rules where
    the selector is inherited from the parent. *)

val container : ?name:string -> condition:string -> block -> statement
(** [container ?name ~condition content] creates a [@container] rule. *)

val supports : condition:string -> block -> statement
(** [supports ~condition content] creates a [@supports] rule. *)

val starting_style : block -> statement
(** [starting_style content] creates a [@starting-style] rule. *)

val starting_style_nested : Declaration.declaration list -> statement
(** [starting_style_nested declarations] creates a [@starting-style] rule for
    CSS nesting, containing bare declarations (no selector). Used inside rules
    where the selector is inherited from the parent. *)

val keyframes : string -> keyframe list -> statement
(** [keyframes name frames] creates a [@keyframes] animation rule. *)

val v : statement list -> stylesheet
(** [v statements] creates a stylesheet from a list of statements. *)

val empty_stylesheet : stylesheet
(** [empty_stylesheet] is an empty stylesheet. *)

(** {1 Accessors} *)

val selector : rule -> Selector.t
(** [selector rule] returns the selector of a rule. *)

val declarations : rule -> declaration list
(** [declarations rule] returns the declarations of a rule. *)

val nested : rule -> statement list
(** [nested rule] returns the nested statements of a rule. *)

(** {1 Reading/Parsing} *)

val read_rule : Reader.t -> rule
(** [read_rule r] reads a CSS rule from the reader. *)

val read_block : Reader.t -> block
(** [read_block r] reads a CSS block from the reader. *)

val read_stylesheet : Reader.t -> stylesheet
(** [read_stylesheet r] reads a complete CSS stylesheet from the reader. *)

(** {1 Pretty Printing} *)

val pp_rule : rule Pp.t
(** [pp_rule] pretty-prints CSS rules. *)

val pp_stylesheet : stylesheet Pp.t
(** [pp_stylesheet] pretty-prints CSS stylesheets. *)

(** {1 Variable Extraction} *)

val vars_of_stylesheet : stylesheet -> Variables.any_var list
(** [vars_of_stylesheet ss] extracts all variables referenced in a stylesheet.
*)

(** {1 Rendering} *)

val to_string :
  ?minify:bool ->
  ?mode:mode ->
  ?newline:bool ->
  ?header:bool ->
  stylesheet ->
  string
(** [to_string ?minify ?mode ?newline stylesheet] renders a stylesheet to CSS.
*)

val pp :
  ?minify:bool ->
  ?mode:mode ->
  ?newline:bool ->
  ?header:bool ->
  stylesheet ->
  string
(** [pp] is {!to_string}. *)

val inline_style_of_declarations :
  ?minify:bool -> ?mode:mode -> ?newline:bool -> declaration list -> string
(** [inline_style_of_declarations declarations] converts declarations to inline
    style string. *)

(** {1 Legacy Compatibility} *)

val empty : t
(** [empty] is an empty stylesheet. *)

val rules : t -> rule list
(** [rules t] returns the top-level rules from the stylesheet. *)

val layers : t -> string list
(** [layers t] returns the layer names from the stylesheet. *)

val media_queries : t -> (string * rule list) list
(** [media_queries t] returns the media queries from the stylesheet. *)

val container_queries : t -> (string option * string * rule list) list
(** [container_queries t] returns the container queries from the stylesheet. *)

(** {1 Parsing and Pretty-printing} *)

val read : Reader.t -> t
(** [read r] parses a stylesheet from the reader. *)

val pp_import_rule : import_rule Pp.t
(** [pp_import_rule] pretty-prints an import rule. *)

val read_import_rule : Reader.t -> import_rule
(** [read_import_rule r] parses an import rule. *)

val pp_config : config Pp.t
(** [pp_config] pretty-prints a config. *)

val read_config : Reader.t -> config
(** [read_config r] parses a config. *)
