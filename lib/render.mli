(** CSS rendering and pretty-printing utilities *)

open Declaration
open Stylesheet
include module type of Render_intf

(** {1 Inline Styles} *)

val inline_style_of_declarations :
  ?optimize:bool -> ?minify:bool -> ?mode:mode -> declaration list -> string
(** Convert declarations to inline style string.
    @param optimize Apply optimization to declarations
    @param minify Produce minified output
    @param mode Rendering mode (Variables or Inline) *)

(** {1 Pretty-Printing} *)

val pp_rule : rule Pp.t
(** Pretty-print a CSS rule *)

val pp_media_rule : media_rule Pp.t
(** Pretty-print a media rule *)

val pp_container_rule : container_rule Pp.t
(** Pretty-print a container rule *)

val pp_supports_query : supports_rule Pp.t
(** Pretty-print a supports rule *)

val pp_supports_content : supports_content Pp.t
(** Pretty-print supports content *)

val pp_starting_style_rule : optimize:bool -> starting_style_rule Pp.t
(** Pretty-print a starting-style rule *)

val pp_property_rule : property_rule Pp.t
(** Pretty-print a property rule *)

val pp_layer : layer_rule Pp.t
(** Pretty-print a layer *)

val pp_nested_rule : nested_rule Pp.t
(** Pretty-print nested rules *)

val pp_layers : minify:bool -> layer_rule list Pp.t
(** Pretty-print layers with optional minification *)

val pp_stylesheet_sections : optimize:bool -> t Pp.t
(** Pretty-print stylesheet sections *)

(** {1 Stylesheet Rendering} *)

val version : string
(** Version string for generated CSS *)

val header : string
(** Header comment for generated CSS *)

val to_string : ?minify:bool -> ?optimize:bool -> ?mode:mode -> t -> string
(** Convert stylesheet to string.
    @param minify Produce minified output
    @param optimize Apply optimization
    @param mode Rendering mode (Variables or Inline) *)

val pp : ?minify:bool -> ?optimize:bool -> ?mode:mode -> t -> string
(** Pretty-print stylesheet (equivalent to to_string) *)
