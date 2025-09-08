(** CSS stylesheet interface types *)

(** {1 Types} *)

type rule = {
  selector : Selector.t;
  declarations : Declaration.declaration list;
}
(** A CSS rule with a selector and declarations *)

type media_rule = { media_condition : string; media_rules : rule list }
(** A CSS media query rule *)

type container_rule = {
  container_name : string option;
  container_condition : string;
  container_rules : rule list;
}
(** A CSS container query rule *)

type starting_style_rule = { starting_rules : rule list }
(** A CSS [@starting-style] rule *)

type supports_content =
  | Support_rules of rule list
  | Support_nested of rule list * supports_rule list
      (** Content of a [@supports] rule *)

and supports_rule = {
  supports_condition : string;
  supports_content : supports_content;
}
(** A CSS [@supports] rule *)

type nested_rule =
  | Rule of rule
  | Supports of supports_rule  (** A nested rule within a layer *)

type property_rule = {
  name : string;
  syntax : string;
  inherits : bool;
  initial_value : string option;
}
(** A CSS [@property] rule *)

type layer_rule = {
  layer : string;
  rules : nested_rule list;
  media_queries : media_rule list;
  container_queries : container_rule list;
  supports_queries : supports_rule list;
}
(** A CSS [@layer] rule *)

type keyframe_block = {
  selector : string;  (** e.g., "0%", "from", "50%", "to" *)
  declarations : Declaration.declaration list;
}
(** A single keyframe block within [@keyframes] *)

type keyframes_rule = {
  name : string;  (** Animation name *)
  keyframes : keyframe_block list;
}
(** A CSS [@keyframes] rule for animations *)

type font_face_rule = {
  font_family : string option;
  src : string option;  (** URL or local font *)
  font_style : string option;
  font_weight : string option;
  font_stretch : string option;
  font_display : string option;
  unicode_range : string option;
  font_variant : string option;
  font_feature_settings : string option;
  font_variation_settings : string option;
}
(** A CSS [@font-face] rule for custom fonts *)

type import_rule = {
  url : string;  (** URL or string to import *)
  layer : string option;  (** Optional layer name *)
  supports : string option;  (** Optional supports condition *)
  media : string option;  (** Optional media query *)
}
(** A CSS [@import] rule *)

type page_rule = {
  selector : string option;  (** e.g., ":first", ":left", ":right" *)
  declarations : Declaration.declaration list;
}
(** A CSS [@page] rule for print styling *)

type charset_rule = { encoding : string  (** e.g., "UTF-8" *) }
(** A CSS [@charset] rule (must be first in stylesheet) *)

type namespace_rule = {
  prefix : string option;  (** Optional namespace prefix *)
  uri : string;  (** Namespace URI *)
}
(** A CSS [@namespace] rule for XML namespaces *)

type t = {
  charset : charset_rule option;  (** Must be first if present *)
  imports : import_rule list;
      (** Must come before other rules except [@charset] *)
  namespaces : namespace_rule list;
      (** Must come after [@charset] and [@import] *)
  layers : layer_rule list;
  keyframes : keyframes_rule list;
  font_faces : font_face_rule list;
  pages : page_rule list;
  rules : rule list;
  media_queries : media_rule list;
  container_queries : container_rule list;
  starting_styles : starting_style_rule list;
  supports_queries : supports_rule list;
  at_properties : property_rule list;
}
(** A complete CSS stylesheet with proper at-rule ordering *)

type sheet_item =
  | Charset of charset_rule
  | Import of import_rule
  | Namespace of namespace_rule
  | Rule of rule
  | Media of media_rule
  | Supports of supports_rule
  | Container of container_rule
  | Layer of layer_rule
  | Property of property_rule
  | Starting_style of starting_style_rule
  | Keyframes of keyframes_rule
  | Font_face of font_face_rule
  | Page of page_rule  (** A single item that can appear in a stylesheet *)
