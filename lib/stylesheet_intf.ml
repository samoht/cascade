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
(** A CSS @starting-style rule *)

type supports_content =
  | Support_rules of rule list
  | Support_nested of rule list * supports_rule list
      (** Content of a @supports rule *)

and supports_rule = {
  supports_condition : string;
  supports_content : supports_content;
}
(** A CSS @supports rule *)

type nested_rule =
  | Rule of rule
  | Supports of supports_rule  (** A nested rule within a layer *)

type property_rule = {
  name : string;
  syntax : string;
  inherits : bool;
  initial_value : string option;
}
(** A CSS @property rule *)

type layer_rule = {
  layer : string;
  rules : nested_rule list;
  media_queries : media_rule list;
  container_queries : container_rule list;
  supports_queries : supports_rule list;
}
(** A CSS @layer rule *)

type t = {
  layers : layer_rule list;
  rules : rule list;
  media_queries : media_rule list;
  container_queries : container_rule list;
  starting_styles : starting_style_rule list;
  supports_queries : supports_rule list;
  at_properties : property_rule list;
}
(** A complete CSS stylesheet *)

type sheet_item =
  | Rule of rule
  | Media of media_rule
  | Supports of supports_rule
  | Container of container_rule
  | Layer of layer_rule
  | Property of property_rule
  | Starting_style of starting_style_rule
      (** A single item that can appear in a stylesheet *)
