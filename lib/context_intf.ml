type cascade_rule = {
  property_name : string;
  important : bool;
  layer : string option;
  source_order : int;
  declaration : Declaration.declaration;
}

type t = {
  custom_properties : Declaration.declaration list;
  runtime_vars : string list;
      (** Custom-property names (no [--] prefix) that must stay live [var()]
          references: a resolver keeps them like a [~runtime] var, so the
          inliner can still apply value-independent simplifications without
          collapsing the reference. *)
  inherited_values : Declaration.declaration list;
  initial_values : Declaration.declaration list;
  layer_order : string list;
  layer : string option;
  cascade_rules : cascade_rule list option;
  base_url : string option;
  root_font_size : Values.length option;
  parent_font_size : Values.length option;
  current_color : Values.color option;
  viewport_width : Values.length option;
  viewport_height : Values.length option;
  container_width : Values.length option;
  container_height : Values.length option;
}

type document = {
  root : string option;
  scope : Selector.t option;
  element : string option;
  classes : string list;
  ids : string list;
  attributes : (string * string option) list;
  pseudo_classes : string list;
  pseudo_elements : string list;
}

type query = {
  media_type : string option;
  media_features : Media.t list;
      (** Media features the rendering environment claims to expose. Build
          entries with [Media.feature] or [Media.boolean]. *)
  media_inapplicable : Media.name list;
  supports : Supports.t list;
      (** Capability flags the environment claims to support, normally a
          [Supports.Property]/[Supports.Func] leaf. Compound forms
          ([And]/[Or]/[Not]) are accepted but match only a structurally
          identical query. *)
  container_name : string option;
  container_features : Container.t list;
      (** Container capabilities exposed by the matching container. Build size
          features with [Container.feature], style queries with
          [Container.style], and scroll-state queries with
          [Container.scroll_state]. *)
}

type loader = { base_url : string option; imports : (string * string) list }

type animation = {
  timeline_time : string option;
  progress : float option;
  animated_properties : string list;
}

type property_registration = {
  name : string;
  syntax : Variables.any_syntax;
  inherits : bool;
  initial_value : string option;
}

type property_registry = { property_registrations : property_registration list }
