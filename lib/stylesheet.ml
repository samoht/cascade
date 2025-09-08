(** CSS stylesheet types and construction functions *)

open Declaration
include Stylesheet_intf

(** {1 Accessors} *)

let stylesheet_rules t = t.rules
let stylesheet_layers t = t.layers
let stylesheet_media_queries t = t.media_queries
let stylesheet_container_queries t = t.container_queries

(** {1 Creation} *)

let rule ~selector declarations : rule = { selector; declarations }
let selector (rule : rule) = rule.selector
let declarations (rule : rule) = rule.declarations

let media ~condition rules : media_rule =
  { media_condition = condition; media_rules = rules }

let media_condition media = media.media_condition
let media_rules media = media.media_rules

let supports ~condition rules =
  { supports_condition = condition; supports_content = Support_rules rules }

let supports_nested ~condition rules nested_queries =
  {
    supports_condition = condition;
    supports_content = Support_nested (rules, nested_queries);
  }

let container ?name ~condition rules : container_rule =
  {
    container_name = name;
    container_condition = condition;
    container_rules = rules;
  }

let property ~syntax ?initial_value ?(inherits = false) name : property_rule =
  { name; syntax; inherits; initial_value }

let property_rule_name (r : property_rule) = r.name
let property_rule_initial (r : property_rule) = r.initial_value

let default_decl_of_property_rule (r : property_rule) =
  match r.initial_value with
  | Some v -> custom_property r.name v
  | None -> custom_property r.name ""

let rule_to_nested rule : nested_rule = Rule rule
let supports_to_nested supports : nested_rule = Supports supports

let layer ~name ?(media = []) ?(container = []) ?(supports = []) rules =
  {
    layer = name;
    rules;
    media_queries = media;
    container_queries = container;
    supports_queries = supports;
  }

let layer_name (layer : layer_rule) = layer.layer
let layer_rules (layer : layer_rule) : nested_rule list = layer.rules

let empty =
  {
    charset = None;
    imports = [];
    namespaces = [];
    layers = [];
    keyframes = [];
    font_faces = [];
    pages = [];
    rules = [];
    media_queries = [];
    container_queries = [];
    starting_styles = [];
    supports_queries = [];
    at_properties = [];
  }

let concat stylesheets =
  List.fold_left
    (fun acc sheet ->
      {
        charset = (match acc.charset with None -> sheet.charset | c -> c);
        imports = acc.imports @ sheet.imports;
        namespaces = acc.namespaces @ sheet.namespaces;
        layers = acc.layers @ sheet.layers;
        keyframes = acc.keyframes @ sheet.keyframes;
        font_faces = acc.font_faces @ sheet.font_faces;
        pages = acc.pages @ sheet.pages;
        rules = acc.rules @ sheet.rules;
        media_queries = acc.media_queries @ sheet.media_queries;
        container_queries = acc.container_queries @ sheet.container_queries;
        starting_styles = acc.starting_styles @ sheet.starting_styles;
        supports_queries = acc.supports_queries @ sheet.supports_queries;
        at_properties = acc.at_properties @ sheet.at_properties;
      })
    empty stylesheets

let stylesheet items =
  List.fold_left
    (fun acc item ->
      match item with
      | Charset c -> { acc with charset = Some c }
      | Import i -> { acc with imports = acc.imports @ [ i ] }
      | Namespace n -> { acc with namespaces = acc.namespaces @ [ n ] }
      | Rule r -> { acc with rules = acc.rules @ [ r ] }
      | Media m -> { acc with media_queries = acc.media_queries @ [ m ] }
      | Container c ->
          { acc with container_queries = acc.container_queries @ [ c ] }
      | Starting_style s ->
          { acc with starting_styles = acc.starting_styles @ [ s ] }
      | Supports s ->
          { acc with supports_queries = acc.supports_queries @ [ s ] }
      | Property a -> { acc with at_properties = acc.at_properties @ [ a ] }
      | Layer l -> { acc with layers = acc.layers @ [ l ] }
      | Keyframes k -> { acc with keyframes = acc.keyframes @ [ k ] }
      | Font_face f -> { acc with font_faces = acc.font_faces @ [ f ] }
      | Page p -> { acc with pages = acc.pages @ [ p ] })
    empty items

let stylesheet_items t =
  let charset = match t.charset with None -> [] | Some c -> [ Charset c ] in
  let imports = List.map (fun i -> Import i) t.imports in
  let namespaces = List.map (fun n -> Namespace n) t.namespaces in
  let rules = List.map (fun r -> Rule r) t.rules in
  let media = List.map (fun m -> Media m) t.media_queries in
  let containers = List.map (fun c -> Container c) t.container_queries in
  let supports = List.map (fun s -> Supports s) t.supports_queries in
  let layers = List.map (fun l -> Layer l) t.layers in
  let properties = List.map (fun p -> Property p) t.at_properties in
  let starting = List.map (fun s -> Starting_style s) t.starting_styles in
  let keyframes = List.map (fun k -> Keyframes k) t.keyframes in
  let font_faces = List.map (fun f -> Font_face f) t.font_faces in
  let pages = List.map (fun p -> Page p) t.pages in
  charset @ imports @ namespaces @ keyframes @ font_faces @ pages @ rules
  @ media @ containers @ supports @ layers @ properties @ starting
