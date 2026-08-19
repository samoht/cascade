(** CSS stylesheet interface types *)

(** {1 Core Types} *)

(** {2 Import Rule} *)

type import_rule = {
  url : string;  (** URL or string to import *)
  layer : string option;  (** Optional layer name *)
  supports : Supports.t option;  (** Optional supports condition *)
  media : Media.t option;  (** Optional media query *)
}
(** A CSS [\@import] rule *)

(** {2 Property Rule} *)

type 'a property_rule = {
  name : string;
  syntax : 'a Variables.syntax;
  inherits : bool;
  initial_value : 'a option;
}
(** Type-safe CSS [@property] rule with typed syntax and initial value *)

(** {2 Cascade Origins} *)

type cascade_origin =
  | User_agent
  | User
  | Author_presentational_hint
  | Author
  | Animation
  | Transition
      (** Cascade origins from CSS Cascading and Inheritance. [Animation] and
          [Transition] represent generated virtual rules. *)

type cascade_layer_candidate = {
  layer : string option;
      (** Explicit layer name, or [None] for the implicit unlayered layer. *)
  important : bool;
      (** Whether this candidate comes from an important declaration. *)
  source_order : int;
      (** Later source-order values win after layer precedence ties. *)
  value : string;  (** Test/API payload representing the cascaded value. *)
}
(** A minimal same-origin/same-specificity cascade candidate used to model the
    layer and source-order parts of the cascade sorting order. *)

type cascade_origin_candidate = {
  origin : cascade_origin;
      (** Origin bucket that contributes this same-property candidate. *)
  important : bool;
      (** Whether this candidate comes from an important declaration. *)
  source_order : int;
      (** Later source-order values win after origin/importance ties. *)
  value : string;  (** Test/API payload representing the cascaded value. *)
}
(** A minimal same-specificity cascade candidate used to model origin,
    importance, and source-order sorting. *)

type declared_value = {
  property : string;
  value : string;
  important : bool;
  source_order : int;
}
(** A declared value contributed by one property declaration before cascade
    sorting. The value is the declaration's minified CSS value string. *)

type value_source =
  | Cascaded
  | Initial_default
  | Inherited_default
  | Initial_keyword
  | Inherit_keyword
  | Unset_initial
  | Unset_inherited
      (** Why a specified value was selected during defaulting. *)

type value = { value : string; value_source : value_source }
(** Result of applying the non-layout parts of specified-value defaulting. *)

type value_processing_stage =
  | Declared_value
  | Cascaded_value
  | Specified_value
  | Computed_value
  | Used_value
  | Actual_value  (** CSS Cascade value-processing stages. *)

(** [@namespace] prelude URI form. CSS Namespaces 3 1: [<string>] and
    [url(<string>)] are spec-equivalent; [url_form] further distinguishes
    whether the [url()] body itself quoted its argument so the pretty-printer
    can round-trip the source spelling. Under [--minify] the printer collapses
    every form to the bare quoted string (the shortest spelling). *)
type url_form = Bare | Quoted of char

type namespace_url = Url of string * url_form | Quoted of string

type cascade_candidate = {
  origin : cascade_origin;
  layer : string option;
  important : bool;
  specificity : int;
  scope_hops : int option;
  source_order : int;
  value : string;
}
(** A same-property cascade candidate covering the cascade ordering criteria
    this library can model without a DOM: origin/importance, layer, specificity,
    scoping proximity, and source order. *)

(** {2 Basic Rules} *)

type rule = {
  selector : Selector.t;
  declarations : Declaration.declaration list;
  nested : statement list;
  merge_key : string option;
}
(** A CSS rule with a selector, declarations, optional nested rules/at-rules,
    and an optional merge key for combining rules with identical declarations.
    When [merge_key] is [Some key], rules with the same key and identical
    declarations can be combined into a single rule with a selector list. *)

(** {2 Statements and Blocks} *)

(** A CSS statement - either a rule or an at-rule *)
and statement =
  | Rule of rule
  | Declarations of Declaration.declaration list
      (** Bare declarations for CSS nesting (no selector) *)
  | Bang_comment of string
      (** Preserved [/*! ... */] comment (license header convention). The body
          excludes the surrounding [/*!] / [*/] delimiters. *)
  | Charset of string  (** [@charset "UTF-8";] *)
  | Import of import_rule  (** [@import url(...) layer(...) supports(...);] *)
  | Namespace of string option * namespace_url  (** [@namespace prefix? url;] *)
  | Property : 'a property_rule -> statement  (** [@property --name { ... }] *)
  | Layer_decl of string list  (** [@layer theme, base, utilities;] *)
  | Layer of string option * block  (** [@layer name? { ... }] *)
  | Media of Media.t * block  (** [@media (...) { ... }] *)
  | Container of string option * Container.t option * block
      (** [@container name? (...) { ... }] *)
  | Supports of Supports.t * block  (** [@supports (...) { ... }] *)
  | Moz_document of moz_document_condition list * block
      (** [@-moz-document url-prefix(...) { ... }] *)
  | Starting_style of block  (** [@starting-style { ... }] *)
  | When of conditional * block  (** [@when media(...) { ... }] *)
  | Else of conditional option * block  (** [@else supports(...)? { ... }] *)
  | Supports_condition of string * Declaration.declaration list
      (** [@supports-condition --name { ... }] *)
  | Origin of cascade_origin * block
      (** API-level wrapper recording the cascade origin of a stylesheet block.
          It has no CSS surface syntax, but lets optimizers and tests preserve
          origin boundaries. *)
  | Scope of Selector.t option * Selector.t option * block
      (** [@scope (start)? to (end)? { ... }] *)
  | Keyframes of string * keyframe list  (** [@keyframes name { ... }] *)
  | Webkit_keyframes of string * keyframe list
      (** [@-webkit-keyframes name { ... }] *)
  | Moz_keyframes of string * keyframe list
      (** [@-moz-keyframes name { ... }] *)
  | Font_face of font_face_descriptor list  (** [@font-face { ... }] *)
  | Counter_style of string * counter_style_descriptor list
      (** [@counter-style name { ... }] *)
  | Page of page_selector list * Declaration.declaration list
      (** [@page :first { ... }]; empty list is a bare [@page] *)
  | Page_with_margins of
      page_selector list * page_descriptor list * page_margin_rule list
      (** [@page :first { margin: 1cm; @top-left { content: ... } }] *)
  | Font_palette_values of string * font_palette_descriptor list
      (** [@font-palette-values --name { ... }] *)
  | Font_feature_values of
      Properties.font_family list * font_feature_values_block list
      (** [@font-feature-values <family-name># { @styleset { nice: 1 } }] *)
  | View_transition of view_transition_descriptor list
      (** [@view-transition { navigation: auto }] *)
  | Position_try of string * Declaration.declaration list
      (** [@position-try --name { top: anchor(...) }] *)
  | Viewport of viewport_prefix * viewport_descriptor list
      (** [@viewport { ... }] / [@-ms-viewport { ... }] (CSS Device Adaptation
          1, deprecated but still emitted by minifiers for legacy IE). *)
  | Unknown_at_rule of {
      name : string;
      prelude : string;
      block : string option;
    }
      (** CSS Syntax 3 sec. 5.4.2 "consume an at-rule" preserves any
          unrecognised at-rule as raw text so authors can ship unknown vendor or
          future at-rules without dropping the whole stylesheet. *)

and block = statement list
(** A block contains a list of statements *)

and conditional =
  | Media_condition of Media.t
  | Supports_condition_test of Supports.t
  | And of conditional * conditional
  | Or of conditional * conditional

and moz_document_condition = Url_prefix of string option
and viewport_prefix = Standard | Ms_prefixed

and viewport_descriptor = { name : string; value : string }
(** Raw [<name>:<value>] pair inside [@viewport] / [@-ms-viewport]; viewport
    descriptors share names with regular CSS properties (e.g., [width]) but take
    a viewport-specific value grammar that includes [device-width],
    [device-height], so they aren't typed against the property reader. *)

and keyframe = {
  selector : Keyframe.selector;  (** e.g., [From], [To], [Percent 50.] *)
  declarations : Declaration.declaration list;
}
(** A single keyframe within [\@keyframes] *)

and page_pseudo = First | Left | Right | Blank

and page_selector = { name : string option; pseudos : page_pseudo list }
(** [@page] selector: an optional page name and zero or more pseudo-pages, e.g.
    [invoice:blank:first] *)

and page_descriptor = Declaration.declaration
and font_palette_base = Light | Dark | Index of int | Palette_ident of string

and font_palette_descriptor =
  | Palette_font_family of Properties.font_family list
  | Base_palette of font_palette_base
  | Override_colors of (int * Values.color) list

and font_feature_values_block = string * (string * int list) list

and counter_style_system =
  | Cyclic
  | Numeric
  | Alphabetic
  | Symbolic
  | Fixed of int option
  | Additive
  | Extends of string

and counter_style_descriptor =
  | System of counter_style_system
  | Symbols of string list
  | Suffix of string
  | Prefix of string
  | Fallback of string
  | Range of string
  | Pad of string
  | Negative of string
  | Additive_symbols of string
  | Speak_as of string

and view_transition_descriptor =
  | Navigation of [ `Auto | `None ]
  | Types of string list option

and font_variant_descriptor =
  | Normal
  | None
  | Values of font_variant_descriptor_value list

and font_variant_descriptor_value =
  | Ligature of Properties.font_variant_ligature
  | Caps of Properties.font_variant_caps
  | Numeric of Properties.font_variant_numeric_token
  | East_asian of Properties.east_asian_feature

and page_margin_rule = {
  name : string;
  descriptors : Declaration.declaration list;
}
(** CSS page margin at-rule inside [@page]. *)

(** Font-face descriptors per CSS Fonts spec *)
and font_face_descriptor =
  | Font_family of Properties.font_family list  (** Font family name *)
  | Src of Font_face.src  (** Font source (url(), local(), etc.) *)
  | Font_style of Properties.font_style  (** normal, italic, oblique *)
  | Font_style_range of Properties.font_style * Properties.font_style
      (** variable font style range, e.g. [normal italic] *)
  | Font_weight of Properties.font_weight  (** normal, bold, 100-900 *)
  | Font_weight_range of Properties.font_weight * Properties.font_weight
      (** variable font weight range, e.g. [100 900] *)
  | Font_stretch of Properties.font_stretch
      (** normal, condensed, expanded, etc. *)
  | Font_stretch_range of string  (** variable font stretch range *)
  | Font_display of Properties.font_display
      (** auto, block, swap, fallback, optional *)
  | Unicode_range of Properties.unicode_range list
      (** CSS Fonts 4 sec. 4.5 comma-separated [unicode-range] list. *)
  | Font_variant of font_variant_descriptor  (** [font-variant] descriptor *)
  | Font_feature_settings of Properties.font_feature_settings
      (** OpenType feature settings *)
  | Font_variation_settings of Properties.font_variation_settings
      (** Variable font settings *)
  | Font_tech of string  (** [font-tech] descriptor *)
  | Size_adjust of Font_face.size_adjust  (** Size adjustment percentage *)
  | Ascent_override of Font_face.metric_override  (** Ascent metric override *)
  | Descent_override of Font_face.metric_override
      (** Descent metric override *)
  | Line_gap_override of Font_face.metric_override
      (** Line gap metric override *)

(** {1 Stylesheet Structure} *)

type stylesheet = statement list
(** A CSS stylesheet is a list of statements *)

type t = stylesheet
(** Alias for backwards compatibility *)

(** {1 Rendering} *)

type mode = Variables | Inline  (** Rendering mode for CSS output *)

let equal_cascade_origin (a : cascade_origin) b = a = b
let equal (a : stylesheet) b = a = b
