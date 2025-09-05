(** Typed CSS construction.

    This library provides types and functions to construct CSS declarations,
    rules and stylesheets. It avoids stringly-typed CSS by keeping close to the
    CSS syntax and specifications.

    The main notions are:
    - A {!type:declaration} is a property/value pair.
    - A {!type:rule} couples a selector with declarations.
    - A {!type:t} is a stylesheet: a sequence of rules and at-rules (see
      {!type:sheet_item}).
    - Values are typed (e.g., {!type:length}, {!type:color}); invalid constructs
      raise [Invalid_argument].

    Minimal example:
    {[
      open Css

      let s =
        stylesheet
          [ Rule (rule ~selector:(Selector.class_ "button")
                     [ background_color (hex "#3b82f6");
                       color (hex "#ffffff");
                       padding (Rem 0.5);
                       border_radius (Rem 0.375);
                       display Inline_block ]);
            Media (media ~condition:"(min-width: 768px)"
                     [ rule ~selector:".button" [ padding (Rem 1.) ] ]) ]
      in
      to_string s
    ]}

    Custom properties:
    {[
      let def, v = var "primary-color" Color (hex "#3b82f6") in
      stylesheet
        [
          Rule (rule ~selector:(Selector.pseudo_class "root") [ def ]);
          Rule (rule ~selector:(Selector.class_ "card") [ color (Var v) ]);
        ]
      |> to_string
    ]}

    Interface
    - Selectors and rules: {!section:css_selectors}, {!section:css_rules},
      {!section:at_rules}
    - Values and declarations: {!section:values}
    - Property groups: see sections below
    - Rendering and optimization: {!section:rendering}, {!section:optimization}

    {b Core Concepts} - Core CSS system setup and construction:
    - {!section:css_selectors} - CSS Selectors
    - {!section:css_rules} - CSS Rules and Stylesheets
    - {!section:at_rules} - At-Rules
    - {!section:stylesheet_construction} - Stylesheet Construction
    - {!section:nesting_helpers} - Nesting Helpers

    {b Declarations} - Core value types and declaration building:
    - {!section:core_types} - Core Types & Calculations
    - {!section:values} - CSS Values & Units

    {b Property Categories} - Organized CSS properties by functionality:
    - {!section:box_model} - Box Model & Sizing
    - {!section:display_positioning} - Display & Positioning
    - {!section:flexbox} - Flexbox Layout
    - {!section:grid} - Grid Layout
    - {!section:logical_properties} - Logical Properties
    - {!section:colors_backgrounds} - Colors & Backgrounds
    - {!section:typography} - Typography
    - {!section:borders_outlines} - Borders & Outlines
    - {!section:transforms_animations} - Transforms & Animations
    - {!section:visual_effects} - Visual Effects
    - {!section:interaction} - User Interaction

    {b Rendering & Optimization} - CSS output and performance:
    - {!section:rendering} - Rendering
    - {!section:optimization} - Optimization

    {b Advanced Features} - Specialized functionality:
    - {!section:vendor_specific} - Vendor Prefixes & Legacy Support
    - {!section:custom_properties} - Custom Properties API

    @see <https://www.w3.org/Style/CSS/specs.en.html> W3C CSS Specifications
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS>
      MDN CSS Documentation *)

(** {1 Core Concepts}

    Core CSS system setup and construction tools for building stylesheets. *)

(** {2:css_selectors CSS Selectors}

    Structured representation of CSS selectors for targeting HTML elements.

    @see <https://www.w3.org/TR/selectors-4/> CSS Selectors Level 4 *)

module Selector : sig
  type t
  (** Abstract type for CSS selectors *)

  type combinator =
    | Descendant  (** Space: "div p" *)
    | Child  (** >: "div > p" *)
    | Next_sibling  (** +: "div + p" *)
    | Subsequent_sibling  (** ~: "div ~ p" *)

  val to_string : ?minify:bool -> t -> string
  (** [to_string ?minify s] is the string representation of [s]. *)

  val element : string -> t
  (** [element name] is an element selector (e.g., "div", "p"). Validates that
      [name] is a valid CSS identifier. Raise [Invalid_argument] if [name]
      violates CSS identifier rules. *)

  val class_ : string -> t
  (** [class_ name] is a class selector (e.g., ".button", ".prose"). Validates
      that [name] is a valid CSS identifier. Raise [Invalid_argument] if [name]
      violates CSS identifier rules. *)

  val id : string -> t
  (** [id name] is an ID selector (e.g., "#header"). Validates that [name] is a
      valid CSS identifier. Raise [Invalid_argument] if [name] violates CSS
      identifier rules. *)

  val universal : t
  (** [universal] is the universal selector "*". *)

  (** Typed attribute selector values *)
  type attribute_match =
    | Presence  (** [attribute] - attribute exists *)
    | Exact of string  (** [attribute="value"] - exact match *)
    | Whitespace_list of string
        (** [attribute~="value"] - whitespace-separated list *)
    | Hyphen_list of string  (** [attribute|="value"] - hyphen-separated list *)
    | Prefix of string  (** [attribute^="value"] - prefix match *)
    | Suffix of string  (** [attribute$="value"] - suffix match *)
    | Substring of string  (** [attribute*="value"] - substring match *)

  val attribute : string -> attribute_match -> t
  (** [attribute name match] is a typed attribute selector. Validates that
      [name] is a valid CSS identifier. Raise [Invalid_argument] if [name]
      violates CSS identifier rules. *)

  val pseudo_class : string -> t
  (** [pseudo_class name] is a pseudo-class selector (e.g., ":hover",
      ":first-child"). Validates that [name] is a valid CSS identifier. Raise
      [Invalid_argument] if [name] violates CSS identifier rules. *)

  val pseudo_element : string -> t
  (** [pseudo_element name] is a pseudo-element selector (e.g., "::before",
      "::marker"). Validates that [name] is a valid CSS identifier. Raise
      [Invalid_argument] if [name] violates CSS identifier rules. *)

  val combine : t -> combinator -> t -> t
  (** [combine a c b] is [a] combined with [b] using combinator [c]. *)

  val ( ++ ) : t -> t -> t
  (** [sel1 ++ sel2] is [sel1] and [sel2] combined with the descendant
      combinator (space). Equivalent to [combine sel1 Descendant sel2]. *)

  val ( >> ) : t -> t -> t
  (** [sel1 >> sel2] is [sel1] and [sel2] combined with the child combinator
      (>). Equivalent to [combine sel1 Child sel2]. *)

  val where : t list -> t
  (** [where sels] is a [:where()] functional pseudo-class. *)

  val not : t list -> t
  (** [not sels] is a [:not()] functional pseudo-class. *)

  val fun_ : string -> t list -> t
  (** [fun_ name sels] is a functional pseudo-class like [:is()], [:has()], etc.
  *)

  val list : t list -> t
  (** [list sels] is a comma-separated selector list. *)

  val is_compound_list : t -> bool
  (** [is_compound_list s] is [true] if [s] is already a comma-separated list of
      selectors. *)

  val compound : t list -> t
  (** [compound selectors] is a compound selector made by combining multiple
      simple selectors (e.g., element + attribute: "div[class=foo]"). *)

  val ( && ) : t -> t -> t
  (** [sel1 && sel2] is a compound selector of two simple selectors. Equivalent
      to [compound [sel1; sel2]]. Example: [element "div" && class_ "foo"]. *)

  val ( || ) : t -> t -> t
  (** [sel1 || sel2] is a comma-separated selector list (OR semantics).
      Equivalent to [list [sel1; sel2]]. Example:
      [element "h1" || element "h2"]. *)
end

(** {2:css_rules CSS Rules and Stylesheets}

    Core building blocks for CSS rules and stylesheet construction.

    @see <https://www.w3.org/TR/css-syntax-3/> CSS Syntax Module Level 3 *)

type rule
(** Abstract type for CSS rules (selector + declarations). *)

type declaration
(** Abstract type for CSS declarations (property-value pairs). *)

val rule : selector:Selector.t -> declaration list -> rule
(** [rule ~selector declarations] is a CSS rule with the given selector and
    declarations. *)

type nested_rule
(** Abstract type for rules that can be nested in at-rules. *)

type t
(** Abstract type for CSS stylesheets. *)

val selector : rule -> Selector.t
(** [selector rule] is the selector of [rule]. *)

val declarations : rule -> declaration list
(** [declarations rule] is the list of declarations in [rule]. *)

(** {2:at_rules At-Rules}

    At-rules are CSS statements that instruct CSS how to behave. They begin with
    an at sign (@) followed by an identifier and include everything up to the
    next semicolon or CSS block.

    @see <https://www.w3.org/TR/css-conditional-3/>
      CSS Conditional Rules Module Level 3
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/At-rule> MDN At-rules
*)

type media_rule
(** Abstract type for [@media] rules. Media queries apply styles based on device
    characteristics. *)

type supports_rule
(** Abstract type for [@supports] rules. Feature queries apply styles based on
    CSS feature support. *)

type container_rule
(** Abstract type for [@container] rules. Container queries apply styles based
    on containing element size. *)

type layer_rule
(** Abstract type for [@layer] rules. Cascade layers control style precedence.
*)

type property_rule
(** Abstract type for [@property] rules. Property rules register custom CSS
    properties with type checking and constraints. *)

type starting_style_rule
(** Abstract type for [@starting-style] rules. Starting style rules define
    initial styles for animating elements. *)

val media : condition:string -> rule list -> media_rule
(** [media ~condition rules] is a [@media] rule.
    @param condition Media query condition (e.g., "(min-width: 768px)"). *)

val media_condition : media_rule -> string
(** [media_condition media] is the condition string of [media]. *)

val media_rules : media_rule -> rule list
(** [media_rules media] is the list of rules contained in [media]. *)

val supports : condition:string -> rule list -> supports_rule
(** [supports ~condition rules] is a [@supports] rule for feature queries.
    @param condition Feature query condition (e.g., "(display: grid)"). *)

val supports_nested :
  condition:string -> rule list -> supports_rule list -> supports_rule
(** [supports_nested ~condition rules nested_supports] is a [@supports] rule
    with nested [@supports] rules. *)

val container :
  ?name:string option -> condition:string -> rule list -> container_rule
(** [container ?name ~condition rules] is a [\@container] rule.
    @param name Optional container name.
    @param condition Container query condition (e.g., "(min-width: 700px)"). *)

val layer :
  name:string ->
  ?media:media_rule list ->
  ?container:container_rule list ->
  ?supports:supports_rule list ->
  nested_rule list ->
  layer_rule
(** [layer ~name ?media ?container ?supports rules] is a [\@layer] rule.
    @param name Layer name.
    @param media Optional nested [@media] rules.
    @param container Optional nested [@container] rules.
    @param supports Optional nested [@supports] rules.

    {b CSS Cascade Layer Ordering:} Layers are applied in the order they are
    declared. The standard layer order is: 1. [theme] - CSS custom properties
    and design tokens 2. [properties] - [@property] rules for custom property
    registration 3. [base] - Normalize/reset styles and base element styling 4.
    [components] - Reusable component styles 5. [utilities] - Single-purpose
    utility classes

    Later layers override earlier layers, and unlayered styles have the highest
    priority.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/@layer> *)

val layer_name : layer_rule -> string
(** [layer_name layer] is the name of [layer]. *)

val layer_rules : layer_rule -> nested_rule list
(** [layer_rules layer] is the list of rules contained in [layer]. *)

val property :
  syntax:string ->
  ?initial_value:string ->
  ?inherits:bool ->
  string ->
  property_rule
(** [property ~syntax ?initial_value ?inherits name] is a [\@property] rule to
    register a custom CSS property.
    @param syntax Property syntax (e.g., "<color>").
    @param initial_value Optional initial value.
    @param inherits Whether the property inherits (default: false).
    @param name Property name (including --). *)

val property_rule_name : property_rule -> string
(** [property_rule_name rule] is the property name of [rule]. *)

val property_rule_initial : property_rule -> string option
(** [property_rule_initial rule] is the initial value of [rule], if any. *)

val default_decl_of_property_rule : property_rule -> declaration
(** [default_decl_of_property_rule rule] is the custom property declaration for
    [rule] with its initial value. Useful to add defaults in the properties
    layer. *)

(** {2:stylesheet_construction Stylesheet Construction}

    Tools for building complete CSS stylesheets from rules and declarations. *)

(** Items that can appear at the top level of a stylesheet. *)
type sheet_item =
  | Rule of rule  (** Regular CSS rule *)
  | Media of media_rule  (** [\@media] at-rule *)
  | Supports of supports_rule  (** [@supports] at-rule *)
  | Container of container_rule  (** [\@container] at-rule *)
  | Layer of layer_rule  (** [\@layer] at-rule *)
  | Property of property_rule  (** [\@property] at-rule *)
  | Starting_style of starting_style_rule  (** [\@starting-style] at-rule *)

val empty : t
(** [empty] is an empty stylesheet. *)

val concat : t list -> t
(** [concat stylesheets] is [stylesheets] concatenated into one. Rules are
    combined in order, with later stylesheets taking precedence. *)

val stylesheet : sheet_item list -> t
(** [stylesheet items] is a stylesheet from a list of items. *)

val stylesheet_items : t -> sheet_item list
(** [stylesheet_items t] is the list of top-level items in [t]. *)

val stylesheet_layers : t -> layer_rule list
(** [stylesheet_layers stylesheet] is the list of layer rules of [stylesheet].
*)

val stylesheet_media_queries : t -> media_rule list
(** [stylesheet_media_queries stylesheet] is the list of media queries of
    [stylesheet]. *)

val stylesheet_container_queries : t -> container_rule list
(** [stylesheet_container_queries stylesheet] is the list of container queries
    of [stylesheet]. *)

val merge_rules : rule list -> rule list
(** [merge_rules rules] merges consecutive rules with identical selectors,
    combining their declarations. Preserves CSS cascade order. *)

val combine_identical_rules : rule list -> rule list
(** [combine_identical_rules rules] combines rules with identical declarations
    into comma-separated selectors. Only combines consecutive rules to preserve
    CSS cascade semantics. *)

(** {2:nesting_helpers Nesting Helpers}

    Utilities for CSS nesting and hierarchical rule organization. *)

val rule_to_nested : rule -> nested_rule
(** [rule_to_nested rule] is [rule] as a [nested_rule] for use in at-rules. *)

val supports_to_nested : supports_rule -> nested_rule
(** [supports_to_nested supports] is [supports] as a [nested_rule]. *)

(** {1 Declarations}

    Core value types and declaration building blocks. *)

(** {2:core_types Core Types & Calculations}

    Fundamental types for CSS values, variables, and calculations that underpin
    the entire CSS system.

    @see <https://www.w3.org/TR/css-variables-1/>
      CSS Custom Properties for Cascading Variables Module Level 1
    @see <https://www.w3.org/TR/css-values-3/>
      CSS Values and Units Module Level 3 *)

type 'a var
(** The type of CSS variable holding values of type ['a]. *)

val var_name : 'a var -> string
(** [var_name v] is [v]'s variable name (without --). *)

val var_layer : 'a var -> string option
(** [var_layer v] is the optional layer where [v] is defined. *)

(** CSS generation mode. *)
type mode =
  | Variables  (** Emit var(--name) and generate a theme layer *)
  | Inline  (** Resolve vars to default, no CSS variables *)

(** CSS calc operations. *)
type calc_op = Add | Sub | Mult | Div

(** CSS calc values. *)
type 'a calc =
  | Var of 'a var  (** CSS variable *)
  | Val of 'a
  | Expr of 'a calc * calc_op * 'a calc

(** {2:values CSS Values & Units}

    Core value types used across CSS properties.

    @see <https://www.w3.org/TR/css-values-4/>
      CSS Values and Units Module Level 4 *)

(** CSS length values.

    Supports absolute units (px), relative units (rem, em, %, viewport units),
    character-based units (ch, lh), keywords, and calculated expressions. *)
type length =
  | Px of int
  | Rem of float
  | Em of float
  | Pct of float
  | Vw of float
  | Vh of float
  | Ch of float  (** Character units *)
  | Lh of float  (** Line height units *)
  | Num of float  (** Unitless numbers, e.g., line-height multipliers *)
  | Auto
  | Zero
  | Inherit
  | Fit_content  (** fit-content keyword *)
  | Max_content  (** max-content keyword *)
  | Min_content  (** min-content keyword *)
  | From_font  (** from-font keyword for text-decoration-thickness *)
  | Var of length var  (** CSS variable reference *)
  | Calc of length calc  (** Calculated expressions *)

(** Builder functions for calc() expressions. *)
module Calc : sig
  val add : 'a calc -> 'a calc -> 'a calc
  (** [add left right] is [left + right]. *)

  val sub : 'a calc -> 'a calc -> 'a calc
  (** [sub left right] is [left - right]. *)

  val mul : 'a calc -> 'a calc -> 'a calc
  (** [mul left right] is [left * right]. *)

  val div : 'a calc -> 'a calc -> 'a calc
  (** [div left right] is [left / right]. *)

  val ( + ) : 'a calc -> 'a calc -> 'a calc
  (** [(+)] is {!add}. *)

  val ( - ) : 'a calc -> 'a calc -> 'a calc
  (** [(-)] is {!sub}. *)

  val ( * ) : 'a calc -> 'a calc -> 'a calc
  (** [( * )] is {!mul}. *)

  val ( / ) : 'a calc -> 'a calc -> 'a calc
  (** [(/)] is {!div}. *)

  val length : length -> length calc
  (** [length len] is [len] lifted into [calc]. *)

  val var : ?default:'a -> ?fallback:'a -> string -> 'a calc
  (** [var ?default ?fallback name] is a variable reference for [calc]
      expressions. Example: [var "spacing"] or
      [var ~fallback:(Rem 1.2) "tw-leading"]. *)

  val float : float -> length calc
  (** [float f] is a numeric value for [calc] expressions. *)

  val infinity : length calc
  (** [infinity] is the CSS infinity value for [calc] expressions. *)

  val px : int -> length calc
  (** [px n] is a pixel value for [calc] expressions. *)

  val rem : float -> length calc
  (** [rem f] is a rem value for [calc] expressions. *)

  val em : float -> length calc
  (** [em f] is an em value for [calc] expressions. *)

  val pct : float -> length calc
  (** [pct f] is a percentage value for [calc] expressions. *)
end

type _ property
(** GADT for typed CSS properties. *)

(** CSS color spaces for color-mix() *)
type color_space =
  | Srgb
  | Srgb_linear
  | Display_p3
  | A98_rgb
  | Prophoto_rgb
  | Rec2020
  | Lab
  | Oklab
  | Xyz
  | Xyz_d50
  | Xyz_d65
  | Lch
  | Oklch
  | Hsl
  | Hwb

(** CSS named colors as defined in the CSS Color Module specification. *)
type color_name =
  | Red
  | Blue
  | Green
  | White
  | Black
  | Yellow
  | Cyan
  | Magenta
  | Gray
  | Grey
  | Orange
  | Purple
  | Pink
  | Silver
  | Maroon
  | Fuchsia
  | Lime
  | Olive
  | Navy
  | Teal
  | Aqua

(** CSS color values. *)
type color =
  | Hex of { hash : bool; value : string }
      (** hash indicates if # was present *)
  | Rgb of { r : int; g : int; b : int }
  | Rgba of { r : int; g : int; b : int; a : float }
  | Oklch of { l : float; c : float; h : float }  (** OKLCH color space *)
  | Named of color_name  (** Named colors like Red, Blue, etc. *)
  | Var of color var
  | Current
  | Transparent
  | Inherit
  | Mix of {
      in_space : color_space;  (** Color space for mixing *)
      color1 : color;  (** First color *)
      percent1 : int option;  (** Optional percentage for first color *)
      color2 : color;  (** Second color *)
      percent2 : int option;  (** Optional percentage for second color *)
    }

val hex : string -> color
(** [hex s] is a hexadecimal color. Accepts with or without leading [#].
    Examples: [hex "#3b82f6"], [hex "ffffff"]. *)

val rgb : int -> int -> int -> color
(** [rgb r g b] is an RGB color (0–255 components). *)

val rgba : int -> int -> int -> float -> color
(** [rgba r g b a] is an RGBA color with alpha in [0., 1.]. *)

val oklch : float -> float -> float -> color
(** [oklch l c h] is an OKLCH color. *)

val named : color_name -> color
(** [named n] is a named color. *)

val current_color : color
(** [current_color] is the CSS [currentcolor] value. *)

val transparent : color
(** [transparent] is the CSS [transparent] value. *)

val color_mix :
  ?in_space:color_space ->
  ?percent1:int ->
  ?percent2:int ->
  color ->
  color ->
  color
(** [color_mix ?in_space ?percent1 ?percent2 c1 c2] is a color-mix value.
    Defaults: [in_space = Srgb], [percent1 = None], [percent2 = None]. *)

(** CSS angle values *)
type angle =
  | Deg of float
  | Rad of float
  | Turn of float
  | Grad of float
  | Var of angle var  (** CSS variable reference *)

(** CSS aspect-ratio values *)
type aspect_ratio = Auto | Ratio of float * float | Inherit

(** CSS blend-mode values *)
type blend_mode =
  | Normal
  | Multiply
  | Screen
  | Overlay
  | Darken
  | Lighten
  | Color_dodge
  | Color_burn
  | Hard_light
  | Soft_light
  | Difference
  | Exclusion
  | Hue
  | Saturation
  | Color
  | Luminosity

(** CSS font-feature-settings values *)
type font_feature_settings =
  | Normal
  | Feature_list of string
  | Inherit
  | String of string
  | Var of font_feature_settings var

(** CSS font-variation-settings values *)
type font_variation_settings =
  | Normal
  | Axis_list of string
  | Inherit
  | String of string
  | Var of font_variation_settings var

val important : declaration -> declaration
(** [important decl] is [decl] marked as [!important]. *)

(** {1 Property Categories}

    CSS properties organized by functionality and usage patterns. *)

(** {2:box_model Box Model & Sizing}

    The CSS Box Model defines how element dimensions are calculated and how
    space is distributed around content. This includes width/height properties,
    padding, margins, and box sizing behavior.

    @see <https://www.w3.org/TR/css-box-3/> CSS Box Model Module Level 3
    @see <https://www.w3.org/TR/css-sizing-3/>
      CSS Intrinsic & Extrinsic Sizing Module Level 3 *)

(** CSS box sizing values. *)
type box_sizing = Border_box | Content_box | Inherit

val width : length -> declaration
(** [width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/width} width} property.
*)

val height : length -> declaration
(** [height len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/height} height}
    property. *)

val min_width : length -> declaration
(** [min_width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/min-width} min-width}
    property. *)

val max_width : length -> declaration
(** [max_width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/max-width} max-width}
    property. *)

val min_height : length -> declaration
(** [min_height len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/min-height} min-height}
    property. *)

val max_height : length -> declaration
(** [max_height len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/max-height} max-height}
    property. *)

val padding : length -> declaration
(** [padding len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding} padding}
    property for all sides. *)

val padding_top : length -> declaration
(** [padding_top len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-top}
     padding-top} property. *)

val padding_right : length -> declaration
(** [padding_right len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-right}
     padding-right} property. *)

val padding_bottom : length -> declaration
(** [padding_bottom len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-bottom}
     padding-bottom} property. *)

val padding_left : length -> declaration
(** [padding_left len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-left}
     padding-left} property. *)

val margin : length -> declaration
(** [margin len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin} margin} property
    for all sides. *)

val margin_top : length -> declaration
(** [margin_top len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin-top} margin-top}
    property. *)

val margin_right : length -> declaration
(** [margin_right len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin-right}
     margin-right} property. *)

val margin_bottom : length -> declaration
(** [margin_bottom len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin-bottom}
     margin-bottom} property. *)

val margin_left : length -> declaration
(** [margin_left len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin-left}
     margin-left} property. *)

val box_sizing : box_sizing -> declaration
(** [box_sizing value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/box-sizing} box-sizing}
    property. *)

val aspect_ratio : aspect_ratio -> declaration
(** [aspect_ratio value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/aspect-ratio}
     aspect-ratio} property. *)

(** {2:logical_properties Logical Properties}

    CSS Logical Properties provide writing-mode-relative property equivalents
    for physical properties. These adapt to different writing directions and
    text orientations.

    @see <https://www.w3.org/TR/css-logical-1/>
      CSS Logical Properties and Values Level 1 *)

val border_inline_start_width : length -> declaration
(** [border_inline_start_width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-start-width}
     border-inline-start-width} property. *)

val border_inline_end_width : length -> declaration
(** [border_inline_end_width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-end-width}
     border-inline-end-width} property. *)

val border_inline_start_color : color -> declaration
(** [border_inline_start_color c] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-start-color}
     border-inline-start-color} property. *)

val border_inline_end_color : color -> declaration
(** [border_inline_end_color c] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-end-color}
     border-inline-end-color} property. *)

val padding_inline_start : length -> declaration
(** [padding_inline_start value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-inline-start}
     padding-inline-start} property. *)

val padding_inline_end : length -> declaration
(** [padding_inline_end value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-inline-end}
     padding-inline-end} property. *)

val padding_inline : length -> declaration
(** [padding_inline value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-inline}
     padding-inline} shorthand property. *)

val padding_block : length -> declaration
(** [padding_block value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-block}
     padding-block} shorthand property. *)

val margin_inline : length -> declaration
(** [margin_inline len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin-inline}
     margin-inline} property with a length value. *)

val margin_block : length -> declaration
(** [margin_block len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin-block}
     margin-block} property with a length value. *)

val margin_inline_end : length -> declaration
(** [margin_inline_end len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin-inline-end}
     margin-inline-end} property. *)

(** {2:display_positioning Display & Positioning}

    Controls how elements are displayed and positioned in the document flow.
    This includes the display model, positioning schemes, and stacking context.

    @see <https://www.w3.org/TR/css-display-3/> CSS Display Module Level 3
    @see <https://www.w3.org/TR/css-position-3/>
      CSS Positioned Layout Module Level 3 *)

(** CSS display values. *)
type display =
  | Block
  | Inline
  | Inline_block
  | Flex
  | Inline_flex
  | Grid
  | Inline_grid
  | None
  | Flow_root
  | Table
  | Table_row
  | Table_cell
  | Table_caption
  | Table_column
  | Table_column_group
  | Table_footer_group
  | Table_header_group
  | Table_row_group
  | Inline_table
  | List_item
  | Contents
  | Webkit_box

(** CSS position values. *)
type position = Static | Relative | Absolute | Fixed | Sticky

(** CSS visibility values. *)
type visibility = Visible | Hidden | Collapse

(** CSS z-index values. *)
type z_index = Auto | Index of int

(** CSS overflow values. *)
type overflow = Visible | Hidden | Scroll | Auto | Clip

val display : display -> declaration
(** [display d] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/display} display}
    property. *)

val position : position -> declaration
(** [position p] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/position} position}
    property. *)

val top : length -> declaration
(** [top len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/top} top} property for
    positioned elements. *)

val right : length -> declaration
(** [right len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/right} right} property
    for positioned elements. *)

val bottom : length -> declaration
(** [bottom len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/bottom} bottom} property
    for positioned elements. *)

val left : length -> declaration
(** [left len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/left} left} property for
    positioned elements. *)

val z_index : z_index -> declaration
(** [z_index value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/z-index} z-index}
    property. *)

val z_index_auto : declaration
(** [z_index_auto] is the CSS z-index property set to [auto]. *)

(** CSS isolation values *)
type isolation = Auto | Isolate | Inherit

val isolation : isolation -> declaration
(** [isolation value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/isolation} isolation}
    property for stacking context control. *)

val visibility : visibility -> declaration
(** [visibility v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/visibility} visibility}
    property. *)

(** CSS float side values. *)
type float_side = None | Left | Right

val float : float_side -> declaration
(** [float value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/float} float} property.
*)

(* Clear property values *)
type clear = None | Left | Right | Both

val clear : clear -> declaration
(** [clear value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/clear} clear} property.
*)

val overflow : overflow -> declaration
(** [overflow value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overflow} overflow}
    property. *)

val overflow_x : overflow -> declaration
(** [overflow_x value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-x} overflow-x}
    property. *)

val overflow_y : overflow -> declaration
(** [overflow_y value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-y} overflow-y}
    property. *)

(** CSS content values *)
type content =
  | String of string
  | None
  | Normal
  | Open_quote
  | Close_quote
  | Var of content var

val content : content -> declaration
(** [content c] is the CSS content property. *)

(** CSS object-fit values *)
type object_fit = Fill | Contain | Cover | None | Scale_down | Inherit

val object_fit : object_fit -> declaration
(** [object_fit value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/object-fit} object-fit}
    property. *)

val object_position : string -> declaration
(** [object_position value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/object-position}
     object-position} property. *)

(** CSS text-overflow values *)
type text_overflow = Clip | Ellipsis | String of string | Inherit

val text_overflow : text_overflow -> declaration
(** [text_overflow value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-overflow}
     text-overflow} property. *)

(** CSS text-wrap values *)
type text_wrap = Wrap | No_wrap | Balance | Pretty | Inherit

val text_wrap : text_wrap -> declaration
(** [text_wrap value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-wrap} text-wrap}
    property. *)

(** CSS backface-visibility values *)
type backface_visibility = Visible | Hidden | Inherit

val backface_visibility : backface_visibility -> declaration
(** [backface_visibility value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/backface-visibility}
     backface-visibility} property (3D transforms). *)

(** CSS content-visibility values. *)
type content_visibility =
  | Visible  (** Content is visible and rendered *)
  | Hidden  (** Content is hidden from rendering *)
  | Auto  (** Browser determines visibility based on relevance *)
  | Inherit  (** Inherit from parent *)
  | Var of content_visibility var  (** CSS variable reference *)

val content_visibility : content_visibility -> declaration
(** [content_visibility value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/content-visibility}
     content-visibility} property. *)

(** CSS list-style-position values *)
type list_style_position = Inside | Outside | Inherit

val list_style_position : list_style_position -> declaration
(** [list_style_position pos] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/list-style-position}
     list-style-position} property. *)

(** {2:colors_backgrounds Colors & Backgrounds}

    Properties for controlling foreground colors, background colors, images, and
    related visual styling for element backgrounds.

    @see <https://www.w3.org/TR/css-color-4/> CSS Color Module Level 4
    @see <https://www.w3.org/TR/css-backgrounds-3/>
      CSS Backgrounds and Borders Module Level 3 *)

(** CSS forced-color-adjust values. *)
type forced_color_adjust = Auto | None | Inherit

(** CSS background-repeat values. *)
type background_repeat =
  | Repeat
  | Repeat_x
  | Repeat_y
  | No_repeat
  | Space
  | Round
  | Inherit

(** CSS background-size values. *)
type background_size =
  | Auto
  | Cover
  | Contain
  | Length of length
  | Percentage of float
  | Size of length * length
  | Inherit

(** CSS background-attachment values. *)
type background_attachment = Scroll | Fixed | Local | Inherit

(** Gradient direction values *)
type gradient_direction =
  | To_top
  | To_top_right
  | To_right
  | To_bottom_right
  | To_bottom
  | To_bottom_left
  | To_left
  | To_top_left
  | Angle of angle

(** Gradient stop values *)
type gradient_stop =
  | Color_stop of color
  | Color_position of color * length
  | Var of color var  (** Reference to a color variable *)
  | Computed_stops of string
      (** For complex computed values like --tw-gradient-stops *)

(** Background image values *)
type background_image =
  | Url of string
  | Linear_gradient of gradient_direction * gradient_stop list
  | Radial_gradient of gradient_stop list
  | None

val color : color -> declaration
(** [color value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/color} color} property.
*)

val background_color : color -> declaration
(** [background_color color] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-color}
     background-color} property. *)

val background_image : background_image -> declaration
(** [background_image value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-image}
     background-image} property. *)

val background_position : string -> declaration
(** [background_position value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-position}
     background-position} property. *)

val background_size : background_size -> declaration
(** [background_size value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-size}
     background-size} property. *)

val background_repeat : background_repeat -> declaration
(** [background_repeat value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-repeat}
     background-repeat} property. *)

val background_attachment : background_attachment -> declaration
(** [background_attachment value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-attachment}
     background-attachment} property. *)

val opacity : float -> declaration
(** [opacity value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/opacity} opacity}
    property. *)

val url : string -> background_image
(** [url path] is a URL background image value. *)

val linear_gradient :
  gradient_direction -> gradient_stop list -> background_image
(** [linear_gradient dir stops] is a linear gradient background. *)

val radial_gradient : gradient_stop list -> background_image
(** [radial_gradient stops] is a radial gradient background. *)

val color_stop : color -> gradient_stop
(** [color_stop c] is a simple color stop. *)

val color_position : color -> length -> gradient_stop
(** [color_position c pos] is a color stop at a specific position. *)

(** {2:flexbox Flexbox Layout}

    Properties for CSS Flexible Box Layout, a one-dimensional layout method for
    distributing space between items and providing alignment capabilities.

    @see <https://www.w3.org/TR/css-flexbox-1/>
      CSS Flexible Box Layout Module Level 1 *)

(** CSS flex direction values. *)
type flex_direction = Row | Row_reverse | Column | Column_reverse

(** CSS flex wrap values. *)
type flex_wrap = Nowrap | Wrap | Wrap_reverse

(** CSS flex shorthand values. *)
type flex =
  | Initial  (** 0 1 auto *)
  | Auto  (** 1 1 auto *)
  | None  (** 0 0 auto *)
  | Grow of float  (** Single grow value *)
  | Basis of length  (** 1 1 <length> *)
  | Grow_shrink of float * float  (** grow shrink 0% *)
  | Full of float * float * length  (** grow shrink basis *)

(** CSS align-items values. *)
type align_items =
  | Normal
  | Stretch
  | Center
  | Start
  | End
  | Self_start
  | Self_end
  | Flex_start
  | Flex_end
  | Baseline
  | First_baseline
  | Last_baseline
  | Safe_center
  | Unsafe_center
  | Inherit_align
  | Initial
  | Unset
  | Revert
  | Revert_layer

(** CSS justify-content values. *)
type justify_content =
  | Normal
  | Flex_start
  | Flex_end
  | Center
  | Space_between
  | Space_around
  | Space_evenly
  | Stretch
  | Start
  | End
  | Left
  | Right

(** CSS align-self values. *)
type align_self = Auto | Flex_start | Flex_end | Center | Baseline | Stretch

(** CSS justify values (used by justify-self, justify-items, etc.). *)
type justify = Auto | Start | End | Center | Stretch | Flex_start | Flex_end

(** CSS align/justify values. *)
type align =
  | Flex_start
  | Flex_end
  | Center
  | Space_between
  | Space_around
  | Space_evenly
  | Stretch
  | Start
  | End
  | Baseline
  | Auto

val flex_direction : flex_direction -> declaration
(** [flex_direction value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex-direction}
     flex-direction} property. *)

val flex_wrap : flex_wrap -> declaration
(** [flex_wrap value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex-wrap} flex-wrap}
    property. *)

val justify_content : justify_content -> declaration
(** [justify_content value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-content}
     justify-content} property. *)

val align_items : align_items -> declaration
(** [align_items value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/align-items}
     align-items} property. *)

val align_content : align -> declaration
(** [align_content value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/align-content}
     align-content} property. *)

val flex : flex -> declaration
(** [flex value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex} flex} shorthand
    property. *)

val flex_grow : float -> declaration
(** [flex_grow value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex-grow} flex-grow}
    property. *)

val flex_shrink : float -> declaration
(** [flex_shrink value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex-shrink}
     flex-shrink} property. *)

val flex_basis : length -> declaration
(** [flex_basis value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex-basis} flex-basis}
    property. *)

val align_self : align_self -> declaration
(** [align_self value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/align-self} align-self}
    property. *)

val justify_self : justify -> declaration
(** [justify_self value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-self}
     justify-self} property. *)

val justify_items : justify -> declaration
(** [justify_items value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-items}
     justify-items} property. *)

val order : int -> declaration
(** [order value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/order} order} property.
*)

val gap : length -> declaration
(** [gap value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/gap} gap} property
    (applies to both row and column gaps). *)

val row_gap : length -> declaration
(** [row_gap value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/row-gap} row-gap}
    property. *)

val column_gap : length -> declaration
(** [column_gap value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/column-gap} column-gap}
    property. *)

(** {2:grid Grid Layout}

    Properties for CSS Grid Layout, a two-dimensional layout system optimized
    for user interface design with explicit row and column positioning.

    @see <https://www.w3.org/TR/css-grid-1/> CSS Grid Layout Module Level 1
    @see <https://www.w3.org/TR/css-grid-2/> CSS Grid Layout Module Level 2 *)

(** CSS grid track sizing *)
type grid_track_size =
  | Fr of float
  | Min_max of length * grid_track_size
  | Grid_auto
  | Max_content
  | Min_content
  | Fit_content of length
  | Grid_length of length

(** CSS grid template values *)
type grid_template =
  | Tracks of grid_track_size list
  | Repeat of int * grid_track_size
  | Repeat_auto_fill of grid_track_size
  | Repeat_auto_fit of grid_track_size
  | None
  | Inherit

(** CSS grid line values *)
type grid_line =
  | Line_number of int  (** 1, 2, 3, ... or -1, -2, ... *)
  | Line_name of string  (** "header-start", "main-end", etc. *)
  | Span of int  (** span 2, span 3, etc. *)
  | Auto  (** auto *)

val grid_template_columns : grid_template -> declaration
(** [grid_template_columns value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template-columns}
     grid-template-columns} property. *)

val grid_template_rows : grid_template -> declaration
(** [grid_template_rows value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template-rows}
     grid-template-rows} property. *)

val grid_template_areas : string -> declaration
(** [grid_template_areas value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template-areas}
     grid-template-areas} property. *)

val grid_template : grid_template -> declaration
(** [grid_template value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template}
     grid-template} shorthand property. *)

val grid_auto_columns : grid_template -> declaration
(** [grid_auto_columns value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-auto-columns}
     grid-auto-columns} property. *)

val grid_auto_rows : grid_template -> declaration
(** [grid_auto_rows value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-auto-rows}
     grid-auto-rows} property. *)

(** CSS grid-auto-flow values *)
type grid_auto_flow = Row | Column | Row_dense | Column_dense

val grid_auto_flow : grid_auto_flow -> declaration
(** [grid_auto_flow value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-auto-flow}
     grid-auto-flow} property. *)

val grid_row_start : grid_line -> declaration
(** [grid_row_start value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-row-start}
     grid-row-start} property. *)

val grid_row_end : grid_line -> declaration
(** [grid_row_end value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-row-end}
     grid-row-end} property. *)

val grid_column_start : grid_line -> declaration
(** [grid_column_start value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-column-start}
     grid-column-start} property. *)

val grid_column_end : grid_line -> declaration
(** [grid_column_end value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-column-end}
     grid-column-end} property. *)

val grid_row : grid_line * grid_line -> declaration
(** [grid_row (start, end)] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-row} grid-row}
    shorthand property. *)

val grid_column : grid_line * grid_line -> declaration
(** [grid_column (start, end)] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-column}
     grid-column} shorthand property. *)

val grid_area : string -> declaration
(** [grid_area value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-area} grid-area}
    property. *)

(** CSS place-items values *)
type place_items =
  | Normal
  | Auto
  | Stretch
  | Center
  | Start
  | End
  | Self_start
  | Self_end
  | Flex_start
  | Flex_end
  | Baseline
  | First_baseline
  | Last_baseline
  | Inherit

val place_items : place_items -> declaration
(** [place_items value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/place-items}
     place-items} shorthand property. *)

(** CSS place-content values *)
type place_content =
  | Start
  | End
  | Center
  | Stretch
  | Space_between
  | Space_around
  | Space_evenly

val place_content : place_content -> declaration
(** [place_content value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/place-content}
     place-content} shorthand property. *)

val place_self : align_self -> declaration
(** [place_self value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/place-self} place-self}
    shorthand property. *)

(** {2:typography Typography}

    Properties for controlling text appearance, fonts, and text layout. This
    includes font properties, text decoration, alignment, and spacing.

    @see <https://www.w3.org/TR/css-fonts-4/> CSS Fonts Module Level 4
    @see <https://www.w3.org/TR/css-text-3/> CSS Text Module Level 3
    @see <https://www.w3.org/TR/css-text-4/> CSS Text Module Level 4 *)

(** CSS font weight values. *)
type font_weight =
  | Weight of int
  | Normal
  | Bold
  | Bolder
  | Lighter
  | Inherit
  | Var of font_weight var  (** CSS variable reference *)

(** CSS text align values. *)
type text_align = Left | Right | Center | Justify | Start | End | Inherit

(** CSS text decoration values. *)
type text_decoration =
  | None
  | Underline
  | Overline
  | Line_through
  | Inherit
  | Underline_dotted  (** underline dotted *)

(** CSS font style values. *)
type font_style = Normal | Italic | Oblique | Inherit

(** CSS text transform values. *)
type text_transform =
  | None
  | Uppercase
  | Lowercase
  | Capitalize
  | Full_width
  | Full_size_kana
  | Inherit

(** CSS font-family values *)
type font_family =
  (* Generic CSS font families *)
  | Sans_serif
  | Serif
  | Monospace
  | Cursive
  | Fantasy
  | System_ui
  | Ui_sans_serif
  | Ui_serif
  | Ui_monospace
  | Ui_rounded
  | Emoji
  | Math
  | Fangsong
  (* Popular web fonts *)
  | Inter
  | Roboto
  | Open_sans
  | Lato
  | Montserrat
  | Poppins
  | Source_sans_pro
  | Raleway
  | Oswald
  | Noto_sans
  | Ubuntu
  | Playfair_display
  | Merriweather
  | Lora
  | PT_sans
  | PT_serif
  | Nunito
  | Nunito_sans
  | Work_sans
  | Rubik
  | Fira_sans
  | Fira_code
  | JetBrains_mono
  | IBM_plex_sans
  | IBM_plex_serif
  | IBM_plex_mono
  | Source_code_pro
  | Space_mono
  | DM_sans
  | DM_serif_display
  | Bebas_neue
  | Barlow
  | Mulish
  | Josefin_sans
  (* Platform-specific fonts *)
  | Helvetica
  | Helvetica_neue
  | Arial
  | Verdana
  | Tahoma
  | Trebuchet_ms
  | Times_new_roman
  | Times
  | Georgia
  | Cambria
  | Garamond
  | Courier_new
  | Courier
  | Lucida_console
  | SF_pro
  | SF_pro_display
  | SF_pro_text
  | SF_mono
  | NY
  | Segoe_ui
  | Segoe_ui_emoji
  | Segoe_ui_symbol
  | Apple_color_emoji
  | Noto_color_emoji
  | Android_emoji
  | Twemoji_mozilla
  (* Developer fonts *)
  | Menlo
  | Monaco
  | Consolas
  | Liberation_mono
  | SFMono_regular
  | Cascadia_code
  | Cascadia_mono
  | Victor_mono
  | Inconsolata
  | Hack
  (* CSS keywords *)
  | Inherit
  | Initial
  | Unset
  (* CSS variables *)
  | Var of font_family list var

val font_family : font_family list -> declaration
(** [font_family fonts] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-family}
     font-family} property. *)

val font_size : length -> declaration
(** [font_size value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-size} font-size}
    property. *)

val font_weight : font_weight -> declaration
(** [font_weight value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-weight}
     font-weight} property. *)

val font_style : font_style -> declaration
(** [font_style value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-style} font-style}
    property. *)

(** CSS line-height values *)
type line_height =
  | Normal
  | Length of length
  | Number of float
  | Percentage of float
  | Inherit
  | Var of line_height var

val line_height : line_height -> declaration
(** [line_height value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/line-height}
     line-height} property. Accepts [Normal], Length values (e.g., `Length (Rem
    1.5)`), Number values (e.g., `Number 1.5`), or Percentage values. *)

val letter_spacing : length -> declaration
(** [letter_spacing value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/letter-spacing}
     letter-spacing} property. *)

val word_spacing : length -> declaration
(** [word_spacing value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/word-spacing}
     word-spacing} property. *)

val text_align : text_align -> declaration
(** [text_align value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-align} text-align}
    property. *)

val text_decoration : text_decoration -> declaration
(** [text_decoration value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration}
     text-decoration} property. *)

val text_transform : text_transform -> declaration
(** [text_transform value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-transform}
     text-transform} property. *)

val text_indent : length -> declaration
(** [text_indent value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-indent}
     text-indent} property. *)

(** CSS white-space values *)
type white_space =
  | Normal
  | Nowrap
  | Pre
  | Pre_wrap
  | Pre_line
  | Break_spaces
  | Inherit

val white_space : white_space -> declaration
(** [white_space value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/white-space}
     white-space} property. *)

(** CSS word-break values *)
type word_break = Normal | Break_all | Keep_all | Break_word | Inherit

val word_break : word_break -> declaration
(** [word_break value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/word-break} word-break}
    property. *)

val text_decoration_color : color -> declaration
(** [text_decoration_color value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-color}
     text-decoration-color} property. *)

val text_size_adjust : string -> declaration
(** [text_size_adjust value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-size-adjust}
     text-size-adjust} property. *)

(** CSS text-decoration-style values *)
type text_decoration_style = Solid | Double | Dotted | Dashed | Wavy | Inherit

val text_decoration_style : text_decoration_style -> declaration
(** [text_decoration_style value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-style}
     text-decoration-style} property. *)

val text_underline_offset : string -> declaration
(** [text_underline_offset value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-underline-offset}
     text-underline-offset} property. *)

(** CSS overflow-wrap values *)
type overflow_wrap = Normal | Break_word | Anywhere | Inherit

val overflow_wrap : overflow_wrap -> declaration
(** [overflow_wrap value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-wrap}
     overflow-wrap} property. *)

(** CSS hyphens values *)
type hyphens = None | Manual | Auto | Inherit

val hyphens : hyphens -> declaration
(** [hyphens value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/hyphens} hyphens}
    property. *)

(** CSS font-stretch values *)
type font_stretch =
  | Pct of float  (** Percentage values from 50% to 200% *)
  | Ultra_condensed
  | Extra_condensed
  | Condensed
  | Semi_condensed
  | Normal
  | Semi_expanded
  | Expanded
  | Extra_expanded
  | Ultra_expanded
  | Inherit

val font_stretch : font_stretch -> declaration
(** [font_stretch value] is the CSS font-stretch property. *)

(** CSS font-variant-numeric token values *)
type font_variant_numeric_token =
  | Normal  (** Reset to normal font variant *)
  | Lining_nums
  | Oldstyle_nums
  | Proportional_nums
  | Tabular_nums
  | Diagonal_fractions
  | Stacked_fractions
  | Ordinal
  | Slashed_zero
  | Var of font_variant_numeric_token var  (** Variable reference *)

(** CSS font-variant-numeric values *)
type font_variant_numeric =
  | Normal
  | Tokens of font_variant_numeric_token list
  | Var of font_variant_numeric var
  | Composed of {
      ordinal : font_variant_numeric_token option;
      slashed_zero : font_variant_numeric_token option;
      numeric_figure : font_variant_numeric_token option;
      numeric_spacing : font_variant_numeric_token option;
      numeric_fraction : font_variant_numeric_token option;
    }

val font_variant_numeric : font_variant_numeric -> declaration
(** list of tokens or composed CSS variables. *)

val font_variant_numeric_tokens :
  font_variant_numeric_token list -> font_variant_numeric
(** [font_variant_numeric_tokens tokens] is a font-variant-numeric value from
    tokens. *)

val font_variant_numeric_composed :
  ?ordinal:font_variant_numeric_token ->
  ?slashed_zero:font_variant_numeric_token ->
  ?numeric_figure:font_variant_numeric_token ->
  ?numeric_spacing:font_variant_numeric_token ->
  ?numeric_fraction:font_variant_numeric_token ->
  unit ->
  font_variant_numeric
(** [font_variant_numeric_composed ...] is a composed font-variant-numeric value
    using CSS variables for style composition. *)

val font_feature_settings : font_feature_settings -> declaration
(** [font_feature_settings value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-feature-settings}
     font-feature-settings} property. *)

(** CSS shadow values *)
type shadow =
  | Shadow of {
      inset : bool;
      h_offset : length;
      v_offset : length;
      blur : length;
      spread : length;
      color : color;
    }
  | Var of shadow var

val shadow :
  ?inset:bool ->
  ?h_offset:length ->
  ?v_offset:length ->
  ?blur:length ->
  ?spread:length ->
  ?color:color ->
  unit ->
  shadow
(** [shadow ?inset ?h_offset ?v_offset ?blur ?spread ?color ()] is a shadow
    value with optional parameters. Defaults: inset=false, h_offset=0px,
    v_offset=0px, blur=0px, spread=0px, color=Transparent *)

val inset_ring_shadow :
  ?h_offset:length ->
  ?v_offset:length ->
  ?blur:length ->
  ?spread:length ->
  ?color:color ->
  unit ->
  shadow
(** [inset_ring_shadow ?h_offset ?v_offset ?blur ?spread ?color ()] is an inset
    shadow value suitable for ring utilities. Defaults: h_offset=0px,
    v_offset=0px, blur=0px, spread=0px, color=Transparent *)

(** CSS text-shadow values *)
type text_shadow =
  | Shadow of shadow
  | Shadows of shadow list  (** Multiple text shadows *)
  | None  (** No shadow *)
  | Var of text_shadow var  (** Composed shadow variable *)

val text_shadow : text_shadow -> declaration
(** [text_shadow value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-shadow}
     text-shadow} property. *)

val font : string -> declaration
(** [font value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font} font} shorthand
    property. *)

(** CSS direction values *)
type direction = Ltr | Rtl | Inherit

val direction : direction -> declaration
(** [direction value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/direction} direction}
    property. *)

(** CSS unicode-bidi values *)
type unicode_bidi =
  | Normal
  | Embed
  | Bidi_override
  | Isolate
  | Isolate_override
  | Plaintext
  | Inherit

val unicode_bidi : unicode_bidi -> declaration
(** [unicode_bidi value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/unicode-bidi}
     unicode-bidi} property. *)

(** CSS writing-mode values *)
type writing_mode = Horizontal_tb | Vertical_rl | Vertical_lr | Inherit

val writing_mode : writing_mode -> declaration
(** [writing_mode value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/writing-mode}
     writing-mode} property. *)

val text_decoration_thickness : length -> declaration
(** [text_decoration_thickness value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-thickness}
     text-decoration-thickness} property. *)

(** CSS text-decoration-skip-ink values *)
type text_decoration_skip_ink = Auto | None | All | Inherit

val text_decoration_skip_ink : text_decoration_skip_ink -> declaration
(** [text_decoration_skip_ink value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-skip-ink}
     text-decoration-skip-ink} property. *)

(** {2:borders_outlines Borders & Outlines}

    Properties for styling element borders, outlines, and related decorative
    features including border radius for rounded corners.

    @see <https://www.w3.org/TR/css-backgrounds-3/>
      CSS Backgrounds and Borders Module Level 3
    @see <https://www.w3.org/TR/css-ui-4/>
      CSS Basic User Interface Module Level 4 *)

(** CSS border style values. *)
type border_style =
  | None
  | Solid
  | Dashed
  | Dotted
  | Double
  | Groove
  | Ridge
  | Inset
  | Outset
  | Hidden
  | Var of border_style var  (** CSS variable reference *)

(** CSS outline style values. *)
type outline_style =
  | None
  | Auto
  | Dotted
  | Dashed
  | Solid
  | Double
  | Groove
  | Ridge
  | Inset
  | Outset
  | Inherit

val border : string -> declaration
(** [border value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border} border}
    shorthand property. *)

val border_width : length -> declaration
(** [border_width value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-width}
     border-width} property. *)

val border_style : border_style -> declaration
(** [border_style value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-style}
     border-style} property. *)

val border_color : color -> declaration
(** [border_color value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-color}
     border-color} property. *)

val border_radius : length -> declaration
(** [border_radius value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-radius}
     border-radius} property. *)

val border_top : string -> declaration
(** [border_top value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-top} border-top}
    property. *)

val border_right : string -> declaration
(** [border_right value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-right}
     border-right} property. *)

val border_bottom : string -> declaration
(** [border_bottom value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom}
     border-bottom} property. *)

val border_left : string -> declaration
(** [border_left value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-left}
     border-left} property. *)

val outline : string -> declaration
(** [outline value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/outline} outline}
    property. *)

val outline_width : length -> declaration
(** [outline_width value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/outline-width}
     outline-width} property. *)

val outline_style : outline_style -> declaration
(** [outline_style value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/outline-style}
     outline-style} property. *)

val outline_color : color -> declaration
(** [outline_color value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/outline-color}
     outline-color} property. *)

val outline_offset : length -> declaration
(** [outline_offset value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/outline-offset}
     outline-offset} property. *)

val border_top_style : border_style -> declaration
(** [border_top_style s] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-style}
     border-top-style} property. *)

val border_right_style : border_style -> declaration
(** [border_right_style s] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-style}
     border-right-style} property. *)

val border_bottom_style : border_style -> declaration
(** [border_bottom_style s] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-style}
     border-bottom-style} property. *)

val border_left_style : border_style -> declaration
(** [border_left_style s] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-style}
     border-left-style} property. *)

val border_left_width : length -> declaration
(** [border_left_width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-width}
     border-left-width} property. *)

val border_top_width : length -> declaration
(** [border_top_width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-width}
     border-top-width} property. *)

val border_right_width : length -> declaration
(** [border_right_width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-width}
     border-right-width} property. *)

val border_bottom_width : length -> declaration
(** [border_bottom_width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-width}
     border-bottom-width} property. *)

val border_top_color : color -> declaration
(** [border_top_color c] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-color}
     border-top-color} property. *)

val border_right_color : color -> declaration
(** [border_right_color c] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-color}
     border-right-color} property. *)

val border_bottom_color : color -> declaration
(** [border_bottom_color c] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-color}
     border-bottom-color} property. *)

val border_left_color : color -> declaration
(** [border_left_color c] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-color}
     border-left-color} property. *)

(** CSS border-collapse values *)
type border_collapse = Collapse | Separate | Inherit

val border_collapse : border_collapse -> declaration
(** [border_collapse value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-collapse}
     border-collapse} property. *)

(** {2:transforms_animations Transforms & Animations}

    Properties for 2D/3D transformations, CSS animations, and transitions. Based
    on multiple CSS specification modules for comprehensive animation support.

    @see <https://www.w3.org/TR/css-transforms-1/> CSS Transforms Module Level 1
    @see <https://www.w3.org/TR/css-animations-1/> CSS Animations Level 1
    @see <https://www.w3.org/TR/css-transitions-1/> CSS Transitions Level 1 *)

(** CSS transform scale values *)
type transform_scale =
  | Num of float
  | Pct of float
  | Var of transform_scale var

(** CSS transform values *)
type transform =
  | Translate of length * length option
  | Translate_x of length
  | Translate_y of length
  | Translate_z of length
  | Translate3d of length * length * length
  | Rotate of angle
  | Rotate_x of angle
  | Rotate_y of angle
  | Rotate_z of angle
  | Rotate3d of float * float * float * angle
  | Scale of transform_scale * transform_scale option
  | Scale_x of transform_scale
  | Scale_y of transform_scale
  | Scale_z of transform_scale
  | Scale3d of transform_scale * transform_scale * transform_scale
  | Skew of angle * angle option
  | Skew_x of angle
  | Skew_y of angle
  | Matrix of (float * float * float * float * float * float)
  | Matrix3d of
      (float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float)
  | Perspective of length
  | None
  | Inherit
  | Var of transform list var

val transform : transform list -> declaration
(** [transform values] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transform} transform}
    property with a list of transformations. *)

val transform_origin : string -> declaration
(** [transform_origin value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transform-origin}
     transform-origin} property. *)

val rotate : angle -> declaration
(** [rotate value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/rotate} rotate}
    property. *)

(** CSS scale property values *)
type scale =
  | String of string
  | Num of float
  | Pct of float
  | None
  | Vars of transform_scale var list

val scale : scale -> declaration
(** [scale value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scale} scale} property.
*)

val perspective : length -> declaration
(** [perspective value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/perspective}
     perspective} property (3D transforms). *)

val perspective_origin : string -> declaration
(** [perspective_origin value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/perspective-origin}
     perspective-origin} property. *)

(** CSS transform-style values *)
type transform_style = Flat | Preserve_3d | Inherit

val transform_style : transform_style -> declaration
(** [transform_style value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transform-style}
     transform-style} property (3D transforms). *)

(** CSS animation timing function values. *)
type timing_function =
  | Ease
  | Linear
  | Ease_in
  | Ease_out
  | Ease_in_out
  | Step_start
  | Step_end
  | Steps of int * [ `Start | `End ]
  | Cubic_bezier of float * float * float * float

(** CSS duration values. *)
type duration =
  | Ms of int  (** milliseconds *)
  | S of float  (** seconds *)
  | Var of duration var  (** CSS variable reference *)

(** CSS transition property values. *)
type transition_property = All | None | Property of string

(** CSS transition values. *)
type transition =
  | Simple of transition_property * duration
  | With_timing of transition_property * duration * timing_function
  | With_delay of transition_property * duration * timing_function * duration
  | Multiple of transition list

val transition : transition -> declaration
(** [transition value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transition} transition}
    property. *)

val transition_timing_function : timing_function -> declaration
(** [transition_timing_function value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transition-timing-function}
     transition-timing-function} property. *)

val transition_duration : duration -> declaration
(** [transition_duration value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transition-duration}
     transition-duration} property. *)

val transition_delay : duration -> declaration
(** [transition_delay value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transition-delay}
     transition-delay} property. *)

(** CSS animation fill mode values *)
type animation_fill_mode = None | Forwards | Backwards | Both | Inherit

(** CSS animation direction values *)
type animation_direction =
  | Normal
  | Reverse
  | Alternate
  | Alternate_reverse
  | Inherit

(** CSS animation play state values *)
type animation_play_state = Running | Paused | Inherit

(** CSS animation iteration count values *)
type animation_iteration_count = Count of int | Infinite | Inherit

type animation = {
  name : string option;
  duration : duration option;
  timing_function : timing_function option;
  delay : duration option;
  iteration_count : animation_iteration_count option;
  direction : animation_direction option;
  fill_mode : animation_fill_mode option;
  play_state : animation_play_state option;
}
(** CSS animation properties *)

val make_animation :
  ?name:string ->
  ?duration:duration ->
  ?timing_function:timing_function ->
  ?delay:duration ->
  ?iteration_count:animation_iteration_count ->
  ?direction:animation_direction ->
  ?fill_mode:animation_fill_mode ->
  ?play_state:animation_play_state ->
  unit ->
  animation
(** [make_animation ()] is an animation record with optional parameters *)

val animation : animation -> declaration
(** [animation props] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation} animation}
    shorthand property. *)

val animation_name : string -> declaration
(** [animation_name name] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-name}
     animation-name} property. *)

val animation_duration : duration -> declaration
(** [animation_duration value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-duration}
     animation-duration} property. *)

val animation_timing_function : timing_function -> declaration
(** [animation_timing_function value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function}
     animation-timing-function} property. *)

val animation_delay : duration -> declaration
(** [animation_delay value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-delay}
     animation-delay} property. *)

val animation_iteration_count : animation_iteration_count -> declaration
(** [animation_iteration_count value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-iteration-count}
     animation-iteration-count} property. *)

val animation_direction : animation_direction -> declaration
(** [animation_direction value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-direction}
     animation-direction} property. *)

val animation_fill_mode : animation_fill_mode -> declaration
(** [animation_fill_mode value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-fill-mode}
     animation-fill-mode} property. *)

val animation_play_state : animation_play_state -> declaration
(** [animation_play_state value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-play-state}
     animation-play-state} property. *)

(** {2:visual_effects Visual Effects}

    Properties for visual effects including shadows, filters, clipping, and
    other advanced rendering features.

    @see <https://www.w3.org/TR/filter-effects-1/> Filter Effects Module Level 1
    @see <https://www.w3.org/TR/css-masking-1/> CSS Masking Module Level 1 *)

(** CSS box-shadow values *)
type box_shadow =
  | Shadow of shadow
  | Shadows of shadow list
  | None
  | Var of box_shadow var

val box_shadow : box_shadow -> declaration
(** [box_shadow values] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/box-shadow} box-shadow}
    property. *)

(** CSS number values (unitless numbers for filters, transforms, etc.) *)
type number =
  | Float of float  (** Floating point number *)
  | Int of int  (** Integer number *)
  | Pct of float  (** Percentage value *)
  | Var of number var  (** CSS variable reference *)

(** CSS filter values *)
type filter =
  | None  (** No filter *)
  | Blur of length  (** blur(px) *)
  | Brightness of number  (** brightness(%) *)
  | Contrast of number  (** contrast(%) *)
  | Drop_shadow of shadow  (** drop-shadow(...) *)
  | Grayscale of number  (** grayscale(%) *)
  | Hue_rotate of angle  (** hue-rotate(deg) *)
  | Invert of number  (** invert(%) *)
  | Opacity of number  (** opacity(%) *)
  | Saturate of number  (** saturate(%) *)
  | Sepia of number  (** sepia(%) *)
  | Url of string  (** url(...) *)
  | List of filter list  (** Multiple filters *)
  | Var of filter var  (** Custom filter variable *)

val filter : filter -> declaration
(** [filter values] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/filter} filter}
    property. *)

val backdrop_filter : filter -> declaration
(** [backdrop_filter values] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/backdrop-filter}
     backdrop-filter} property. *)

val clip : string -> declaration
(** [clip value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/clip} clip} property
    (deprecated). *)

val clip_path : string -> declaration
(** [clip_path value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/clip-path} clip-path}
    property. *)

val mask : string -> declaration
(** [mask value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/mask} mask} property. *)

val mix_blend_mode : blend_mode -> declaration
(** [mix_blend_mode value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/mix-blend-mode}
     mix-blend-mode} property. *)

val background_blend_mode : blend_mode -> declaration
(** [background_blend_mode values] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-blend-mode}
     background-blend-mode} property. *)

(** {2:interaction User Interaction}

    Properties that affect user interaction with elements including cursor
    appearance, user selection behavior, and pointer events.

    @see <https://www.w3.org/TR/css-ui-4/>
      CSS Basic User Interface Module Level 4 *)

(** CSS cursor values. *)
type cursor =
  | Auto
  | Default
  | Pointer
  | Wait
  | Text
  | Move
  | Help
  | Not_allowed
  | None
  | Context_menu
  | Progress
  | Cell
  | Crosshair
  | Vertical_text
  | Alias
  | Copy
  | No_drop
  | Grab
  | Grabbing
  | All_scroll
  | Col_resize
  | Row_resize
  | N_resize
  | E_resize
  | S_resize
  | W_resize
  | Ne_resize
  | Nw_resize
  | Se_resize
  | Sw_resize
  | Ew_resize
  | Ns_resize
  | Nesw_resize
  | Nwse_resize
  | Zoom_in
  | Zoom_out

(** CSS user-select values. *)
type user_select = None | Auto | Text | All | Contain

(** CSS resize values. *)
type resize = None | Both | Horizontal | Vertical | Block | Inline | Inherit

val cursor : cursor -> declaration
(** [cursor value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/cursor} cursor}
    property. *)

(** CSS pointer-events values *)
type pointer_events =
  | Auto
  | None
  | Visible_painted
  | Visible_fill
  | Visible_stroke
  | Visible
  | Painted
  | Fill
  | Stroke
  | All
  | Inherit

val pointer_events : pointer_events -> declaration
(** [pointer_events value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/pointer-events}
     pointer-events} property. *)

val user_select : user_select -> declaration
(** [user_select value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/user-select}
     user-select} property. *)

val resize : resize -> declaration
(** [resize value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/resize} resize}
    property. *)

(** {2:container_containment Container Queries & Containment}

    CSS container queries and containment features for component-based
    responsive design and performance optimization through layout isolation.

    @see <https://www.w3.org/TR/css-contain-3/> CSS Containment Module Level 3
    @see <https://www.w3.org/TR/css-contain-2/> CSS Containment Module Level 2
*)

(** [aspect_ratio value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/aspect-ratio}
     aspect-ratio} property. *)

(** CSS container-type values *)
type container_type = Normal | Size | Inline_size | Inherit

val container_type : container_type -> declaration
(** [container_type value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/container-type}
     container-type} property for container queries. *)

val container_name : string -> declaration
(** [container_name value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/container-name}
     container-name} property. *)

(** CSS contain values *)
type contain =
  | None  (** No containment *)
  | Layout  (** Layout containment *)
  | Style  (** Style containment *)
  | Paint  (** Paint containment *)
  | Size  (** Size containment *)
  | Inline_size  (** Inline-size containment *)
  | Block_size  (** Block-size containment *)
  | Strict  (** Strict containment (layout + style + paint + size) *)
  | Content  (** Content containment (layout + style + paint) *)
  | Inherit  (** Inherit from parent *)
  | Var of contain var  (** CSS variable reference *)

val contain : contain -> declaration
(** [contain value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/contain} contain}
    property. *)

(** {1 Advanced Features}

    Specialized functionality for advanced CSS features and legacy support. *)

(** {2:vendor_specific Vendor-Specific Properties}

    Vendor-prefixed properties for browser compatibility and legacy support.
    These are implementation-specific extensions that may be needed for older
    browsers.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Extensions>
      MDN: CSS Extensions *)

(** CSS webkit-box-orient values. *)
type webkit_box_orient = Horizontal | Vertical | Inherit

(** CSS -webkit-appearance values. *)
type webkit_appearance =
  | None  (** No appearance styling *)
  | Auto  (** Default browser styling *)
  | Button  (** Button appearance *)
  | Textfield  (** Text field appearance *)
  | Menulist  (** Select/dropdown appearance *)
  | Listbox  (** List box appearance *)
  | Checkbox  (** Checkbox appearance *)
  | Radio  (** Radio button appearance *)
  | Push_button  (** Push button appearance *)
  | Square_button  (** Square button appearance *)

(** CSS -webkit-font-smoothing values. *)
type webkit_font_smoothing =
  | Auto
  | None
  | Antialiased
  | Subpixel_antialiased
  | Inherit

(** CSS -moz-osx-font-smoothing values. *)
type moz_osx_font_smoothing = Auto | Grayscale | Inherit

val webkit_appearance : webkit_appearance -> declaration
(** [webkit_appearance value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/-webkit-appearance}
     -webkit-appearance} property. *)

val webkit_font_smoothing : webkit_font_smoothing -> declaration
(** [webkit_font_smoothing value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/-webkit-font-smoothing}
     -webkit-font-smoothing} property. *)

val moz_osx_font_smoothing : moz_osx_font_smoothing -> declaration
(** [moz_osx_font_smoothing value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/-moz-osx-font-smoothing}
     -moz-osx-font-smoothing} property. *)

val webkit_tap_highlight_color : color -> declaration
(** [webkit_tap_highlight_color value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/-webkit-tap-highlight-color}
     -webkit-tap-highlight-color} property. *)

val webkit_text_decoration : text_decoration -> declaration
(** [webkit_text_decoration value] is the -webkit-text-decoration property. *)

val webkit_text_decoration_color : color -> declaration
(** [webkit_text_decoration_color color] is the -webkit-text-decoration-color
    property. *)

val webkit_line_clamp : int -> declaration
val webkit_box_orient : webkit_box_orient -> declaration

val webkit_hyphens : hyphens -> declaration
(** [webkit_hyphens value] is the -webkit-hyphens property. *)

val webkit_text_size_adjust : string -> declaration
(** [webkit_text_size_adjust value] is the -webkit-text-size-adjust property. *)

(** {1 Additional Properties}

    Specialized CSS properties organized by their functional purpose. *)

(** {2:lists_tables Lists & Tables}

    Properties for styling HTML lists and tables. *)

(** CSS list-style-type values *)
type list_style_type =
  | None
  | Disc
  | Circle
  | Square
  | Decimal
  | Lower_alpha
  | Upper_alpha
  | Lower_roman
  | Upper_roman

(** CSS list-style-image values *)
type list_style_image = None_img | Url of string | Inherit

val list_style_type : list_style_type -> declaration
(** [list_style_type lst] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/list-style-type}
     list-style-type} property. *)

val list_style_image : list_style_image -> declaration
(** [list_style_image img] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/list-style-image}
     list-style-image} property. *)

(* Table layout and vertical-align types *)
type table_layout = Auto | Fixed | Inherit

type vertical_align =
  | Baseline
  | Top
  | Middle
  | Bottom
  | Text_top
  | Text_bottom
  | Sub
  | Super
  | Length of length
  | Percentage of float
  | Inherit

val table_layout : table_layout -> declaration
(** [table_layout value] is the CSS table-layout property. *)

val vertical_align : vertical_align -> declaration
(** [vertical_align value] is the CSS vertical-align property. *)

val list_style : string -> declaration
(** [list_style value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/list-style} list-style}
    shorthand property. *)

val border_spacing : length -> declaration
(** [border_spacing value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-spacing}
     border-spacing} property. *)

(** {2:svg_properties SVG Properties}

    Properties specific to SVG rendering and styling. *)

(** SVG paint values for fill and stroke properties *)
type svg_paint =
  | None  (** No paint *)
  | Current_color  (** Current color value *)
  | Color of color  (** Specific color value *)

val fill : svg_paint -> declaration
(** [fill value] is the SVG fill property. *)

val stroke : svg_paint -> declaration
(** [stroke value] is the SVG stroke property. *)

val stroke_width : length -> declaration
(** [stroke_width value] is the SVG stroke-width property. *)

(** {2:scroll_touch Scroll & Touch}

    Properties for scroll behavior and touch interaction. *)

(** CSS touch-action values *)
type touch_action = Auto | None | Pan_x | Pan_y | Manipulation | Inherit

(** CSS scroll-snap-axis values *)
type scroll_snap_axis = X | Y | Block | Inline | Both

(** CSS scroll-snap-strictness values *)
type scroll_snap_strictness =
  | Mandatory
  | Proximity
  | Var of scroll_snap_strictness var

(** CSS scroll-snap-type values *)
type scroll_snap_type =
  | None
  | Axis of scroll_snap_axis * scroll_snap_strictness option
  | Inherit

(** CSS scroll-snap-align values *)
type scroll_snap_align = None | Start | End | Center

val touch_action : touch_action -> declaration
(** [touch_action value] is the CSS touch-action property. *)

val scroll_snap_type : scroll_snap_type -> declaration
(** [scroll_snap_type value] is the CSS scroll-snap-type property. *)

val scroll_snap_align : scroll_snap_align -> declaration
(** [scroll_snap_align value] is the CSS scroll-snap-align property. *)

(** CSS scroll-snap-stop values *)
type scroll_snap_stop = Normal | Always | Inherit

val scroll_snap_stop : scroll_snap_stop -> declaration
(** [scroll_snap_stop value] is the CSS scroll-snap-stop property. *)

(** CSS scroll behavior values *)
type scroll_behavior = Auto | Smooth | Inherit

val scroll_behavior : scroll_behavior -> declaration
(** [scroll_behavior value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-behavior}
     scroll-behavior} property for smooth scrolling. *)

val scroll_margin : length -> declaration
(** [scroll_margin value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin}
     scroll-margin} property. *)

val scroll_margin_top : length -> declaration
(** [scroll_margin_top value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-top}
     scroll-margin-top} property. *)

val scroll_margin_right : length -> declaration
(** [scroll_margin_right value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-right}
     scroll-margin-right} property. *)

val scroll_margin_bottom : length -> declaration
(** [scroll_margin_bottom value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-bottom}
     scroll-margin-bottom} property. *)

val scroll_margin_left : length -> declaration
(** [scroll_margin_left value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-left}
     scroll-margin-left} property. *)

val scroll_padding : length -> declaration
(** [scroll_padding value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding}
     scroll-padding} property. *)

val scroll_padding_top : length -> declaration
(** [scroll_padding_top value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-top}
     scroll-padding-top} property. *)

val scroll_padding_right : length -> declaration
(** [scroll_padding_right value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-right}
     scroll-padding-right} property. *)

val scroll_padding_bottom : length -> declaration
(** [scroll_padding_bottom value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-bottom}
     scroll-padding-bottom} property. *)

val scroll_padding_left : length -> declaration
(** [scroll_padding_left value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-left}
     scroll-padding-left} property. *)

(** CSS overscroll behavior values *)
type overscroll_behavior = Auto | Contain | None | Inherit

val overscroll_behavior : overscroll_behavior -> declaration
(** [overscroll_behavior value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overscroll-behavior}
     overscroll-behavior} property. *)

val overscroll_behavior_x : overscroll_behavior -> declaration
(** [overscroll_behavior_x value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overscroll-behavior-x}
     overscroll-behavior-x} property. *)

val overscroll_behavior_y : overscroll_behavior -> declaration
(** [overscroll_behavior_y value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overscroll-behavior-y}
     overscroll-behavior-y} property. *)

val accent_color : color -> declaration
(** [accent_color value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/accent-color}
     accent-color} property for form controls. *)

val caret_color : color -> declaration
(** [caret_color value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/caret-color}
     caret-color} property for the text input cursor. *)

(** {2:misc_properties Miscellaneous}

    Other properties that don't fit into specific categories. *)

val forced_color_adjust : forced_color_adjust -> declaration
(** [forced_color_adjust value] is the CSS forced-color-adjust property. *)

val quotes : string -> declaration
(** [quotes value] is the CSS quotes property. *)

type appearance = None | Auto | Button | Textfield | Menulist | Inherit

val appearance : appearance -> declaration
(** [appearance value] is the CSS appearance property. *)

val tab_size : int -> declaration
(** [tab_size value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/tab-size} tab-size}
    property. *)

val font_variation_settings : font_variation_settings -> declaration
(** [font_variation_settings value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-variation-settings}
     font-variation-settings} property. *)

(** {1:custom_properties Custom Properties}

    Type-safe CSS custom properties (CSS variables) with GADT-based type
    checking. *)

(** Value kind GADT for typed custom properties *)
type _ kind =
  | Length : length kind
  | Color : color kind
  | Int : int kind
  | Float : float kind
  | String : string kind
  | Duration : duration kind
  | Aspect_ratio : aspect_ratio kind
  | Border_style : border_style kind
  | Font_weight : font_weight kind
  | Font_family : font_family list kind
  | Font_feature_settings : font_feature_settings kind
  | Font_variation_settings : font_variation_settings kind
  | Font_variant_numeric : font_variant_numeric kind
  | Font_variant_numeric_token : font_variant_numeric_token kind
  | Blend_mode : blend_mode kind
  | Scroll_snap_strictness : scroll_snap_strictness kind
  | Angle : angle kind
  | Shadow : shadow kind
  | Transform_scale : transform_scale kind
  | Box_shadow : box_shadow kind
  | Content : content kind

type meta
(** The type for CSS variable metadata. *)

val meta : unit -> ('a -> meta) * (meta -> 'a option)
(** [meta ()] is an injection/projection pair for metadata. The injection
    function converts a value to metadata, the projection function attempts to
    extract the value back. *)

val var_ref :
  ?fallback:'a -> ?default:'a -> ?layer:string -> ?meta:meta -> string -> 'a var
(** [var_ref ?fallback ?default ?layer ?meta name] is a CSS variable reference.
    This is primarily for the CSS parser to create var() references.

    - [name] is the variable name (without the -- prefix)
    - [fallback] is used inside var(--name, fallback) in CSS output
    - [default] is the resolved value when mode is Inline
    - [layer] is an optional CSS layer name
    - [meta] is optional metadata *)

val var :
  ?fallback:'a ->
  ?layer:string ->
  ?meta:meta ->
  string ->
  'a kind ->
  'a ->
  declaration * 'a var
(** [var ?fallback ?layer name kind value] is a CSS custom property declaration
    and returns a variable handle.

    - [name] is the variable name without the [--] prefix
    - [kind] specifies the value type (Length, Color, Angle, Float, etc.)
    - [value] becomes both the CSS custom property value and the variable's
      default
    - [fallback] is used inside [var(--name, fallback)] in CSS output
    - [layer] is an optional CSS layer name where the variable should be placed

    Example:
    {[
      let def_radius, radius_var = var "radius-md" Length (Rem 0.5) in
      rule ~selector:".card" [ def_radius; border_radius (Var radius_var) ]
    ]}

    The returned [radius_var] must be wrapped with [Var] when used in CSS
    properties. In variables mode, it emits "--radius-md: 0.5rem" and uses
    "var(--radius-md)". In inline mode, it uses "0.5rem" directly when the
    default equals the defined value. *)

val declaration_meta : declaration -> meta option
(** [declaration_meta decl] extracts metadata from a declaration if it has any.
*)

val custom_property : ?layer:string -> string -> string -> declaration
(** [custom_property ?layer name value] is a CSS custom property declaration.

    For type-safe variable declarations and usage, prefer using the {!val:var}
    API which provides compile-time checking and automatic variable management.

    @param layer Optional CSS layer name for the custom property
    @param name CSS custom property name (must start with --)
    @param value CSS value as string

    Example: [custom_property "--primary-color" "#3b82f6"]

    See also {!val:var} (type-safe CSS variable API). *)

val custom_declaration_name : declaration -> string option
(** [custom_declaration_name decl] is the variable name if [decl] is a custom
    property declaration, [None] otherwise. *)

val custom_declaration_layer : declaration -> string option
(** [custom_declaration_layer decl] is the declared layer for a custom property
    declaration if present (e.g., "theme" or "utilities"). It is [None] for
    non-custom declarations or when no layer metadata is attached. *)

(** {1 Rendering & Optimization}

    CSS output generation and performance optimization tools. *)

(** {2:rendering Rendering}

    Functions for converting CSS structures to string output. *)

val to_string : ?minify:bool -> ?optimize:bool -> ?mode:mode -> t -> string
(** [to_string ?minify ?optimize ?mode stylesheet] renders a complete stylesheet
    to CSS.
    - If [minify] is [true], the output will be compact (no unnecessary
      whitespace).
    - If [optimize] is [true], rule-level optimizations are applied
      (deduplication, merging consecutive rules, combining identical rules).
    - [mode] controls variable layer emission behavior.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS> "MDN: CSS" *)

val pp : ?minify:bool -> ?optimize:bool -> ?mode:mode -> t -> string
(** [pp] is {!to_string}. *)

type any_var = V : 'a var -> any_var

val vars_of_rules : rule list -> any_var list
(** [vars_of_rules rules] extracts all CSS variables referenced in the rules'
    declarations, returning them sorted and deduplicated. *)

val vars_of_media_queries : media_rule list -> any_var list
(** [vars_of_media_queries media_queries] extracts all CSS variables referenced
    in the media queries' rules, returning them sorted and deduplicated. *)

(** [vars_of_container_queries container_queries] extracts all CSS variables
    referenced in the container queries' rules, returning them sorted and
    deduplicated. *)

val vars_of_stylesheet : t -> any_var list
(** [vars_of_stylesheet stylesheet] extracts all CSS variables referenced in the
    entire stylesheet, returning them sorted and deduplicated. *)

val any_var_name : any_var -> string
(** [any_var_name v] is the name of a CSS variable (with [--] prefix). *)

val analyze_declarations : declaration list -> any_var list
(** [analyze_declarations declarations] is the typed CSS variables extracted
    from [declarations]. *)

val extract_custom_declarations : declaration list -> declaration list
(** [extract_custom_declarations decls] is only the custom property declarations
    from [decls]. *)

(** {2:optimization Optimization}

    Tools for optimizing CSS output for performance and file size. *)

val optimize : t -> t
(** [optimize stylesheet] applies CSS optimizations to the stylesheet, including
    merging consecutive identical selectors and combining rules with identical
    properties. Preserves CSS cascade semantics. *)

type layer_stats = {
  name : string;
  rules : int;
  selectors : string list;  (** First few selectors as examples *)
}

val stylesheet_rules : t -> rule list
(** [stylesheet_rules stylesheet] is the list of top-level rules of
    [stylesheet]. *)

val will_change : string -> declaration
(** [will_change value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/will-change}
     will-change} property for performance optimization. *)

val deduplicate_declarations : declaration list -> declaration list
(** [deduplicate_declarations declarations] removes duplicate declarations,
    keeping the last occurrence. It also automatically duplicates properties
    that have known browser bugs requiring duplication for correct rendering
    (e.g., -webkit-text-decoration-color). *)

val inline_style_of_declarations :
  ?optimize:bool -> ?minify:bool -> ?mode:mode -> declaration list -> string
(** [inline_style_of_declarations declarations] converts a list of declarations
    to an inline style string. *)

(** {2 Pretty-printing functions for types} *)

val pp_display : display Pp.t
(** [pp_display] is the pretty printer for display values. *)

val pp_position : position Pp.t
(** [pp_position] is the pretty printer for position values. *)

val pp_length : length Pp.t
(** [pp_length] is the pretty printer for length values. *)

val pp_color : color Pp.t
(** [pp_color] is the pretty printer for color values. *)

val pp_angle : angle Pp.t
(** [pp_angle] is the pretty printer for angle values. *)

val pp_duration : duration Pp.t
(** [pp_duration] is the pretty printer for duration values. *)

val pp_font_weight : font_weight Pp.t
(** [pp_font_weight] is the pretty printer for font-weight values. *)

val pp_cursor : cursor Pp.t
(** [pp_cursor] is the pretty printer for cursor values. *)

val pp_transform : transform Pp.t
(** [pp_transform] is the pretty printer for transform values. *)

val pp_box_shadow : box_shadow Pp.t
(** [pp_box_shadow] is the pretty printer for box-shadow values. *)

val pp_calc : 'a Pp.t -> 'a calc Pp.t
(** [pp_calc pp_value] is the pretty printer for calc expressions. *)

val pp_font_style : font_style Pp.t
(** [pp_font_style] is the pretty printer for font-style values. *)

val pp_text_align : text_align Pp.t
(** [pp_text_align] is the pretty printer for text-align values. *)

val pp_text_decoration : text_decoration Pp.t
(** [pp_text_decoration] is the pretty printer for text-decoration values. *)

val pp_text_transform : text_transform Pp.t
(** [pp_text_transform] is the pretty printer for text-transform values. *)

val pp_overflow : overflow Pp.t
(** [pp_overflow] is the pretty printer for overflow values. *)

val pp_border_style : border_style Pp.t
(** [pp_border_style] is the pretty printer for border-style values. *)

val pp_flex_direction : flex_direction Pp.t
(** [pp_flex_direction] is the pretty printer for flex-direction values. *)

val pp_align_items : align_items Pp.t
(** [pp_align_items] is the pretty printer for align-items values. *)

val pp_justify_content : justify_content Pp.t
(** [pp_justify_content] is the pretty printer for justify-content values. *)

(**/**)

module Pp = Pp

val box_shadows : shadow list -> box_shadow
(** [box_shadows lst] is multiple box-shadows. *)
