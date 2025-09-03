(** Type-safe CSS generation library.

    This library provides OCaml types to represent CSS declarations, rules, and
    stylesheets, preventing common errors through a structured, typed approach
    instead of raw strings.

    {b Core Concepts:}
    - A {!type:declaration} is a single property-value pair (e.g., [color: red])
    - A {!type:rule} combines a selector with a list of declarations
    - A {!type:t} (stylesheet) is a collection of rules and other top-level
      items
    - Typed values like {!type:length} and {!type:color} ensure valid CSS

    {b Example:}
    {[
      open Css

      let my_stylesheet =
        stylesheet
          [
            Rule
              (rule ~selector:".button"
                 [
                   background_color (Hex "3b82f6");
                   color (Hex "ffffff");
                   padding (Rem 0.5);
                   border_radius "0.375rem";
                   font_weight Bold;
                   display Inline_block;
                 ]);
            Media
              (media ~condition:"(min-width: 768px)"
                 [ rule ~selector:".button" [ padding (Rem 1.0) ] ]);
          ]

      let () = print_endline (to_string my_stylesheet)
    ]}

    {b CSS Custom Properties:}
    {[
      (* Define CSS variables with typed API *)
      let primary_color_def, primary_color_var =
        var "primary-color" Color (Hex "#3b82f6")
      in
      let text_color_def, text_color_var =
        var "text-color" Color (Hex "#1f2937")
      in
      let spacing_def, spacing_var =
        var "spacing" Length (Rem 1.0)
      in
      let theme_vars =
        rule ~selector:":root"
          [
            primary_color_def;
            text_color_def;
            spacing_def;
          ]

      (* Use CSS variables *)
      let component =
        rule ~selector:".card"
          [
            background_color (Var primary_color_var);
            color (Var text_color_var);
            padding (Var spacing_var);
          ]
    ]}

    {b Interface Organization:} This interface is organized into a hierarchical
    structure that follows logical learning and usage patterns, progressing from
    basic setup to advanced features:

    {b Core Concepts} - Core CSS system setup and construction:
    - {!section:css_selectors} - CSS Selectors
    - {!section:css_rules} - CSS Rules and Stylesheets
    - {!section:at_rules} - At-Rules
    - {!section:stylesheet_construction} - Stylesheet Construction
    - {!section:nesting_helpers} - Nesting Helpers

    {b Declarations} - Core value types and declaration building:
    - {!section:core_types} - Core Types & Calculations
    - {!section:css_values} - CSS Values & Units

    {b Property Categories} - Organized CSS properties by functionality:
    - {!section:box_model} - Box Model & Sizing
    - {!section:display_positioning} - Display & Positioning
    - {!section:flexbox} - Flexbox Layout
    - {!section:grid} - Grid Layout
    - {!section:modern_layout} - Modern Layout Features
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
    - {!section:vendor_prefixes} - Vendor Prefixes & Legacy Support
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
  (** Convert a selector to its string representation *)

  val element : string -> t
  (** [element name] creates an element selector (e.g., "div", "p"). Validates
      that [name] is a valid CSS identifier.
      @raise Invalid_argument if [name] violates CSS identifier rules. *)

  val class_ : string -> t
  (** [class_ name] creates a class selector (e.g., ".button", ".prose").
      Validates that [name] is a valid CSS identifier.
      @raise Invalid_argument if [name] violates CSS identifier rules. *)

  val id : string -> t
  (** [id name] creates an ID selector (e.g., "#header"). Validates that [name]
      is a valid CSS identifier.
      @raise Invalid_argument if [name] violates CSS identifier rules. *)

  val universal : t
  (** [universal] creates the universal selector "*" *)

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
  (** [attribute name match] creates a typed attribute selector. Validates that
      [name] is a valid CSS identifier.
      @raise Invalid_argument if [name] violates CSS identifier rules. *)

  val pseudo_class : string -> t
  (** [pseudo_class name] creates a pseudo-class selector (e.g., ":hover",
      ":first-child"). Validates that [name] is a valid CSS identifier.
      @raise Invalid_argument if [name] violates CSS identifier rules. *)

  val pseudo_element : string -> t
  (** [pseudo_element name] creates a pseudo-element selector (e.g., "::before",
      "::marker"). Validates that [name] is a valid CSS identifier.
      @raise Invalid_argument if [name] violates CSS identifier rules. *)

  val combine : t -> combinator -> t -> t
  (** Combine two selectors with a combinator *)

  val ( ++ ) : t -> t -> t
  (** [sel1 ++ sel2] combines two selectors with descendant combinator (space).
      Equivalent to [combine sel1 Descendant sel2]. *)

  val ( >> ) : t -> t -> t
  (** [sel1 >> sel2] combines two selectors with child combinator (>).
      Equivalent to [combine sel1 Child sel2]. *)

  val where : t list -> t
  (** Create a :where() functional pseudo-class *)

  val not : t list -> t
  (** Create a :not() functional pseudo-class *)

  val fun_ : string -> t list -> t
  (** Create a functional pseudo-class like :is(), :has(), etc. *)

  val list : t list -> t
  (** Create a comma-separated selector list *)

  val is_compound_list : t -> bool
  (** Check if this is already a comma-separated list of selectors *)

  val compound : t list -> t
  (** [compound selectors] creates a compound selector by combining multiple
      simple selectors (e.g., element + attribute: "div[class=foo]") *)

  val ( && ) : t -> t -> t
  (** [sel1 && sel2] creates a compound selector by combining two simple
      selectors. Equivalent to [compound [sel1; sel2]]. Example:
      [element "div" && class_ "foo"] *)

  val ( || ) : t -> t -> t
  (** [sel1 || sel2] creates a comma-separated selector list (OR semantics).
      Equivalent to [list [sel1; sel2]]. Example: [element "h1" || element "h2"]
  *)
end

(** {2:css_rules CSS Rules and Stylesheets}

    Core building blocks for CSS rules and stylesheet construction.

    @see <https://www.w3.org/TR/css-syntax-3/> CSS Syntax Module Level 3 *)

type rule
(** Abstract type for CSS rules (selector + declarations). *)

type declaration
(** Abstract type for CSS declarations (property-value pairs). *)

val rule : selector:Selector.t -> declaration list -> rule
(** [rule ~selector declarations] creates a CSS rule with the given selector and
    declarations. *)

type nested_rule
(** Abstract type for rules that can be nested in at-rules. *)

type t
(** Abstract type for CSS stylesheets. *)

val selector : rule -> Selector.t
(** [selector rule] returns the selector of a CSS rule. *)

val declarations : rule -> declaration list
(** [declarations rule] returns the list of declarations in a CSS rule. *)

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
(** [media ~condition rules] creates a [@media] rule.
    @param condition Media query condition (e.g., "(min-width: 768px)"). *)

val media_condition : media_rule -> string
(** [media_condition media] returns the condition string of a media rule. *)

val media_rules : media_rule -> rule list
(** [media_rules media] returns the rules contained in a media rule. *)

val supports : condition:string -> rule list -> supports_rule
(** [supports ~condition rules] creates a [@supports] rule for feature queries.
    @param condition Feature query condition (e.g., "(display: grid)"). *)

val supports_nested :
  condition:string -> rule list -> supports_rule list -> supports_rule
(** [supports_nested ~condition rules nested_supports] creates a [@supports]
    rule with nested [@supports] rules. *)

val container :
  ?name:string option -> condition:string -> rule list -> container_rule
(** [container ?name ~condition rules] creates a [@container] rule.
    @param name Optional container name.
    @param condition Container query condition (e.g., "(min-width: 700px)"). *)

val layer :
  name:string ->
  ?media:media_rule list ->
  ?container:container_rule list ->
  ?supports:supports_rule list ->
  nested_rule list ->
  layer_rule
(** [layer ~name ?media ?container ?supports rules] creates a [@layer] rule.
    @param name Layer name.
    @param media Optional nested [@media] rules.
    @param container Optional nested [@container] rules.
    @param supports Optional nested [@supports] rules.

    {b CSS Cascade Layer Ordering:}
    Layers are applied in the order they are declared. The standard layer order is:
    1. [theme] - CSS custom properties and design tokens
    2. [properties] - @property rules for custom property registration
    3. [base] - Normalize/reset styles and base element styling
    4. [components] - Reusable component styles
    5. [utilities] - Single-purpose utility classes

    Later layers override earlier layers, and unlayered styles have the highest priority.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/@layer> MDN: @layer *)

val layer_name : layer_rule -> string
(** [layer_name layer] returns the name of a layer rule. *)

val layer_rules : layer_rule -> nested_rule list
(** [layer_rules layer] returns the rules contained in a layer rule. *)

val property :
  syntax:string ->
  ?initial_value:string ->
  ?inherits:bool ->
  string ->
  property_rule
(** [property ~syntax ?initial_value ?inherits name] creates a [@property] rule
    to register a custom CSS property.
    @param syntax Property syntax (e.g., "<color>").
    @param initial_value Optional initial value.
    @param inherits Whether the property inherits (default: false).
    @param name Property name (including --). *)

val property_rule_name : property_rule -> string
(** [property_rule_name rule] returns the property name from a property rule. *)

val property_rule_initial : property_rule -> string option
(** [property_rule_initial rule] returns the initial value from a property rule.
*)

val default_decl_of_property_rule : property_rule -> declaration
(** [default_decl_of_property_rule rule] converts a property rule to a custom
    property declaration with its initial value. This is useful for creating
    default declarations in the properties layer. *)

(** {2:stylesheet_construction Stylesheet Construction}

    Tools for building complete CSS stylesheets from rules and declarations. *)

(** Items that can appear at the top level of a stylesheet. *)
type sheet_item =
  | Rule of rule  (** Regular CSS rule *)
  | Media of media_rule  (** [@media] at-rule *)
  | Supports of supports_rule  (** [@supports] at-rule *)
  | Container of container_rule  (** [@container] at-rule *)
  | Layer of layer_rule  (** [@layer] at-rule *)
  | Property of property_rule  (** [@property] at-rule *)
  | Starting_style of starting_style_rule  (** [@starting-style] at-rule *)

val empty : t
(** [empty] is an empty stylesheet. *)

val concat : t list -> t
(** [concat stylesheets] concatenates multiple stylesheets into one. Rules are
    combined in order, with later stylesheets taking precedence. *)

val stylesheet : sheet_item list -> t
(** [stylesheet items] creates a stylesheet from a list of items. *)

val stylesheet_items : t -> sheet_item list
(** [stylesheet_items t] returns all top-level items in the stylesheet. *)

val stylesheet_layers : t -> layer_rule list
(** [stylesheet_layers stylesheet] returns the layer rules of a stylesheet. *)

val stylesheet_media_queries : t -> media_rule list
(** [stylesheet_media_queries stylesheet] returns the media queries of a
    stylesheet. *)

val stylesheet_container_queries : t -> container_rule list
(** [stylesheet_container_queries stylesheet] returns the container queries of a
    stylesheet. *)

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
(** [rule_to_nested rule] converts a rule to nested_rule for use in at-rules. *)

val supports_to_nested : supports_rule -> nested_rule
(** [supports_to_nested supports] converts a supports rule to nested_rule. *)

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
(** [var_layer v] returns the optional layer where [v] is defined. *)

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
  (** [add left right] creates [left + right] *)

  val sub : 'a calc -> 'a calc -> 'a calc
  (** [sub left right] creates [left - right] *)

  val mul : 'a calc -> 'a calc -> 'a calc
  (** [mul left right] creates [left * right] *)

  val div : 'a calc -> 'a calc -> 'a calc
  (** [div left right] creates [left / right] *)

  val ( + ) : 'a calc -> 'a calc -> 'a calc
  (** [(+)] is {!add} *)

  val ( - ) : 'a calc -> 'a calc -> 'a calc
  (** [(-)] is {!sub} *)

  val ( * ) : 'a calc -> 'a calc -> 'a calc
  (** [( * )] is {!mul} *)

  val ( / ) : 'a calc -> 'a calc -> 'a calc
  (** [(/)] is {!div} *)

  val length : length -> length calc
  (** [length len] lifts a length value into calc *)

  val var : ?default:'a -> ?fallback:'a -> string -> 'a calc
  (** [var ?default ?fallback name] creates a variable reference for calc
      expressions. Example: [var "spacing"] or
      [var ~fallback:(Var lh_var) "tw-leading"] *)

  val float : float -> length calc
  (** [float f] creates a numeric value for calc expressions *)

  val infinity : length calc
  (** [infinity] represents the CSS infinity value for calc expressions *)

  val px : int -> length calc
  (** [px n] creates a pixel value for calc expressions *)

  val rem : float -> length calc
  (** [rem f] creates a rem value for calc expressions *)

  val em : float -> length calc
  (** [em f] creates an em value for calc expressions *)

  val pct : float -> length calc
  (** [pct f] creates a percentage value for calc expressions *)
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
type font_feature_settings = Normal | Feature_list of string

(** CSS font-variation-settings values *)
type font_variation_settings = Normal | Axis_list of string

val important : declaration -> declaration
(** [important decl] marks a declaration as !important. Validates that [name] is
    a valid CSS identifier.
    @raise Invalid_argument if [name] violates CSS identifier rules. *)

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
(** [width len] sets the CSS width property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/width> MDN: width *)

val height : length -> declaration
(** [height len] sets the CSS height property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/height> MDN: height
*)

val min_width : length -> declaration
(** [min_width len] sets the CSS min-width property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/min-width>
      MDN: min-width *)

val max_width : length -> declaration
(** [max_width len] sets the CSS max-width property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/max-width>
      MDN: max-width *)

val min_height : length -> declaration
(** [min_height len] sets the CSS min-height property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/min-height>
      MDN: min-height *)

val max_height : length -> declaration
(** [max_height len] sets the CSS max-height property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/max-height>
      MDN: max-height *)

val padding : length -> declaration
(** [padding len] sets the CSS padding property for all sides.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/padding> MDN: padding
*)

val padding_top : length -> declaration
(** [padding_top len] sets the CSS padding-top property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/padding-top>
      MDN: padding-top *)

val padding_right : length -> declaration
(** [padding_right len] sets the CSS padding-right property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/padding-right>
      MDN: padding-right *)

val padding_bottom : length -> declaration
(** [padding_bottom len] sets the CSS padding-bottom property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/padding-bottom>
      MDN: padding-bottom *)

val padding_left : length -> declaration
(** [padding_left len] sets the CSS padding-left property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/padding-left>
      MDN: padding-left *)

val margin : length -> declaration
(** [margin len] sets the CSS margin property for all sides.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/margin> MDN: margin
*)

val margin_top : length -> declaration
(** [margin_top len] sets the CSS margin-top property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/margin-top>
      MDN: margin-top *)

val margin_right : length -> declaration
(** [margin_right len] sets the CSS margin-right property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/margin-right>
      MDN: margin-right *)

val margin_bottom : length -> declaration
(** [margin_bottom len] sets the CSS margin-bottom property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/margin-bottom>
      MDN: margin-bottom *)

val margin_left : length -> declaration
(** [margin_left len] sets the CSS margin-left property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/margin-left>
      MDN: margin-left *)

val box_sizing : box_sizing -> declaration
(** [box_sizing value] sets the CSS box-sizing property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/box-sizing>
      MDN: box-sizing *)

(** {2:logical_properties Logical Properties}

    CSS Logical Properties provide writing-mode-relative property equivalents
    for physical properties. These adapt to different writing directions and
    text orientations.

    @see <https://www.w3.org/TR/css-logical-1/>
      CSS Logical Properties and Values Level 1 *)

val border_inline_start_width : length -> declaration
(** [border_inline_start_width len] sets the CSS border-inline-start-width
    property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-start-width>
      MDN: border-inline-start-width *)

val border_inline_end_width : length -> declaration
(** [border_inline_end_width len] sets the CSS border-inline-end-width property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-end-width>
      MDN: border-inline-end-width *)

val border_inline_start_color : color -> declaration
(** [border_inline_start_color c] sets the CSS border-inline-start-color
    property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-start-color>
      MDN: border-inline-start-color *)

val border_inline_end_color : color -> declaration
(** [border_inline_end_color c] sets the CSS border-inline-end-color property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-end-color>
      MDN: border-inline-end-color *)

val padding_inline_start : length -> declaration
(** [padding_inline_start value] sets the CSS padding-inline-start property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/padding-inline-start>
      MDN: padding-inline-start. *)

val padding_inline_end : length -> declaration
(** [padding_inline_end value] sets the CSS padding-inline-end property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/padding-inline-end>
      MDN: padding-inline-end. *)

val padding_inline : length -> declaration
(** [padding_inline value] sets the CSS padding-inline shorthand property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/padding-inline>
      MDN: padding-inline. *)

val padding_block : length -> declaration
(** [padding_block value] sets the CSS padding-block shorthand property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/padding-block>
      MDN: padding-block. *)

val margin_inline : length -> declaration
(** [margin_inline len] sets the CSS margin-inline property with a length value.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/margin-inline>
      MDN: margin-inline. *)

val margin_block : length -> declaration
(** [margin_block len] sets the CSS margin-block property with a length value.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/margin-block>
      MDN: margin-block. *)

val margin_inline_end : length -> declaration
(** [margin_inline_end len] sets the CSS margin-inline-end property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/margin-inline-end>
      MDN: margin-inline-end. *)

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
  | Table
  | Table_row
  | Table_cell
  | List_item
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
(** [display d] sets the CSS display property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/display> MDN: display
*)

val position : position -> declaration
(** [position p] sets the CSS position property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/position>
      MDN: position *)

val top : length -> declaration
(** [top len] sets the CSS top property for positioned elements.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/top> MDN: top *)

val right : length -> declaration
(** [right len] sets the CSS right property for positioned elements.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/right> MDN: right *)

val bottom : length -> declaration
(** [bottom len] sets the CSS bottom property for positioned elements.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/bottom> MDN: bottom
*)

val left : length -> declaration
(** [left len] sets the CSS left property for positioned elements.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/left> MDN: left *)

val z_index : z_index -> declaration
(** [z_index value] sets the CSS z-index property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/z-index> MDN: z-index
*)

val visibility : visibility -> declaration
(** [visibility v] sets the CSS visibility property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/visibility>
      MDN: visibility *)

(** CSS float side values. *)
type float_side = None | Left | Right

val float : float_side -> declaration
(** [float value] sets the CSS float property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/float> MDN: float *)

(* Clear property values *)
type clear = None | Left | Right | Both

val clear : clear -> declaration
(** [clear value] sets the CSS clear property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/clear> MDN: clear *)

val overflow : overflow -> declaration
(** [overflow value] sets the CSS overflow property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/overflow>
      MDN: overflow *)

val overflow_x : overflow -> declaration
(** [overflow_x value] sets the CSS overflow-x property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-x>
      MDN: overflow-x *)

val overflow_y : overflow -> declaration
(** [overflow_y value] sets the CSS overflow-y property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-y>
      MDN: overflow-y *)

(** CSS content values *)
type content =
  | String of string
  | None
  | Normal
  | Open_quote
  | Close_quote
  | Var of content var

val content : content -> declaration
(** [content c] sets the CSS content property. *)

(** CSS text-overflow values *)
type text_overflow = Clip | Ellipsis | String of string | Inherit

val text_overflow : text_overflow -> declaration
(** [text_overflow value] sets the CSS text-overflow property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/text-overflow>
      MDN: text-overflow *)

(** CSS backface-visibility values *)
type backface_visibility = Visible | Hidden | Inherit

val backface_visibility : backface_visibility -> declaration
(** [backface_visibility value] sets the CSS backface-visibility property (3D
    transforms).
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/backface-visibility>
      MDN: backface-visibility *)

(** CSS content-visibility values. *)
type content_visibility =
  | Visible  (** Content is visible and rendered *)
  | Hidden  (** Content is hidden from rendering *)
  | Auto  (** Browser determines visibility based on relevance *)
  | Inherit  (** Inherit from parent *)
  | Var of content_visibility var  (** CSS variable reference *)

val content_visibility : content_visibility -> declaration
(** [content_visibility value] sets the CSS content-visibility property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/content-visibility>
      MDN: content-visibility *)

(** CSS list-style-position values *)
type list_style_position = Inside | Outside | Inherit

val list_style_position : list_style_position -> declaration
(** [list_style_position pos] sets the CSS list-style-position property.

    MDN: list-style-position. *)

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
(** [color value] sets the CSS color property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/color> MDN: color *)

val background_color : color -> declaration
(** [background_color color] sets the CSS background-color property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/background-color>
      MDN: background-color *)

val background_image : background_image -> declaration
(** [background_image value] sets the CSS background-image property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/background-image>
      MDN: background-image *)

val background_position : string -> declaration
(** [background_position value] sets the CSS background-position property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/background-position>
      MDN: background-position *)

val background_size : background_size -> declaration
(** [background_size value] sets the CSS background-size property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/background-size>
      MDN: background-size *)

val background_repeat : background_repeat -> declaration
(** [background_repeat value] sets the CSS background-repeat property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/background-repeat>
      MDN: background-repeat *)

val background_attachment : background_attachment -> declaration
(** [background_attachment value] sets the CSS background-attachment property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/background-attachment>
      MDN: background-attachment *)

val opacity : float -> declaration
(** [opacity value] sets the CSS opacity property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/opacity> MDN: opacity
*)

val url : string -> background_image
(** [url path] creates a URL background image value. *)

val linear_gradient :
  gradient_direction -> gradient_stop list -> background_image
(** [linear_gradient dir stops] creates a linear gradient background. *)

val radial_gradient : gradient_stop list -> background_image
(** [radial_gradient stops] creates a radial gradient background. *)

val color_stop : color -> gradient_stop
(** [color_stop c] creates a simple color stop. *)

val color_position : color -> length -> gradient_stop
(** [color_position c pos] creates a color stop at a specific position. *)

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
type align_items = Flex_start | Flex_end | Center | Baseline | Stretch

(** CSS justify-content values. *)
type justify_content =
  | Flex_start
  | Flex_end
  | Center
  | Space_between
  | Space_around
  | Space_evenly

(** CSS align-self values. *)
type align_self = Auto | Flex_start | Flex_end | Center | Baseline | Stretch

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
(** [flex_direction value] sets the CSS flex-direction property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/flex-direction>
      MDN: flex-direction *)

val flex_wrap : flex_wrap -> declaration
(** [flex_wrap value] sets the CSS flex-wrap property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/flex-wrap>
      MDN: flex-wrap *)

val justify_content : justify_content -> declaration
(** [justify_content value] sets the CSS justify-content property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/justify-content>
      MDN: justify-content *)

val align_items : align_items -> declaration
(** [align_items value] sets the CSS align-items property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/align-items>
      MDN: align-items *)

val flex : flex -> declaration
(** [flex value] sets the CSS flex shorthand property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/flex> MDN: flex *)

val flex_grow : float -> declaration
(** [flex_grow value] sets the CSS flex-grow property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/flex-grow>
      MDN: flex-grow *)

val flex_shrink : float -> declaration
(** [flex_shrink value] sets the CSS flex-shrink property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/flex-shrink>
      MDN: flex-shrink *)

val flex_basis : length -> declaration
(** [flex_basis value] sets the CSS flex-basis property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/flex-basis>
      MDN: flex-basis *)

val align_self : align_self -> declaration
(** [align_self value] sets the CSS align-self property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/align-self>
      MDN: align-self *)

val order : int -> declaration
(** [order value] sets the CSS order property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/order> MDN: order *)

val gap : length -> declaration
(** [gap value] sets the CSS gap property (applies to both row and column gaps).
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/gap> MDN: gap *)

val row_gap : length -> declaration
(** [row_gap value] sets the CSS row-gap property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/row-gap> MDN: row-gap
*)

val column_gap : length -> declaration
(** [column_gap value] sets the CSS column-gap property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/column-gap>
      MDN: column-gap *)

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
(** [grid_template_columns value] sets the CSS grid-template-columns property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template-columns>
      MDN: grid-template-columns *)

val grid_template_rows : grid_template -> declaration
(** [grid_template_rows value] sets the CSS grid-template-rows property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template-rows>
      MDN: grid-template-rows *)

val grid_template_areas : string -> declaration
(** [grid_template_areas value] sets the CSS grid-template-areas property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template-areas>
      MDN: grid-template-areas *)

val grid_template : grid_template -> declaration
(** [grid_template value] sets the CSS grid-template shorthand property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template>
      MDN: grid-template *)

val grid_auto_columns : grid_template -> declaration
(** [grid_auto_columns value] sets the CSS grid-auto-columns property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/grid-auto-columns>
      MDN: grid-auto-columns *)

val grid_auto_rows : grid_template -> declaration
(** [grid_auto_rows value] sets the CSS grid-auto-rows property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/grid-auto-rows>
      MDN: grid-auto-rows *)

val grid_row_start : grid_line -> declaration
(** [grid_row_start value] sets the CSS grid-row-start property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/grid-row-start>
      MDN: grid-row-start *)

val grid_row_end : grid_line -> declaration
(** [grid_row_end value] sets the CSS grid-row-end property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/grid-row-end>
      MDN: grid-row-end *)

val grid_column_start : grid_line -> declaration
(** [grid_column_start value] sets the CSS grid-column-start property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/grid-column-start>
      MDN: grid-column-start *)

val grid_column_end : grid_line -> declaration
(** [grid_column_end value] sets the CSS grid-column-end property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/grid-column-end>
      MDN: grid-column-end *)

val grid_row : grid_line * grid_line -> declaration
(** [grid_row (start, end)] sets the CSS grid-row shorthand property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/grid-row>
      MDN: grid-row *)

val grid_column : grid_line * grid_line -> declaration
(** [grid_column (start, end)] sets the CSS grid-column shorthand property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/grid-column>
      MDN: grid-column *)

val grid_area : string -> declaration
(** [grid_area value] sets the CSS grid-area property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/grid-area>
      MDN: grid-area *)

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

(** CSS line height values. *)
type line_height =
  | Normal
  | Length of length
  | Number of float
  | Percentage of float
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
(** @see <https://developer.mozilla.org/en-US/docs/Web/CSS/font-family>
      MDN: font-family *)

val font_size : length -> declaration
(** [font_size value] sets the CSS font-size property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/font-size>
      MDN: font-size *)

val font_weight : font_weight -> declaration
(** [font_weight value] sets the CSS font-weight property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/font-weight>
      MDN: font-weight *)

val font_style : font_style -> declaration
(** [font_style value] sets the CSS font-style property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/font-style>
      MDN: font-style *)

val line_height : line_height -> declaration
(** [line_height value] sets the CSS line-height property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/line-height>
      MDN: line-height *)

val letter_spacing : length -> declaration
(** [letter_spacing value] sets the CSS letter-spacing property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/letter-spacing>
      MDN: letter-spacing *)

val word_spacing : length -> declaration
(** [word_spacing value] sets the CSS word-spacing property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/word-spacing>
      MDN: word-spacing *)

val text_align : text_align -> declaration
(** [text_align value] sets the CSS text-align property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/text-align>
      MDN: text-align *)

val text_decoration : text_decoration -> declaration
(** [text_decoration value] sets the CSS text-decoration property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration>
      MDN: text-decoration *)

val text_transform : text_transform -> declaration
(** [text_transform value] sets the CSS text-transform property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/text-transform>
      MDN: text-transform *)

val text_indent : length -> declaration
(** [text_indent value] sets the CSS text-indent property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/text-indent>
      MDN: text-indent *)

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
(** [white_space value] sets the CSS white-space property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/white-space>
      MDN: white-space *)

(** CSS word-break values *)
type word_break = Normal | Break_all | Keep_all | Break_word | Inherit

val word_break : word_break -> declaration
(** [word_break value] sets the CSS word-break property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/word-break>
      MDN: word-break *)

val text_decoration_color : color -> declaration
(** [text_decoration_color value] sets the CSS text-decoration-color property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-color>
      MDN: text-decoration-color *)

val text_size_adjust : string -> declaration
(** [text_size_adjust value] sets the CSS text-size-adjust property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/text-size-adjust>
      MDN: text-size-adjust *)

(** CSS text-decoration-style values *)
type text_decoration_style = Solid | Double | Dotted | Dashed | Wavy | Inherit

val text_decoration_style : text_decoration_style -> declaration
(** [text_decoration_style value] sets the CSS text-decoration-style property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-style>
      MDN: text-decoration-style *)

val text_underline_offset : string -> declaration
(** [text_underline_offset value] sets the CSS text-underline-offset property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/text-underline-offset>
      MDN: text-underline-offset *)

(** CSS overflow-wrap values *)
type overflow_wrap = Normal | Break_word | Anywhere | Inherit

val overflow_wrap : overflow_wrap -> declaration
(** [overflow_wrap value] sets the CSS overflow-wrap property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-wrap>
      MDN: overflow-wrap *)

(** CSS hyphens values *)
type hyphens = None | Manual | Auto | Inherit

val hyphens : hyphens -> declaration
(** [hyphens value] sets the CSS hyphens property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/hyphens> MDN: hyphens
*)

(** CSS font-stretch values *)
type font_stretch =
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
(** [font_stretch value] sets the CSS font-stretch property. *)

(** CSS font-variant-numeric token values *)
type font_variant_numeric_token =
  | Normal_figures
  | Lining_nums
  | Oldstyle_nums
  | Proportional_nums
  | Tabular_nums
  | Diagonal_fractions
  | Stacked_fractions
  | Ordinal
  | Slashed_zero

(** CSS font-variant-numeric values *)
type font_variant_numeric =
  | Normal
  | Tokens of font_variant_numeric_token list
  | Var of font_variant_numeric var

val font_variant_numeric : font_variant_numeric -> declaration
(** list of tokens or composed CSS variables. *)

val font_variant_numeric_tokens :
  font_variant_numeric_token list -> font_variant_numeric
(** [font_variant_numeric_tokens tokens] creates a font-variant-numeric value
    from tokens. *)

val font_variant_numeric_composed :
  ?ordinal:font_variant_numeric_token ->
  ?slashed_zero:font_variant_numeric_token ->
  ?numeric_figure:font_variant_numeric_token ->
  ?numeric_spacing:font_variant_numeric_token ->
  ?numeric_fraction:font_variant_numeric_token ->
  unit ->
  font_variant_numeric
(** [font_variant_numeric_composed ...] creates a composed font-variant-numeric
    value using CSS variables for style composition. *)

type shadow = {
  inset : bool;
  h_offset : length;
  v_offset : length;
  blur : length;
  spread : length;
  color : color;
}
(** CSS shadow values *)

(** CSS text-shadow values *)
type text_shadow =
  | Shadow of shadow
  | Shadows of shadow list  (** Multiple text shadows *)
  | None  (** No shadow *)
  | Var of text_shadow var  (** Composed shadow variable *)

val text_shadow : text_shadow list -> declaration
(** [text_shadow values] sets the CSS text-shadow property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/text-shadow>
      MDN: text-shadow *)

val font : string -> declaration
(** [font value] sets the CSS font shorthand property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/font> MDN: font. *)

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
(** [border value] sets the CSS border shorthand property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border> MDN: border
*)

val border_width : length -> declaration
(** [border_width value] sets the CSS border-width property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-width>
      MDN: border-width *)

val border_style : border_style -> declaration
(** [border_style value] sets the CSS border-style property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-style>
      MDN: border-style *)

val border_color : color -> declaration
(** [border_color value] sets the CSS border-color property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-color>
      MDN: border-color *)

val border_radius : length -> declaration
(** [border_radius value] sets the CSS border-radius property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-radius>
      MDN: border-radius *)

val border_spacing : length -> declaration
(** [border_spacing value] sets the CSS border-spacing property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-spacing>
      MDN: border-spacing *)

val border_top : string -> declaration
(** [border_top value] sets the CSS border-top property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-top>
      MDN: border-top *)

val border_right : string -> declaration
(** [border_right value] sets the CSS border-right property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-right>
      MDN: border-right *)

val border_bottom : string -> declaration
(** [border_bottom value] sets the CSS border-bottom property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom>
      MDN: border-bottom *)

val border_left : string -> declaration
(** [border_left value] sets the CSS border-left property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-left>
      MDN: border-left *)

val outline : string -> declaration
(** [outline value] sets the CSS outline property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/outline>
      MDN: outline `*)

val outline_width : length -> declaration
(** [outline_width value] sets the CSS outline-width property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/outline-width>
      MDN: outline-width *)

val outline_style : border_style -> declaration
(** [outline_style value] sets the CSS outline-style property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/outline-style>
      MDN: outline-style *)

val outline_color : color -> declaration
(** [outline_color value] sets the CSS outline-color property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/outline-color>
      MDN: outline-color *)

val outline_offset : length -> declaration
(** [outline_offset value] sets the CSS outline-offset property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/outline-offset>
      MDN: outline-offset *)

val border_top_style : border_style -> declaration
(** [border_top_style s] sets the CSS border-top-style property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-style>
      MDN: border-top-style *)

val border_right_style : border_style -> declaration
(** [border_right_style s] sets the CSS border-right-style property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-style>
      MDN: border-right-style *)

val border_bottom_style : border_style -> declaration
(** [border_bottom_style s] sets the CSS border-bottom-style property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-style>
      MDN: border-bottom-style *)

val border_left_style : border_style -> declaration
(** [border_left_style s] sets the CSS border-left-style property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-style>
      MDN: border-left-style *)

val border_left_width : length -> declaration
(** [border_left_width len] sets the CSS border-left-width property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-width>
      MDN: border-left-width *)

val border_top_width : length -> declaration
(** [border_top_width len] sets the CSS border-top-width property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-width>
      MDN: border-top-width *)

val border_right_width : length -> declaration
(** [border_right_width len] sets the CSS border-right-width property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-width>
      MDN: border-right-width *)

val border_bottom_width : length -> declaration
(** [border_bottom_width len] sets the CSS border-bottom-width property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-width>
      MDN: border-bottom-width *)

val border_top_color : color -> declaration
(** [border_top_color c] sets the CSS border-top-color property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-color>
      MDN: border-top-color *)

val border_right_color : color -> declaration
(** [border_right_color c] sets the CSS border-right-color property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-color>
      MDN: border-right-color *)

val border_bottom_color : color -> declaration
(** [border_bottom_color c] sets the CSS border-bottom-color property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-color>
      MDN: border-bottom-color *)

val border_left_color : color -> declaration
(** [border_left_color c] sets the CSS border-left-color property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-color>
      MDN: border-left-color *)

(** CSS border-collapse values *)
type border_collapse = Collapse | Separate | Inherit

val border_collapse : border_collapse -> declaration
(** [border_collapse value] sets the CSS border-collapse property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-collapse>
      MDN: border-collapse *)

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
  | Matrix of float * float * float * float * float * float
  | Matrix3d of
      float
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
      * float
  | Perspective of length
  | None
  | Inherit
  | Var of transform list var

val transform : transform list -> declaration
(** [transform values] sets the CSS transform property with a list of
    transformations.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/transform>
      MDN: transform *)

val transform_origin : string -> declaration
(** [transform_origin value] sets the CSS transform-origin property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/transform-origin>
      MDN: transform-origin *)

val rotate : angle -> declaration
(** [rotate value] sets the CSS rotate property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/rotate> MDN: rotate
*)

(** CSS scale property values *)
type scale =
  | String of string
  | Num of float
  | Pct of float
  | None
  | Vars of transform_scale var list

val scale : scale -> declaration
(** [scale value] sets the CSS scale property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/scale> MDN: scale *)

val perspective : length -> declaration
(** [perspective value] sets the CSS perspective property (3D transforms).
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/perspective>
      MDN: perspective *)

val perspective_origin : string -> declaration
(** [perspective_origin value] sets the CSS perspective-origin property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/perspective-origin>
      MDN: perspective-origin *)

(** CSS transform-style values *)
type transform_style = Flat | Preserve_3d | Inherit

val transform_style : transform_style -> declaration
(** [transform_style value] sets the CSS transform-style property (3D
    transforms).
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/transform-style>
      MDN: transform-style *)

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
(** [transition value] sets the CSS transition property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/transition>
      MDN: transition *)

val transition_timing_function : timing_function -> declaration
(** [transition_timing_function value] sets the CSS transition-timing-function
    property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/transition-timing-function>
*)

val transition_delay : duration -> declaration
(** [transition_delay value] sets the CSS transition-delay property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/transition-delay> *)

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

val box_shadow : box_shadow list -> declaration
(** [box_shadow values] sets the CSS box-shadow property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/box-shadow>
      MDN: box-shadow *)

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
  | Var of filter var  (** Custom filter variable *)

val filter : filter list -> declaration
(** [filter values] sets the CSS filter property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/filter> MDN: filter
*)

val backdrop_filter : filter list -> declaration
(** [backdrop_filter values] sets the CSS backdrop-filter property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/backdrop-filter>
      MDN: backdrop-filter *)

val clip_path : string -> declaration
(** [clip_path value] sets the CSS clip-path property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/clip-path>
      MDN: clip-path *)

val mask : string -> declaration
(** [mask value] sets the CSS mask property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/mask> MDN: mask *)

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
(** [cursor value] sets the CSS cursor property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/cursor> MDN: cursor
*)

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
(** [pointer_events value] sets the CSS pointer-events property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/pointer-events>
      MDN: pointer-events *)

val user_select : user_select -> declaration
(** [user_select value] sets the CSS user-select property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/user-select>
      MDN: user-select *)

val resize : resize -> declaration
(** [resize value] sets the CSS resize property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/resize> MDN: resize
*)

(** {2:container_containment Container Queries & Containment}

    CSS container queries and containment features for component-based
    responsive design and performance optimization through layout isolation.

    @see <https://www.w3.org/TR/css-contain-3/> CSS Containment Module Level 3
    @see <https://www.w3.org/TR/css-contain-2/> CSS Containment Module Level 2
*)

(** [aspect_ratio value] sets the CSS aspect-ratio property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/aspect-ratio>
      MDN: aspect-ratio *)

(** CSS container-type values *)
type container_type = Normal | Size | Inline_size | Inherit

val container_type : container_type -> declaration
(** [container_type value] sets the CSS container-type property for container
    queries.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/container-type>
      MDN: container-type *)

val container_name : string -> declaration
(** [container_name value] sets the CSS container-name property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/container-name>
      MDN: container-name *)

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
(** [contain value] sets the CSS contain property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/contain> MDN: contain
*)

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
(** [webkit_appearance value] sets the -webkit-appearance property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/-webkit-appearance>
      MDN: -webkit-appearance *)

val webkit_font_smoothing : webkit_font_smoothing -> declaration
(** [webkit_font_smoothing value] sets the -webkit-font-smoothing property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/-webkit-font-smoothing>
      MDN: -webkit-font-smoothing *)

val moz_osx_font_smoothing : moz_osx_font_smoothing -> declaration
(** [moz_osx_font_smoothing value] sets the -moz-osx-font-smoothing property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/-moz-osx-font-smoothing>
      MDN: -moz-osx-font-smoothing *)

val webkit_tap_highlight_color : color -> declaration
(** [webkit_tap_highlight_color value] sets the -webkit-tap-highlight-color
    property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/-webkit-tap-highlight-color>
      MDN: -webkit-tap-highlight-color. *)

val webkit_text_decoration : text_decoration -> declaration
(** [webkit_text_decoration value] sets the -webkit-text-decoration property. *)

val webkit_text_decoration_color : color -> declaration
(** [webkit_text_decoration_color color] sets the -webkit-text-decoration-color
    property. *)

val webkit_line_clamp : int -> declaration
val webkit_box_orient : webkit_box_orient -> declaration

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
  | Angle : angle kind
  | Transform_scale : transform_scale kind
  | Box_shadow : box_shadow kind
  | Content : content kind

type meta
(** The type for CSS variable metadata. *)

val meta : unit -> ('a -> meta) * (meta -> 'a option)
(** [meta ()] creates an injection/projection pair for metadata. The injection
    function converts a value to metadata, the projection function attempts to
    extract the value back. *)

val var_ref :
  ?fallback:'a -> ?default:'a -> ?layer:string -> ?meta:meta -> string -> 'a var
(** [var_ref ?fallback ?default ?layer ?meta name] creates a CSS variable
    reference. This is primarily for the CSS parser to create var() references.

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
(** [var ?fallback ?layer name kind value] creates a CSS custom property
    declaration and returns a variable handle.

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
(** [custom_property ?layer name value] creates a CSS custom property
    declaration.

    For type-safe variable declarations and usage, prefer using the {!var} API
    which provides compile-time checking and automatic variable management.

    @param layer Optional CSS layer name for the custom property
    @param name CSS custom property name (must start with --)
    @param value CSS value as string

    Example: [custom_property "--primary-color" "#3b82f6"]

    @see {!var} Type-safe CSS variable API *)

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
      @see {{:https://developer.mozilla.org/en-US/docs/Web/CSS} MDN: CSS} *)

val pp : ?minify:bool -> ?optimize:bool -> ?mode:mode -> t -> string
(** [pp] is {!to_string}. *)

val string_of_property : _ property -> string
(** [string_of_property prop] converts a property to its CSS string
    representation. *)

type any_var = V : 'a var -> any_var

val vars_of_rules : rule list -> any_var list
(** [vars_of_rules rules] extracts all CSS variable referenced in the rules'
    declarations, returning them sorted and deduplicated. *)

val vars_of_media_queries : media_rule list -> any_var list
(** [vars_of_media_queries media_queries] extracts all CSS variable referenced
    in the media queries' rules, returning them sorted and deduplicated. *)

(** [vars_of_container_queries container_queries] extracts all CSS variable
    referenced in the container queries' rules, returning them sorted and
    deduplicated. *)

val vars_of_stylesheet : t -> any_var list
(** [vars_of_stylesheet stylesheet] extracts all CSS variable referenced in the
    entire stylesheet, returning them sorted and deduplicated. *)

val any_var_name : any_var -> string
(** [any_var_name v] returns the name of a CSS variable (with -- prefix). *)

val analyze_declarations : declaration list -> any_var list
(** [analyze_declarations declarations] extracts typed CSS variables from
    declarations. *)

val extract_custom_declarations : declaration list -> declaration list
(** [extract_custom_declarations decls] returns only the custom property
    declarations from a list. *)

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
(** Type representing statistics about a CSS stylesheet *)

type stats = {
  rules : int;  (** Number of top-level rules *)
  layer_rules : int;  (** Total number of rules in all layers *)
  layers : layer_stats list;  (** Per-layer rule counts and sample selectors *)
  media_queries : int;  (** Number of media query blocks *)
  container_queries : int;  (** Number of container query blocks *)
  top_selectors : string list;  (** First few top-level selectors as examples *)
}

val stats : t -> stats
(** [stats stylesheet] computes statistics about the stylesheet, including rule
    counts per layer and total counts for various CSS constructs. *)

val pp_stats : stats -> string
(** [pp_stats stats] pretty-prints stylesheet statistics in a human-readable
    format without using Printf/Format to minimize js_of_ocaml bundle size. *)

val pp_stats_diff : before:stats -> after:stats -> string
(** [pp_stats_diff ~before ~after] pretty-prints the difference between two sets
    of statistics, showing the optimization results. Useful for comparing before
    and after optimization. *)

val stylesheet_rules : t -> rule list
(** [stylesheet_rules stylesheet] returns the top-level rules of a stylesheet.
*)

val will_change : string -> declaration
(** [will_change value] sets the CSS will-change property for performance
    optimization.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/will-change>
      MDN: will-change. *)

(** CSS isolation values *)
type isolation = Auto | Isolate | Inherit

val isolation : isolation -> declaration
(** [isolation value] sets the CSS isolation property for stacking context
    control.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/isolation>
      MDN: isolation. *)

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
(** [list_style_type lst] sets the CSS list-style-type property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/list-style-type> *)

val list_style_image : list_style_image -> declaration
(** [list_style_image img] sets the CSS list-style-image property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/list-style-image>
      MDN: list-style-image. *)

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
(** [table_layout value] sets the CSS table-layout property. *)

val vertical_align : vertical_align -> declaration
(** [vertical_align value] sets the CSS vertical-align property. *)

(** {2:svg_properties SVG Properties}

    Properties specific to SVG rendering and styling. *)

(** SVG paint values for fill and stroke properties *)
type svg_paint =
  | None  (** No paint *)
  | Current_color  (** Current color value *)
  | Color of color  (** Specific color value *)

val stroke : svg_paint -> declaration
(** [stroke value] sets the SVG stroke property. *)

val stroke_width : length -> declaration
(** [stroke_width value] sets the SVG stroke-width property. *)

(** {2:scroll_touch Scroll & Touch}

    Properties for scroll behavior and touch interaction. *)

(** CSS touch-action values *)
type touch_action = Auto | None | Pan_x | Pan_y | Manipulation | Inherit

(** CSS scroll-snap-type values *)
type scroll_snap_type =
  | None
  | X
  | Y
  | Block
  | Inline
  | Both
  | Mandatory
  | Proximity

(** CSS scroll-snap-align values *)
type scroll_snap_align = None | Start | End | Center

val touch_action : touch_action -> declaration
(** [touch_action value] sets the CSS touch-action property. *)

val scroll_snap_type : scroll_snap_type -> declaration
(** [scroll_snap_type value] sets the CSS scroll-snap-type property. *)

val scroll_snap_align : scroll_snap_align -> declaration
(** [scroll_snap_align value] sets the CSS scroll-snap-align property. *)

(** {2:misc_properties Miscellaneous}

    Other properties that don't fit into specific categories. *)

val forced_color_adjust : forced_color_adjust -> declaration
(** [forced_color_adjust value] sets the CSS forced-color-adjust property. *)

val quotes : string -> declaration
(** [quotes value] sets the CSS quotes property. *)

type appearance = None | Auto | Button | Textfield | Menulist | Inherit

val appearance : appearance -> declaration
(** [appearance value] sets the CSS appearance property. *)

(** [border value] sets the CSS border shorthand property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border> MDN: border.
*)

val tab_size : int -> declaration
(** [tab_size value] sets the CSS tab-size property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/tab-size>
      MDN: tab-size. *)

(** [webkit_text_size_adjust value] sets the -webkit-text-size-adjust property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/text-size-adjust>
      MDN: text-size-adjust. *)

(** [font_feature_settings value] sets the CSS font-feature-settings property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/font-feature-settings>
      MDN: font-feature-settings. *)

val font_variation_settings : font_variation_settings -> declaration
(** [font_variation_settings value] sets the CSS font-variation-settings
    property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/font-variation-settings>
      MDN: font-variation-settings. *)

(** [border_spacing len] sets the CSS border-spacing property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-spacing>
      MDN: border-spacing. *)

(** [list_style value] sets the CSS list-style shorthand property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/list-style>
      MDN: list-style. *)

(** [cursor c] sets the CSS cursor property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/cursor>
      MDN: cursor. MDN: user-select. *)

val custom_declaration_name : declaration -> string option
(** [custom_declaration_name decl] returns the variable name if the declaration
    is a custom property declaration, None otherwise. *)

val custom_declaration_layer : declaration -> string option
(** [custom_declaration_layer decl] returns the declared layer for a custom
    property declaration if present (e.g., "theme" or "utilities"). Returns
    [None] for non-custom declarations or when no layer metadata is attached. *)

val deduplicate_declarations : declaration list -> declaration list
(** [deduplicate_declarations declarations] removes duplicate declarations,
    keeping the last occurrence. It also automatically duplicates properties
    that have known browser bugs requiring duplication for correct rendering
    (e.g., -webkit-text-decoration-color). *)

val inline_style_of_declarations :
  ?optimize:bool -> ?minify:bool -> ?mode:mode -> declaration list -> string
(** [inline_style_of_declarations declarations] converts a list of declarations
    to an inline style string. *)

(**/**)

module Pp = Pp
