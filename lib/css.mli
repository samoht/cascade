(** Typed CSS construction.

    This library provides types and functions to construct CSS declarations,
    rules and stylesheets. It avoids stringly-typed CSS by keeping close to the
    CSS syntax and specifications.

    The main notions are:
    - A {!type:declaration} is a property/value pair.
    - A {!type:rule} couples a selector with declarations.
    - A {!type:t} is a stylesheet (sequence of rules and at-rules; internal
      representation is not exposed).
    - Values are typed (e.g., {!type:length}, {!type:color}); invalid constructs
      raise [Invalid_argument].

    Minimal example:
    {[
      open Css

      (* Build a ".btn" rule and render a stylesheet from it. *)
      let button =
        rule ~selector:(Selector.class_ "btn")
          [ display Inline_block
          ; background_color (hex "#3b82f6")
          ; color (hex "#ffffff")
          ; padding (Rem 0.5)
          ; border_radius (Rem 0.375)
          ]
      in
      to_string (v [ button ])
    ]}

    Custom properties:
    {[
      let def, v = var "primary-color" Color (hex "#3b82f6") in
      let root = rule ~selector:(Selector.pseudo_class "root") [ def ] in
      let card = rule ~selector:(Selector.class_ "card") [ color (Var v) ] in
      to_string (v [ root; card ])
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

    See {:https://www.w3.org/Style/CSS/specs.en.html W3C CSS Specifications} and
    {:https://developer.mozilla.org/en-US/docs/Web/CSS MDN CSS Documentation}.
*)

(** {1 Core Concepts}

    Core CSS system setup and construction tools for building stylesheets. *)

(** {2:css_selectors CSS Selectors}

    Structured representation of CSS selectors for targeting HTML elements.

    See {:https://www.w3.org/TR/selectors-4/ CSS Selectors Level 4}. *)

module Selector = Selector

(** {2:css_rules CSS Rules and Stylesheets}

    Core building blocks for CSS rules and stylesheet construction.

    See {:https://www.w3.org/TR/css-syntax-3/ CSS Syntax Module Level 3}. *)

type declaration
(** Abstract type for CSS declarations (property-value pairs). *)

type statement
(** The type for CSS statements. *)

val rule :
  selector:Selector.t -> ?nested:statement list -> declaration list -> statement
(** [rule ~selector declarations] creates a CSS rule statement with the given
    selector and declarations. *)

val statement_selector : statement -> Selector.t option
(** [statement_selector stmt] returns [Some selector] if the statement is a
    rule, [None] otherwise. *)

val statement_declarations : statement -> declaration list option
(** [statement_declarations stmt] returns [Some declarations] if the statement
    is a rule, [None] otherwise. *)

val statement_nested : statement -> statement list option
(** [statement_nested stmt] returns [Some nested] if the statement is a rule
    with nested statements, [None] otherwise. *)

val is_rule : statement -> bool
(** [is_rule stmt] returns [true] if the statement is a rule, [false] otherwise.
*)

val as_rule :
  statement -> (Selector.t * declaration list * statement list) option
(** [as_rule stmt] returns [Some (selector, declarations, nested)] if the
    statement is a rule, [None] otherwise. *)

val as_layer : statement -> (string option * statement list) option
(** [as_layer stmt] returns [Some (name, statements)] if the statement is a
    layer, [None] otherwise. *)

val as_media : statement -> (string * statement list) option
(** [as_media stmt] returns [Some (condition, statements)] if the statement is a
    media query, [None] otherwise. *)

val as_container : statement -> (string option * string * statement list) option
(** [as_container stmt] returns [Some (name, condition, statements)] if the
    statement is a container query, [None] otherwise. *)

val as_supports : statement -> (string * statement list) option
(** [as_supports stmt] returns [Some (condition, statements)] if the statement
    is a supports query, [None] otherwise. *)

(** Existential type for property information that preserves type safety *)
type property_info =
  | Property_info : {
      name : string;
      syntax : 'a Variables.syntax;
      inherits : bool;
      initial_value : 'a option;
    }
      -> property_info

val as_property : statement -> property_info option
(** [as_property stmt] returns [Some (Property_info {...})] if the statement is
    a [@property] declaration, [None] otherwise. The existential type preserves
    the relationship between syntax type and initial value type. *)

val as_property_legacy :
  statement -> (string * string * bool * string option) option
(** Legacy version that returns strings. Deprecated: use [as_property] instead.
*)

(** {2:at_rules At-Rules}

    At-rules are CSS statements that instruct CSS how to behave. They begin with
    an at sign (@) followed by an identifier and include everything up to the
    next semicolon or CSS block.

    See {:https://www.w3.org/TR/css-conditional-3/ CSS Conditional Rules Module
    Level 3} and {:https://developer.mozilla.org/en-US/docs/Web/CSS/At-rule MDN
    At-rules}. *)

(** {2:stylesheet_construction Stylesheet Construction}

    Tools for building complete CSS stylesheets from rules and declarations. *)

type t
(** The type for CSS stylesheets. *)

val empty : t
(** [empty] is an empty stylesheet. *)

val concat : t list -> t
(** [concat stylesheets] concatenates multiple stylesheets into one. *)

val v : statement list -> t
(** [v statements] creates a stylesheet from a list of statements. *)

val of_statements : statement list -> t
(** [of_statements statements] creates a stylesheet from a list of statements.
*)

val rules : t -> statement list
(** [rules t] returns the top-level rule statements from the stylesheet. *)

val statements : t -> statement list
(** [statements t] returns all top-level statements from the stylesheet. *)

val media_queries : t -> (string * statement list) list
(** [media_queries t] returns media queries and their rule statements. *)

val layers : t -> string list
(** [layers t] returns the layer names from the stylesheet. *)

val media : condition:string -> statement list -> statement
(** [media ~condition statements] creates a [@media] statement with the given
    condition. *)

val layer : ?name:string -> statement list -> statement
(** [layer ?name statements] creates a [@layer] statement with the given
    statements. *)

val layer_decl : string list -> statement
(** [layer_decl names] creates a [@layer] declaration statement that declares
    layer names without any content (e.g.,
    [@layer theme, base, components, utilities;]). *)

val layer_of : ?name:string -> t -> t
(** [layer_of ?name stylesheet] wraps an entire stylesheet in [@layer],
    preserving @supports and other at-rules within it. *)

val container : ?name:string -> condition:string -> statement list -> statement
(** [container ?name ~condition statements] creates a [@container] statement
    with the given statements. *)

val supports : condition:string -> statement list -> statement
(** [supports ~condition statements] creates a [@supports] statement with the
    given condition. *)

(** {1 Declarations}

    Core value types and declaration building blocks. *)

(** {2:variables Custom Properties (Variables) *)

type 'a var
(** The type of CSS variable holding values of type ['a]. *)

val var_name : 'a var -> string
(** [var_name v] is [v]'s variable name (without [--]). *)

val var_layer : 'a var -> string option
(** [var_layer v] is the optional layer where [v] is defined. *)

type any_var = V : 'a var -> any_var  (** The type of CSS variables. *)

val vars_of_rules : statement list -> any_var list
(** [vars_of_rules statements] extracts all CSS variables referenced in rule
    statements' declarations, returning them sorted and deduplicated. *)

val vars_of_declarations : declaration list -> any_var list
(** [vars_of_declarations decls] extracts all CSS variables referenced in the
    declarations list. *)

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

(** {2:core_types Core Types & Calculations}

    Fundamental types for CSS values, variables, and calculations that underpin
    the entire CSS system.

    See {:https://www.w3.org/TR/css-variables-1/ CSS Custom Properties for
    Cascading Variables Module Level 1} and
    {:https://www.w3.org/TR/css-values-3/ CSS Values and Units Module Level 3}.
*)

(** CSS calc operations. *)
type calc_op = Add | Sub | Mul | Div

(** CSS calc values. *)
type 'a calc =
  | Var of 'a var  (** CSS variable *)
  | Val of 'a
  | Num of float  (** Unitless number *)
  | Expr of 'a calc * calc_op * 'a calc

type 'a fallback =
  | Empty  (** Empty fallback: var(--name,) *)
  | None  (** No fallback: var(--name) *)
  | Fallback of 'a  (** Value fallback: var(--name, value) *)

(** {2:values CSS Values & Units}

    Core value types used across CSS properties.

    See {:https://www.w3.org/TR/css-values-4/ CSS Values and Units Module Level
    4}. *)

(** CSS length values.

    Supports absolute, relative, viewport (including dynamic/large/small),
    character-based units, keywords, and calculated expressions. *)
type length =
  | Px of float
  | Cm of float
  | Mm of float
  | Q of float
  | In of float
  | Pt of float
  | Pc of float
  | Rem of float
  | Em of float
  | Ex of float
  | Cap of float
  | Ic of float
  | Rlh of float
  | Pct of float
  | Vw of float
  | Vh of float
  | Vmin of float
  | Vmax of float
  | Vi of float
  | Vb of float
  | Dvh of float
  | Dvw of float
  | Dvmin of float
  | Dvmax of float
  | Lvh of float
  | Lvw of float
  | Lvmin of float
  | Lvmax of float
  | Svh of float
  | Svw of float
  | Svmin of float
  | Svmax of float
  | Ch of float  (** Character units *)
  | Lh of float  (** Line height units *)
  | Auto
  | Zero
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Fit_content  (** fit-content keyword *)
  | Content  (** content keyword *)
  | Max_content  (** max-content keyword *)
  | Min_content  (** min-content keyword *)
  | From_font  (** from-font keyword for text-decoration-thickness *)
  | Function of string
      (** CSS functions like clamp(), minmax(), min(), max() *)
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

  val var : ?default:'a -> ?fallback:'a fallback -> string -> 'a calc
  (** [var ?default ?fallback name] is a variable reference for [calc]
      expressions. Example: [var "spacing"] or
      [var ~fallback:(Rem 1.2) "tw-leading"]. *)

  val float : float -> length calc
  (** [float f] is a numeric value for [calc] expressions. *)

  val infinity : length calc
  (** [infinity] is the CSS infinity value for [calc] expressions. *)

  val px : float -> length calc
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
  | Alice_blue
  | Antique_white
  | Aquamarine
  | Azure
  | Beige
  | Bisque
  | Blanched_almond
  | Blue_violet
  | Brown
  | Burlywood
  | Cadet_blue
  | Chartreuse
  | Chocolate
  | Coral
  | Cornflower_blue
  | Cornsilk
  | Crimson
  | Dark_blue
  | Dark_cyan
  | Dark_goldenrod
  | Dark_gray
  | Dark_green
  | Dark_grey
  | Dark_khaki
  | Dark_magenta
  | Dark_olive_green
  | Dark_orange
  | Dark_orchid
  | Dark_red
  | Dark_salmon
  | Dark_sea_green
  | Dark_slate_blue
  | Dark_slate_gray
  | Dark_slate_grey
  | Dark_turquoise
  | Dark_violet
  | Deep_pink
  | Deep_sky_blue
  | Dim_gray
  | Dim_grey
  | Dodger_blue
  | Firebrick
  | Floral_white
  | Forest_green
  | Gainsboro
  | Ghost_white
  | Gold
  | Goldenrod
  | Green_yellow
  | Honeydew
  | Hot_pink
  | Indian_red
  | Indigo
  | Ivory
  | Khaki
  | Lavender
  | Lavender_blush
  | Lawn_green
  | Lemon_chiffon
  | Light_blue
  | Light_coral
  | Light_cyan
  | Light_goldenrod_yellow
  | Light_gray
  | Light_green
  | Light_grey
  | Light_pink
  | Light_salmon
  | Light_sea_green
  | Light_sky_blue
  | Light_slate_gray
  | Light_slate_grey
  | Light_steel_blue
  | Light_yellow
  | Lime_green
  | Linen
  | Medium_aquamarine
  | Medium_blue
  | Medium_orchid
  | Medium_purple
  | Medium_sea_green
  | Medium_slate_blue
  | Medium_spring_green
  | Medium_turquoise
  | Medium_violet_red
  | Midnight_blue
  | Mint_cream
  | Misty_rose
  | Moccasin
  | Navajo_white
  | Old_lace
  | Olive_drab
  | Orange_red
  | Orchid
  | Pale_goldenrod
  | Pale_green
  | Pale_turquoise
  | Pale_violet_red
  | Papaya_whip
  | Peach_puff
  | Peru
  | Plum
  | Powder_blue
  | Rebecca_purple
  | Rosy_brown
  | Royal_blue
  | Saddle_brown
  | Salmon
  | Sandy_brown
  | Sea_green
  | Sea_shell
  | Sienna
  | Sky_blue
  | Slate_blue
  | Slate_gray
  | Slate_grey
  | Snow
  | Spring_green
  | Steel_blue
  | Tan
  | Thistle
  | Tomato
  | Turquoise
  | Violet
  | Wheat
  | White_smoke
  | Yellow_green

(** CSS channel values (for RGB) *)
type channel =
  | Int of int (* 0–255, legacy/comma syntax *)
  | Num of float (* 0–255, modern/space syntax *)
  | Pct of float (* 0%–100% *)
  | Var of channel var

(** CSS alpha values (for HSL/HWB/etc) *)
type alpha =
  | None
  | Num of float (* Number value (0-1) *)
  | Pct of float (* Percentage value (0%-100%) *)
  | Var of alpha var

(** CSS hue values (for HSL/HWB) *)
type hue =
  | Unitless of float (* Unitless number, defaults to degrees *)
  | Angle of Values.angle (* Explicit angle unit *)
  | Var of hue var

(** CSS color component values *)
type component =
  | Num of float
  | Pct of float
  | Angle of hue (* for color(lch ...) / color(lab ...) syntaxes *)
  | Var of component var
  | Calc of component calc

(** CSS percentage values *)
type percentage =
  | Pct of float (* 0%–100% as a % token *)
  | Var of percentage var
  | Calc of percentage calc (* calc(...) that resolves to a % *)

type length_percentage =
  | Length of length
  | Percentage of percentage
  | Var of length_percentage var
  | Calc of length_percentage calc

(** CSS hue interpolation options *)
type hue_interpolation = Shorter | Longer | Increasing | Decreasing | Default

(** CSS color values. *)
type color =
  | Hex of { hash : bool; value : string }
      (** hash indicates if # was present *)
  | Rgb of { r : channel; g : channel; b : channel }
  | Rgba of { r : channel; g : channel; b : channel; a : alpha }
  | Hsl of { h : hue; s : percentage; l : percentage; a : alpha }
  | Hwb of { h : hue; w : percentage; b : percentage; a : alpha }
  | Color of { space : color_space; components : component list; alpha : alpha }
  | Oklch of { l : percentage; c : float; h : hue; alpha : alpha }
      (** OKLCH color space *)
  | Oklab of { l : percentage; a : float; b : float; alpha : alpha }
      (** Oklab color space *)
  | Lch of { l : percentage; c : float; h : hue; alpha : alpha }
      (** LCH color space *)
  | Named of color_name  (** Named colors like Red, Blue, etc. *)
  | Var of color var
  | Current
  | Transparent
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Mix of {
      in_space : color_space option; (* None => default per spec *)
      hue : hue_interpolation;
      color1 : color;
      percent1 : percentage option;
      color2 : color;
      percent2 : percentage option;
    }

val hex : string -> color
(** [hex s] is a hexadecimal color. Accepts with or without leading [#].
    Examples: [hex "#3b82f6"], [hex "ffffff"]. *)

val rgb : int -> int -> int -> color
(** [rgb r g b] is an RGB color (0–255 components). *)

val rgba : int -> int -> int -> float -> color
(** [rgba r g b a] is an RGBA color with alpha in [0., 1.]. *)

val hsl : float -> float -> float -> color
(** [hsl h s l] is an HSL color with h in degrees, s and l in percentages
    (0-100). *)

val hsla : float -> float -> float -> float -> color
(** [hsla h s l a] is an HSLA color with alpha in [0., 1.]. *)

val hwb : float -> float -> float -> color
(** [hwb h w b] is an HWB color with h in degrees, w and b in percentages
    (0-100). *)

val hwba : float -> float -> float -> float -> color
(** [hwba h w b a] is an HWB color with alpha in [0., 1.]. *)

val oklch : float -> float -> float -> color
(** [oklch l c h] is an OKLCH color. L in percentage (0-100), h in degrees. *)

val oklcha : float -> float -> float -> float -> color
(** [oklcha l c h a] is an OKLCH color with alpha in [0., 1.]. *)

val oklab : float -> float -> float -> color
(** [oklab l a b] is an OKLAB color. L in percentage (0-100). *)

val oklaba : float -> float -> float -> float -> color
(** [oklaba l a b alpha] is an OKLAB color with alpha in [0., 1.]. *)

val lch : float -> float -> float -> color
(** [lch l c h] is an LCH color. L in percentage (0-100), h in degrees. *)

val lcha : float -> float -> float -> float -> color
(** [lcha l c h a] is an LCH color with alpha in [0., 1.]. *)

val color_name : color_name -> color
(** [color_name n] is a named color as defined in the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/named-color} CSS Color
     specification}. *)

val current_color : color
(** [current_color] is the CSS
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/color_value#currentcolor_keyword}
     currentcolor} value. *)

val transparent : color
(** [transparent] is the CSS
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/color_value#transparent_keyword}
     transparent} value. *)

val color_mix :
  ?in_space:color_space ->
  ?hue:hue_interpolation ->
  ?percent1:int ->
  ?percent2:int ->
  color ->
  color ->
  color
(** [color_mix ?in_space ?percent1 ?percent2 c1 c2] is a
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/color_value/color-mix}
     color-mix} value. Defaults: [in_space = Srgb], [percent1 = None],
    [percent2 = None]. *)

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
  | Var of blend_mode var

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

val declaration_is_important : declaration -> bool
(** [declaration_is_important decl] returns [true] if [decl] has the
    [!important] flag. *)

val declaration_name : declaration -> string
(** [declaration_name decl] returns the property name of [decl]. *)

val declaration_value : ?minify:bool -> declaration -> string
(** [declaration_value ~minify decl] returns the value of [decl] as a string. If
    [minify] is [true] (default: [false]), the output is minified. *)

val string_of_declaration : ?minify:bool -> declaration -> string
(** [string_of_declaration ~minify decl] converts a declaration to its string
    representation. If [minify] is [true] (default: [false]), the output is
    minified. *)

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

val padding : length list -> declaration
(** [padding values] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding} padding}
    shorthand property. Accepts 1-4 values. *)

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

val margin : length list -> declaration
(** [margin values] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin} margin}
    shorthand property. Accepts 1-4 values. *)

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
(** [box_sizing sizing] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/box-sizing} box-sizing}
    property. *)

val aspect_ratio : aspect_ratio -> declaration
(** [aspect_ratio ratio] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/aspect-ratio}
     aspect-ratio} property. *)

(** {2:logical_properties Logical Properties}

    CSS Logical Properties provide writing-mode-relative property equivalents
    for physical properties. These adapt to different writing directions and
    text orientations.

    @see <https://www.w3.org/TR/css-logical-1/>
      CSS Logical Properties and Values Level 1 *)

type border_width =
  | Thin
  | Medium
  | Thick
  | Px of float
  | Rem of float
  | Em of float
  | Ch of float
  | Vh of float
  | Vw of float
  | Vmin of float
  | Vmax of float
  | Pct of float
  | Zero
  | Auto
  | Max_content
  | Min_content
  | Fit_content
  | From_font
  | Calc of border_width calc
  | Var of border_width var
  | Inherit

val border_inline_start_width : border_width -> declaration
(** [border_inline_start_width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-start-width}
     border-inline-start-width} property. *)

val border_inline_end_width : border_width -> declaration
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
(** [padding_inline_start len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-inline-start}
     padding-inline-start} property. *)

val padding_inline_end : length -> declaration
(** [padding_inline_end len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-inline-end}
     padding-inline-end} property. *)

val padding_inline : length -> declaration
(** [padding_inline len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-inline}
     padding-inline} shorthand property. *)

val padding_block : length -> declaration
(** [padding_block len] is the
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
  | Inherit
  | Initial
  | Unset

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
(** [z_index z] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/z-index} z-index}
    property. *)

val z_index_auto : declaration
(** [z_index_auto] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/z-index} z-index}
    property set to [auto]. *)

(** CSS isolation values *)
type isolation = Auto | Isolate | Inherit

val isolation : isolation -> declaration
(** [isolation iso] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/isolation} isolation}
    property for stacking context control. *)

val visibility : visibility -> declaration
(** [visibility v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/visibility} visibility}
    property. *)

(** CSS float side values. *)
type float_side = None | Left | Right | Inline_start | Inline_end | Inherit

val float : float_side -> declaration
(** [float side] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/float} float} property.
*)

(** CSS clear values. *)
type clear = None | Left | Right | Both | Inline_start | Inline_end

val clear : clear -> declaration
(** [clear clr] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/clear} clear} property.
*)

val overflow : overflow -> declaration
(** [overflow ov] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overflow} overflow}
    property. *)

val overflow_x : overflow -> declaration
(** [overflow_x ov] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-x} overflow-x}
    property. *)

val overflow_y : overflow -> declaration
(** [overflow_y ov] is the
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
(** [content c] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/content} content}
    property. *)

(** CSS object-fit values *)
type object_fit = Fill | Contain | Cover | None | Scale_down | Inherit

val object_fit : object_fit -> declaration
(** [object_fit fit] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/object-fit} object-fit}
    property. *)

type position_2d =
  | Center
  | Left_top
  | Left_center
  | Left_bottom
  | Right_top
  | Right_center
  | Right_bottom
  | Center_top
  | Center_bottom
  | XY of length * length
  | Inherit

val object_position : position_2d -> declaration
(** [object_position pos] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/object-position}
     object-position} property. *)

(** CSS text-overflow values *)
type text_overflow = Clip | Ellipsis | String of string | Inherit

val text_overflow : text_overflow -> declaration
(** [text_overflow ov] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-overflow}
     text-overflow} property. *)

(** CSS text-wrap values *)
type text_wrap = Wrap | No_wrap | Balance | Pretty | Inherit

val text_wrap : text_wrap -> declaration
(** [text_wrap wrap] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-wrap} text-wrap}
    property. *)

(** CSS backface-visibility values *)
type backface_visibility = Visible | Hidden | Inherit

val backface_visibility : backface_visibility -> declaration
(** [backface_visibility vis] is the
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
(** [content_visibility vis] is the
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
  | Space
  | Round
  | No_repeat
  | Repeat_x
  | Repeat_y
  | Repeat_repeat
  | Repeat_space
  | Repeat_round
  | Repeat_no_repeat
  | Space_repeat
  | Space_space
  | Space_round
  | Space_no_repeat
  | Round_repeat
  | Round_space
  | Round_round
  | Round_no_repeat
  | No_repeat_repeat
  | No_repeat_space
  | No_repeat_round
  | No_repeat_no_repeat
  | Inherit

(** CSS background-size values. *)
type background_size =
  | Auto
  | Cover
  | Contain
  | Px of float
  | Rem of float
  | Em of float
  | Pct of float
  | Vw of float
  | Vh of float
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

type background_box = Border_box | Padding_box | Content_box | Text | Inherit

type background_shorthand = {
  color : color option;
  image : background_image option;
  position : position_2d option;
  size : background_size option;
  repeat : background_repeat option;
  attachment : background_attachment option;
  clip : background_box option;
  origin : background_box option;
}
(** CSS background shorthand values. *)

type background =
  | Inherit
  | Initial
  | None
  | Var of background var
  | Shorthand of background_shorthand  (** CSS background values. *)

val background_shorthand :
  ?color:color ->
  ?image:background_image ->
  ?position:position_2d ->
  ?size:background_size ->
  ?repeat:background_repeat ->
  ?attachment:background_attachment ->
  ?clip:background_box ->
  ?origin:background_box ->
  unit ->
  background
(** [background_shorthand ?color ?image ?position ?size ?repeat ?attachment
     ?clip ?origin ()] is the background shorthand.
    - [color]: background color
    - [image]: background image (url or gradient)
    - [position]: image position
    - [size]: image size (cover, contain, or specific size)
    - [repeat]: repeat behavior (repeat, no-repeat, etc.)
    - [attachment]: scroll behavior (scroll, fixed, local)
    - [clip]: clipping area
    - [origin]: positioning area. *)

val color : color -> declaration
(** [color c] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/color} color} property.
*)

val background_color : color -> declaration
(** [background_color c] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-color}
     background-color} property. *)

val background_image : background_image -> declaration
(** [background_image img] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-image}
     background-image} property. *)

val background_position : position_2d list -> declaration
(** [background_position pos] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-position}
     background-position} property. *)

val background_size : background_size -> declaration
(** [background_size sz] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-size}
     background-size} property. *)

val background_repeat : background_repeat -> declaration
(** [background_repeat rep] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-repeat}
     background-repeat} property. *)

val background_attachment : background_attachment -> declaration
(** [background_attachment att] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-attachment}
     background-attachment} property. *)

val opacity : float -> declaration
(** [opacity op] is the
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

(** CSS flex basis values. *)
type flex_basis =
  | Auto
  | Content
  | Px of float
  | Cm of float
  | Mm of float
  | Q of float
  | In of float
  | Pt of float
  | Pc of float
  | Rem of float
  | Em of float
  | Ex of float
  | Cap of float
  | Ic of float
  | Rlh of float
  | Pct of float
  | Vw of float
  | Vh of float
  | Vmin of float
  | Vmax of float
  | Vi of float
  | Vb of float
  | Dvh of float
  | Dvw of float
  | Dvmin of float
  | Dvmax of float
  | Lvh of float
  | Lvw of float
  | Lvmin of float
  | Lvmax of float
  | Svh of float
  | Svw of float
  | Svmin of float
  | Svmax of float
  | Ch of float
  | Lh of float
  | Num of float
  | Zero
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Fit_content
  | Max_content
  | Min_content
  | From_font
  | Var of flex_basis var
  | Calc of flex_basis calc

(** CSS flex shorthand values. *)
type flex =
  | Initial  (** 0 1 auto *)
  | Auto  (** 1 1 auto *)
  | None  (** 0 0 auto *)
  | Grow of float  (** Single grow value *)
  | Basis of flex_basis  (** 1 1 <flex-basis> *)
  | Grow_shrink of float * float  (** grow shrink 0% *)
  | Full of float * float * flex_basis  (** grow shrink basis *)

(** CSS align-content values.
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/align-content} MDN:
     align-content} *)
type align_content =
  | Normal
  | Baseline
  | First_baseline
  | Last_baseline
  | Center
  | Start
  | End
  | Flex_start
  | Flex_end
  | Left
  | Right
  (* Safe content position values *)
  | Safe_center
  | Safe_start
  | Safe_end
  | Safe_flex_start
  | Safe_flex_end
  | Safe_left
  | Safe_right
  (* Unsafe content position values *)
  | Unsafe_center
  | Unsafe_start
  | Unsafe_end
  | Unsafe_flex_start
  | Unsafe_flex_end
  | Unsafe_left
  | Unsafe_right
  | Space_between
  | Space_around
  | Space_evenly
  | Stretch

(** CSS align-items values.
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/align-items} MDN:
     align-items} *)
type align_items =
  | Normal
  | Stretch
  | Baseline
  | First_baseline
  | Last_baseline
  | Center
  | Start
  | End
  | Self_start
  | Self_end
  | Flex_start
  | Flex_end
  | Unsafe_center
  | Unsafe_start
  | Unsafe_end
  | Unsafe_self_start
  | Unsafe_self_end
  | Unsafe_flex_start
  | Unsafe_flex_end
  | Anchor_center

(** CSS justify-content values.
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-content} MDN:
     justify-content} *)
type justify_content =
  | Normal
  | Center
  | Start
  | End
  | Flex_start
  | Flex_end
  | Left
  | Right
  | Unsafe_center
  | Unsafe_start
  | Unsafe_end
  | Unsafe_flex_start
  | Unsafe_flex_end
  | Unsafe_left
  | Unsafe_right
  | Space_between
  | Space_around
  | Space_evenly
  | Stretch

(** CSS align-self values.
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/align-self} MDN:
     align-self} *)
type align_self =
  | Auto
  | Normal
  | Stretch
  | Baseline
  | First_baseline
  | Last_baseline
  | Center
  | Start
  | End
  | Self_start
  | Self_end
  | Flex_start
  | Flex_end
  | Unsafe_center
  | Unsafe_start
  | Unsafe_end
  | Unsafe_self_start
  | Unsafe_self_end
  | Unsafe_flex_start
  | Unsafe_flex_end

(** CSS justify-items values.
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-items} MDN:
     justify-items} *)
type justify_items =
  | Normal
  | Stretch
  | Baseline
  | First_baseline
  | Last_baseline
  | Center
  | Start
  | End
  | Self_start
  | Self_end
  | Flex_start
  | Flex_end
  | Left
  | Right
  | Safe_center
  | Safe_start
  | Safe_end
  | Safe_self_start
  | Safe_self_end
  | Safe_flex_start
  | Safe_flex_end
  | Safe_left
  | Safe_right
  | Unsafe_center
  | Unsafe_start
  | Unsafe_end
  | Unsafe_self_start
  | Unsafe_self_end
  | Unsafe_flex_start
  | Unsafe_flex_end
  | Unsafe_left
  | Unsafe_right
  | Anchor_center
  | Legacy

(** CSS justify-self values.
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-self} MDN:
     justify-self} *)
type justify_self =
  | Auto
  | Normal
  | Stretch
  | Baseline
  | First_baseline
  | Last_baseline
  | Center
  | Start
  | End
  | Self_start
  | Self_end
  | Flex_start
  | Flex_end
  | Left
  | Right
  (* Safe self position values *)
  | Safe_center
  | Safe_start
  | Safe_end
  | Safe_self_start
  | Safe_self_end
  | Safe_flex_start
  | Safe_flex_end
  | Safe_left
  | Safe_right
  (* Unsafe self position values *)
  | Unsafe_center
  | Unsafe_start
  | Unsafe_end
  | Unsafe_self_start
  | Unsafe_self_end
  | Unsafe_flex_start
  | Unsafe_flex_end
  | Unsafe_left
  | Unsafe_right
  | Anchor_center
  | Inherit

(** {2:alignment_properties Alignment Properties}
    CSS Box Alignment properties for flexbox and grid layouts. *)

val align_content : align_content -> declaration
(** [align_content alignment] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/align-content}
     align-content} property. Sets how content is aligned along the cross axis.
    Common values: Normal, Baseline, Center, Start, End, Space_between, Stretch.
*)

val justify_content : justify_content -> declaration
(** [justify_content alignment] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-content}
     justify-content} property. Sets how content is aligned along the main axis.
    Common values: Normal, Center, Start, End, Space_between, Space_around,
    Stretch. *)

val align_items : align_items -> declaration
(** [align_items alignment] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/align-items}
     align-items} property. Sets alignment for all items along the cross axis.
    Common values: Normal, Baseline, Center, Start, End, Stretch. *)

val align_self : align_self -> declaration
(** [align_self alignment] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/align-self} align-self}
    property. Overrides align-items for an individual item. Common values: Auto,
    Normal, Baseline, Center, Start, End, Stretch. *)

val justify_items : justify_items -> declaration
(** [justify_items justification] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-items}
     justify-items} property. Sets default justification for all items. Common
    values: Normal, Baseline, Center, Start, End, Stretch, Legacy. *)

val justify_self : justify_self -> declaration
(** [justify_self justification] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-self}
     justify-self} property. Sets justification for an individual item on the
    inline (main) axis. *)

val flex_direction : flex_direction -> declaration
(** [flex_direction direction] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex-direction}
     flex-direction} property. *)

val flex_wrap : flex_wrap -> declaration
(** [flex_wrap wrap] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex-wrap} flex-wrap}
    property. *)

val flex : flex -> declaration
(** [flex flex] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex} flex} shorthand
    property. *)

val flex_grow : float -> declaration
(** [flex_grow amount] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex-grow} flex-grow}
    property. *)

val flex_shrink : float -> declaration
(** [flex_shrink amount] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex-shrink}
     flex-shrink} property. *)

val flex_basis : length -> declaration
(** [flex_basis basis] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex-basis} flex-basis}
    property. *)

val order : int -> declaration
(** [order order] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/order} order} property.
*)

type gap = { row_gap : length option; column_gap : length option }
(** CSS gap shorthand type. *)

val gap : gap -> declaration
(** [gap gap] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/gap} gap} property
    shorthand (applies to both row and column gaps). *)

val row_gap : length -> declaration
(** [row_gap gap] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/row-gap} row-gap}
    property. *)

val column_gap : length -> declaration
(** [column_gap gap] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/column-gap} column-gap}
    property. *)

(** {2:grid Grid Layout}

    Properties for CSS Grid Layout, a two-dimensional layout system optimized
    for user interface design with explicit row and column positioning.

    @see <https://www.w3.org/TR/css-grid-1/> CSS Grid Layout Module Level 1
    @see <https://www.w3.org/TR/css-grid-2/> CSS Grid Layout Module Level 2 *)

(** CSS grid template values *)
type grid_template =
  | None
  | Px of float
  | Rem of float
  | Em of float
  | Pct of float
  | Vw of float
  | Vh of float
  | Vmin of float
  | Vmax of float
  | Zero
  | Fr of float
  | Auto
  | Min_content
  | Max_content
  | Inherit
  | Min_max of grid_template * grid_template
  | Fit_content of length
  | Repeat of int * grid_template list
  | Tracks of grid_template list
  | Named_tracks of (string option * grid_template) list
  | Subgrid
  | Masonry

(** CSS grid line values *)
type grid_line =
  | Auto  (** auto *)
  | Num of int  (** 1, 2, 3, ... or -1, -2, ... *)
  | Name of string  (** "header-start", "main-end", etc. *)
  | Span of int  (** span 2, span 3, etc. *)

val grid_template_columns : grid_template -> declaration
(** [grid_template_columns cols] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template-columns}
     grid-template-columns} property. *)

val grid_template_rows : grid_template -> declaration
(** [grid_template_rows rows] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template-rows}
     grid-template-rows} property. *)

val grid_template_areas : string -> declaration
(** [grid_template_areas areas] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template-areas}
     grid-template-areas} property. *)

val grid_template : grid_template -> declaration
(** [grid_template template] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template}
     grid-template} shorthand property. *)

val grid_auto_columns : grid_template -> declaration
(** [grid_auto_columns cols] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-auto-columns}
     grid-auto-columns} property. *)

val grid_auto_rows : grid_template -> declaration
(** [grid_auto_rows rows] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-auto-rows}
     grid-auto-rows} property. *)

(** CSS grid-auto-flow values *)
type grid_auto_flow = Row | Column | Dense | Row_dense | Column_dense

val grid_auto_flow : grid_auto_flow -> declaration
(** [grid_auto_flow flow] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-auto-flow}
     grid-auto-flow} property. *)

val grid_row_start : grid_line -> declaration
(** [grid_row_start start] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-row-start}
     grid-row-start} property. *)

val grid_row_end : grid_line -> declaration
(** [grid_row_end end_] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-row-end}
     grid-row-end} property. *)

val grid_column_start : grid_line -> declaration
(** [grid_column_start start] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-column-start}
     grid-column-start} property. *)

val grid_column_end : grid_line -> declaration
(** [grid_column_end end_] is the
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
(** [grid_area area] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-area} grid-area}
    property. *)

(** CSS place-items values *)
type place_items =
  | Normal
  | Start
  | End
  | Center
  | Stretch
  | Align_justify of align_items * justify_items
  | Inherit

val place_items : place_items -> declaration
(** [place_items items] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/place-items}
     place-items} shorthand property. *)

(** CSS place-content values *)
type place_content =
  | Normal
  | Start
  | End
  | Center
  | Stretch
  | Space_between
  | Space_around
  | Space_evenly
  | Align_justify of align_content * justify_content
  | Inherit

val place_content : place_content -> declaration
(** [place_content content] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/place-content}
     place-content} shorthand property. *)

val place_self : align_self * justify_self -> declaration
(** [place_self self_] is the
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
type text_align =
  | Left
  | Right
  | Center
  | Justify
  | Start
  | End
  | Match_parent
  | Inherit

(** CSS text decoration values. *)
type text_decoration_line = Underline | Overline | Line_through

type text_decoration_style = Solid | Double | Dotted | Dashed | Wavy | Inherit

type text_decoration_shorthand = {
  lines : text_decoration_line list;
  style : text_decoration_style option;
  color : color option;
  thickness : length option;
}

type text_decoration =
  | None
  | Shorthand of text_decoration_shorthand
  | Inherit
  | Var of text_decoration var

val text_decoration_shorthand :
  ?lines:text_decoration_line list ->
  ?style:text_decoration_style ->
  ?color:color ->
  ?thickness:length ->
  unit ->
  text_decoration
(** [text_decoration_shorthand ?lines ?style ?color ?thickness ()] is the
    text-decoration shorthand.
    - [lines]: decoration lines (underline, overline, line-through)
    - [style]: line style (solid, double, dotted, dashed, wavy)
    - [color]: decoration color
    - [thickness]: line thickness. *)

(** CSS font style values. *)
type font_style = Normal | Italic | Oblique | Inherit

(** CSS text transform values. *)
type text_transform =
  | None
  | Capitalize
  | Uppercase
  | Lowercase
  | Full_width
  | Full_size_kana
  | Inherit
  | Var of text_transform var

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
  (* Arbitrary font family name *)
  | Name of string
  (* CSS variables *)
  | Var of font_family var
  (* List of fonts for composition *)
  | List of font_family list

val font_family : font_family -> declaration
(** [font_family fonts] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-family}
     font-family} property. *)

val font_families : font_family list -> declaration
(** [font_families fonts] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-family}
     font-family} property from a comma-separated list. *)

val font_size : length -> declaration
(** [font_size size] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-size} font-size}
    property. *)

val font_weight : font_weight -> declaration
(** [font_weight weight] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-weight}
     font-weight} property. *)

val font_style : font_style -> declaration
(** [font_style style] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-style} font-style}
    property. *)

(** CSS line-height values *)
type line_height =
  | Normal
  | Px of float
  | Rem of float
  | Em of float
  | Pct of float
  | Num of float
  | Inherit
  | Var of line_height var
  | Calc of line_height calc

val line_height : line_height -> declaration
(** [line_height height] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/line-height}
     line-height} property. Accepts [Normal], Length values (e.g., `Length (Rem
    1.5)`), Number values (e.g., `Num 1.5`), or Percentage values. *)

val letter_spacing : length -> declaration
(** [letter_spacing spacing] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/letter-spacing}
     letter-spacing} property. *)

val word_spacing : length -> declaration
(** [word_spacing spacing] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/word-spacing}
     word-spacing} property. *)

val text_align : text_align -> declaration
(** [text_align align] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-align} text-align}
    property. *)

val text_decoration : text_decoration -> declaration
(** [text_decoration decoration] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration}
     text-decoration} property. *)

val text_transform : text_transform -> declaration
(** [text_transform transform] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-transform}
     text-transform} property. *)

val text_indent : length -> declaration
(** [text_indent indent] is the
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
(** [white_space space] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/white-space}
     white-space} property. *)

(** CSS word-break values *)
type word_break = Normal | Break_all | Keep_all | Break_word | Inherit

val word_break : word_break -> declaration
(** [word_break break] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/word-break} word-break}
    property. *)

val text_decoration_color : color -> declaration
(** [text_decoration_color color] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-color}
     text-decoration-color} property. *)

val text_size_adjust : string -> declaration
(** [text_size_adjust adjust] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-size-adjust}
     text-size-adjust} property. *)

(* CSS text-decoration-style values. *)

val text_decoration_style : text_decoration_style -> declaration
(** [text_decoration_style style] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-style}
     text-decoration-style} property. *)

val text_underline_offset : string -> declaration
(** [text_underline_offset offset] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-underline-offset}
     text-underline-offset} property. *)

(** CSS overflow-wrap values *)
type overflow_wrap = Normal | Break_word | Anywhere | Inherit

val overflow_wrap : overflow_wrap -> declaration
(** [overflow_wrap wrap] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-wrap}
     overflow-wrap} property. *)

(** CSS hyphens values *)
type hyphens = None | Manual | Auto | Inherit

val hyphens : hyphens -> declaration
(** [hyphens hyphens] is the
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
(** [font_stretch stretch] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-stretch}
     font-stretch} property. *)

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
(** [font_variant_numeric numeric] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-variant-numeric}
     font-variant-numeric} property using a list of tokens or a composed value.
*)

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
(** [font_feature_settings settings] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-feature-settings}
     font-feature-settings} property. *)

(** CSS shadow values *)
type shadow =
  | Shadow of {
      inset : bool;
      h_offset : length;
      v_offset : length;
      blur : length option;
      spread : length option;
      color : color option;
    }
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of shadow var
  | List of shadow list

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
    v_offset=0px, blur=0px, spread=0px, color=Transparent. *)

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
    v_offset=0px, blur=0px, spread=0px, color=Transparent. *)

(** CSS text-shadow values *)
type text_shadow =
  | None
  | Text_shadow of {
      h_offset : length;
      v_offset : length;
      blur : length option;
      color : color option;
    }
  | Inherit

val text_shadow : text_shadow -> declaration
(** [text_shadow shadow] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-shadow}
     text-shadow} property. *)

val font : string -> declaration
(** [font spec] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font} font} shorthand
    property. *)

(** CSS direction values *)
type direction = Ltr | Rtl | Inherit

val direction : direction -> declaration
(** [direction dir] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/direction} direction}
    property. *)

(** CSS unicode-bidi values *)
type unicode_bidi =
  | Normal
  | Embed
  | Isolate
  | Bidi_override
  | Isolate_override
  | Plaintext
  | Inherit

val unicode_bidi : unicode_bidi -> declaration
(** [unicode_bidi bidi] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/unicode-bidi}
     unicode-bidi} property. *)

(** CSS writing-mode values *)
type writing_mode =
  | Horizontal_tb
  | Vertical_rl
  | Vertical_lr
  | Sideways_lr
  | Sideways_rl
  | Inherit

val writing_mode : writing_mode -> declaration
(** [writing_mode mode] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/writing-mode}
     writing-mode} property. *)

val text_decoration_thickness : length -> declaration
(** [text_decoration_thickness thick] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-thickness}
     text-decoration-thickness} property. *)

(** CSS text-decoration-skip-ink values *)
type text_decoration_skip_ink = Auto | None | All | Inherit

val text_decoration_skip_ink : text_decoration_skip_ink -> declaration
(** [text_decoration_skip_ink skip] is the
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

type border_shorthand = {
  width : border_width option;
  style : border_style option;
  color : color option;
}
(** CSS border shorthand type. *)

type border = Inherit | Initial | None | Shorthand of border_shorthand

(** CSS outline style values. *)
type outline_style =
  | None
  | Solid
  | Dashed
  | Dotted
  | Double
  | Groove
  | Ridge
  | Inset
  | Outset
  | Auto
  | Inherit

val border_shorthand :
  ?width:border_width -> ?style:border_style -> ?color:color -> unit -> border
(** [border_shorthand ?width ?style ?color ()] is the border shorthand.
    - [width]: border width (thin, medium, thick, or specific length)
    - [style]: border style (solid, dashed, dotted, etc.)
    - [color]: border color. *)

val border :
  ?width:border_width ->
  ?style:border_style ->
  ?color:color ->
  unit ->
  declaration
(** [border border] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border} border}
    shorthand property. *)

val border_width : border_width -> declaration
(** [border_width width] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-width}
     border-width} property. *)

val border_style : border_style -> declaration
(** [border_style style] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-style}
     border-style} property. *)

val border_color : color -> declaration
(** [border_color color] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-color}
     border-color} property. *)

val border_radius : length -> declaration
(** [border_radius radius] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-radius}
     border-radius} property. *)

val border_top : string -> declaration
(** [border_top border] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-top} border-top}
    property. *)

val border_right : string -> declaration
(** [border_right border] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-right}
     border-right} property. *)

val border_bottom : string -> declaration
(** [border_bottom border] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom}
     border-bottom} property. *)

val border_left : string -> declaration
(** [border_left border] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-left}
     border-left} property. *)

val outline : string -> declaration
(** [outline outline] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/outline} outline}
    property. *)

val outline_width : length -> declaration
(** [outline_width width] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/outline-width}
     outline-width} property. *)

val outline_style : outline_style -> declaration
(** [outline_style style] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/outline-style}
     outline-style} property. *)

val outline_color : color -> declaration
(** [outline_color color] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/outline-color}
     outline-color} property. *)

val outline_offset : length -> declaration
(** [outline_offset offset] is the
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

val border_left_width : border_width -> declaration
(** [border_left_width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-width}
     border-left-width} property. *)

val border_top_width : border_width -> declaration
(** [border_top_width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-width}
     border-top-width} property. *)

val border_right_width : border_width -> declaration
(** [border_right_width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-width}
     border-right-width} property. *)

val border_bottom_width : border_width -> declaration
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

(** CSS transform values *)
type transform =
  | Translate of length * length option
  | Translate_x of length
  | Translate_y of length
  | Translate_z of length
  | Translate_3d of length * length * length
  | Rotate of angle
  | Rotate_x of angle
  | Rotate_y of angle
  | Rotate_z of angle
  | Rotate_3d of float * float * float * angle
  | Scale of float * float option
  | Scale_x of float
  | Scale_y of float
  | Scale_z of float
  | Scale_3d of float * float * float
  | Skew of angle * angle option
  | Skew_x of angle
  | Skew_y of angle
  | Matrix of float * float * float * float * float * float
  | Matrix_3d of
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
  | Var of transform var
  | List of transform list

val transform : transform list -> declaration
(** [transform values] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transform} transform}
    property with a list of transformations. *)

type transform_origin =
  | Center
  | Left_top
  | Left_center
  | Left_bottom
  | Right_top
  | Right_center
  | Right_bottom
  | Center_top
  | Center_bottom
  | Top_left
  | Top_right
  | Bottom_left
  | Bottom_right
  | XY of length * length
  | XYZ of length * length * length
  | Inherit  (** Transform origin (2D or 3D). *)

val origin : length -> length -> transform_origin
(** [origin x y] is a transform-origin helper for 2D positions. *)

val origin3d : length -> length -> length -> transform_origin
(** [origin3d x y z] is a transform-origin helper for 3D positions. *)

val transform_origin : transform_origin -> declaration
(** [transform_origin origin] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transform-origin}
     transform-origin} property. *)

val rotate : angle -> declaration
(** [rotate angle] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/rotate} rotate}
    property. *)

(** CSS scale property values *)
type scale =
  | X of float
  | XY of float * float
  | XYZ of float * float * float
  | None
  | Var of scale var

val scale : scale -> declaration
(** [scale scale] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scale} scale} property.
*)

val perspective : length -> declaration
(** [perspective perspective] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/perspective}
     perspective} property (3D transforms). *)

val perspective_origin : string -> declaration
(** [perspective_origin origin] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/perspective-origin}
     perspective-origin} property. *)

(** CSS transform-style values *)
type transform_style = Flat | Preserve_3d | Inherit

val transform_style : transform_style -> declaration
(** [transform_style style] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transform-style}
     transform-style} property (3D transforms). *)

(** CSS steps direction values. *)
type steps_direction =
  | Jump_start
  | Jump_end
  | Jump_none
  | Jump_both
  | Start
  | End

(** CSS animation timing function values. *)
type timing_function =
  | Ease
  | Linear
  | Ease_in
  | Ease_out
  | Ease_in_out
  | Step_start
  | Step_end
  | Steps of int * steps_direction option
  | Cubic_bezier of float * float * float * float

(** CSS duration values. *)
type duration =
  | Ms of float  (** milliseconds *)
  | S of float  (** seconds *)
  | Var of duration var  (** CSS variable reference *)

(** CSS transition property values. *)
type transition_property = All | None | Property of string

type transition_shorthand = {
  property : transition_property;
  duration : duration option;
  timing_function : timing_function option;
  delay : duration option;
}
(** CSS transition shorthand values. *)

type transition =
  | Inherit
  | Initial
  | None
  | Var of transition var
  | Shorthand of transition_shorthand  (** CSS transition values. *)

val transition_shorthand :
  ?property:transition_property ->
  ?duration:duration ->
  ?timing_function:timing_function ->
  ?delay:duration ->
  unit ->
  transition
(** [transition_shorthand ?property ?duration ?timing_function ?delay ()] is the
    transition shorthand.
    - [property]: CSS property to transition (defaults to All)
    - [duration]: transition duration
    - [timing_function]: easing function (ease, linear, ease-in, etc.)
    - [delay]: delay before transition starts. *)

val transition : transition -> declaration
(** [transition transition] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transition} transition}
    property. *)

val transitions : transition list -> declaration
(** [transitions values] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transition} transition}
    property from a comma-separated list. *)

val transition_timing_function : timing_function -> declaration
(** [transition_timing_function tf] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transition-timing-function}
     transition-timing-function} property. *)

val transition_duration : duration -> declaration
(** [transition_duration dur] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transition-duration}
     transition-duration} property. *)

val transition_delay : duration -> declaration
(** [transition_delay delay] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transition-delay}
     transition-delay} property. *)

(** CSS animation fill mode values *)
type animation_fill_mode = None | Forwards | Backwards | Both

(** CSS animation direction values *)
type animation_direction = Normal | Reverse | Alternate | Alternate_reverse

(** CSS animation play state values *)
type animation_play_state = Running | Paused

(** CSS animation iteration count values *)
type animation_iteration_count = Num of float | Infinite

type animation_shorthand = {
  name : string option; (* Optional animation name, defaults to None *)
  duration : duration option;
  timing_function : timing_function option;
  delay : duration option;
  iteration_count : animation_iteration_count option;
  direction : animation_direction option;
  fill_mode : animation_fill_mode option;
  play_state : animation_play_state option;
}
(** CSS animation shorthand values *)

type animation =
  | Inherit
  | Initial
  | None
  | Var of animation var
  | Shorthand of animation_shorthand  (** CSS animation values *)

val animation_shorthand :
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
(** [animation_shorthand ?name ?duration ?timing_function ?delay
     ?iteration_count ?direction ?fill_mode ?play_state ()] is the animation
    shorthand.
    - [name]: animation name
    - [duration]: animation duration
    - [timing_function]: easing function
    - [delay]: delay before animation starts
    - [iteration_count]: number of iterations (or Infinite)
    - [direction]: animation direction (normal, reverse, alternate, etc.)
    - [fill_mode]: how styles apply before/after animation
    - [play_state]: running or paused. *)

val animation : animation -> declaration
(** [animation props] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation} animation}
    shorthand property. *)

val animation_name : string -> declaration
(** [animation_name name] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-name}
     animation-name} property. *)

val animation_duration : duration -> declaration
(** [animation_duration dur] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-duration}
     animation-duration} property. *)

val animation_timing_function : timing_function -> declaration
(** [animation_timing_function tf] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function}
     animation-timing-function} property. *)

val animation_delay : duration -> declaration
(** [animation_delay delay] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-delay}
     animation-delay} property. *)

val animation_iteration_count : animation_iteration_count -> declaration
(** [animation_iteration_count count] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-iteration-count}
     animation-iteration-count} property. *)

val animation_direction : animation_direction -> declaration
(** [animation_direction dir] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-direction}
     animation-direction} property. *)

val animation_fill_mode : animation_fill_mode -> declaration
(** [animation_fill_mode mode] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-fill-mode}
     animation-fill-mode} property. *)

val animation_play_state : animation_play_state -> declaration
(** [animation_play_state state] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-play-state}
     animation-play-state} property. *)

(** {2:visual_effects Visual Effects}

    Properties for visual effects including shadows, filters, clipping, and
    other advanced rendering features.

    @see <https://www.w3.org/TR/filter-effects-1/> Filter Effects Module Level 1
    @see <https://www.w3.org/TR/css-masking-1/> CSS Masking Module Level 1 *)

val box_shadow : shadow -> declaration
(** [box_shadow shadow] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/box-shadow} box-shadow}
    property. *)

val box_shadows : shadow list -> declaration
(** [box_shadows values] is the
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
(** [clip clip] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/clip} clip} property
    (deprecated). *)

val clip_path : string -> declaration
(** [clip_path path] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/clip-path} clip-path}
    property. *)

val mask : string -> declaration
(** [mask mask] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/mask} mask} property. *)

val mix_blend_mode : blend_mode -> declaration
(** [mix_blend_mode mode] is the
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
  | None
  | Context_menu
  | Help
  | Pointer
  | Progress
  | Wait
  | Cell
  | Crosshair
  | Text
  | Vertical_text
  | Alias
  | Copy
  | Move
  | No_drop
  | Not_allowed
  | Grab
  | Grabbing
  | E_resize
  | N_resize
  | Ne_resize
  | Nw_resize
  | S_resize
  | Se_resize
  | Sw_resize
  | W_resize
  | Ew_resize
  | Ns_resize
  | Nesw_resize
  | Nwse_resize
  | Col_resize
  | Row_resize
  | All_scroll
  | Zoom_in
  | Zoom_out
  | Url of string * (float * float) option * cursor
  | Inherit

(** CSS user-select values. *)
type user_select = None | Auto | Text | All | Contain

(** CSS resize values. *)
type resize = None | Both | Horizontal | Vertical | Block | Inline | Inherit

val cursor : cursor -> declaration
(** [cursor cursor] is the
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
(** [pointer_events events] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/pointer-events}
     pointer-events} property. *)

val user_select : user_select -> declaration
(** [user_select select] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/user-select}
     user-select} property. *)

val resize : resize -> declaration
(** [resize resize] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/resize} resize}
    property. *)

(** {2:container_containment Container Queries & Containment}

    CSS container queries and containment features for component-based
    responsive design and performance optimization through layout isolation.

    @see <https://www.w3.org/TR/css-contain-3/> CSS Containment Module Level 3
    @see <https://www.w3.org/TR/css-contain-2/> CSS Containment Module Level 2
*)

(** [aspect_ratio ratio] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/aspect-ratio}
     aspect-ratio} property. *)

(** CSS container-type values *)
type container_type = Size | Inline_size | Scroll_state | Normal

val container_type : container_type -> declaration
(** [container_type type_] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/container-type}
     container-type} property for container queries. *)

val container_name : string -> declaration
(** [container_name name] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/container-name}
     container-name} property. *)

(** CSS contain values *)
type contain =
  | None
  | Strict
  | Content
  | Size
  | Layout
  | Style
  | Paint
  | List of contain list

val contain : contain -> declaration
(** [contain contain] is the
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
  | Inherit  (** Inherit from parent *)

(** CSS -webkit-font-smoothing values. *)
type webkit_font_smoothing =
  | Auto
  | None
  | Antialiased
  | Subpixel_antialiased
  | Inherit

(** CSS -moz-osx-font-smoothing values. *)
type moz_osx_font_smoothing = Auto | Grayscale | Inherit

(** CSS text-size-adjust values (including vendor prefixes). *)
type text_size_adjust = None | Auto | Pct of float | Inherit

val webkit_appearance : webkit_appearance -> declaration
(** [webkit_appearance app] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/-webkit-appearance}
     -webkit-appearance} property. *)

val webkit_font_smoothing : webkit_font_smoothing -> declaration
(** [webkit_font_smoothing smoothing] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/-webkit-font-smoothing}
     -webkit-font-smoothing} property. *)

val moz_osx_font_smoothing : moz_osx_font_smoothing -> declaration
(** [moz_osx_font_smoothing smoothing] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/-moz-osx-font-smoothing}
     -moz-osx-font-smoothing} property. *)

val webkit_tap_highlight_color : color -> declaration
(** [webkit_tap_highlight_color color] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/-webkit-tap-highlight-color}
     -webkit-tap-highlight-color} property. *)

val webkit_text_decoration : text_decoration -> declaration
(** [webkit_text_decoration decoration] is the WebKit-only
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration}
     -webkit-text-decoration} property. *)

val webkit_text_decoration_color : color -> declaration
(** [webkit_text_decoration_color color] is the WebKit-only
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-color}
     -webkit-text-decoration-color} property. *)

val webkit_line_clamp : int -> declaration
(** [webkit_line_clamp clamp] is the WebKit-only
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/-webkit-line-clamp}
     -webkit-line-clamp} property. *)

val webkit_box_orient : webkit_box_orient -> declaration
(** [webkit_box_orient orient] is the WebKit-only
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/-webkit-box-orient}
     -webkit-box-orient} property. *)

val webkit_hyphens : hyphens -> declaration
(** [webkit_hyphens hyphens] is the WebKit-only
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/hyphens}
     -webkit-hyphens} property. *)

val webkit_text_size_adjust : text_size_adjust -> declaration
(** [webkit_text_size_adjust adjust] is the WebKit-only
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-size-adjust}
     -webkit-text-size-adjust} property. *)

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
type list_style_image = None | Url of string | Inherit

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
  | Px of float
  | Rem of float
  | Em of float
  | Pct of float
  | Inherit

val table_layout : table_layout -> declaration
(** [table_layout value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/table-layout}
     table-layout} property. *)

val vertical_align : vertical_align -> declaration
(** [vertical_align value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/vertical-align}
     vertical-align} property. *)

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
  | Url of string * svg_paint option  (** url(#id) with optional fallback *)

val fill : svg_paint -> declaration
(** [fill paint] is the SVG fill property. *)

val stroke : svg_paint -> declaration
(** [stroke paint] is the SVG stroke property. *)

val stroke_width : length -> declaration
(** [stroke_width width] is the SVG stroke-width property. *)

(** {2:scroll_touch Scroll & Touch}

    Properties for scroll behavior and touch interaction. *)

(** CSS touch-action values *)
type touch_action = Auto | None | Pan_x | Pan_y | Manipulation | Inherit

(** CSS scroll-snap-strictness values *)
type scroll_snap_strictness =
  | Mandatory
  | Proximity
  | Var of scroll_snap_strictness var

(** CSS scroll-snap-type values *)
type scroll_snap_type =
  | None
  | X
  | Y
  | Block
  | Inline
  | Both
  | X_mandatory
  | Y_mandatory
  | Block_mandatory
  | Inline_mandatory
  | Both_mandatory
  | X_proximity
  | Y_proximity
  | Block_proximity
  | Inline_proximity
  | Both_proximity
  | Inherit

(** CSS scroll-snap-align values *)
type scroll_snap_align = None | Start | End | Center

val touch_action : touch_action -> declaration
(** [touch_action action] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/touch-action}
     touch-action} property. *)

val scroll_snap_type : scroll_snap_type -> declaration
(** [scroll_snap_type type_] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-snap-type}
     scroll-snap-type} property. *)

val scroll_snap_align : scroll_snap_align -> declaration
(** [scroll_snap_align align] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-snap-align}
     scroll-snap-align} property. *)

(** CSS scroll-snap-stop values *)
type scroll_snap_stop = Normal | Always | Inherit

val scroll_snap_stop : scroll_snap_stop -> declaration
(** [scroll_snap_stop stop] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-snap-stop}
     scroll-snap-stop} property. *)

(** CSS scroll behavior values *)
type scroll_behavior = Auto | Smooth | Inherit

val scroll_behavior : scroll_behavior -> declaration
(** [scroll_behavior behavior] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-behavior}
     scroll-behavior} property for smooth scrolling. *)

val scroll_margin : length -> declaration
(** [scroll_margin margin] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin}
     scroll-margin} property. *)

val scroll_margin_top : length -> declaration
(** [scroll_margin_top margin] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-top}
     scroll-margin-top} property. *)

val scroll_margin_right : length -> declaration
(** [scroll_margin_right margin] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-right}
     scroll-margin-right} property. *)

val scroll_margin_bottom : length -> declaration
(** [scroll_margin_bottom margin] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-bottom}
     scroll-margin-bottom} property. *)

val scroll_margin_left : length -> declaration
(** [scroll_margin_left margin] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-left}
     scroll-margin-left} property. *)

val scroll_padding : length -> declaration
(** [scroll_padding padding] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding}
     scroll-padding} property. *)

val scroll_padding_top : length -> declaration
(** [scroll_padding_top padding] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-top}
     scroll-padding-top} property. *)

val scroll_padding_right : length -> declaration
(** [scroll_padding_right padding] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-right}
     scroll-padding-right} property. *)

val scroll_padding_bottom : length -> declaration
(** [scroll_padding_bottom padding] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-bottom}
     scroll-padding-bottom} property. *)

val scroll_padding_left : length -> declaration
(** [scroll_padding_left padding] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-left}
     scroll-padding-left} property. *)

(** CSS overscroll behavior values *)
type overscroll_behavior = Auto | Contain | None | Inherit

val overscroll_behavior : overscroll_behavior -> declaration
(** [overscroll_behavior behavior] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overscroll-behavior}
     overscroll-behavior} property. *)

val overscroll_behavior_x : overscroll_behavior -> declaration
(** [overscroll_behavior_x behavior] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overscroll-behavior-x}
     overscroll-behavior-x} property. *)

val overscroll_behavior_y : overscroll_behavior -> declaration
(** [overscroll_behavior_y behavior] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overscroll-behavior-y}
     overscroll-behavior-y} property. *)

val accent_color : color -> declaration
(** [accent_color color] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/accent-color}
     accent-color} property for form controls. *)

val caret_color : color -> declaration
(** [caret_color color] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/caret-color}
     caret-color} property for the text input cursor. *)

(** {2:misc_properties Miscellaneous}

    Other properties that don't fit into specific categories. *)

val forced_color_adjust : forced_color_adjust -> declaration
(** [forced_color_adjust adjust] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/forced-color-adjust}
     forced-color-adjust} property. *)

val quotes : string -> declaration
(** [quotes quotes] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/quotes} quotes}
    property. *)

type appearance = None | Auto | Button | Textfield | Menulist | Inherit

val appearance : appearance -> declaration
(** [appearance app] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/appearance} appearance}
    property. *)

val tab_size : int -> declaration
(** [tab_size size] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/tab-size} tab-size}
    property. *)

val font_variation_settings : font_variation_settings -> declaration
(** [font_variation_settings settings] is the
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
  | Percentage : percentage kind
  | String : string kind
  | Duration : duration kind
  | Aspect_ratio : aspect_ratio kind
  | Border_style : border_style kind
  | Border : border kind
  | Font_weight : font_weight kind
  | Line_height : line_height kind
  | Font_family : font_family kind
  | Font_feature_settings : font_feature_settings kind
  | Font_variation_settings : font_variation_settings kind
  | Font_variant_numeric : font_variant_numeric kind
  | Font_variant_numeric_token : font_variant_numeric_token kind
  | Blend_mode : blend_mode kind
  | Scroll_snap_strictness : scroll_snap_strictness kind
  | Angle : angle kind
  | Shadow : shadow kind
  | Box_shadow : shadow kind
  | Content : content kind

val pp_kind_value : ('a kind * 'a) Pp.t
(** [pp_kind_value ctx (kind, value)] pretty-prints a typed value using the
    appropriate printer for its kind. *)

type meta
(** The type for CSS variable metadata. *)

val var_meta : 'a var -> meta option
(** [var_meta v] is the optional metadata associated with [v]. *)

val meta : unit -> ('a -> meta) * (meta -> 'a option)
(** [meta ()] is an injection/projection pair for metadata. The injection
    function converts a value to metadata, the projection function attempts to
    extract the value back. *)

val var_ref :
  ?fallback:'a fallback ->
  ?default:'a ->
  ?layer:string ->
  ?meta:meta ->
  string ->
  'a var
(** [var_ref ?fallback ?default ?layer ?meta name] is a CSS variable reference.
    This is primarily for the CSS parser to create var() references.

    - [name] is the variable name (without the -- prefix)
    - [fallback] is used inside var(--name, fallback) in CSS output
    - [default] is the resolved value when mode is Inline
    - [layer] is an optional CSS layer name
    - [meta] is optional metadata. *)

(** {2 CSS @property Support} *)

type 'a syntax =
  | Length : length syntax
  | Color : color syntax
  | Number : float syntax
  | Integer : int syntax
  | Percentage : percentage syntax
  | Length_percentage : length_percentage syntax
  | Angle : angle syntax
  | Time : duration syntax
  | Custom_ident : string syntax
  | String : string syntax
  | Url : string syntax
  | Image : string syntax
  | Transform_function : string syntax
  | Universal : string syntax
  (* Compound syntax types *)
  | Or : 'a syntax * 'b syntax -> ('a, 'b) Either.t syntax
  | Plus : 'a syntax -> 'a list syntax
  | Hash : 'a syntax -> 'a list syntax
  | Question : 'a syntax -> 'a option syntax
  | Brackets : string -> string syntax
      (** Type-safe syntax descriptors for CSS [@property] rules. *)

val property :
  name:string -> 'a syntax -> ?initial_value:'a -> ?inherits:bool -> unit -> t
(** [property ~name syntax ?initial_value ?inherits ()] creates a [@property]
    rule for registering a custom CSS property with type-safe syntax and initial
    value.

    Examples:
    - [property ~name:"--my-color" Variables.Color ~initial_value:(hex
       "#ff0000") ()]
    - [property ~name:"--my-size" Variables.Length ~initial_value:(Px 10.) ()]

    See {:https://developer.mozilla.org/en-US/docs/Web/CSS/@property MDN
    @property}. *)

val var :
  ?default:'a ->
  ?fallback:'a fallback ->
  ?layer:string ->
  ?meta:meta ->
  string ->
  'a kind ->
  'a ->
  declaration * 'a var
(** [var ?default ?fallback ?layer name kind value] returns a declaration and a
    variable handle.

    - [name] is the variable name without the [--] prefix
    - [kind] specifies the value type (Length, Color, Angle, Float, etc.)
    - [default] specifies the value to use in inline mode instead of var()
      reference
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

val meta_of_declaration : declaration -> meta option
(** [meta_of_declaration decl] extracts metadata from a declaration if it has
    any. *)

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

type mode =
  | Variables
  | Inline
      (** Rendering mode for CSS output.
          - [Variables]: Standard rendering with CSS custom properties support
          - [Inline]: For inline styles (no at-rules, variables expanded with
            their values) *)

val to_string :
  ?minify:bool -> ?optimize:bool -> ?mode:mode -> ?newline:bool -> t -> string
(** [to_string ?minify ?optimize ?mode ?newline stylesheet] renders a complete
    stylesheet to CSS.
    - If [minify] is [true], the output will be compact (no unnecessary
      whitespace).
    - If [optimize] is [true], rule-level optimizations are applied
      (deduplication, merging consecutive rules, combining identical rules).
    - [mode] controls variable layer emission behavior.
    - If [newline] is [true] (default), adds a trailing newline for POSIX
      compliance.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS> "MDN: CSS". *)

val pp :
  ?minify:bool -> ?optimize:bool -> ?mode:mode -> ?newline:bool -> t -> string
(** [pp] is {!to_string}. *)

type parse_error = Reader.parse_error

val pp_parse_error : parse_error -> string
(** [pp_parse_error error] formats a parse error as a string, including call
    stack if available. *)

val of_string : ?filename:string -> string -> (t, parse_error) result
(** [of_string ?filename css] parses a CSS string into a stylesheet. Returns
    [Error error] on invalid CSS. The optional [filename] parameter is used for
    error reporting (defaults to "<string>"). *)

(** {2:optimization Optimization}

    Tools for optimizing CSS output for performance and file size. *)

val optimize : t -> t
(** [optimize stylesheet] applies CSS optimizations to the stylesheet, including
    merging consecutive identical selectors and combining rules with identical
    properties. Preserves CSS cascade semantics. *)

val will_change : string -> declaration
(** [will_change value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/will-change}
     will-change} property for performance optimization. *)

val inline_style_of_declarations :
  ?optimize:bool ->
  ?minify:bool ->
  ?mode:mode ->
  ?newline:bool ->
  declaration list ->
  string
(** [inline_style_of_declarations declarations] converts a list of declarations
    to an inline style string. *)

(** {2 Pretty-printing functions for types} *)

val pp_display : display Pp.t
(** [pp_display] is the pretty printer for display values. *)

val pp_position : position Pp.t
(** [pp_position] is the pretty printer for position values. *)

val pp_length : ?always:bool -> length Pp.t
(** [pp_length ?always] is the pretty printer for length values. When [always]
    is true, units are always included even for zero values. *)

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

module Pp = Pp
(** Printer *)

(**/**)

module Reader = Reader
module Values = Values
module Properties = Properties
module Declaration = Declaration
module Variables = Variables
module Optimize = Optimize
module Stylesheet = Stylesheet
