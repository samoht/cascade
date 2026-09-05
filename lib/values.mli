(** CSS values and units: core types, printers, and parsers. *)

(** {1 Core Types} *)

include module type of Values_intf
(** Shared value/unit types exposed by both implementation and interface. *)

val var_ref :
  ?fallback:'a fallback ->
  ?default:'a ->
  ?layer:string ->
  ?meta:meta ->
  ?runtime:bool ->
  string ->
  'a var
(** [var_ref ?fallback ?default ?layer ?meta ?runtime name] creates a CSS
    variable reference to [--name]. With [~runtime:true] a context keeps the
    [var()] reference instead of folding it to its [default], so a runtime
    stylesheet or script can still override [--name]. *)

val syntax_fallback : string -> 'a fallback
(** [syntax_fallback s] parses [s] as a CSS declaration-value fallback for a
    [var()] reference. *)

val string_of_number_percentage : number_percentage -> string
(** [string_of_number_percentage value] serializes a number/percentage value for
    CSS custom-property initial values. *)

(** {1 Constructor Functions} *)

val hex : string -> color
(** [hex s] is the colour of the hex spelling [s], with or without the leading
    [#]. Raises [Invalid_argument] when [s] is not one of [#rgb], [#rrggbb],
    [#rgba] or [#rrggbbaa]: no colour is denoted, and returning one would put a
    plausible wrong colour into the output. See {!hex_opt} to decide. *)

val hex_opt : string -> color option
(** [hex_opt s] is {!val-hex} without the exception: the colour when [s] is a
    hex spelling, and nothing otherwise. *)

val rgb : ?alpha:float -> int -> int -> int -> color
(** [rgb ?alpha r g b] creates an RGB color with optional alpha. *)

val hsl : float -> float -> float -> color
(** [hsl h s l] creates an HSL color. *)

val hsla : float -> float -> float -> float -> color
(** [hsla h s l a] creates an HSLA color with alpha. *)

val hwb : float -> float -> float -> color
(** [hwb h w b] creates an HWB color. *)

val hwba : float -> float -> float -> float -> color
(** [hwba h w b a] creates an HWB color with alpha. *)

val oklch : float -> float -> float -> color
(** [oklch l c h] creates an OKLCH color. *)

val oklcha : float -> float -> float -> float -> color
(** [oklcha l c h a] creates an OKLCH color with alpha. *)

val oklch_none_hue : float -> float -> color
(** [oklch_none_hue l c] creates an OKLCH color whose hue is [none]. The hue of
    an achromatic color is powerless, and [none] keeps the component missing, so
    interpolation takes the other color's hue rather than 0. *)

val oklab : float -> float -> float -> color
(** [oklab l a b] creates an OKLAB color. *)

val oklaba : float -> float -> float -> float -> color
(** [oklaba l a b alpha] creates an OKLAB color with alpha. *)

val oklaba_none_zeros : float -> float -> float -> float -> color
(** [oklaba_none_zeros l a b alpha] is like {!val-oklaba} but uses [none] for
    zero a/b components. *)

val lch : float -> float -> float -> color
(** [lch l c h] creates an LCH color. *)

val lcha : float -> float -> float -> float -> color
(** [lcha l c h a] creates an LCH color with alpha. *)

val color_name : color_name -> color
(** [color_name n] creates a named color. *)

val with_alpha : color -> alpha -> color
(** [with_alpha c a] is [c] carrying the alpha [a], replacing whatever alpha [c]
    already spelled.

    CSS Color 4 sec. 4.1 gives an [<alpha-value>] slot to the functional colour
    notations and to no other. A hex spells its alpha as a fourth pair of
    digits, which holds a byte rather than a percentage or a [calc()], and a
    named colour has no channel at all, so a hex, a named colour and
    {!val-transparent} are re-spelled as the [rgb()] over the same sRGB
    channels: [red] becomes [rgb(255 0 0 / a)], and {!val-transparent} the
    [rgb(0 0 0 / a)] of sec. 6.3. A notation that already has the slot keeps
    both its notation and its channels. [light-dark()] resolves to exactly one
    of its two arguments, so the alpha goes onto both.

    A colour whose channels are known only at used-value time ([currentcolor], a
    [var()], a system colour, [color-mix()], [contrast-color()], [attr()], a
    relative colour) has nothing to write into and comes back unchanged, the
    answer {!gamut_map_color} gives one too. So do the CSS-wide keywords and
    [auto], which are not colours. *)

val equal_color : color -> color -> bool
(** [equal_color a b] tests colours for structural equality. Two spellings of
    one colour are two colours: [#fff] and [white] name the same sRGB, and each
    is its own node until {!normalize_color} folds it. *)

val hash_color : color -> int
(** [hash_color c] returns a hash consistent with {!equal_color}: two colours
    that are equal always return the same value, and the converse may fail on a
    collision, so use it as a cheap pre-filter before falling back to
    {!equal_color}. *)

val minify_color : color -> color
(** [minify_color c] converts named colors to hex when the hex form is shorter
    or equal length, matching Lightning CSS behavior. *)

val nonkeyword_color : color -> color
(** [nonkeyword_color c] re-spells a colour written as a bare keyword (a named
    colour, or transparent) as its hex form, leaving every other colour
    unchanged. A bare colour keyword is also a valid [<custom-ident>], so it
    must not be introduced when folding a colour inside an opaque
    custom-property token stream. *)

val gamut_map_color : color -> color
(** [gamut_map_color c] is the sRGB colour a display renders for the static
    colour [c], as a hex colour: CSS Color 4 sec. 14.2 reduces the chroma of a
    colour sRGB cannot hold until it fits, holding lightness and hue. A colour
    already inside the sRGB gamut keeps its bytes, and one that is not static (a
    [var()], [currentcolor], a [color-mix()] over either) is returned unchanged.

    Emission does not use this: the mapped colour is not the authored one, and a
    display shows the wider colour on hardware that has it. It is for a caller
    that has to commit to sRGB bytes. *)

val current_color : color
(** [current_color] is the CSS currentcolor value. *)

val transparent : color
(** [transparent] is the CSS transparent value. *)

val color_mix :
  ?in_space:color_space ->
  ?hue:hue_interpolation ->
  ?percent1:float ->
  ?percent2:float ->
  color ->
  color ->
  color
(** [color_mix ?in_space ?percent1 ?percent2 color1 color2] creates a
    color-mix() value. *)

val color_mix_var_percent :
  ?in_space:color_space ->
  ?hue:hue_interpolation ->
  var_name:string ->
  color ->
  color ->
  color
(** [color_mix_var_percent ?in_space ?hue ~var_name c1 c2] is like
    {!val-color_mix} but uses a CSS var reference for the first percentage. *)

val color_mix_var_pct_fallback :
  ?in_space:color_space ->
  ?hue:hue_interpolation ->
  var_name:string ->
  fallback:percentage fallback ->
  color ->
  color ->
  color
(** [color_mix_var_pct_fallback ?in_space ?hue ~var_name ~fallback c1 c2] is
    like {!val-color_mix_var_percent} but with an explicit fallback on the
    percentage variable. Used for named opacity modifiers where the fallback is
    either a concrete number ([Fallback (Num 0.5)]) or a theme variable
    reference ([Var_fallback "custom-opacity"]). *)

(** {1 Pretty-printing Functions} *)

val pp_length : ?always:bool -> length Pp.t
(** [pp_length ?always] pretty-prints {!length} values. When [always] is true,
    units are always included even for zero values (required for CSS [@property]
    initial-value). *)

val length_percentage_is_vendor_prefixed : length_percentage -> bool
(** [length_percentage_is_vendor_prefixed v] is [true] when [v] is a legacy
    vendor-prefixed intrinsic sizing keyword ([-webkit-max-content] etc.). *)

val length_has_runtime_subst : length -> bool
(** [length_has_runtime_subst l] is [true] when [l] is [var()] / [env()] /
    [attr()] / an anchor query, or a [calc()] containing one. *)

val calc_length_unit : length -> (string * float) option
(** [calc_length_unit l] is [Some (unit, value)] when [l] is a dimension, with
    the unit lower-cased. A keyword, a [var()] or a math function is [None]. *)

val absolute_unit_px_ratio : string -> float option
(** [absolute_unit_px_ratio unit] is [Some ratio] when [unit] is one of the
    absolute units CSS Values 4 sec. 6.2 puts on the [px] scale, with [ratio]
    the number of [px] in one [unit]. A relative unit is [None]. *)

val length_is_zero : length -> bool
(** [length_is_zero l] is [true] when [l] is a literal zero in any length unit
    ([0], [0px], [0%], etc.). *)

val pp_color : color Pp.t
(** [pp_color] pretty-prints {!color} values. *)

val pp_specified_color : color Pp.t
(** [pp_specified_color] pretty-prints {!color} values without crossing the
    computed-value color-space boundary. *)

val pp_color_in_mix : color Pp.t
(** [pp_color_in_mix] pretty-prints color values for use inside color-mix
    functions, using lowercase [currentcolor]. *)

val pp_angle : angle Pp.t
(** [pp_angle] pretty-prints {!angle} values. *)

val angle_degrees_opt : angle -> float option
(** [angle_degrees_opt a] converts concrete angle units to degrees. Dynamic
    angles such as [calc()] and [var()] return [None]. *)

val pp_duration : duration Pp.t
(** [pp_duration] pretty-prints {!duration} values. Minified output shortens a
    millisecond leaf to seconds where that is exact and no longer, except inside
    a [calc()], whose operands {!normalize_duration} leaves as authored. *)

val pp_duration_preserve_ms : duration Pp.t
(** [pp_duration_preserve_ms] pretty-prints {!duration} values without
    shortening milliseconds to seconds. *)

val pp_number : number Pp.t
(** [pp_number] pretty-prints {!number} values. *)

val pp_transition_behavior : transition_behavior Pp.t
(** [pp_transition_behavior] pretty-prints {!transition_behavior} values. *)

val pp_percentage : ?always:bool -> percentage Pp.t
(** [pp_percentage ?always] pretty-prints {!percentage} values. When [always] is
    true, always includes the % unit even for 0. *)

val pp_length_percentage : ?always:bool -> length_percentage Pp.t
(** [pp_length_percentage ?always] pretty-prints {!length_percentage} values.
    When [always] is true, always includes units even for 0. *)

type calc_ctx = { var_is_single_valued : string -> bool }
(** Context threaded through the calc folds for stylesheet-dependent rewrites.
    [var_is_single_valued n] reports whether [--n] is registered with a
    single-component [@property] syntax, so its [var()] substitutes exactly one
    calc term and a redundant [calc(var(--n))] nested inside another [calc()]
    may be unwrapped. *)

val default_calc_ctx : calc_ctx
(** [default_calc_ctx] knows of no single-valued variables, so every
    context-dependent calc rewrite is a no-op. *)

val normalize_length_percentage :
  ?strip:bool -> ?ctx:calc_ctx -> length_percentage -> length_percentage
(** [normalize_length_percentage lp] folds the numeric parts of a [calc()],
    keeping any [var()]: [calc(var(--x) + 1px + 2px)] becomes
    [calc(var(--x) + 3px)], and [calc(1px + 2px)] becomes [3px]. [strip]
    (default [true]) also drops a wrapped zero length's unit; pass [strip:false]
    for CSS function operands. *)

val normalize_length : ?strip:bool -> ?ctx:calc_ctx -> length -> length
(** [normalize_length ?strip l] evaluates the static CSS math functions on a
    [<length>] (min / max / clamp reduce to one dimension on shared units; round
    / mod / rem / hypot / abs fold on {!Calc.px}; calc folds through the
    simplifier), recursing into nested calls; a non-static operand keeps the
    call. [strip] (default [true]) additionally drops the unit on a top-level
    zero ([0px] -> [0]); pass [strip:false] for a calc / function operand, which
    keeps its unit. *)

val normalize_angle : ?ctx:calc_ctx -> angle -> angle
(** [normalize_angle a] folds the static angle math functions (round / mod / rem
    on [deg] operands) and converts to the shortest of the
    losslessly-interconvertible units (deg / turn / grad); [rad] (irrational via
    pi) stays as-is. *)

val normalize_number_percentage :
  ?ctx:calc_ctx -> number_percentage -> number_percentage
(** [normalize_number_percentage np] picks the shorter spelling for a typed
    [<number-percentage>] leaf where percentage and number are spec-equivalent
    (100% = 1, e.g. transform [scale()], the [scale] property, and the [filter]
    [brightness()]/[contrast()]/... functions; CSS Transforms 2 secs. 5 and
    12.1-12.2 and Filter Effects 1 sec. 6.1). [Var] and {!module-Calc} sub-forms
    stay opaque - inside a [calc()], the two spellings are not interchangeable.
*)

val normalize_number : ?ctx:calc_ctx -> number -> number
(** [normalize_number n] evaluates the static CSS math functions on a [<number>]
    ([hypot(3, 4)] becomes [5], [calc(1 + 2)] becomes [3]), recursing into
    nested calls; an operand with a [var()] keeps the call. *)

val normalize_percentage : ?ctx:calc_ctx -> percentage -> percentage
(** [normalize_percentage p] folds the value-independent parts of a
    [<percentage>] [calc()] ([calc(1 / 2 * 100%)] becomes [calc(.5*100%)]),
    keeping any [var()]. *)

val normalize_duration :
  ?ctx:calc_ctx -> ?canonicalize_ms:bool -> duration -> duration
(** [normalize_duration d] folds the value-independent parts of a [<time>]
    [calc()] ([calc(var(--d) * 1)] becomes [calc(var(--d))]), keeping any
    [var()]. It chooses the shorter seconds spelling by default;
    [canonicalize_ms:false] preserves millisecond units. *)

val normalize_color :
  ?lossless:bool -> ?exact_srgb:bool -> ?resolve_missing:bool -> color -> color
(** [normalize_color ?lossless c] canonicalises a color to its shortest
    spelling: a static colour in any space folds through sRGB to hex/named, hex
    shortens, and named<->hex picks the shorter. [lossless] disables lossy
    static colour-space and color-mix folds while preserving exact named/hex and
    byte-exact rgb folds.

    [exact_srgb] (default [false], and only consulted under [lossless])
    additionally folds a [color(srgb ...)] whose channels all land on a whole
    byte, the one [color()] conversion that loses nothing. It exists for the
    canonical diff projection, where [color(srgb 1 0 0)] and [rgb(255 0 0)] must
    not read as a difference; emission leaves the authored function alone.

    [resolve_missing] (default [false]) reads a [none] channel of a Lab-family
    colour as the zero CSS Color 4 sec. 4.4 says a missing component behaves as,
    so [oklab(0% none none / .5)] folds like [oklab(0% 0 0 / .5)] does. It is
    not carried into a nested colour, because sec. 13.3 gives a missing
    component the other colour's analogous component wherever two colours are
    interpolated: a [color-mix()] operand, a [var()] fallback and a
    relative-colour origin keep their [none]. Like [exact_srgb] it is for the
    canonical diff projection, and emission never sets it. *)

val pp_number_percentage : ?always:bool -> number_percentage Pp.t
(** [pp_number_percentage ?always] pretty-prints {!number_percentage} values.
    When [always] is true, always includes units even for 0. *)

val pp_calc : 'a Pp.t -> 'a calc Pp.t
(** [pp_calc pp] pretty-prints [calc] expressions using [pp] for leaf values. *)

val pp_color_name : color_name Pp.t
(** [pp_color_name] pretty-prints {!type-color_name} values. *)

val read_color_name : Cursor.t -> color_name
(** [read_color_name] reads a {!type-color_name} value: every name
    {!pp_color_name} prints, matched case-insensitively, and read back as the
    constructor that prints it, so [grey] is [Grey] and not the [Gray] that
    prints [gray]. *)

val pp_color_space : color_space Pp.t
(** [pp_color_space] pretty-prints {!color_space} values. *)

val read_color_space : Cursor.t -> color_space
(** [read_color_space] reads a {!color_space} value. *)

val pp_attr_syntax : attr_syntax Pp.t
(** [pp_attr_syntax] pretty-prints an [attr()] type() syntax keyword. *)

val read_attr_syntax : Cursor.t -> attr_syntax
(** [read_attr_syntax] reads an [attr()] type() syntax keyword. *)

val pp_attr_type : attr_type Pp.t
(** [pp_attr_type] pretty-prints an [attr()] type hint. *)

val read_attr_type : Cursor.t -> attr_type
(** [read_attr_type] reads an [attr()] type hint. *)

val pp_attr_call : 'a Pp.t -> 'a attr_call Pp.t
(** [pp_attr_call pp] pretty-prints an [attr()] call using [pp] for typed
    fallback values. *)

(** {2 Helper Functions} *)

val pp_var : 'a Pp.t -> 'a var Pp.t
(** [pp_var pp] pretty-prints CSS variables using [pp] for the payload. *)

val read_var : (Cursor.t -> 'a) -> Cursor.t -> 'a var
(** [read_var read t] parses a CSS variable with [var(...)] syntax using [read]
    for the payload. Expects to be positioned at [var(] and parses the full
    expression. *)

val read_var_body : (Cursor.t -> 'a) -> Cursor.t -> 'a var
(** [read_var_body read t] parses a [var()] reference body -- the contents of a
    [var(...)] form without the surrounding [var(] and [)] -- using [read] for
    the fallback's payload. *)

(** {1 Calc Module} *)
module Calc : sig
  val add : 'a calc -> 'a calc -> 'a calc
  (** [add a b] is the calc addition [a + b]. *)

  val sub : 'a calc -> 'a calc -> 'a calc
  (** [sub a b] is the calc subtraction [a - b]. *)

  val mul : 'a calc -> 'a calc -> 'a calc
  (** [mul a b] is the calc multiplication [a * b]. *)

  val div : 'a calc -> 'a calc -> 'a calc
  (** [div a b] is the calc division [a / b]. *)

  val ( + ) : 'a calc -> 'a calc -> 'a calc
  (** [a + b] is the calc addition. *)

  val ( - ) : 'a calc -> 'a calc -> 'a calc
  (** [a - b] is the calc subtraction. *)

  val ( * ) : 'a calc -> 'a calc -> 'a calc
  (** [a * b] is the calc multiplication. *)

  val ( / ) : 'a calc -> 'a calc -> 'a calc
  (** [a / b] is the calc division. *)

  val length : length -> length calc
  (** [length v] wraps a value as a calc leaf. *)

  val var : ?default:'a -> ?fallback:'a fallback -> string -> 'a calc
  (** [var ?default ?fallback name] is a CSS variable reference in calc. *)

  val float : float -> 'a calc
  (** [float f] is a dimensionless [<number>] literal in calc. Polymorphic per
      CSS Values 4 sec. 10: a [<number>] multiplied by a [<length>] yields a
      [<length>], by an [<angle>] yields an [<angle>], etc., so the same literal
      flows into any dimension. *)

  val infinity : 'a calc
  (** [infinity] is the CSS [infinity] math constant in calc. Polymorphic for
      the same reason as {!float}: a math constant is a [<number>]. *)

  val px : float -> length calc
  (** [px f] is a pixel length in calc. *)

  val rem : float -> length calc
  (** [rem f] is a rem length in calc. *)

  val em : float -> length calc
  (** [em f] is an em length in calc. *)

  val pct : float -> length calc
  (** [pct f] is a percentage value in calc. *)

  val nested : 'a calc -> 'a calc
  (** [nested inner] wraps [inner] in an explicit nested [calc()] call. *)

  val parens : 'a calc -> 'a calc
  (** [parens inner] wraps [inner] in parentheses only. *)
end

(** {1 Parsing Functions} *)

val normalize_signed_zero : float -> string -> float * string
(** [normalize_signed_zero n repr] collapses any zero to the canonical [0]: when
    [n] is zero it returns [(0., "0")], dropping a signed or over-precise
    authored spelling so [-0px] / [+0px] / [0.0px] all read (and serialise) with
    a [0] repr. Other values pass through unchanged. *)

val read_length :
  ?allow_negative:bool ->
  ?with_keywords:bool ->
  ?sizing:bool ->
  ?length_only:bool ->
  Cursor.t ->
  length
(** [read_length t] parses a CSS length. [length_only] excludes percentages,
    including those nested in math, and sizing-only functions. It defaults to
    [false] for readers whose grammar permits length-percentage values. [sizing]
    adds the intrinsic sizes CSS Sizing 3 sec. 5 defines - [min-content],
    [fit-content] and the rest - which belong to the sizing properties and not
    to a [<length>] as such; it defaults to [false], so a reader whose property
    takes them asks. *)

val read_non_negative_length : ?with_keywords:bool -> Cursor.t -> length
(** [read_non_negative_length reader] parses a length value that must be
    non-negative. Used for padding properties, whose CSS Box 4 sec. 4.1 grammar
    excludes negative values. *)

val read_padding_shorthand : Cursor.t -> length list
(** [read_padding_shorthand reader] parses a padding shorthand property
    accepting 1-4 space-separated non-negative length values (CSS Box 4 sec.
    4.2). *)

val read_margin_shorthand : Cursor.t -> length list
(** [read_margin_shorthand reader] parses a margin shorthand property accepting
    1-4 space-separated length values (CSS Box 4 sec. 3.2). *)

val read_color : Cursor.t -> color
(** [read_color t] parses a CSS color (hex, rgb/rgba, keywords, etc.). *)

val read_color_keyword_of_string : string -> color option
(** [read_color_keyword_of_string s] is the color keyword named by the
    lower-case ident [s]: a named color, {!val-transparent}, [currentcolor],
    [auto], [inherit], or a system color. [None] when [s] names no color
    keyword. *)

val fold_custom_value_ident : string -> string
(** [fold_custom_value_ident s] is the canonical lower-case spelling of [s] when
    [s] names a color keyword other than a system color, or one of the idents
    {!Parser.fold_value_ident} folds; other idents keep their source case. Pass
    as [fold_ident] to {!Parser.to_string_custom_minified}. *)

val pp_hue : hue Pp.t
(** [pp_hue] pretty-prints {!hue} values. *)

val read_hue : Cursor.t -> hue
(** [read_hue t] parses a CSS hue value. *)

val pp_alpha : alpha Pp.t
(** [pp_alpha] pretty-prints {!alpha} values. *)

val read_alpha : Cursor.t -> alpha
(** [read_alpha t] parses a CSS alpha value. *)

val pp_hue_interpolation : hue_interpolation Pp.t
(** [pp_hue_interpolation] pretty-prints {!hue_interpolation} values. *)

val read_hue_interpolation : Cursor.t -> hue_interpolation
(** [read_hue_interpolation t] parses a hue interpolation method. *)

val pp_calc_op : calc_op Pp.t
(** [pp_calc_op] pretty-prints {!calc_op} values. *)

val read_calc_op : Cursor.t -> calc_op
(** [read_calc_op t] parses a calc operation. *)

val pp_component_values : component_values Pp.t
(** [pp_component_values] pretty-prints preserved component values. *)

val read_component_values : Cursor.t -> component_values
(** [read_component_values t] returns the remaining component values from [t].
*)

val pp_invalid_value : invalid_value Pp.t
(** [pp_invalid_value] pretty-prints a preserved invalid value. *)

val read_invalid_value : Cursor.t -> invalid_value
(** [read_invalid_value t] reads a preserved invalid value. *)

val pp_math_const : math_const Pp.t
(** [pp_math_const] pretty-prints a math constant. *)

val read_math_const : Cursor.t -> math_const
(** [read_math_const t] parses a math constant. *)

val pp_math_arg : math_arg Pp.t
(** [pp_math_arg] pretty-prints a numeric math argument. *)

val read_math_arg : Cursor.t -> math_arg
(** [read_math_arg t] parses a numeric math argument. *)

val pp_component : component Pp.t
(** [pp_component] pretty-prints {!component} values. *)

val read_component : Cursor.t -> component
(** [read_component t] parses a component value. *)

val pp_channel : channel Pp.t
(** [pp_channel] pretty-prints {!channel} values. *)

val read_channel : Cursor.t -> channel
(** [read_channel t] parses a channel value. *)

val pp_rgb : rgb Pp.t
(** [pp_rgb] pretty-prints {!type-rgb} values. *)

val read_rgb : Cursor.t -> rgb
(** [read_rgb t] parses an RGB value. *)

val color_has_specified_hue : color -> bool
(** [color_has_specified_hue color] is [true] when [color] contains the obsolete
    [specified hue] interpolation keyword. Stylesheet recovery keeps the
    declaration for compatibility, while strict parsing reports it. *)

val color_is_color_4 : color -> bool
(** [color_is_color_4 c] is [true] when [c] uses a CSS Color 4 / 5 construct
    ([lab], {!val-lch}, {!val-oklab}, {!val-oklch}, {!val-hwb}, [color()],
    [color-mix()], [light-dark()], [contrast-color()], or [from <origin>]
    relative forms). Recurses through [Mix], [Light_dark], [Contrast_color]. *)

val read_angle : Cursor.t -> angle
(** [read_angle t] parses a CSS angle. *)

val read_angle_unit_required : Cursor.t -> angle
(** [read_angle_unit_required t] parses a generic CSS [<angle>] value, where
    bare zero is invalid under CSS Values 4 sec. 7.1. Legacy property contexts
    that allow unitless zero use {!read_angle}. *)

val read_duration : Cursor.t -> duration
(** [read_duration t] parses a CSS duration. *)

val read_duration_preserve_ms : Cursor.t -> duration
(** [read_duration_preserve_ms t] parses a CSS duration without canonicalizing
    milliseconds to seconds. *)

val read_time : Cursor.t -> duration
(** [read_time t] parses a CSS time value (can be negative). *)

val read_number : Cursor.t -> number
(** [read_number t] parses a CSS number (int/float). *)

val read_transition_behavior : Cursor.t -> transition_behavior
(** [read_transition_behavior t] parses a CSS transition-behavior value. *)

val read_percentage : Cursor.t -> percentage
(** [read_percentage t] parses a CSS percentage. *)

val read_length_percentage :
  ?allow_negative:bool ->
  ?with_keywords:bool ->
  ?sizing:bool ->
  Cursor.t ->
  length_percentage
(** [read_length_percentage t] parses a CSS length or percentage. *)

val read_number_percentage : Cursor.t -> number_percentage
(** [read_number_percentage t] parses a CSS number or percentage. *)

val read_calc :
  ?result_type:[ `Number | `Number_or_value | `Value ] ->
  (Cursor.t -> 'a) ->
  Cursor.t ->
  'a calc
(** [read_calc ~result_type read t] parses a [calc(...)] expression or a
    promotable value and checks that its statically knowable result type matches
    the property's numeric grammar. Expressions containing [var()] remain
    deferred until substitution. Omitting [result_type] retains the generic AST
    reader behaviour. *)

val read_integer_calc : string -> Cursor.t -> [ `Int of int | `Calc of 'a calc ]
(** [read_integer_calc name t] parses the math function at an [<integer>]
    position, which CSS Values 4 sec. 10.9 accepts wherever a literal integer
    is. A constant expression comes back folded, as [`Int]; one holding a
    [var()] comes back as [`Calc], for a value type carrying a calc node. [name]
    names the property in the error a non-integer constant raises. *)

val read_integer : string -> Cursor.t -> int
(** [read_integer name t] parses an [<integer>] position whose value type
    carries no calc node: a literal integer, or a [calc()] folding to one. *)

val read_calc_expr : (Cursor.t -> 'a) -> Cursor.t -> 'a calc
(** [read_calc_expr read t] parses a calc expression body -- the contents of a
    [calc(...)] form without the surrounding [calc(] and [)]. *)

val eval_numeric_calc : 'a calc -> float option
(** [eval_numeric_calc calc] tries to evaluate a calc expression containing only
    numbers to a float. Returns [None] if the expression contains variables or
    non-numeric values. *)

val eval_math_fn : math_fn -> float option
(** [eval_math_fn fn] evaluates a CSS Values 4 (ED) sec. 9.1 numeric math
    function call to a float. Returns [None] when an arg references an
    unresolved variable. *)

val read_numeric_expression : Cursor.t -> float
(** [read_numeric_expression t] parses and evaluates a numeric math expression,
    including top-level math functions such as [min()], [max()], and [clamp()].
*)

val map_calc : ('a -> 'b) -> 'a calc -> 'b calc
(** [map_calc f calc] rewrites every [Val] leaf via [f], preserving the calc
    structure (operators, [Nested], [Parens], [Var] fallbacks). *)

val map_calc_opt : ('a -> 'b option) -> 'a calc -> 'b calc option
(** [map_calc_opt f calc] is {!map_calc} for a partial [f]: the result is [None]
    as soon as one [Val] leaf has no image, and a [Var] / [Sibling_index] /
    [Sibling_count] node fails the same way since it carries no leaf to retype.
*)

val eval_calc : ?ctx:calc_ctx -> 'a calc -> 'a calc
(** [eval_calc calc] applies CSS Values 4 sec. 10.10.1 structural
    simplification: folds [Expr (Num _, op, Num _)] subtrees into a single [Num]
    and unwraps trivial [Nested] / [Parens] around leaves. [Val] / [Var] leaves
    and mixed-type operands survive -- type-specific simplification (e.g.,
    length arithmetic) is the caller's job. With [~ctx], a [Nested] / [Parens]
    around a single-valued [Var] leaf is also unwrapped (see {!calc_ctx}). *)

val calc_identity :
  zero:'a calc ->
  is_zero:('a -> bool) ->
  'a calc ->
  calc_op ->
  'a calc ->
  'a calc option
(** [calc_identity ~zero ~is_zero l op r] applies the value-independent CSS
    Values 4 sec. 10.10.1 identities to one [Expr (l, op, r)] node, returning
    the folded operand when one applies. These hold for any operand including a
    kept [var()] (inside [calc()] a [var()] is single-valued): [x * 1], [1 * x],
    and [x / 1] keep [x]; [x * 0] / [0 * x] reduce to [zero]. [is_zero]
    recognises a typed zero leaf ([0px]). Additive zero terms do not fold here:
    the spec only combines sum children with identical units. Numeric [op]
    numeric is the caller's job. *)

val var_name : 'a var -> string
(** [var_name v] is [v]'s variable name (without --). *)

val var_layer : 'a var -> string option
(** [var_layer v] is [v]'s optional layer name. *)

val with_fallback : 'a var -> 'a -> 'a var
(** [with_fallback var_ref fallback_value] creates a new variable reference with
    the same variable name but a different fallback value. This is useful when
    you need to reference a variable from another module with a specific
    fallback, without creating a declaration. *)

val var_meta : 'a var -> meta option
(** [var_meta v] is [v]'s optional metadata. *)

val pp_system_color : system_color Pp.t
(** [pp_system_color] is the pretty-printer for [system_color]. *)

val read_system_color : Cursor.t -> system_color
(** [read_system_color t] is the [system_color] parsed from [t]. *)
