(** CSS declarations and parser. *)

type 'a kind = 'a Properties_intf.kind

(** A typed declaration. Constructors remain available for pattern matching, but
    values must be built with {!v} or {!theme_guarded} so the cached hash
    matches the payload. *)
type declaration = private
  | Declaration : {
      property : 'a Properties_intf.property;
      value : 'a;
      important : bool;
      hash : int;
    }
      -> declaration
  | Theme_guarded : {
      var_name : string;
      decl : declaration;
      hash : int;
    }
      -> declaration

type t = declaration

val pp_property : 'a Properties.property Pp.t
(** [pp_property] is the pretty-printer for CSS property names. *)

val pp : t Pp.t
(** [pp] is the pretty-printer for declarations. *)

val pp_opaque : t Pp.t
(** [pp_opaque] minifies separators but preserves authored numeric token
    spellings in an opaque declaration value. It serves declaration feature
    queries, where the spelling is the compatibility question. *)

val pp_value : ('a kind * 'a) Pp.t
(** [pp_value] is the pretty-printer for typed values. *)

val meta_of_declaration : declaration -> Values.meta option
(** [meta_of_declaration d] is the metadata of [d], if any. *)

val important : declaration -> declaration
(** [important d] is [d] marked as [!important]. *)

val normalize :
  ?lossless:bool ->
  ?exact_srgb:bool ->
  ?resolve_missing:bool ->
  ?ctx:Values.calc_ctx ->
  declaration ->
  declaration
(** [normalize ?lossless d] applies AST-level semantic value canonicalisation so
    the optimizer holds a canonical declaration and the pretty-printer stays a
    pure serialiser. [lossless] disables bounded colour and numeric
    approximation. [exact_srgb] and [resolve_missing] are
    {!Properties.normalize_property_value}'s flags of the same names, for the
    canonical diff projection only. *)

val to_string : ?minify:bool -> t -> string
(** [to_string ~minify d] converts a declaration to CSS source text. *)

val is_declaration_value : string -> bool
(** [is_declaration_value s] is whether [s] is a [<declaration-value>]: one or
    more component values with no unmatched closing bracket, no top-level [;] or
    [!], no [<bad-string-token>] or [<bad-url-token>] and no unterminated
    function, block or string (CSS Syntax 3 (ED) sec. 7.2). Text outside it
    stops being part of the declaration it is written into. Pass the value with
    its [!important] flag already off, as {!split_important} takes it. *)

val split_important : string -> string * bool
(** [split_important s] is [s] without its [!important] flag, and whether it
    carried one. CSS Syntax 3 (ED) sec. 5.5.6 takes the flag off when the last
    two non-whitespace values are a [!] delim and the ident naming the flag, so
    [1 ! important] carries one and [1 !importantly] does not. *)

val custom_property : ?layer:string -> string -> string -> declaration
(** [custom_property ?layer name value] is a custom property declaration from
    authored CSS text.

    @raise Failure
      if the pair does not make the one declaration it names. [name] has to be a
      [<dashed-ident>], and [value] the [<declaration-value>?] CSS Variables 1
      sec. 2 gives a custom property: no top-level [;], no unmatched closing
      bracket, no unterminated function, block or string. A [name] holding a
      code point no bare ident carries is written back with the escapes that
      read it. {!parse_custom_property} is the same check as an option. *)

val parse_declaration : ?layer:string -> string -> string -> declaration option
(** [parse_declaration ?layer property value] reads [property] and [value] with
    the declaration parser into a fully-typed declaration. The two are read as
    the tokens they are rather than as one ["property: value"] text, so a
    [property] carrying a [;], a [}] or a [:] names this declaration or names
    none:
    - a known property (e.g. [mask-type], {!val-display}) becomes a typed
      declaration;
    - a custom property ([--x]) or an unknown property keeps its parsed
      component stream (not an opaque [Tokens] wrapper), so [var()] references
      in [value] are visible to [vars_of_declarations];
    - [None] if [value] does not parse.

    [layer] applies only to a custom property (a typed declaration has no
    layer). Unlike {!custom_property}, this parses the full property grammar. *)

val parse_opaque_declaration : string -> string -> declaration option
(** [parse_opaque_declaration property value] is {!parse_declaration} with the
    typed grammar skipped: [value] keeps the component stream it was written as
    even where [property] has a typed grammar, so nothing about its spelling is
    canonicalised. A custom property already reads that way and is unaffected.
    It serves an [\@supports] condition, where the declaration is handed to
    another parser rather than rendered, so the spelling is the question being
    asked. [None] on a [value] no declaration can hold. *)

val parse_custom_property : string -> string -> declaration option
(** [parse_custom_property name value] is {!parse_declaration} restricted to a
    custom property that a rule can hold. It is [None] unless [name] is a
    [<dashed-ident>] and [value] is the [<declaration-value>?] CSS Variables 1
    sec. 2 gives a custom property: no [<bad-string-token>], no
    [<bad-url-token>], no unmatched closing bracket, no unterminated function or
    block, and no top-level [;] or [!] (CSS Syntax 3 (ED) sec. 7.2). The empty
    value is one of its values.

    Use it for a name or value that comes from outside the parser, where the
    string may close the block it is placed in or start a second declaration.
    {!custom_property} takes the same pairs and raises on the rest. *)

val custom_declaration_layer : declaration -> string option
(** [custom_declaration_layer d] is the layer of [d], if any. *)

val map_custom_value : (string -> string) -> declaration -> declaration
(** [map_custom_value f d] is [d] with its custom-property value replaced by
    [f]'s rewrite of the minified serialisation. The importance, layer, metadata
    and theme guard of [d] are kept; any other declaration passes through
    unchanged. *)

val unquote_custom_font_strings : declaration -> declaration
(** [unquote_custom_font_strings d] rewrites a quoted family name in a
    custom-property token stream as the equivalent unquoted [<ident>] sequence,
    one word or several, when a generic family in the stream proves the stream
    is a font-family list. Equivalence-only normalisation for structural
    diffing; a stream without that proof, and any other declaration, passes
    through unchanged. *)

val canonicalize_custom_whitespace : declaration -> declaration
(** [canonicalize_custom_whitespace d] drops from a custom-property token stream
    the whitespace CSS reads as nothing: around the [*] and [/] of CSS Values 4
    (ED) sec. 10.8 arithmetic, and after a function or block whose closing
    bracket already separates it from what follows. The whitespace sec. 10.8
    requires around a math [+] or [-] stays, and so does the whitespace next to
    a [var()], [env()] or [attr()] whose substitution would otherwise merge with
    its neighbour. Equivalence-only normalisation for structural diffing; any
    other declaration passes through unchanged. *)

val read_property_name : Cursor.t -> string
(** [read_property_name t] is the property name read from [t]. *)

val read_property_value : Cursor.t -> string
(** [read_property_value t] is the value read from [t] (until ';' or '\}'). *)

val read_declaration : Cursor.t -> declaration option
(** [read_declaration t] is one typed declaration, or [None] when no more valid
    declarations. Performs full property name and value validation per CSS spec.
*)

val read : Cursor.t -> t
(** [read t] parses one typed declaration from [t]. *)

val of_string : string -> declaration
(** [of_string s] parses a single declaration from [s] (e.g. ["color: red"]).
    Raises {!Cursor.exception-Parse_error}, anchored on the offending text, when
    [s] is not a valid declaration. *)

val read_declarations : Cursor.t -> declaration list
(** [read_declarations t] is all typed declarations in an unbraced block. *)

val read_block : Cursor.t -> declaration list
(** [read_block t] is the typed declarations parsed from a braced block. *)

(** {2 Type-driven helper functions} *)

val v : ?important:bool -> 'a Properties.property -> 'a -> declaration
(** [v ?important property value] creates a typed declaration. *)

val theme_guarded : var_name:string -> declaration -> declaration
(** [theme_guarded ~var_name decl] wraps [decl] in a theme guard. *)

val hash : declaration -> int
(** [hash decl] returns the structural fingerprint cached at construction. Two
    declarations that are equal under [=] always return the same value; the
    converse may fail on hash collisions, so use it only as a cheap pre-filter
    before falling back to structural equality. *)

val is_important : declaration -> bool
(** [is_important decl] returns true if the declaration has !important. *)

val is_invalid : declaration -> bool
(** [is_invalid decl] is [true] when [decl]'s typed value is a CSS
    spec-violation cascade detected at parse time. [Optimize.drop_invalid],
    which every serialisation runs, uses this predicate to remove the
    declaration. *)

val value_uses_color_4 : declaration -> bool
(** [value_uses_color_4 decl] is [true] when [decl]'s typed value contains any
    CSS Color 4 / 5 construct. [var()] returns [false]. *)

val value_uses_runtime_subst : declaration -> bool
(** [value_uses_runtime_subst decl] is [true] when [decl]'s typed length value
    contains a [var()] / [env()] / [attr()] / anchor query, possibly through
    [calc()]. Covers length / length-list / length-percentage properties used in
    cross-tier fallback patterns. *)

val property_name : declaration -> string
(** [property_name decl] returns the property name as a string. *)

(** A property identity comparable with stdlib structural equality, without
    serialising the name to a string. *)
type prop_key = Key : 'a Properties.property -> prop_key [@@unboxed]

val equal_declaration : declaration -> declaration -> bool
(** [equal_declaration a b] tests declarations for structural equality. A NaN
    equals itself here, as it does under {!hash}: CSS has one NaN, a keyword of
    the [<number>] grammar (Values 4 sec. 10.7.2) serialised as [calc(NaN)]
    (sec. 10.13). *)

val same_minified : declaration -> declaration -> bool
(** [same_minified a b] is [true] when [a] and [b] render to the same minified
    text. Two declarations do so exactly when their canonical ASTs agree on
    property, value and importance, so this is {!equal_declaration} behind the
    {!hash} pre-filter rather than a comparison of rendered strings. *)

val equal_prop_key : prop_key -> prop_key -> bool
(** [equal_prop_key a b] tests property identities for equality. *)

val hash_prop_key : prop_key -> int
(** [hash_prop_key key] returns a hash consistent with {!equal_prop_key}. *)

val compare_prop_key : prop_key -> prop_key -> int
(** [compare_prop_key a b] is a total order on property identities, [0] exactly
    where {!equal_prop_key} is [true]. It orders the property tag rather than
    the runtime representation, so an ordered container keyed on a property
    stays off {!Stdlib.compare}. *)

val property_key : declaration -> prop_key
(** [property_key decl] is the identity of [decl]'s property. Two declarations
    have the same property name iff their keys are structurally equal. *)

val same_property : declaration -> declaration -> bool
(** [same_property d1 d2] is [property_key d1 = property_key d2]: the two
    declarations target the same property name. *)

val with_value : declaration -> string -> declaration
(** [with_value decl value] is [decl] carrying [value], read as a value of the
    property [decl] already holds and re-typed where that property accepts it.
    The property is taken from [decl] rather than from its name, so one whose
    minified spelling belongs to another property ([page-break-*] renders as the
    CSS Fragmentation 3 sec. 3.4 [break-*] alias) is rebuilt as itself. Raises
    [Cursor.Parse_error] when the property accepts no such value. *)

val with_opaque_value : declaration -> string -> declaration
(** [with_opaque_value decl value] is [decl]'s property carrying [value] as an
    unknown property's token stream, for a [value] {!with_value} rejects. The
    property is named as it parses back, not as it minifies. *)

val value_of : declaration -> 'a Properties.property -> 'a option
(** [value_of decl property] is [decl]'s value when {!property_key} names
    [property], and [None] otherwise. The witness ties the result to the type
    that property carries, so a caller reads the typed value without naming the
    existential {!constructor-Declaration} binds, which no constructor name
    resolves against. A theme guard is read through, as {!property_key} reads
    through it. *)

val string_of_value : ?minify:bool -> ?inline:bool -> declaration -> string
(** [string_of_value ?minify decl] returns the value as a string. *)

val value_size : ?minify:bool -> ?inline:bool -> declaration -> int
(** [value_size ?minify decl] is the byte length of
    [string_of_value ?minify decl], computed without allocating the string. *)

val value_has_css_wide_mix : declaration -> bool
(** [value_has_css_wide_mix decl] is [true] when [decl]'s value holds a CSS-wide
    keyword beside other components. CSS Cascade 5 sec. 7.3 makes such a keyword
    the whole value of a declaration or nothing, so the reader rejects one and a
    browser drops the declaration. This is that rule read off an emission, for
    the composer deciding whether a contraction it built can be written out. *)

(* Single-to-list property helpers. These construct typed declarations for
   properties that accept comma-separated lists, while keeping a simple
   single-value API. *)

open Values
open Properties

val background_image : background_image -> declaration
(** [background_image value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-image}
     background-image} property. *)

val text_shadow : text_shadow -> declaration
(** [text_shadow value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-shadow}
     text-shadow} property. *)

val text_shadows : text_shadow list -> declaration
(** [text_shadows values] is the text-shadow property with multiple shadows. *)

val transition : transition -> declaration
(** [transition value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transition} transition}
    property. *)

val transitions : transition list -> declaration
(** [transitions values] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transition} transition}
    property from a comma-separated list. *)

val transition_behavior : transition_behavior -> declaration
(** [transition_behavior v] is the CSS [transition-behavior] property. *)

val animation : animation -> declaration
(** [animation value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation} animation}
    property. *)

val box_shadow : shadow -> declaration
(** [box_shadow value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/box-shadow} box-shadow}
    property. *)

val box_shadows : shadow list -> declaration
(** [box_shadows values] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/box-shadow} box-shadow}
    property from a comma-separated list. Raises [Invalid_argument] when
    [values] is empty. *)

(** Declaration constructors *)

val z_index_auto : declaration
(** [z_index_auto] sets CSS [z-index] to [auto]. *)

val font_variant_numeric_tokens :
  font_variant_numeric_token list -> font_variant_numeric
(** [font_variant_numeric_tokens tokens] composes numeric font-variant tokens
    into a {!val-font_variant_numeric} value. *)

val font_variant_numeric_composed :
  ?ordinal:font_variant_numeric_token ->
  ?slashed_zero:font_variant_numeric_token ->
  ?numeric_figure:font_variant_numeric_token ->
  ?numeric_spacing:font_variant_numeric_token ->
  ?numeric_fraction:font_variant_numeric_token ->
  unit ->
  font_variant_numeric
(** [font_variant_numeric_composed ?ordinal ?slashed_zero ?numeric_figure
     ?numeric_spacing ?numeric_fraction ()] composes optional numeric
    font-variant tokens into a {!val-font_variant_numeric} value. *)

val background : background -> declaration
(** [background bg] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background} background}
    shorthand property. *)

val background_color : color -> declaration
(** [background_color c] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-color}
     background-color} property. *)

val color : color -> declaration
(** [color c] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/color} color} property.
*)

val border_color : color -> declaration
(** [border_color c] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-color}
     border-color} property. *)

val border_style : border_style -> declaration
(** [border_style v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-style}
     border-style} property. *)

val border_top_style : border_style -> declaration
(** [border_top_style v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-style}
     border-top-style} property. *)

val border_right_style : border_style -> declaration
(** [border_right_style v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-style}
     border-right-style} property. *)

val border_bottom_style : border_style -> declaration
(** [border_bottom_style v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-style}
     border-bottom-style} property. *)

val border_left_style : border_style -> declaration
(** [border_left_style v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-style}
     border-left-style} property. *)

val border_inline_style : logical_border_style -> declaration
(** [border_inline_style v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-style}
     border-inline-style} property. *)

val border_block_style : logical_border_style -> declaration
(** [border_block_style v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-block-style}
     border-block-style} property. *)

val border_inline_start_style : border_style -> declaration
(** [border_inline_start_style v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-start-style}
     border-inline-start-style} property. *)

val border_inline_end_style : border_style -> declaration
(** [border_inline_end_style v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-end-style}
     border-inline-end-style} property. *)

val border_block_start_style : border_style -> declaration
(** [border_block_start_style v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-block-start-style}
     border-block-start-style} property. *)

val border_block_end_style : border_style -> declaration
(** [border_block_end_style v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-block-end-style}
     border-block-end-style} property. *)

val border_start_start_radius : length -> declaration
(** [border_start_start_radius len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-start-start-radius}
     border-start-start-radius} property. *)

val border_start_end_radius : length -> declaration
(** [border_start_end_radius len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-start-end-radius}
     border-start-end-radius} property. *)

val border_end_start_radius : length -> declaration
(** [border_end_start_radius len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-end-start-radius}
     border-end-start-radius} property. *)

val border_end_end_radius : length -> declaration
(** [border_end_end_radius len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-end-end-radius}
     border-end-end-radius} property. *)

val text_decoration : text_decoration -> declaration
(** [text_decoration v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration}
     text-decoration} property. *)

val font_style : font_style -> declaration
(** [font_style v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-style} font-style}
    property. *)

val list_style_type : list_style_type -> declaration
(** [list_style_type v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/list-style-type}
     list-style-type} property. *)

val list_style_position : list_style_position -> declaration
(** [list_style_position v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/list-style-position}
     list-style-position} property. *)

val list_style_image : list_style_image -> declaration
(** [list_style_image v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/list-style-image}
     list-style-image} property. *)

val padding : length list -> declaration
(** [padding values] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding} padding}
    shorthand property. *)

val padding_left : length -> declaration
(** [padding_left v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-left}
     padding-left} property. *)

val padding_right : length -> declaration
(** [padding_right v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-right}
     padding-right} property. *)

val padding_bottom : length -> declaration
(** [padding_bottom v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-bottom}
     padding-bottom} property. *)

val padding_top : length -> declaration
(** [padding_top v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-top}
     padding-top} property. *)

val margin : length list -> declaration
(** [margin values] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin} margin}
    shorthand property. *)

val margin_left : length -> declaration
(** [margin_left v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin-left}
     margin-left} property. *)

val margin_right : length -> declaration
(** [margin_right v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin-right}
     margin-right} property. *)

val margin_top : length -> declaration
(** [margin_top v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin-top} margin-top}
    property. *)

val margin_bottom : length -> declaration
(** [margin_bottom v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin-bottom}
     margin-bottom} property. *)

val gap : Properties.gap -> declaration
(** [gap v] is the {{:https://developer.mozilla.org/en-US/docs/Web/CSS/gap} gap}
    property. *)

val column_gap : length -> declaration
(** [column_gap v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/column-gap} column-gap}
    property. *)

val row_gap : length -> declaration
(** [row_gap v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/row-gap} row-gap}
    property. *)

val grid_template_areas : grid_template_areas -> declaration
(** [grid_template_areas v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template-areas}
     grid-template-areas} property. *)

val grid_template : grid_template -> declaration
(** [grid_template v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template}
     grid-template} property. *)

val grid_auto_columns : grid_template -> declaration
(** [grid_auto_columns v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-auto-columns}
     grid-auto-columns} property. *)

val grid_auto_rows : grid_template -> declaration
(** [grid_auto_rows v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-auto-rows}
     grid-auto-rows} property. *)

val grid_row_start : grid_line -> declaration
(** [grid_row_start v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-row-start}
     grid-row-start} property. *)

val grid_row_end : grid_line -> declaration
(** [grid_row_end v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-row-end}
     grid-row-end} property. *)

val grid_column_start : grid_line -> declaration
(** [grid_column_start v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-column-start}
     grid-column-start} property. *)

val grid_column_end : grid_line -> declaration
(** [grid_column_end v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-column-end}
     grid-column-end} property. *)

val grid_row : grid_line * grid_line -> declaration
(** [grid_row v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-row} grid-row}
    property. *)

val grid_column : grid_line * grid_line -> declaration
(** [grid_column v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-column}
     grid-column} property. *)

val grid_area : grid_area -> declaration
(** [grid_area v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-area} grid-area}
    property. *)

val width : length -> declaration
(** [width v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/width} width} property.
*)

val height : length -> declaration
(** [height v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/height} height}
    property. *)

val min_width : length -> declaration
(** [min_width v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/min-width} min-width}
    property. *)

val min_height : length -> declaration
(** [min_height v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/min-height} min-height}
    property. *)

val max_width : length -> declaration
(** [max_width v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/max-width} max-width}
    property. *)

val max_height : length -> declaration
(** [max_height v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/max-height} max-height}
    property. *)

val inline_size : length -> declaration
(** [inline_size len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/inline-size}
     inline-size} logical property. *)

val min_inline_size : length -> declaration
(** [min_inline_size len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/min-inline-size}
     min-inline-size} logical property. *)

val max_inline_size : length -> declaration
(** [max_inline_size len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/max-inline-size}
     max-inline-size} logical property. *)

val block_size : length -> declaration
(** [block_size len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/block-size} block-size}
    logical property. *)

val min_block_size : length -> declaration
(** [min_block_size len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/min-block-size}
     min-block-size} logical property. *)

val max_block_size : length -> declaration
(** [max_block_size len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/max-block-size}
     max-block-size} logical property. *)

val font_size : length -> declaration
(** [font_size v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-size} font-size}
    property. *)

val font_size_kw : font_size -> declaration
(** [font_size_kw fs] is the font-size property accepting the full
    {!val-font_size} type including absolute/relative size keywords. *)

val line_height : line_height -> declaration
(** [line_height v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/line-height}
     line-height} property. *)

val font_weight : font_weight -> declaration
(** [font_weight v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-weight}
     font-weight} property. *)

val text_align : text_align -> declaration
(** [text_align v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-align} text-align}
    property. *)

val text_decoration_style : text_decoration_style -> declaration
(** [text_decoration_style v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-style}
     text-decoration-style} property. *)

val text_decoration_line : text_decoration_line -> declaration
(** [text_decoration_line v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-line}
     text-decoration-line} property. *)

val text_underline_offset : length -> declaration
(** [text_underline_offset v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-underline-offset}
     text-underline-offset} property. *)

val text_decoration_skip : text_decoration_skip -> declaration
(** [text_decoration_skip v] is the CSS [text-decoration-skip] property. *)

val text_decoration_skip_self : text_decoration_skip_self -> declaration
(** [text_decoration_skip_self v] is the CSS [text-decoration-skip-self]
    property. *)

val text_decoration_skip_box : text_decoration_skip_box -> declaration
(** [text_decoration_skip_box v] is the CSS [text-decoration-skip-box] property.
*)

val text_decoration_skip_inset : text_decoration_skip_inset -> declaration
(** [text_decoration_skip_inset v] is the CSS [text-decoration-skip-inset]
    property. *)

val text_decoration_skip_spaces : text_decoration_skip_spaces -> declaration
(** [text_decoration_skip_spaces v] is the CSS [text-decoration-skip-spaces]
    property. *)

val text_emphasis : text_emphasis -> declaration
(** [text_emphasis v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-emphasis}
     text-emphasis} property. *)

val text_emphasis_style : text_emphasis_style -> declaration
(** [text_emphasis_style v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-emphasis-style}
     text-emphasis-style} property. *)

val text_emphasis_color : color -> declaration
(** [text_emphasis_color v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-emphasis-color}
     text-emphasis-color} property. *)

val text_emphasis_position : text_emphasis_position -> declaration
(** [text_emphasis_position v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-emphasis-position}
     text-emphasis-position} property. *)

val text_emphasis_skip : text_emphasis_skip -> declaration
(** [text_emphasis_skip v] is the CSS [text-emphasis-skip] property. *)

val text_orientation : text_orientation -> declaration
(** [text_orientation v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-orientation}
     text-orientation} property. *)

val text_transform : text_transform -> declaration
(** [text_transform v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-transform}
     text-transform} property. *)

val letter_spacing : length -> declaration
(** [letter_spacing v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/letter-spacing}
     letter-spacing} property. *)

val white_space : white_space -> declaration
(** [white_space v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/white-space}
     white-space} property. *)

val display : display -> declaration
(** [display v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/display} display}
    property. *)

val position : position -> declaration
(** [position v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/position} position}
    property. *)

val visibility : visibility -> declaration
(** [visibility v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/visibility} visibility}
    property. *)

val inset : length list -> declaration
(** [inset v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/inset} inset} property.
*)

val inset_inline : length list -> declaration
(** [inset_inline v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/inset-inline}
     inset-inline} property. *)

val inset_inline_start : length -> declaration
(** [inset_inline_start v] is the inset-inline-start property. *)

val inset_inline_end : length -> declaration
(** [inset_inline_end v] is the inset-inline-end property. *)

val inset_block : length list -> declaration
(** [inset_block v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/inset-block}
     inset-block} property. *)

val inset_block_start : length -> declaration
(** [inset_block_start v] is the inset-block-start property. *)

val inset_block_end : length -> declaration
(** [inset_block_end v] is the inset-block-end property. *)

val top : length -> declaration
(** [top v] is the {{:https://developer.mozilla.org/en-US/docs/Web/CSS/top} top}
    property. *)

val right : length -> declaration
(** [right v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/right} right} property.
*)

val bottom : length -> declaration
(** [bottom v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/bottom} bottom}
    property. *)

val left : length -> declaration
(** [left v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/left} left} property. *)

val opacity : opacity -> declaration
(** [opacity v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/opacity} opacity}
    property. *)

val flex_direction : flex_direction -> declaration
(** [flex_direction v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex-direction}
     flex-direction} property. *)

val flex : flex -> declaration
(** [flex v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex} flex} property. *)

val flex_grow : float -> declaration
(** [flex_grow v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex-grow} flex-grow}
    property. *)

val flex_shrink : float -> declaration
(** [flex_shrink v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex-shrink}
     flex-shrink} property. *)

val flex_basis : flex_basis -> declaration
(** [flex_basis v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex-basis} flex-basis}
    property. *)

val flex_wrap : flex_wrap -> declaration
(** [flex_wrap v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex-wrap} flex-wrap}
    property. *)

val flex_flow : flex_flow -> declaration
(** [flex_flow v] is the CSS [flex-flow] property. *)

val order : Properties.order -> declaration
(** [order v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/order} order} property.
*)

val align_items : Properties.align_items -> declaration
(** [align_items v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/align-items}
     align-items} property. *)

val align_content : Properties.align_content -> declaration
(** [align_content v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/align-content}
     align-content} property. *)

val align_self : Properties.align_self -> declaration
(** [align_self v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/align-self} align-self}
    property. *)

val justify_content : Properties.justify_content -> declaration
(** [justify_content v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-content}
     justify-content} property. *)

val justify_items : Properties.justify_items -> declaration
(** [justify_items v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-items}
     justify-items} property. *)

val justify_self : Properties.justify_self -> declaration
(** [justify_self v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-self}
     justify-self} property. *)

val place_content : place_content -> declaration
(** [place_content v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/place-content}
     place-content} property. *)

val place_items : place_items -> declaration
(** [place_items v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/place-items}
     place-items} property. *)

val place_self : align_self * justify_self -> declaration
(** [place_self v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/place-self} place-self}
    property. *)

val border_width : border_width -> declaration
(** [border_width v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-width}
     border-width} property. *)

val border_radius : border_radius -> declaration
(** [border_radius v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-radius}
     border-radius} property. *)

val border_top_left_radius : length -> declaration
(** [border_top_left_radius v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-left-radius}
     border-top-left-radius} property. *)

val border_top_right_radius : length -> declaration
(** [border_top_right_radius v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-right-radius}
     border-top-right-radius} property. *)

val border_bottom_left_radius : length -> declaration
(** [border_bottom_left_radius v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-left-radius}
     border-bottom-left-radius} property. *)

val border_bottom_right_radius : length -> declaration
(** [border_bottom_right_radius v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-right-radius}
     border-bottom-right-radius} property. *)

val fill : svg_paint -> declaration
(** [fill v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fill} fill}
    property. *)

val stroke : svg_paint -> declaration
(** [stroke v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke}
     stroke} property. *)

val stroke_width : stroke_width -> declaration
(** [stroke_width v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke-width}
     stroke-width} property. *)

val fill_rule : fill_rule -> declaration
(** [fill_rule v] is the [fill-rule] property. *)

val clip_rule : fill_rule -> declaration
(** [clip_rule v] is the [clip-rule] property, which takes what [fill-rule]
    takes. *)

val fill_opacity : opacity -> declaration
(** [fill_opacity v] is the [fill-opacity] property. *)

val stroke_opacity : opacity -> declaration
(** [stroke_opacity v] is the [stroke-opacity] property. *)

val stroke_linecap : stroke_linecap -> declaration
(** [stroke_linecap v] is the [stroke-linecap] property. *)

val stroke_linejoin : stroke_linejoin -> declaration
(** [stroke_linejoin v] is the [stroke-linejoin] property. *)

val stroke_miterlimit : stroke_miterlimit -> declaration
(** [stroke_miterlimit v] is the [stroke-miterlimit] property. *)

val stroke_dashoffset : stroke_dashoffset -> declaration
(** [stroke_dashoffset v] is the [stroke-dashoffset] property. *)

val stroke_dasharray : stroke_dasharray -> declaration
(** [stroke_dasharray v] is the [stroke-dasharray] property. *)

val paint_order : paint_order -> declaration
(** [paint_order v] is the [paint-order] property. *)

val vector_effect : vector_effect -> declaration
(** [vector_effect v] is the [vector-effect] property. *)

val stop_color : color -> declaration
(** [stop_color v] is the [stop-color] property of a gradient stop. *)

val stop_opacity : opacity -> declaration
(** [stop_opacity v] is the [stop-opacity] property of a gradient stop. *)

val flood_color : color -> declaration
(** [flood_color v] is the [flood-color] property of [feFlood]. *)

val flood_opacity : opacity -> declaration
(** [flood_opacity v] is the [flood-opacity] property of [feFlood]. *)

val lighting_color : color -> declaration
(** [lighting_color v] is the [lighting-color] property of a light filter. *)

val dominant_baseline : dominant_baseline -> declaration
(** [dominant_baseline v] is the [dominant-baseline] property. *)

val alignment_baseline : alignment_baseline -> declaration
(** [alignment_baseline v] is the [alignment-baseline] property. *)

val baseline_shift : baseline_shift -> declaration
(** [baseline_shift v] is the [baseline-shift] property. *)

val baseline_source : baseline_source -> declaration
(** [baseline_source v] is the [baseline-source] property. *)

val anchor_name : anchor_name -> declaration
(** [anchor_name v] is the [anchor-name] property. *)

val position_anchor : position_anchor -> declaration
(** [position_anchor v] is the [position-anchor] property. *)

val position_area : position_area -> declaration
(** [position_area v] is the [position-area] property. *)

val position_try_fallbacks : position_try_fallbacks -> declaration
(** [position_try_fallbacks v] is the [position-try-fallbacks] property. *)

val position_try_order : position_try_order -> declaration
(** [position_try_order v] is the [position-try-order] property. *)

val position_try : position_try -> declaration
(** [position_try v] is the [position-try] shorthand. *)

val position_visibility : position_visibility -> declaration
(** [position_visibility v] is the [position-visibility] property. *)

val view_transition_name : view_transition_name -> declaration
(** [view_transition_name v] is the [view-transition-name] property. *)

val view_transition_class : view_transition_class -> declaration
(** [view_transition_class v] is the [view-transition-class] property. *)

val offset_path : offset_path -> declaration
(** [offset_path v] is the [offset-path] property. *)

val offset_distance : length_percentage -> declaration
(** [offset_distance v] is the [offset-distance] property. *)

val offset_rotate : offset_rotate -> declaration
(** [offset_rotate v] is the [offset-rotate] property. *)

val offset_anchor : offset_anchor -> declaration
(** [offset_anchor v] is the [offset-anchor] property. *)

val offset_position : offset_position -> declaration
(** [offset_position v] is the [offset-position] property. *)

val offset : offset -> declaration
(** [offset v] is the [offset] shorthand. *)

val column_width : column_width -> declaration
(** [column_width v] is the [column-width] property. *)

val column_count : column_count -> declaration
(** [column_count v] is the [column-count] property. *)

val column_height : column_height -> declaration
(** [column_height v] is the [column-height] property. *)

val column_wrap : column_wrap -> declaration
(** [column_wrap v] is the [column-wrap] property. *)

val column_rule_width : border_width list -> declaration
(** [column_rule_width v] is the [column-rule-width] property, one entry per gap
    decoration line. *)

val column_rule_style : border_style list -> declaration
(** [column_rule_style v] is the [column-rule-style] property, one entry per gap
    decoration line. *)

val column_rule_color : color list -> declaration
(** [column_rule_color v] is the [column-rule-color] property, one entry per gap
    decoration line. *)

val border_image : border_image -> declaration
(** [border_image v] is the [border-image] shorthand. *)

val border_image_source : background_image -> declaration
(** [border_image_source v] is the [border-image-source] property. *)

val border_image_slice : border_image_slice -> declaration
(** [border_image_slice v] is the [border-image-slice] property. *)

val border_image_width : border_image_width -> declaration
(** [border_image_width v] is the [border-image-width] property. *)

val border_image_outset : border_image_outset -> declaration
(** [border_image_outset v] is the [border-image-outset] property. *)

val border_image_repeat : border_image_repeat -> declaration
(** [border_image_repeat v] is the [border-image-repeat] property. *)

val contain_intrinsic_size : contain_intrinsic_size -> declaration
(** [contain_intrinsic_size v] is the [contain-intrinsic-size] shorthand. *)

val contain_intrinsic_width : contain_intrinsic_longhand -> declaration
(** [contain_intrinsic_width v] is the [contain-intrinsic-width] property. *)

val contain_intrinsic_height : contain_intrinsic_longhand -> declaration
(** [contain_intrinsic_height v] is the [contain-intrinsic-height] property. *)

val contain_intrinsic_block_size : contain_intrinsic_longhand -> declaration
(** [contain_intrinsic_block_size v] is the [contain-intrinsic-block-size]
    property. *)

val contain_intrinsic_inline_size : contain_intrinsic_longhand -> declaration
(** [contain_intrinsic_inline_size v] is the [contain-intrinsic-inline-size]
    property. *)

val mask_border : border_image -> declaration
(** [mask_border v] is the [mask-border] shorthand, which takes what
    [border-image] takes plus the mode slot. *)

val animation_timeline : animation_timeline -> declaration
(** [animation_timeline v] is the [animation-timeline] property. *)

val animation_range : animation_range -> declaration
(** [animation_range v] is the [animation-range] shorthand. *)

val animation_range_start : animation_range_item -> declaration
(** [animation_range_start v] is the [animation-range-start] property. *)

val animation_range_end : animation_range_item -> declaration
(** [animation_range_end v] is the [animation-range-end] property. *)

val scroll_timeline : timeline_shorthand -> declaration
(** [scroll_timeline v] is the [scroll-timeline] shorthand. *)

val scroll_timeline_name : timeline_name -> declaration
(** [scroll_timeline_name v] is the [scroll-timeline-name] property. *)

val scroll_timeline_axis : timeline_axis -> declaration
(** [scroll_timeline_axis v] is the [scroll-timeline-axis] property. *)

val view_timeline : view_timeline_shorthand -> declaration
(** [view_timeline v] is the [view-timeline] shorthand. *)

val view_timeline_name : timeline_name -> declaration
(** [view_timeline_name v] is the [view-timeline-name] property. *)

val view_timeline_axis : timeline_axis -> declaration
(** [view_timeline_axis v] is the [view-timeline-axis] property. *)

val view_timeline_inset : timeline_inset -> declaration
(** [view_timeline_inset v] is the [view-timeline-inset] property. *)

val timeline_scope : timeline_name -> declaration
(** [timeline_scope v] is the [timeline-scope] property. *)

val all : css_wide -> declaration
(** [all v] is the [all] property. It resets every longhand to [v] but the two
    writing-mode ones CSS Cascading 5 sec. 3.3 excepts. *)

val outline_style : outline_style -> declaration
(** [outline_style v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/outline-style}
     outline-style} property. *)

val outline_width : border_width -> declaration
(** [outline_width v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/outline-width}
     outline-width} property. *)

val outline_color : color -> declaration
(** [outline_color v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/outline-color}
     outline-color} property. *)

val forced_color_adjust : forced_color_adjust -> declaration
(** [forced_color_adjust v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/forced-color-adjust}
     forced-color-adjust} property. *)

val table_layout : table_layout -> declaration
(** [table_layout v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/table-layout}
     table-layout} property. *)

val border_spacing : border_spacing -> declaration
(** [border_spacing values] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-spacing}
     border-spacing} property. Accepts 1 or 2 length values. *)

val overflow : overflow -> declaration
(** [overflow v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overflow} overflow}
    property. *)

val object_fit : object_fit -> declaration
(** [object_fit v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/object-fit} object-fit}
    property. *)

val object_view_box : object_view_box -> declaration
(** [object_view_box v] is the CSS [object-view-box] property. *)

val clip : clip -> declaration
(** [clip v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/clip} clip} property
    (deprecated in favor of [clip-path]). *)

val clear : clear -> declaration
(** [clear v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/clear} clear} property.
*)

val float : float_side -> declaration
(** [float v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/float} float} property.
*)

val interactivity : interactivity -> declaration
(** [interactivity v] is the CSS [interactivity] property. *)

val caret_animation : caret_animation -> declaration
(** [caret_animation v] is the CSS [caret-animation] property. *)

val caret_shape : caret_shape -> declaration
(** [caret_shape v] is the CSS [caret-shape] property. *)

val caret : caret -> declaration
(** [caret v] is the CSS [caret] property. *)

val interest_delay : interest_delay -> declaration
(** [interest_delay v] is the CSS [interest-delay] property. *)

val interest_delay_start : interest_delay -> declaration
(** [interest_delay_start v] is the CSS [interest-delay-start] property. *)

val interest_delay_end : interest_delay -> declaration
(** [interest_delay_end v] is the CSS [interest-delay-end] property. *)

val nav_up : nav -> declaration
(** [nav_up v] is the CSS [nav-up] property. *)

val nav_right : nav -> declaration
(** [nav_right v] is the CSS [nav-right] property. *)

val nav_down : nav -> declaration
(** [nav_down v] is the CSS [nav-down] property. *)

val nav_left : nav -> declaration
(** [nav_left v] is the CSS [nav-left] property. *)

val touch_action : touch_action -> declaration
(** [touch_action v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/touch-action}
     touch-action} property. *)

val direction : direction -> declaration
(** [direction v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/direction} direction}
    property. *)

val unicode_bidi : unicode_bidi -> declaration
(** [unicode_bidi v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/unicode-bidi}
     unicode-bidi} property. *)

val writing_mode : writing_mode -> declaration
(** [writing_mode v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/writing-mode}
     writing-mode} property. *)

val text_combine_upright : text_combine_upright -> declaration
(** [text_combine_upright v] is the CSS [text-combine-upright] property. *)

val text_decoration_skip_ink : text_decoration_skip_ink -> declaration
(** [text_decoration_skip_ink v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-skip-ink}
     text-decoration-skip-ink} property. *)

val animation_name : animation_name -> declaration
(** [animation_name v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-name}
     animation-name} property. *)

val animation_duration : duration -> declaration
(** [animation_duration v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-duration}
     animation-duration} property. *)

val animation_timing_function : timing_function -> declaration
(** [animation_timing_function v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function}
     animation-timing-function} property. *)

val animation_delay : duration -> declaration
(** [animation_delay v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-delay}
     animation-delay} property. *)

val animation_iteration_count : animation_iteration_count -> declaration
(** [animation_iteration_count v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-iteration-count}
     animation-iteration-count} property. *)

val animation_direction : animation_direction -> declaration
(** [animation_direction v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-direction}
     animation-direction} property. *)

val animation_fill_mode : animation_fill_mode -> declaration
(** [animation_fill_mode v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-fill-mode}
     animation-fill-mode} property. *)

val animation_play_state : animation_play_state -> declaration
(** [animation_play_state v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-play-state}
     animation-play-state} property. *)

val background_blend_mode : blend_mode -> declaration
(** [background_blend_mode v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-blend-mode}
     background-blend-mode} property. *)

val scroll_margin : length list -> declaration
(** [scroll_margin v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin}
     scroll-margin} shorthand. *)

val scroll_margin_top : length -> declaration
(** [scroll_margin_top v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-top}
     scroll-margin-top} property. *)

val scroll_margin_right : length -> declaration
(** [scroll_margin_right v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-right}
     scroll-margin-right} property. *)

val scroll_margin_bottom : length -> declaration
(** [scroll_margin_bottom v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-bottom}
     scroll-margin-bottom} property. *)

val scroll_margin_left : length -> declaration
(** [scroll_margin_left v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-left}
     scroll-margin-left} property. *)

val scroll_margin_inline : length list -> declaration
(** [scroll_margin_inline v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-inline}
     scroll-margin-inline} property. *)

val scroll_margin_inline_start : length -> declaration
(** [scroll_margin_inline_start v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-inline-start}
     scroll-margin-inline-start} property. *)

val scroll_margin_inline_end : length -> declaration
(** [scroll_margin_inline_end v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-inline-end}
     scroll-margin-inline-end} property. *)

val scroll_margin_block : length list -> declaration
(** [scroll_margin_block vs] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-block}
     scroll-margin-block} property; takes 1 (both edges) or 2 (start, end)
    length values per the spec. *)

val scroll_margin_block_start : length -> declaration
(** [scroll_margin_block_start v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-block-start}
     scroll-margin-block-start} property. *)

val scroll_margin_block_end : length -> declaration
(** [scroll_margin_block_end v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-block-end}
     scroll-margin-block-end} property. *)

val scroll_padding : length list -> declaration
(** [scroll_padding v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding}
     scroll-padding} shorthand. *)

val scroll_padding_top : length -> declaration
(** [scroll_padding_top v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-top}
     scroll-padding-top} property. *)

val scroll_padding_right : length -> declaration
(** [scroll_padding_right v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-right}
     scroll-padding-right} property. *)

val scroll_padding_bottom : length -> declaration
(** [scroll_padding_bottom v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-bottom}
     scroll-padding-bottom} property. *)

val scroll_padding_left : length -> declaration
(** [scroll_padding_left v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-left}
     scroll-padding-left} property. *)

val scroll_padding_inline : length list -> declaration
(** [scroll_padding_inline v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-inline}
     scroll-padding-inline} property. *)

val scroll_padding_inline_start : length -> declaration
(** [scroll_padding_inline_start v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-inline-start}
     scroll-padding-inline-start} property. *)

val scroll_padding_inline_end : length -> declaration
(** [scroll_padding_inline_end v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-inline-end}
     scroll-padding-inline-end} property. *)

val scroll_padding_block : length list -> declaration
(** [scroll_padding_block v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-block}
     scroll-padding-block} property. *)

val scroll_padding_block_start : length -> declaration
(** [scroll_padding_block_start v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-block-start}
     scroll-padding-block-start} property. *)

val scroll_padding_block_end : length -> declaration
(** [scroll_padding_block_end v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-block-end}
     scroll-padding-block-end} property. *)

val overscroll_behavior : overscroll_behavior list -> declaration
(** [overscroll_behavior v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overscroll-behavior}
     overscroll-behavior} property. *)

val overscroll_behavior_x : overscroll_behavior -> declaration
(** [overscroll_behavior_x v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overscroll-behavior}
     overscroll-behavior-x} property. *)

val overscroll_behavior_y : overscroll_behavior -> declaration
(** [overscroll_behavior_y v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overscroll-behavior}
     overscroll-behavior-y} property. *)

val accent_color : color -> declaration
(** [accent_color v] is the CSS [accent-color] property. *)

val caret_color : color -> declaration
(** [caret_color v] is the CSS [caret-color] property. *)

val text_decoration_color : color -> declaration
(** [text_decoration_color v] is the CSS [text-decoration-color] property. *)

val text_decoration_thickness : length -> declaration
(** [text_decoration_thickness v] is the CSS [text-decoration-thickness]
    property. *)

val text_size_adjust : text_size_adjust -> declaration
(** [text_size_adjust v] is the CSS [text-size-adjust] property. *)

val aspect_ratio : aspect_ratio -> declaration
(** [aspect_ratio v] is the CSS [aspect-ratio] property. *)

val filter : filter -> declaration
(** [filter v] is the CSS [filter] property. *)

val filter_var_empty : string -> filter
(** [filter_var_empty name] creates a filter var reference with empty fallback,
    i.e., [var(--name, )]. Used for composable filter utilities. *)

val background_image_var_none : string -> background_image
(** [background_image_var_none name] creates a background_image var reference
    with no fallback, i.e., [var(--name)]. Used for mask gradient utilities. *)

val mix_blend_mode : blend_mode -> declaration
(** [mix_blend_mode v] is the CSS [mix-blend-mode] property. *)

val grid_template_columns : grid_template -> declaration
(** [grid_template_columns v] is the CSS [grid-template-columns] property. *)

val grid_template_rows : grid_template -> declaration
(** [grid_template_rows v] is the CSS [grid-template-rows] property. *)

val grid_auto_flow : grid_auto_flow -> declaration
(** [grid_auto_flow v] is the CSS [grid-auto-flow] property. *)

val pointer_events : pointer_events -> declaration
(** [pointer_events v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/pointer-events}
     pointer-events} property. *)

val z_index : z_index -> declaration
(** [z_index v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/z-index} z-index}
    property. *)

val appearance : appearance -> declaration
(** [appearance v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/appearance} appearance}
    property. *)

val overflow_x : overflow -> declaration
(** [overflow_x v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-x} overflow-x}
    property. *)

val overflow_y : overflow -> declaration
(** [overflow_y v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-y} overflow-y}
    property. *)

val resize : resize -> declaration
(** [resize v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/resize} resize}
    property. *)

val vertical_align : vertical_align -> declaration
(** [vertical_align v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/vertical-align}
     vertical-align} property. *)

val box_sizing : box_sizing -> declaration
(** [box_sizing v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/box-sizing} box-sizing}
    property. *)

val field_sizing : field_sizing -> declaration
(** [field_sizing v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/field-sizing}
     field-sizing} property. *)

val caption_side : caption_side -> declaration
(** [caption_side v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/caption-side}
     caption-side} property. *)

val font_family : font_family -> declaration
(** [font_family v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-family}
     font-family} property. *)

val print_color_adjust : print_color_adjust -> declaration
(** [print_color_adjust v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/print-color-adjust}
     print-color-adjust} property. *)

val webkit_print_color_adjust : print_color_adjust -> declaration
(** [webkit_print_color_adjust v] is the [-webkit-print-color-adjust] property,
    the legacy WebKit-prefixed alias of [print-color-adjust]. *)

val box_decoration_break : box_decoration_break -> declaration
(** [box_decoration_break v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/box-decoration-break}
     box-decoration-break} property. *)

val webkit_box_decoration_break : box_decoration_break -> declaration
(** [webkit_box_decoration_break v] is the [-webkit-box-decoration-break]
    property. *)

val background_origin : background_box -> declaration
(** [background_origin v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-origin}
     background-origin} property. *)

val background_clip : background_box -> declaration
(** [background_clip v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-clip}
     background-clip} property. *)

val webkit_background_clip : background_box -> declaration
(** [webkit_background_clip v] is the [-webkit-background-clip] property. *)

val font_families : font_family list -> declaration
(** [font_families fonts] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-family}
     font-family} property from a comma-separated list. Raises
    [Invalid_argument] when [fonts] is empty. *)

val word_spacing : length -> declaration
(** [word_spacing v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/word-spacing}
     word-spacing} property. *)

val background_attachment : background_attachment -> declaration
(** [background_attachment v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-attachment}
     background-attachment} property. *)

val border_top : border -> declaration
(** [border_top v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-top} border-top}
    shorthand. *)

val border_right : border -> declaration
(** [border_right v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-right}
     border-right} shorthand. *)

val border_bottom : border -> declaration
(** [border_bottom v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom}
     border-bottom} shorthand. *)

val border_left : border -> declaration
(** [border_left v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-left}
     border-left} shorthand. *)

val object_position : position_value -> declaration
(** [object_position v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/object-position}
     object-position} property. *)

val transform_origin : transform_origin -> declaration
(** [transform_origin v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transform-origin}
     transform-origin} property. *)

val transform_box : transform_box -> declaration
(** [transform_box v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transform-box}
     transform-box} property. *)

val clip_path : clip_path -> declaration
(** [clip_path v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/clip-path} clip-path}
    property. *)

val mask : mask -> declaration
(** [mask v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/mask} mask} property. *)

val webkit_mask_image : background_image -> declaration
(** [webkit_mask_image img] is the [-webkit-mask-image] property. *)

val mask_image : background_image -> declaration
(** [mask_image img] is the [mask-image] property. *)

val webkit_mask_composite : webkit_mask_composite -> declaration
(** [webkit_mask_composite v] is the [-webkit-mask-composite] property. *)

val mask_composite : mask_composite -> declaration
(** [mask_composite v] is the [mask-composite] property. *)

val webkit_mask_source_type : webkit_mask_source_type -> declaration
(** [webkit_mask_source_type v] is the [-webkit-mask-source-type] property. *)

val mask_mode : mask_mode -> declaration
(** [mask_mode v] is the [mask-mode] property. *)

val mask_type : mask_type -> declaration
(** [mask_type v] is the [mask-type] property. *)

val webkit_mask_size : background_size -> declaration
(** [webkit_mask_size v] is the [-webkit-mask-size] property. *)

val mask_size : background_size -> declaration
(** [mask_size v] is the [mask-size] property. *)

val webkit_mask_position : position_value list -> declaration
(** [webkit_mask_position v] is the [-webkit-mask-position] property. *)

val mask_position : position_value list -> declaration
(** [mask_position v] is the [mask-position] property. *)

val webkit_mask_repeat : background_repeat -> declaration
(** [webkit_mask_repeat v] is the [-webkit-mask-repeat] property. *)

val mask_repeat : background_repeat -> declaration
(** [mask_repeat v] is the [mask-repeat] property. *)

val webkit_mask_clip : mask_box -> declaration
(** [webkit_mask_clip v] is the [-webkit-mask-clip] property. *)

val mask_clip : mask_box -> declaration
(** [mask_clip v] is the [mask-clip] property. *)

val webkit_mask_origin : mask_box -> declaration
(** [webkit_mask_origin v] is the [-webkit-mask-origin] property. *)

val mask_origin : mask_box -> declaration
(** [mask_origin v] is the [mask-origin] property. *)

val content_visibility : content_visibility -> declaration
(** [content_visibility v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/content-visibility}
     content-visibility} property. *)

val moz_osx_font_smoothing : moz_osx_font_smoothing -> declaration
(** [moz_osx_font_smoothing v] is the Mozilla-only [-moz-osx-font-smoothing]
    property. *)

val webkit_line_clamp : webkit_line_clamp -> declaration
(** [webkit_line_clamp v] is the WebKit-only [-webkit-line-clamp] property. *)

val webkit_box_orient : webkit_box_orient -> declaration
(** [webkit_box_orient v] is the WebKit-only [-webkit-box-orient] property. *)

val text_overflow : text_overflow -> declaration
(** [text_overflow v] is the CSS [text-overflow] property. *)

val text_wrap : text_wrap -> declaration
(** [text_wrap v] is the CSS [text-wrap] property. *)

val text_wrap_mode : text_wrap_mode -> declaration
(** [text_wrap_mode v] is the CSS [text-wrap-mode] property. *)

val text_underline_position : text_underline_position -> declaration
(** [text_underline_position v] is the CSS [text-underline-position] property.
*)

val text_box_edge : text_box_edge -> declaration
(** [text_box_edge v] is the CSS [text-box-edge] property. *)

val inline_sizing : inline_sizing -> declaration
(** [inline_sizing v] is the CSS [inline-sizing] property. *)

val line_fit_edge : line_fit_edge -> declaration
(** [line_fit_edge v] is the CSS [line-fit-edge] property. *)

val interpolate_size : interpolate_size -> declaration
(** [interpolate_size v] is the CSS [interpolate-size] property. *)

val min_intrinsic_sizing : min_intrinsic_sizing -> declaration
(** [min_intrinsic_sizing v] is the CSS [min-intrinsic-sizing] property. *)

val ruby_align : ruby_align -> declaration
(** [ruby_align v] is the CSS [ruby-align] property. *)

val ruby_merge : ruby_merge -> declaration
(** [ruby_merge v] is the CSS [ruby-merge] property. *)

val ruby_overhang : ruby_overhang -> declaration
(** [ruby_overhang v] is the CSS [ruby-overhang] property. *)

val ruby_position : ruby_position -> declaration
(** [ruby_position v] is the CSS [ruby-position] property. *)

val glyph_orientation_vertical : glyph_orientation_vertical -> declaration
(** [glyph_orientation_vertical v] is the CSS [glyph-orientation-vertical]
    property. *)

val word_break : word_break -> declaration
(** [word_break v] is the CSS [word-break] property. *)

val overflow_wrap : overflow_wrap -> declaration
(** [overflow_wrap v] is the CSS [overflow-wrap] property. *)

val line_break : line_break -> declaration
(** [line_break v] is the CSS [line-break] property. *)

val hyphens : hyphens -> declaration
(** [hyphens v] is the CSS [hyphens] property. *)

val webkit_hyphens : hyphens -> declaration
(** [webkit_hyphens v] is the WebKit-only [-webkit-hyphens] property. *)

val font_stretch : font_stretch -> declaration
(** [font_stretch v] is the CSS [font-stretch] property. *)

val font_optical_sizing : font_optical_sizing -> declaration
(** [font_optical_sizing v] is the CSS [font-optical-sizing] property. *)

val font_kerning : font_kerning -> declaration
(** [font_kerning v] is the CSS [font-kerning] property. *)

val font_language_override : font_language_override -> declaration
(** [font_language_override v] is the CSS [font-language-override] property. *)

val font_synthesis_style : font_synthesis_style -> declaration
(** [font_synthesis_style v] is the CSS [font-synthesis-style] property. *)

val font_synthesis_weight : font_synthesis_weight -> declaration
(** [font_synthesis_weight v] is the CSS [font-synthesis-weight] property. *)

val font_synthesis_small_caps : font_synthesis_small_caps -> declaration
(** [font_synthesis_small_caps v] is the CSS [font-synthesis-small-caps]
    property. *)

val font_synthesis_position : font_synthesis_position -> declaration
(** [font_synthesis_position v] is the CSS [font-synthesis-position] property.
*)

val font_variant_ligatures : font_variant_ligatures -> declaration
(** [font_variant_ligatures v] is the CSS [font-variant-ligatures] property. *)

val font_variant_caps : font_variant_caps -> declaration
(** [font_variant_caps v] is the CSS [font-variant-caps] property. *)

val font_variant_numeric : font_variant_numeric -> declaration
(** [font_variant_numeric v] is the CSS [font-variant-numeric] property. *)

val font_variant_position : font_variant_position -> declaration
(** [font_variant_position v] is the CSS [font-variant-position] property. *)

val font_variant_east_asian : font_variant_east_asian -> declaration
(** [font_variant_east_asian v] is the CSS [font-variant-east-asian] property.
*)

val backdrop_filter : filter -> declaration
(** [backdrop_filter v] is the CSS [backdrop-filter] property. *)

val webkit_backdrop_filter : filter -> declaration
(** [webkit_backdrop_filter v] is the CSS [-webkit-backdrop-filter] property. *)

val background_position : position_value list -> declaration
(** [background_position v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-position}
     background-position} property. *)

val background_repeat : background_repeat -> declaration
(** [background_repeat v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-repeat}
     background-repeat} property. *)

val background_size : background_size -> declaration
(** [background_size v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-size}
     background-size} property. *)

val content : content -> declaration
(** [content v] is the CSS [content] property. *)

val counter_reset : counter_set -> declaration
(** [counter_reset v] is the CSS [counter-reset] property. *)

val counter_increment : counter_set -> declaration
(** [counter_increment v] is the CSS [counter-increment] property. *)

val border_left_width : border_width -> declaration
(** [border_left_width v] is the CSS [border-left-width] property. *)

val border_inline_start_width : border_width -> declaration
(** [border_inline_start_width v] is the CSS [border-inline-start-width] proper
    ty. *)

val border_inline_end_width : border_width -> declaration
(** [border_inline_end_width v] is the CSS [border-inline-end-width] property.
*)

val border_block_start_width : border_width -> declaration
(** [border_block_start_width v] is the CSS [border-block-start-width] property.
*)

val border_block_end_width : border_width -> declaration
(** [border_block_end_width v] is the CSS [border-block-end-width] property. *)

val border_bottom_width : border_width -> declaration
(** [border_bottom_width v] is the CSS [border-bottom-width] property. *)

val border_top_width : border_width -> declaration
(** [border_top_width v] is the CSS [border-top-width] property. *)

val border_right_width : border_width -> declaration
(** [border_right_width v] is the CSS [border-right-width] property. *)

val border_top_color : color -> declaration
(** [border_top_color v] is the CSS [border-top-color] property. *)

val border_right_color : color -> declaration
(** [border_right_color v] is the CSS [border-right-color] property. *)

val border_bottom_color : color -> declaration
(** [border_bottom_color v] is the CSS [border-bottom-color] property. *)

val border_left_color : color -> declaration
(** [border_left_color v] is the CSS [border-left-color] property. *)

val border_inline_start_color : color -> declaration
(** [border_inline_start_color v] is the CSS [border-inline-start-color] proper
    ty. *)

val border_inline_end_color : color -> declaration
(** [border_inline_end_color v] is the CSS [border-inline-end-color] property.
*)

val border_block_start_color : color -> declaration
(** [border_block_start_color v] is the CSS [border-block-start-color] property.
*)

val border_block_end_color : color -> declaration
(** [border_block_end_color v] is the CSS [border-block-end-color] property. *)

val border_inline_color : logical_border_color -> declaration
(** [border_inline_color v] is the CSS [border-inline-color] property. *)

val border_block_color : logical_border_color -> declaration
(** [border_block_color v] is the CSS [border-block-color] property. *)

val border_inline_width : logical_border_width -> declaration
(** [border_inline_width v] is the CSS [border-inline-width] property. *)

val border_block_width : logical_border_width -> declaration
(** [border_block_width v] is the CSS [border-block-width] property. *)

val quotes : Properties.quotes -> declaration
(** [quotes v] is the CSS [quotes] property. *)

val border :
  ?width:Properties.border_width ->
  ?style:Properties.border_style ->
  ?color:Values.color ->
  unit ->
  declaration
(** [border ?width ?style ?color ()] is the CSS [border] shorthand. *)

val tab_size : int -> declaration
(** [tab_size v] is the CSS [tab-size] property. *)

val tab_size_value : tab_size -> declaration
(** [tab_size_value v] is the CSS [tab-size] property from a typed value,
    allowing a [<length>] in addition to an integer. *)

val scrollbar_width : scrollbar_width -> declaration
(** [scrollbar_width v] is the CSS [scrollbar-width] property. *)

val scrollbar_color : scrollbar_color -> declaration
(** [scrollbar_color v] is the CSS [scrollbar-color] property. *)

val scrollbar_gutter : scrollbar_gutter -> declaration
(** [scrollbar_gutter v] is the CSS [scrollbar-gutter] property. *)

val zoom : zoom -> declaration
(** [zoom v] is the CSS [zoom] property. *)

val webkit_text_size_adjust : text_size_adjust -> declaration
(** [webkit_text_size_adjust v] is the WebKit-only [-webkit-text-size-adjust] p
    roperty. *)

val font_feature_settings : font_feature_settings -> declaration
(** [font_feature_settings v] is the CSS [font-feature-settings] property. *)

val font_variation_settings : font_variation_settings -> declaration
(** [font_variation_settings v] is the CSS [font-variation-settings] property.
*)

val webkit_tap_highlight_color : color -> declaration
(** [webkit_tap_highlight_color v] is the WebKit-only
    [-webkit-tap-highlight-colo r] property. *)

val webkit_text_decoration : text_decoration -> declaration
(** [webkit_text_decoration v] is the WebKit-only [-webkit-text-decoration]
    property. *)

val webkit_text_decoration_color : color -> declaration
(** [webkit_text_decoration_color v] is the WebKit-only
    [-webkit-text-decoration- color] property. *)

val text_indent : text_indent_value -> declaration
(** [text_indent v] is the CSS [text-indent] property. *)

val border_collapse : border_collapse -> declaration
(** [border_collapse v] is the CSS [border-collapse] property. *)

val list_style : list_style -> declaration
(** [list_style v] is the CSS [list-style] shorthand. *)

val font : font -> declaration
(** [font v] is the CSS [font] shorthand. *)

val webkit_appearance : webkit_appearance -> declaration
(** [webkit_appearance v] is the WebKit-only
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/appearance}
     -webkit-appearance} property. *)

val webkit_font_smoothing : webkit_font_smoothing -> declaration
(** [webkit_font_smoothing v] is the WebKit-only [-webkit-font-smoothing]
    property. *)

val cursor : cursor -> declaration
(** [cursor v] is the CSS [cursor] property. *)

val user_select : user_select -> declaration
(** [user_select v] is the CSS [user-select] property. *)

val webkit_user_select : user_select -> declaration
(** [webkit_user_select v] is the CSS [-webkit-user-select] property. *)

val container_type : container_type -> declaration
(** [container_type v] is the CSS [container-type] property. *)

val container_name : string -> declaration
(** [container_name v] is the CSS [container-name] property. *)

val container : ?type_:container_type -> string -> declaration
(** [container ?type_ name] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/container} container}
    shorthand declaration. Emits [container: <name>] when [type_] is omitted, or
    [container: <name> / <type>] when both are present. The name is mandatory
    because [container:] with neither slot is invalid per spec. *)

val transform : transform -> declaration
(** [transform t] is the CSS [transform] property with a single transformation.
*)

val transforms : transform list -> declaration
(** [transforms ts] is the CSS {!val-transform} property with multiple
    transformations. *)

val rotate : rotate_value -> declaration
(** [rotate v] is the CSS [rotate] property. *)

val scale : scale -> declaration
(** [scale v] is the CSS [scale] property. *)

val translate : translate_value -> declaration
(** [translate v] is the CSS [translate] property. *)

val perspective : length -> declaration
(** [perspective v] is the CSS [perspective] property. *)

val perspective_origin : perspective_origin -> declaration
(** [perspective_origin v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/perspective-origin}
     perspective-origin} property. *)

val transform_style : transform_style -> declaration
(** [transform_style v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transform-style}
     transform-style} property. *)

val backface_visibility : backface_visibility -> declaration
(** [backface_visibility v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/backface-visibility}
     backface-visibility} property. *)

val transition_duration : duration -> declaration
(** [transition_duration v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transition-duration}
     transition-duration} property. *)

val transition_timing_function : timing_function -> declaration
(** [transition_timing_function v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transition-timing-function}
     transition-timing-function} property. *)

val transition_delay : duration -> declaration
(** [transition_delay v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transition-delay}
     transition-delay} property. *)

val transition_property : transition_property -> declaration
(** [transition_property v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transition-property}
     transition-property} property. *)

val will_change : will_change -> declaration
(** [will_change v] is the CSS [will-change] property. *)

val contain : contain -> declaration
(** [contain v] is the CSS [contain] property. *)

val isolation : isolation -> declaration
(** [isolation v] is the CSS [isolation] property. *)

val break_before : break_value -> declaration
(** [break_before v] is the CSS [break-before] property for page/column/region
    breaks. *)

val break_after : break_value -> declaration
(** [break_after v] is the CSS [break-after] property for page/column/region
    breaks. *)

val break_inside : break_inside_value -> declaration
(** [break_inside v] is the CSS [break-inside] property for page/column/region
    breaks. *)

val page_break_before : page_break_value -> declaration
(** [page_break_before v] is the legacy [page-break-before] property. *)

val page_break_after : page_break_value -> declaration
(** [page_break_after v] is the legacy [page-break-after] property. *)

val page_break_inside : page_break_inside_value -> declaration
(** [page_break_inside v] is the legacy [page-break-inside] property. *)

val columns : columns_value -> declaration
(** [columns v] is the CSS [columns] property for multi-column layout. *)

val column_rule : border -> declaration
(** [column_rule v] is the CSS [column-rule] shorthand property. *)

val border_block : border -> declaration
(** [border_block v] is the CSS [border-block] shorthand property. *)

val column_span : column_span -> declaration
(** [column_span v] is the CSS [column-span] property. *)

val padding_inline : length list -> declaration
(** [padding_inline v] is the CSS [padding-inline] property. *)

val padding_inline_start : length -> declaration
(** [padding_inline_start v] is the CSS [padding-inline-start] property. *)

val padding_inline_end : length -> declaration
(** [padding_inline_end v] is the CSS [padding-inline-end] property. *)

val padding_block : length list -> declaration
(** [padding_block v] is the CSS [padding-block] property. *)

val padding_block_start : length -> declaration
(** [padding_block_start v] is the CSS [padding-block-start] property. *)

val padding_block_end : length -> declaration
(** [padding_block_end v] is the CSS [padding-block-end] property. *)

val margin_inline : length -> declaration
(** [margin_inline v] is the CSS [margin-inline] property. *)

val margin_inline_start : length -> declaration
(** [margin_inline_start v] is the CSS [margin-inline-start] property. *)

val margin_inline_end : length -> declaration
(** [margin_inline_end v] is the CSS [margin-inline-end] property. *)

val margin_block : length -> declaration
(** [margin_block v] is the CSS [margin-block] property. *)

val margin_block_start : length -> declaration
(** [margin_block_start v] is the CSS [margin-block-start] property. *)

val margin_block_end : length -> declaration
(** [margin_block_end v] is the CSS [margin-block-end] property. *)

val outline : outline -> declaration
(** [outline v] is the CSS [outline] shorthand. *)

val outline_offset : length -> declaration
(** [outline_offset v] is the CSS [outline-offset] property. *)

val scroll_snap_type : scroll_snap_type -> declaration
(** [scroll_snap_type v] is the CSS [scroll-snap-type] property. *)

val scroll_snap_align : scroll_snap_align -> declaration
(** [scroll_snap_align v] is the CSS [scroll-snap-align] property. *)

val scroll_snap_stop : scroll_snap_stop -> declaration
(** [scroll_snap_stop v] is the CSS [scroll-snap-stop] property. *)

val scroll_behavior : scroll_behavior -> declaration
(** [scroll_behavior v] is the CSS [scroll-behavior] property. *)

val color_scheme : color_scheme -> declaration
(** [color_scheme v] is the CSS [color-scheme] property. *)

val image_rendering : image_rendering -> declaration
(** [image_rendering v] is the CSS [image-rendering] property. *)

val image_resolution : image_resolution -> declaration
(** [image_resolution v] is the CSS [image-resolution] property. *)
