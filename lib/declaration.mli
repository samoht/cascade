(** CSS declarations and parser. *)

include module type of Declaration_intf

val pp_property : 'a Properties.property Pp.t
(** [pp_property] is the pretty-printer for CSS property names. *)

val pp_declaration : declaration Pp.t
(** [pp_declaration] is the pretty-printer for declarations. *)

val pp_value : ('a kind * 'a) Pp.t
(** [pp_value] is the pretty-printer for typed values. *)

val meta_of_declaration : declaration -> Values.meta option
(** [meta_of_declaration d] is the metadata of [d], if any. *)

val important : declaration -> declaration
(** [important d] is [d] marked as [!important]. *)

val string_of_declaration : ?minify:bool -> declaration -> string
(** [string_of_declaration ~minify decl] converts a declaration to its string
    representation. If [minify] is [true] (default: [false]), the output is
    minified. *)

val custom_property : ?layer:string -> string -> string -> declaration
(** [custom_property ?layer name value] is a raw custom property declaration. *)

val custom_declaration_layer : declaration -> string option
(** [custom_declaration_layer d] is the layer of [d], if any. *)

val read_property_name : Reader.t -> string
(** [read_property_name t] is the property name read from [t]. *)

val read_property_value : Reader.t -> string
(** [read_property_value t] is the value read from [t] (until ';' or '\}'). *)

val read_declaration : Reader.t -> declaration option
(** [read_declaration t] is one typed declaration, or [None] when no more valid
    declarations. Performs full property name and value validation per CSS spec.
*)

val read_declarations : Reader.t -> declaration list
(** [read_declarations t] is all typed declarations in an unbraced block. *)

val read_block : Reader.t -> declaration list
(** [read_block t] is the typed declarations parsed from a braced block. *)

(** {2 Type-driven helper functions} *)

val v : ?important:bool -> 'a Properties.property -> 'a -> declaration
(** [v ?important property value] creates a typed declaration. *)

val custom_declaration :
  ?important:bool ->
  ?layer:string ->
  ?meta:Values.meta ->
  string ->
  'a kind ->
  'a ->
  declaration
(** [custom_declaration ?important ?layer ?meta name kind value] creates a
    custom property declaration. *)

val is_important : declaration -> bool
(** [is_important decl] returns true if the declaration has !important. *)

val property_name : declaration -> string
(** [property_name decl] returns the property name as a string. *)

val string_of_value : ?minify:bool -> ?inline:bool -> declaration -> string
(** [string_of_value ?minify decl] returns the value as a string. *)

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
    property from a comma-separated list. *)

(** Declaration constructors *)

val z_index_auto : declaration
(** [z_index_auto] sets CSS [z-index] to [auto]. *)

val font_variant_numeric_tokens :
  font_variant_numeric_token list -> font_variant_numeric
(** [font_variant_numeric_tokens tokens] composes numeric font-variant tokens
    into a [font_variant_numeric] value. *)

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
    font-variant tokens into a [font_variant_numeric] value. *)

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

val grid_template_areas : string -> declaration
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
(** [grid_row (start, stop)] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-row} grid-row}
    property. *)

val grid_column : grid_line * grid_line -> declaration
(** [grid_column (start, stop)] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-column}
     grid-column} property. *)

val grid_area : string -> declaration
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

val inset : length -> declaration
(** [inset v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/inset} inset} property.
*)

val inset_inline : length -> declaration
(** [inset_inline v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/inset-inline}
     inset-inline} property. *)

val inset_inline_start : length -> declaration
(** [inset_inline_start v] is the inset-inline-start property. *)

val inset_inline_end : length -> declaration
(** [inset_inline_end v] is the inset-inline-end property. *)

val inset_block : length -> declaration
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

val flex_basis : length -> declaration
(** [flex_basis v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex-basis} flex-basis}
    property. *)

val flex_wrap : flex_wrap -> declaration
(** [flex_wrap v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex-wrap} flex-wrap}
    property. *)

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
(** [justify_self ?safe ?position ?baseline ()] is the
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

val border_radius : length -> declaration
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

val stroke_width : length -> declaration
(** [stroke_width v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke-width}
     stroke-width} property. *)

val outline_style : outline_style -> declaration
(** [outline_style v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/outline-style}
     outline-style} property. *)

val outline_width : length -> declaration
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

val border_spacing : length -> declaration
(** [border_spacing v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-spacing}
     border-spacing} property. *)

val overflow : overflow -> declaration
(** [overflow v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overflow} overflow}
    property. *)

val object_fit : object_fit -> declaration
(** [object_fit v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/object-fit} object-fit}
    property. *)

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

val text_decoration_skip_ink : text_decoration_skip_ink -> declaration
(** [text_decoration_skip_ink v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-skip-ink}
     text-decoration-skip-ink} property. *)

val animation_name : string -> declaration
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

val scroll_margin : length -> declaration
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

val scroll_padding : length -> declaration
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

val overscroll_behavior : overscroll_behavior -> declaration
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
     font-family} property from a comma-separated list. *)

val word_spacing : length -> declaration
(** [word_spacing v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/word-spacing}
     word-spacing} property. *)

val background_attachment : background_attachment -> declaration
(** [background_attachment v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-attachment}
     background-attachment} property. *)

val border_top : string -> declaration
(** [border_top v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-top} border-top}
    shorthand. *)

val border_right : string -> declaration
(** [border_right v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-right}
     border-right} shorthand. *)

val border_bottom : string -> declaration
(** [border_bottom v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom}
     border-bottom} shorthand. *)

val border_left : string -> declaration
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

val mask : string -> declaration
(** [mask v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/mask} mask} property. *)

val webkit_mask_image : background_image -> declaration
(** [-webkit-mask-image] property. *)

val mask_image : background_image -> declaration
(** [mask-image] property. *)

val webkit_mask_composite : webkit_mask_composite -> declaration
(** [-webkit-mask-composite] property. *)

val mask_composite : mask_composite -> declaration
(** [mask-composite] property. *)

val webkit_mask_source_type : webkit_mask_source_type -> declaration
(** [-webkit-mask-source-type] property. *)

val mask_mode : mask_mode -> declaration
(** [mask-mode] property. *)

val mask_type : mask_type -> declaration
(** [mask-type] property. *)

val webkit_mask_size : background_size -> declaration
(** [-webkit-mask-size] property. *)

val mask_size : background_size -> declaration
(** [mask-size] property. *)

val webkit_mask_clip : mask_box -> declaration
(** [-webkit-mask-clip] property. *)

val mask_clip : mask_box -> declaration
(** [mask-clip] property. *)

val webkit_mask_origin : mask_box -> declaration
(** [-webkit-mask-origin] property. *)

val mask_origin : mask_box -> declaration
(** [mask-origin] property. *)

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

val word_break : word_break -> declaration
(** [word_break v] is the CSS [word-break] property. *)

val overflow_wrap : overflow_wrap -> declaration
(** [overflow_wrap v] is the CSS [overflow-wrap] property. *)

val hyphens : hyphens -> declaration
(** [hyphens v] is the CSS [hyphens] property. *)

val webkit_hyphens : hyphens -> declaration
(** [webkit_hyphens v] is the WebKit-only [-webkit-hyphens] property. *)

val font_stretch : font_stretch -> declaration
(** [font_stretch v] is the CSS [font-stretch] property. *)

val font_variant_numeric : font_variant_numeric -> declaration
(** [font_variant_numeric v] is the CSS [font-variant-numeric] property. *)

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

val border_left_width : border_width -> declaration
(** [border_left_width v] is the CSS [border-left-width] property. *)

val border_inline_start_width : border_width -> declaration
(** [border_inline_start_width v] is the CSS [border-inline-start-width] proper
    ty. *)

val border_inline_end_width : border_width -> declaration
(** [border_inline_end_width v] is the CSS [border-inline-end-width] property.
*)

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

val text_indent : length -> declaration
(** [text_indent v] is the CSS [text-indent] property. *)

val border_collapse : border_collapse -> declaration
(** [border_collapse v] is the CSS [border-collapse] property. *)

val list_style : string -> declaration
(** [list_style v] is the CSS [list-style] shorthand. *)

val font : string -> declaration
(** [font v] is the CSS [font] shorthand. *)

val webkit_appearance : webkit_appearance -> declaration
(** [webkit_appearance v] is the WebKit-only
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/appearance}
     -webkit-appearance} property. *)

val webkit_font_smoothing : webkit_font_smoothing -> declaration
(** [webkit_font_smoothing v] is the WebKit-only [-webkit-font-smoothing]
    property. *)

val cursor : cursor -> declaration
(** [cu rsor v] is the CSS [cursor] property. *)

val user_select : user_select -> declaration
(** [user_select v] is the CSS [user-select] property. *)

val webkit_user_select : user_select -> declaration
(** [webkit_user_select v] is the CSS [-webkit-user-select] property. *)

val container_type : container_type -> declaration
(** [container_type v] is the CSS [container-type] property. *)

val container_name : string -> declaration
(** [container_name v] is the CSS [container-name] property. *)

val transform : transform -> declaration
(** [transform t] is the CSS [transform] property with a single transformation.
*)

val transforms : transform list -> declaration
(** [transforms ts] is the CSS [transform] property with multiple
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

val columns : columns_value -> declaration
(** [columns v] is the CSS [columns] property for multi-column layout. *)

val padding_inline : length -> declaration
(** [padding_inline v] is the CSS [padding-inline] property. *)

val padding_inline_start : length -> declaration
(** [padding_inline_start v] is the CSS [padding-inline-start] property. *)

val padding_inline_end : length -> declaration
(** [padding_inline_end v] is the CSS [padding-inline-end] property. *)

val padding_block : length -> declaration
(** [padding_block v] is the CSS [padding-block] property. *)

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
