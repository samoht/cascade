(** CSS generation utilities *)

(* Simple string formatting utilities *)
let str ?(sep = "") segments = String.concat sep segments
let lines segments = str ~sep:"\n" segments

type var =
  | Color of string * int option (* color name and optional shade *)
  | Spacing of int (* spacing value *)
  | Font of string (* font family *)
  | Text_size of string (* text size *)
  | Font_weight of string (* font weight *)
  | Radius of string (* border radius *)
  | Transition (* transition timing *)
  | Custom of string * string (* custom variable name and value *)

type property =
  | Background_color
  | Color
  | Border_color
  | Border_style
  | Padding
  | Padding_left
  | Padding_right
  | Padding_bottom
  | Padding_top
  | Padding_inline
  | Padding_block
  | Margin
  | Margin_left
  | Margin_right
  | Margin_top
  | Margin_bottom
  | Margin_inline
  | Margin_block
  | Gap
  | Column_gap
  | Row_gap
  | Width
  | Height
  | Min_width
  | Min_height
  | Max_width
  | Max_height
  | Font_size
  | Line_height
  | Font_weight
  | Font_style
  | Text_align
  | Text_decoration
  | Text_decoration_style
  | Text_underline_offset
  | Text_transform
  | Letter_spacing
  | White_space
  | Display
  | Position
  | Flex_direction
  | Flex_wrap
  | Flex
  | Flex_grow
  | Flex_shrink
  | Align_items
  | Justify_content
  | Align_content
  | Align_self
  | Justify_self
  | Place_content
  | Place_items
  | Place_self
  | Grid_template_columns
  | Grid_template_rows
  | Border_width
  | Border_radius
  | Box_shadow
  | Opacity
  | Transition
  | Transform
  | Cursor
  | Table_layout
  | Border_collapse
  | Border_spacing
  | User_select
  | Pointer_events
  | Overflow
  | Object_fit
  | Top
  | Right
  | Bottom
  | Left
  | Z_index
  | Border_top_width
  | Border_right_width
  | Border_bottom_width
  | Border_left_width
  | Outline
  | Outline_offset
  | Clip
  | Filter
  | Background_image
  | Animation
  | Appearance
  | Overflow_x
  | Overflow_y
  | Resize
  | Vertical_align
  | Box_sizing
  | Font_family
  | Background_position
  | Background_repeat
  | Background_size
  | Webkit_font_smoothing
  | Moz_osx_font_smoothing
  | Webkit_line_clamp
  | Backdrop_filter
  | Scroll_snap_type
  | Scroll_snap_align
  | Scroll_snap_stop
  | Scroll_behavior
  | Custom of string  (** CSS property names as a variant type *)

type declaration = property * string
(** A CSS property as (name, value) pair *)

(* Property constructor functions *)
let background_color value = (Background_color, value)
let color value = (Color, value)
let border_color value = (Border_color, value)
let border_style value = (Border_style, value)
let padding value = (Padding, value)
let padding_left value = (Padding_left, value)
let padding_right value = (Padding_right, value)
let padding_bottom value = (Padding_bottom, value)
let padding_top value = (Padding_top, value)
let padding_inline value = (Padding_inline, value)
let padding_block value = (Padding_block, value)
let margin value = (Margin, value)
let margin_left value = (Margin_left, value)
let margin_right value = (Margin_right, value)
let margin_top value = (Margin_top, value)
let margin_bottom value = (Margin_bottom, value)
let margin_inline value = (Margin_inline, value)
let margin_block value = (Margin_block, value)
let gap value = (Gap, value)
let column_gap value = (Column_gap, value)
let row_gap value = (Row_gap, value)
let width value = (Width, value)
let height value = (Height, value)
let min_width value = (Min_width, value)
let min_height value = (Min_height, value)
let max_width value = (Max_width, value)
let max_height value = (Max_height, value)
let font_size value = (Font_size, value)
let line_height value = (Line_height, value)
let font_weight value = (Font_weight, value)
let font_style value = (Font_style, value)
let text_align value = (Text_align, value)
let text_decoration value = (Text_decoration, value)
let text_decoration_style value = (Text_decoration_style, value)
let text_underline_offset value = (Text_underline_offset, value)
let text_transform value = (Text_transform, value)
let letter_spacing value = (Letter_spacing, value)
let white_space value = (White_space, value)
let display value = (Display, value)
let position value = (Position, value)
let flex_direction value = (Flex_direction, value)
let flex_wrap value = (Flex_wrap, value)
let flex value = (Flex, value)
let flex_grow value = (Flex_grow, value)
let flex_shrink value = (Flex_shrink, value)
let align_items value = (Align_items, value)
let justify_content value = (Justify_content, value)
let align_content value = (Align_content, value)
let align_self value = (Align_self, value)
let justify_self value = (Justify_self, value)
let place_content value = (Place_content, value)
let place_items value = (Place_items, value)
let place_self value = (Place_self, value)
let grid_template_columns value = (Grid_template_columns, value)
let grid_template_rows value = (Grid_template_rows, value)
let border_width value = (Border_width, value)
let border_radius value = (Border_radius, value)
let box_shadow value = (Box_shadow, value)
let opacity value = (Opacity, value)
let transition value = (Transition, value)
let transform value = (Transform, value)
let cursor value = (Cursor, value)
let table_layout value = (Table_layout, value)
let border_collapse value = (Border_collapse, value)
let border_spacing value = (Border_spacing, value)
let user_select value = (User_select, value)
let pointer_events value = (Pointer_events, value)
let overflow value = (Overflow, value)
let object_fit value = (Object_fit, value)
let top value = (Top, value)
let right value = (Right, value)
let bottom value = (Bottom, value)
let left value = (Left, value)
let z_index value = (Z_index, value)
let border_top_width value = (Border_top_width, value)
let border_right_width value = (Border_right_width, value)
let border_bottom_width value = (Border_bottom_width, value)
let border_left_width value = (Border_left_width, value)
let outline value = (Outline, value)
let outline_offset value = (Outline_offset, value)
let clip value = (Clip, value)
let filter value = (Filter, value)
let background_image value = (Background_image, value)
let animation value = (Animation, value)
let appearance value = (Appearance, value)
let overflow_x value = (Overflow_x, value)
let overflow_y value = (Overflow_y, value)
let resize value = (Resize, value)
let vertical_align value = (Vertical_align, value)
let box_sizing value = (Box_sizing, value)
let font_family value = (Font_family, value)
let background_position value = (Background_position, value)
let background_repeat value = (Background_repeat, value)
let background_size value = (Background_size, value)
let webkit_font_smoothing value = (Webkit_font_smoothing, value)
let moz_osx_font_smoothing value = (Moz_osx_font_smoothing, value)
let webkit_line_clamp value = (Webkit_line_clamp, value)
let backdrop_filter value = (Backdrop_filter, value)
let scroll_snap_type value = (Scroll_snap_type, value)
let scroll_snap_align value = (Scroll_snap_align, value)
let scroll_snap_stop value = (Scroll_snap_stop, value)
let scroll_behavior value = (Scroll_behavior, value)
let declaration name value = (Custom name, value)

type rule = { selector : string; declarations : declaration list }
type media_query = { media_condition : string; media_rules : rule list }

type container_query = {
  container_name : string option;
  container_condition : string;
  container_rules : rule list;
}

type starting_style = { starting_rules : rule list }

type supports_content =
  | Support_rules of rule list
  | Support_nested of rule list * supports_query list

and supports_query = {
  supports_condition : string;
  supports_content : supports_content;
}

type nested_rule = Rule of rule | Supports of supports_query

type at_property = {
  name : string;
  syntax : string;
  inherits : bool;
  initial_value : string;
}

type layer = Properties | Theme | Base | Components | Utilities

type layered_rules = {
  layer : layer;
  rules : nested_rule list;
  media_queries : media_query list;
  container_queries : container_query list;
  supports_queries : supports_query list;
}

type t = {
  layers : layered_rules list;
  rules : rule list;
  media_queries : media_query list;
  container_queries : container_query list;
  starting_styles : starting_style list;
  supports_queries : supports_query list;
  at_properties : at_property list;
}

type sheet_item =
  | Rule of rule
  | Media of media_query
  | Container of container_query
  | Starting_style of starting_style
  | Supports of supports_query
  | At_property of at_property
  | Layer of layered_rules

(** {1 Creation} *)

let declaration_value (_, value) = value
let declaration_property (prop_name, _) = prop_name

let is_custom_property (prop_name, _) =
  match prop_name with
  | Custom name -> String.starts_with ~prefix:"--" name
  | _ -> false

let string_of_property = function
  | Background_color -> "background-color"
  | Color -> "color"
  | Border_color -> "border-color"
  | Border_style -> "border-style"
  | Padding -> "padding"
  | Padding_left -> "padding-left"
  | Padding_right -> "padding-right"
  | Padding_bottom -> "padding-bottom"
  | Padding_top -> "padding-top"
  | Padding_inline -> "padding-inline"
  | Padding_block -> "padding-block"
  | Margin -> "margin"
  | Margin_left -> "margin-left"
  | Margin_right -> "margin-right"
  | Margin_top -> "margin-top"
  | Margin_bottom -> "margin-bottom"
  | Margin_inline -> "margin-inline"
  | Margin_block -> "margin-block"
  | Gap -> "gap"
  | Column_gap -> "column-gap"
  | Row_gap -> "row-gap"
  | Width -> "width"
  | Height -> "height"
  | Min_width -> "min-width"
  | Min_height -> "min-height"
  | Max_width -> "max-width"
  | Max_height -> "max-height"
  | Font_size -> "font-size"
  | Line_height -> "line-height"
  | Font_weight -> "font-weight"
  | Font_style -> "font-style"
  | Text_align -> "text-align"
  | Text_decoration -> "text-decoration"
  | Text_decoration_style -> "text-decoration-style"
  | Text_underline_offset -> "text-underline-offset"
  | Text_transform -> "text-transform"
  | Letter_spacing -> "letter-spacing"
  | White_space -> "white-space"
  | Display -> "display"
  | Position -> "position"
  | Flex_direction -> "flex-direction"
  | Flex_wrap -> "flex-wrap"
  | Flex -> "flex"
  | Flex_grow -> "flex-grow"
  | Flex_shrink -> "flex-shrink"
  | Align_items -> "align-items"
  | Justify_content -> "justify-content"
  | Align_content -> "align-content"
  | Align_self -> "align-self"
  | Justify_self -> "justify-self"
  | Place_content -> "place-content"
  | Place_items -> "place-items"
  | Place_self -> "place-self"
  | Grid_template_columns -> "grid-template-columns"
  | Grid_template_rows -> "grid-template-rows"
  | Border_width -> "border-width"
  | Border_radius -> "border-radius"
  | Box_shadow -> "box-shadow"
  | Opacity -> "opacity"
  | Transition -> "transition"
  | Transform -> "transform"
  | Cursor -> "cursor"
  | Table_layout -> "table-layout"
  | Border_collapse -> "border-collapse"
  | Border_spacing -> "border-spacing"
  | User_select -> "user-select"
  | Pointer_events -> "pointer-events"
  | Overflow -> "overflow"
  | Object_fit -> "object-fit"
  | Top -> "top"
  | Right -> "right"
  | Bottom -> "bottom"
  | Left -> "left"
  | Z_index -> "z-index"
  | Border_top_width -> "border-top-width"
  | Border_right_width -> "border-right-width"
  | Border_bottom_width -> "border-bottom-width"
  | Border_left_width -> "border-left-width"
  | Outline -> "outline"
  | Outline_offset -> "outline-offset"
  | Clip -> "clip"
  | Filter -> "filter"
  | Background_image -> "background-image"
  | Animation -> "animation"
  | Appearance -> "appearance"
  | Overflow_x -> "overflow-x"
  | Overflow_y -> "overflow-y"
  | Resize -> "resize"
  | Vertical_align -> "vertical-align"
  | Box_sizing -> "box-sizing"
  | Font_family -> "font-family"
  | Background_position -> "background-position"
  | Background_repeat -> "background-repeat"
  | Background_size -> "background-size"
  | Webkit_font_smoothing -> "-webkit-font-smoothing"
  | Moz_osx_font_smoothing -> "-moz-osx-font-smoothing"
  | Webkit_line_clamp -> "-webkit-line-clamp"
  | Backdrop_filter -> "backdrop-filter"
  | Scroll_snap_type -> "scroll-snap-type"
  | Scroll_snap_align -> "scroll-snap-align"
  | Scroll_snap_stop -> "scroll-snap-stop"
  | Scroll_behavior -> "scroll-behavior"
  | Custom s -> s

let rule ~selector declarations = { selector; declarations }
let selector rule = rule.selector
let declarations rule = rule.declarations

let media ~condition rules =
  { media_condition = condition; media_rules = rules }

let supports ~condition rules =
  { supports_condition = condition; supports_content = Support_rules rules }

let supports_nested ~condition rules nested_queries =
  {
    supports_condition = condition;
    supports_content = Support_nested (rules, nested_queries);
  }

let container ?(name = None) ~condition rules =
  {
    container_name = name;
    container_condition = condition;
    container_rules = rules;
  }

let at_property ~name ~syntax ~initial_value ?(inherits = false) () =
  { name; syntax; inherits; initial_value }

let rule_to_nested rule : nested_rule = Rule rule
let supports_to_nested supports : nested_rule = Supports supports

let layered_rules ~layer ?(media_queries = []) ?(container_queries = [])
    ?(supports_queries = []) rules =
  { layer; rules; media_queries; container_queries; supports_queries }

let empty =
  {
    layers = [];
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
        layers = acc.layers @ sheet.layers;
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
      | Rule r -> { acc with rules = acc.rules @ [ r ] }
      | Media m -> { acc with media_queries = acc.media_queries @ [ m ] }
      | Container c ->
          { acc with container_queries = acc.container_queries @ [ c ] }
      | Starting_style s ->
          { acc with starting_styles = acc.starting_styles @ [ s ] }
      | Supports s ->
          { acc with supports_queries = acc.supports_queries @ [ s ] }
      | At_property a -> { acc with at_properties = acc.at_properties @ [ a ] }
      | Layer l -> { acc with layers = acc.layers @ [ l ] })
    empty items

(** {1 Utilities} *)

(** Extract all CSS variables referenced in properties (for theme layer) *)
let all_vars properties =
  let extract_var_name var_content =
    match String.index var_content ',' with
    | exception Not_found -> String.trim var_content
    | comma -> String.trim (String.sub var_content 0 comma)
  in

  let process_var_at_position value var_pos acc =
    if var_pos + 4 > String.length value || String.sub value var_pos 4 <> "var("
    then None
    else
      let var_start = var_pos + 4 in
      match String.index_from value var_start ')' with
      | exception Not_found -> None
      | end_paren ->
          let var_content =
            String.sub value var_start (end_paren - var_start)
          in
          let var_name = extract_var_name var_content in
          let new_acc =
            if String.length var_name > 2 && String.sub var_name 0 2 = "--" then
              var_name :: acc
            else acc
          in
          Some (new_acc, end_paren + 1)
  in

  let rec extract_vars_from_value value acc pos =
    if pos >= String.length value then acc
    else
      try
        let var_pos = String.index_from value pos 'v' in
        match process_var_at_position value var_pos acc with
        | Some (new_acc, next_pos) ->
            extract_vars_from_value value new_acc next_pos
        | None -> extract_vars_from_value value acc (var_pos + 1)
      with Not_found -> acc
  in
  List.concat_map
    (fun (_, value) -> extract_vars_from_value value [] 0)
    properties
  |> List.sort_uniq String.compare

let deduplicate_declarations props =
  (* Keep last occurrence of each property while preserving order *)
  let seen = Hashtbl.create 16 in
  List.fold_right
    (fun (prop_name, value) acc ->
      if Hashtbl.mem seen prop_name then acc
      else (
        Hashtbl.add seen prop_name ();
        (prop_name, value) :: acc))
    props []

let inline_style_of_declarations props =
  props
  |> List.map (fun (prop_name, value) ->
         str [ string_of_property prop_name; ": "; value ])
  |> String.concat "; "

let merge_rules rules =
  (* Group rules by selector while preserving first occurrence order *)
  let rec merge_helper acc seen = function
    | [] -> List.rev acc
    | rule :: rest ->
        if List.mem rule.selector seen then
          (* Selector already seen, merge properties into existing rule *)
          let acc' =
            List.map
              (fun r ->
                if r.selector = rule.selector then
                  { r with declarations = r.declarations @ rule.declarations }
                else r)
              acc
          in
          merge_helper acc' seen rest
        else
          (* New selector, add to acc and mark as seen *)
          merge_helper
            ({
               rule with
               declarations = deduplicate_declarations rule.declarations;
             }
            :: acc)
            (rule.selector :: seen) rest
  in
  merge_helper [] [] rules
  |> List.map (fun r ->
         { r with declarations = deduplicate_declarations r.declarations })

(* Merge rules with identical properties into combined selectors *)
let merge_by_properties rules =
  (* Create a hash of properties for comparison *)
  let properties_hash props =
    props
    |> List.map (fun (name, value) ->
           str [ string_of_property name; ":"; value ])
    |> List.sort String.compare |> String.concat ";"
  in

  (* Group rules by their properties *)
  let groups = Hashtbl.create 16 in
  List.iter
    (fun rule ->
      let hash = properties_hash rule.declarations in
      let existing = try Hashtbl.find groups hash with Not_found -> [] in
      Hashtbl.replace groups hash (rule :: existing))
    rules;

  (* Create merged rules *)
  Hashtbl.fold
    (fun _hash rules_with_same_props acc ->
      match rules_with_same_props with
      | [] -> acc
      | [ rule ] -> rule :: acc
      | multiple ->
          let selectors =
            multiple
            |> List.map (fun r -> r.selector)
            |> List.sort String.compare |> String.concat ","
          in
          let declarations = (List.hd multiple).declarations in
          { selector = selectors; declarations } :: acc)
    groups []
  |> List.sort (fun a b -> String.compare a.selector b.selector)

let minify_selector s =
  (* Remove unnecessary whitespace in selectors *)
  s
  |> Re.replace_string
       (Re.compile (Re.seq [ Re.rep Re.space; Re.char '>'; Re.rep Re.space ]))
       ~by:">"
  |> Re.replace_string
       (Re.compile (Re.seq [ Re.rep Re.space; Re.char '+'; Re.rep Re.space ]))
       ~by:"+"
  |> Re.replace_string
       (Re.compile (Re.seq [ Re.rep Re.space; Re.char '~'; Re.rep Re.space ]))
       ~by:"~"
  |> Re.replace_string
       (Re.compile (Re.seq [ Re.rep Re.space; Re.char ','; Re.rep Re.space ]))
       ~by:","
  |> Re.replace_string
       (Re.compile (Re.seq [ Re.rep Re.space; Re.char ':'; Re.rep Re.space ]))
       ~by:":"
  |> String.trim

let minify_value v =
  let v = String.trim v in
  (* Special case: plain "0" or "1" should stay as is *)
  if v = "0" || v = "1" then v
  else
    let v =
      (* Remove spaces after commas in specific contexts *)
      (* Keep spaces in: color functions like rgb(), hsl(), color-mix() *)
      if
        Re.execp
          (Re.compile
             (Re.str "\\b(rgb|rgba|hsl|hsla|color-mix|oklab|oklch)\\s*\\("))
          v
      then v (* Don't remove spaces in color functions *)
      else Re.replace_string (Re.compile (Re.str ", ")) ~by:"," v
    in
    let v =
      (* Remove spaces in calc expressions *)
      if String.contains v '(' && String.contains v ')' then
        Re.replace_string (Re.compile (Re.str " * ")) ~by:"*" v
        |> Re.replace_string (Re.compile (Re.str " + ")) ~by:"+"
        |> Re.replace_string (Re.compile (Re.str " - ")) ~by:"-"
        |> Re.replace_string (Re.compile (Re.str " / ")) ~by:"/"
      else v
    in
    (* Remove leading 0 from decimals in oklch values and measurements *)
    (* Handle oklch values specially - replace " 0." with " ." *)
    let v =
      if String.contains v 'o' && String.contains v 'k' then
        Re.replace_string (Re.compile (Re.str " 0.")) ~by:" ." v
      else v
    in
    (* Remove leading 0 from decimals but preserve units - e.g., "0.5rem" ->
       ".5rem" *)
    let decimal_re =
      Re.compile
        (Re.seq
           [
             Re.bos;
             Re.str "0.";
             Re.group (Re.rep1 Re.digit);
             Re.group (Re.rep Re.any);
           ])
    in
    match Re.exec_opt decimal_re v with
    | Some m -> str [ "."; Re.Group.get m 1; Re.Group.get m 2 ]
    | None -> v

(** {1 Rendering} *)

let render_minified_rule rule =
  let selector = minify_selector rule.selector in
  let props =
    rule.declarations
    |> List.map (fun (prop_name, value) ->
           str [ string_of_property prop_name; ":"; minify_value value ])
  in
  str [ selector; "{"; str ~sep:";" props; "}" ]

let render_formatted_rule rule =
  let props =
    rule.declarations
    |> List.map (fun (prop_name, value) ->
           str [ "  "; string_of_property prop_name; ": "; value; ";" ])
  in
  lines [ str [ rule.selector; " {" ]; lines props; "}" ]

let render_formatted_media_rule rule =
  let props =
    rule.declarations
    |> List.map (fun (prop_name, value) ->
           str [ "    "; string_of_property prop_name; ": "; value; ";" ])
  in
  str [ "  "; rule.selector; " {\n"; lines props; "\n  }" ]

let layer_to_string = function
  | Properties -> "properties"
  | Theme -> "theme"
  | Base -> "base"
  | Components -> "components"
  | Utilities -> "utilities"

let rec render_supports_content ~minify content =
  match content with
  | Support_rules rules ->
      if minify then
        rules |> merge_rules |> merge_by_properties
        |> List.map render_minified_rule
        |> String.concat ""
      else rules |> List.map render_formatted_media_rule |> String.concat "\n"
  | Support_nested (rules, nested_queries) ->
      let rules_str =
        if minify then
          rules |> merge_rules |> merge_by_properties
          |> List.map render_minified_rule
          |> String.concat ""
        else rules |> List.map render_formatted_media_rule |> String.concat "\n"
      in
      let nested_str =
        nested_queries
        |> List.map (fun nsq ->
               let content_str =
                 render_supports_content ~minify nsq.supports_content
               in
               if minify then
                 str
                   [
                     "@supports "; nsq.supports_condition; "{"; content_str; "}";
                   ]
               else
                 str
                   [
                     "@supports ";
                     nsq.supports_condition;
                     " {\n";
                     content_str;
                     "\n}";
                   ])
        |> String.concat (if minify then "" else "\n")
      in
      if minify then str [ rules_str; nested_str ]
      else str [ rules_str; "\n"; nested_str ]

(* Version information *)
let version =
  match Build_info.V1.version () with
  | None -> "dev"
  | Some v -> Build_info.V1.Version.to_string v

let header =
  str
    [ "/*! tw v"; version; " | MIT License | https://github.com/samoht/tw */" ]

(* Configuration for stylesheet rendering *)
type config = { minify : bool }

(* TODO: Complete migration to structured CSS representation Intermediate
   representation for CSS output type css_block = | RuleBlock of { selector:
   string; properties: (declaration_property * string) list } | MediaBlock of {
   condition: string; blocks: css_block list } | ContainerBlock of { name:
   string option; condition: string; blocks: css_block list } | SupportsBlock of
   { condition: string; blocks: css_block list } | LayerBlock of { name: string;
   blocks: css_block list } | StartingStyleBlock of { blocks: css_block list } |
   PropertyBlock of { name: string; syntax: string; inherits: bool;
   initial_value: string } | Raw of string

   let rules_to_blocks rules = List.map (fun r -> RuleBlock { selector =
   r.selector; properties = r.declarations }) rules

   let rec format_block ~config block = match block with | RuleBlock { selector;
   properties } -> let sel = if config.minify then minify_selector selector else
   selector in let props = properties |> List.map (fun (name, value) -> let
   prop_name = string_of_property name in let val_str = if config.minify then
   minify_value value else value in str [prop_name; ":"; val_str]) in if
   config.minify then str [sel; "{"; str ~sep:";" props; "}"] else let
   prop_lines = props |> List.map (fun p -> " " ^ p ^ ";") |> String.concat "\n"
   in str [sel; " {\n"; prop_lines; "\n}"]

   | MediaBlock { condition; blocks } -> let content = blocks |> List.map
   (format_block ~config) |> String.concat (if config.minify then "" else "\n")
   in if config.minify then str ["@media "; condition; "{"; content; "}"] else
   let indented = blocks |> List.map (format_block ~config) |> List.map (fun s
   -> " " ^ String.concat "\n " (String.split_on_char '\n' s)) |> String.concat
   "\n" in str ["@media "; condition; " {\n"; indented; "\n}"]

   | ContainerBlock { name; condition; blocks } -> let name_part = match name
   with None -> "" | Some n -> n ^ " " in let content = blocks |> List.map
   (format_block ~config) |> String.concat (if config.minify then "" else "\n")
   in if config.minify then str ["@container "; name_part; condition; "{";
   content; "}"] else let indented = blocks |> List.map (format_block ~config)
   |> List.map (fun s -> " " ^ String.concat "\n " (String.split_on_char '\n'
   s)) |> String.concat "\n" in str ["@container "; name_part; condition; "
   {\n"; indented; "\n}"]

   | SupportsBlock { condition; blocks } -> let content = blocks |> List.map
   (format_block ~config) |> String.concat (if config.minify then "" else "\n")
   in if config.minify then str ["@supports "; condition; "{"; content; "}"]
   else let indented = blocks |> List.map (format_block ~config) |> List.map
   (fun s -> " " ^ String.concat "\n " (String.split_on_char '\n' s)) |>
   String.concat "\n" in str ["@supports "; condition; " {\n"; indented; "\n}"]

   | LayerBlock { name; blocks } -> if blocks = [] then str ["@layer "; name;
   ";"] else let content = blocks |> List.map (format_block ~config) |>
   String.concat (if config.minify then "" else "\n") in if config.minify then
   str ["@layer "; name; "{"; content; "}"] else let indented = blocks |>
   List.map (format_block ~config) |> List.map (fun s -> " " ^ String.concat "\n
   " (String.split_on_char '\n' s)) |> String.concat "\n" in str ["@layer ";
   name; " {\n"; indented; "\n}"]

   | StartingStyleBlock { blocks } -> let content = blocks |> List.map
   (format_block ~config) |> String.concat (if config.minify then "" else "\n")
   in if config.minify then str ["@starting-style{"; content; "}"] else let
   indented = blocks |> List.map (format_block ~config) |> List.map (fun s -> "
   " ^ String.concat "\n " (String.split_on_char '\n' s)) |> String.concat "\n"
   in str ["@starting-style {\n"; indented; "\n}"]

   | PropertyBlock { name; syntax; inherits; initial_value } -> if config.minify
   then str ["@property "; name; "{syntax:\""; syntax; "\";inherits:"; (if
   inherits then "true" else "false"); ";initial-value:"; initial_value; "}"]
   else str ["@property "; name; " {\n syntax: \""; syntax; "\";\n inherits: ";
   (if inherits then "true" else "false"); ";\n initial-value: "; initial_value;
   ";\n}"]

   | Raw s -> s *)

(* Helper: Render rules string for a layer *)
let render_layer_rules ~config rules =
  let render_nested_rule : nested_rule -> string = function
    | Rule r ->
        if config.minify then render_minified_rule r
        else render_formatted_rule r
    | Supports sq ->
        let sq_content =
          render_supports_content ~minify:config.minify sq.supports_content
        in
        if config.minify then
          str [ "@supports "; sq.supports_condition; "{"; sq_content; "}" ]
        else
          str [ "@supports "; sq.supports_condition; " {\n"; sq_content; "\n}" ]
  in

  (* Render rules in order without merging *)
  rules
  |> List.map render_nested_rule
  |> String.concat (if config.minify then "" else "\n")

(* Helper: Render media queries *)
let render_layer_media ~config media_queries =
  media_queries
  |> List.map (fun mq ->
         let content =
           if config.minify then
             mq.media_rules |> merge_rules |> merge_by_properties
             |> List.map render_minified_rule
             |> String.concat ""
           else
             mq.media_rules
             |> List.map render_formatted_media_rule
             |> String.concat "\n"
         in
         if config.minify then
           str [ "@media "; mq.media_condition; "{"; content; "}" ]
         else str [ "@media "; mq.media_condition; " {\n"; content; "\n}" ])
  |> String.concat (if config.minify then "" else "\n")

(* Helper: Render container queries *)
let render_layer_containers ~config container_queries =
  container_queries
  |> List.map (fun cq ->
         let name_part =
           match cq.container_name with None -> "" | Some name -> name ^ " "
         in
         let content =
           if config.minify then
             cq.container_rules |> merge_rules |> merge_by_properties
             |> List.map render_minified_rule
             |> String.concat ""
           else
             cq.container_rules
             |> List.map render_formatted_media_rule
             |> String.concat "\n"
         in
         if config.minify then
           str
             [
               "@container ";
               name_part;
               cq.container_condition;
               "{";
               content;
               "}";
             ]
         else
           str
             [
               "@container ";
               name_part;
               cq.container_condition;
               " {\n";
               content;
               "\n}";
             ])
  |> String.concat (if config.minify then "" else "\n")

(* Helper: Render supports queries *)
let render_layer_supports ~config supports_queries =
  supports_queries
  |> List.map (fun sq ->
         let content =
           render_supports_content ~minify:config.minify sq.supports_content
         in
         if config.minify then
           str [ "@supports "; sq.supports_condition; "{"; content; "}" ]
         else
           str [ "@supports "; sq.supports_condition; " {\n"; content; "\n}" ])
  |> String.concat (if config.minify then "" else "\n")

(* Helper: Check if layer is empty *)
let is_layer_empty (lr : layered_rules) =
  lr.rules = [] && lr.media_queries = [] && lr.container_queries = []
  && lr.supports_queries = []

(* Helper: Render non-layered elements *)
let render_stylesheet_rules ~config rules =
  if config.minify then
    rules |> merge_rules |> merge_by_properties
    |> List.map render_minified_rule
    |> String.concat ""
  else rules |> List.map render_formatted_rule |> String.concat "\n"

let render_stylesheet_media ~config media_queries =
  media_queries
  |> List.map (fun mq ->
         let content =
           if config.minify then
             mq.media_rules |> merge_rules |> merge_by_properties
             |> List.map render_minified_rule
             |> String.concat ""
           else
             mq.media_rules
             |> List.map render_formatted_rule
             |> String.concat "\n"
         in
         if config.minify then
           str [ "@media "; mq.media_condition; "{"; content; "}" ]
         else str [ "@media "; mq.media_condition; " {\n"; content; "\n}" ])
  |> String.concat (if config.minify then "" else "\n")

let render_stylesheet_containers ~config container_queries =
  container_queries
  |> List.map (fun cq ->
         let name_part =
           match cq.container_name with None -> "" | Some name -> name ^ " "
         in
         let content =
           if config.minify then
             cq.container_rules |> merge_rules |> merge_by_properties
             |> List.map render_minified_rule
             |> String.concat ""
           else
             cq.container_rules
             |> List.map render_formatted_rule
             |> String.concat "\n"
         in
         if config.minify then
           str
             [
               "@container ";
               name_part;
               cq.container_condition;
               "{";
               content;
               "}";
             ]
         else
           str
             [
               "@container ";
               name_part;
               cq.container_condition;
               " {\n";
               content;
               "\n}";
             ])
  |> String.concat (if config.minify then "" else "\n")

let render_stylesheet_supports ~config supports_queries =
  supports_queries
  |> List.map (fun sq ->
         let content =
           render_supports_content ~minify:config.minify sq.supports_content
         in
         if config.minify then
           str [ "@supports "; sq.supports_condition; "{"; content; "}" ]
         else
           str [ "@supports "; sq.supports_condition; " {\n"; content; "\n}" ])
  |> String.concat (if config.minify then "" else "\n")

let render_starting_styles ~config starting_styles =
  starting_styles
  |> List.map (fun ss ->
         let content =
           if config.minify then
             ss.starting_rules |> merge_rules |> merge_by_properties
             |> List.map render_minified_rule
             |> String.concat ""
           else
             ss.starting_rules
             |> List.map render_formatted_rule
             |> String.concat "\n"
         in
         if config.minify then str [ "@starting-style{"; content; "}" ]
         else str [ "@starting-style {\n"; content; "\n}" ])
  |> String.concat (if config.minify then "" else "\n")

let render_at_properties ~config at_properties =
  at_properties
  |> List.map (fun at ->
         if config.minify then
           let initial_value_part =
             if at.initial_value = "" then ""
             else str [ ";initial-value:"; at.initial_value ]
           in
           str
             [
               "@property ";
               at.name;
               "{syntax:\"";
               at.syntax;
               "\";inherits:";
               (if at.inherits then "true" else "false");
               initial_value_part;
               "}";
             ]
         else
           let initial_value_part =
             if at.initial_value = "" then ""
             else str [ ";\n  initial-value: "; at.initial_value ]
           in
           str
             [
               "@property ";
               at.name;
               " {\n  syntax: \"";
               at.syntax;
               "\";\n  inherits: ";
               (if at.inherits then "true" else "false");
               initial_value_part;
               ";\n}";
             ])
  |> String.concat (if config.minify then "" else "\n")

(* Helper functions for to_string *)
let render_layer ~config layer_rules =
  let layer_name = layer_to_string layer_rules.layer in
  let all_parts =
    [
      render_layer_rules ~config layer_rules.rules;
      render_layer_media ~config layer_rules.media_queries;
      render_layer_containers ~config layer_rules.container_queries;
      render_layer_supports ~config layer_rules.supports_queries;
    ]
    |> List.filter (fun s -> s <> "")
  in
  if all_parts = [] then str [ "@layer "; layer_name; ";" ]
  else
    let content =
      String.concat (if config.minify then "" else "\n") all_parts
    in
    if config.minify then str [ "@layer "; layer_name; "{"; content; "}" ]
    else str [ "@layer "; layer_name; " {\n"; content; "\n}" ]

let prepare_layer_strings ~config stylesheet =
  let has_empty_components_and_utilities =
    List.exists
      (fun lr -> lr.layer = Components && is_layer_empty lr)
      stylesheet.layers
    && List.exists
         (fun lr -> lr.layer = Utilities && is_layer_empty lr)
         stylesheet.layers
  in
  let layer_strings =
    if has_empty_components_and_utilities && config.minify then
      stylesheet.layers
      |> List.filter (fun lr ->
             not
               ((lr.layer = Components || lr.layer = Utilities)
               && is_layer_empty lr))
      |> List.map (render_layer ~config)
    else stylesheet.layers |> List.map (render_layer ~config)
  in
  let empty_layers_decl =
    if has_empty_components_and_utilities && config.minify then
      [ "@layer components,utilities;" ]
    else []
  in
  (layer_strings, empty_layers_decl)

let render_optional_section render_fn items =
  let rendered = render_fn items in
  if rendered = "" then [] else [ rendered ]

let to_string ?(minify = false) stylesheet =
  let config = { minify } in
  (* Add tw library header *)
  let header_str =
    if List.length stylesheet.layers > 0 then str [ header; "\n" ] else ""
  in

  (* Prepare layer strings *)
  let layer_strings, empty_layers_decl =
    prepare_layer_strings ~config stylesheet
  in

  (* Render all optional sections *)
  let rule_strings =
    render_optional_section (render_stylesheet_rules ~config) stylesheet.rules
  in
  let at_property_strings =
    render_optional_section
      (render_at_properties ~config)
      stylesheet.at_properties
  in
  let starting_style_strings =
    render_optional_section
      (render_starting_styles ~config)
      stylesheet.starting_styles
  in
  let container_strings =
    render_optional_section
      (render_stylesheet_containers ~config)
      stylesheet.container_queries
  in
  let supports_strings =
    render_optional_section
      (render_stylesheet_supports ~config)
      stylesheet.supports_queries
  in
  let media_strings =
    render_optional_section
      (render_stylesheet_media ~config)
      stylesheet.media_queries
  in

  (* Combine all parts *)
  let all_parts =
    [ header_str; "" ] (* layer_declarations is always empty for Tailwind v4 *)
    @ layer_strings @ empty_layers_decl @ rule_strings @ starting_style_strings
    @ container_strings @ supports_strings @ media_strings @ at_property_strings
  in

  if config.minify then String.concat "" all_parts
  else String.concat "\n" (List.filter (fun s -> s <> "") all_parts)

let pp = to_string
