(** CSS generation utilities *)

(* Simple string formatting utilities *)
let pr ?(sep = "") segments = String.concat sep segments
let lines segments = pr ~sep:"\n" segments
let indent n s = String.make n ' ' ^ s

type var =
  | Color of string * int option (* color name and optional shade *)
  | Spacing of int (* spacing value *)
  | Font of string (* font family *)
  | Text_size of string (* text size *)
  | Font_weight of string (* font weight *)
  | Radius of string (* border radius *)
  | Transition (* transition timing *)
  | Custom of string * string (* custom variable name and value *)

type property_name =
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

type property = property_name * string
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
let property name value = (Custom name, value)

type rule = { selector : string; properties : property list }
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

type rule_or_nested = Rule of rule | Nested_supports of supports_query

type at_property = {
  name : string;
  syntax : string;
  inherits : bool;
  initial_value : string;
}

type layer = Properties | Theme | Base | Components | Utilities

type layered_rules = {
  layer : layer;
  rules : rule_or_nested list;
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

(** {1 Creation} *)

let property_value (_, value) = value
let property_name (prop_name, _) = prop_name

let is_custom_property (prop_name, _) =
  match prop_name with
  | Custom name -> String.starts_with ~prefix:"--" name
  | _ -> false

let property_name_to_string = function
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

let rule ~selector properties = { selector; properties }
let selector rule = rule.selector
let properties rule = rule.properties

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

let rule_to_nested rule = Rule rule
let supports_to_nested supports = Nested_supports supports

let layered_rules ~layer ?(media_queries = []) ?(container_queries = [])
    ?(supports_queries = []) rules =
  { layer; rules; media_queries; container_queries; supports_queries }

let stylesheet ?(layers = []) ?(media_queries = []) ?(container_queries = [])
    ?(starting_styles = []) ?(supports_queries = []) ?(at_properties = []) rules
    =
  {
    layers;
    rules;
    media_queries;
    container_queries;
    starting_styles;
    supports_queries;
    at_properties;
  }

(** {1 Utilities} *)

(** Extract all CSS variables referenced in properties (for theme layer) *)
let all_vars properties =
  let rec extract_vars_from_value value acc pos =
    if pos >= String.length value then acc
    else
      try
        let var_pos = String.index_from value pos 'v' in
        if
          var_pos + 4 <= String.length value
          && String.sub value var_pos 4 = "var("
        then
          (* Found var( *)
          let var_start = var_pos + 4 in
          match String.index_from value var_start ')' with
          | exception Not_found -> acc
          | end_paren ->
              let var_content =
                String.sub value var_start (end_paren - var_start)
              in
              (* Extract just the variable name *)
              let var_name =
                match String.index var_content ',' with
                | exception Not_found -> String.trim var_content
                | comma -> String.trim (String.sub var_content 0 comma)
              in
              let acc' =
                if String.length var_name > 2 && String.sub var_name 0 2 = "--"
                then var_name :: acc
                else acc
              in
              extract_vars_from_value value acc' (end_paren + 1)
        else extract_vars_from_value value acc (var_pos + 1)
      with Not_found -> acc
  in
  List.concat_map
    (fun (_, value) -> extract_vars_from_value value [] 0)
    properties
  |> List.sort_uniq String.compare

let deduplicate_properties props =
  (* Keep last occurrence of each property while preserving order *)
  let seen = Hashtbl.create 16 in
  List.fold_right
    (fun (prop_name, value) acc ->
      if Hashtbl.mem seen prop_name then acc
      else (
        Hashtbl.add seen prop_name ();
        (prop_name, value) :: acc))
    props []

let properties_to_inline_style props =
  props
  |> List.map (fun (prop_name, value) ->
         property_name_to_string prop_name ^ ": " ^ value)
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
                  { r with properties = r.properties @ rule.properties }
                else r)
              acc
          in
          merge_helper acc' seen rest
        else
          (* New selector, add to acc and mark as seen *)
          merge_helper
            ({ rule with properties = deduplicate_properties rule.properties }
            :: acc)
            (rule.selector :: seen) rest
  in
  merge_helper [] [] rules
  |> List.map (fun r ->
         { r with properties = deduplicate_properties r.properties })

(* Merge rules with identical properties into combined selectors *)
let merge_by_properties rules =
  (* Create a hash of properties for comparison *)
  let properties_hash props =
    props
    |> List.map (fun (name, value) ->
           property_name_to_string name ^ ":" ^ value)
    |> List.sort String.compare |> String.concat ";"
  in

  (* Group rules by their properties *)
  let groups = Hashtbl.create 16 in
  List.iter
    (fun rule ->
      let hash = properties_hash rule.properties in
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
          let properties = (List.hd multiple).properties in
          { selector = selectors; properties } :: acc)
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
    | Some m -> "." ^ Re.Group.get m 1 ^ Re.Group.get m 2
    | None -> v

(** {1 Rendering} *)

let render_minified_rule rule =
  let selector = minify_selector rule.selector in
  let props =
    rule.properties
    |> List.map (fun (prop_name, value) ->
           property_name_to_string prop_name ^ ":" ^ minify_value value)
  in
  pr [ selector; "{"; pr ~sep:";" props; "}" ]

let render_formatted_rule rule =
  let props =
    rule.properties
    |> List.map (fun (prop_name, value) ->
           pr [ "  "; property_name_to_string prop_name; ": "; value; ";" ])
  in
  lines [ pr [ rule.selector; " {" ]; lines props; "}" ]

let render_formatted_media_rule rule =
  let props =
    rule.properties
    |> List.map (fun (prop_name, value) ->
           pr [ "    "; property_name_to_string prop_name; ": "; value; ";" ])
  in
  pr [ "  "; rule.selector; " {\n"; lines props; "\n  }" ]

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
                 "@supports " ^ nsq.supports_condition ^ "{" ^ content_str ^ "}"
               else
                 "@supports " ^ nsq.supports_condition ^ " {\n" ^ content_str
                 ^ "\n}")
        |> String.concat (if minify then "" else "\n")
      in
      if minify then rules_str ^ nested_str else rules_str ^ "\n" ^ nested_str

(* Version information *)
let version =
  match Build_info.V1.version () with
  | None -> "dev"
  | Some v -> Build_info.V1.Version.to_string v

let header =
  String.concat ""
    [ "/*! tw v"; version; " | MIT License | https://github.com/samoht/tw */" ]

let to_string ?(minify = false) ?(preserve_order = false) stylesheet =
  let render_layer layer_rules =
    let layer_name = layer_to_string layer_rules.layer in
    let rules = layer_rules.rules in
    let media_queries = layer_rules.media_queries in
    let container_queries = layer_rules.container_queries in
    let supports_queries = layer_rules.supports_queries in

    if minify then
      (* Render rule_or_nested items *)
      let render_rule_or_nested = function
        | Rule r -> render_minified_rule r
        | Nested_supports sq ->
            let sq_content =
              render_supports_content ~minify:true sq.supports_content
            in
            "@supports " ^ sq.supports_condition ^ "{" ^ sq_content ^ "}"
      in

      let rules_str =
        (* For base and utilities layers, preserve exact order *)
        if
          preserve_order || layer_rules.layer = Base
          || layer_rules.layer = Utilities
        then rules |> List.map render_rule_or_nested |> String.concat ""
        else
          (* For other layers, separate rules from nested supports *)
          let regular_rules, nested_supports =
            List.partition_map
              (function
                | Rule r -> Either.Left r
                | Nested_supports sq -> Either.Right sq)
              rules
          in
          let merged_rules = merge_rules regular_rules |> merge_by_properties in
          let rules_part =
            merged_rules |> List.map render_minified_rule |> String.concat ""
          in
          let supports_part =
            nested_supports
            |> List.map (fun sq ->
                   let sq_content =
                     render_supports_content ~minify:true sq.supports_content
                   in
                   "@supports " ^ sq.supports_condition ^ "{" ^ sq_content ^ "}")
            |> String.concat ""
          in
          rules_part ^ supports_part
      in
      (* Render media queries within the layer *)
      let media_str =
        media_queries
        |> List.map (fun mq ->
               let mq_rules_str =
                 mq.media_rules |> merge_rules |> merge_by_properties
                 |> List.map render_minified_rule
                 |> String.concat ""
               in
               "@media " ^ mq.media_condition ^ "{" ^ mq_rules_str ^ "}")
        |> String.concat ""
      in
      (* Render container queries within the layer *)
      let container_str =
        container_queries
        |> List.map (fun cq ->
               let container_rule =
                 match cq.container_name with
                 | Some name ->
                     "@container " ^ name ^ " " ^ cq.container_condition
                 | None -> "@container " ^ cq.container_condition
               in
               let cq_rules_str =
                 cq.container_rules |> merge_rules |> merge_by_properties
                 |> List.map render_minified_rule
                 |> String.concat ""
               in
               container_rule ^ "{" ^ cq_rules_str ^ "}")
        |> String.concat ""
      in
      (* Render supports queries within the layer *)
      let supports_str =
        supports_queries
        |> List.map (fun sq ->
               let sq_rules_str =
                 render_supports_content ~minify:true sq.supports_content
               in
               "@supports " ^ sq.supports_condition ^ "{" ^ sq_rules_str ^ "}")
        |> String.concat ""
      in
      (* If layer is completely empty, render as declaration only *)
      if
        rules_str = "" && media_str = "" && container_str = ""
        && supports_str = ""
      then "@layer " ^ layer_name ^ ";"
      else
        "@layer " ^ layer_name ^ "{" ^ rules_str ^ media_str ^ container_str
        ^ supports_str ^ "}"
    else
      (* Render rule_or_nested items for non-minified output *)
      let render_rule_or_nested_formatted = function
        | Rule r -> render_formatted_rule r
        | Nested_supports sq ->
            let sq_content =
              render_supports_content ~minify:false sq.supports_content
            in
            "@supports " ^ sq.supports_condition ^ " {\n" ^ sq_content ^ "\n}"
      in
      let rules_str =
        rules |> List.map render_rule_or_nested_formatted |> String.concat "\n"
      in
      (* Render media queries within the layer *)
      let media_str =
        media_queries
        |> List.map (fun mq ->
               let mq_rules_str =
                 mq.media_rules
                 |> List.map render_formatted_media_rule
                 |> String.concat "\n"
               in
               "@media " ^ mq.media_condition ^ " {\n" ^ mq_rules_str ^ "\n}")
        |> String.concat "\n"
      in
      (* Render container queries within the layer *)
      let container_str =
        container_queries
        |> List.map (fun cq ->
               let container_rule =
                 match cq.container_name with
                 | Some name ->
                     "@container " ^ name ^ " " ^ cq.container_condition
                 | None -> "@container " ^ cq.container_condition
               in
               let cq_rules_str =
                 cq.container_rules
                 |> List.map render_formatted_media_rule
                 |> String.concat "\n"
               in
               container_rule ^ " {\n" ^ cq_rules_str ^ "\n}")
        |> String.concat "\n"
      in
      (* Render supports queries within the layer *)
      let supports_str =
        supports_queries
        |> List.map (fun sq ->
               let sq_rules_str =
                 render_supports_content ~minify:false sq.supports_content
               in
               "@supports " ^ sq.supports_condition ^ " {\n" ^ sq_rules_str
               ^ "\n}")
        |> String.concat "\n"
      in
      let all_content =
        [ rules_str; media_str; container_str; supports_str ]
        |> List.filter (fun s -> s <> "")
        |> String.concat "\n"
      in
      (* If layer is completely empty, render as declaration only *)
      if all_content = "" then "@layer " ^ layer_name ^ ";"
      else "@layer " ^ layer_name ^ " {\n" ^ all_content ^ "\n}"
  in

  (* Add tw library header *)
  let header_str =
    if List.length stylesheet.layers > 0 then header ^ "\n" else ""
  in

  (* No layer declarations - Tailwind v4 doesn't use them *)
  let layer_declarations = "" in

  (* Render layered rules *)
  (* Special handling for empty components and utilities layers *)
  let has_empty_components_and_utilities =
    List.exists
      (fun layer ->
        layer.layer = Components && layer.rules = [] && layer.media_queries = []
        && layer.container_queries = []
        && layer.supports_queries = [])
      stylesheet.layers
    && List.exists
         (fun layer ->
           layer.layer = Utilities && layer.rules = []
           && layer.media_queries = []
           && layer.container_queries = []
           && layer.supports_queries = [])
         stylesheet.layers
  in

  let layer_strings =
    if has_empty_components_and_utilities && minify then
      (* Filter out empty components and utilities, will add combined
         declaration later *)
      stylesheet.layers
      |> List.filter (fun layer ->
             (not (layer.layer = Components || layer.layer = Utilities))
             || layer.rules <> [] || layer.media_queries <> []
             || layer.container_queries <> []
             || layer.supports_queries <> [])
      |> List.map render_layer
    else stylesheet.layers |> List.map render_layer
  in

  (* Add combined empty layer declaration if needed *)
  let empty_layers_decl =
    if has_empty_components_and_utilities && minify then
      [ "@layer components,utilities;" ]
    else []
  in

  (* Render non-layered rules *)
  let rule_strings =
    if minify then
      let rules = merge_rules stylesheet.rules |> merge_by_properties in
      List.map render_minified_rule rules
    else List.map render_formatted_rule stylesheet.rules
  in

  (* Render @property rules *)
  let at_property_strings =
    stylesheet.at_properties
    |> List.map (fun (prop : at_property) ->
           if minify then
             let parts =
               [
                 "@property ";
                 prop.name;
                 "{syntax:";
                 prop.syntax;
                 ";inherits:";
                 (if prop.inherits then "true" else "false");
               ]
               @ (if prop.initial_value = "" then []
                  else [ ";initial-value:"; prop.initial_value ])
               @ [ "}" ]
             in
             pr parts
           else
             let line_list =
               [
                 pr [ "@property "; prop.name; " {" ];
                 pr [ "  syntax: "; prop.syntax; ";" ];
                 pr
                   [
                     "  inherits: ";
                     (if prop.inherits then "true" else "false");
                     ";";
                   ];
               ]
               @ (if prop.initial_value = "" then []
                  else [ pr [ "  initial-value: "; prop.initial_value; ";" ] ])
               @ [ "}" ]
             in
             lines line_list)
  in

  (* Render @starting-style rules *)
  let starting_style_strings =
    stylesheet.starting_styles
    |> List.map (fun (ss : starting_style) ->
           if minify then
             let rules_str =
               ss.starting_rules |> merge_rules |> merge_by_properties
               |> List.map render_minified_rule
               |> String.concat ""
             in
             "@starting-style{" ^ rules_str ^ "}"
           else
             let rules_str =
               ss.starting_rules
               |> List.map render_formatted_rule
               |> String.concat "\n"
             in
             lines [ "@starting-style {"; indent 2 rules_str; "}" ])
  in

  (* Render @container queries *)
  let container_strings =
    stylesheet.container_queries
    |> List.map (fun (cq : container_query) ->
           let container_rule =
             match cq.container_name with
             | Some name -> "@container " ^ name ^ " " ^ cq.container_condition
             | None -> "@container " ^ cq.container_condition
           in
           if minify then
             let rules_str =
               cq.container_rules |> merge_rules |> merge_by_properties
               |> List.map render_minified_rule
               |> String.concat ""
             in
             container_rule ^ "{" ^ rules_str ^ "}"
           else
             let rules_str =
               cq.container_rules
               |> List.map render_formatted_media_rule
               |> String.concat "\n"
             in
             lines [ pr [ container_rule; " {" ]; indent 2 rules_str; "}" ])
  in

  (* Render @supports queries *)
  let supports_strings =
    stylesheet.supports_queries
    |> List.map (fun (sq : supports_query) ->
           if minify then
             let rules_str =
               render_supports_content ~minify:true sq.supports_content
             in
             "@supports " ^ sq.supports_condition ^ "{" ^ rules_str ^ "}"
           else
             let rules_str =
               render_supports_content ~minify:false sq.supports_content
             in
             lines
               [
                 pr [ "@supports "; sq.supports_condition; " {" ];
                 indent 2 rules_str;
                 "}";
               ])
  in

  (* Render media queries *)
  let media_strings =
    stylesheet.media_queries
    |> List.map (fun (mq : media_query) ->
           if minify then
             let rules_str =
               mq.media_rules |> merge_rules |> merge_by_properties
               |> List.map render_minified_rule
               |> String.concat ""
             in
             "@media " ^ mq.media_condition ^ "{" ^ rules_str ^ "}"
           else
             let rules_str =
               mq.media_rules
               |> List.map render_formatted_media_rule
               |> String.concat "\n"
             in
             lines
               [
                 pr [ "@media "; mq.media_condition; " {" ];
                 indent 2 rules_str;
                 "}";
               ])
  in

  (* Combine all parts *)
  let all_parts =
    [ header_str; layer_declarations ]
    @ layer_strings @ empty_layers_decl @ rule_strings @ starting_style_strings
    @ container_strings @ supports_strings @ media_strings @ at_property_strings
  in

  if minify then String.concat "" all_parts
  else String.concat "\n" (List.filter (fun s -> s <> "") all_parts)

let pp stylesheet = to_string ~minify:false ~preserve_order:false stylesheet
