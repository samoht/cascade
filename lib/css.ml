(** CSS generation utilities - Pure re-export module *)

(* Module re-exports *)

module Pp = Pp
module Reader = Reader
module Loc = Loc
module Token = Token
module Syntax = Syntax
module Lexer = Lexer
module Component = Component
module Parser = Parser
module Cursor = Cursor
module Sort = Sort
module Error = Error
module Values = Values
module Context = Context
module Declaration = Declaration
module Properties = Properties
module Selector = Selector
module Selector_summary = Selector_summary
module Stylesheet = Stylesheet

let parse_full ~property read s =
  let c = Cursor.of_string s in
  Cursor.ws c;
  if Cursor.is_done c then
    Error (Error.bad_value Loc.dummy ~property ~reason:"empty value")
  else
    try
      let v = read c in
      Cursor.ws c;
      if not (Cursor.is_done c) then
        Error
          (Error.bad_value (Cursor.position c) ~property
             ~reason:"trailing input after parse")
      else Ok v
    with Cursor.Parse_error e -> Error e

module Transform = struct
  let of_string s = parse_full ~property:"transform" Properties.read_transform s
end

module Gradient_direction = struct
  let of_string s =
    parse_full ~property:"gradient-direction" Properties.read_gradient_prelude s
end

module Transform_origin = struct
  let of_string s =
    parse_full ~property:"transform-origin" Properties.read_transform_origin s
end

module Perspective_origin = struct
  let of_string s =
    parse_full ~property:"perspective-origin" Properties.read_perspective_origin
      s
end

module Animation = struct
  let of_string s = parse_full ~property:"animation" Properties.read_animation s
end

let eval_declaration ?layer_order ?layer ctx decl =
  Context.eval ?layer_order ?layer ctx decl

let eval_value ?layer_order ?layer ctx property value =
  eval_declaration ?layer_order ?layer ctx (Declaration.v property value)

let eval_page_margin_rule ~layer_order ?layer ctx rule =
  {
    rule with
    Stylesheet.descriptors =
      List.map
        (eval_declaration ~layer_order ?layer ctx)
        rule.Stylesheet.descriptors;
  }

let layer_known ~layer_order = function
  | None -> true
  | Some name -> List.exists (String.equal name) layer_order

let collect_cascade_rules ~layer_order stylesheet =
  let source_order = ref 0 in
  let add layer decl acc =
    let rule : Context.cascade_rule =
      {
        property_name = Declaration.property_name decl;
        important = Declaration.is_important decl;
        layer;
        source_order = !source_order;
        declaration = decl;
      }
    in
    incr source_order;
    rule :: acc
  in
  let rec statement current_layer acc =
    let open Stylesheet in
    function
    | Rule rule ->
        let acc =
          List.fold_left
            (fun acc decl -> add current_layer decl acc)
            acc rule.declarations
        in
        List.fold_left (statement current_layer) acc rule.nested
    | Declarations declarations ->
        List.fold_left
          (fun acc decl -> add current_layer decl acc)
          acc declarations
    | Layer (name, block) ->
        let current_layer =
          match name with Some _ -> name | None -> current_layer
        in
        List.fold_left (statement current_layer) acc block
    | Media (_, block)
    | Supports (_, block)
    | Moz_document (_, block)
    | When (_, block)
    | Else (_, block)
    | Starting_style block
    | Origin (_, block) ->
        List.fold_left (statement current_layer) acc block
    | Container (_, _, block) | Scope (_, _, block) ->
        List.fold_left (statement current_layer) acc block
    | _ -> acc
  in
  List.fold_left (statement None) [] stylesheet
  |> List.filter (fun (rule : Context.cascade_rule) ->
      layer_known ~layer_order rule.layer)

let eval_declarations ~layer_order ?layer ctx declarations =
  List.map (eval_declaration ~layer_order ?layer ctx) declarations

let eval_keyframes ~layer_order ?layer ctx frames =
  let eval_frame (frame : Stylesheet.keyframe) =
    {
      frame with
      Stylesheet.declarations =
        eval_declarations ~layer_order ?layer ctx frame.declarations;
    }
  in
  List.map eval_frame frames

let eval_declaration_statement ~layer_order ?layer:context_layer ctx =
  let open Stylesheet in
  function
  | Keyframes (name, frames) ->
      Some
        (Keyframes
           (name, eval_keyframes ~layer_order ?layer:context_layer ctx frames))
  | Webkit_keyframes (name, frames) ->
      Some
        (Webkit_keyframes
           (name, eval_keyframes ~layer_order ?layer:context_layer ctx frames))
  | Moz_keyframes (name, frames) ->
      Some
        (Moz_keyframes
           (name, eval_keyframes ~layer_order ?layer:context_layer ctx frames))
  | Page (selector, declarations) ->
      Some
        (Page
           ( selector,
             eval_declarations ~layer_order ?layer:context_layer ctx
               declarations ))
  | Position_try (name, declarations) ->
      Some
        (Position_try
           ( name,
             eval_declarations ~layer_order ?layer:context_layer ctx
               declarations ))
  | Supports_condition (name, declarations) ->
      Some
        (Supports_condition
           ( name,
             eval_declarations ~layer_order ?layer:context_layer ctx
               declarations ))
  | Page_with_margins (selector, descriptors, margins) ->
      Some
        (Page_with_margins
           ( selector,
             eval_declarations ~layer_order ?layer:context_layer ctx descriptors,
             List.map
               (eval_page_margin_rule ~layer_order ?layer:context_layer ctx)
               margins ))
  | _ -> None

let rec eval_block ?ctx_for_layer ~layer_order ?layer ctx block =
  List.map (eval_statement ?ctx_for_layer ~layer_order ?layer ctx) block

and eval_nested_block_statement ?ctx_for_layer ~layer_order ?layer:context_layer
    ctx =
  let open Stylesheet in
  let eval block =
    eval_block ?ctx_for_layer ~layer_order ?layer:context_layer ctx block
  in
  function
  | Media (condition, block) -> Some (Media (condition, eval block))
  | Container (name, condition, block) ->
      Some (Container (name, condition, eval block))
  | Supports (condition, block) -> Some (Supports (condition, eval block))
  | Moz_document (conditions, block) ->
      Some (Moz_document (conditions, eval block))
  | When (condition, block) -> Some (When (condition, eval block))
  | Else (condition, block) -> Some (Else (condition, eval block))
  | Starting_style block -> Some (Starting_style (eval block))
  | Origin (origin, block) -> Some (Origin (origin, eval block))
  | Scope (start, end_, block) -> Some (Scope (start, end_, eval block))
  | _ -> None

and eval_block_statement ?ctx_for_layer ~layer_order ?layer:context_layer ctx =
  let open Stylesheet in
  function
  | Rule rule ->
      Some
        (Rule
           (eval_rule_with_ctx ?ctx_for_layer ~layer_order ?layer:context_layer
              ctx rule))
  | Declarations declarations ->
      Some
        (Declarations
           (eval_declarations ~layer_order ?layer:context_layer ctx declarations))
  | Layer (name, block) ->
      let current_layer =
        match name with Some _ -> name | None -> context_layer
      in
      let ctx =
        match ctx_for_layer with Some f -> f current_layer | None -> ctx
      in
      Some
        (Layer
           ( name,
             eval_block ?ctx_for_layer ~layer_order ?layer:current_layer ctx
               block ))
  | statement ->
      eval_nested_block_statement ?ctx_for_layer ~layer_order
        ?layer:context_layer ctx statement

and eval_statement ?ctx_for_layer ~layer_order ?layer:context_layer ctx
    statement =
  match
    eval_block_statement ?ctx_for_layer ~layer_order ?layer:context_layer ctx
      statement
  with
  | Some statement -> statement
  | None -> (
      match
        eval_declaration_statement ~layer_order ?layer:context_layer ctx
          statement
      with
      | Some statement -> statement
      | None -> statement)

and eval_rule_with_ctx ?ctx_for_layer ~layer_order ?layer:context_layer ctx rule
    =
  {
    rule with
    Stylesheet.declarations =
      List.map
        (eval_declaration ~layer_order ?layer:context_layer ctx)
        rule.Stylesheet.declarations;
    nested =
      List.map
        (eval_statement ?ctx_for_layer ~layer_order ?layer:context_layer ctx)
        rule.nested;
  }

let eval_rule ?layer_order ?layer ctx rule =
  let layer_order = Option.value ~default:ctx.Context.layer_order layer_order in
  let layer = match layer with Some _ -> layer | None -> ctx.Context.layer in
  eval_rule_with_ctx ~layer_order ?layer
    { ctx with Context.layer_order; layer }
    rule

let eval_stylesheet ?layer_order ?layer ctx stylesheet =
  let layer_order = Option.value ~default:ctx.Context.layer_order layer_order in
  let layer = match layer with Some _ -> layer | None -> ctx.Context.layer in
  let cascade_rules = collect_cascade_rules ~layer_order stylesheet in
  let ctx_for_layer layer =
    {
      ctx with
      Context.layer_order;
      layer;
      Context.cascade_rules = Some cascade_rules;
    }
  in
  List.map
    (eval_statement ~ctx_for_layer ~layer_order ?layer (ctx_for_layer layer))
    stylesheet

module Variables = Variables
module Optimize = Optimize
module Media = Media
module Container = Container
module Supports = Supports
module Keyframe = Keyframe
module Font_face = Font_face

(* Include all public APIs except Stylesheet *)

include Values
include Declaration
include Properties
include Variables
include Optimize
include Stylesheet

let radius value = Radius { horizontal = [ Length value ]; vertical = None }
let gaps ?column row : gap = Lengths { row_gap = Some row; column_gap = column }
let font_stack fonts = (List fonts : font_family)
let ratio width height = (Ratio_calc (Num width, Num height) : aspect_ratio)

let auto_ratio width height =
  (Auto_ratio_calc (Num width, Num height) : aspect_ratio)

let position_xy x y = (XY (x, y) : position_value)
let position_length value = (Single value : position_value)
let text_overflow_string value = (String value : text_overflow)
let text_overflow_pair start end_ = (Pair (start, end_) : text_overflow)
let content_string value = (String value : content)

let content_attr name =
  (Attr { name; type_ = None; fallback = No_fallback } : content)

let content_counter name = (Counter name : content)
let content_counters name separator = (Counters (name, separator) : content)
let content_list items = (Content_list items : content)
let columns_count count = (Count count : columns_value)
let columns_width width = (Width width : columns_value)
let columns_both width count = (Both (width, count) : columns_value)
let counter_item ?value name : counter_item = { name; value }
let counter_set items = (Counters items : counter_set)
let background_size_pair width height = (Size (width, height) : background_size)

let mask_layer ?image ?position ?size ?repeat ?origin ?clip ?mode ?composite ()
    =
  { image; position; size; repeat; origin; clip; mode; composite }

let mask_layers layers = (Layers layers : mask)
let gradient_stops stops = (List stops : gradient_stop)
let gradient_hint_length value = (Length value : gradient_stop)
let gradient_hint_percentage value = (Percentage value : gradient_stop)

let radial_gradient_config ?shape ?size ?position ?interpolation () =
  { shape; size; position; interpolation }

let conic_gradient_config ?angle ?position ?interpolation () =
  { angle; position; interpolation }

let conic_gradient
    ?(config : conic_gradient_config =
      { angle = None; position = None; interpolation = None }) stops =
  (Conic_gradient (config, stops) : background_image)

let object_view_box_inset ?right ?bottom ?left top : object_view_box =
  Inset (top, right, bottom, left)

let grid_tracks tracks = (Tracks tracks : grid_template)
let grid_repeat count tracks = (Repeat (count, tracks) : grid_template)
let grid_line_num value = (Num value : grid_line)
let grid_line_name value = (Name value : grid_line)
let grid_line_span value = (Span value : grid_line)
let grid_line_span_name value = (Span_name value : grid_line)
let grid_lines start end_ = (Lines (start, end_) : grid_line_pair)

let outline_shorthand ?width ?style ?color () : outline =
  Shorthand { width; style; color }

let logical_border_color value = (Single value : logical_border_color)

let logical_border_colors start end_ =
  (Pair (start, end_) : logical_border_color)

let logical_border_width value = (Single value : logical_border_width)

let logical_border_widths start end_ =
  (Pair (start, end_) : logical_border_width)

let transform_list items = (List items : transform)
let filter_list items = (List items : filter)
let cursor_url ?hotspot ~fallback url = (Url (url, hotspot, fallback) : cursor)
let contain_list items = (List items : contain)
let border_spacing_values values = (Lengths values : border_spacing)
let list_style_symbol_string value = (String value : list_style_symbol)
let list_style_symbol_url value = (Url value : list_style_symbol)
let list_style_string value = (String value : list_style_type)

let list_style_symbols ?kind symbols =
  (Symbols (kind, symbols) : list_style_type)

let list_style_image_url value = (Url value : list_style_image)
let svg_paint_color value = (Color value : svg_paint)
let svg_paint_url ?fallback value = (Url (value, fallback) : svg_paint)

let text_shadow_value ?blur ?color h_offset v_offset : text_shadow =
  Text_shadow { h_offset; v_offset; blur; color }

(* Declaration accessor functions *)
let declaration_is_important = Declaration.is_important
let declaration_name = Declaration.property_name

let declaration_value ?(minify = false) ?(inline = false) decl =
  Declaration.string_of_value ~minify ~inline decl

let declaration_value_for_equivalence decl =
  Declaration.string_of_value ~minify:false
    (Declaration.unquote_custom_font_strings decl)

(* Override rule function to return statement directly *)
let rule ~selector ?nested ?merge_key declarations =
  Rule (Stylesheet.rule ~selector ?nested ?merge_key declarations)

let keyframe ~selector ~declarations =
  { Stylesheet.selector = Keyframe.selector_of_string selector; declarations }

(* Re-export keyframes from Stylesheet *)
let keyframes = Stylesheet.keyframes

(* Query functions for statements *)
let statement_selector = function
  | Rule r -> Some (Stylesheet.selector r)
  | _ -> None

let statement_declarations = function
  | Rule r -> Some (Stylesheet.declarations r)
  | Declarations decls -> Some decls
  | _ -> None

let as_rule = function
  | Rule r ->
      Some
        (Stylesheet.selector r, Stylesheet.declarations r, Stylesheet.nested r)
  | _ -> None

let media_min_width_length l =
  Media.Cond
    (Media.Feature (Media.Plain (Media.Min Media.Width, Media.Length l)))

let media_not_min_width_length l =
  Media.Type
    {
      prefix = Some Media.Not;
      type_ = Media.All;
      trailing =
        Some
          (Media.Feature (Media.Plain (Media.Min Media.Width, Media.Length l)));
    }

let parse_length s =
  match
    let c = Cursor.of_string s in
    let l = Values.read_length c in
    if Cursor.is_done c then Some l else None
  with
  | value -> value
  | exception (Cursor.Parse_error _ | Invalid_argument _) -> None

let parse_color s =
  match
    let c = Cursor.of_string s in
    let col = Values.read_color c in
    if Cursor.is_done c then Some col else None
  with
  | value -> value
  | exception (Cursor.Parse_error _ | Invalid_argument _) -> None

let parse_shadow s =
  match
    let r = Cursor.of_string s in
    let sh = Properties.read_shadow r in
    if Cursor.is_done r then Some sh else None
  with
  | value -> value
  | exception (Cursor.Parse_error _ | Invalid_argument _) -> None

let parse_background_image s =
  match
    let r = Cursor.of_string s in
    let imgs = Properties.read_background_images r in
    if Cursor.is_done r then Some imgs else None
  with
  | value -> value
  | exception (Cursor.Parse_error _ | Invalid_argument _) -> None

let parse_font_family s =
  match
    let r = Cursor.of_string s in
    let v = Properties.read_font_family r in
    if Cursor.is_done r then Some v else None
  with
  | value -> value
  | exception (Cursor.Parse_error _ | Invalid_argument _) -> None

let parse_list_style_type s =
  match
    let r = Cursor.of_string s in
    let v = Properties.read_list_style_type r in
    if Cursor.is_done r then Some v else None
  with
  | value -> value
  | exception (Cursor.Parse_error _ | Invalid_argument _) -> None

let parse_list_style_image s =
  match
    let r = Cursor.of_string s in
    let v = Properties.read_list_style_image r in
    if Cursor.is_done r then Some v else None
  with
  | value -> value
  | exception (Cursor.Parse_error _ | Invalid_argument _) -> None

let as_layer = function
  | Layer (name, content) -> Some (name, content)
  | _ -> None

let as_media = function
  | Media (condition, content) -> Some (condition, content)
  | _ -> None

let as_container = function
  | Container (name, condition, content) -> Some (name, condition, content)
  | _ -> None

let as_supports = function
  | Supports (condition, content) -> Some (condition, content)
  | _ -> None

let is_nested_media = function
  | Media (_, [ Declarations _ ]) -> true
  | _ -> false

let is_nested_supports = function
  | Supports (_, [ Declarations _ ]) -> true
  | _ -> false

let as_declarations = function Declarations decls -> Some decls | _ -> None

let as_origin = function
  | Origin (origin, content) -> Some (origin, content)
  | _ -> None

let map_container_block f = function
  | Media (condition, content) -> Some (media ~condition (f content))
  | Supports (condition, content) -> Some (supports ~condition (f content))
  | Layer (name, content) -> Some (layer ?name (f content))
  | Container (name, condition, content) ->
      Some (container ?name ?condition (f content))
  | Origin (origin, content) -> Some (Origin (origin, f content))
  | _ -> None

let statement_children = Stylesheet.statement_children

let rec map f stmts =
  List.map
    (fun stmt ->
      match as_rule stmt with
      | Some (sel, decls, nested) -> (
          match f sel decls with
          | Rule mapped when Stylesheet.nested mapped = [] ->
              (* Callback didn't supply nested; preserve the original tree (with
                 [f] applied recursively) so map doesn't silently drop nested
                 rules / at-rules. *)
              Rule { mapped with nested = map f nested }
          | other ->
              (* Callback supplied its own nested (or returned a non-Rule);
                 trust it and replace the original. *)
              other)
      | None -> (
          match map_container_block (map f) stmt with
          | Some stmt -> stmt
          | None -> stmt))
    stmts

let rec sort cmp stmts =
  (* First, recursively sort within containers and inside rule.nested. *)
  let stmts_with_sorted_contents =
    List.map
      (fun stmt ->
        match stmt with
        | Rule rule -> Rule { rule with nested = sort cmp rule.nested }
        | _ -> (
            match map_container_block (sort cmp) stmt with
            | Some stmt -> stmt
            | None -> stmt))
      stmts
  in

  (* Now sort the rules at this level *)
  List.sort
    (fun stmt1 stmt2 ->
      match (as_rule stmt1, as_rule stmt2) with
      | Some (sel1, decls1, _), Some (sel2, decls2, _) ->
          cmp (sel1, decls1) (sel2, decls2)
      | Some _, None -> -1 (* Rules before non-rules *)
      | None, Some _ -> 1 (* Non-rules after rules *)
      | None, None -> 0 (* Preserve order of non-rules *))
    stmts_with_sorted_contents

(* Existential type for property information *)
type property_info =
  | Property_info : {
      name : string;
      syntax : 'a Variables.syntax;
      inherits : bool;
      initial_value : 'a option;
    }
      -> property_info

let as_property = function
  | Property { name; syntax; inherits; initial_value } ->
      Some (Property_info { name; syntax; inherits; initial_value })
  | _ -> None

let as_keyframes = function
  | Keyframes (name, frames) -> Some (name, frames)
  | _ -> None

let as_font_face = function
  | Font_face descriptors -> Some descriptors
  | _ -> None

let as_import = function Import import_rule -> Some import_rule | _ -> None
let concat = List.concat
let empty = []
let v = Stylesheet.v
let theme_guarded = Declaration.theme_guarded

let as_theme_guarded = function
  | Theme_guarded { var_name; decl; _ } -> Some (var_name, decl)
  | _ -> None

(* Override to return statements instead of rules *)
let rule_statements t =
  let raw_rules = Stylesheet.rules t in
  List.map (fun r -> Rule r) raw_rules

(* Function to extract all statements, not just rules *)
let statements t = t

(* Fold over all statements recursively, descending into nested structures *)
let rec fold f acc t =
  List.fold_left
    (fun acc stmt ->
      let acc' = f acc stmt in
      (* Recursively fold over nested statements *)
      fold f acc' (statement_children stmt))
    acc t

let media_queries t =
  let raw_media = Stylesheet.media_queries t in
  List.map
    (fun (condition, rules) -> (condition, List.map (fun r -> Rule r) rules))
    raw_media

(* AST Introspection Helpers *)

(* CSS Cascade 6 sec. 6.4.3: a dotted layer name [foo.bar] is shorthand for the
   nested [@layer foo { @layer bar { ... } }]. Walk the @layer tree once,
   expanding dotted names and prefixing each block with its parent's path, so
   [foo.bar] is reachable under one canonical name whatever the input shape. *)
let qualified_layer_blocks sheet =
  let prefix_with parent name =
    if parent = "" then name else parent ^ "." ^ name
  in
  let rec emit_dotted parent rest inner acc =
    match rest with
    | [] -> acc
    | [ leaf ] ->
        let qualified = prefix_with parent leaf in
        walk qualified ((qualified, inner) :: acc) inner
    | head :: tail ->
        let qualified = prefix_with parent head in
        emit_dotted qualified tail inner ((qualified, []) :: acc)
  and walk parent acc statements =
    List.fold_left
      (fun acc s ->
        match as_layer s with
        | Some (Some name, inner) ->
            let segments = String.split_on_char '.' name in
            emit_dotted parent segments inner acc
        | _ -> acc)
      acc statements
  in
  List.rev (walk "" [] sheet)

let layer_block name sheet = List.assoc_opt name (qualified_layer_blocks sheet)

let layers t =
  (* Derive the layer set from the canonical [@layer] block walk plus any
     explicit [@layer a, b, c;] forward declarations. Going through
     [qualified_layer_blocks] alone keeps dotted and nested input forms
     producing the same result. *)
  let from_blocks = List.map fst (qualified_layer_blocks t) in
  let from_decls =
    List.concat_map
      (fun s -> match s with Stylesheet.Layer_decl names -> names | _ -> [])
      t
  in
  let seen = Hashtbl.create 16 in
  let dedup name : string option =
    if Hashtbl.mem seen name then None
    else (
      Hashtbl.add seen name ();
      Some name)
  in
  List.filter_map dedup (from_blocks @ from_decls)

let rules_of_statements stmts =
  List.filter_map
    (fun stmt ->
      match as_rule stmt with
      | Some (sel, decls, _) -> Some (sel, decls)
      | None -> None)
    stmts

let custom_prop_names decls = List.filter_map custom_declaration_name decls

let custom_props_of_rules rules =
  List.concat_map (fun (_, decls) -> custom_prop_names decls) rules

let custom_props_nested_block stmt =
  (* Block-container statements whose children continue the [in_layer] context
     unchanged: walk into [@media], [@supports], [@container], [@origin] blocks;
     [@layer] is handled by the caller because it adjusts [in_layer]. *)
  let extractors =
    [
      (fun s -> Option.map snd (as_media s));
      (fun s -> Option.map snd (as_supports s));
      (fun s -> Option.map (fun (_, _, c) -> c) (as_container s));
      (fun s -> Option.map snd (as_origin s));
    ]
  in
  List.find_map (fun f -> f stmt) extractors

let custom_props ?layer sheet =
  (* Walk the statement tree directly so [in_layer] follows the structure: it is
     set on entry to a [Layer] node and reset on exit, never persists into
     sibling statements. *)
  let rec walk in_layer acc stmt =
    let acc =
      match as_rule stmt with
      | Some (_, decls, nested) when in_layer ->
          let acc = custom_prop_names decls @ acc in
          List.fold_left (walk in_layer) acc nested
      | Some (_, _, nested) -> List.fold_left (walk in_layer) acc nested
      | None -> acc
    in
    let descend block in_layer' = List.fold_left (walk in_layer') acc block in
    match as_layer stmt with
    | Some (Some name, content) ->
        let in_layer' =
          match layer with
          | None -> true
          | Some target -> in_layer || name = target
        in
        descend content in_layer'
    | Some (None, content) -> descend content in_layer
    | None -> (
        match custom_props_nested_block stmt with
        | Some content -> descend content in_layer
        | None -> acc)
  in
  let initial = layer = None in
  List.rev (List.fold_left (walk initial) [] sheet)

let media ~condition statements = Media (condition, statements)

let media_nested ~condition declarations =
  Stylesheet.media_nested ~condition declarations

let declarations decls = Declarations decls
let layer ?name statements = Layer (name, statements)
let layer_decl names = Layer_decl names
let with_origin = Stylesheet.with_origin
let origin_importance_rank = Stylesheet.origin_importance_rank

let layer_of ?name stylesheet =
  (* Wrap the stylesheet statements in a layer *)
  [ Layer (name, stylesheet) ]

let container ?name ?condition statements =
  Container (name, condition, statements)

let supports ~condition statements = Supports (condition, statements)

let property ~name syntax ?initial_value ?(inherits = false) () =
  [ property ~syntax ?initial_value ~inherits name ]

(* Top-level convenience helpers for non-calc values *)

let vars_of_declarations = Variables.vars_of_declarations

let vars_of_rules statements =
  let decls =
    List.concat_map
      (fun stmt -> match stmt with Rule r -> r.declarations | _ -> [])
      statements
  in
  vars_of_declarations decls

type parse = { stylesheet : t; warnings : Error.t list }

let of_string ?(strict = false) ?(filename = "<string>")
    ?(meta = Loc.default_meta_level) ?(enforce_spec = false) css =
  let stamp e = Error.with_filename ~filename e in
  try
    let stylesheet, warnings =
      Stylesheet.parse_stylesheet_partial ~meta ~enforce_spec css
    in
    let warnings = List.map stamp warnings in
    if strict then
      match warnings with
      | [] -> Ok { stylesheet; warnings }
      | first :: _ -> Error first
    else Ok { stylesheet; warnings }
  with Error.Parse_error error -> Error (stamp error)

let of_string_exn ?strict ?filename ?meta ?enforce_spec css =
  match of_string ?strict ?filename ?meta ?enforce_spec css with
  | Ok { stylesheet; _ } -> stylesheet
  | Error error -> Error.fail error

let rec statements_for_inline statement =
  let inline_block block = List.concat_map statements_for_inline block in
  match statement with
  | Layer (_, block) -> List.concat_map statements_for_inline block
  | Layer_decl _ | Property _ -> []
  | Media (condition, block) -> [ Media (condition, inline_block block) ]
  | Supports (condition, block) -> [ Supports (condition, inline_block block) ]
  | Moz_document (conditions, block) ->
      [ Moz_document (conditions, inline_block block) ]
  | Container (name, condition, block) ->
      [ Container (name, condition, inline_block block) ]
  | Scope (start, end_, block) -> [ Scope (start, end_, inline_block block) ]
  | Origin (origin, block) -> [ Origin (origin, inline_block block) ]
  | When (condition, block) -> [ When (condition, inline_block block) ]
  | Else (condition, block) -> [ Else (condition, inline_block block) ]
  | Starting_style block -> [ Starting_style (inline_block block) ]
  | statement -> [ statement ]

(* Pure serialiser: walk the AST and emit CSS, no optimise/theme/inline-vars
   rewriting. Spec recovery (drop invalid declarations, unknown at-rules, empty
   rules) still applies - browsers discard those at parse, so it keeps the
   output observationally equal to a fresh parse, not an optimisation. Compose
   {!optimize}, {!resolve_theme}, {!inline_vars} upstream when needed. *)
let to_string ?(minify = false) ?indent ?lossless ?enforce_spec stylesheet =
  let stylesheet =
    stylesheet |> Optimize.drop_invalid |> Optimize.drop_unknown_at_rules
    |> Optimize.drop_empty_rules
  in
  Stylesheet.to_string ~minify ?indent ?lossless ?enforce_spec stylesheet

let pp = to_string

(* Append the serialised stylesheet to [buf]. *)
let to_buffer buf ?(minify = false) ?indent ?lossless ?enforce_spec stylesheet =
  let stylesheet =
    stylesheet |> Optimize.drop_invalid |> Optimize.drop_unknown_at_rules
    |> Optimize.drop_empty_rules
  in
  let pp ctx () = Stylesheet.pp_stylesheet ctx stylesheet in
  Pp.to_buffer ~minify ?indent ?lossless ?enforce_spec buf pp ()

let inline_style_of_declarations ?(optimize = false) ?minify ?mode declarations
    =
  let declarations =
    if optimize then Optimize.deduplicate_declarations declarations
    else declarations
  in
  inline_style_of_declarations ?minify ?mode declarations

let optimize ?scope ?flatten_nesting ?lossless ?enforce_spec ?aggressive
    ?regroup ?closed_world ?objective ?prune_unused_custom_props ?stats
    stylesheet =
  Optimize.stylesheet ?scope ?flatten_nesting ?lossless ?enforce_spec
    ?aggressive ?regroup ?closed_world ?objective ?prune_unused_custom_props
    ?stats stylesheet

let flatten_nesting = Optimize.flatten_nesting
let canonicalize_rule_order = Rule_order.canonicalize

(** {1 Closed-world inlining} *)

(* Explicit AST step matching what [to_string ~mode:Inline] does internally:
   substitute every resolvable [var()] reference, then strip the now-empty
   [@layer] wrappers and the [@property] / [@layer-decl] rules that only existed
   to register the substituted variables. *)
let inline_vars ?keep_vars ?warn stylesheet =
  let substituted =
    match keep_vars with
    | None -> Inline.vars ?warn stylesheet
    | Some keep_vars -> Inline.vars ?warn ~keep_vars stylesheet
  in
  List.concat_map statements_for_inline substituted

(* Collect every [var(--name)] reference's name (with leading [--]) from a
   stylesheet. Used by [resolve_theme] to know which names to ask the
   [theme_defaults] resolver for. *)
let collect_var_names stylesheet =
  let seen = Hashtbl.create 8 in
  let record_decls decls =
    List.iter
      (fun (Variables.V vv as v) ->
        Hashtbl.replace seen (Variables.any_var_name v) ();
        (* A theme default reachable only through a [var()] fallback
           ([var(--tw-duration, var(--default-transition-duration))]) is still a
           resolution root, so record the nested fallback name too. *)
        match vv.Values.fallback with
        | Values.Var_fallback fname ->
            Hashtbl.replace seen (String.concat "" [ "--"; fname ]) ()
        | _ -> ())
      (Variables.vars_of_declarations decls)
  in
  let rec walk = function
    | [] -> ()
    | stmt :: rest ->
        (match stmt with
        | Stylesheet.Rule r ->
            record_decls r.declarations;
            walk r.nested
        | Declarations decls -> record_decls decls
        | Media (_, b)
        | Supports (_, b)
        | Container (_, _, b)
        | Layer (_, b)
        | Origin (_, b)
        | Scope (_, _, b)
        | Starting_style b
        | Moz_document (_, b)
        | When (_, b)
        | Else (_, b) ->
            walk b
        | _ -> ());
        walk rest
  in
  walk stylesheet;
  Hashtbl.fold (fun k () acc -> k :: acc) seen []

(* Resolve [Theme_guarded { var_name; decl }] declarations against the theme
   keep-set: keep the wrapped declaration when [var_name] is in the theme, drop
   it otherwise. When no [theme] is supplied the guards pass through unchanged
   (the default "everything is in theme" behaviour mirrors [Pp.in_theme]'s
   no-theme branch). *)
let resolve_theme_guards_in_decls ~(theme : Pp.String_set.t option) decls =
  match theme with
  | Option.None -> decls
  | Option.Some set ->
      List.filter_map
        (function
          | Declaration.Theme_guarded { var_name; decl; _ } ->
              if Pp.String_set.mem var_name set then Option.Some decl
              else Option.None
          | d -> Option.Some d)
        decls

let rec resolve_theme_guards_in_stmts ~theme = function
  | [] -> []
  | stmt :: rest ->
      let stmt =
        match stmt with
        | Stylesheet.Rule r ->
            Stylesheet.Rule
              {
                r with
                declarations =
                  resolve_theme_guards_in_decls ~theme r.declarations;
                nested = resolve_theme_guards_in_stmts ~theme r.nested;
              }
        | Declarations decls ->
            Declarations (resolve_theme_guards_in_decls ~theme decls)
        | Media (c, b) -> Media (c, resolve_theme_guards_in_stmts ~theme b)
        | Supports (c, b) -> Supports (c, resolve_theme_guards_in_stmts ~theme b)
        | Container (n, c, b) ->
            Container (n, c, resolve_theme_guards_in_stmts ~theme b)
        | Layer (n, b) -> Layer (n, resolve_theme_guards_in_stmts ~theme b)
        | Origin (o, b) -> Origin (o, resolve_theme_guards_in_stmts ~theme b)
        | Scope (s, e, b) -> Scope (s, e, resolve_theme_guards_in_stmts ~theme b)
        | Starting_style b ->
            Starting_style (resolve_theme_guards_in_stmts ~theme b)
        | Moz_document (c, b) ->
            Moz_document (c, resolve_theme_guards_in_stmts ~theme b)
        | When (c, b) -> When (c, resolve_theme_guards_in_stmts ~theme b)
        | Else (c, b) -> Else (c, resolve_theme_guards_in_stmts ~theme b)
        | other -> other
      in
      stmt :: resolve_theme_guards_in_stmts ~theme rest

let bare_theme_name raw_name =
  if String.length raw_name >= 2 && String.sub raw_name 0 2 = "--" then
    String.sub raw_name 2 (String.length raw_name - 2)
  else raw_name

(* [var()] references nested anywhere inside a value, so resolving one theme
   default pulls its targets into the inject set and [Inline.vars]' recursive
   substitution chains them. A custom-property value is an opaque token stream,
   so the typed AST var collector does not see inside it; recognise references
   structurally (see {!Variables.var_refs_in_value_string}). *)
let var_names_in_theme_value = Variables.var_refs_in_value_string

(* Every [var()] reference found by structurally scanning each declaration's
   serialized value, typed declarations included. [collect_var_names] misses a
   var nested inside a *typed* fallback (var(--tw-ease, var(--default-...))), so
   seeding theme resolution from this too lets such a nested theme var resolve
   transitively instead of surviving. *)
let structural_var_refs (stmts : Stylesheet.statement list) : string list =
  let acc = ref [] in
  let note d =
    acc := List.rev_append (var_names_in_theme_value (declaration_value d)) !acc
  in
  let rec scan (stmt : Stylesheet.statement) =
    match stmt with
    | Stylesheet.Rule r ->
        List.iter note r.declarations;
        List.iter scan r.nested
    | Stylesheet.Declarations decls -> List.iter note decls
    | Stylesheet.Media (_, b)
    | Stylesheet.Supports (_, b)
    | Stylesheet.Container (_, _, b)
    | Stylesheet.Layer (_, b)
    | Stylesheet.Origin (_, b)
    | Stylesheet.Scope (_, _, b)
    | Stylesheet.Starting_style b
    | Stylesheet.Moz_document (_, b)
    | Stylesheet.When (_, b)
    | Stylesheet.Else (_, b) ->
        List.iter scan b
    | _ -> ()
  in
  List.iter scan stmts;
  !acc

let collect_theme_defaults ~theme ~theme_defaults ~keep_set stylesheet =
  let resolved : (string, string) Hashtbl.t = Hashtbl.create 16 in
  let cyclic = ref [] in
  let visiting : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  let mark_cyclic raw_name =
    if not (List.mem raw_name !cyclic) then cyclic := raw_name :: !cyclic
  in
  let rec dfs raw_name =
    if Hashtbl.mem resolved raw_name then ()
    else if Hashtbl.mem visiting raw_name then
      (* Back-edge: every name on the current path is on the cycle, so none of
         them resolves to a concrete value - keep them all live. *)
      Hashtbl.iter (fun k () -> mark_cyclic k) visiting
    else if Pp.String_set.mem (bare_theme_name raw_name) keep_set then ()
    else
      match theme_defaults with
      | Option.None -> ()
      | Option.Some lookup -> (
          match lookup (bare_theme_name raw_name) with
          | Option.None -> ()
          | Option.Some value ->
              Hashtbl.replace visiting raw_name ();
              List.iter dfs (var_names_in_theme_value value);
              Hashtbl.remove visiting raw_name;
              if not (List.mem raw_name !cyclic) then
                Hashtbl.replace resolved raw_name value)
  in
  (match theme with
  | Option.Some _ ->
      List.iter dfs
        (collect_var_names stylesheet @ structural_var_refs stylesheet)
  | Option.None -> ());
  Hashtbl.fold (fun k v acc -> (bare_theme_name k, v) :: acc) resolved []

let theme_defaults_source defaults =
  let body =
    defaults
    |> List.map (fun (name, value) ->
        let n =
          if String.length name >= 2 && String.sub name 0 2 = "--" then name
          else "--" ^ name
        in
        n ^ ":" ^ value)
    |> String.concat ";"
  in
  ":root{" ^ body ^ "}"

(* Names declared as a custom property anywhere in the tree (bare, no [--]). *)
let declared_custom_prop_names (stmts : Stylesheet.statement list) :
    (string, unit) Hashtbl.t =
  let tbl = Hashtbl.create 16 in
  let note decls =
    List.iter
      (fun d ->
        match Variables.custom_declaration_name d with
        | Some n -> Hashtbl.replace tbl (bare_theme_name n) ()
        | None -> ())
      decls
  in
  let rec scan (stmt : Stylesheet.statement) =
    match stmt with
    | Stylesheet.Rule r ->
        note r.declarations;
        List.iter scan r.nested
    | Stylesheet.Declarations decls -> note decls
    | Stylesheet.Media (_, b)
    | Stylesheet.Supports (_, b)
    | Stylesheet.Container (_, _, b)
    | Stylesheet.Layer (_, b)
    | Stylesheet.Origin (_, b)
    | Stylesheet.Scope (_, _, b)
    | Stylesheet.Starting_style b
    | Stylesheet.Moz_document (_, b)
    | Stylesheet.When (_, b)
    | Stylesheet.Else (_, b) ->
        List.iter scan b
    | _ -> ()
  in
  List.iter scan stmts;
  tbl

(* Bare names of every [var()] reference. [collect_var_names] reads typed
   values; the byte scan covers only opaque custom-property streams, so a [var(]
   inside a string ([content: "var(--x)"]) is not counted as a reference. *)
let referenced_var_names (stmts : Stylesheet.statement list) : string list =
  let opaque = ref [] in
  let note d =
    match Variables.custom_declaration_name d with
    | Option.Some _ ->
        opaque :=
          List.rev_append
            (var_names_in_theme_value (declaration_value d))
            !opaque
    | Option.None -> ()
  in
  let rec scan (stmt : Stylesheet.statement) =
    match stmt with
    | Stylesheet.Rule r ->
        List.iter note r.declarations;
        List.iter scan r.nested
    | Stylesheet.Declarations decls -> List.iter note decls
    | Stylesheet.Media (_, b)
    | Stylesheet.Supports (_, b)
    | Stylesheet.Container (_, _, b)
    | Stylesheet.Layer (_, b)
    | Stylesheet.Origin (_, b)
    | Stylesheet.Scope (_, _, b)
    | Stylesheet.Starting_style b
    | Stylesheet.Moz_document (_, b)
    | Stylesheet.When (_, b)
    | Stylesheet.Else (_, b) ->
        List.iter scan b
    | _ -> ()
  in
  List.iter scan stmts;
  List.map bare_theme_name (collect_var_names stmts @ !opaque)

(* [:root], [:host], or a comma list of only those. *)
let is_root_scope_selector sel =
  let parts = String.split_on_char ',' (Selector.to_string ~minify:true sel) in
  parts <> []
  && List.for_all
       (fun p ->
         match String.trim p with ":root" | ":host" -> true | _ -> false)
       parts

(* Prepend [decls] to the first root-scope rule reachable without crossing a
   conditional at-rule (descend through [@layer] only, not [@supports] /
   [@media] whose [:root] is conditional). Returns the tree and whether a merge
   happened. *)
let merge_into_root_scope decls (stmts : Stylesheet.statement list) :
    Stylesheet.statement list * bool =
  let merged = ref false in
  let rec go = function
    | [] -> []
    | stmt :: rest when !merged -> stmt :: go rest
    | stmt :: rest ->
        let stmt' =
          match stmt with
          | Stylesheet.Rule r when is_root_scope_selector r.selector ->
              merged := true;
              Stylesheet.Rule { r with declarations = decls @ r.declarations }
          | Stylesheet.Layer (n, b) -> Stylesheet.Layer (n, go b)
          | other -> other
        in
        stmt' :: go rest
  in
  let result = go stmts in
  (result, !merged)

(* From [roots], the [(name, value)] bindings for each free variable resolvable
   through [lookup]. [emittable] keeps only names that close: unbound in
   [declared], and every [var()] in the value declared or itself emittable, no
   cycle. A cyclic ([--a: var(--b)], [--b: var(--a)]) or dead-end chain is
   dropped: its [:root] binding would be guaranteed-invalid. *)
let resolve_theme_defaults ~declared ~lookup roots =
  let rec emittable path name =
    let bare = bare_theme_name name in
    if Hashtbl.mem declared bare then true
    else if List.mem bare path then false
    else
      match lookup bare with
      | Option.None -> false
      | Option.Some value ->
          List.for_all
            (emittable (bare :: path))
            (var_names_in_theme_value value)
  in
  let rec resolve acc name =
    let bare = bare_theme_name name in
    if List.mem_assoc bare acc || Hashtbl.mem declared bare then acc
    else if not (emittable [] bare) then acc
    else
      match lookup bare with
      | Option.None -> acc
      | Option.Some value ->
          List.fold_left resolve ((bare, value) :: acc)
            (var_names_in_theme_value value)
  in
  List.rev (List.fold_left resolve [] roots)

(* Bind every free theme variable at root scope: each [var()] reference
   resolvable through [theme_defaults] (see [resolve_theme_defaults]) is emitted
   into the root-scope theme block - an existing [:root] / [:host] rule, else a
   fresh [:root]. [theme_defaults] returning [None] leaves the variable free.
   See [resolve_theme]'s interface doc for why root scope. *)
let emit_transitive_theme_refs ~theme_defaults stylesheet =
  match theme_defaults with
  | Option.None -> stylesheet
  | Option.Some lookup ->
      let declared = declared_custom_prop_names stylesheet in
      let to_emit =
        resolve_theme_defaults ~declared ~lookup
          (referenced_var_names stylesheet)
      in
      let injected =
        if to_emit = [] then []
        else
          match of_string ~strict:false (theme_defaults_source to_emit) with
          | Ok { stylesheet = root_stmts; _ } -> root_stmts
          | Error _ -> []
      in
      let injected_decls =
        match injected with [ Stylesheet.Rule r ] -> r.declarations | _ -> []
      in
      if injected_decls = [] then stylesheet
      else
        let result, merged = merge_into_root_scope injected_decls stylesheet in
        if merged then result else injected @ stylesheet

(* Inline only the theme defaults the resolver resolved, leaving every other
   [var()] live. [Inline.vars] alone would also collapse [var(--x, fallback)]
   for unresolved names (its fallback arm fires when no declaration is visible).
   A [:root] binding is injected only for resolved names with no declaration of
   their own; an already-declared token inlines from that declaration, and a
   second binding would keep it live. [keep_vars] gains every name except the
   ones being resolved, so unrelated definitions (an @supports polyfill's
   [--tw-x:initial]) stay. *)
let inline_theme_defaults ?theme ?theme_defaults ~keep_set stylesheet =
  let keep_vars = Pp.String_set.elements keep_set in
  let defaults =
    collect_theme_defaults ~theme ~theme_defaults ~keep_set stylesheet
  in
  if defaults = [] then stylesheet
  else
    let declared = declared_custom_prop_names stylesheet in
    let resolved = List.map fst defaults in
    let unresolved_keep =
      referenced_var_names stylesheet
      |> List.filter (fun n -> not (List.mem n resolved))
    in
    let declared_keep =
      Hashtbl.fold (fun n () acc -> n :: acc) declared []
      |> List.filter (fun n -> not (List.mem n resolved))
    in
    let keep_vars =
      List.fold_left
        (fun acc n -> if List.mem n acc then acc else n :: acc)
        keep_vars
        (unresolved_keep @ declared_keep)
    in
    let inject =
      List.filter (fun (n, _) -> not (Hashtbl.mem declared n)) defaults
    in
    if inject = [] then Inline.vars ~keep_vars stylesheet
    else
      match of_string ~strict:false (theme_defaults_source inject) with
      | Ok { stylesheet = root_stmts; _ } ->
          (* Do NOT run the closed-world [statements_for_inline] cleanup: this
             is a partial inline, so it must preserve unrelated [@property]
             registrations and [@layer] structure (the cleanup strips every
             [@property] and flattens every [@layer], dropping author
             registrations like an unrelated [@property --tw-foo]). *)
          Inline.vars ~keep_vars (root_stmts @ stylesheet)
      | Error _ -> stylesheet

(* Theme resolution as an explicit AST step. [theme] names the variables whose
   [var()] references should survive (handed to [Inline.vars]' keep-set) and
   filters [Theme_guarded] declarations to keep only those whose [var_name] is
   in the set. [theme_defaults] supplies external defaults for the other
   variables; [inline_theme_defaults] resolves and inlines them, and
   [emit_transitive_theme_refs] then injects root definitions for references the
   AST collector could not see. *)
let resolve_theme ?theme ?theme_defaults stylesheet =
  let stylesheet = resolve_theme_guards_in_stmts ~theme stylesheet in
  let keep_set =
    match theme with None -> Pp.String_set.empty | Some set -> set
  in
  let stylesheet =
    inline_theme_defaults ?theme ?theme_defaults ~keep_set stylesheet
  in
  (* Emit root-scope definitions for theme vars referenced but undefined,
     including references the AST collector above cannot see inside opaque
     values. *)
  emit_transitive_theme_refs ~theme_defaults stylesheet

let decode_import_url = Inline.decode_import_url
let inline_imports = Inline.imports
