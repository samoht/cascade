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

let eval_declaration ?layer_order ?layer ctx decl =
  Context.eval ?layer_order ?layer ctx decl

let eval_value ?layer_order ?layer ctx property value =
  eval_declaration ?layer_order ?layer ctx (Declaration.v property value)

let eval_page_margin_rule ~layer_order ?layer ctx rule =
  {
    rule with
    Stylesheet.margin_descriptors =
      List.map
        (eval_declaration ~layer_order ?layer ctx)
        rule.Stylesheet.margin_descriptors;
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
      Stylesheet.keyframe_declarations =
        eval_declarations ~layer_order ?layer ctx frame.keyframe_declarations;
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
  function
  | Media (condition, block) ->
      Some
        (Media
           ( condition,
             eval_block ?ctx_for_layer ~layer_order ?layer:context_layer ctx
               block ))
  | Container (name, condition, block) ->
      Some
        (Container
           ( name,
             condition,
             eval_block ?ctx_for_layer ~layer_order ?layer:context_layer ctx
               block ))
  | Supports (condition, block) ->
      Some
        (Supports
           ( condition,
             eval_block ?ctx_for_layer ~layer_order ?layer:context_layer ctx
               block ))
  | Moz_document (conditions, block) ->
      Some
        (Moz_document
           ( conditions,
             eval_block ?ctx_for_layer ~layer_order ?layer:context_layer ctx
               block ))
  | When (condition, block) ->
      Some
        (When
           ( condition,
             eval_block ?ctx_for_layer ~layer_order ?layer:context_layer ctx
               block ))
  | Else (condition, block) ->
      Some
        (Else
           ( condition,
             eval_block ?ctx_for_layer ~layer_order ?layer:context_layer ctx
               block ))
  | Starting_style block ->
      Some
        (Starting_style
           (eval_block ?ctx_for_layer ~layer_order ?layer:context_layer ctx
              block))
  | Origin (origin, block) ->
      Some
        (Origin
           ( origin,
             eval_block ?ctx_for_layer ~layer_order ?layer:context_layer ctx
               block ))
  | Scope (start, end_, block) ->
      Some
        (Scope
           ( start,
             end_,
             eval_block ?ctx_for_layer ~layer_order ?layer:context_layer ctx
               block ))
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

(* Override rule function to return statement directly *)
let rule ~selector ?nested ?merge_key declarations =
  Rule (Stylesheet.rule ~selector ?nested ?merge_key declarations)

let keyframe ~selector ~declarations =
  {
    Stylesheet.keyframe_selector = Keyframe.selector_of_string selector;
    keyframe_declarations = declarations;
  }

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

let media_min_width_length l = Media.Min_width_length l
let media_not_min_width_length l = Media.Not_min_width_length l

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

let statement_children = function
  | Rule rule -> rule.nested
  | Layer (_, nested)
  | Media (_, nested)
  | Supports (_, nested)
  | Origin (_, nested)
  | Starting_style nested
  | Scope (_, _, nested) ->
      nested
  | Container (_, _, nested) -> nested
  | _ -> []

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
let theme_guarded ~var_name decl = Theme_guarded { var_name; decl }

let as_theme_guarded = function
  | Theme_guarded { var_name; decl } -> Some (var_name, decl)
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

(* Per CSS Cascade 6 section 6.4.3, a dotted layer name like [foo.bar] is
   shorthand for the nested form [@layer foo { @layer bar { ... } }]: both forms
   declare the layers [foo] and [foo.bar] and place the block contents in
   [foo.bar]. We walk the @layer tree once, expanding any dotted names into
   their nested equivalent and prefixing each block with its parent's path, so
   [foo.bar] is reachable under one canonical name regardless of input shape. *)
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
    ?(meta = Loc.default_meta_level) css =
  let stamp e = Error.with_filename ~filename e in
  try
    let stylesheet, warnings = Stylesheet.parse_stylesheet_partial ~meta css in
    let warnings = List.map stamp warnings in
    if strict then
      match warnings with
      | [] -> Ok { stylesheet; warnings }
      | first :: _ -> Error first
    else Ok { stylesheet; warnings }
  with Error.Parse_error error -> Error (stamp error)

let of_string_exn ?strict ?filename ?meta css =
  match of_string ?strict ?filename ?meta css with
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

let to_string ?(minify = false) ?indent ?(mode = Variables) ?theme
    ?(theme_defaults = Pp.no_theme_defaults) stylesheet =
  let stylesheet =
    match mode with
    | Inline -> Inline.vars stylesheet |> List.concat_map statements_for_inline
    | Variables -> stylesheet
  in
  let stylesheet =
    if minify then Optimize.stylesheet stylesheet
    else
      (* Spec recovery applies in both modes: invalid declarations and unknown
         at-rules drop (browsers do too), and empty rules left behind drop. The
         full [Optimize.stylesheet] pass also rewrites and merges, which we only
         want under minify. *)
      stylesheet |> Optimize.drop_invalid |> Optimize.drop_unknown_at_rules
      |> Optimize.drop_empty_rules
  in
  Stylesheet.to_string ~minify ?indent ~mode ?theme ~theme_defaults stylesheet

let pp = to_string

let inline_style_of_declarations ?(optimize = false) ?minify ?mode declarations
    =
  let declarations =
    if optimize then Optimize.deduplicate_declarations declarations
    else declarations
  in
  inline_style_of_declarations ?minify ?mode declarations

(* Keep Css.optimize alias for convenience *)
let optimize = Optimize.stylesheet
let flatten_nesting = Optimize.flatten_nesting

(** {1 Closed-world inlining} *)

let inline_vars ?keep_vars stylesheet =
  match keep_vars with
  | None -> Inline.vars stylesheet
  | Some keep_vars -> Inline.vars ~keep_vars stylesheet

let decode_import_url = Inline.decode_import_url
let inline_imports = Inline.imports
