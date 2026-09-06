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
module Aria = Aria
module Color_space = Color_space
module Values = Values
module Context = Context
module Declaration = Declaration
module Properties = Properties
module Selector = Selector
module Selector_summary = Selector_summary
module Stylesheet = Stylesheet
module Nest = Nest

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

module Gradient_direction = struct
  let of_string s =
    parse_full ~property:"gradient-direction" Properties.read_gradient_prelude s
end

let eval_declaration ?layer_order ?layer ctx decl =
  Context.eval ?layer_order ?layer ctx decl

let eval_value ?layer_order ?layer ctx property value =
  eval_declaration ?layer_order ?layer ctx (Declaration.v property value)

let layer_known ~layer_order = function
  | None -> true
  | Some name -> List.exists (String.equal name) layer_order

(* The layer a [Context] keys a declaration by: the CSS text of the [@layer]
   name entered here, or the layer already entered when the block is anonymous.
   Two names never share their text (CSS Cascade 5 sec. 6.4.1), so the key tells
   the layer named [a.b] from the sublayer [b] of [a]. *)
let entered_layer name outer =
  match name with
  | Some name -> Some (Stylesheet.string_of_layer_name name)
  | None -> outer

(* The places a declaration contributes to ordinary element matching. The other
   declaration sites belong to another cascade origin or to no element at all
   (CSS Cascading 5 sec. 6.1): [@keyframes] is the animation origin,
   [@position-try] the position fallback origin, [@page] and its margin boxes
   are not elements, and [@supports-condition] is never applied to a box.
   Written out in full so that a site added to the record has to be classified
   here. *)
let element_matching_sites =
  {
    Stylesheet.element_rule = true;
    animation_frame = false;
    page_box = false;
    position_fallback = false;
    condition_test = false;
  }

(* The layer a declaration sits in is carried down the tree and differs per
   branch, which no accumulator can do, so this spells its own recursion rather
   than calling [fold_declarations]. The descent is still [statement_children]'s
   and the sites are still named, so a grouping at-rule or a declaration site
   added later reaches this walk. *)
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
  let rec statement current_layer acc stmt =
    let open Stylesheet in
    let current_layer =
      match stmt with
      | Layer (name, _) -> entered_layer name current_layer
      | _ -> current_layer
    in
    let acc =
      if at_declaration_site element_matching_sites stmt then
        List.fold_left
          (fun acc decl -> add current_layer decl acc)
          acc
          (statement_declarations stmt)
      else acc
    in
    List.fold_left (statement current_layer) acc (statement_children stmt)
  in
  List.fold_left (statement None) [] stylesheet
  |> List.filter (fun (rule : Context.cascade_rule) ->
      layer_known ~layer_order rule.layer)

let eval_declarations ~layer_order ?layer ctx declarations =
  List.map (eval_declaration ~layer_order ?layer ctx) declarations

let rec eval_block ?ctx_for_layer ~layer_order ?layer ctx block =
  List.map (eval_statement ?ctx_for_layer ~layer_order ?layer ctx) block

(* [@layer] is the one statement named here: it sets the layer the declarations
   below it sit in, and [ctx_for_layer] hands back the context that layer
   resolves in. Every other statement is evaluated the same way, its own
   declarations and then the block it nests, so the shared rebuilders reach an
   at-rule added later without a word here. *)
and eval_statement ?ctx_for_layer ~layer_order ?layer:context_layer ctx
    statement =
  let open Stylesheet in
  match statement with
  | Layer (name, block) ->
      let current_layer = entered_layer name context_layer in
      let ctx =
        match ctx_for_layer with Some f -> f current_layer | None -> ctx
      in
      Layer
        ( name,
          eval_block ?ctx_for_layer ~layer_order ?layer:current_layer ctx block
        )
  | statement ->
      statement
      |> map_statement_declarations
           (eval_declarations ~layer_order ?layer:context_layer ctx)
      |> map_statement_children
           (eval_block ?ctx_for_layer ~layer_order ?layer:context_layer ctx)

let eval_rule ?layer_order ?layer ctx (rule : Stylesheet.rule) =
  let layer_order = Option.value ~default:ctx.Context.layer_order layer_order in
  let layer = match layer with Some _ -> layer | None -> ctx.Context.layer in
  let ctx = { ctx with Context.layer_order; layer } in
  {
    rule with
    declarations = eval_declarations ~layer_order ?layer ctx rule.declarations;
    nested = eval_block ~layer_order ?layer ctx rule.nested;
  }

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
let ratio width height = (Ratio (width, height) : aspect_ratio)
let auto_ratio width height = (Auto_ratio (width, height) : aspect_ratio)
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

let logical_border_style value = (Single value : logical_border_style)

let logical_border_styles start end_ =
  (Pair (start, end_) : logical_border_style)

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

let list_style_image_url value = (Image (Url value) : list_style_image)
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
  Declaration.string_of_value ~minify:true
    (Declaration.canonicalize_custom_whitespace
       (Declaration.unquote_custom_font_strings decl))

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

let parse_option read s =
  match
    let c = Cursor.of_string s in
    let value = read c in
    if Cursor.is_done c then Some value else None
  with
  | value -> value
  | exception (Cursor.Parse_error _ | Invalid_argument _) -> None

let parse_length s = parse_option Values.read_length s
let parse_color s = parse_option Values.read_color s
let parse_shadow s = parse_option Properties.read_shadow s
let parse_background_image s = parse_option Properties.read_background_images s
let parse_font_family s = parse_option Properties.read_font_family s
let parse_list_style_type s = parse_option Properties.read_list_style_type s
let parse_list_style_image s = parse_option Properties.read_list_style_image s

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

(* [f] decides a rule's fate; the descent into what holds it is
   [map_statement_children]'s, so every block at-rule is walked and one added
   later is walked without a word here. *)
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
      | None -> map_statement_children (map f) stmt)
    stmts

let rec sort cmp stmts =
  (* Sort each block first, so the pass reaches a rule at any depth. The descent
     is [map_statement_children]'s: an at-rule that grows a block later is
     sorted inside without a word here, and a nesting block inside a rule is one
     of them. *)
  let stmts_with_sorted_contents =
    List.map (map_statement_children (sort cmp)) stmts
  in
  (* [stable_sort], not [sort]: the comparison answers 0 for two non-rules, so
     stability is what keeps an [@else] with the [@when] it answers. *)
  List.stable_sort
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
let equal_statement = Stylesheet.equal_statement
let hash_statement = Stylesheet.hash_statement
let fold f acc t = Stylesheet.fold_statements f acc t

let media_queries t =
  let raw_media = Stylesheet.media_queries t in
  List.map
    (fun (condition, rules) -> (condition, List.map (fun r -> Rule r) rules))
    raw_media

(* AST Introspection Helpers *)

let layer_block = Stylesheet.layer_block
let layers = Stylesheet.layers

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

let custom_props ?layer sheet =
  (* [in_layer] is set on entry to a named [@layer] and differs per branch,
     never persisting into a sibling, which no accumulator can carry, so this
     spells its own recursion rather than calling [fold_declarations]. The
     descent is still [statement_children]'s and the sites are still
     [element_matching_sites], so this reports a name wherever it is declared
     for an element and a grouping at-rule added later reaches the walk. *)
  let rec walk in_layer acc stmt =
    let in_layer =
      match stmt with
      | Layer (Some name, _) -> (
          match layer with
          | None -> true
          | Some target -> in_layer || Stylesheet.equal_layer_name name target)
      | _ -> in_layer
    in
    let acc =
      if in_layer && at_declaration_site element_matching_sites stmt then
        custom_prop_names (statement_declarations stmt) @ acc
      else acc
    in
    List.fold_left (walk in_layer) acc (statement_children stmt)
  in
  List.rev (List.fold_left (walk (layer = None)) [] sheet)

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

(* Same question as [vars_of_stylesheet], differing only in what it is handed: a
   statement list is a stylesheet. Two walks would answer differently the moment
   one of them met a construct the other knew. *)
let vars_of_rules = vars_of_stylesheet

type parse = {
  stylesheet : t;
  warnings : Error.t list;
  source : Source.t option;
}

let of_string ?(strict = false) ?(filename = "<string>")
    ?(meta = Loc.default_meta_level) ?(enforce_spec = false)
    ?(preserve_source = false) css =
  let stamp e = Error.with_filename ~filename e in
  try
    let source : Source.t option ref = ref Option.None in
    let on_source =
      if preserve_source then
        Option.Some (fun captured -> source := Option.Some captured)
      else Option.None
    in
    let stylesheet, warnings =
      Stylesheet.parse_stylesheet_partial ~meta ~enforce_spec ?on_source css
    in
    let warnings = List.map stamp warnings in
    if strict then
      match warnings with
      | [] -> Ok { stylesheet; warnings; source = !source }
      | first :: _ -> Error first
    else Ok { stylesheet; warnings; source = !source }
  with Error.Parse_error error -> Error (stamp error)

let of_string_exn ?strict ?filename ?meta ?enforce_spec css =
  match of_string ?strict ?filename ?meta ?enforce_spec css with
  | Ok { stylesheet; _ } -> stylesheet
  | Error error -> Error.fail error

(* Splicing a [@layer] body into its parent replaces one statement with several,
   which [edit_statements] cannot express and which does not need it: the splice
   is the [concat_map] over a block, and the descent below it is still
   [map_statement_children]'s, so an at-rule added later is walked without a
   word here.

   [live] holds every custom-property name the substituted stylesheet still
   mentions, with the leading [--]. A [@property] registration is not
   scaffolding for the [var()] it feeds: CSS Properties and Values API 1 sec. 2
   makes its [initial-value] the computed value wherever no declaration wins,
   and its [inherits] descriptor decides whether the property inherits at all.
   Both change computed values, so the registration dies only with the property
   itself - once substitution has left neither a declaration of it nor a [var()]
   reading it.

   [keep_layers] leaves the [@layer] wrappers and declarations standing.
   Dropping them replays the layer stack as document order, which only preserves
   the cascade once every layered competition has been resolved: the layered
   custom properties by the fold {!Inline.vars} runs, and the rest by the stack
   and document order already agreeing, which is what
   {!Inline.flattening_layers_is_safe} answers. *)
let rec statements_for_inline ~live ~keep_layers block =
  List.concat_map (statement_for_inline ~live ~keep_layers) block

and statement_for_inline ~live ~keep_layers statement =
  let inline_block = statements_for_inline ~live ~keep_layers in
  match statement with
  | Layer (name, block) when keep_layers -> [ Layer (name, inline_block block) ]
  | Layer (_, block) -> inline_block block
  | Property rule -> if List.mem rule.name live then [ statement ] else []
  | Layer_decl _ when keep_layers -> [ statement ]
  (* Every [@layer] wrapper is spliced into its parent above, so the layers an
     [@layer] statement orders no longer exist to be ordered. *)
  | Layer_decl _ -> []
  | statement -> [ map_statement_children inline_block statement ]

(* Pure serialiser: walk the AST and emit CSS, no optimise/theme/inline-vars
   rewriting. Spec recovery (drop invalid declarations, empty rules) still
   applies - browsers discard those at parse, so it keeps the output
   observationally equal to a fresh parse, not an optimisation. An at-rule with
   no handler is not in that set: the browser ignoring it is a cascade step, and
   a serialiser has no agent to be. Compose {!optimize}, {!resolve_theme},
   {!inline_vars} upstream when needed. *)
let pp ctx stylesheet =
  let stylesheet =
    stylesheet |> Optimize.drop_invalid |> Optimize.drop_empty_rules
  in
  Stylesheet.pp ctx stylesheet

let to_string ?(minify = false) ?indent ?lossless ?enforce_spec stylesheet =
  Pp.to_string ~minify ?indent ?lossless ?enforce_spec pp stylesheet

(* Append the serialised stylesheet to [buf]. *)
let to_buffer buf ?(minify = false) ?indent ?lossless ?enforce_spec stylesheet =
  Pp.to_buffer ~minify ?indent ?lossless ?enforce_spec buf pp stylesheet

let pp_inline_important ~minify ctx =
  if minify then Pp.string ctx "!important"
  else (
    Pp.space ctx ();
    Pp.string ctx "!important")

let pp_inline_declaration ~minify ~mode ctx declaration =
  let name = Declaration.property_name declaration in
  let value =
    Declaration.string_of_value ~minify ~inline:(mode = Inline) declaration
  in
  Pp.string ctx name;
  Pp.char ctx ':';
  if not minify then Pp.space ctx ();
  Pp.string ctx value;
  if Declaration.is_important declaration then pp_inline_important ~minify ctx

let render_inline_style ?(minify = false) ?(mode : mode = Inline) declarations =
  let buffer = Buffer.create 128 in
  let ctx = Pp.ctx ~minify ~inline:(mode = Inline) buffer in
  List.iteri
    (fun index declaration ->
      if index > 0 then (
        Pp.semicolon ctx ();
        if not minify then Pp.space ctx ());
      pp_inline_declaration ~minify ~mode ctx declaration)
    declarations;
  Buffer.contents buffer

let inline_style_of_declarations ?(optimize = false) ?minify ?mode declarations
    =
  let declarations =
    if optimize then Optimize.deduplicate_declarations declarations
    else declarations
  in
  render_inline_style ?minify ?mode declarations

let optimize ?scope ?targets ?flatten_nesting ?lossless ?enforce_spec
    ?aggressive ?regroup ?closed_world ?objective ?prune_unused_custom_props
    ?stats stylesheet =
  Optimize.stylesheet ?scope ?targets ?flatten_nesting ?lossless ?enforce_spec
    ?aggressive ?regroup ?closed_world ?objective ?prune_unused_custom_props
    ?stats stylesheet

let flatten_nesting = Optimize.flatten_nesting
let canonicalize_rule_order = Rule_order.canonicalize

(** {1 Closed-world inlining} *)

(* Explicit AST step matching what [to_string ~mode:Inline] does internally:
   substitute every resolvable [var()] reference, then strip the now-empty
   [@layer] wrappers, the [@layer-decl] rules ordering them, and the [@property]
   registrations whose property the substitution removed. *)
let inline_vars ?keep_vars ?warn stylesheet =
  let substituted =
    match keep_vars with
    | None -> Inline.vars ?warn stylesheet
    | Some keep_vars -> Inline.vars ?warn ~keep_vars stylesheet
  in
  let live = Inline.mentioned_custom_names substituted in
  let keep_layers = not (Inline.flattening_layers_is_safe substituted) in
  statements_for_inline ~live ~keep_layers substituted

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
  Stylesheet.iter_declarations record_decls stylesheet;
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

(* A guard the keep-set rejects has to be dropped wherever the declaration sits,
   so this goes through the exhaustive declaration walk rather than a local
   match. *)
let resolve_theme_guards_in_stmts ~theme stmts =
  Stylesheet.map_declarations (resolve_theme_guards_in_decls ~theme) stmts

let bare_theme_name raw_name = Custom_property_name.strip_prefix raw_name

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
  Stylesheet.iter_declarations (List.iter note) stmts;
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

(* A theme default is CSS text the caller supplies, so a pair binds only when it
   makes exactly one custom-property declaration. A name or value that would
   escape it - a [}] closing the block, a top-level [;] starting a second
   declaration, an unterminated string - is not a binding this library can
   write, so the name stays unresolved and its [var()] reference stays live. An
   empty answer is a value a custom property may hold, but here it is the caller
   saying the name has no default, so it binds nothing either. *)
let theme_default_declaration (name, value) =
  if String.trim value = "" then Option.None
  else
    let name = Custom_property_name.add_prefix name in
    Declaration.parse_custom_property name value

(* [lookup] restricted to the answers that bind: anything else reads as [None],
   the "no default for this name" answer the rest of the resolver
   understands. *)
let bindable_theme_defaults lookup name =
  match lookup name with
  | Option.Some value
    when Option.is_some (theme_default_declaration (name, value)) ->
      Option.Some value
  | _ -> Option.None

(* The fresh root-scope theme block, used when no [:root] / [:host] rule is
   available to merge into. *)
let root_theme_rule declarations =
  Stylesheet.Rule
    {
      selector = Selector.of_string ":root";
      declarations;
      nested = [];
      merge_key = None;
    }

(* Names a style rule declares as a custom property (bare, no [--]). Only the
   element-matching sites count: a name written in another cascade origin is not
   declared for the element that merely references it. *)
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
  Stylesheet.iter_declarations ~sites:element_matching_sites note stmts;
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
  Stylesheet.iter_declarations (List.iter note) stmts;
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
      let injected_decls = List.filter_map theme_default_declaration to_emit in
      if injected_decls = [] then stylesheet
      else
        let result, merged = merge_into_root_scope injected_decls stylesheet in
        if merged then result else root_theme_rule injected_decls :: stylesheet

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
    match List.filter_map theme_default_declaration inject with
    | [] -> Inline.vars ~keep_vars stylesheet
    | decls ->
        (* Do NOT run the closed-world [statements_for_inline] cleanup: this is
           a partial inline, so it must preserve unrelated [@property]
           registrations and [@layer] structure (the cleanup strips every
           [@property] and flattens every [@layer], dropping author
           registrations like an unrelated [@property --tw-foo]). *)
        Inline.vars ~keep_vars (root_theme_rule decls :: stylesheet)

(* Theme resolution as an explicit AST step. [theme] names the variables whose
   [var()] references should survive (handed to [Inline.vars]' keep-set) and
   filters [Theme_guarded] declarations to keep only those whose [var_name] is
   in the set. [theme_defaults] supplies external defaults for the other
   variables; [inline_theme_defaults] resolves and inlines them, and
   [emit_transitive_theme_refs] then injects root definitions for references the
   AST collector could not see. *)
let resolve_theme ?theme ?theme_defaults stylesheet =
  let theme_defaults = Option.map bindable_theme_defaults theme_defaults in
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
