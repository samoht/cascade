(** CSS generation utilities - Pure re-export module *)

(* Module re-exports *)

module Pp = Pp
module Reader = Reader
module Loc = Loc
module Token = Token
module Lexer = Lexer
module Component = Component
module Parser = Parser
module Cursor = Cursor
module Sort = Sort
module Error = Error
module Values = Values
module Context = Context

module Declaration = struct
  include Declaration

  let eval = Context.eval
end

module Properties = struct
  include Properties

  let eval_value ?layer_order ?layer ctx property value =
    Declaration.eval ?layer_order ?layer ctx (Declaration.v property value)
end

module Selector = Selector

module Stylesheet = struct
  include Stylesheet

  let eval_page_margin_rule ~layer_order ?layer ctx rule =
    {
      rule with
      margin_descriptors =
        List.map
          (Declaration.eval ~layer_order ?layer ctx)
          rule.margin_descriptors;
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
    let rec statement layer acc = function
      | Rule rule ->
          let acc =
            List.fold_left
              (fun acc decl -> add layer decl acc)
              acc rule.declarations
          in
          List.fold_left (statement layer) acc rule.nested
      | Declarations declarations ->
          List.fold_left (fun acc decl -> add layer decl acc) acc declarations
      | Layer (name, block) ->
          let layer = match name with Some _ -> name | None -> layer in
          List.fold_left (statement layer) acc block
      | Media (_, block)
      | Supports (_, block)
      | Moz_document (_, block)
      | When (_, block)
      | Else (_, block)
      | Starting_style block
      | Origin (_, block) ->
          List.fold_left (statement layer) acc block
      | Container (_, _, block) | Scope (_, _, block) ->
          List.fold_left (statement layer) acc block
      | _ -> acc
    in
    List.fold_left (statement None) [] stylesheet
    |> List.filter (fun (rule : Context.cascade_rule) ->
        layer_known ~layer_order rule.layer)

  let rec eval_statement ?ctx_for_layer ~layer_order ?layer ctx = function
    | Rule rule ->
        Rule (eval_rule_with_ctx ?ctx_for_layer ~layer_order ?layer ctx rule)
    | Declarations declarations ->
        Declarations
          (List.map (Declaration.eval ~layer_order ?layer ctx) declarations)
    | Layer (name, block) ->
        let layer = match name with Some _ -> name | None -> layer in
        let ctx = match ctx_for_layer with Some f -> f layer | None -> ctx in
        Layer
          ( name,
            List.map
              (eval_statement ?ctx_for_layer ~layer_order ?layer ctx)
              block )
    | Media (condition, block) ->
        Media
          ( condition,
            List.map
              (eval_statement ?ctx_for_layer ~layer_order ?layer ctx)
              block )
    | Container (name, condition, block) ->
        Container
          ( name,
            condition,
            List.map
              (eval_statement ?ctx_for_layer ~layer_order ?layer ctx)
              block )
    | Supports (condition, block) ->
        Supports
          ( condition,
            List.map
              (eval_statement ?ctx_for_layer ~layer_order ?layer ctx)
              block )
    | Moz_document (conditions, block) ->
        Moz_document
          ( conditions,
            List.map
              (eval_statement ?ctx_for_layer ~layer_order ?layer ctx)
              block )
    | When (condition, block) ->
        When
          ( condition,
            List.map
              (eval_statement ?ctx_for_layer ~layer_order ?layer ctx)
              block )
    | Else (condition, block) ->
        Else
          ( condition,
            List.map
              (eval_statement ?ctx_for_layer ~layer_order ?layer ctx)
              block )
    | Starting_style block ->
        Starting_style
          (List.map
             (eval_statement ?ctx_for_layer ~layer_order ?layer ctx)
             block)
    | Origin (origin, block) ->
        Origin
          ( origin,
            List.map
              (eval_statement ?ctx_for_layer ~layer_order ?layer ctx)
              block )
    | Scope (start, end_, block) ->
        Scope
          ( start,
            end_,
            List.map
              (eval_statement ?ctx_for_layer ~layer_order ?layer ctx)
              block )
    | Keyframes (name, frames) ->
        let eval_frame frame =
          {
            frame with
            keyframe_declarations =
              List.map
                (Declaration.eval ~layer_order ?layer ctx)
                frame.keyframe_declarations;
          }
        in
        Keyframes (name, List.map eval_frame frames)
    | Webkit_keyframes (name, frames) ->
        let eval_frame frame =
          {
            frame with
            keyframe_declarations =
              List.map
                (Declaration.eval ~layer_order ?layer ctx)
                frame.keyframe_declarations;
          }
        in
        Webkit_keyframes (name, List.map eval_frame frames)
    | Moz_keyframes (name, frames) ->
        let eval_frame frame =
          {
            frame with
            keyframe_declarations =
              List.map
                (Declaration.eval ~layer_order ?layer ctx)
                frame.keyframe_declarations;
          }
        in
        Moz_keyframes (name, List.map eval_frame frames)
    | Page (selector, declarations) ->
        Page
          ( selector,
            List.map (Declaration.eval ~layer_order ?layer ctx) declarations )
    | Position_try (name, declarations) ->
        Position_try
          ( name,
            List.map (Declaration.eval ~layer_order ?layer ctx) declarations )
    | Viewport _ as statement -> statement
    | Supports_condition (name, declarations) ->
        Supports_condition
          ( name,
            List.map (Declaration.eval ~layer_order ?layer ctx) declarations )
    | Page_with_margins (selector, descriptors, margins) ->
        Page_with_margins
          ( selector,
            List.map (Declaration.eval ~layer_order ?layer ctx) descriptors,
            List.map (eval_page_margin_rule ~layer_order ?layer ctx) margins )
    | ( Charset _ | Import _ | Namespace _ | Property _ | Layer_decl _
      | Font_face _ | Font_palette_values _ | View_transition _
      | Unknown_at_rule _ ) as statement ->
        statement

  and eval_rule_with_ctx ?ctx_for_layer ~layer_order ?layer ctx rule =
    {
      rule with
      declarations =
        List.map (Declaration.eval ~layer_order ?layer ctx) rule.declarations;
      nested =
        List.map
          (eval_statement ?ctx_for_layer ~layer_order ?layer ctx)
          rule.nested;
    }

  let eval_rule ?layer_order ?layer ctx rule =
    let layer_order =
      Option.value ~default:ctx.Context.layer_order layer_order
    in
    let layer =
      match layer with Some _ -> layer | None -> ctx.Context.layer
    in
    eval_rule_with_ctx ~layer_order ?layer
      { ctx with Context.layer_order; layer }
      rule

  let eval ?layer_order ?layer ctx stylesheet =
    let layer_order =
      Option.value ~default:ctx.Context.layer_order layer_order
    in
    let layer =
      match layer with Some _ -> layer | None -> ctx.Context.layer
    in
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
end

module Variables = Variables
module Optimize = Optimize
module Media = Media
module Container = Container
module Supports = Supports
module Keyframe = Keyframe
module Font_face = Font_face

(* CSS Parsing *)

type parse_error = Error.t * string

let pp_parse_error (err, filename) =
  String.concat "" [ filename; ": "; Error.to_string err ]

(* Include all public APIs except Stylesheet *)

include Values
include Declaration
include Properties
include Variables
include Optimize
include Stylesheet

(* Declaration accessor functions *)
let declaration_is_important = Declaration.is_important
let declaration_name = Declaration.property_name

let declaration_value ?(minify = false) ?(inline = false) decl =
  Declaration.string_of_value ~minify ~inline decl

(* Override rule function to return statement directly *)
let rule ~selector ?nested ?merge_key declarations =
  Rule (Stylesheet.rule ~selector ?nested ?merge_key declarations)

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
  try
    let c = Cursor.of_string s in
    let l = Values.read_length c in
    if Cursor.is_done c then Some l else None
  with Cursor.Parse_error _ | Invalid_argument _ -> None

let parse_color s =
  try
    let c = Cursor.of_string s in
    let col = Values.read_color c in
    if Cursor.is_done c then Some col else None
  with Cursor.Parse_error _ | Invalid_argument _ -> None

let parse_shadow s =
  try
    let r = Cursor.of_string s in
    let sh = Properties.read_shadow r in
    if Cursor.is_done r then Some sh else None
  with Cursor.Parse_error _ | Invalid_argument _ -> None

let parse_background_image s =
  try
    let r = Cursor.of_string s in
    let imgs = Properties.read_background_images r in
    if Cursor.is_done r then Some imgs else None
  with Cursor.Parse_error _ | Invalid_argument _ -> None

let syntax_fallback = Values.syntax_fallback
let custom_value_ident = Variables.custom_value_ident
let custom_value_var_empty_fallback = Variables.custom_value_var_empty_fallback
let string_of_custom_value = Variables.string_of_custom_value
let string_of_number_percentage = Values.string_of_number_percentage
let string_of_kind_value = Properties.string_of_kind_value

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
          match as_media stmt with
          | Some (condition, content) -> media ~condition (map f content)
          | None -> (
              match as_supports stmt with
              | Some (condition, content) -> supports ~condition (map f content)
              | None -> (
                  match as_layer stmt with
                  | Some (name, content) -> layer ?name (map f content)
                  | None -> (
                      match as_container stmt with
                      | Some (name, condition, content) ->
                          container ?name ~condition (map f content)
                      | None -> (
                          match as_origin stmt with
                          | Some (origin, content) ->
                              Origin (origin, map f content)
                          | None -> stmt))))))
    stmts

let rec sort cmp stmts =
  (* First, recursively sort within containers and inside rule.nested. *)
  let stmts_with_sorted_contents =
    List.map
      (fun stmt ->
        match stmt with
        | Rule rule -> Rule { rule with nested = sort cmp rule.nested }
        | _ -> (
            match as_media stmt with
            | Some (condition, content) -> media ~condition (sort cmp content)
            | None -> (
                match as_supports stmt with
                | Some (condition, content) ->
                    supports ~condition (sort cmp content)
                | None -> (
                    match as_layer stmt with
                    | Some (name, content) -> layer ?name (sort cmp content)
                    | None -> (
                        match as_container stmt with
                        | Some (name, condition, content) ->
                            container ?name ~condition (sort cmp content)
                        | None -> (
                            match as_origin stmt with
                            | Some (origin, content) ->
                                Origin (origin, sort cmp content)
                            | None -> stmt))))))
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
      let nested =
        match as_rule stmt with
        | Some (_, _, nested) -> nested
        | None -> (
            match as_layer stmt with
            | Some (_, nested) -> nested
            | None -> (
                match as_media stmt with
                | Some (_, nested) -> nested
                | None -> (
                    match as_container stmt with
                    | Some (_, _, nested) -> nested
                    | None -> (
                        match as_supports stmt with
                        | Some (_, nested) -> nested
                        | None -> (
                            match as_origin stmt with
                            | Some (_, nested) -> nested
                            | None -> (
                                match stmt with
                                | Starting_style nested -> nested
                                | Scope (_, _, nested) -> nested
                                | _ -> []))))))
      in
      fold f acc' nested)
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

let rules_from_statements stmts =
  List.filter_map
    (fun stmt ->
      match as_rule stmt with
      | Some (sel, decls, _) -> Some (sel, decls)
      | None -> None)
    stmts

let custom_prop_names decls = List.filter_map custom_declaration_name decls

let custom_props_from_rules rules =
  List.concat_map (fun (_, decls) -> custom_prop_names decls) rules

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
        match as_media stmt with
        | Some (_, content) -> descend content in_layer
        | None -> (
            match as_supports stmt with
            | Some (_, content) -> descend content in_layer
            | None -> (
                match as_container stmt with
                | Some (_, _, content) -> descend content in_layer
                | None -> (
                    match as_origin stmt with
                    | Some (_, content) -> descend content in_layer
                    | None -> acc))))
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

let container ?name ~condition statements =
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

let of_string ?(filename = "<string>") ?(meta = Loc.default_meta_level) css =
  let reader = Cursor.of_string ~meta css in
  try Ok (read_stylesheet reader)
  with Cursor.Parse_error error -> Error (error, filename)

type parse_warning = Error.t * string
type parse_result = { stylesheet : t; warnings : parse_warning list }

let parse ?(filename = "<string>") ?(meta = Loc.default_meta_level) css =
  let stylesheet, warnings = Stylesheet.parse_stylesheet_partial ~meta css in
  let warnings = List.map (fun e -> (e, filename)) warnings in
  { stylesheet; warnings }

let rec statements_for_inline = function
  | Layer (_, block) -> List.concat_map statements_for_inline block
  | Layer_decl _ | Property _ -> []
  | Media (condition, block) ->
      [ Media (condition, List.concat_map statements_for_inline block) ]
  | Supports (condition, block) ->
      [ Supports (condition, List.concat_map statements_for_inline block) ]
  | Moz_document (conditions, block) ->
      [ Moz_document (conditions, List.concat_map statements_for_inline block) ]
  | Container (name, condition, block) ->
      [
        Container (name, condition, List.concat_map statements_for_inline block);
      ]
  | Scope (start, end_, block) ->
      [ Scope (start, end_, List.concat_map statements_for_inline block) ]
  | Origin (origin, block) ->
      [ Origin (origin, List.concat_map statements_for_inline block) ]
  | When (condition, block) ->
      [ When (condition, List.concat_map statements_for_inline block) ]
  | Else (condition, block) ->
      [ Else (condition, List.concat_map statements_for_inline block) ]
  | Starting_style block ->
      [ Starting_style (List.concat_map statements_for_inline block) ]
  | statement -> [ statement ]

let to_string ?(minify = false) ?(optimize = false) ?(mode = Variables)
    ?(newline = true) ?theme ?(theme_defaults = Pp.no_theme_defaults) stylesheet
    =
  let stylesheet =
    match mode with
    | Inline -> Inline.vars stylesheet |> List.concat_map statements_for_inline
    | Variables -> stylesheet
  in
  let stylesheet =
    if optimize then Optimize.stylesheet stylesheet
    else if minify then Optimize.apply_property_duplication stylesheet
    else stylesheet
  in
  Stylesheet.to_string ~minify ~mode ~newline ?theme ~theme_defaults stylesheet

let pp = to_string

let inline_style_of_declarations ?(optimize = false) ?minify ?mode ?newline
    declarations =
  let declarations =
    if optimize then Optimize.deduplicate_declarations declarations
    else declarations
  in
  inline_style_of_declarations ?minify ?mode ?newline declarations

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
