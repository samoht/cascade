(** CSS stylesheet types and construction functions *)

open Declaration
include Stylesheet_intf

(** {1 Accessors} *)

let stylesheet_rules t = t.rules
let stylesheet_layers t = t.layers
let stylesheet_media_queries t = t.media_queries
let stylesheet_container_queries t = t.container_queries

(** {1 Creation} *)

let rule ~selector declarations : rule = { selector; declarations }
let selector (rule : rule) = rule.selector
let declarations (rule : rule) = rule.declarations

let media ~condition rules : media_rule =
  { media_condition = condition; media_rules = rules }

let media_condition media = media.media_condition
let media_rules media = media.media_rules

let supports ~condition rules =
  { supports_condition = condition; supports_content = Support_rules rules }

let supports_nested ~condition rules nested_queries =
  {
    supports_condition = condition;
    supports_content = Support_nested (rules, nested_queries);
  }

let container ?name ~condition rules : container_rule =
  {
    container_name = name;
    container_condition = condition;
    container_rules = rules;
  }

let property ~syntax ?initial_value ?(inherits = false) name : property_rule =
  let initial_value =
    match initial_value with
    | None -> None
    | Some s -> Universal s (* For now, use Universal for string values *)
  in
  { name; syntax; inherits; initial_value }

let property_rule_name (r : property_rule) = r.name

let property_rule_initial (r : property_rule) =
  match r.initial_value with
  | Universal s -> Some s
  | None -> None
  | V (kind, v) ->
      (* Render the typed initial value consistently with Declaration
         printers *)
      let ctx =
        { Pp.minify = true; indent = 0; buf = Buffer.create 16; inline = false }
      in
      Declaration.pp_value ctx (kind, v);
      Some (Buffer.contents ctx.buf)

let default_decl_of_property_rule (r : property_rule) =
  match r.initial_value with
  | Universal v -> custom_property r.name v
  | V (kind, v) -> custom_declaration r.name kind v
  | None -> custom_property r.name ""

let rule_to_nested rule : nested_rule = Rule rule
let supports_to_nested supports : nested_rule = Supports supports

let layer ~name ?(media = []) ?(container = []) ?(supports = []) rules =
  {
    layer = name;
    rules;
    media_queries = media;
    container_queries = container;
    supports_queries = supports;
  }

let layer_name (layer : layer_rule) = layer.layer
let layer_rules (layer : layer_rule) : nested_rule list = layer.rules

let empty =
  {
    charset = None;
    imports = [];
    namespaces = [];
    layers = [];
    keyframes = [];
    font_faces = [];
    pages = [];
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
        charset = (match acc.charset with None -> sheet.charset | c -> c);
        imports = acc.imports @ sheet.imports;
        namespaces = acc.namespaces @ sheet.namespaces;
        layers = acc.layers @ sheet.layers;
        keyframes = acc.keyframes @ sheet.keyframes;
        font_faces = acc.font_faces @ sheet.font_faces;
        pages = acc.pages @ sheet.pages;
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
      | Charset c -> { acc with charset = Some c }
      | Import i -> { acc with imports = acc.imports @ [ i ] }
      | Namespace n -> { acc with namespaces = acc.namespaces @ [ n ] }
      | Rule r -> { acc with rules = acc.rules @ [ r ] }
      | Media m -> { acc with media_queries = acc.media_queries @ [ m ] }
      | Container c ->
          { acc with container_queries = acc.container_queries @ [ c ] }
      | Starting_style s ->
          { acc with starting_styles = acc.starting_styles @ [ s ] }
      | Supports s ->
          { acc with supports_queries = acc.supports_queries @ [ s ] }
      | Property a -> { acc with at_properties = acc.at_properties @ [ a ] }
      | Layer l -> { acc with layers = acc.layers @ [ l ] }
      | Keyframes k -> { acc with keyframes = acc.keyframes @ [ k ] }
      | Font_face f -> { acc with font_faces = acc.font_faces @ [ f ] }
      | Page p -> { acc with pages = acc.pages @ [ p ] })
    empty items

let stylesheet_items t =
  let charset = match t.charset with None -> [] | Some c -> [ Charset c ] in
  let imports = List.map (fun i -> Import i) t.imports in
  let namespaces = List.map (fun n -> Namespace n) t.namespaces in
  let rules = List.map (fun r -> Rule r) t.rules in
  let media = List.map (fun m -> Media m) t.media_queries in
  let containers = List.map (fun c -> Container c) t.container_queries in
  let supports = List.map (fun s -> Supports s) t.supports_queries in
  let layers = List.map (fun l -> Layer l) t.layers in
  let properties = List.map (fun p -> Property p) t.at_properties in
  let starting = List.map (fun s -> Starting_style s) t.starting_styles in
  let keyframes = List.map (fun k -> Keyframes k) t.keyframes in
  let font_faces = List.map (fun f -> Font_face f) t.font_faces in
  let pages = List.map (fun p -> Page p) t.pages in
  charset @ imports @ namespaces @ keyframes @ font_faces @ pages @ rules
  @ media @ containers @ supports @ layers @ properties @ starting

(** {1 Printers & Readers} *)

(** {2 Rule} *)

let pp_rule : rule Pp.t =
 fun ctx rule ->
  Selector.pp ctx rule.selector;
  Pp.sp ctx ();
  let pp_body ctx () =
    match rule.declarations with
    | [] -> ()
    | decls ->
        Pp.cut ctx ();
        Pp.nest 2
          (Pp.list
             ~sep:(fun ctx () ->
               Pp.semicolon ctx ();
               Pp.cut ctx ())
             Declaration.pp_declaration)
          ctx decls;
        Pp.cut ctx ()
  in
  Pp.surround ~left:Pp.block_open ~right:Pp.block_close pp_body ctx ()

(* Reader for Rule — placed next to its printer for cohesion *)
let read_rule (r : Reader.t) : rule =
  Reader.with_context r "rule" @@ fun () ->
  let selector = Selector.read_selector_list r in
  Reader.ws r;
  Reader.expect '{' r;
  let declarations = Declaration.read_declarations r in
  Reader.ws r;
  Reader.expect '}' r;
  ({ selector; declarations } : rule)

let rec pp_supports_content : supports_content Pp.t =
 fun ctx -> function
  | Support_rules rules -> Pp.list ~sep:Pp.cut pp_rule ctx rules
  | Support_nested (rules, nested_queries) ->
      Pp.list ~sep:Pp.cut pp_rule ctx rules;
      if rules <> [] && nested_queries <> [] then Pp.cut ctx ();
      Pp.list ~sep:Pp.cut pp_supports_rule ctx nested_queries

(** {2 Supports} *)
and pp_supports_rule : supports_rule Pp.t =
 fun ctx sq ->
  Pp.string ctx "@supports ";
  Pp.string ctx sq.supports_condition;
  Pp.sp ctx ();
  Pp.braces pp_supports_content ctx sq.supports_content

(** {2 Media} *)
let pp_media_rule : media_rule Pp.t =
 fun ctx mq ->
  Pp.string ctx "@media ";
  Pp.string ctx mq.media_condition;
  Pp.sp ctx ();
  Pp.braces (Pp.list ~sep:Pp.cut pp_rule) ctx mq.media_rules

(* Reader for Media *)
let read_media_rule (r : Reader.t) : media_rule =
  Reader.with_context r "@media" @@ fun () ->
  Reader.expect_string "@media" r;
  Reader.ws r;
  let condition = Reader.until r '{' |> String.trim in
  if String.length condition = 0 then
    Reader.err_invalid r "media query requires a condition";
  Reader.expect '{' r;
  let rec read_rules acc =
    Reader.ws r;
    if Reader.peek r = Some '}' then (
      Reader.skip r;
      List.rev acc)
    else
      let rule = read_rule r in
      read_rules (rule :: acc)
  in
  let media_rules = read_rules [] in
  { media_condition = condition; media_rules }

(** {2 Container} *)
let pp_container_rule : container_rule Pp.t =
 fun ctx cq ->
  Pp.string ctx "@container ";
  (match cq.container_name with
  | Some name ->
      Pp.string ctx name;
      Pp.sp ctx ()
  | None -> ());
  Pp.string ctx cq.container_condition;
  Pp.sp ctx ();
  Pp.braces (Pp.list ~sep:Pp.cut pp_rule) ctx cq.container_rules

(* Reader for Container *)
let read_container_rule (r : Reader.t) : container_rule =
  Reader.with_context r "@container" @@ fun () ->
  Reader.expect_string "@container" r;
  Reader.ws r;
  (* Split the header up to the opening brace: optional name then condition *)
  let header = Reader.until r '{' |> String.trim in
  let name_opt, condition =
    try
      let i = String.index header ' ' in
      if i > 0 then
        let name = String.sub header 0 i |> String.trim in
        let cond =
          String.sub header (i + 1) (String.length header - i - 1)
          |> String.trim
        in
        ((if name <> "" then Some name else None), cond)
      else (None, header)
    with Not_found -> (None, header)
  in
  Reader.expect '{' r;
  let rec read_rules acc =
    Reader.ws r;
    if Reader.peek r = Some '}' then (
      Reader.skip r;
      List.rev acc)
    else
      let rule = read_rule r in
      read_rules (rule :: acc)
  in
  let rules = read_rules [] in
  {
    container_name = name_opt;
    container_condition = condition;
    container_rules = rules;
  }

(** {2 Property} *)
let pp_property_rule : property_rule Pp.t =
 fun ctx prop ->
  Pp.string ctx "@property ";
  Pp.string ctx prop.name;
  Pp.sp ctx ();
  let pp_body ctx () =
    Pp.cut ctx ();
    Pp.nest 2
      (fun ctx () ->
        Pp.string ctx "syntax:\"";
        Pp.string ctx prop.syntax;
        Pp.string ctx "\"";
        Pp.semicolon ctx ();
        Pp.cut ctx ();
        Pp.string ctx "inherits:";
        Pp.string ctx (if prop.inherits then "true" else "false");
        match prop.initial_value with
        | None -> ()
        | Universal s ->
            Pp.semicolon ctx ();
            Pp.cut ctx ();
            Pp.string ctx "initial-value:";
            Pp.string ctx s
        | V (kind, v) ->
            Pp.semicolon ctx ();
            Pp.cut ctx ();
            Pp.string ctx "initial-value:";
            (* Pretty-print the value based on its kind *)
            Declaration.pp_value ctx (kind, v))
      ctx ();
    Pp.cut ctx ()
  in
  Pp.surround ~left:Pp.block_open ~right:Pp.block_close pp_body ctx ()

let pp_nested_rule : nested_rule Pp.t =
 fun ctx -> function
  | Rule r -> pp_rule ctx r
  | Supports sq -> pp_supports_rule ctx sq

(** {2 Layer} *)
let pp_layer_rule : layer_rule Pp.t =
 fun ctx layer ->
  Pp.string ctx "@layer ";
  Pp.string ctx layer.layer;
  if
    layer.rules = [] && layer.media_queries = []
    && layer.container_queries = []
    && layer.supports_queries = []
  then Pp.semicolon ctx ()
  else (
    Pp.sp ctx ();
    Pp.braces
      (fun ctx () ->
        Pp.list ~sep:Pp.cut pp_nested_rule ctx layer.rules;
        if layer.rules <> [] && layer.media_queries <> [] then Pp.cut ctx ();
        Pp.list ~sep:Pp.cut pp_media_rule ctx layer.media_queries;
        if
          (layer.rules <> [] || layer.media_queries <> [])
          && layer.container_queries <> []
        then Pp.cut ctx ();
        Pp.list ~sep:Pp.cut pp_container_rule ctx layer.container_queries;
        if
          (layer.rules <> [] || layer.media_queries <> []
          || layer.container_queries <> [])
          && layer.supports_queries <> []
        then Pp.cut ctx ();
        Pp.list ~sep:Pp.cut pp_supports_rule ctx layer.supports_queries)
      ctx ())

(* Pretty-printers kept unified for clarity and consistency *)

(** {1 Reading/Parsing} *)

(* (removed duplicate read_media_rule definition) *)

let rec read_supports_rule (r : Reader.t) : supports_rule =
  Reader.with_context r "@supports" @@ fun () ->
  Reader.expect_string "@supports" r;
  Reader.ws r;
  let condition = Reader.until r '{' in
  let condition = String.trim condition in
  Reader.expect '{' r;
  (* Read rules and nested @supports *)
  let rec read_nested_content rules_acc nested_acc =
    Reader.ws r;
    if Reader.peek r = Some '}' then (
      Reader.skip r;
      (List.rev rules_acc, List.rev nested_acc))
    else if Reader.looking_at r "@supports" then
      (* Handle nested @supports *)
      let nested_supports = read_supports_rule r in
      read_nested_content rules_acc (nested_supports :: nested_acc)
    else
      let rule = read_rule r in
      read_nested_content (rule :: rules_acc) nested_acc
  in
  let rules, nested_supports = read_nested_content [] [] in
  let content =
    match nested_supports with
    | [] -> Support_rules rules
    | _ -> Support_nested (rules, nested_supports)
  in
  { supports_condition = condition; supports_content = content }

(* Property descriptors record *)
type property_descriptors = {
  mutable syntax : string option;
  mutable inherits : bool option;
  mutable initial_value : string option;
}

let read_property_rule (r : Reader.t) : property_rule =
  Reader.with_context r "@property" @@ fun () ->
  Reader.expect_string "@property" r;
  Reader.ws r;
  let name = Reader.ident ~keep_case:true r in
  Reader.ws r;
  Reader.expect '{' r;
  Reader.ws r;

  (* Read descriptors in any order *)
  let descriptors = { syntax = None; inherits = None; initial_value = None } in

  let rec read_descriptors () =
    Reader.ws r;
    if Reader.peek r = Some '}' then ()
    else if Reader.looking_at r "syntax:" then (
      Reader.expect_string "syntax:" r;
      Reader.ws r;
      Reader.expect '"' r;
      let value = Reader.until r '"' in
      Reader.expect '"' r;
      descriptors.syntax <- Some value;
      Reader.ws r;
      if Reader.peek r = Some ';' then (
        Reader.skip r;
        Reader.ws r);
      read_descriptors ())
    else if Reader.looking_at r "inherits:" then (
      Reader.expect_string "inherits:" r;
      Reader.ws r;
      let value =
        Reader.enum "inherits" [ ("true", true); ("false", false) ] r
      in
      descriptors.inherits <- Some value;
      Reader.ws r;
      if Reader.peek r = Some ';' then (
        Reader.skip r;
        Reader.ws r);
      read_descriptors ())
    else if Reader.looking_at r "initial-value:" then (
      Reader.expect_string "initial-value:" r;
      Reader.ws r;
      let rec read_value acc =
        match Reader.peek r with
        | Some ';' | Some '}' -> String.concat "" (List.rev acc)
        | Some c ->
            Reader.skip r;
            read_value (String.make 1 c :: acc)
        | None -> String.concat "" (List.rev acc)
      in
      let value = String.trim (read_value []) in
      descriptors.initial_value <- Some value;
      Reader.ws r;
      if Reader.peek r = Some ';' then (
        Reader.skip r;
        Reader.ws r);
      read_descriptors ())
    else Reader.err_invalid r "unexpected property descriptor"
  in

  read_descriptors ();

  (* Validate required descriptors *)
  let syntax =
    match descriptors.syntax with
    | None -> Reader.err_invalid r "@property requires 'syntax' descriptor"
    | Some s -> s
  in

  let inherits =
    match descriptors.inherits with
    | None -> Reader.err_invalid r "@property requires 'inherits' descriptor"
    | Some b -> b
  in

  (* Parse initial value based on syntax *)
  let parse_initial_value value syntax : property_initial_value =
    match syntax with
    | "*" -> Universal value
    | "<color>" -> (
        try
          let r = Reader.of_string value in
          V (Declaration.Color, Values.read_color r)
        with Reader.Parse_error _ | End_of_file -> Universal value)
    | "<length>" -> (
        try
          let r = Reader.of_string value in
          V (Declaration.Length, Values.read_length r)
        with Reader.Parse_error _ | End_of_file -> Universal value)
    | _ -> Universal value (* Fallback for unsupported syntax *)
  in

  let initial_value =
    match descriptors.initial_value with
    | None -> None
    | Some v -> parse_initial_value v syntax
  in

  Reader.expect '}' r;
  { name; syntax; inherits; initial_value }

let read_layer_rule (r : Reader.t) : layer_rule =
  Reader.with_context r "@layer" @@ fun () ->
  Reader.expect_string "@layer" r;
  Reader.ws r;

  (* Check if this is a layer statement (ends with semicolon) or layer rule (has braces) *)
  (* First, try to read layer names *)
  let rec read_layer_names acc =
    let name = Reader.ident ~keep_case:true r in
    Reader.ws r;
    if Reader.peek r = Some ',' then (
      Reader.skip r;
      Reader.ws r;
      read_layer_names (name :: acc))
    else name :: acc
  in

  let first_name = Reader.ident ~keep_case:true r in
  Reader.ws r;

  (* Check what comes next *)
  if Reader.peek r = Some ',' then (
    (* Multiple layer names - this is a statement *)
    Reader.skip r;
    Reader.ws r;
    let rest_names = read_layer_names [] in
    let all_names = first_name :: List.rev rest_names in
    Reader.expect ';' r;
    (* Return an empty layer rule for each name - they'll be split later *)
    (* For now, just return one with the first name and empty rules *)
    {
      layer = String.concat "," all_names;
      (* Store combined names; splitting can be added later. *)
      rules = [];
      media_queries = [];
      container_queries = [];
      supports_queries = [];
    })
  else if Reader.peek r = Some ';' then (
    (* Single layer name statement without rules *)
    Reader.skip r;
    {
      layer = first_name;
      rules = [];
      media_queries = [];
      container_queries = [];
      supports_queries = [];
    })
  else (
    (* Layer rule with braces *)
    Reader.expect '{' r;
    let rec read_nested_rules (acc : nested_rule list) =
      Reader.ws r;
      if Reader.peek r = Some '}' then (
        Reader.skip r;
        List.rev acc)
      else if Reader.looking_at r "@supports" then
        let supports = read_supports_rule r in
        read_nested_rules (Supports supports :: acc)
      else
        let rule = read_rule r in
        read_nested_rules (Rule rule :: acc)
    in
    let rules : nested_rule list = read_nested_rules [] in
    {
      layer = first_name;
      rules;
      media_queries = [];
      container_queries = [];
      supports_queries = [];
    })

(* Forward declaration of readers needed by read_sheet_item *)

let read_keyframes_rule (r : Reader.t) : keyframes_rule =
  Reader.with_context r "@keyframes" @@ fun () ->
  Reader.expect_string "@keyframes" r;
  Reader.ws r;
  let name = Reader.ident ~keep_case:true r in
  Reader.ws r;
  Reader.expect '{' r;
  let rec read_blocks acc =
    Reader.ws r;
    match Reader.peek r with
    | Some '}' ->
        Reader.skip r;
        List.rev acc
    | _ ->
        let selector = String.trim (Reader.until r '{') in
        Reader.expect '{' r;
        let declarations = Declaration.read_declarations r in
        Reader.ws r;
        Reader.expect '}' r;
        read_blocks (({ selector; declarations } : keyframe_block) :: acc)
  in
  { name; keyframes = read_blocks [] }

let read_font_face_rule (r : Reader.t) : font_face_rule =
  Reader.with_context r "@font-face" @@ fun () ->
  Reader.expect_string "@font-face" r;
  Reader.ws r;
  Reader.expect '{' r;
  let ff : font_face_rule ref =
    ref
      {
        font_family = None;
        src = None;
        font_style = None;
        font_weight = None;
        font_stretch = None;
        font_display = None;
        unicode_range = None;
        font_variant = None;
        font_feature_settings = None;
        font_variation_settings = None;
      }
  in
  let set name value =
    let v = Some (String.trim value) in
    match String.lowercase_ascii name with
    | "font-family" -> ff := { !ff with font_family = v }
    | "src" -> ff := { !ff with src = v }
    | "font-style" -> ff := { !ff with font_style = v }
    | "font-weight" -> ff := { !ff with font_weight = v }
    | "font-stretch" -> ff := { !ff with font_stretch = v }
    | "font-display" -> ff := { !ff with font_display = v }
    | "unicode-range" -> ff := { !ff with unicode_range = v }
    | "font-variant" -> ff := { !ff with font_variant = v }
    | "font-feature-settings" -> ff := { !ff with font_feature_settings = v }
    | "font-variation-settings" ->
        ff := { !ff with font_variation_settings = v }
    | _ -> Reader.err_invalid r "unknown font-face descriptor"
  in
  let rec loop () =
    Reader.ws r;
    match Reader.peek r with
    | Some '}' -> Reader.skip r
    | _ ->
        let name = Reader.ident ~keep_case:true r in
        Reader.ws r;
        Reader.expect ':' r;
        Reader.ws r;
        let value = Reader.until r ';' in
        (match Reader.peek r with Some ';' -> Reader.skip r | _ -> ());
        set name value;
        loop ()
  in
  loop ();
  !ff

let read_page_rule (r : Reader.t) : page_rule =
  Reader.with_context r "@page" @@ fun () ->
  Reader.expect_string "@page" r;
  Reader.ws r;
  let header = String.trim (Reader.until r '{') in
  let selector : string option = if header = "" then None else Some header in
  Reader.expect '{' r;
  let declarations = Declaration.read_declarations r in
  Reader.ws r;
  Reader.expect '}' r;
  ({ selector; declarations } : page_rule)

let read_sheet_item (r : Reader.t) : sheet_item =
  Reader.ws r;
  if Reader.looking_at r "@layer" then Layer (read_layer_rule r)
  else if Reader.looking_at r "@media" then Media (read_media_rule r)
  else if Reader.looking_at r "@supports" then Supports (read_supports_rule r)
  else if Reader.looking_at r "@container" then
    Container (read_container_rule r)
  else if Reader.looking_at r "@keyframes" then
    Keyframes (read_keyframes_rule r)
  else if Reader.looking_at r "@font-face" then
    Font_face (read_font_face_rule r)
  else if Reader.looking_at r "@page" then Page (read_page_rule r)
  else if Reader.looking_at r "@property" then Property (read_property_rule r)
  else Rule (read_rule r)

let read_stylesheet (r : Reader.t) : t =
  let items =
    Reader.with_context r "stylesheet" @@ fun () ->
    let rec read_items acc =
      Reader.ws r;
      if Reader.is_done r then List.rev acc
      else
        let item = read_sheet_item r in
        read_items (item :: acc)
    in
    read_items []
  in
  (* Organize items into proper stylesheet structure *)
  let empty_sheet =
    {
      charset = None;
      imports = [];
      namespaces = [];
      layers = [];
      keyframes = [];
      font_faces = [];
      pages = [];
      rules = [];
      media_queries = [];
      container_queries = [];
      starting_styles = [];
      supports_queries = [];
      at_properties = [];
    }
  in
  let rec categorize_items acc = function
    | [] -> acc
    | Charset c :: rest -> categorize_items { acc with charset = Some c } rest
    | Import i :: rest ->
        categorize_items { acc with imports = acc.imports @ [ i ] } rest
    | Namespace n :: rest ->
        categorize_items { acc with namespaces = acc.namespaces @ [ n ] } rest
    | Layer l :: rest ->
        categorize_items { acc with layers = acc.layers @ [ l ] } rest
    | Keyframes k :: rest ->
        categorize_items { acc with keyframes = acc.keyframes @ [ k ] } rest
    | Font_face f :: rest ->
        categorize_items { acc with font_faces = acc.font_faces @ [ f ] } rest
    | Page p :: rest ->
        categorize_items { acc with pages = acc.pages @ [ p ] } rest
    | Rule r :: rest ->
        categorize_items { acc with rules = acc.rules @ [ r ] } rest
    | Media m :: rest ->
        categorize_items
          { acc with media_queries = acc.media_queries @ [ m ] }
          rest
    | Container c :: rest ->
        categorize_items
          { acc with container_queries = acc.container_queries @ [ c ] }
          rest
    | Starting_style s :: rest ->
        categorize_items
          { acc with starting_styles = acc.starting_styles @ [ s ] }
          rest
    | Supports s :: rest ->
        categorize_items
          { acc with supports_queries = acc.supports_queries @ [ s ] }
          rest
    | Property p :: rest ->
        categorize_items
          { acc with at_properties = acc.at_properties @ [ p ] }
          rest
  in
  categorize_items empty_sheet items

(** {1 Inline Styles} *)

let pp_important config pp_ctx =
  if config.minify then Pp.string pp_ctx "!important"
  else (
    Pp.space pp_ctx ();
    Pp.string pp_ctx "!important")

let pp_decl_inline config pp_ctx decl =
  let name = Declaration.property_name decl in
  let value =
    Declaration.string_of_value ~minify:config.minify
      ~inline:(config.mode = Inline) decl
  in
  let is_important = Declaration.is_important decl in
  Pp.string pp_ctx name;
  Pp.char pp_ctx ':';
  if not config.minify then Pp.space pp_ctx ();
  Pp.string pp_ctx value;
  if is_important then pp_important config pp_ctx

let inline_style_of_declarations ?(minify = false) ?(mode : mode = Inline)
    ?(newline = false) props =
  let config = { mode; minify; optimize = false; newline } in
  let pp ctx () =
    let sep ctx () =
      Pp.semicolon ctx ();
      if not config.minify then Pp.space ctx ()
    in
    Pp.list ~sep (pp_decl_inline config) ctx props
  in
  Pp.to_string ~minify ~inline:(mode = Inline) pp ()

(** {1 Pretty-Printing} *)

(* Forward declarations for mutually recursive functions *)
(* Pretty-printer helpers *)

(* Generic at-rule pretty printer - currently unused but kept for potential
   future use *)
let _pp_at_rules ~at_rule ~condition ~name_part : rule list Pp.t =
 fun ctx rules ->
  Pp.char ctx '@';
  Pp.string ctx at_rule;
  Pp.space ctx ();
  if name_part <> "" then Pp.string ctx name_part;
  Pp.string ctx condition;
  Pp.sp ctx ();
  Pp.braces (Pp.list ~sep:Pp.cut pp_rule) ctx rules

let pp_layer_media : media_rule list Pp.t =
 fun ctx media_queries -> Pp.list ~sep:Pp.cut pp_media_rule ctx media_queries

let pp_layer_containers : container_rule list Pp.t =
 fun ctx container_queries ->
  Pp.list ~sep:Pp.cut pp_container_rule ctx container_queries

let pp_layer_supports : supports_rule list Pp.t =
 fun ctx supports_queries ->
  Pp.list ~sep:Pp.cut pp_supports_rule ctx supports_queries

let pp_layer_rules : nested_rule list Pp.t =
 fun ctx rules -> Pp.list ~sep:Pp.cut pp_nested_rule ctx rules

(* Helpers for layered stylesheet pretty-printing *)
let pp_stylesheet_rules : rule list Pp.t =
 fun ctx rules -> Pp.list ~sep:Pp.cut pp_rule ctx rules

let pp_stylesheet_media : media_rule list Pp.t =
 fun ctx media_queries -> Pp.list ~sep:Pp.cut pp_media_rule ctx media_queries

let pp_stylesheet_containers : container_rule list Pp.t =
 fun ctx container_queries ->
  Pp.list ~sep:Pp.cut pp_container_rule ctx container_queries

let pp_stylesheet_supports : supports_rule list Pp.t =
 fun ctx supports_queries ->
  Pp.list ~sep:Pp.cut pp_supports_rule ctx supports_queries

let pp_starting_style_rule : starting_style_rule Pp.t =
 fun ctx ssr ->
  Pp.string ctx "@starting-style";
  Pp.sp ctx ();
  Pp.braces (Pp.list ~sep:Pp.cut pp_rule) ctx ssr.starting_rules

let pp_starting_styles : starting_style_rule list Pp.t =
 fun ctx starting_styles ->
  Pp.list ~sep:Pp.cut pp_starting_style_rule ctx starting_styles

let pp_at_properties : property_rule list Pp.t =
 fun ctx at_properties -> Pp.list ~sep:Pp.cut pp_property_rule ctx at_properties

let pp_charset_rule : charset_rule Pp.t =
 fun ctx charset ->
  Pp.string ctx "@charset \"";
  Pp.string ctx charset.encoding;
  Pp.string ctx "\";"

let pp_import_rule : import_rule Pp.t =
 fun ctx import ->
  Pp.string ctx "@import ";
  Pp.string ctx import.url;
  (match import.layer with
  | Some l ->
      Pp.string ctx " layer(";
      Pp.string ctx l;
      Pp.string ctx ")"
  | None -> ());
  (match import.supports with
  | Some s ->
      Pp.string ctx " supports(";
      Pp.string ctx s;
      Pp.string ctx ")"
  | None -> ());
  (match import.media with
  | Some m ->
      Pp.sp ctx ();
      Pp.string ctx m
  | None -> ());
  Pp.semicolon ctx ()

let pp_namespace_rule : namespace_rule Pp.t =
 fun ctx ns ->
  Pp.string ctx "@namespace ";
  (match ns.prefix with
  | Some p ->
      Pp.string ctx p;
      Pp.sp ctx ()
  | None -> ());
  Pp.string ctx "url(";
  Pp.string ctx ns.uri;
  Pp.string ctx ");"

let pp_keyframes_rule : keyframes_rule Pp.t =
 fun ctx kf ->
  Pp.string ctx "@keyframes ";
  Pp.string ctx kf.name;
  Pp.sp ctx ();
  let pp_keyframe_block ctx (block : keyframe_block) =
    Pp.string ctx block.selector;
    Pp.sp ctx ();
    Pp.braces
      (fun ctx () ->
        Pp.cut ctx ();
        Pp.nest 2
          (Pp.list
             ~sep:(fun ctx () ->
               Pp.semicolon ctx ();
               Pp.cut ctx ())
             Declaration.pp_declaration)
          ctx block.declarations;
        Pp.cut ctx ())
      ctx ()
  in
  Pp.braces
    (fun ctx () ->
      Pp.cut ctx ();
      Pp.nest 2 (Pp.list ~sep:Pp.cut pp_keyframe_block) ctx kf.keyframes;
      Pp.cut ctx ())
    ctx ()

let pp_font_face_rule : font_face_rule Pp.t =
 fun ctx ff ->
  Pp.string ctx "@font-face ";
  let pp_descriptor ctx name value_opt =
    match value_opt with
    | Some value ->
        Pp.string ctx name;
        Pp.char ctx ':';
        Pp.sp ctx ();
        Pp.string ctx value;
        Pp.semicolon ctx ();
        Pp.cut ctx ()
    | None -> ()
  in
  Pp.braces
    (fun ctx () ->
      Pp.cut ctx ();
      Pp.nest 2
        (fun ctx () ->
          pp_descriptor ctx "font-family" ff.font_family;
          pp_descriptor ctx "src" ff.src;
          pp_descriptor ctx "font-style" ff.font_style;
          pp_descriptor ctx "font-weight" ff.font_weight;
          pp_descriptor ctx "font-stretch" ff.font_stretch;
          pp_descriptor ctx "font-display" ff.font_display;
          pp_descriptor ctx "unicode-range" ff.unicode_range;
          pp_descriptor ctx "font-variant" ff.font_variant;
          pp_descriptor ctx "font-feature-settings" ff.font_feature_settings;
          pp_descriptor ctx "font-variation-settings" ff.font_variation_settings)
        ctx ();
      Pp.cut ctx ())
    ctx ()

let pp_page_rule : page_rule Pp.t =
 fun ctx page ->
  Pp.string ctx "@page";
  (match page.selector with
  | Some s ->
      Pp.sp ctx ();
      Pp.string ctx s
  | None -> ());
  Pp.sp ctx ();
  Pp.braces
    (fun ctx () ->
      Pp.cut ctx ();
      Pp.nest 2
        (Pp.list
           ~sep:(fun ctx () ->
             Pp.semicolon ctx ();
             Pp.cut ctx ())
           Declaration.pp_declaration)
        ctx page.declarations;
      Pp.cut ctx ())
    ctx ()

(* Helper functions for to_string *)
let is_layer_empty (layer : layer_rule) =
  layer.rules = [] && layer.media_queries = []
  && layer.container_queries = []
  && layer.supports_queries = []

let pp_layer_body ctx (layer_rules : layer_rule) =
  let sep_if_needed prev_empty current =
    if (not prev_empty) && current <> [] then Pp.cut ctx ()
  in
  let rules_empty = layer_rules.rules = [] in
  pp_layer_rules ctx layer_rules.rules;
  sep_if_needed rules_empty layer_rules.media_queries;
  let media_empty = layer_rules.media_queries = [] in
  pp_layer_media ctx layer_rules.media_queries;
  sep_if_needed (rules_empty && media_empty) layer_rules.container_queries;
  let container_empty = layer_rules.container_queries = [] in
  pp_layer_containers ctx layer_rules.container_queries;
  sep_if_needed
    (rules_empty && media_empty && container_empty)
    layer_rules.supports_queries;
  pp_layer_supports ctx layer_rules.supports_queries

let pp_layer : layer_rule Pp.t =
 fun ctx layer_rules ->
  Pp.string ctx "@layer ";
  Pp.string ctx layer_rules.layer;
  if is_layer_empty layer_rules then Pp.semicolon ctx ()
  else (
    Pp.sp ctx ();
    Pp.braces (fun ctx () -> pp_layer_body ctx layer_rules) ctx ())

let pp_stylesheet_sections : t Pp.t =
 fun ctx stylesheet ->
  let sep_if_needed prev_empty current =
    if (not prev_empty) && current <> [] then Pp.cut ctx ()
  in

  let rules_empty = stylesheet.rules = [] in
  pp_stylesheet_rules ctx stylesheet.rules;

  sep_if_needed rules_empty stylesheet.starting_styles;
  let starting_empty = stylesheet.starting_styles = [] in
  pp_starting_styles ctx stylesheet.starting_styles;

  sep_if_needed (rules_empty && starting_empty) stylesheet.container_queries;
  let container_empty = stylesheet.container_queries = [] in
  pp_stylesheet_containers ctx stylesheet.container_queries;

  sep_if_needed
    (rules_empty && starting_empty && container_empty)
    stylesheet.supports_queries;
  let supports_empty = stylesheet.supports_queries = [] in
  pp_stylesheet_supports ctx stylesheet.supports_queries;

  sep_if_needed
    (rules_empty && starting_empty && container_empty && supports_empty)
    stylesheet.media_queries;
  let media_empty = stylesheet.media_queries = [] in
  pp_stylesheet_media ctx stylesheet.media_queries;

  sep_if_needed
    (rules_empty && starting_empty && container_empty && supports_empty
   && media_empty)
    stylesheet.at_properties;
  pp_at_properties ctx stylesheet.at_properties

let pp_empty_layer_group ctx prev_empty names =
  if names = [] then ()
  else (
    if not prev_empty then Pp.cut ctx ();
    Pp.string ctx "@layer ";
    Pp.string ctx (String.concat "," (List.rev names));
    Pp.semicolon ctx ())

let pp_layers : layer_rule list Pp.t =
 fun ctx layers ->
  if not ctx.minify then Pp.list ~sep:Pp.cut pp_layer ctx layers
  else
    let rec process_layers prev_empty current_empty_group = function
      | [] -> pp_empty_layer_group ctx prev_empty current_empty_group
      | layer :: rest when is_layer_empty layer ->
          process_layers prev_empty (layer.layer :: current_empty_group) rest
      | layer :: rest ->
          pp_empty_layer_group ctx prev_empty current_empty_group;
          if current_empty_group <> [] then Pp.cut ctx ()
          else if not prev_empty then Pp.cut ctx ();
          pp_layer ctx layer;
          process_layers false [] rest
    in
    process_layers true [] layers

(** {1 Stylesheet Rendering} *)

(* Version information *)
let version =
  match Build_info.V1.version () with
  | None -> "dev"
  | Some v -> Build_info.V1.Version.to_string v

let header =
  String.concat ""
    [ "/*! tw v"; version; " | MIT License | https://github.com/samoht/tw */" ]

let to_string ?(minify = false) ?(mode = Variables) ?(newline = true) stylesheet
    =
  let pp ctx () =
    (* Charset must be first *)
    (match stylesheet.charset with
    | Some cs ->
        pp_charset_rule ctx cs;
        Pp.cut ctx ()
    | None -> ());

    (* Imports after charset *)
    List.iter
      (fun imp ->
        pp_import_rule ctx imp;
        Pp.cut ctx ())
      stylesheet.imports;

    (* Namespaces after imports *)
    List.iter
      (fun ns ->
        pp_namespace_rule ctx ns;
        Pp.cut ctx ())
      stylesheet.namespaces;

    (* Add header if there are layers *)
    if List.length stylesheet.layers > 0 then (
      Pp.string ctx header;
      Pp.cut ctx ());

    (* Render layers with merging for minified mode *)
    pp_layers ctx stylesheet.layers;

    (* Add separator if needed *)
    if
      stylesheet.layers <> []
      && (stylesheet.rules <> []
         || stylesheet.starting_styles <> []
         || stylesheet.container_queries <> []
         || stylesheet.supports_queries <> []
         || stylesheet.media_queries <> []
         || stylesheet.at_properties <> [])
    then Pp.cut ctx ();

    (* Render stylesheet sections *)
    pp_stylesheet_sections ctx stylesheet;

    (* Add trailing newline (standard for CSS files) *)
    if newline && mode <> Inline then Pp.char ctx '\n'
  in
  Pp.to_string ~minify ~inline:(mode = Inline) pp ()

let pp = to_string
