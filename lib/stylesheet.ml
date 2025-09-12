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
  { name; syntax; inherits; initial_value }

let property_rule_name (r : property_rule) = r.name
let property_rule_initial (r : property_rule) = r.initial_value

let default_decl_of_property_rule (r : property_rule) =
  match r.initial_value with
  | Some v -> custom_property r.name v
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

(** {1 Pretty-printing} *)

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

let rec pp_supports_content : supports_content Pp.t =
 fun ctx -> function
  | Support_rules rules -> Pp.list ~sep:Pp.cut pp_rule ctx rules
  | Support_nested (rules, nested_queries) ->
      Pp.list ~sep:Pp.cut pp_rule ctx rules;
      if rules <> [] && nested_queries <> [] then Pp.cut ctx ();
      Pp.list ~sep:Pp.cut pp_supports_rule ctx nested_queries

and pp_supports_rule : supports_rule Pp.t =
 fun ctx sq ->
  Pp.string ctx "@supports ";
  Pp.string ctx sq.supports_condition;
  Pp.sp ctx ();
  Pp.braces pp_supports_content ctx sq.supports_content

let pp_media_rule : media_rule Pp.t =
 fun ctx mq ->
  Pp.string ctx "@media ";
  Pp.string ctx mq.media_condition;
  Pp.sp ctx ();
  Pp.braces (Pp.list ~sep:Pp.cut pp_rule) ctx mq.media_rules

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
        | None | Some "" -> ()
        | Some v ->
            Pp.semicolon ctx ();
            Pp.cut ctx ();
            Pp.string ctx "initial-value:";
            Pp.string ctx v)
      ctx ();
    Pp.cut ctx ()
  in
  Pp.surround ~left:Pp.block_open ~right:Pp.block_close pp_body ctx ()

let pp_nested_rule : nested_rule Pp.t =
 fun ctx -> function
  | Rule r -> pp_rule ctx r
  | Supports sq -> pp_supports_rule ctx sq

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

let pp_starting_style_rule : starting_style_rule Pp.t =
 fun ctx ss ->
  Pp.string ctx "@starting-style";
  Pp.sp ctx ();
  Pp.braces (Pp.list ~sep:Pp.cut pp_rule) ctx ss.starting_rules

let pp_keyframe_block : keyframe_block Pp.t =
 fun ctx kb ->
  Pp.string ctx kb.selector;
  Pp.sp ctx ();
  let pp_body ctx () =
    match kb.declarations with
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

let pp_keyframes_rule : keyframes_rule Pp.t =
 fun ctx kf ->
  Pp.string ctx "@keyframes ";
  Pp.string ctx kf.name;
  Pp.sp ctx ();
  Pp.braces (Pp.list ~sep:Pp.cut pp_keyframe_block) ctx kf.keyframes

let pp_font_face_rule : font_face_rule Pp.t =
 fun ctx ff ->
  Pp.string ctx "@font-face";
  Pp.sp ctx ();
  let pp_body ctx () =
    Pp.cut ctx ();
    Pp.nest 2
      (fun ctx () ->
        let pp_prop name value =
          match value with
          | None -> ()
          | Some v ->
              Pp.string ctx name;
              Pp.string ctx ": ";
              Pp.string ctx v;
              Pp.semicolon ctx ();
              Pp.cut ctx ()
        in
        pp_prop "font-family" ff.font_family;
        pp_prop "src" ff.src;
        pp_prop "font-style" ff.font_style;
        pp_prop "font-weight" ff.font_weight;
        pp_prop "font-stretch" ff.font_stretch;
        pp_prop "font-display" ff.font_display;
        pp_prop "unicode-range" ff.unicode_range;
        pp_prop "font-variant" ff.font_variant;
        pp_prop "font-feature-settings" ff.font_feature_settings;
        pp_prop "font-variation-settings" ff.font_variation_settings)
      ctx ();
    Pp.cut ctx ()
  in
  Pp.surround ~left:Pp.block_open ~right:Pp.block_close pp_body ctx ()

let pp_import_rule : import_rule Pp.t =
 fun ctx imp ->
  Pp.string ctx "@import ";
  Pp.string ctx "\"";
  Pp.string ctx imp.url;
  Pp.string ctx "\"";
  (match imp.layer with
  | Some l ->
      Pp.string ctx " layer(";
      Pp.string ctx l;
      Pp.string ctx ")"
  | None -> ());
  (match imp.supports with
  | Some s ->
      Pp.string ctx " supports(";
      Pp.string ctx s;
      Pp.string ctx ")"
  | None -> ());
  (match imp.media with
  | Some m ->
      Pp.string ctx " ";
      Pp.string ctx m
  | None -> ());
  Pp.semicolon ctx ()

let pp_charset_rule : charset_rule Pp.t =
 fun ctx cs ->
  Pp.string ctx "@charset \"";
  Pp.string ctx cs.encoding;
  Pp.string ctx "\"";
  Pp.semicolon ctx ()

let pp_namespace_rule : namespace_rule Pp.t =
 fun ctx ns ->
  Pp.string ctx "@namespace ";
  (match ns.prefix with
  | Some p ->
      Pp.string ctx p;
      Pp.sp ctx ()
  | None -> ());
  Pp.string ctx "\"";
  Pp.string ctx ns.uri;
  Pp.string ctx "\"";
  Pp.semicolon ctx ()

let pp_page_rule : page_rule Pp.t =
 fun ctx pg ->
  Pp.string ctx "@page";
  (match pg.selector with
  | Some s ->
      Pp.string ctx " ";
      Pp.string ctx s
  | None -> ());
  Pp.sp ctx ();
  let pp_body ctx () =
    match pg.declarations with
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

let pp : t Pp.t =
 fun ctx sheet ->
  (* Charset must be first *)
  (match sheet.charset with
  | Some cs ->
      pp_charset_rule ctx cs;
      Pp.cut ctx ()
  | None -> ());

  (* Imports after charset *)
  List.iter
    (fun imp ->
      pp_import_rule ctx imp;
      Pp.cut ctx ())
    sheet.imports;

  (* Namespaces after imports *)
  List.iter
    (fun ns ->
      pp_namespace_rule ctx ns;
      Pp.cut ctx ())
    sheet.namespaces;

  (* Layers *)
  List.iter
    (fun l ->
      pp_layer_rule ctx l;
      Pp.cut ctx ())
    sheet.layers;

  (* Keyframes *)
  List.iter
    (fun kf ->
      pp_keyframes_rule ctx kf;
      Pp.cut ctx ())
    sheet.keyframes;

  (* Font faces *)
  List.iter
    (fun ff ->
      pp_font_face_rule ctx ff;
      Pp.cut ctx ())
    sheet.font_faces;

  (* Pages *)
  List.iter
    (fun pg ->
      pp_page_rule ctx pg;
      Pp.cut ctx ())
    sheet.pages;

  (* Property rules *)
  List.iter
    (fun pr ->
      pp_property_rule ctx pr;
      Pp.cut ctx ())
    sheet.at_properties;

  (* Regular rules *)
  List.iter
    (fun r ->
      pp_rule ctx r;
      Pp.cut ctx ())
    sheet.rules;

  (* Media queries *)
  List.iter
    (fun mq ->
      pp_media_rule ctx mq;
      Pp.cut ctx ())
    sheet.media_queries;

  (* Container queries *)
  List.iter
    (fun cq ->
      pp_container_rule ctx cq;
      Pp.cut ctx ())
    sheet.container_queries;

  (* Starting styles *)
  List.iter
    (fun ss ->
      pp_starting_style_rule ctx ss;
      Pp.cut ctx ())
    sheet.starting_styles;

  (* Supports queries *)
  List.iter
    (fun sq ->
      pp_supports_rule ctx sq;
      Pp.cut ctx ())
    sheet.supports_queries

(** {1 Reading/Parsing} *)

let read_rule (r : Reader.t) : rule =
  Reader.with_context r "rule" @@ fun () ->
  let selector = Selector.read_selector_list r in
  Reader.ws r;
  Reader.expect '{' r;
  let declarations = Declaration.read_declarations r in
  Reader.ws r;
  Reader.expect '}' r;
  ({ selector; declarations } : rule)

let read_media_rule (r : Reader.t) : media_rule =
  Reader.with_context r "@media" @@ fun () ->
  Reader.expect_string "@media" r;
  Reader.ws r;
  let condition = Reader.until r '{' in
  let condition = String.trim condition in
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

let read_property_rule (r : Reader.t) : property_rule =
  Reader.with_context r "@property" @@ fun () ->
  Reader.expect_string "@property" r;
  Reader.ws r;
  let name = Reader.ident ~keep_case:true r in
  Reader.ws r;
  Reader.expect '{' r;
  Reader.ws r;

  (* Read syntax *)
  Reader.expect_string "syntax:" r;
  Reader.ws r;
  Reader.expect '"' r;
  let syntax = Reader.until r '"' in
  Reader.expect '"' r;
  Reader.ws r;
  Reader.expect ';' r;
  Reader.ws r;

  (* Read inherits *)
  Reader.expect_string "inherits:" r;
  Reader.ws r;
  let inherits =
    Reader.enum "inherits" [ ("true", true); ("false", false) ] r
  in
  Reader.ws r;
  Reader.expect ';' r;
  Reader.ws r;

  (* Optional initial-value *)
  let initial_value =
    Reader.option
      (fun r ->
        Reader.expect_string "initial-value:" r;
        Reader.ws r;
        let value = Reader.until r ';' in
        Reader.expect ';' r;
        Reader.ws r;
        String.trim value)
      r
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
      (* Store all names for now *)
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

let read_sheet_item (r : Reader.t) : sheet_item =
  Reader.ws r;
  if Reader.looking_at r "@layer" then Layer (read_layer_rule r)
  else if Reader.looking_at r "@media" then Media (read_media_rule r)
  else if Reader.looking_at r "@supports" then Supports (read_supports_rule r)
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
