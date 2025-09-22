(** CSS stylesheet types and construction functions *)

include Stylesheet_intf

(** {1 Construction Functions} *)

let rule ~selector ?(nested = []) declarations : rule =
  { selector; declarations; nested }

let charset encoding = Charset encoding

let import ~url ?layer ?supports ?media () =
  Import { url; layer; supports; media }

let namespace ?prefix uri = Namespace (prefix, uri)

let property ~syntax ?initial_value ?(inherits = false) name =
  Property { name; syntax; inherits; initial_value }

let layer_decl names = Layer_decl names
let layer ?name content = Layer (name, content)
let media ~condition content = Media (condition, content)
let container ?name ~condition content = Container (name, condition, content)
let supports ~condition content = Supports (condition, content)
let starting_style content = Starting_style content
let scope ?start ?end_ content = Scope (start, end_, content)
let keyframes name frames = Keyframes (name, frames)
let font_face descriptors = Font_face descriptors
let page ?selector declarations = Page (selector, declarations)
let v statements : stylesheet = statements
let empty_stylesheet : stylesheet = []

(** {1 Accessors} *)

let selector (rule : rule) = rule.selector
let declarations (rule : rule) = rule.declarations
let nested (rule : rule) = rule.nested

let property_rule_initial (type a) (r : a property_rule) =
  match r.initial_value with
  | None -> None
  | Some v ->
      let ctx =
        { Pp.minify = true; indent = 0; buf = Buffer.create 16; inline = false }
      in
      Variables.pp_value r.syntax ctx v;
      Some (Buffer.contents ctx.buf)

let default_decl_of_property_rule (type a) (r : a property_rule) =
  match property_rule_initial r with
  | Some s -> Declaration.custom_property r.name s
  | None -> Declaration.custom_property r.name ""

(** {1 Pretty Printing} *)

let pp_property_rule : 'a property_rule Pp.t =
 fun ctx { name; syntax; inherits; initial_value } ->
  Pp.string ctx "@property ";
  Pp.string ctx name;
  Pp.sp ctx ();
  Pp.braces
    (fun ctx () ->
      Pp.cut ctx ();
      Pp.nest 2
        (fun ctx () ->
          Pp.string ctx "syntax:";
          Pp.space_if_pretty ctx ();
          Variables.pp_syntax ctx syntax;
          Pp.string ctx ";";
          Pp.cut ctx ();
          Pp.string ctx "inherits:";
          Pp.space_if_pretty ctx ();
          Pp.string ctx (if inherits then "true" else "false");
          match initial_value with
          | None -> ()
          | Some v ->
              Pp.semicolon ctx ();
              Pp.cut ctx ();
              Pp.string ctx "initial-value:";
              Pp.space_if_pretty ctx ();
              Variables.pp_value syntax ctx v)
        ctx ();
      Pp.cut ctx ())
    ctx ()

let rec pp_rule : rule Pp.t =
 fun ctx rule ->
  Selector.pp ctx rule.selector;
  Pp.sp ctx ();
  let pp_body ctx () =
    match (rule.declarations, rule.nested) with
    | [], [] -> ()
    | decls, nested ->
        Pp.cut ctx ();
        let pp_declarations ctx () =
          Pp.nest 2
            (Pp.list
               ~sep:(fun ctx () ->
                 Pp.semicolon ctx ();
                 Pp.cut ctx ())
               Declaration.pp_declaration)
            ctx decls
        in
        let pp_nested ctx () = Pp.list ~sep:Pp.cut pp_statement ctx nested in
        (match (decls, nested) with
        | [], _ -> pp_nested ctx ()
        | _, [] -> pp_declarations ctx ()
        | _, _ ->
            pp_declarations ctx ();
            Pp.semicolon ctx ();
            Pp.cut ctx ();
            pp_nested ctx ());
        Pp.cut ctx ()
  in
  Pp.surround ~left:Pp.block_open ~right:Pp.block_close pp_body ctx ()

and pp_keyframe : keyframe Pp.t =
 fun ctx kf ->
  Pp.string ctx kf.keyframe_selector;
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
        ctx kf.keyframe_declarations;
      Pp.cut ctx ())
    ctx ()

and pp_font_face_descriptor : font_face_descriptor Pp.t =
 fun ctx desc ->
  let pp_descriptor name pp_value value =
    Pp.string ctx name;
    Pp.string ctx ":";
    Pp.space_if_pretty ctx ();
    pp_value ctx value
  in
  match desc with
  | Font_family families ->
      pp_descriptor "font-family"
        (fun ctx fams ->
          Pp.list ~sep:Pp.comma Properties.pp_font_family ctx fams)
        families
  | Src value -> pp_descriptor "src" Pp.string value
  | Font_style style ->
      pp_descriptor "font-style" Properties.pp_font_style style
  | Font_weight weight ->
      pp_descriptor "font-weight" Properties.pp_font_weight weight
  | Font_stretch stretch ->
      pp_descriptor "font-stretch" Properties.pp_font_stretch stretch
  | Font_display value ->
      pp_descriptor "font-display" Properties.pp_font_display value
  | Unicode_range value ->
      pp_descriptor "unicode-range" Properties.pp_unicode_range value
  | Font_variant value -> pp_descriptor "font-variant" Pp.string value
  | Font_feature_settings value ->
      pp_descriptor "font-feature-settings" Pp.string value
  | Font_variation_settings value ->
      pp_descriptor "font-variation-settings" Pp.string value
  | Size_adjust value -> pp_descriptor "size-adjust" Pp.string value
  | Ascent_override value -> pp_descriptor "ascent-override" Pp.string value
  | Descent_override value -> pp_descriptor "descent-override" Pp.string value
  | Line_gap_override value -> pp_descriptor "line-gap-override" Pp.string value

and pp_statement : statement Pp.t =
 fun ctx -> function
  | Rule rule -> pp_rule ctx rule
  | Charset encoding ->
      Pp.string ctx "@charset \"";
      Pp.string ctx encoding;
      Pp.string ctx "\";"
  | Import { url; layer; supports; media } ->
      Pp.string ctx "@import ";
      Pp.string ctx url;
      (match layer with
      | Some l ->
          Pp.string ctx " layer(";
          Pp.string ctx l;
          Pp.string ctx ")"
      | None -> ());
      (match supports with
      | Some s ->
          Pp.string ctx " supports(";
          Pp.string ctx s;
          Pp.string ctx ")"
      | None -> ());
      (match media with
      | Some m ->
          Pp.sp ctx ();
          Pp.string ctx m
      | None -> ());
      Pp.semicolon ctx ()
  | Namespace (prefix, uri) ->
      Pp.string ctx "@namespace ";
      (match prefix with
      | Some p ->
          Pp.string ctx p;
          Pp.sp ctx ()
      | None -> ());
      Pp.string ctx "url(";
      Pp.string ctx uri;
      Pp.string ctx ");"
  | Property r -> pp_property_rule ctx r
  | Layer_decl names ->
      Pp.string ctx "@layer ";
      Pp.list ~sep:Pp.comma Pp.string ctx names;
      Pp.semicolon ctx ()
  | Layer (name, content) ->
      Pp.string ctx "@layer";
      (match name with
      | Some n ->
          Pp.string ctx " ";
          Pp.string ctx n
      | None -> ());
      (* For empty layers: use statement form when minifying (more concise), but
         preserve block form otherwise for roundtrip fidelity *)
      if content = [] && Pp.minified ctx then Pp.semicolon ctx ()
      else (
        Pp.sp ctx ();
        Pp.braces pp_block ctx content)
  | Media (condition, content) ->
      Pp.string ctx "@media ";
      Pp.string ctx condition;
      Pp.sp ctx ();
      Pp.braces pp_block ctx content
  | Container (name, condition, content) ->
      Pp.string ctx "@container";
      (match name with
      | Some n ->
          Pp.char ctx ' ';
          Pp.string ctx n
      | None -> ());
      Pp.string ctx " ";
      Pp.string ctx condition;
      Pp.sp ctx ();
      Pp.braces pp_block ctx content
  | Supports (condition, content) ->
      Pp.string ctx "@supports ";
      Pp.string ctx condition;
      Pp.sp ctx ();
      Pp.braces pp_block ctx content
  | Starting_style content ->
      Pp.string ctx "@starting-style ";
      Pp.braces pp_block ctx content
  | Scope (start, end_, content) ->
      Pp.string ctx "@scope";
      (match start with
      | Some s ->
          Pp.sp ctx ();
          Pp.string ctx "(";
          Pp.string ctx s;
          Pp.string ctx ")"
      | None -> ());
      (match end_ with
      | Some e ->
          Pp.string ctx " to (";
          Pp.string ctx e;
          Pp.string ctx ")"
      | None -> ());
      Pp.sp ctx ();
      Pp.braces pp_block ctx content
  | Keyframes (name, frames) ->
      Pp.string ctx "@keyframes ";
      Pp.string ctx name;
      Pp.sp ctx ();
      Pp.braces
        (fun ctx () ->
          Pp.cut ctx ();
          Pp.nest 2 (Pp.list ~sep:Pp.cut pp_keyframe) ctx frames;
          Pp.cut ctx ())
        ctx ()
  | Font_face descriptors ->
      Pp.string ctx "@font-face ";
      Pp.braces
        (fun ctx () ->
          Pp.cut ctx ();
          Pp.nest 2
            (Pp.list
               ~sep:(fun ctx () ->
                 Pp.semicolon ctx ();
                 Pp.cut ctx ())
               pp_font_face_descriptor)
            ctx descriptors;
          Pp.cut ctx ())
        ctx ()
  | Page (selector, declarations) ->
      Pp.string ctx "@page";
      (match selector with
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
            ctx declarations;
          Pp.cut ctx ())
        ctx ()

and pp_block : block Pp.t =
 fun ctx statements ->
  Pp.cut ctx ();
  Pp.nest 2 (Pp.list ~sep:Pp.cut pp_statement) ctx statements;
  Pp.cut ctx ()

let pp_stylesheet : stylesheet Pp.t =
 fun ctx statements -> Pp.list ~sep:Pp.cut pp_statement ctx statements

(** {1 Rendering} *)

let version =
  match Build_info.V1.version () with
  | None -> "dev"
  | Some v -> Build_info.V1.Version.to_string v

let header_string =
  String.concat ""
    [ "/*! tw v"; version; " | MIT License | https://github.com/samoht/tw */" ]

let to_string ?(minify = false) ?(mode = Variables) ?(newline = true)
    ?(header = true) statements =
  let pp ctx () =
    (* Add header if enabled and there are any layer statements *)
    let has_layers =
      List.exists
        (function Layer _ | Layer_decl _ -> true | _ -> false)
        statements
    in
    if header && has_layers then (
      Pp.string ctx header_string;
      Pp.cut ctx ());
    pp_stylesheet ctx statements;
    if newline && mode <> Inline then Pp.char ctx '\n'
  in
  Pp.to_string ~minify ~inline:(mode = Inline) pp ()

let pp = to_string

(** {1 Legacy Compatibility} *)

(* Helper functions to extract specific elements from a stylesheet *)
let rec extract_rules = function
  | [] -> []
  | Rule r :: rest -> r :: extract_rules rest
  | _ :: rest -> extract_rules rest

let rec extract_layer_names = function
  | [] -> []
  | Layer (Some name, _) :: rest -> name :: extract_layer_names rest
  | Layer_decl names :: rest -> names @ extract_layer_names rest
  | _ :: rest -> extract_layer_names rest

let rec extract_media_queries = function
  | [] -> []
  | Media (condition, content) :: rest ->
      (condition, extract_rules content) :: extract_media_queries rest
  | _ :: rest -> extract_media_queries rest

let rec extract_container_queries = function
  | [] -> []
  | Container (name, condition, content) :: rest ->
      (name, condition, extract_rules content) :: extract_container_queries rest
  | _ :: rest -> extract_container_queries rest

(* Legacy compatibility functions *)
let empty = empty_stylesheet
let rules t = extract_rules t
let layers t = extract_layer_names t
let media_queries t = extract_media_queries t
let container_queries t = extract_container_queries t

(** {1 Reading/Parsing} *)

let read_keyframe (r : Reader.t) : keyframe =
  Reader.ws r;
  let selector = String.trim (Reader.until r '{') in
  Reader.expect '{' r;
  let declarations = Declaration.read_declarations r in
  Reader.ws r;
  Reader.expect '}' r;
  { keyframe_selector = selector; keyframe_declarations = declarations }

(* Helper functions for reading specific at-rules *)
let read_charset (r : Reader.t) : statement =
  Reader.expect_string "@charset" r;
  Reader.ws r;
  Reader.expect '"' r;
  let encoding = Reader.until r '"' in
  Reader.expect '"' r;
  Reader.ws r;
  Reader.expect ';' r;
  Charset encoding

let read_import (r : Reader.t) : statement =
  Reader.expect_string "@import" r;
  Reader.ws r;
  let content = Reader.until r ';' in
  Reader.expect ';' r;
  (* Parse import content - simplified for now *)
  Import
    { url = String.trim content; layer = None; supports = None; media = None }

let read_namespace (r : Reader.t) : statement =
  Reader.expect_string "@namespace" r;
  Reader.ws r;
  (* Check for optional prefix *)
  let prefix =
    if Reader.looking_at r "url(" then None
    else Some (Reader.ident ~keep_case:true r)
  in
  Reader.ws r;
  (* Read the URL *)
  let uri = Reader.url r in
  Reader.ws r;
  Reader.expect ';' r;
  Namespace (prefix, uri)

let read_keyframes (r : Reader.t) : statement =
  Reader.with_context r "@keyframes" @@ fun () ->
  Reader.expect_string "@keyframes" r;
  Reader.ws r;
  let name = Reader.ident ~keep_case:true r in
  Reader.ws r;
  Reader.expect '{' r;
  let rec read_frames acc =
    Reader.ws r;
    if Reader.peek r = Some '}' then (
      Reader.skip r;
      List.rev acc)
    else
      let kf = read_keyframe r in
      read_frames (kf :: acc)
  in
  let frames = read_frames [] in
  Keyframes (name, frames)

(* Read a font-face descriptor *)
let read_font_face_descriptor (r : Reader.t) : font_face_descriptor option =
  Reader.ws r;
  if Reader.peek r = Some '}' then None
  else if Reader.peek r = Some ';' then (
    Reader.skip r;
    None)
  else
    (* Define local readers for each descriptor type *)
    let read_font_family r =
      Reader.ws r;
      Reader.expect ':' r;
      Reader.ws r;
      let families =
        Reader.list ~sep:Reader.comma Properties.read_font_family r
      in
      Font_family families
    in
    let read_src r =
      Reader.ws r;
      Reader.expect ':' r;
      Reader.ws r;
      (* TODO: Proper src parsing with url() and local() *)
      let value = Declaration.read_property_value r in
      Src value
    in
    let read_font_style r =
      Reader.ws r;
      Reader.expect ':' r;
      Reader.ws r;
      let style = Properties.read_font_style r in
      Font_style style
    in
    let read_font_weight r =
      Reader.ws r;
      Reader.expect ':' r;
      Reader.ws r;
      let weight = Properties.read_font_weight r in
      Font_weight weight
    in
    let read_font_stretch r =
      Reader.ws r;
      Reader.expect ':' r;
      Reader.ws r;
      let stretch = Properties.read_font_stretch r in
      Font_stretch stretch
    in
    let read_font_display r =
      Reader.ws r;
      Reader.expect ':' r;
      Reader.ws r;
      let display = Properties.read_font_display r in
      Font_display display
    in
    let read_unicode_range r =
      Reader.ws r;
      Reader.expect ':' r;
      Reader.ws r;
      let range = Properties.read_unicode_range r in
      Unicode_range range
    in
    let read_font_variant r =
      Reader.ws r;
      Reader.expect ':' r;
      Reader.ws r;
      (* TODO: Proper font-variant parsing *)
      let value = Declaration.read_property_value r in
      Font_variant value
    in
    let read_font_feature_settings r =
      Reader.ws r;
      Reader.expect ':' r;
      Reader.ws r;
      (* TODO: Proper font-feature-settings parsing *)
      let value = Declaration.read_property_value r in
      Font_feature_settings value
    in
    let read_font_variation_settings r =
      Reader.ws r;
      Reader.expect ':' r;
      Reader.ws r;
      (* TODO: Proper font-variation-settings parsing *)
      let value = Declaration.read_property_value r in
      Font_variation_settings value
    in
    let read_size_adjust r =
      Reader.ws r;
      Reader.expect ':' r;
      Reader.ws r;
      (* TODO: Proper percentage parsing *)
      let value = Declaration.read_property_value r in
      Size_adjust value
    in
    let read_ascent_override r =
      Reader.ws r;
      Reader.expect ':' r;
      Reader.ws r;
      (* TODO: Proper percentage or normal parsing *)
      let value = Declaration.read_property_value r in
      Ascent_override value
    in
    let read_descent_override r =
      Reader.ws r;
      Reader.expect ':' r;
      Reader.ws r;
      (* TODO: Proper percentage or normal parsing *)
      let value = Declaration.read_property_value r in
      Descent_override value
    in
    let read_line_gap_override r =
      Reader.ws r;
      Reader.expect ':' r;
      Reader.ws r;
      (* TODO: Proper percentage or normal parsing *)
      let value = Declaration.read_property_value r in
      Line_gap_override value
    in
    (* Use Reader.enum to get the descriptor name, then parse its value *)
    let name = Reader.ident ~keep_case:false r in
    let descriptor =
      match name with
      | "font-family" -> read_font_family r
      | "src" -> read_src r
      | "font-style" -> read_font_style r
      | "font-weight" -> read_font_weight r
      | "font-stretch" -> read_font_stretch r
      | "font-display" -> read_font_display r
      | "unicode-range" -> read_unicode_range r
      | "font-variant" -> read_font_variant r
      | "font-feature-settings" -> read_font_feature_settings r
      | "font-variation-settings" -> read_font_variation_settings r
      | "size-adjust" -> read_size_adjust r
      | "ascent-override" -> read_ascent_override r
      | "descent-override" -> read_descent_override r
      | "line-gap-override" -> read_line_gap_override r
      | _ -> Reader.err_invalid r ("unknown font-face descriptor: " ^ name)
    in
    Reader.ws r;
    if Reader.peek r = Some ';' then Reader.skip r;
    Some descriptor

let read_font_face (r : Reader.t) : statement =
  Reader.with_context r "@font-face" @@ fun () ->
  Reader.expect_string "@font-face" r;
  Reader.ws r;
  Reader.expect '{' r;
  let rec read_descriptors acc =
    match read_font_face_descriptor r with
    | Some desc -> read_descriptors (desc :: acc)
    | None ->
        Reader.ws r;
        if Reader.peek r = Some '}' then List.rev acc else read_descriptors acc
  in
  let descriptors = read_descriptors [] in
  Reader.ws r;
  Reader.expect '}' r;
  Font_face descriptors

let read_page (r : Reader.t) : statement =
  Reader.with_context r "@page" @@ fun () ->
  Reader.expect_string "@page" r;
  Reader.ws r;
  let (selector : string option) =
    if Reader.peek r = Some '{' then None
    else Some (String.trim (Reader.until r '{'))
  in
  Reader.expect '{' r;
  let declarations = Declaration.read_declarations r in
  Reader.ws r;
  Reader.expect '}' r;
  Page (selector, declarations)

type property_reader_state = {
  syntax : Variables.any_syntax option;
  inherits : bool option;
  initial_value : string option;
}

let rec read_statement (r : Reader.t) : statement =
  Reader.ws r;
  let table : (string * (Reader.t -> statement)) list =
    [
      ("@charset", read_charset);
      ("@import", read_import);
      ("@namespace", read_namespace);
      ("@layer", read_layer);
      ("@media", read_media);
      ("@container", read_container);
      ("@supports", read_supports);
      ("@starting-style", read_starting_style);
      ("@scope", read_scope);
      ("@keyframes", read_keyframes);
      ("@font-face", read_font_face);
      ("@page", read_page);
      ("@property", read_property_rule);
    ]
  in
  if Reader.peek r = Some '@' then
    (* Try matching known at-rules by prefix *)
    let rec try_table = function
      | [] -> Rule (read_rule r)
      | (prefix, p) :: rest ->
          if Reader.looking_at r prefix then p r else try_table rest
    in
    try_table table
  else Rule (read_rule r)

and read_block (r : Reader.t) : block =
  let rec read_statements acc =
    Reader.ws r;
    if Reader.peek r = Some '}' then List.rev acc
    else
      let stmt = read_statement r in
      read_statements (stmt :: acc)
  in
  read_statements []

and read_starting_style (r : Reader.t) : statement =
  Reader.expect_string "@starting-style" r;
  Reader.ws r;
  Reader.expect '{' r;
  let content = read_block r in
  Reader.expect '}' r;
  Starting_style content

and read_media (r : Reader.t) : statement =
  Reader.expect_string "@media" r;
  Reader.ws r;
  let condition = String.trim (Reader.until r '{') in
  if String.length condition = 0 then
    Reader.err r "@media rule requires a media query condition";
  Reader.expect '{' r;
  let content = read_block r in
  Reader.expect '}' r;
  Media (condition, content)

and read_supports (r : Reader.t) : statement =
  Reader.expect_string "@supports" r;
  Reader.ws r;
  let condition = String.trim (Reader.until r '{') in
  if String.length condition = 0 then
    Reader.err r "@supports rule requires a condition";
  Reader.expect '{' r;
  let content = read_block r in
  Reader.expect '}' r;
  Supports (condition, content)

and read_scope (r : Reader.t) : statement =
  Reader.expect_string "@scope" r;
  Reader.ws r;
  (* Parse scope selectors - simplified *)
  let _header = String.trim (Reader.until r '{') in
  Reader.expect '{' r;
  let content = read_block r in
  Reader.expect '}' r;
  Scope (None, None, content)

and read_container (r : Reader.t) : statement =
  Reader.expect_string "@container" r;
  Reader.ws r;
  let header = String.trim (Reader.until r '{') in
  let (container_name : string option), condition =
    let rr = Reader.of_string header in
    try
      let nm = Reader.ident ~keep_case:true rr in
      Reader.ws rr;
      if Reader.is_done rr then (None, header)
      else (Some nm, Reader.string ~trim:true rr)
    with Reader.Parse_error _ -> (None, header)
  in
  Reader.expect '{' r;
  let content = read_block r in
  Reader.expect '}' r;
  Container (container_name, condition, content)

and read_layer (r : Reader.t) : statement =
  Reader.expect_string "@layer" r;
  Reader.ws r;
  if Reader.peek r = Some '{' then (
    (* Anonymous layer *)
    Reader.expect '{' r;
    let content = read_block r in
    Reader.expect '}' r;
    Layer (None, content))
  else
    let first = Reader.ident ~keep_case:true r in
    Reader.ws r;
    match Reader.peek r with
    | Some ';' ->
        Reader.skip r;
        Layer_decl [ first ]
    | Some ',' ->
        Reader.skip r;
        (* Consume the comma *)
        Reader.ws r;
        let rest =
          Reader.list ~sep:Reader.comma ~at_least:1
            (fun r ->
              Reader.ws r;
              Reader.ident ~keep_case:true r)
            r
        in
        Reader.ws r;
        Reader.expect ';' r;
        Layer_decl (first :: rest)
    | Some '{' ->
        Reader.expect '{' r;
        let content = read_block r in
        Reader.expect '}' r;
        Layer (Some first, content)
    | _ -> Reader.err_invalid r "expected ';' or '{' after @layer name"

(* Helper: Read declarations until closing brace *)
and read_declarations_block (r : Reader.t) : Declaration.declaration list =
  let rec loop acc =
    Reader.ws r;
    if Reader.peek r = Some '}' then List.rev acc
    else
      match Declaration.read_declaration r with
      | Some d ->
          Reader.ws r;
          (match Reader.peek r with Some ';' -> Reader.skip r | _ -> ());
          loop (d :: acc)
      | None ->
          (* If we can't read a declaration, check if we're at the end *)
          if Reader.peek r = Some '}' then List.rev acc else List.rev acc
  in
  loop []

(* Helper: Read nested at-rule with declarations content *)
and read_nested_at_rule (r : Reader.t) (at_rule : string)
    (selector : Selector.t) : statement =
  Reader.with_context r at_rule @@ fun () ->
  Reader.expect_string at_rule r;
  Reader.ws r;
  let condition = String.trim (Reader.until r '{') in
  Reader.expect '{' r;
  let decls = read_declarations_block r in
  Reader.expect '}' r;
  (* Wrap declarations in a rule with the parent selector *)
  let content = [ Rule { selector; declarations = decls; nested = [] } ] in
  match at_rule with
  | "@supports" -> Supports (condition, content)
  | "@media" -> Media (condition, content)
  | "@container" ->
      (* Parse container name if present *)
      let container_name, cond =
        let rr = Reader.of_string condition in
        try
          let nm = Reader.ident ~keep_case:true rr in
          Reader.ws rr;
          if Reader.is_done rr then (None, condition)
          else (Some nm, Reader.string ~trim:true rr)
        with Reader.Parse_error _ -> (None, condition)
      in
      Container (container_name, cond, content)
  | _ -> Reader.err_invalid r ("Unexpected nested at-rule: " ^ at_rule)

and read_rule (r : Reader.t) : rule =
  Reader.with_context r "rule" @@ fun () ->
  let selector = Selector.read_selector_list r in
  Reader.ws r;
  Reader.expect '{' r;
  (* Helper to handle cases where no declaration is parsed *)
  let rec handle_no_declaration decls nested =
    (* Check if we're at the end of file or end of block *)
    (* If no declaration was parsed, check why *)
    if Reader.peek r = Some '}' then
      (* We've reached the end of this rule block *)
      { selector; declarations = List.rev decls; nested = List.rev nested }
    else
      (* Try to parse as a nested rule - CSS nesting is valid *)
      let nr = read_rule r in
      loop decls (Rule nr :: nested)
  and loop decls nested =
    Reader.ws r;
    match Reader.peek r with
    | Some '}' ->
        Reader.skip r;
        { selector; declarations = List.rev decls; nested = List.rev nested }
    | Some '@' ->
        (* Handle nested at-rules within rule blocks *)
        let stmt =
          if
            Reader.looking_at r "@supports"
            || Reader.looking_at r "@media"
            || Reader.looking_at r "@container"
          then
            read_nested_at_rule r
              (if Reader.looking_at r "@supports" then "@supports"
               else if Reader.looking_at r "@media" then "@media"
               else "@container")
              selector
          else if Reader.looking_at r "@layer" then (
            Reader.expect_string "@layer" r;
            Reader.ws r;
            if Reader.peek r = Some '{' then (
              Reader.expect '{' r;
              let decls = read_declarations_block r in
              Reader.expect '}' r;
              let content =
                [ Rule { selector; declarations = decls; nested = [] } ]
              in
              Layer (None, content))
            else
              let name = Reader.ident ~keep_case:true r in
              Reader.ws r;
              Reader.expect '{' r;
              let decls = read_declarations_block r in
              Reader.expect '}' r;
              let content =
                [ Rule { selector; declarations = decls; nested = [] } ]
              in
              Layer (Some name, content))
          else
            (* For other at-rules, use the standard read_statement *)
            read_statement r
        in
        loop decls (stmt :: nested)
    | Some ';' ->
        (* Skip empty statements/extra semicolons *)
        Reader.skip r;
        loop decls nested
    | _ -> (
        match Declaration.read_declaration r with
        | Some d ->
            Reader.ws r;
            (match Reader.peek r with Some ';' -> Reader.skip r | _ -> ());
            loop (d :: decls) nested
        | None -> handle_no_declaration decls nested)
  in
  loop [] []

and read_property_rule (r : Reader.t) : statement =
  (* Read @property descriptors as a separate helper to keep the reader tidy. *)
  Reader.expect_string "@property" r;
  Reader.ws r;
  let name = Reader.ident ~keep_case:true r in
  Reader.ws r;
  Reader.expect '{' r;
  let state = read_property_descriptors r in
  match (state.syntax, state.inherits) with
  | None, _ ->
      Reader.err_invalid r "@property: missing required 'syntax' descriptor"
  | _, None ->
      Reader.err_invalid r "@property: missing required 'inherits' descriptor"
  | Some (Variables.Syntax syntax), Some inherits ->
      (* Check if initial-value is required (when syntax is not "*") *)
      let is_universal_syntax =
        match syntax with Universal -> true | _ -> false
      in
      let initial_value =
        match state.initial_value with
        | None when not is_universal_syntax ->
            Reader.err_invalid r
              "@property: initial-value is required for non-universal syntax"
        | None -> None
        | Some str ->
            let value_reader = Reader.of_string str in
            Some (Variables.read_value value_reader syntax)
      in
      Property { name; syntax; inherits; initial_value }

and read_property_descriptors (r : Reader.t) : property_reader_state =
  let state = ref { syntax = None; inherits = None; initial_value = None } in
  let rec loop () =
    Reader.ws r;
    if Reader.peek r = Some '}' then (
      Reader.skip r;
      !state)
    else
      let key = Reader.ident ~keep_case:false r in
      Reader.ws r;
      Reader.expect ':' r;
      Reader.ws r;
      (match key with
      | "syntax" ->
          let syn = Variables.read_syntax r in
          state := { !state with syntax = Some syn }
      | "inherits" ->
          let inherits_value = Reader.bool r in
          state := { !state with inherits = Some inherits_value }
      | "initial-value" ->
          let value_str = Reader.css_value ~stops:[ ';'; '}' ] r in
          state := { !state with initial_value = Some value_str }
      | _ -> Reader.err_invalid r "unknown property descriptor");
      Reader.ws r;
      if Reader.peek r = Some ';' then Reader.skip r;
      loop ()
  in
  loop ()

let read_stylesheet (r : Reader.t) : stylesheet =
  Reader.with_context r "stylesheet" (fun () ->
      let rec read_statements acc =
        Reader.ws r;
        if Reader.is_done r then List.rev acc
        else
          match Reader.peek r with
          | Some '}' ->
              (* Unexpected closing brace at stylesheet level is an error *)
              Reader.err_invalid r "unexpected '}' at stylesheet level"
          | _ ->
              let stmt = read_statement r in
              read_statements (stmt :: acc)
      in
      read_statements [])

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
  (* Build the inline style string with minimal nesting to satisfy linter *)
  let buf = Buffer.create 128 in
  let pp_ctx =
    { Pp.minify = config.minify; indent = 0; buf; inline = mode = Inline }
  in
  let first = ref true in
  List.iter
    (fun decl ->
      if !first then first := false
      else (
        Pp.semicolon pp_ctx ();
        if not config.minify then Pp.space pp_ctx ());
      pp_decl_inline config pp_ctx decl)
    props;
  Buffer.contents buf

(** {1 Variable extraction from stylesheets} *)

let rec vars_of_statement (stmt : statement) : Variables.any_var list =
  match stmt with
  | Rule rule -> Variables.vars_of_declarations rule.declarations
  | Media (_, block)
  | Container (_, _, block)
  | Supports (_, block)
  | Layer (_, block)
  | Starting_style block
  | Scope (_, _, block) ->
      vars_of_block block
  | Font_face _ -> [] (* Font-face descriptors don't contribute CSS variables *)
  | Page (_, decls) -> Variables.vars_of_declarations decls
  | Charset _ | Import _ | Namespace _ | Property _ | Layer_decl _ | Keyframes _
    ->
      []

and vars_of_block (block : block) : Variables.any_var list =
  List.concat_map vars_of_statement block

let vars_of_stylesheet (ss : stylesheet) : Variables.any_var list =
  vars_of_block ss

(* Alias for API consistency *)
let read = read_stylesheet

(* Pretty-printer for import_rule *)
let pp_import_rule : import_rule Pp.t =
 fun ctx { url; layer; supports; media } ->
  Pp.string ctx "@import ";
  (* Always use string form - it's valid CSS and shorter than url() *)
  Pp.char ctx '"';
  Pp.string ctx url;
  Pp.char ctx '"';
  Option.iter
    (fun l ->
      Pp.string ctx " layer(";
      Pp.string ctx l;
      Pp.char ctx ')')
    layer;
  Option.iter
    (fun s ->
      Pp.string ctx " supports(";
      Pp.string ctx s;
      Pp.char ctx ')')
    supports;
  Option.iter
    (fun m ->
      Pp.space ctx ();
      Pp.string ctx m)
    media;
  Pp.string ctx ";"

(* Reader for import_rule *)
let read_import_rule (r : Reader.t) : import_rule =
  Reader.ws r;
  Reader.expect_string "@import" r;
  Reader.ws r;
  let url = Reader.one_of [ Reader.url; Reader.string ] r in
  Reader.ws r;
  let layer =
    if Reader.looking_at r "layer(" then (
      for _ = 1 to 6 do
        Reader.skip r
      done;
      let l = Reader.until r ')' in
      Reader.expect ')' r;
      Some l)
    else None
  in
  Reader.ws r;
  let supports =
    if Reader.looking_at r "supports(" then (
      for _ = 1 to 9 do
        Reader.skip r
      done;
      let s = Reader.until r ')' in
      Reader.expect ')' r;
      Some s)
    else None
  in
  Reader.ws r;
  let media =
    if Reader.looking_at r ";" then None else Some (Reader.until r ';')
  in
  if Reader.looking_at r ";" then Reader.expect ';' r;
  { url; layer; supports; media }

(* Pretty-printer for config *)
let pp_config : config Pp.t =
 fun ctx { minify; mode; optimize; newline } ->
  Pp.string ctx "{ minify = ";
  Pp.string ctx (if minify then "true" else "false");
  Pp.string ctx "; mode = ";
  Pp.string ctx
    (match mode with Inline -> "Inline" | Variables -> "Variables");
  Pp.string ctx "; optimize = ";
  Pp.string ctx (if optimize then "true" else "false");
  Pp.string ctx "; newline = ";
  Pp.string ctx (if newline then "true" else "false");
  Pp.string ctx " }"

(* Reader for config *)
let read_config (r : Reader.t) : config =
  Reader.ws r;
  Reader.expect_string "{" r;
  Reader.ws r;

  let minify = ref false in
  let mode = ref Variables in
  let optimize = ref false in
  let newline = ref false in

  while not (Reader.looking_at r "}") do
    let field_name = Reader.ident r in
    Reader.ws r;
    Reader.expect_string "=" r;
    Reader.ws r;
    let value = Reader.ident r in
    (match field_name with
    | "minify" -> minify := value = "true"
    | "mode" -> mode := if value = "Inline" then Inline else Variables
    | "optimize" -> optimize := value = "true"
    | "newline" -> newline := value = "true"
    | _ -> () (* ignore unknown fields *));
    Reader.ws r;
    (* Skip semicolon if present *)
    if Reader.looking_at r ";" then (
      Reader.skip r;
      Reader.ws r)
  done;
  Reader.expect_string "}" r;
  { minify = !minify; mode = !mode; optimize = !optimize; newline = !newline }
