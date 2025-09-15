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
let font_face declarations = Font_face declarations
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
  | Property { name; syntax; inherits; initial_value } ->
      Pp.string ctx "@property ";
      Pp.string ctx name;
      Pp.sp ctx ();
      Pp.braces
        (fun ctx () ->
          Pp.cut ctx ();
          Pp.nest 2
            (fun ctx () ->
              Pp.string ctx "syntax: \"";
              Variables.pp_syntax ctx syntax;
              Pp.string ctx "\";";
              Pp.cut ctx ();
              Pp.string ctx "inherits: ";
              Pp.string ctx (if inherits then "true" else "false");
              match initial_value with
              | None -> ()
              | Some v ->
                  Pp.semicolon ctx ();
                  Pp.cut ctx ();
                  Pp.string ctx "initial-value: ";
                  Variables.pp_value syntax ctx v)
            ctx ();
          Pp.cut ctx ())
        ctx ()
  | Layer_decl names ->
      Pp.string ctx "@layer ";
      Pp.string ctx (String.concat ", " names);
      Pp.semicolon ctx ()
  | Layer (name, content) ->
      Pp.string ctx "@layer";
      (match name with
      | Some n ->
          Pp.sp ctx ();
          Pp.string ctx n
      | None -> ());
      Pp.sp ctx ();
      Pp.braces pp_block ctx content
  | Media (condition, content) ->
      Pp.string ctx "@media ";
      Pp.string ctx condition;
      Pp.sp ctx ();
      Pp.braces pp_block ctx content
  | Container (name, condition, content) ->
      Pp.string ctx "@container";
      (match name with
      | Some n ->
          Pp.sp ctx ();
          Pp.string ctx n
      | None -> ());
      Pp.sp ctx ();
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
  | Font_face declarations ->
      Pp.string ctx "@font-face ";
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

let header =
  String.concat ""
    [ "/*! tw v"; version; " | MIT License | https://github.com/samoht/tw */" ]

let to_string ?(minify = false) ?(mode = Variables) ?(newline = true) statements
    =
  let pp ctx () =
    (* Add header if there are any layer statements *)
    let has_layers =
      List.exists
        (function Layer _ | Layer_decl _ -> true | _ -> false)
        statements
    in
    if has_layers then (
      Pp.string ctx header;
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
  let content = Reader.until r ';' in
  Reader.expect ';' r;
  (* Parse namespace - simplified *)
  let parts = String.split_on_char ' ' (String.trim content) in
  match parts with
  | [ uri ] -> Namespace (None, uri)
  | prefix :: uri :: _ -> Namespace (Some prefix, uri)
  | [] -> Namespace (None, "")

let read_keyframes (r : Reader.t) : statement =
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

let read_font_face (r : Reader.t) : statement =
  Reader.expect_string "@font-face" r;
  Reader.ws r;
  Reader.expect '{' r;
  let declarations = Declaration.read_declarations r in
  Reader.ws r;
  Reader.expect '}' r;
  Font_face declarations

let read_page (r : Reader.t) : statement =
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

type property_reader_state =
  | Init : property_reader_state
  | Syntax : 'a Variables.syntax -> property_reader_state
  | Final :
      'a Variables.syntax * bool option * 'a option
      -> property_reader_state

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
  Reader.expect '{' r;
  let content = read_block r in
  Reader.expect '}' r;
  Media (condition, content)

and read_supports (r : Reader.t) : statement =
  Reader.expect_string "@supports" r;
  Reader.ws r;
  let condition = String.trim (Reader.until r '{') in
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
        let rest =
          Reader.list ~sep:Reader.comma
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

and read_rule (r : Reader.t) : rule =
  Reader.with_context r "rule" @@ fun () ->
  let selector = Selector.read_selector_list r in
  Reader.ws r;
  Reader.expect '{' r;
  let rec loop decls nested =
    Reader.ws r;
    match Reader.peek r with
    | Some '}' ->
        Reader.skip r;
        { selector; declarations = List.rev decls; nested = List.rev nested }
    | Some '@' ->
        let stmt = read_statement r in
        loop decls (stmt :: nested)
    | _ -> (
        match Declaration.read_declaration r with
        | Some d ->
            Reader.ws r;
            (match Reader.peek r with Some ';' -> Reader.skip r | _ -> ());
            loop (d :: decls) nested
        | None ->
            let nr = read_rule r in
            loop decls (Rule nr :: nested))
  in
  loop [] []

and read_property_rule (r : Reader.t) : statement =
  (* Read @property descriptors as a separate helper to keep the reader tidy. *)
  Reader.expect_string "@property" r;
  Reader.ws r;
  let name = Reader.ident ~keep_case:true r in
  Reader.ws r;
  Reader.expect '{' r;
  match read_property_descriptors r with
  | Final (syntax, inherits, initial_value) ->
      Property
        {
          name;
          syntax;
          inherits = Option.value inherits ~default:false;
          initial_value;
        }
  | _ -> Reader.err_invalid r "read_property_rule"

and read_property_descriptors (r : Reader.t) : property_reader_state =
  let rec loop (state : property_reader_state) =
    Reader.ws r;
    if Reader.peek r = Some '}' then (
      Reader.skip r;
      state)
    else if Reader.looking_at r "syntax:" then (
      Reader.expect_string "syntax:" r;
      Reader.ws r;
      let (Variables.Syntax syntax) = Variables.read_syntax r in
      Reader.ws r;
      if Reader.peek r = Some ';' then Reader.skip r;
      loop (Syntax syntax))
    else if Reader.looking_at r "inherits:" then (
      Reader.expect_string "inherits:" r;
      Reader.ws r;
      let inherits_value = Reader.ident r = "true" in
      Reader.ws r;
      if Reader.peek r = Some ';' then Reader.skip r;
      match state with
      | Init -> loop Init (* Need syntax first *)
      | Syntax syntax -> loop (Final (syntax, Some inherits_value, None))
      | Final (syntax, _, initial) ->
          loop (Final (syntax, Some inherits_value, initial)))
    else if Reader.looking_at r "initial-value:" then (
      Reader.expect_string "initial-value:" r;
      Reader.ws r;
      match state with
      | Syntax syntax ->
          let initial_value = Variables.read_value r syntax in
          Reader.ws r;
          if Reader.peek r = Some ';' then Reader.skip r;
          loop (Final (syntax, None, Some initial_value))
      | Final (syntax, inherits, _) ->
          let initial_value = Variables.read_value r syntax in
          Reader.ws r;
          if Reader.peek r = Some ';' then Reader.skip r;
          loop (Final (syntax, inherits, Some initial_value))
      | Init ->
          Reader.err_invalid r "syntax must be specified before initial-value")
    else Reader.err_invalid r "unknown property descriptor"
  in
  loop Init

let read_stylesheet (r : Reader.t) : stylesheet =
  Reader.with_context r "stylesheet" (fun () ->
      let rec read_statements acc =
        Reader.ws r;
        if Reader.is_done r then List.rev acc
        else
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
  let pp ctx () =
    let sep ctx () =
      Pp.semicolon ctx ();
      if not config.minify then Pp.space ctx ()
    in
    Pp.list ~sep (pp_decl_inline config) ctx props
  in
  Pp.to_string ~minify ~inline:(mode = Inline) pp ()

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
  | Font_face decls | Page (_, decls) -> Variables.vars_of_declarations decls
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
  if String.contains url ' ' then (
    Pp.char ctx '"';
    Pp.string ctx url;
    Pp.char ctx '"')
  else (
    Pp.string ctx "url(";
    Pp.string ctx url;
    Pp.char ctx ')');
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
  if Reader.looking_at r "@import" then Reader.skip r;
  Reader.ws r;
  let url =
    if Reader.looking_at r "url(" then (
      Reader.skip r;
      let u = Reader.until r ')' in
      Reader.expect ')' r;
      u)
    else Reader.string r
  in
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
let read_config (_ : Reader.t) : config =
  (* Simple default config reader *)
  { minify = false; mode = Variables; optimize = false; newline = false }
