(** CSS stylesheet types and construction functions *)

include Stylesheet_intf

(** {1 Construction Functions} *)

let rule ~selector ?(nested = []) ?merge_key declarations : rule =
  { selector; declarations; nested; merge_key }

let property ~syntax ?initial_value ?(inherits = false) name =
  Property { name; syntax; inherits; initial_value }

let layer_decl names = Layer_decl names
let layer ?name content = Layer (name, content)
let media ~condition content = Media (condition, content)

let media_nested ~condition declarations =
  Media (condition, [ Declarations declarations ])

let container ?name ~condition content = Container (name, condition, content)
let supports ~condition content = Supports (condition, content)
let starting_style content = Starting_style content

let starting_style_nested declarations =
  Starting_style [ Declarations declarations ]

let keyframes name frames = Keyframes (name, frames)
let v statements : stylesheet = statements
let empty_stylesheet : stylesheet = []

(** {1 Accessors} *)

let selector (rule : rule) = rule.selector
let declarations (rule : rule) = rule.declarations
let nested (rule : rule) = rule.nested

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
  Pp.block_open ctx ();
  let decls = Declaration.resolve_theme_guards ctx rule.declarations in
  (match (decls, rule.nested) with
  | [], [] -> ()
  | decls, nested ->
      let ctx = { ctx with indent = ctx.indent + 1 } in
      let pp_declarations ctx () =
        Pp.list
          ~sep:(fun ctx () ->
            Pp.semicolon ctx ();
            Pp.cut ctx ())
          (Pp.indent Declaration.pp_declaration)
          ctx decls
      in
      let pp_nested ctx () =
        Pp.list ~sep:Pp.cut (Pp.indent pp_statement) ctx nested
      in
      Pp.cut ctx ();
      (match (decls, nested) with
      | [], _ -> pp_nested ctx ()
      | _, [] ->
          pp_declarations ctx ();
          (* Trailing semicolon after last declaration to match Tailwind *)
          if not ctx.minify then Pp.semicolon ctx ()
      | _, _ ->
          pp_declarations ctx ();
          Pp.semicolon ctx ();
          Pp.cut ctx ();
          pp_nested ctx ());
      if not ctx.minify then (
        Pp.cut ctx ();
        if ctx.indent > 1 then
          Pp.string ctx (String.make (2 * (ctx.indent - 1)) ' ')));
  Pp.block_close ctx ()

and pp_keyframe_selector : Keyframe.selector Pp.t =
 fun ctx sel ->
  match sel with
  | Keyframe.Positions positions ->
      Pp.list ~sep:Pp.comma
        (fun ctx pos -> Pp.string ctx (Keyframe.position_to_string pos))
        ctx positions
  | Keyframe.Raw s -> Pp.string ctx s

and pp_keyframe : keyframe Pp.t =
 fun ctx kf ->
  pp_keyframe_selector ctx kf.keyframe_selector;
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
  | Src value ->
      pp_descriptor "src"
        (fun ctx v -> Pp.string ctx (Font_face.src_to_string v))
        value
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
  | Size_adjust value ->
      pp_descriptor "size-adjust"
        (fun ctx v -> Pp.string ctx (Font_face.size_adjust_to_string v))
        value
  | Ascent_override value ->
      pp_descriptor "ascent-override"
        (fun ctx v -> Pp.string ctx (Font_face.metric_override_to_string v))
        value
  | Descent_override value ->
      pp_descriptor "descent-override"
        (fun ctx v -> Pp.string ctx (Font_face.metric_override_to_string v))
        value
  | Line_gap_override value ->
      pp_descriptor "line-gap-override"
        (fun ctx v -> Pp.string ctx (Font_face.metric_override_to_string v))
        value

and pp_statement : statement Pp.t =
 fun ctx -> function
  | Rule rule -> pp_rule ctx rule
  | Declarations raw_decls ->
      (* Bare declarations for CSS nesting - no selector/braces, just
         declarations. No extra indent since the containing block handles it *)
      let decls = Declaration.resolve_theme_guards ctx raw_decls in
      Pp.list
        ~sep:(fun ctx () ->
          Pp.semicolon ctx ();
          Pp.cut ctx ())
        Declaration.pp_declaration ctx decls;
      if decls <> [] then Pp.semicolon ctx ()
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
          Pp.string ctx (Supports.to_string s);
          Pp.string ctx ")"
      | None -> ());
      (match media with
      | Some m ->
          Pp.sp ctx ();
          Media.pp ctx m
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
      Media.pp ctx condition;
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
      Pp.string ctx (Container.to_string condition);
      Pp.sp ctx ();
      Pp.braces pp_block ctx content
  | Supports (condition, content) ->
      Pp.string ctx "@supports ";
      Supports.pp ctx condition;
      Pp.sp ctx ();
      Pp.braces pp_block ctx content
  | Starting_style content ->
      Pp.string ctx "@starting-style";
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
  | Page (selector, raw_declarations) ->
      Pp.string ctx "@page";
      (match selector with
      | Some s ->
          Pp.sp ctx ();
          Pp.string ctx s
      | None -> ());
      Pp.sp ctx ();
      let declarations =
        Declaration.resolve_theme_guards ctx raw_declarations
      in
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
  (* Block printing for at-rules (@media, @supports, etc.) The braces helper
     adds nest 1 and indent for the first item only. Subsequent items need
     explicit indentation and blank line separation to match Tailwind format. *)
  match statements with
  | [] -> ()
  | [ s ] -> pp_statement ctx s
  | s :: rest ->
      pp_statement ctx s;
      List.iter
        (fun stmt ->
          Pp.cut ctx ();
          if not ctx.Pp.minify then Pp.cut ctx ();
          Pp.indent pp_statement ctx stmt)
        rest

let is_layer_block = function Layer _ -> true | _ -> false

let pp_stylesheet : stylesheet Pp.t =
 fun ctx statements ->
  let rec loop = function
    | [] -> ()
    | [ s ] -> pp_statement ctx s
    | s :: (next :: _ as rest) ->
        pp_statement ctx s;
        Pp.cut ctx ();
        (* Add blank line between consecutive @layer { } blocks *)
        if (not (Pp.minified ctx)) && is_layer_block s && is_layer_block next
        then Pp.cut ctx ();
        loop rest
  in
  loop statements

(** {1 Rendering} *)

let to_string ?(minify = false) ?(mode = Variables) ?(newline = true)
    ?(header = "") ?theme ?(theme_defaults = Pp.no_theme_defaults) statements =
  let pp ctx () =
    (if header <> "" then
       let has_layers =
         List.exists
           (function Layer _ | Layer_decl _ -> true | _ -> false)
           statements
       in
       if has_layers then (
         Pp.string ctx header;
         Pp.cut ctx ()));
    pp_stylesheet ctx statements;
    if newline && mode <> Inline then Pp.char ctx '\n'
  in
  Pp.to_string ~minify ~inline:(mode = Inline) ?theme ~theme_defaults pp ()

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

let read_keyframe (r : Cursor.t) : keyframe =
  Cursor.ws r;
  let selector_str = Cursor.drain_until_block_to_string ~trim:true r in
  let declarations =
    Cursor.braces (fun inner -> Declaration.read_declarations inner) r
  in
  {
    keyframe_selector = Keyframe.selector_of_string selector_str;
    keyframe_declarations = declarations;
  }

(* Helper functions for reading specific at-rules *)

(* WHATWG Encoding labels for UTF-8 (https://encoding.spec.whatwg.org/#names-
   and-labels). Matched case-insensitively with ASCII whitespace trimmed. *)
let is_utf8_label s =
  let s = String.lowercase_ascii (String.trim s) in
  match s with
  | "utf-8" | "utf8" | "unicode-1-1-utf-8" | "unicode11utf8" | "unicode20utf8"
  | "x-unicode20utf8" ->
      true
  | _ -> false

let read_charset (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "charset" r;
  Cursor.ws r;
  (* CSS Syntax section 8.2 / css-syntax-3 requires the exact byte sequence
     [@charset "..."]; single-quoted or otherwise non-conforming charset
     at-rules are not recognized as charset declarations. *)
  let encoding =
    match Cursor.string_with_quote_opt r with
    | Some (value, '"') -> value
    | Some (_, _) ->
        Error.fail_bad_value (Cursor.position r) ~property:"@charset"
          ~reason:"@charset requires double quotes"
    | None -> Cursor.string r
  in
  (* Cascade is UTF-8-only. A [@charset] pointing at any other encoding is a
     request to decode the byte stream with a legacy decoder we don't ship, so
     reject it rather than silently pretending the bytes are already UTF-8. *)
  if not (is_utf8_label encoding) then
    Error.fail_bad_value (Cursor.position r) ~property:"@charset"
      ~reason:
        (String.concat ""
           [
             "Cascade accepts UTF-8 input only; cannot honour @charset \"";
             encoding;
             "\"";
           ]);
  Cursor.ws r;
  if Cursor.peek_semicolon r then Cursor.skip r;
  Charset encoding

let read_import (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "import" r;
  Cursor.ws r;
  let prelude = Cursor.drain_until_block r in
  if Cursor.peek_semicolon r then Cursor.skip r;
  Import
    {
      url = String.trim (Parser.to_string prelude);
      layer = None;
      supports = None;
      media = None;
    }

let read_namespace (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "namespace" r;
  Cursor.ws r;
  (* Optional prefix ident; absent when the URL comes first. *)
  let prefix =
    match Cursor.peek r with
    | Some (Component.Preserved { kind = Token.Ident _; _ }) ->
        Some (Cursor.ident ~keep_case:true r)
    | _ -> None
  in
  Cursor.ws r;
  let uri =
    match Cursor.peek r with
    | Some (Component.Preserved { kind = Token.String { value; _ }; _ }) ->
        Cursor.skip r;
        value
    | _ -> Cursor.url r
  in
  Cursor.ws r;
  if Cursor.peek_semicolon r then Cursor.skip r;
  Namespace (prefix, uri)

let read_keyframes_block inner =
  let rec read_frames acc =
    Cursor.ws inner;
    if Cursor.is_done inner then List.rev acc
    else read_frames (read_keyframe inner :: acc)
  in
  read_frames []

let read_keyframes (r : Cursor.t) : statement =
  Cursor.with_context r "@keyframes" @@ fun () ->
  Cursor.expect_at_keyword "keyframes" r;
  Cursor.ws r;
  let name = Cursor.ident ~keep_case:true r in
  Cursor.ws r;
  let frames = Cursor.braces read_keyframes_block r in
  Keyframes (name, frames)

(* Read a font-face descriptor *)
(* Helper to parse descriptor value after colon *)
let read_descriptor_value parse_fn constructor r =
  Cursor.ws r;
  if not (Cursor.colon r) then Cursor.err_expected r "':'";
  Cursor.ws r;
  let value = parse_fn r in
  constructor value

let read_font_face_descriptor (r : Cursor.t) : font_face_descriptor option =
  Cursor.ws r;
  if Cursor.is_done r then None
  else if Cursor.peek_semicolon r then (
    Cursor.skip r;
    None)
  else
    let name = Cursor.ident ~keep_case:false r in
    let descriptor =
      match name with
      | "font-family" ->
          read_descriptor_value
            (fun r ->
              Cursor.list ~sep:Cursor.comma Properties.read_font_family r)
            (fun v -> Font_family v)
            r
      | "src" ->
          read_descriptor_value Declaration.read_property_value
            (fun v -> Src (Font_face.src_of_string v))
            r
      | "font-style" ->
          read_descriptor_value Properties.read_font_style
            (fun v -> Font_style v)
            r
      | "font-weight" ->
          read_descriptor_value Properties.read_font_weight
            (fun v -> Font_weight v)
            r
      | "font-stretch" ->
          read_descriptor_value Properties.read_font_stretch
            (fun v -> Font_stretch v)
            r
      | "font-display" ->
          read_descriptor_value Properties.read_font_display
            (fun v -> Font_display v)
            r
      | "unicode-range" ->
          read_descriptor_value Properties.read_unicode_range
            (fun v -> Unicode_range v)
            r
      | "font-variant" ->
          read_descriptor_value Declaration.read_property_value
            (fun v -> Font_variant v)
            r
      | "font-feature-settings" ->
          read_descriptor_value Declaration.read_property_value
            (fun v -> Font_feature_settings v)
            r
      | "font-variation-settings" ->
          read_descriptor_value Declaration.read_property_value
            (fun v -> Font_variation_settings v)
            r
      | "size-adjust" ->
          read_descriptor_value Declaration.read_property_value
            (fun v -> Size_adjust (Font_face.size_adjust_of_string v))
            r
      | "ascent-override" ->
          read_descriptor_value Declaration.read_property_value
            (fun v -> Ascent_override (Font_face.metric_override_of_string v))
            r
      | "descent-override" ->
          read_descriptor_value Declaration.read_property_value
            (fun v -> Descent_override (Font_face.metric_override_of_string v))
            r
      | "line-gap-override" ->
          read_descriptor_value Declaration.read_property_value
            (fun v -> Line_gap_override (Font_face.metric_override_of_string v))
            r
      | _ -> Cursor.err_invalid r ("unknown font-face descriptor: " ^ name)
    in
    Cursor.ws r;
    if Cursor.peek_semicolon r then Cursor.skip r;
    Some descriptor

let read_font_face_block inner =
  let rec read_descriptors acc =
    match read_font_face_descriptor inner with
    | Some desc -> read_descriptors (desc :: acc)
    | None ->
        Cursor.ws inner;
        if Cursor.is_done inner then List.rev acc else read_descriptors acc
  in
  read_descriptors []

let read_font_face (r : Cursor.t) : statement =
  Cursor.with_context r "@font-face" @@ fun () ->
  Cursor.expect_at_keyword "font-face" r;
  Cursor.ws r;
  let descriptors = Cursor.braces read_font_face_block r in
  Font_face descriptors

let read_page (r : Cursor.t) : statement =
  Cursor.with_context r "@page" @@ fun () ->
  Cursor.expect_at_keyword "page" r;
  Cursor.ws r;
  let selector =
    let s = Cursor.drain_until_block_to_string ~trim:true r in
    if s = "" then None else Some s
  in
  let declarations =
    Cursor.braces (fun inner -> Declaration.read_declarations inner) r
  in
  Page (selector, declarations)

type property_reader_state = {
  syntax : Variables.any_syntax option;
  inherits : bool option;
  initial_value : string option;
}

let rec read_statement (r : Cursor.t) : statement =
  Cursor.ws r;
  let table : (string * (Cursor.t -> statement)) list =
    [
      ("charset", read_charset);
      ("import", read_import);
      ("namespace", read_namespace);
      ("layer", read_layer);
      ("media", read_media);
      ("container", read_container);
      ("supports", read_supports);
      ("starting-style", read_starting_style);
      ("scope", read_scope);
      ("keyframes", read_keyframes);
      ("font-face", read_font_face);
      ("page", read_page);
      ("property", read_property_rule);
    ]
  in
  match Cursor.peek r with
  | Some (Component.Preserved { kind = Token.At_keyword name; _ }) -> (
      match List.assoc_opt name table with
      | Some p -> p r
      | None -> Rule (read_rule r))
  | _ -> Rule (read_rule r)

and read_block (r : Cursor.t) : block =
  let rec read_statements acc =
    Cursor.ws r;
    if Cursor.is_done r then List.rev acc
    else
      let stmt = read_statement r in
      read_statements (stmt :: acc)
  in
  read_statements []

and read_starting_style (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "starting-style" r;
  Cursor.ws r;
  let content = Cursor.braces (fun inner -> read_nesting_block inner) r in
  Starting_style content

and read_media (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "media" r;
  Cursor.ws r;
  let condition_str = Cursor.drain_until_block_to_string ~trim:true r in
  if String.length condition_str = 0 then
    Cursor.err r "@media rule requires a media query condition";
  let content = Cursor.braces (fun inner -> read_block inner) r in
  Media (Media.Raw condition_str, content)

and read_supports (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "supports" r;
  Cursor.ws r;
  let condition = Cursor.drain_until_block_to_string ~trim:true r in
  if String.length condition = 0 then
    Cursor.err r "@supports rule requires a condition";
  let content = Cursor.braces (fun inner -> read_block inner) r in
  Supports (Supports.of_string condition, content)

and read_scope (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "scope" r;
  Cursor.ws r;
  ignore (Cursor.drain_until_block r : Component.t list);
  let content = Cursor.braces (fun inner -> read_block inner) r in
  Scope (None, None, content)

and read_container (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "container" r;
  Cursor.ws r;
  let container_name = Cursor.option Cursor.ident r in
  Cursor.ws r;
  let condition_str = Cursor.drain_until_block_to_string ~trim:true r in
  let content = Cursor.braces (fun inner -> read_block inner) r in
  Container (container_name, Container.Raw condition_str, content)

and read_layer (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "layer" r;
  Cursor.ws r;
  match Cursor.peek r with
  | Some (Component.Block { node = { opening = Token.Curly; _ }; _ }) ->
      (* Anonymous layer *)
      let content = Cursor.braces (fun inner -> read_block inner) r in
      Layer (None, content)
  | _ -> (
      let first = Cursor.ident ~keep_case:true r in
      Cursor.ws r;
      match Cursor.peek r with
      | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
          Cursor.skip r;
          Layer_decl [ first ]
      | Some (Component.Preserved { kind = Token.Comma; _ }) ->
          Cursor.skip r;
          Cursor.ws r;
          let rest =
            Cursor.list ~sep:Cursor.comma ~at_least:1
              (fun r ->
                Cursor.ws r;
                Cursor.ident ~keep_case:true r)
              r
          in
          Cursor.ws r;
          if Cursor.peek_semicolon r then Cursor.skip r;
          Layer_decl (first :: rest)
      | Some (Component.Block { node = { opening = Token.Curly; _ }; _ }) ->
          let content = Cursor.braces (fun inner -> read_block inner) r in
          Layer (Some first, content)
      | _ -> Cursor.err_invalid r "expected ';' or '{' after @layer name")

(* Helper: Read declarations until closing brace *)
and _read_declarations_block (r : Cursor.t) : Declaration.declaration list =
  let rec loop acc =
    Cursor.ws r;
    if Cursor.is_done r then List.rev acc
    else
      match Declaration.read_declaration r with
      | Some d ->
          Cursor.ws r;
          if Cursor.peek_semicolon r then Cursor.skip r;
          loop (d :: acc)
      | None ->
          (* If we can't read a declaration, check if we're at the end *)
          List.rev acc
  in
  loop []

(* Helper: Read a block that can contain either bare declarations or statements.
   Used for CSS nesting contexts where content inside @media/@supports/etc can
   be either bare declarations (inheriting the parent selector) or nested
   rules. *)
and read_nesting_block (r : Cursor.t) : block =
  let rec read_items acc =
    Cursor.ws r;
    match Cursor.peek r with
    | None -> List.rev acc
    | Some (Component.Preserved { kind = Token.At_keyword _; _ }) ->
        let stmt = read_statement r in
        read_items (stmt :: acc)
    | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
        Cursor.skip r;
        read_items acc
    | _ -> (
        match Declaration.read_declaration r with
        | Some decl ->
            Cursor.ws r;
            let rec read_more_decls acc =
              match Cursor.peek r with
              | Some (Component.Preserved { kind = Token.Semicolon; _ }) -> (
                  Cursor.skip r;
                  Cursor.ws r;
                  match Cursor.peek r with
                  | None -> List.rev acc
                  | Some (Component.Preserved { kind = Token.At_keyword _; _ })
                    ->
                      List.rev acc
                  | _ -> (
                      match Declaration.read_declaration r with
                      | Some d ->
                          Cursor.ws r;
                          read_more_decls (d :: acc)
                      | None -> List.rev acc))
              | _ -> List.rev acc
            in
            let all_decls = read_more_decls [ decl ] in
            let stmt = Declarations all_decls in
            read_items (stmt :: acc)
        | None ->
            let stmt = read_statement r in
            read_items (stmt :: acc))
  in
  read_items []

(* Helper: Read nested at-rule with declarations content *)
and read_nested_at_rule (r : Cursor.t) (at_rule : string)
    (_selector : Selector.t) : statement =
  Cursor.with_context r at_rule @@ fun () ->
  let name = String.sub at_rule 1 (String.length at_rule - 1) in
  Cursor.expect_at_keyword name r;
  Cursor.ws r;
  match at_rule with
  | "@container" ->
      let container_name = Cursor.option Cursor.ident r in
      Cursor.ws r;
      let condition_str = Cursor.drain_until_block_to_string ~trim:true r in
      let content = Cursor.braces (fun inner -> read_nesting_block inner) r in
      Container (container_name, Container.Raw condition_str, content)
  | "@supports" ->
      let condition = Cursor.drain_until_block_to_string ~trim:true r in
      let content = Cursor.braces (fun inner -> read_nesting_block inner) r in
      Supports (Supports.of_string condition, content)
  | "@media" ->
      let condition_str = Cursor.drain_until_block_to_string ~trim:true r in
      let content = Cursor.braces (fun inner -> read_nesting_block inner) r in
      Media (Media.Raw condition_str, content)
  | _ -> Cursor.err_invalid r ("Unexpected nested at-rule: " ^ at_rule)

and read_nested_at_within_rule (r : Cursor.t) (selector : Selector.t) :
    statement =
  match Cursor.peek r with
  | Some (Component.Preserved { kind = Token.At_keyword name; _ })
    when name = "supports" || name = "media" || name = "container" ->
      read_nested_at_rule r ("@" ^ name) selector
  | Some (Component.Preserved { kind = Token.At_keyword "layer"; _ }) -> (
      Cursor.expect_at_keyword "layer" r;
      Cursor.ws r;
      match Cursor.peek r with
      | Some (Component.Block { node = { opening = Token.Curly; _ }; _ }) ->
          let content = Cursor.braces (fun inner -> read_block inner) r in
          Layer (None, content)
      | _ ->
          let name = Cursor.ident ~keep_case:true r in
          Cursor.ws r;
          let content = Cursor.braces (fun inner -> read_block inner) r in
          Layer (Some name, content))
  | _ -> read_statement r

and read_rule_selector r =
  let prelude = Cursor.drain_until_block r in
  let c = Cursor.subcursor r prelude in
  try Selector.read_selector_list c
  with Error.Parse_error e -> Cursor.err r (Error.to_string e)

and read_rule_item selector inner decls nested =
  match Cursor.peek inner with
  | None -> `Done (List.rev decls, List.rev nested)
  | Some (Component.Preserved { kind = Token.At_keyword _; _ }) ->
      let stmt = read_nested_at_within_rule inner selector in
      `Continue (decls, stmt :: nested)
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
      Cursor.skip inner;
      `Continue (decls, nested)
  | _ -> (
      match Declaration.read_declaration inner with
      | Some d ->
          Cursor.ws inner;
          if Cursor.peek_semicolon inner then Cursor.skip inner;
          `Continue (d :: decls, nested)
      | None ->
          if Cursor.is_done inner then `Done (List.rev decls, List.rev nested)
          else
            let nr = read_rule inner in
            `Continue (decls, Rule nr :: nested))

and read_rule_body selector inner =
  let rec loop decls nested =
    Cursor.ws inner;
    if Cursor.recover inner then (
      match
        try Ok (read_rule_item selector inner decls nested)
        with Error.Parse_error e -> Error e
      with
      | Ok (`Done result) -> result
      | Ok (`Continue (decls, nested)) -> loop decls nested
      | Error e ->
          (* Per 5.4.4, an invalid declaration is discarded; the enclosing rule
             survives. Push the warning on the cursor so the top-level
             [read_stylesheet_from_rules] drain sees it, and skip to the next
             [;] or end of block. *)
          Cursor.push_warning inner e;
          let rec skip () =
            match Cursor.next_raw inner with
            | None -> ()
            | Some (Component.Preserved { kind = Token.Semicolon; _ }) -> ()
            | Some _ -> skip ()
          in
          skip ();
          loop decls nested)
    else
      match read_rule_item selector inner decls nested with
      | `Done result -> result
      | `Continue (decls, nested) -> loop decls nested
  in
  loop [] []

and read_rule (r : Cursor.t) : rule =
  Cursor.with_context r "rule" @@ fun () ->
  let selector = read_rule_selector r in
  let declarations, nested = Cursor.braces (read_rule_body selector) r in
  { selector; declarations; nested; merge_key = None }

and read_property_rule (r : Cursor.t) : statement =
  (* Read @property descriptors as a separate helper to keep the reader tidy. *)
  Cursor.expect_at_keyword "property" r;
  Cursor.ws r;
  let name = Cursor.ident ~keep_case:true r in
  Cursor.ws r;
  let state = Cursor.braces (fun inner -> read_property_descriptors inner) r in
  match (state.syntax, state.inherits) with
  | None, _ ->
      Cursor.err_invalid r "@property: missing required 'syntax' descriptor"
  | _, None ->
      Cursor.err_invalid r "@property: missing required 'inherits' descriptor"
  | Some (Variables.Syntax syntax), Some inherits ->
      let is_universal_syntax =
        match syntax with Universal -> true | _ -> false
      in
      let initial_value =
        match state.initial_value with
        | None when not is_universal_syntax ->
            Cursor.err_invalid r
              "@property: initial-value is required for non-universal syntax"
        | None -> None
        | Some str ->
            let value_reader = Cursor.of_string str in
            Some (Variables.read_value value_reader syntax)
      in
      Property { name; syntax; inherits; initial_value }

and read_property_descriptors (r : Cursor.t) : property_reader_state =
  let state = ref { syntax = None; inherits = None; initial_value = None } in
  let rec loop () =
    Cursor.ws r;
    if Cursor.is_done r then !state
    else
      let key = Cursor.ident ~keep_case:false r in
      Cursor.ws r;
      if not (Cursor.colon r) then Cursor.err_expected r "':'";
      Cursor.ws r;
      (match key with
      | "syntax" ->
          let syn = Variables.read_syntax r in
          state := { !state with syntax = Some syn }
      | "inherits" ->
          let inherits_value = Cursor.bool r in
          state := { !state with inherits = Some inherits_value }
      | "initial-value" ->
          let value_str = Cursor.consume_to_semicolon ~trim:true r in
          state := { !state with initial_value = Some value_str }
      | _ -> Cursor.err_invalid r "unknown property descriptor");
      Cursor.ws r;
      if Cursor.peek_semicolon r then Cursor.skip r;
      loop ()
  in
  loop ()

let read_stylesheet (r : Cursor.t) : stylesheet =
  Cursor.with_context r "stylesheet" (fun () ->
      let rec read_statements acc =
        Cursor.ws r;
        if Cursor.is_done r then List.rev acc
        else
          let stmt = read_statement r in
          read_statements (stmt :: acc)
      in
      read_statements [])

(* Replay a Parser-recovered rule as a Cursor by flattening its components.
   [Component.at_rule] stores the at-keyword as a bare [name] string; the
   original [At_keyword] token was already consumed during section 5.3, so
   synthesize one at the rule's location so dispatch in [read_statement] still
   sees the opening [\@name]. [?source] flows through so errors raised by
   validators downstream pick up source-context snippets. *)
let cursor_of_rule ?source ?meta : Component.rule -> Cursor.t = function
  | Qualified { node = { prelude; block }; _ } ->
      Cursor.of_components ?source ?meta ~recover:true
        (prelude @ [ Component.Block block ])
  | At { node = { name; prelude; block }; loc } ->
      let at_kw = Token.v ~kind:(Token.At_keyword name) ~loc in
      let at_cv = Component.Preserved at_kw in
      let block_cv =
        match block with Some b -> [ Component.Block b ] | None -> []
      in
      Cursor.of_components ?source ?meta ~recover:true
        ((at_cv :: prelude) @ block_cv)

(* Validate one Parser-recovered rule to a typed statement, or convert the
   validator's [Parse_error] into a rule-level error and drop the rule. Per-
   declaration warnings accumulated on the cursor are drained separately by the
   caller. *)
let read_statement_from_rule ?source ?meta (rule : Component.rule) :
    Cursor.t * (statement, Error.t) result =
  let r = cursor_of_rule ?source ?meta rule in
  let result =
    try Ok (read_statement r) with Error.Parse_error e -> Error e
  in
  (r, result)

let read_stylesheet_from_rules ?source ?meta (rules : Component.rule list) :
    stylesheet * Error.t list =
  let warnings = ref [] in
  let statements =
    List.filter_map
      (fun rule ->
        let cursor, result = read_statement_from_rule ?source ?meta rule in
        (* Drain declaration-level warnings first so source order is preserved:
           decl warnings come from inside the rule, the rule-level error (if
           any) comes after them. *)
        List.iter
          (fun w -> warnings := w :: !warnings)
          (Cursor.drain_warnings cursor);
        match result with
        | Ok stmt -> Some stmt
        | Error e ->
            warnings := e :: !warnings;
            None)
      rules
  in
  (statements, List.rev !warnings)

(* Top-level partial-recovery entry point: combine section 5.3 syntax warnings
   from [Parser.parse_stylesheet] with per-rule typed-validation warnings. *)
let parse_stylesheet_partial ?(meta = Loc.default_meta_level) (source : string)
    : stylesheet * Error.t list =
  let reader = Reader.of_string source in
  let out = Parser.parse_stylesheet ~meta reader in
  let sheet, typed_warnings =
    read_stylesheet_from_rules ~source ~meta out.value
  in
  (sheet, out.warnings @ typed_warnings)

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
    {
      Pp.minify = config.minify;
      indent = 0;
      buf;
      inline = mode = Inline;
      in_function = false;
      theme = None;
      theme_defaults = Pp.no_theme_defaults;
    }
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
  | Declarations decls -> Variables.vars_of_declarations decls
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
      Pp.string ctx (Supports.to_string s);
      Pp.char ctx ')')
    supports;
  Option.iter
    (fun m ->
      Pp.space ctx ();
      Media.pp ctx m)
    media;
  Pp.string ctx ";"

(* Reader for import_rule *)
let read_import_rule (r : Cursor.t) : import_rule =
  Cursor.ws r;
  Cursor.expect_at_keyword "import" r;
  Cursor.ws r;
  let url = Cursor.one_of [ Cursor.url; Cursor.string ] r in
  Cursor.ws r;
  let layer =
    Cursor.function_call "layer"
      (fun inner -> Cursor.remaining_to_string ~trim:true inner)
      r
  in
  Cursor.ws r;
  let supports =
    Cursor.function_call "supports"
      (fun inner ->
        Supports.of_string (Cursor.remaining_to_string ~trim:true inner))
      r
  in
  Cursor.ws r;
  let media =
    if Cursor.peek_semicolon r || Cursor.is_done r then None
    else Some (Media.Raw (Cursor.consume_to_semicolon ~trim:true r))
  in
  if Cursor.peek_semicolon r then Cursor.skip r;
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
let read_config (r : Cursor.t) : config =
  Cursor.braces
    (fun inner ->
      let minify = ref false in
      let mode = ref Variables in
      let optimize = ref false in
      let newline = ref false in
      let rec loop () =
        if Cursor.is_done inner then ()
        else
          let field_name = Cursor.ident inner in
          Cursor.expect '=' inner;
          let value = Cursor.ident inner in
          (match field_name with
          | "minify" -> minify := value = "true"
          | "mode" -> mode := if value = "Inline" then Inline else Variables
          | "optimize" -> optimize := value = "true"
          | "newline" -> newline := value = "true"
          | _ -> ());
          if Cursor.peek_semicolon inner then Cursor.skip inner;
          loop ()
      in
      loop ();
      {
        minify = !minify;
        mode = !mode;
        optimize = !optimize;
        newline = !newline;
      })
    r
