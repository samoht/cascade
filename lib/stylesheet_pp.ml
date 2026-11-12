open Stylesheet_intf
open Stylesheet_walk

(** {1 Pretty Printing} *)

let pp_property_rule : 'a property_rule Pp.t =
 fun ctx { name; syntax; inherits; initial_value } ->
  let is_empty_universal_initial : type a.
      a Variables.syntax -> a option -> bool =
   fun syntax initial_value ->
    match (syntax, initial_value) with
    | Variables.Universal, Some "" -> true
    | _ -> false
  in
  let pp_initial_value ctx v =
    Pp.semicolon ctx ();
    Pp.cut ctx ();
    Pp.indent
      (fun ctx () ->
        Pp.string ctx "initial-value:";
        Pp.space_if_pretty ctx ();
        Variables.pp_value syntax ctx v)
      ctx ()
  in
  let pp_initial_value_opt ctx =
    if Pp.minified ctx && is_empty_universal_initial syntax initial_value then
      ()
    else
      match initial_value with None -> () | Some v -> pp_initial_value ctx v
  in
  Pp.string ctx "@property ";
  (* CSS Syntax 3 (ED) sec. 4.3.7: the prelude names the custom property this
     rule registers, so it is written with the escapes that read the same name
     back (see [Properties.pp_property]). *)
  Pp.string ctx (Parser.escape_ident name);
  Pp.sp ctx ();
  Pp.braces
    (fun ctx () ->
      Pp.string ctx "syntax:";
      Pp.space_if_pretty ctx ();
      Variables.pp_syntax ctx syntax;
      Pp.string ctx ";";
      Pp.cut ctx ();
      Pp.indent
        (fun ctx () ->
          Pp.string ctx "inherits:";
          Pp.space_if_pretty ctx ();
          Pp.string ctx (if inherits then "true" else "false"))
        ctx ();
      pp_initial_value_opt ctx;
      if not ctx.Pp.minify then Pp.semicolon ctx ())
    ctx ()

(* CSS Animations 1 sec. 3 [<keyframes-name>] is [<custom-ident> | <string>].
   The two arms give the same name, so the reader keeps the name alone and sec.
   3 settles the arm on output: "the value is serialized as an <ident> unless
   it's a disallowed keyword, in which case it's serialized as a <string>". A
   name an escape would rewrite takes the string too, which is no longer. *)
let keyframes_name_reserved name =
  Cursor.is_reserved_custom_ident ~reserved:[ "none" ] name

let pp_keyframes_name ctx name =
  let len = String.length name in
  let is_ident_continue c =
    (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9')
    || c = '-' || c = '_'
  in
  let is_ident_start c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
  in
  let is_safe_ident =
    len > 0
    && (is_ident_start name.[0]
       || name.[0] = '-'
          && len >= 2
          && (is_ident_start name.[1] || name.[1] = '-'))
    &&
    let ok = ref true in
    String.iter (fun c -> if not (is_ident_continue c) then ok := false) name;
    !ok
  in
  if is_safe_ident && not (keyframes_name_reserved name) then Pp.string ctx name
  else Pp.quoted_string ctx name

let pp_keyframe_position : Keyframe.position Pp.t =
 fun ctx pos ->
  (* CSS Animations 1 7.1: [from] is equivalent to [0%] and [to] to [100%].
     Under minification, canonicalise to the shorter spelling. *)
  match (pos, Pp.minified ctx) with
  | Keyframe.From, true -> Pp.string ctx "0%"
  | Keyframe.Percent 100., true -> Pp.string ctx "to"
  | _ -> Pp.string ctx (Keyframe.string_of_position pos)

let pp_keyframe_selector : Keyframe.selector Pp.t =
 fun ctx -> function
  | Keyframe.Positions positions ->
      Pp.list ~sep:Pp.comma pp_keyframe_position ctx positions

let pp_keyframe : keyframe Pp.t =
 fun ctx kf ->
  pp_keyframe_selector ctx kf.selector;
  Pp.sp ctx ();
  Pp.braced_semicolon_list Declaration.pp ctx kf.declarations

let pp_keyframes_block_statement ctx header name frames =
  Pp.string ctx header;
  pp_keyframes_name ctx name;
  Pp.sp ctx ();
  (* Sibling frame blocks separate with a blank line, like statements in any
     other block (see [pp_block]). *)
  Pp.braced_list ~sep:Pp.(cut ++ cut) pp_keyframe ctx frames

let pp_page_pseudo ctx p =
  Pp.char ctx ':';
  Pp.string ctx
    (match p with
    | First -> "first"
    | Left -> "left"
    | Right -> "right"
    | Blank -> "blank")

let pp_page_selector_one ctx ({ name; pseudos } : page_selector) =
  Option.iter (Pp.string ctx) name;
  List.iter (pp_page_pseudo ctx) pseudos

let pp_page_selector ctx (selectors : page_selector list) =
  match selectors with
  | [] -> ()
  | first :: _ as selectors ->
      (* A named selector needs a hard space after [@page]; a pseudo-only one
         takes a collapsible space ([@page:first] minifies cleanly). *)
      (match (first : page_selector).name with
      | Some _ -> Pp.space ctx ()
      | None -> Pp.sp ctx ());
      Pp.list ~sep:Pp.comma pp_page_selector_one ctx selectors

let pp_page_margin_rule : page_margin_rule Pp.t =
 fun ctx rule ->
  Pp.string ctx "@";
  Pp.string ctx rule.name;
  Pp.sp ctx ();
  Pp.braced_semicolon_list Declaration.pp ctx rule.descriptors

let pp_page_with_margins_body ctx descriptors margins =
  Pp.braces
    (fun ctx () ->
      Pp.list ~sep:Pp.semicolon_cut
        (fun ctx (i, d) ->
          if i = 0 then Declaration.pp ctx d else Pp.indent Declaration.pp ctx d)
        ctx
        (List.mapi (fun i d -> (i, d)) descriptors);
      if descriptors <> [] && margins <> [] then (
        Pp.semicolon ctx ();
        Pp.cut ctx ())
      else if descriptors <> [] && not ctx.Pp.minify then Pp.semicolon ctx ();
      Pp.list ~sep:Pp.cut
        (fun ctx (i, m) ->
          if i = 0 && descriptors = [] then pp_page_margin_rule ctx m
          else Pp.indent pp_page_margin_rule ctx m)
        ctx
        (List.mapi (fun i m -> (i, m)) margins))
    ctx ()

let pp_scope_selector ctx s = Selector.pp ctx s

let pp_viewport_prefix_keyword = function
  | Standard -> "@viewport"
  | Ms_prefixed -> "@-ms-viewport"

let pp_viewport_descriptor ctx { name; value } =
  Pp.string ctx name;
  Pp.char ctx ':';
  Pp.string ctx value

let pp_viewport_statement ctx prefix descriptors =
  Pp.string ctx (pp_viewport_prefix_keyword prefix);
  Pp.sp ctx ();
  Pp.braced_semicolon_list pp_viewport_descriptor ctx descriptors

let trim_unknown_at_prelude prelude =
  let rec trim_end i =
    if i < 0 then 0
    else
      match prelude.[i] with
      | ';' | '}' | ' ' | '\t' | '\n' | '\r' -> trim_end (i - 1)
      | _ -> i + 1
  in
  let n = String.length prelude in
  String.sub prelude 0 (trim_end (n - 1))

(* CSS Syntax 3 (ED) sec. 4.3.1: a backslash starts an escape unless a newline
   follows it. A raw body ending on an odd run of backslashes therefore escapes
   the closer written straight after it, and the at-rule swallows whatever comes
   next instead of ending. *)
let body_escapes_its_closer body =
  let rec backslashes i n =
    if i < 0 || body.[i] <> '\\' then n else backslashes (i - 1) (n + 1)
  in
  backslashes (String.length body - 1) 0 land 1 = 1

let pp_unknown_at_rule_statement ctx name prelude (block : string option) =
  (* CSS Syntax 3 (ED) section 5.5.2: an at-rule terminates on [;], [}], or EOF.
     When the Parser captures an unterminated nested block ([(...], [[...],
     [{...]) its source slice can include the at-rule terminator or the
     enclosing block's close - don't echo them as part of the prelude or each
     round-trip stacks another [;]/[}]. *)
  let prelude = trim_unknown_at_prelude prelude in
  Pp.char ctx '@';
  (* CSS Syntax 3 (ED) section 9.1: an at-rule name with non-ident-continue code
     points (control chars, escapes, non-ASCII) must round-trip through
     [escape_ident] so the serialized [\6 T] re-tokenizes back to the same
     at-keyword token. *)
  Pp.string ctx (Parser.escape_ident name);
  (* CSS Syntax 3 (ED) sec. 4.3.1: the at-keyword consumes an ident sequence, so
     the whitespace before the prelude is the only thing keeping them apart. It
     is a hard space, not layout - minifying [@foo bar] to [@foobar] names a
     different at-rule. *)
  if prelude <> "" then (
    Pp.space ctx ();
    Pp.string ctx prelude);
  match block with
  | None -> Pp.semicolon ctx ()
  | Some body ->
      Pp.sp ctx ();
      Pp.char ctx '{';
      Pp.string ctx body;
      (* Sec. 4.3.7 reads a space or a hex digit as part of the escape, so a
         newline is the only separator that leaves the last backslash the delim
         token it was. *)
      if body_escapes_its_closer body then Pp.char ctx '\n';
      Pp.char ctx '}'

let font_face_participates descriptors =
  List.exists (function Font_family _ -> true | _ -> false) descriptors
  && List.exists (function Src _ -> true | _ -> false) descriptors

let should_print_statement ctx = function
  | Font_face descriptors when Pp.minified ctx ->
      font_face_participates descriptors
  | _ -> true

let printable_statements ctx statements =
  if Pp.minified ctx then List.filter (should_print_statement ctx) statements
  else statements

module String_set = Pp.String_set

let normalise_charset statements =
  (* CSS Syntax 3 (ED) sec. 8.3: [@charset] is an encoding-declaration byte
     pattern recognised before tokenization, not a stylesheet at-rule after
     parsing. In minified output the serializer emits UTF-8, so [@charset
     "UTF-8"] is redundant; keep at most the first non-UTF-8 declaration for
     byte-level compatibility. *)
  let is_utf8 encoding =
    String.equal (String.lowercase_ascii encoding) "utf-8"
  in
  let kept_one = ref false in
  List.filter
    (fun stmt ->
      match stmt with
      | Charset enc when is_utf8 enc -> false
      | Charset _ when !kept_one -> false
      | Charset _ ->
          kept_one := true;
          true
      | _ -> true)
    statements

let color_custom_property_names stylesheet =
  let var_name_of_custom_property = Custom_property_name.strip_prefix in
  let declaration names = function
    | Declaration.Declaration
        {
          property = Properties.Custom_property name;
          value =
            Properties.Custom_value
              { value = Properties.Typed { kind = Properties.Color; _ }; _ };
          _;
        } ->
        String_set.add (var_name_of_custom_property name) names
    | _ -> names
  in
  let rec statement names stmt =
    let names =
      List.fold_left declaration names (statement_declarations stmt)
    in
    List.fold_left statement names (statement_children stmt)
  in
  List.fold_left statement String_set.empty stylesheet

let color_fallback_of_length_fallback :
    Values.length Values.fallback -> Values.color Values.fallback = function
  | Values.None -> Values.None
  | Values.Empty -> Values.Empty
  | Values.Empty2 -> Values.Empty2
  | Values.Syntax_fallback components -> Values.Syntax_fallback components
  | Values.Var_fallback name -> Values.Var_fallback name
  | Values.Fallback value ->
      Values.Syntax_fallback
        (Cursor.remaining
           (Cursor.of_string (Pp.to_string ~minify:true Values.pp_length value)))

(* The shadow rewrite uses the [var(--ring)] reference as the colour slot of a
   [box-shadow]; the inline [default] (a length) doesn't apply when the var is
   consumed in colour position, so drop it. *)
let color_var_of_length_var (var : Values.length Values.var) :
    Values.color Values.var =
  {
    name = var.name;
    fallback = color_fallback_of_length_fallback var.fallback;
    default = None;
    layer = var.layer;
    meta = var.meta;
    runtime = var.runtime;
  }

let rec rewrite_shadow_value color_vars (value : Properties.shadow) :
    Properties.shadow =
  match value with
  | Properties.Shadow
      ({ blur = Some (Values.Var var); spread = None; color = None; _ } as
       shadow)
    when String_set.mem var.name color_vars ->
      Properties.Shadow
        {
          shadow with
          blur = Some Values.Zero;
          color = Some (Values.Var (color_var_of_length_var var));
        }
  | Properties.List shadows ->
      Properties.List (List.map (rewrite_shadow_value color_vars) shadows)
  | shadow -> shadow

let rewrite_shadow_decl color_vars = function
  | Declaration.Declaration
      { property = Properties.Box_shadow as property; value; important; _ } as
    decl ->
      let value' = rewrite_shadow_value color_vars value in
      if value' == value then decl else Declaration.v ~important property value'
  | Declaration.Declaration
      {
        property = Properties.Custom_property _ as property;
        value =
          Properties.Custom_value
            ({
               value =
                 Properties.Typed { kind = Properties.Shadow; value = shadow };
               _;
             } as custom_value);
        important;
        _;
      } as decl ->
      let shadow' = rewrite_shadow_value color_vars shadow in
      if shadow' == shadow then decl
      else
        Declaration.v ~important property
          (Properties.Custom_value
             {
               custom_value with
               value =
                 Properties.Typed { kind = Properties.Shadow; value = shadow' };
             })
  | decl -> decl

let normalise_shadows stylesheet =
  let color_vars = color_custom_property_names stylesheet in
  if String_set.is_empty color_vars then stylesheet
  else
    let declarations = List.map (rewrite_shadow_decl color_vars) in
    let rec statement stmt =
      stmt
      |> map_statement_declarations declarations
      |> map_statement_children (List.map statement)
    in
    List.map statement stylesheet

let normalise statements = statements |> normalise_charset |> normalise_shadows

let import_url_starts_with_url url len =
  len >= 4 && String.lowercase_ascii (String.sub url 0 4) = "url("

let import_url_unwrap_quoted url len =
  if len >= 2 && (url.[0] = '"' || url.[0] = '\'') && url.[len - 1] = url.[0]
  then Some (String.sub url 1 (len - 2))
  else None

let import_url_strip_inner_quotes inner : string option =
  let inner_len = String.length inner in
  if inner_len = 0 then None
  else if
    inner_len >= 2
    && (inner.[0] = '"' || inner.[0] = '\'')
    && inner.[inner_len - 1] = inner.[0]
  then Some (String.sub inner 1 (inner_len - 2))
  else Some inner

let import_url_inner_string url len : string option =
  (* Lift the inner string out of [url(...)] - either the quoted form
     [url("foo")] / [url('foo')] or the bare form [url(foo)]. Returns the
     unquoted body so the [@import] / [@namespace] minify path can re-emit it as
     a bare double-quoted string. *)
  if not (import_url_starts_with_url url len) then None
  else if len < 5 || url.[len - 1] <> ')' then None
  else
    let inner = String.sub url 4 (len - 5) |> String.trim in
    import_url_strip_inner_quotes inner

let pp_import_url_minified ctx url len =
  (* Canonicalise to the shortest spec-equivalent form: a double-quoted string
     (CSS Syntax 3 (ED) sec. 4.3.5 prefers double quotes). The [url()] wrapping
     is five characters of overhead that the bare-string form omits per CSS
     Conditional Rules 3, and a single-quoted source string re-emits with double
     quotes. *)
  match import_url_inner_string url len with
  | Some s -> Pp.quoted_string ctx s
  | None -> (
      match import_url_unwrap_quoted url len with
      | Some s -> Pp.quoted_string ctx s
      | None ->
          if len > 0 && import_url_starts_with_url url len then
            Pp.string ctx url
          else Pp.quoted_string ctx url)

let pp_import_url ctx url =
  let len = String.length url in
  if Pp.minified ctx then pp_import_url_minified ctx url len
  else if
    len > 0
    && (url.[0] = '"' || url.[0] = '\'' || import_url_starts_with_url url len)
  then Pp.string ctx url
  else Pp.quoted_string ctx url

(* CSS Cascade 5 sec. 6.4.1: a [<layer-name>] is [<ident> ['.' <ident>]*], so
   each ident takes the escapes CSS Syntax 3 (ED) sec. 4.3.7 needs (see
   [Properties.pp_property]) and the [.] separators stay bare. A [.] an ident
   carries is escaped, and so never reads back as a separator. *)
let string_of_layer_name name =
  String.concat "." (List.map Parser.escape_ident name)

let pp_layer_name : layer_name Pp.t =
 fun ctx name -> Pp.string ctx (string_of_layer_name name)

let pp_import_layer ctx layer =
  Pp.sp ctx ();
  if layer = [] then Pp.string ctx "layer"
  else (
    Pp.string ctx "layer(";
    pp_layer_name ctx layer;
    Pp.char ctx ')')

let pp_import_supports ctx supports =
  Pp.sp ctx ();
  Pp.string ctx "supports(";
  (match supports with
  | Supports.Property decl -> Supports.pp_declaration_feature ctx decl
  | _ -> Supports.pp ctx supports);
  Pp.char ctx ')'

let starts_with s prefix =
  let s_len = String.length s and prefix_len = String.length prefix in
  s_len >= prefix_len && String.sub s 0 prefix_len = prefix

let import_supports_media_needs_space supports media =
  match supports with
  | Supports.Property decl ->
      let supports =
        Pp.to_string ~minify:true Supports.pp_declaration_feature decl
      in
      let media = Media.to_string ~minify:true media in
      starts_with supports "--" && starts_with media "("
  | _ -> false

let pp_import_components ctx { url; layer; supports; media } =
  Pp.sp ctx ();
  pp_import_url ctx url;
  Option.iter (pp_import_layer ctx) layer;
  Option.iter (pp_import_supports ctx) supports;
  Option.iter
    (fun media ->
      (match supports with
      | Some supports when import_supports_media_needs_space supports media ->
          Pp.space ctx ()
      | None when layer = Some [] -> Pp.space ctx ()
      | _ -> Pp.sp ctx ());
      Media.pp ctx media)
    media

let strip_outer_parens s =
  let s = String.trim s in
  let len = String.length s in
  if len >= 2 && s.[0] = '(' && s.[len - 1] = ')' then String.sub s 1 (len - 2)
  else s

let pp_condition_function ctx name rendered =
  Pp.string ctx name;
  Pp.char ctx '(';
  Pp.string ctx (strip_outer_parens rendered);
  Pp.char ctx ')'

let rec pp_conditional : conditional Pp.t =
 fun ctx -> function
  | Media_condition condition ->
      pp_condition_function ctx "media"
        (Pp.to_string ~minify:ctx.Pp.minify ~lossless:ctx.Pp.lossless Media.pp
           condition)
  | Supports_condition_test condition ->
      pp_condition_function ctx "supports"
        (Pp.to_string ~minify:ctx.Pp.minify ~lossless:ctx.Pp.lossless
           Supports.pp condition)
  | And (a, b) ->
      pp_conditional ctx a;
      Pp.string ctx " and ";
      pp_conditional ctx b
  | Or (a, b) ->
      pp_conditional ctx a;
      Pp.string ctx " or ";
      pp_conditional ctx b

(* Every argument but the [<url>] one is a [<string>], so it stays quoted; the
   [<url>] takes the shortest spelling [Pp.url] gives it. *)
let pp_moz_document_condition ctx =
  let call name arg =
    Pp.string ctx name;
    Pp.char ctx '(';
    Pp.quoted_string ctx arg;
    Pp.char ctx ')'
  in
  function
  | Url_exact url -> Pp.url ctx url
  | Url_prefix None -> Pp.string ctx "url-prefix()"
  | Url_prefix (Some prefix) -> call "url-prefix" prefix
  | Domain domain -> call "domain" domain
  | Media_document media -> call "media-document" media
  | Regexp regexp -> call "regexp" regexp

let pp_declarations_statement ctx raw_decls =
  (* Bare declarations for CSS nesting - no selector/braces, just declarations.
     No extra indent since the containing block handles it *)
  let decls = raw_decls in
  Pp.list ~sep:Pp.semicolon_cut Declaration.pp ctx decls;
  if decls <> [] && not ctx.Pp.minify then Pp.semicolon ctx ()

let pp_namespace_uri ctx = function
  | Url (value, _) when Pp.minified ctx -> Pp.quoted_string ctx value
  | Url (value, Bare) ->
      Pp.string ctx "url(";
      Pp.string ctx value;
      Pp.char ctx ')'
  | Url (value, Quoted q) ->
      Pp.string ctx "url(";
      Pp.char ctx q;
      Pp.string ctx value;
      Pp.char ctx q;
      Pp.char ctx ')'
  | Quoted value -> Pp.quoted_string ctx value

let pp_namespace_statement ctx prefix uri =
  Pp.string ctx "@namespace ";
  (match prefix with
  | Some p ->
      Pp.string ctx p;
      Pp.sp ctx ()
  | None -> ());
  pp_namespace_uri ctx uri;
  Pp.semicolon ctx ()

let pp_supports_condition_value ctx condition =
  match condition with
  | Supports.And (_, Supports.Function _) | Supports.And (Supports.Function _, _)
    ->
      Pp.char ctx '(';
      Supports.pp ctx condition;
      Pp.char ctx ')'
  | _ -> Supports.pp ctx condition

let pp_font_variant_descriptor_value ctx = function
  | Ligature value -> Properties.pp_font_variant_ligature ctx value
  | Caps value -> Properties.pp_font_variant_caps ctx value
  | Numeric value -> Properties.pp_font_variant_numeric_token ctx value
  | East_asian value -> Properties.pp_east_asian_feature ctx value

let rec pp_font_variant_descriptor ctx = function
  | Normal -> Pp.string ctx "normal"
  | None -> Pp.string ctx "none"
  | Values values ->
      Pp.list ~sep:Pp.space pp_font_variant_descriptor_value ctx values
  | Var var -> Values.pp_var pp_font_variant_descriptor ctx var

let pp_font_face_descriptor : font_face_descriptor Pp.t =
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
          Pp.list ~sep:Pp.comma Properties.pp_font_family_name ctx fams)
        families
  | Src value -> pp_descriptor "src" Properties.pp_font_src value
  | Font_style style ->
      pp_descriptor "font-style" Properties.pp_font_style style
  | Font_weight weight ->
      pp_descriptor "font-weight" Properties.pp_font_weight weight
  | Font_weight_range (min_weight, max_weight) ->
      pp_descriptor "font-weight"
        (fun ctx (min_weight, max_weight) ->
          Properties.pp_font_weight ctx min_weight;
          Pp.space ctx ();
          Properties.pp_font_weight ctx max_weight)
        (min_weight, max_weight)
  | Font_style_auto -> pp_descriptor "font-style" Pp.string "auto"
  | Font_weight_auto -> pp_descriptor "font-weight" Pp.string "auto"
  | Font_stretch_auto -> pp_descriptor "font-stretch" Pp.string "auto"
  | Font_stretch stretch ->
      pp_descriptor "font-stretch" Properties.pp_font_stretch stretch
  | Font_stretch_range (min_stretch, max_stretch) ->
      pp_descriptor "font-stretch"
        (fun ctx (min_stretch, max_stretch) ->
          Properties.pp_font_stretch ctx min_stretch;
          Pp.space ctx ();
          Properties.pp_font_stretch ctx max_stretch)
        (min_stretch, max_stretch)
  | Font_display value ->
      pp_descriptor "font-display" Properties.pp_font_display value
  | Unicode_range values ->
      pp_descriptor "unicode-range"
        (fun ctx vs -> Pp.list ~sep:Pp.comma Properties.pp_unicode_range ctx vs)
        values
  | Font_variant value ->
      pp_descriptor "font-variant" pp_font_variant_descriptor value
  | Font_feature_settings value ->
      pp_descriptor "font-feature-settings" Properties.pp_font_feature_settings
        value
  | Font_variation_settings value ->
      pp_descriptor "font-variation-settings"
        Properties.pp_font_variation_settings value
  | Size_adjust value ->
      pp_descriptor "size-adjust" Font_face.pp_size_adjust value
  | Ascent_override value ->
      pp_descriptor "ascent-override" Font_face.pp_metric_override value
  | Descent_override value ->
      pp_descriptor "descent-override" Font_face.pp_metric_override value
  | Line_gap_override value ->
      pp_descriptor "line-gap-override" Font_face.pp_metric_override value

let pp_counter_style_system ctx = function
  | Cyclic -> Pp.string ctx "cyclic"
  | Numeric -> Pp.string ctx "numeric"
  | Alphabetic -> Pp.string ctx "alphabetic"
  | Symbolic -> Pp.string ctx "symbolic"
  | Fixed None -> Pp.string ctx "fixed"
  | Fixed (Some n) ->
      Pp.string ctx "fixed";
      Pp.space ctx ();
      Pp.int ctx n
  | Additive -> Pp.string ctx "additive"
  | Extends name ->
      Pp.string ctx "extends";
      Pp.space ctx ();
      Pp.string ctx (Parser.escape_ident name)

let pp_counter_symbol ctx symbol = Pp.quoted_string ctx symbol

let pp_counter_style_descriptor ctx =
  let pp_descriptor name pp_value value =
    Pp.string ctx name;
    Pp.char ctx ':';
    Pp.space_if_pretty ctx ();
    pp_value ctx value
  in
  function
  | System system -> pp_descriptor "system" pp_counter_style_system system
  | Symbols symbols ->
      (* Symbols are quoted strings; the [""] delimiter is unambiguous so the
         inter-symbol space is elidable in minified output. *)
      pp_descriptor "symbols"
        (Pp.list ~sep:Pp.space_if_pretty pp_counter_symbol)
        symbols
  | Suffix suffix -> pp_descriptor "suffix" pp_counter_symbol suffix
  | Prefix prefix -> pp_descriptor "prefix" pp_counter_symbol prefix
  | Fallback value -> pp_descriptor "fallback" Pp.string value
  | Range value -> pp_descriptor "range" Pp.string value
  | Pad value -> pp_descriptor "pad" Pp.string value
  | Negative value -> pp_descriptor "negative" Pp.string value
  | Additive_symbols value -> pp_descriptor "additive-symbols" Pp.string value
  | Speak_as value -> pp_descriptor "speak-as" Pp.string value

let pp_font_palette_base ctx = function
  | Light -> Pp.string ctx "light"
  | Dark -> Pp.string ctx "dark"
  | Index i -> Pp.string ctx (Int.to_string i)
  | Palette_ident s -> Pp.string ctx s

let pp_font_palette_descriptor : font_palette_descriptor Pp.t =
 fun ctx -> function
  | Palette_font_family families ->
      Pp.string ctx "font-family:";
      Pp.space_if_pretty ctx ();
      Pp.list ~sep:Pp.comma Properties.pp_font_family_name ctx families
  | Base_palette base ->
      Pp.string ctx "base-palette:";
      Pp.space_if_pretty ctx ();
      pp_font_palette_base ctx base
  | Override_colors entries ->
      Pp.string ctx "override-colors:";
      Pp.space_if_pretty ctx ();
      Pp.list ~sep:Pp.comma
        (fun ctx (index, color) ->
          Pp.string ctx (Int.to_string index);
          let rendered =
            Pp.to_string ~minify:(Pp.minified ctx) ~lossless:ctx.Pp.lossless
              Values.pp_color color
          in
          if (not (Pp.minified ctx)) || rendered = "" || rendered.[0] <> '#'
          then Pp.space ctx ();
          Pp.string ctx rendered)
        ctx entries

let pp_font_feature_value ctx (name, indexes) =
  Pp.string ctx (Parser.escape_ident name);
  Pp.char ctx ':';
  Pp.space_if_pretty ctx ();
  Pp.list ~sep:Pp.space Pp.int ctx indexes

let pp_font_feature_values_block ctx (name, values) =
  Pp.char ctx '@';
  Pp.string ctx name;
  Pp.sp ctx ();
  Pp.braced_semicolon_list pp_font_feature_value ctx values

let pp_view_transition_descriptor : view_transition_descriptor Pp.t =
 fun ctx -> function
  | Navigation `Auto -> Pp.string ctx "navigation:auto"
  | Navigation `None -> Pp.string ctx "navigation:none"
  | Types None -> Pp.string ctx "types:none"
  | Types (Some types) ->
      Pp.string ctx "types:";
      Pp.space_if_pretty ctx ();
      Pp.list ~sep:Pp.space Pp.string ctx types

(* Under minify a [Declarations] statement carries no trailing [;] of its own,
   so whoever sequences the statements owes it a separator: CSS Syntax 3 (ED)
   sec. 5.5.5 runs a declaration to the next [;] or to the block's [}], and a
   sibling that follows would otherwise be read as part of its value. *)
let pp_declarations_sep ctx = function
  | Declarations decls when decls <> [] && Pp.minified ctx ->
      Pp.semicolon ctx ()
  | _ -> ()

let rec pp_rule : rule Pp.t =
 fun ctx rule ->
  Selector.pp ctx rule.selector;
  Pp.sp ctx ();
  Pp.block_open ctx ();
  let decls = rule.declarations in
  (match (decls, rule.nested) with
  | [], [] -> ()
  | decls, nested ->
      let ctx = Pp.enter_style_rule { ctx with level = ctx.level + 1 } in
      let pp_declarations ctx () =
        Pp.list ~sep:Pp.semicolon_cut (Pp.indent Declaration.pp) ctx decls
      in
      let pp_nested ctx () =
        let rec loop = function
          | [] -> ()
          | [ stmt ] -> Pp.indent pp_statement ctx stmt
          | stmt :: rest ->
              Pp.indent pp_statement ctx stmt;
              pp_declarations_sep ctx stmt;
              Pp.cut ctx ();
              loop rest
        in
        loop nested
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
        match ctx.indent with
        | Some w when ctx.level > 1 ->
            Pp.string ctx (String.make (w * (ctx.level - 1)) ' ')
        | _ -> ()));
  Pp.block_close ctx ()

and pp_layer_statement ctx name content =
  Pp.string ctx "@layer";
  (match name with
  | Some n ->
      Pp.string ctx " ";
      pp_layer_name ctx n
  | None -> ());
  (* For empty layers: use statement form when minifying (more concise), but
     preserve block form otherwise for roundtrip fidelity. A style rule accepts
     no layer-order declaration, so inside one the block form is the only
     spelling that survives a re-read. *)
  if content = [] && Pp.minified ctx && not (Pp.in_style_rule ctx) then
    Pp.semicolon ctx ()
  else (
    Pp.sp ctx ();
    Pp.braces pp_block ctx content)

and pp_media_statement ctx condition content =
  Pp.string ctx "@media";
  (match condition with
  | Media.List [] -> ()
  | _ ->
      (* Per cascade's minify policy (README sec. "Minify policy"), elide
         whitespace at safe token boundaries: a leading [(] needs no space.
         Idents like [screen] still need it. *)
      let rendered =
        Pp.to_string ~minify:ctx.Pp.minify ~lossless:ctx.Pp.lossless Media.pp
          condition
      in
      if Pp.minified ctx && String.length rendered > 0 && rendered.[0] = '('
      then ()
      else Pp.string ctx " ";
      Pp.string ctx rendered);
  Pp.sp ctx ();
  Pp.braces pp_block ctx content

and pp_container_statement ctx name condition content =
  Pp.string ctx "@container";
  (match name with
  | Some n ->
      Pp.char ctx ' ';
      Pp.string ctx (Parser.escape_ident n)
  | None -> ());
  (match condition with
  | Some condition ->
      let condition_str =
        Container.to_stylesheet_string ~minify:ctx.Pp.minify condition
      in
      if condition_str <> "" then (
        (* Per cascade's minify policy: elide whitespace at safe token
           boundaries. A leading [(] needs no space; a name component (after
           [@container <name>] or a non-paren condition like [not (...)] /
           [style(...)]) does. *)
        if Pp.minified ctx && name = None && condition_str.[0] = '(' then ()
        else Pp.char ctx ' ';
        Pp.string ctx condition_str)
  | None -> ());
  Pp.sp ctx ();
  Pp.braces pp_block ctx content

and pp_supports_statement ctx condition content =
  (* Per cascade's minify policy (README sec. "Minify policy"): elide whitespace
     at safe token boundaries. The space after [@supports] is droppable only
     when the condition opens with [(] - any other start ([not],
     [selector(...)], etc.) needs the space to keep token shape. *)
  Pp.string ctx "@supports";
  let rendered =
    Pp.to_string ~minify:ctx.Pp.minify ~lossless:ctx.Pp.lossless
      pp_supports_condition_value condition
  in
  if Pp.minified ctx && String.length rendered > 0 && rendered.[0] = '(' then ()
  else Pp.string ctx " ";
  Pp.string ctx rendered;
  Pp.sp ctx ();
  Pp.braces pp_block ctx content

and pp_else_statement ctx (condition : conditional option) content =
  Pp.string ctx "@else";
  (match condition with
  | None -> ()
  | Some c ->
      Pp.string ctx " ";
      pp_conditional ctx c);
  Pp.sp ctx ();
  Pp.braces pp_block ctx content

and pp_scope_statement ctx start end_ content =
  Pp.string ctx "@scope";
  (match start with
  | Some s ->
      Pp.space_if_pretty ctx ();
      Pp.string ctx "(";
      pp_scope_selector ctx s;
      Pp.string ctx ")"
  | None -> ());
  (match end_ with
  | Some e ->
      (* [@scope] and [to] are both ident-shaped, so with nothing between them
         they lex as the one at-keyword [@scopeto]. The [)] closing a start
         selector already delimits them. *)
      if (not (Pp.minified ctx)) || Option.is_none start then Pp.char ctx ' ';
      Pp.string ctx "to (";
      pp_scope_selector ctx e;
      Pp.string ctx ")"
  | None -> ());
  Pp.sp ctx ();
  Pp.braces pp_block ctx content

and pp_statement : statement Pp.t =
 fun ctx -> function
  | Rule rule -> pp_rule ctx rule
  | Declarations raw_decls -> pp_declarations_statement ctx raw_decls
  | Bang_comment body ->
      Pp.string ctx "/*";
      Pp.string ctx body;
      Pp.string ctx "*/"
  | Charset encoding ->
      Pp.string ctx "@charset \"";
      Pp.string ctx encoding;
      Pp.string ctx "\";"
  | Import { url; layer; supports; media } ->
      Pp.string ctx "@import";
      pp_import_components ctx { url; layer; supports; media };
      Pp.semicolon ctx ()
  | Namespace (prefix, uri) -> pp_namespace_statement ctx prefix uri
  | Property r -> pp_property_rule ctx r
  | Layer_decl names ->
      Pp.string ctx "@layer ";
      Pp.list ~sep:Pp.comma pp_layer_name ctx names;
      Pp.semicolon ctx ()
  | Layer (name, content) -> pp_layer_statement ctx name content
  | Media (condition, content) -> pp_media_statement ctx condition content
  | Container (name, condition, content) ->
      pp_container_statement ctx name condition content
  | Supports (condition, content) -> pp_supports_statement ctx condition content
  | Moz_document (conditions, content) ->
      Pp.string ctx "@-moz-document ";
      Pp.list ~sep:Pp.comma pp_moz_document_condition ctx conditions;
      Pp.sp ctx ();
      Pp.braces pp_block ctx content
  | Starting_style content ->
      Pp.string ctx "@starting-style";
      Pp.braces pp_block ctx content
  | When (condition, content) ->
      Pp.string ctx "@when ";
      pp_conditional ctx condition;
      Pp.sp ctx ();
      Pp.braces pp_block ctx content
  | Else (condition, content) -> pp_else_statement ctx condition content
  | Supports_condition (name, declarations) ->
      Pp.string ctx "@supports-condition ";
      Pp.string ctx name;
      Pp.sp ctx ();
      Pp.braced_semicolon_list Declaration.pp ctx declarations
  | Origin (_, content) -> pp_block ctx content
  | Scope (start, end_, content) -> pp_scope_statement ctx start end_ content
  | Keyframes (name, frames) ->
      pp_keyframes_block_statement ctx "@keyframes " name frames
  | Webkit_keyframes (name, frames) ->
      pp_keyframes_block_statement ctx "@-webkit-keyframes " name frames
  | Moz_keyframes (name, frames) ->
      pp_keyframes_block_statement ctx "@-moz-keyframes " name frames
  | Font_face descriptors ->
      Pp.string ctx "@font-face";
      Pp.sp ctx ();
      Pp.braced_semicolon_list pp_font_face_descriptor ctx descriptors
  | Counter_style (name, descriptors) ->
      Pp.string ctx "@counter-style ";
      Pp.string ctx (Parser.escape_ident name);
      Pp.sp ctx ();
      Pp.braced_semicolon_list pp_counter_style_descriptor ctx descriptors
  | Page (selector, raw_declarations) ->
      Pp.string ctx "@page";
      pp_page_selector ctx selector;
      Pp.sp ctx ();
      Pp.braced_semicolon_list Declaration.pp ctx raw_declarations
  | Page_with_margins (selector, descriptors, margins) ->
      Pp.string ctx "@page";
      pp_page_selector ctx selector;
      Pp.sp ctx ();
      pp_page_with_margins_body ctx descriptors margins
  | Font_palette_values (name, descriptors) ->
      Pp.string ctx "@font-palette-values ";
      Pp.string ctx (Parser.escape_ident name);
      Pp.sp ctx ();
      Pp.braced_semicolon_list pp_font_palette_descriptor ctx descriptors
  | Font_feature_values (families, blocks) ->
      Pp.string ctx "@font-feature-values ";
      Pp.list ~sep:Pp.comma Properties.pp_font_family ctx families;
      Pp.sp ctx ();
      Pp.braced_list
        ~sep:Pp.(cut ++ cut)
        pp_font_feature_values_block ctx blocks
  | View_transition descriptors ->
      Pp.string ctx "@view-transition";
      Pp.sp ctx ();
      Pp.braced_semicolon_list pp_view_transition_descriptor ctx descriptors
  | Position_try (name, declarations) ->
      Pp.string ctx "@position-try ";
      Pp.string ctx (Parser.escape_ident name);
      Pp.sp ctx ();
      Pp.braced_semicolon_list Declaration.pp ctx declarations
  | Viewport (prefix, descriptors) ->
      pp_viewport_statement ctx prefix descriptors
  | Unknown_at_rule { name; prelude; block } ->
      pp_unknown_at_rule_statement ctx name prelude block

and pp_block : block Pp.t =
 fun ctx statements ->
  let statements = printable_statements ctx statements in
  (* Block printing for at-rules (@media, @supports, etc.) The braces helper
     adds nest 1 and indent for the first item only. Subsequent items need
     explicit indentation and blank line separation to match Tailwind format. *)
  match statements with
  | [] -> ()
  | [ s ] -> pp_statement ctx s
  | s :: rest ->
      pp_statement ctx s;
      pp_declarations_sep ctx s;
      let rec pp_rest = function
        | [] -> ()
        | [ stmt ] ->
            Pp.cut ctx ();
            if not ctx.Pp.minify then Pp.cut ctx ();
            Pp.indent pp_statement ctx stmt
        | stmt :: rest ->
            Pp.cut ctx ();
            if not ctx.Pp.minify then Pp.cut ctx ();
            Pp.indent pp_statement ctx stmt;
            pp_declarations_sep ctx stmt;
            pp_rest rest
      in
      pp_rest rest

let is_layer_block = function Layer _ -> true | _ -> false

let pp_stylesheet : stylesheet Pp.t =
 fun ctx statements ->
  let statements =
    if Pp.minified ctx then normalise statements else statements
  in
  let statements = printable_statements ctx statements in
  let rec loop = function
    | [] -> ()
    | [ s ] -> pp_statement ctx s
    | s :: (next :: _ as rest) ->
        pp_statement ctx s;
        pp_declarations_sep ctx s;
        Pp.cut ctx ();
        (* Add blank line between consecutive @layer { } blocks *)
        if (not (Pp.minified ctx)) && is_layer_block s && is_layer_block next
        then Pp.cut ctx ();
        loop rest
  in
  loop statements

let pp = pp_stylesheet

(* Pretty-printer for import_rule *)
let pp_import_rule : import_rule Pp.t =
 fun ctx rule ->
  Pp.string ctx "@import";
  pp_import_components ctx rule;
  Pp.string ctx ";"
