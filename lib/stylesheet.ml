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

let container ?name ?condition content = Container (name, condition, content)
let supports ~condition content = Supports (condition, content)
let starting_style content = Starting_style content
let with_origin cascade_origin content = Origin (cascade_origin, content)

let origin_importance_rank ~important = function
  | Transition -> 9
  | User_agent when important -> 8
  | User when important -> 7
  | Author when important -> 6
  | Animation -> 5
  | Author -> 4
  | Author_presentational_hint -> 3
  | User -> 2
  | User_agent -> 1

let import_layer_name ({ layer; _ } : import_rule) = layer

let layer_block_name = function
  | Layer (Some name, _) -> Some name
  | Layer (None, _) -> Some ""
  | _ -> None

let layer_statement_name_list = function
  | Layer_decl names -> Some names
  | _ -> None

let rec index_of x i = function
  | [] -> None
  | y :: ys -> if x = y then Some i else index_of x (i + 1) ys

let cascade_layer_precedence_rank ~layer_order ~important layer =
  let layer_count = List.length layer_order in
  match (important, layer) with
  | false, None -> layer_count
  | false, Some name ->
      Option.value ~default:layer_count (index_of name 0 layer_order)
  | true, None -> 0
  | true, Some name ->
      let i = Option.value ~default:layer_count (index_of name 0 layer_order) in
      layer_count - i

let compare_cascade_layer_candidate ~layer_order (a : cascade_layer_candidate)
    (b : cascade_layer_candidate) =
  let key (c : cascade_layer_candidate) =
    ( (if c.important then 1 else 0),
      cascade_layer_precedence_rank ~layer_order ~important:c.important c.layer,
      c.source_order )
  in
  compare (key a) (key b)

let winning_cascade_layer_candidate ~layer_order
    (candidates : cascade_layer_candidate list) =
  match candidates with
  | [] -> None
  | first :: rest ->
      Some
        (List.fold_left
           (fun winner candidate ->
             if
               compare_cascade_layer_candidate ~layer_order winner candidate < 0
             then candidate
             else winner)
           first rest)

let cascade_revert_layer_candidates ~layer_order ~important ~current_layer
    (candidates : cascade_layer_candidate list) =
  let current_rank =
    cascade_layer_precedence_rank ~layer_order ~important current_layer
  in
  List.filter
    (fun (candidate : cascade_layer_candidate) ->
      candidate.important = important
      && cascade_layer_precedence_rank ~layer_order ~important candidate.layer
         < current_rank)
    candidates

let compare_cascade_origin_candidate (a : cascade_origin_candidate)
    (b : cascade_origin_candidate) =
  let key (c : cascade_origin_candidate) =
    (origin_importance_rank ~important:c.important c.origin, c.source_order)
  in
  compare (key a) (key b)

let winning_cascade_origin_candidate
    (candidates : cascade_origin_candidate list) =
  match candidates with
  | [] -> None
  | first :: rest ->
      Some
        (List.fold_left
           (fun winner candidate ->
             if compare_cascade_origin_candidate winner candidate < 0 then
               candidate
             else winner)
           first rest)

let revert_origin_rollback_origins = function
  | User_agent -> []
  | User -> [ User_agent ]
  | Author_presentational_hint | Author | Animation -> [ User_agent; User ]
  | Transition ->
      [ User_agent; User; Author_presentational_hint; Author; Animation ]

let cascade_revert_origin_candidates ~important ~current_origin
    (candidates : cascade_origin_candidate list) =
  let origins = revert_origin_rollback_origins current_origin in
  List.filter
    (fun (candidate : cascade_origin_candidate) ->
      candidate.important = important
      && List.exists (( = ) candidate.origin) origins)
    candidates

let declared_values ?property declarations =
  declarations
  |> List.mapi (fun source_order declaration ->
      let declaration_property = Declaration.property_name declaration in
      let value = Declaration.string_of_value ~minify:true declaration in
      {
        property = declaration_property;
        value;
        important = Declaration.is_important declaration;
        source_order;
      })
  |> List.filter (fun declared ->
      match property with
      | None -> true
      | Some property -> declared.property = property)

let cascaded_value candidates =
  winning_cascade_origin_candidate candidates
  |> Option.map (fun (c : cascade_origin_candidate) -> c.value)

let scope_proximity_rank = function None -> min_int | Some hops -> -hops

let compare_cascade_candidate ~layer_order (a : cascade_candidate)
    (b : cascade_candidate) =
  let key (c : cascade_candidate) =
    ( origin_importance_rank ~important:c.candidate_important c.candidate_origin,
      cascade_layer_precedence_rank ~layer_order
        ~important:c.candidate_important c.candidate_layer,
      c.candidate_specificity,
      scope_proximity_rank c.candidate_scope_hops,
      c.candidate_source_order )
  in
  compare (key a) (key b)

let winning_cascade_candidate ~layer_order (candidates : cascade_candidate list)
    =
  match candidates with
  | [] -> None
  | first :: rest ->
      Some
        (List.fold_left
           (fun winner candidate ->
             if compare_cascade_candidate ~layer_order winner candidate < 0 then
               candidate
             else winner)
           first rest)

let specified_value ~inherits ~initial ~inherited ~cascaded =
  let inherited_or_initial () = Option.value ~default:initial inherited in
  match cascaded with
  | Some "initial" ->
      { specified_value = initial; specified_value_source = Initial_keyword }
  | Some "inherit" ->
      {
        specified_value = inherited_or_initial ();
        specified_value_source = Inherit_keyword;
      }
  | Some "unset" when inherits ->
      {
        specified_value = inherited_or_initial ();
        specified_value_source = Unset_inherited;
      }
  | Some "unset" ->
      { specified_value = initial; specified_value_source = Unset_initial }
  | Some value -> { specified_value = value; specified_value_source = Cascaded }
  | None when inherits ->
      {
        specified_value = inherited_or_initial ();
        specified_value_source = Inherited_default;
      }
  | None ->
      { specified_value = initial; specified_value_source = Initial_default }

(* Resolve a chain of [revert] winners. Each revert exposes the next-lower
   origin's winning candidate; if THAT is also [revert], peel another origin
   off, and so on until a non-revert winner or no candidates remain. The
   rollback context comes from the winner itself so callers cannot pass a
   [current_origin] that disagrees with the candidate they exposed. *)
let rec resolve_revert_chain candidates =
  match winning_cascade_origin_candidate candidates with
  | Some winner when winner.value = "revert" ->
      cascade_revert_origin_candidates ~important:winner.important
        ~current_origin:winner.origin candidates
      |> resolve_revert_chain
  | other -> other

let specified_value_after_revert ~inherits ~initial ~inherited candidates =
  let cascaded =
    resolve_revert_chain candidates
    |> Option.map (fun (c : cascade_origin_candidate) -> c.value)
  in
  specified_value ~inherits ~initial ~inherited ~cascaded

let rec resolve_revert_layer_chain ~layer_order candidates =
  match winning_cascade_layer_candidate ~layer_order candidates with
  | Some winner when winner.value = "revert-layer" ->
      cascade_revert_layer_candidates ~layer_order ~important:winner.important
        ~current_layer:winner.layer candidates
      |> resolve_revert_layer_chain ~layer_order
  | other -> other

let specified_value_after_revert_layer ~inherits ~initial ~inherited
    ~layer_order candidates =
  match resolve_revert_layer_chain ~layer_order candidates with
  | Some winner ->
      specified_value ~inherits ~initial ~inherited
        ~cascaded:(Some winner.value)
  | None -> specified_value ~inherits ~initial ~inherited ~cascaded:None

let value_processing_requires_document_context = function
  | Declared_value | Cascaded_value | Specified_value -> false
  | Computed_value | Used_value | Actual_value -> true

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
    Pp.string ctx "initial-value:";
    Pp.space_if_pretty ctx ();
    Variables.pp_value syntax ctx v
  in
  let pp_initial_value_opt ctx =
    if Pp.minified ctx && is_empty_universal_initial syntax initial_value then
      ()
    else
      match initial_value with None -> () | Some v -> pp_initial_value ctx v
  in
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
          pp_initial_value_opt ctx)
        ctx ();
      Pp.cut ctx ())
    ctx ()

(* CSS Animations 1 §3 [<keyframes-name>] is [<custom-ident> | <string>]. The
   reader normalizes either form to a plain OCaml string; on output we prefer
   the bare identifier when the value is a syntactically valid CSS ident
   (shorter than the quoted form), falling back to a double-quoted string when
   the name contains characters that would otherwise need escaping. *)
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
  if is_safe_ident then Pp.string ctx name else Pp.quoted_string ctx name

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
        Pp.list ~sep:Pp.semicolon_cut
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

and pp_keyframe_position : Keyframe.position Pp.t =
 fun ctx pos ->
  (* CSS Animations 1 7.1: [from] is equivalent to [0%] and [to] to [100%].
     Under minification, canonicalise to the shorter spelling. *)
  match (pos, Pp.minified ctx) with
  | Keyframe.From, true -> Pp.string ctx "0%"
  | Keyframe.Percent 100., true -> Pp.string ctx "to"
  | _ -> Pp.string ctx (Keyframe.string_of_position pos)

and pp_keyframe_selector : Keyframe.selector Pp.t =
 fun ctx sel ->
  match sel with
  | Keyframe.Positions positions ->
      Pp.list ~sep:Pp.comma pp_keyframe_position ctx positions

and pp_keyframe : keyframe Pp.t =
 fun ctx kf ->
  pp_keyframe_selector ctx kf.keyframe_selector;
  Pp.sp ctx ();
  Pp.braced_semicolon_list Declaration.pp_declaration ctx
    kf.keyframe_declarations

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
        (fun ctx v ->
          Pp.string ctx (Font_face.string_of_src ~minify:(Pp.minified ctx) v))
        value
  | Font_style style ->
      pp_descriptor "font-style" Properties.pp_font_style style
  | Font_style_range (min_style, max_style) ->
      pp_descriptor "font-style"
        (fun ctx (min_style, max_style) ->
          Properties.pp_font_style ctx min_style;
          Pp.space ctx ();
          Properties.pp_font_style ctx max_style)
        (min_style, max_style)
  | Font_weight weight ->
      pp_descriptor "font-weight" Properties.pp_font_weight weight
  | Font_weight_range (min_weight, max_weight) ->
      pp_descriptor "font-weight"
        (fun ctx (min_weight, max_weight) ->
          Properties.pp_font_weight ctx min_weight;
          Pp.space ctx ();
          Properties.pp_font_weight ctx max_weight)
        (min_weight, max_weight)
  | Font_stretch stretch ->
      pp_descriptor "font-stretch" Properties.pp_font_stretch stretch
  | Font_stretch_range value -> pp_descriptor "font-stretch" Pp.string value
  | Font_display value ->
      pp_descriptor "font-display" Properties.pp_font_display value
  | Unicode_range values ->
      pp_descriptor "unicode-range"
        (fun ctx vs -> Pp.list ~sep:Pp.comma Properties.pp_unicode_range ctx vs)
        values
  | Font_variant value -> pp_descriptor "font-variant" Pp.string value
  | Font_feature_settings value ->
      pp_descriptor "font-feature-settings" Pp.string value
  | Font_variation_settings value ->
      pp_descriptor "font-variation-settings" Pp.string value
  | Font_tech value -> pp_descriptor "font-tech" Pp.string value
  | Size_adjust value ->
      pp_descriptor "size-adjust"
        (fun ctx v -> Pp.string ctx (Font_face.string_of_size_adjust v))
        value
  | Ascent_override value ->
      pp_descriptor "ascent-override"
        (fun ctx v -> Pp.string ctx (Font_face.string_of_metric_override v))
        value
  | Descent_override value ->
      pp_descriptor "descent-override"
        (fun ctx v -> Pp.string ctx (Font_face.string_of_metric_override v))
        value
  | Line_gap_override value ->
      pp_descriptor "line-gap-override"
        (fun ctx v -> Pp.string ctx (Font_face.string_of_metric_override v))
        value

and pp_counter_style_system ctx = function
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
      Pp.string ctx name

and pp_counter_symbol ctx symbol = Pp.quoted_string ctx symbol

and pp_counter_style_descriptor ctx =
  let pp_descriptor name pp_value value =
    Pp.string ctx name;
    Pp.char ctx ':';
    Pp.space_if_pretty ctx ();
    pp_value ctx value
  in
  function
  | Counter_system system ->
      pp_descriptor "system" pp_counter_style_system system
  | Counter_symbols symbols ->
      pp_descriptor "symbols" (Pp.list ~sep:Pp.space pp_counter_symbol) symbols
  | Counter_suffix suffix -> pp_descriptor "suffix" pp_counter_symbol suffix
  | Counter_prefix prefix -> pp_descriptor "prefix" pp_counter_symbol prefix
  | Counter_fallback value -> pp_descriptor "fallback" Pp.string value
  | Counter_range value -> pp_descriptor "range" Pp.string value
  | Counter_pad value -> pp_descriptor "pad" Pp.string value
  | Counter_negative value -> pp_descriptor "negative" Pp.string value
  | Counter_additive_symbols value ->
      pp_descriptor "additive-symbols" Pp.string value
  | Counter_speak_as value -> pp_descriptor "speak-as" Pp.string value

and pp_font_palette_base ctx = function
  | Light -> Pp.string ctx "light"
  | Dark -> Pp.string ctx "dark"
  | Index i -> Pp.string ctx (Int.to_string i)
  | Palette_ident s -> Pp.string ctx s

and pp_font_palette_descriptor : font_palette_descriptor Pp.t =
 fun ctx -> function
  | Palette_font_family families ->
      Pp.string ctx "font-family:";
      Pp.space_if_pretty ctx ();
      Pp.list ~sep:Pp.comma Properties.pp_font_family ctx families
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
            Pp.to_string ~minify:(Pp.minified ctx) Values.pp_color color
          in
          if (not (Pp.minified ctx)) || rendered = "" || rendered.[0] <> '#'
          then Pp.space ctx ();
          Pp.string ctx rendered)
        ctx entries

and pp_font_feature_value ctx (name, indexes) =
  Pp.string ctx name;
  Pp.char ctx ':';
  Pp.space_if_pretty ctx ();
  Pp.list ~sep:Pp.space Pp.int ctx indexes

and pp_font_feature_values_block ctx (name, values) =
  Pp.char ctx '@';
  Pp.string ctx name;
  Pp.sp ctx ();
  Pp.braced_semicolon_list pp_font_feature_value ctx values

and pp_view_transition_descriptor : view_transition_descriptor Pp.t =
 fun ctx -> function
  | Navigation `Auto -> Pp.string ctx "navigation:auto"
  | Navigation `None -> Pp.string ctx "navigation:none"
  | Types None -> Pp.string ctx "types:none"
  | Types (Some types) ->
      Pp.string ctx "types:";
      Pp.space_if_pretty ctx ();
      Pp.list ~sep:Pp.space Pp.string ctx types

and pp_page_margin_rule : page_margin_rule Pp.t =
 fun ctx rule ->
  Pp.string ctx "@";
  Pp.string ctx rule.margin_name;
  Pp.sp ctx ();
  Pp.braced_semicolon_list Declaration.pp_declaration ctx
    rule.margin_descriptors

and pp_page_selector ctx selector =
  match selector with
  | Some s ->
      if String.length s > 0 && s.[0] = ':' then Pp.sp ctx ()
      else Pp.space ctx ();
      Pp.string ctx s
  | None -> ()

and import_url_starts_with_url url len =
  len >= 4 && String.lowercase_ascii (String.sub url 0 4) = "url("

and import_url_unwrap_quoted url len =
  if len >= 2 && (url.[0] = '"' || url.[0] = '\'') && url.[len - 1] = url.[0]
  then Some (String.sub url 1 (len - 2))
  else None

and import_url_strip_inner_quotes inner =
  let inner_len = String.length inner in
  if inner_len = 0 then None
  else if
    inner_len >= 2
    && (inner.[0] = '"' || inner.[0] = '\'')
    && inner.[inner_len - 1] = inner.[0]
  then Some (String.sub inner 1 (inner_len - 2))
  else Some inner

and import_url_inner_string url len =
  (* Lift the inner string out of [url(...)] - either the quoted form
     [url("foo")] / [url('foo')] or the bare form [url(foo)]. Returns the
     unquoted body so the [@import] / [@namespace] minify path can re-emit it as
     a bare double-quoted string. *)
  if not (import_url_starts_with_url url len) then None
  else if len < 5 || url.[len - 1] <> ')' then None
  else
    let inner = String.sub url 4 (len - 5) |> String.trim in
    import_url_strip_inner_quotes inner

and pp_import_url_minified ctx url len =
  (* Canonicalise to the shortest spec-equivalent form: a double-quoted string
     (CSS Syntax 4.3.5 prefers double quotes). The [url()] wrapping is five
     characters of overhead that the bare-string form omits per CSS Conditional
     Rules 3, and a single-quoted source string re-emits with double quotes. *)
  match import_url_inner_string url len with
  | Some s -> Pp.quoted_string ctx s
  | None -> (
      match import_url_unwrap_quoted url len with
      | Some s -> Pp.quoted_string ctx s
      | None ->
          if len > 0 && import_url_starts_with_url url len then
            Pp.string ctx url
          else Pp.quoted_string ctx url)

and pp_import_url ctx url =
  let len = String.length url in
  if Pp.minified ctx then pp_import_url_minified ctx url len
  else if
    len > 0
    && (url.[0] = '"' || url.[0] = '\'' || import_url_starts_with_url url len)
  then Pp.string ctx url
  else Pp.quoted_string ctx url

and pp_import_layer ctx layer =
  Pp.sp ctx ();
  if layer = "" then Pp.string ctx "layer"
  else (
    Pp.string ctx "layer(";
    Pp.string ctx layer;
    Pp.char ctx ')')

and pp_import_supports ctx supports =
  Pp.sp ctx ();
  Pp.string ctx "supports(";
  (match supports with
  | Supports.Property decl -> Supports.pp_declaration_feature ctx decl
  | _ -> Supports.pp ctx supports);
  Pp.char ctx ')'

and starts_with s prefix =
  let s_len = String.length s and prefix_len = String.length prefix in
  s_len >= prefix_len && String.sub s 0 prefix_len = prefix

and import_supports_media_needs_space supports media =
  match supports with
  | Supports.Property decl ->
      let supports =
        Pp.to_string ~minify:true Supports.pp_declaration_feature decl
      in
      let media = Media.to_string ~minify:true media in
      starts_with supports "--" && starts_with media "("
  | _ -> false

and pp_import_components ctx { url; layer; supports; media } =
  Pp.sp ctx ();
  pp_import_url ctx url;
  Option.iter (pp_import_layer ctx) layer;
  Option.iter (pp_import_supports ctx) supports;
  Option.iter
    (fun media ->
      (match supports with
      | Some supports when import_supports_media_needs_space supports media ->
          Pp.space ctx ()
      | None when layer = Some "" -> Pp.space ctx ()
      | _ -> Pp.sp ctx ());
      Media.pp ctx media)
    media

and strip_outer_parens s =
  let s = String.trim s in
  let len = String.length s in
  if len >= 2 && s.[0] = '(' && s.[len - 1] = ')' then String.sub s 1 (len - 2)
  else s

and pp_condition_function ctx name rendered =
  Pp.string ctx name;
  Pp.char ctx '(';
  Pp.string ctx (strip_outer_parens rendered);
  Pp.char ctx ')'

and pp_conditional : conditional Pp.t =
 fun ctx -> function
  | Media_condition condition ->
      pp_condition_function ctx "media"
        (Pp.to_string ~minify:ctx.Pp.minify Media.pp condition)
  | Supports_condition_test condition ->
      pp_condition_function ctx "supports"
        (Pp.to_string ~minify:ctx.Pp.minify Supports.pp condition)
  | And (a, b) ->
      pp_conditional ctx a;
      Pp.string ctx " and ";
      pp_conditional ctx b
  | Or (a, b) ->
      pp_conditional ctx a;
      Pp.string ctx " or ";
      pp_conditional ctx b

and pp_moz_document_condition ctx = function
  | Url_prefix None -> Pp.string ctx "url-prefix()"
  | Url_prefix (Some prefix) ->
      Pp.string ctx "url-prefix(";
      Pp.quoted_string ctx prefix;
      Pp.char ctx ')'

and pp_declarations_statement ctx raw_decls =
  (* Bare declarations for CSS nesting - no selector/braces, just declarations.
     No extra indent since the containing block handles it *)
  let decls = Declaration.resolve_theme_guards ctx raw_decls in
  Pp.list ~sep:Pp.semicolon_cut Declaration.pp_declaration ctx decls;
  if decls <> [] && not ctx.Pp.minify then Pp.semicolon ctx ()

and pp_namespace_uri ctx = function
  | Url (value, _) when Pp.minified ctx -> Pp.quoted_string ctx value
  | Url (value, Url_bare) ->
      Pp.string ctx "url(";
      Pp.string ctx value;
      Pp.char ctx ')'
  | Url (value, Url_quoted q) ->
      Pp.string ctx "url(";
      Pp.char ctx q;
      Pp.string ctx value;
      Pp.char ctx q;
      Pp.char ctx ')'
  | Quoted value -> Pp.quoted_string ctx value

and pp_namespace_statement ctx prefix uri =
  Pp.string ctx "@namespace ";
  (match prefix with
  | Some p ->
      Pp.string ctx p;
      Pp.sp ctx ()
  | None -> ());
  pp_namespace_uri ctx uri;
  Pp.semicolon ctx ()

and pp_layer_statement ctx name content =
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

and pp_media_statement ctx condition content =
  Pp.string ctx "@media";
  (match condition with
  | Media.List [] -> ()
  | _ ->
      Pp.string ctx " ";
      Media.pp ctx condition);
  Pp.sp ctx ();
  Pp.braces pp_block ctx content

and pp_container_statement ctx name condition content =
  Pp.string ctx "@container";
  (match name with
  | Some n ->
      Pp.char ctx ' ';
      Pp.string ctx n
  | None -> ());
  (match condition with
  | Some condition ->
      let condition_str =
        Container.to_stylesheet_string ~minify:ctx.Pp.minify condition
      in
      if condition_str <> "" then (
        Pp.char ctx ' ';
        Pp.string ctx condition_str)
  | None -> ());
  Pp.sp ctx ();
  Pp.braces pp_block ctx content

and pp_supports_condition_value ctx condition =
  match condition with
  | Supports.And (_, Supports.Function _) | Supports.And (Supports.Function _, _)
    ->
      Pp.char ctx '(';
      Supports.pp ctx condition;
      Pp.char ctx ')'
  | _ -> Supports.pp ctx condition

and pp_supports_statement ctx condition content =
  Pp.string ctx "@supports ";
  pp_supports_condition_value ctx condition;
  Pp.sp ctx ();
  Pp.braces pp_block ctx content

and pp_else_statement ctx condition content =
  Pp.string ctx "@else";
  (match condition with
  | None -> ()
  | Some c ->
      Pp.string ctx " ";
      pp_conditional ctx c);
  Pp.sp ctx ();
  Pp.braces pp_block ctx content

and scope_replace_all pattern replacement s =
  let plen = String.length pattern in
  let slen = String.length s in
  let buf = Buffer.create slen in
  let rec loop i =
    if i >= slen then ()
    else if i + plen <= slen && String.sub s i plen = pattern then (
      Buffer.add_string buf replacement;
      loop (i + plen))
    else (
      Buffer.add_char buf s.[i];
      loop (i + 1))
  in
  loop 0;
  Buffer.contents buf

and compact_scope_combinators s =
  s
  |> scope_replace_all " > " ">"
  |> scope_replace_all " + " "+"
  |> scope_replace_all " ~ " "~"
  |> scope_replace_all " || " "||"

and pp_scope_selector ctx s =
  let s =
    try Selector.(to_string ~minify:(Pp.minified ctx) (of_string s))
    with Cursor.Parse_error _ | Invalid_argument _ -> s
  in
  let s = String.trim s in
  let s = compact_scope_combinators s in
  Pp.string ctx (String.trim s)

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
      Pp.string ctx (if Pp.minified ctx then "to (" else " to (");
      pp_scope_selector ctx e;
      Pp.string ctx ")"
  | None -> ());
  Pp.sp ctx ();
  Pp.braces pp_block ctx content

and pp_keyframes_block_statement ctx header name frames =
  Pp.string ctx header;
  pp_keyframes_name ctx name;
  Pp.sp ctx ();
  Pp.braced_list ~sep:Pp.cut pp_keyframe ctx frames

and pp_page_with_margins_body ctx descriptors margins =
  Pp.braces
    (fun ctx () ->
      Pp.cut ctx ();
      Pp.nest 2
        (fun ctx () ->
          Pp.list ~sep:Pp.semicolon_cut Declaration.pp_declaration ctx
            descriptors;
          if descriptors <> [] && margins <> [] then (
            Pp.semicolon ctx ();
            Pp.cut ctx ());
          Pp.list ~sep:Pp.cut pp_page_margin_rule ctx margins)
        ctx ();
      Pp.cut ctx ())
    ctx ()

and pp_viewport_prefix_keyword = function
  | Standard -> "@viewport"
  | Ms_prefixed -> "@-ms-viewport"

and pp_viewport_descriptor ctx { name; value } =
  Pp.string ctx name;
  Pp.char ctx ':';
  Pp.string ctx value

and pp_viewport_statement ctx prefix descriptors =
  Pp.string ctx (pp_viewport_prefix_keyword prefix);
  Pp.sp ctx ();
  Pp.braced_semicolon_list pp_viewport_descriptor ctx descriptors

and trim_unknown_at_prelude prelude =
  let rec trim_end i =
    if i < 0 then 0
    else
      match prelude.[i] with
      | ';' | '}' | ' ' | '\t' | '\n' | '\r' -> trim_end (i - 1)
      | _ -> i + 1
  in
  let n = String.length prelude in
  String.sub prelude 0 (trim_end (n - 1))

and pp_unknown_at_rule_statement ctx name prelude block =
  (* CSS Syntax 3 section 5.4.2: an at-rule terminates on [;], [}], or EOF. When
     the Parser captures an unterminated nested block ([(...], [[...], [{...])
     its source slice can include the at-rule terminator or the enclosing
     block's close - don't echo them as part of the prelude or each round-trip
     stacks another [;]/[}]. *)
  let prelude = trim_unknown_at_prelude prelude in
  Pp.char ctx '@';
  (* CSS Syntax 3 section 9.1: an at-rule name with non-ident-continue code
     points (control chars, escapes, non-ASCII) must round-trip through
     [escape_ident] so the serialized [\6 T] re-tokenizes back to the same
     at-keyword token. *)
  Pp.string ctx (Parser.escape_ident name);
  if prelude <> "" then (
    Pp.sp ctx ();
    Pp.string ctx prelude);
  match block with
  | None -> Pp.semicolon ctx ()
  | Some body ->
      Pp.sp ctx ();
      Pp.char ctx '{';
      Pp.string ctx body;
      Pp.char ctx '}'

and pp_statement : statement Pp.t =
 fun ctx -> function
  | Rule rule -> pp_rule ctx rule
  | Declarations raw_decls -> pp_declarations_statement ctx raw_decls
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
      Pp.list ~sep:Pp.comma Pp.string ctx names;
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
      Pp.braced_semicolon_list Declaration.pp_declaration ctx declarations
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
      Pp.string ctx name;
      Pp.sp ctx ();
      Pp.braced_semicolon_list pp_counter_style_descriptor ctx descriptors
  | Page (selector, raw_declarations) ->
      Pp.string ctx "@page";
      pp_page_selector ctx selector;
      Pp.sp ctx ();
      let declarations =
        Declaration.resolve_theme_guards ctx raw_declarations
      in
      Pp.braced_semicolon_list Declaration.pp_declaration ctx declarations
  | Page_with_margins (selector, descriptors, margins) ->
      Pp.string ctx "@page";
      pp_page_selector ctx selector;
      Pp.sp ctx ();
      pp_page_with_margins_body ctx descriptors margins
  | Font_palette_values (name, descriptors) ->
      Pp.string ctx "@font-palette-values ";
      Pp.string ctx name;
      Pp.sp ctx ();
      Pp.braced_semicolon_list pp_font_palette_descriptor ctx descriptors
  | Font_feature_values (families, blocks) ->
      Pp.string ctx "@font-feature-values ";
      Pp.list ~sep:Pp.comma Properties.pp_font_family ctx families;
      Pp.sp ctx ();
      Pp.braced_list ~sep:Pp.cut pp_font_feature_values_block ctx blocks
  | View_transition descriptors ->
      Pp.string ctx "@view-transition";
      Pp.sp ctx ();
      Pp.braced_semicolon_list pp_view_transition_descriptor ctx descriptors
  | Position_try (name, declarations) ->
      Pp.string ctx "@position-try ";
      Pp.string ctx name;
      Pp.sp ctx ();
      Pp.braced_semicolon_list Declaration.pp_declaration ctx declarations
  | Viewport (prefix, descriptors) ->
      pp_viewport_statement ctx prefix descriptors
  | Unknown_at_rule { name; prelude; block } ->
      pp_unknown_at_rule_statement ctx name prelude block

and font_face_participates descriptors =
  List.exists (function Font_family _ -> true | _ -> false) descriptors
  && List.exists (function Src _ -> true | _ -> false) descriptors

and should_print_statement ctx = function
  | Font_face descriptors when Pp.minified ctx ->
      font_face_participates descriptors
  | _ -> true

and printable_statements ctx statements =
  if Pp.minified ctx then List.filter (should_print_statement ctx) statements
  else statements

and pp_block : block Pp.t =
 fun ctx statements ->
  let statements = printable_statements ctx statements in
  let pp_statement_sep ctx = function
    | Declarations decls when decls <> [] && Pp.minified ctx ->
        Pp.semicolon ctx ()
    | _ -> ()
  in
  (* Block printing for at-rules (@media, @supports, etc.) The braces helper
     adds nest 1 and indent for the first item only. Subsequent items need
     explicit indentation and blank line separation to match Tailwind format. *)
  match statements with
  | [] -> ()
  | [ s ] -> pp_statement ctx s
  | s :: rest ->
      pp_statement ctx s;
      pp_statement_sep ctx s;
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
            pp_statement_sep ctx stmt;
            pp_rest rest
      in
      pp_rest rest

let is_layer_block = function Layer _ -> true | _ -> false

let pp_stylesheet : stylesheet Pp.t =
 fun ctx statements ->
  let statements = printable_statements ctx statements in
  let pp_statement_sep ctx = function
    | Declarations decls when decls <> [] && Pp.minified ctx ->
        Pp.semicolon ctx ()
    | _ -> ()
  in
  let rec loop = function
    | [] -> ()
    | [ s ] -> pp_statement ctx s
    | s :: (next :: _ as rest) ->
        pp_statement ctx s;
        pp_statement_sep ctx s;
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
    (* In pretty mode, lead with a blank line to separate the CSS body from any
       tooling output that printed before it (matches Lightning CSS's and
       prettier's pretty-print convention). The minified form stays compact. *)
    if (not minify) && mode <> Inline && header = "" && statements <> [] then
      Pp.char ctx '\n';
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
  let selector_str = Cursor.drain_until_block_as_string ~trim:true r in
  let keyframe_selector =
    try Keyframe.selector_of_string selector_str
    with Invalid_argument msg -> Cursor.err_invalid r msg
  in
  let declarations =
    Cursor.braces
      (fun inner ->
        Declaration.read_declarations inner
        |> List.filter (fun decl -> not (Declaration.is_important decl)))
      r
  in
  { keyframe_selector; keyframe_declarations = declarations }

(* Helper functions for reading specific at-rules *)

(* CSS Syntax section 8.2 reserves [@charset "UTF-8";] as the byte-stream
   decoder hint. The exact form -- uppercase label, double quotes, semicolon --
   is recognised; any other [@charset] (lowercase, single quotes, different
   label, no terminating ';') is a syntax error per section 8.3. *)
let read_charset (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "charset" r;
  Cursor.ws r;
  let encoding =
    match Cursor.string_with_quote_opt r with
    | Some (value, '"') when value = "UTF-8" -> value
    | _ ->
        Error.fail_bad_value (Cursor.position r) ~property:"@charset"
          ~reason:"@charset only recognises \"UTF-8\""
  in
  Cursor.ws r;
  if Cursor.peek_semicolon r then Cursor.skip r
  else
    Error.fail_bad_value (Cursor.position r) ~property:"@charset"
      ~reason:"@charset must end with ';'";
  Charset encoding

(* Re-anchor [Supports.of_string]'s typed error at the caller's [loc] so the
   partial-parse catch in [read_statement_of_rule] surfaces it as a warning at
   the surrounding rule, not at [Loc.dummy]. *)
let supports_condition ~loc condition =
  try Supports.of_string condition
  with Error.Parse_error { kind = Bad_condition { reason; _ }; _ } ->
    Error.fail_bad_condition loc ~at_rule:"@supports" ~reason

(* CSS Cascade section 6.4.2: a layer name is one or more idents joined by '.'
   with no whitespace around the dot. CSS-wide keywords are reserved. *)
let read_layer_name_component (r : Cursor.t) : string =
  let reserved = function
    | "initial" | "inherit" | "unset" | "revert" | "revert-layer" -> true
    | _ -> false
  in
  let buf = Buffer.create 16 in
  let add_part part =
    if reserved (String.lowercase_ascii part) then
      Cursor.err_invalid r ("layer name reserves CSS-wide keyword: " ^ part);
    Buffer.add_string buf part
  in
  add_part (Cursor.ident ~keep_case:true r);
  let rec extend () =
    match Cursor.peek_raw r with
    | Some (Component.Preserved { kind = Token.Delim "."; _ }) ->
        Cursor.skip r;
        let next =
          match Cursor.peek_raw r with
          | Some (Component.Preserved { kind = Token.Ident s; _ }) ->
              Cursor.skip r;
              s
          | _ -> Cursor.err_expected r "ident after '.' in layer name"
        in
        Buffer.add_char buf '.';
        add_part next;
        extend ()
    | _ -> ()
  in
  extend ();
  Buffer.contents buf

(* CSS Conditional Rules section 3 [@import] prelude: [<url> [layer |
   layer(<layer-name>)]? [supports(<supports-condition>)]? <media-query-list>?].
   The [~keep_url_repr] flavour preserves the original quote / [url(...)] form
   in [url] so the at-rule dispatch can round-trip author input verbatim; the
   canonical flavour ([keep_url_repr=false]) stores the decoded string for the
   top-level [read_import_rule] reader. *)
let read_import_url ~keep_url_repr (r : Cursor.t) =
  let pos = Cursor.position r in
  let value = Cursor.one_of [ Cursor.url; Cursor.string ] r in
  if keep_url_repr then
    let end_pos = Cursor.position r in
    match Cursor.source r with
    | Some source
      when end_pos.start_pos <= String.length source
           && pos.start_pos <= end_pos.start_pos
           && pos.start_pos >= 0 ->
        String.sub source pos.start_pos (end_pos.start_pos - pos.start_pos)
        |> String.trim
    | _ -> value
  else value

let read_import_layer (r : Cursor.t) =
  match
    Cursor.function_call "layer"
      (fun inner ->
        Cursor.ws inner;
        if Cursor.is_done inner then ""
        else
          let name = read_layer_name_component inner in
          Cursor.ws inner;
          Cursor.expect_eof inner;
          name)
      r
  with
  | Some _ as some -> some
  | None -> (
      match Cursor.peek_ident r with
      | Some s when String.lowercase_ascii s = "layer" ->
          let _ = Cursor.ident r in
          Some ""
      | _ -> None)

let read_import_supports (r : Cursor.t) =
  Cursor.function_call "supports"
    (fun inner ->
      let loc = Cursor.position inner in
      try
        Supports.of_string ~allow_unwrapped_decl:true
          (Cursor.string_of_remaining ~trim:true inner)
      with Failure reason ->
        Error.fail_bad_condition loc ~at_rule:"@supports" ~reason)
    r

let read_import_media (r : Cursor.t) =
  if Cursor.peek_semicolon r || Cursor.is_done r then None
  else
    let loc = Cursor.position r in
    let raw = Cursor.consume_until_semicolon ~trim:true r in
    match Media.of_string_strict raw with
    | media -> Some media
    | exception Failure reason ->
        Error.fail_bad_condition loc ~at_rule:"@media" ~reason

let read_import_prelude ~keep_url_repr (r : Cursor.t) : import_rule =
  Cursor.expect_at_keyword "import" r;
  Cursor.ws r;
  let url = read_import_url ~keep_url_repr r in
  Cursor.ws r;
  let layer = read_import_layer r in
  Cursor.ws r;
  let supports = read_import_supports r in
  Cursor.ws r;
  let media = read_import_media r in
  if Cursor.peek_semicolon r then Cursor.skip r;
  { url; layer; supports; media }

let read_import (r : Cursor.t) : statement =
  Import (read_import_prelude ~keep_url_repr:true r)

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
        Quoted value
    | _ ->
        let pos = Cursor.position r in
        let value = Cursor.url r in
        let end_pos = Cursor.position r in
        let form =
          match Cursor.source r with
          | Some source
            when end_pos.start_pos <= String.length source
                 && pos.start_pos <= end_pos.start_pos
                 && pos.start_pos >= 0 ->
              let raw =
                String.sub source pos.start_pos
                  (end_pos.start_pos - pos.start_pos)
              in
              if String.contains raw '\'' then Url_quoted '\''
              else if String.contains raw '"' then Url_quoted '"'
              else Url_bare
          | _ -> Url_bare
        in
        Url (value, form)
  in
  Cursor.ws r;
  if Cursor.peek_semicolon r then Cursor.skip r;
  Namespace (prefix, uri)

let rec skip_bad_keyframe inner =
  match Cursor.peek_raw inner with
  | None -> ()
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
      ignore (Cursor.next_raw inner : Component.t option)
  | Some (Component.Block { node = { opening = Token.Curly; _ }; _ }) ->
      ignore (Cursor.next_raw inner : Component.t option)
  | Some _ ->
      ignore (Cursor.next_raw inner : Component.t option);
      skip_bad_keyframe inner

let read_keyframe_or_skip inner acc =
  let snap = Cursor.save inner in
  match read_keyframe inner with
  | frame -> `Frame frame
  | exception Cursor.Parse_error e ->
      (* Invalid keyframe rules are ignored, but the surrounding block keeps
         parsing. *)
      Cursor.restore inner snap;
      Cursor.push_warning inner e;
      skip_bad_keyframe inner;
      `Skip acc

let read_keyframes_step inner acc =
  match Cursor.peek inner with
  | Some (Component.Preserved { kind = Token.At_keyword name; _ }) ->
      Cursor.err_invalid inner ("@keyframes nested @" ^ name)
  | _ -> (
      match read_keyframe_or_skip inner acc with
      | `Frame frame -> frame :: acc
      | `Skip acc -> acc)

let read_keyframes_block inner =
  (* CSS Syntax 5.4.4: a [@keyframes] block lists keyframe rules; an invalid
     selector (e.g. [entry to] - [to] is not a [<length-percentage>]) only drops
     that rule, the surrounding block keeps parsing. *)
  let rec read_frames acc =
    Cursor.ws inner;
    if Cursor.is_done inner then List.rev acc
    else read_frames (read_keyframes_step inner acc)
  in
  read_frames []

(* CSS Animations 1 §3: [@keyframes <keyframes-name> { ... }], where
   [<keyframes-name> = <custom-ident> | <string>]. The reserved spellings
   ([none], the CSS-wide keywords, and [default]) are excluded from
   [<keyframes-name>] per the spec, but every mainstream minifier accepts them
   in [<string>] form, so cascade keeps them in the AST too: rejecting would
   leak unparsable input that tools downstream already preserve verbatim. *)
let read_keyframes_name r =
  Cursor.ws r;
  match Cursor.string_opt r with
  | Some s -> s
  | None -> Cursor.ident ~keep_case:true r

let read_keyframes_named at_keyword make_statement (r : Cursor.t) : statement =
  Cursor.with_context r ("@" ^ at_keyword) @@ fun () ->
  Cursor.expect_at_keyword at_keyword r;
  let name = read_keyframes_name r in
  Cursor.ws r;
  let frames = Cursor.braces read_keyframes_block r in
  make_statement name frames

let read_keyframes (r : Cursor.t) : statement =
  read_keyframes_named "keyframes"
    (fun name frames -> Keyframes (name, frames))
    r

let read_webkit_keyframes r =
  read_keyframes_named "-webkit-keyframes"
    (fun name frames -> Webkit_keyframes (name, frames))
    r

let read_moz_keyframes r =
  read_keyframes_named "-moz-keyframes"
    (fun name frames -> Moz_keyframes (name, frames))
    r

let read_moz_document_condition r =
  Cursor.ws r;
  match Cursor.next r with
  | Some (Component.Func { node = { name = "url-prefix"; arguments; _ }; _ }) ->
      let arg_cursor = Cursor.of_components arguments in
      Cursor.ws arg_cursor;
      let prefix =
        match Cursor.string_opt arg_cursor with
        | Some "" -> None
        | Some s -> Some s
        | None ->
            Cursor.ws arg_cursor;
            if Cursor.is_done arg_cursor then None
            else Cursor.err_expected arg_cursor "url-prefix string argument"
      in
      Cursor.ws arg_cursor;
      Cursor.expect_eof arg_cursor;
      Url_prefix prefix
  | _ -> Cursor.err_expected r "url-prefix()"

(* Read a font-face descriptor *)
(* Helper to read descriptor value after colon *)
let read_descriptor_value read_fn constructor r =
  Cursor.ws r;
  if not (Cursor.colon r) then Cursor.err_expected r "':'";
  Cursor.ws r;
  try
    let value = read_fn r in
    constructor value
  with Failure msg -> Cursor.err_invalid r msg

let read_descriptor_declaration (r : Cursor.t) : Declaration.declaration option
    =
  Cursor.ws r;
  if Cursor.is_done r then None
  else if Cursor.peek_semicolon r then (
    Cursor.skip r;
    None)
  else Declaration.read_declaration r

let replace_descriptor desc acc =
  desc
  :: List.filter
       (fun existing ->
         Declaration.property_name existing <> Declaration.property_name desc)
       acc

let read_descriptor_block normalize inner =
  let rec loop acc =
    match read_descriptor_declaration inner with
    | Some desc -> loop (normalize desc acc)
    | None ->
        Cursor.ws inner;
        if Cursor.is_done inner then List.rev acc else loop acc
  in
  loop []

let validate_font_weight_range r first second =
  match (first, second) with
  | ( (Properties.Weight a : Properties.font_weight),
      (Properties.Weight b : Properties.font_weight) )
    when a <= b ->
      ()
  | _ -> Cursor.err_invalid r "invalid font-weight descriptor range"

let read_font_weight_descriptor r =
  read_descriptor_value Declaration.read_property_value
    (fun value ->
      let c = Cursor.of_string value in
      let first = Properties.read_font_weight c in
      Cursor.ws c;
      if Cursor.is_done c then Font_weight first
      else
        let second = Properties.read_font_weight c in
        Cursor.ws c;
        Cursor.expect_eof c;
        validate_font_weight_range r first second;
        Font_weight_range (first, second))
    r

let read_font_style_descriptor r =
  read_descriptor_value Declaration.read_property_value
    (fun value ->
      let c = Cursor.of_string value in
      let first = Properties.read_font_style c in
      Cursor.ws c;
      if Cursor.is_done c then Font_style first
      else
        let second = Properties.read_font_style c in
        Cursor.ws c;
        Cursor.expect_eof c;
        Font_style_range (first, second))
    r

let validate_nonempty_descriptor r name value =
  if String.trim value = "" then
    Cursor.err_invalid r ("empty " ^ name ^ " descriptor")

let read_string_descriptor name constructor r =
  read_descriptor_value Declaration.read_property_value
    (fun value ->
      validate_nonempty_descriptor r name value;
      constructor value)
    r

let read_font_family_descriptor r =
  read_descriptor_value
    (fun r -> Cursor.list ~sep:Cursor.comma Properties.read_font_family r)
    (fun v -> Font_family v)
    r

let read_font_stretch_descriptor_value value =
  let c = Cursor.of_string value in
  let first = Properties.read_font_stretch c in
  Cursor.ws c;
  if Cursor.is_done c then Font_stretch first
  else (
    ignore (Properties.read_font_stretch c);
    Cursor.ws c;
    Cursor.expect_eof c;
    Font_stretch_range value)

let read_font_stretch_descriptor r =
  read_descriptor_value Declaration.read_property_value
    read_font_stretch_descriptor_value r

let read_unicode_range_descriptor r =
  read_descriptor_value
    (fun cur ->
      Cursor.list ~at_least:1 ~sep:Cursor.comma Properties.read_unicode_range
        cur)
    (fun vs -> Unicode_range vs)
    r

let read_font_face_desc name r =
  match name with
  | "font-family" -> read_font_family_descriptor r
  | "src" -> read_descriptor_value Font_face.read_src (fun v -> Src v) r
  | "font-style" -> read_font_style_descriptor r
  | "font-weight" -> read_font_weight_descriptor r
  | "font-stretch" -> read_font_stretch_descriptor r
  | "font-display" ->
      read_descriptor_value Properties.read_font_display
        (fun v -> Font_display v)
        r
  | "unicode-range" -> read_unicode_range_descriptor r
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
  | "font-tech" -> read_string_descriptor "font-tech" (fun v -> Font_tech v) r
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

let read_font_face_descriptor (r : Cursor.t) : font_face_descriptor option =
  Cursor.ws r;
  if Cursor.is_done r then None
  else if Cursor.peek_semicolon r then (
    Cursor.skip r;
    None)
  else
    let name = Cursor.ident ~keep_case:false r in
    let descriptor = read_font_face_desc name r in
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
  (* CSS Fonts 4 §11.2.1: missing [font-family] / [src] is a semantic mismatch,
     not a syntax one. [validate_partial_statement] flags it; the syntactic
     reader accepts. *)
  let descriptors = Cursor.braces read_font_face_block r in
  Font_face descriptors

let read_counter_style_system_value r =
  let system = Cursor.ident ~keep_case:false r in
  match String.lowercase_ascii system with
  | "cyclic" -> Cyclic
  | "numeric" -> Numeric
  | "alphabetic" -> Alphabetic
  | "symbolic" -> Symbolic
  | "fixed" ->
      Cursor.ws r;
      Fixed (Cursor.integer_opt r)
  | "additive" -> Additive
  | "extends" ->
      Cursor.ws r;
      Extends (Cursor.ident ~keep_case:true r)
  | _ -> Cursor.err_invalid r ("unknown counter-style system: " ^ system)

let read_counter_style_system_descriptor r =
  read_descriptor_value Declaration.read_property_value
    (fun value ->
      let c = Cursor.of_string value in
      let system = read_counter_style_system_value c in
      Cursor.ws c;
      Cursor.expect_eof c;
      Counter_system system)
    r

let read_counter_symbol r =
  match Cursor.string_opt r with
  | Some symbol -> symbol
  | None -> Cursor.ident ~keep_case:true r

let read_counter_symbols_descriptor r =
  read_descriptor_value Declaration.read_property_value
    (fun value ->
      let c = Cursor.of_string value in
      let symbols =
        Cursor.list ~at_least:1 ~sep:Cursor.ws read_counter_symbol c
      in
      Cursor.ws c;
      Cursor.expect_eof c;
      Counter_symbols symbols)
    r

let read_counter_symbol_descriptor constructor r =
  read_descriptor_value Declaration.read_property_value
    (fun value ->
      let c = Cursor.of_string value in
      let symbol = read_counter_symbol c in
      Cursor.ws c;
      Cursor.expect_eof c;
      constructor symbol)
    r

let read_counter_string_descriptor constructor r =
  read_descriptor_value
    (fun r ->
      let value = Declaration.read_property_value r in
      validate_nonempty_descriptor r "counter-style" value;
      value)
    constructor r

let read_counter_style_descriptor (r : Cursor.t) :
    counter_style_descriptor option =
  Cursor.ws r;
  if Cursor.is_done r then None
  else if Cursor.peek_semicolon r then (
    Cursor.skip r;
    None)
  else
    let name = Cursor.ident ~keep_case:false r |> String.lowercase_ascii in
    let descriptor =
      match name with
      | "system" -> read_counter_style_system_descriptor r
      | "symbols" -> read_counter_symbols_descriptor r
      | "suffix" -> read_counter_symbol_descriptor (fun s -> Counter_suffix s) r
      | "prefix" -> read_counter_symbol_descriptor (fun s -> Counter_prefix s) r
      | "fallback" ->
          read_counter_string_descriptor (fun s -> Counter_fallback s) r
      | "range" -> read_counter_string_descriptor (fun s -> Counter_range s) r
      | "pad" -> read_counter_string_descriptor (fun s -> Counter_pad s) r
      | "negative" ->
          read_counter_string_descriptor (fun s -> Counter_negative s) r
      | "additive-symbols" ->
          read_counter_string_descriptor (fun s -> Counter_additive_symbols s) r
      | "speak-as" ->
          read_counter_string_descriptor (fun s -> Counter_speak_as s) r
      | _ -> Cursor.err_invalid r ("unknown counter-style descriptor: " ^ name)
    in
    Cursor.ws r;
    if Cursor.peek_semicolon r then Cursor.skip r;
    Some descriptor

let counter_style_descriptor_rank = function
  | Counter_system _ -> 0
  | Counter_symbols _ -> 1
  | Counter_additive_symbols _ -> 2
  | Counter_prefix _ -> 3
  | Counter_suffix _ -> 4
  | Counter_range _ -> 5
  | Counter_pad _ -> 6
  | Counter_negative _ -> 7
  | Counter_fallback _ -> 8
  | Counter_speak_as _ -> 9

let replace_counter_style_descriptor desc acc =
  desc
  :: List.filter
       (fun existing ->
         counter_style_descriptor_rank existing
         <> counter_style_descriptor_rank desc)
       acc

let continue_descriptor_loop inner acc loop =
  Cursor.ws inner;
  if Cursor.is_done inner then List.rev acc else loop acc

let read_counter_style_descriptors r =
  Cursor.braces
    (fun inner ->
      let rec loop acc =
        match read_counter_style_descriptor inner with
        | Some desc -> loop (replace_counter_style_descriptor desc acc)
        | None -> continue_descriptor_loop inner acc loop
      in
      loop [])
    r

let counter_style_system descriptors =
  List.find_map
    (function Counter_system system -> Some system | _ -> None)
    descriptors

let counter_style_has_symbols descriptors =
  List.exists (function Counter_symbols _ -> true | _ -> false) descriptors

let counter_style_has_additive_symbols descriptors =
  List.exists
    (function Counter_additive_symbols _ -> true | _ -> false)
    descriptors

let counter_style_validate r descriptors =
  let system =
    match counter_style_system descriptors with
    | Some system -> system
    | None -> Cursor.err_invalid r "@counter-style requires a system descriptor"
  in
  match system with
  | Cyclic | Numeric | Alphabetic | Symbolic | Fixed _ ->
      if not (counter_style_has_symbols descriptors) then
        Cursor.err_invalid r
          "@counter-style system requires a symbols descriptor"
  | Additive ->
      if not (counter_style_has_additive_symbols descriptors) then
        Cursor.err_invalid r
          "@counter-style additive system requires additive-symbols"
  | Extends _ -> ()

let read_counter_style (r : Cursor.t) : statement =
  Cursor.with_context r "@counter-style" @@ fun () ->
  Cursor.expect_at_keyword "counter-style" r;
  Cursor.ws r;
  let name = Cursor.ident ~keep_case:true r in
  Cursor.ws r;
  let descriptors =
    read_counter_style_descriptors r
    |> List.stable_sort (fun a b ->
        compare
          (counter_style_descriptor_rank a)
          (counter_style_descriptor_rank b))
  in
  counter_style_validate r descriptors;
  Counter_style (name, descriptors)

(* CSS Paged Media 3 §3.1: a page selector is an optional page name followed by
   zero or more pseudo-pages from [:first | :left | :right | :blank]. *)
let page_selector_error r s =
  Cursor.err_invalid r ("invalid @page selector: " ^ s)

let page_selector_is_ident_char c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c = '-' || c = '_'

let page_selector_is_ws = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let rec page_selector_consume_ident s len i =
  if i < len && page_selector_is_ident_char s.[i] then
    page_selector_consume_ident s len (i + 1)
  else i

let rec page_selector_skip_ws s len i =
  if i < len && page_selector_is_ws s.[i] then
    page_selector_skip_ws s len (i + 1)
  else i

let validate_pseudo_page r s name =
  match name with
  | "first" | "left" | "right" | "blank" -> ()
  | _ -> page_selector_error r s

let validate_page_selector r selector =
  (* CSS Paged Media 3 section 3.1: [<page-selector> = <ident-token>?
     <pseudo-page>*]. Zero or more [<pseudo-page>] entries from the closed set
     [first | left | right | blank] are allowed, so [@page invoice:blank:first]
     is well-formed. *)
  let s = String.trim selector in
  let len = String.length s in
  let consume_ident = page_selector_consume_ident s len in
  let skip_ws = page_selector_skip_ws s len in
  let consume_pseudo_page i =
    if i >= len || s.[i] <> ':' then i
    else
      let start = i + 1 in
      let stop = consume_ident start in
      if stop = start then page_selector_error r s;
      let name = String.sub s start (stop - start) in
      validate_pseudo_page r s name;
      stop
  in
  let rec consume_pseudo_pages i =
    let stop = consume_pseudo_page i in
    if stop = i then i else consume_pseudo_pages stop
  in
  let rec consume_selectors i =
    let i = skip_ws i in
    if i >= len then ()
    else
      let after_ident = consume_ident i in
      let after_pseudo = consume_pseudo_pages after_ident in
      let after_pseudo = skip_ws after_pseudo in
      if after_pseudo >= len then ()
      else if s.[after_pseudo] = ',' then consume_selectors (after_pseudo + 1)
      else page_selector_error r s
  in
  consume_selectors 0

let page_descriptor_order =
  [
    "size";
    "margin";
    "margin-left";
    "margin-right";
    "margin-top";
    "margin-bottom";
    "bleeds";
    "marks";
  ]

let allowed_page_descriptors = page_descriptor_order

let allowed_page_margin_names =
  [
    "top-left";
    "top-center";
    "top-right";
    "right-top";
    "right-middle";
    "right-bottom";
    "bottom-right";
    "bottom-center";
    "bottom-left";
    "left-bottom";
    "left-middle";
    "left-top";
  ]

let read_page_descriptor r =
  match read_descriptor_declaration r with
  | Some desc
    when List.mem (Declaration.property_name desc) allowed_page_descriptors ->
      desc
  | Some desc ->
      Cursor.err_invalid r
        ("invalid @page descriptor: " ^ Declaration.property_name desc)
  | None -> Cursor.err_expected r "@page descriptor"

let allowed_page_margin_descriptors = "content" :: allowed_page_descriptors

let replace_page_margin_descriptor r desc acc =
  if List.mem (Declaration.property_name desc) allowed_page_margin_descriptors
  then replace_descriptor desc acc
  else
    Cursor.err_invalid r
      ("invalid page margin descriptor: " ^ Declaration.property_name desc)

let read_page_margin_rule r =
  match Cursor.peek r with
  | Some (Component.Preserved { kind = Token.At_keyword name; _ })
    when List.mem name allowed_page_margin_names ->
      Cursor.skip r;
      Cursor.ws r;
      (* CSS Paged Media §5: a page-margin box accepts every descriptor valid in
         [@page] plus [content]. *)
      let margin_descriptors =
        Cursor.braces
          (fun inner ->
            read_descriptor_block (replace_page_margin_descriptor inner) inner)
          r
      in
      if margin_descriptors = [] then
        Cursor.err_invalid r "page margin rule requires descriptors";
      { margin_name = name; margin_descriptors }
  | Some (Component.Preserved { kind = Token.At_keyword name; _ }) ->
      Cursor.err_invalid r ("unknown page margin rule: @" ^ name)
  | _ -> Cursor.err_expected r "page margin rule"

let read_page_body inner =
  let rec loop descriptors margins =
    Cursor.ws inner;
    match Cursor.peek inner with
    | None -> (List.rev descriptors, List.rev margins)
    | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
        Cursor.skip inner;
        loop descriptors margins
    | Some (Component.Preserved { kind = Token.At_keyword _; _ }) ->
        let margin = read_page_margin_rule inner in
        loop descriptors (margin :: margins)
    | Some _ ->
        let desc = read_page_descriptor inner in
        loop (replace_descriptor desc descriptors) margins
  in
  loop [] []

let read_page (r : Cursor.t) : statement =
  Cursor.with_context r "@page" @@ fun () ->
  Cursor.expect_at_keyword "page" r;
  Cursor.ws r;
  let selector =
    let s = Cursor.drain_until_block_as_string ~trim:true r in
    if s = "" then None
    else (
      validate_page_selector r s;
      Some s)
  in
  let descriptors, margins = Cursor.braces read_page_body r in
  Page_with_margins (selector, descriptors, margins)

let validate_dashed_ident r name context =
  if not (String.starts_with ~prefix:"--" name) then
    Cursor.err_invalid r (context ^ " name must be a dashed ident")

let font_palette_descriptor_kind = function
  | Palette_font_family _ -> "font-family"
  | Base_palette _ -> "base-palette"
  | Override_colors _ -> "override-colors"

let replace_font_palette_descriptor desc acc =
  desc
  :: List.filter
       (fun existing ->
         font_palette_descriptor_kind existing
         <> font_palette_descriptor_kind desc)
       acc

let read_base_palette_value inner =
  match Cursor.number inner with
  | n ->
      if n < 0. || Float.floor n <> n then
        Cursor.err_invalid inner "base-palette index must be non-negative";
      Index (Float.to_int n)
  | exception Cursor.Parse_error _ -> (
      match Cursor.ident ~keep_case:false inner with
      | "light" -> Light
      | "dark" -> Dark
      | ident when ident <> "" -> Palette_ident ident
      | _ -> Cursor.err_expected inner "base-palette value")

let read_override_color_entry c =
  let index = Cursor.number c in
  if index < 0. || Float.floor index <> index then
    Cursor.err_invalid c "override-colors index must be non-negative";
  Cursor.ws c;
  let color = Values.read_color c in
  Cursor.ws c;
  (Float.to_int index, color)

let read_font_palette_descriptor outer inner =
  Cursor.ws inner;
  if Cursor.is_done inner then None
  else if Cursor.peek_semicolon inner then (
    Cursor.skip inner;
    None)
  else
    let desc_name = Cursor.ident ~keep_case:false inner in
    Cursor.ws inner;
    if not (Cursor.colon inner) then Cursor.err_expected inner "':'";
    Cursor.ws inner;
    let desc =
      match desc_name with
      | "font-family" ->
          Palette_font_family
            (Cursor.list ~sep:Cursor.comma Properties.read_font_family inner)
      | "base-palette" -> Base_palette (read_base_palette_value inner)
      | "override-colors" ->
          Override_colors
            (Cursor.list ~sep:Cursor.comma ~at_least:1 read_override_color_entry
               inner)
      | name ->
          Cursor.err_invalid outer
            ("unknown font-palette-values descriptor: " ^ name)
    in
    Cursor.ws inner;
    if Cursor.peek_semicolon inner then Cursor.skip inner;
    Some desc

let read_font_palette_descriptors outer =
  let rec loop inner acc =
    match read_font_palette_descriptor outer inner with
    | Some desc -> loop inner (replace_font_palette_descriptor desc acc)
    | None ->
        Cursor.ws inner;
        if Cursor.is_done inner then List.rev acc else loop inner acc
  in
  Cursor.braces (fun inner -> loop inner []) outer

let font_palette_descriptor_rank = function
  | Palette_font_family _ -> 0
  | Base_palette _ -> 1
  | Override_colors _ -> 2

let read_font_palette_values (r : Cursor.t) : statement =
  Cursor.with_context r "@font-palette-values" @@ fun () ->
  Cursor.expect_at_keyword "font-palette-values" r;
  Cursor.ws r;
  let name = Cursor.ident ~keep_case:true r in
  validate_dashed_ident r name "@font-palette-values";
  Cursor.ws r;
  let descriptors =
    read_font_palette_descriptors r
    |> List.stable_sort (fun a b ->
        compare
          (font_palette_descriptor_rank a)
          (font_palette_descriptor_rank b))
  in
  if descriptors = [] then
    Cursor.err_invalid r "@font-palette-values requires descriptors";
  Font_palette_values (name, descriptors)

let valid_font_feature_values_block = function
  | "styleset" | "character-variant" | "stylistic" | "swash" | "ornaments"
  | "annotation" ->
      true
  | _ -> false

let read_font_feature_value_entry outer inner =
  Cursor.ws inner;
  if Cursor.is_done inner then None
  else if Cursor.peek_semicolon inner then (
    Cursor.skip inner;
    None)
  else
    let name = Cursor.ident ~keep_case:true inner in
    Cursor.ws inner;
    if not (Cursor.colon inner) then Cursor.err_expected inner "':'";
    Cursor.ws inner;
    let indexes = Cursor.list ~at_least:1 Cursor.int inner in
    List.iter
      (fun index ->
        if index <= 0 then
          Cursor.err_invalid outer
            "@font-feature-values indexes must be positive")
      indexes;
    Cursor.ws inner;
    if Cursor.peek_semicolon inner then Cursor.skip inner;
    Some (name, indexes)

let replace_font_feature_value ((name, _) as entry) acc =
  entry :: List.filter (fun (existing, _) -> existing <> name) acc

let skip_semicolon_tail inner =
  let rec loop () =
    match Cursor.next_raw inner with
    | None | Some (Component.Preserved { kind = Token.Semicolon; _ }) -> ()
    | Some _ -> loop ()
  in
  loop ()

let read_font_feature_values_entries outer =
  let rec loop inner acc =
    match read_font_feature_value_entry outer inner with
    | Some entry -> loop inner (replace_font_feature_value entry acc)
    | None ->
        Cursor.ws inner;
        if Cursor.is_done inner then List.rev acc else loop inner acc
    | exception Error.Parse_error e ->
        Cursor.push_warning inner e;
        skip_semicolon_tail inner;
        loop inner acc
  in
  Cursor.braces (fun inner -> loop inner []) outer

let read_font_feature_values_block outer inner =
  Cursor.ws inner;
  match Cursor.at_keyword_opt inner with
  | None -> Cursor.err_expected inner "@font-feature-values nested at-rule"
  | Some name ->
      let name = String.lowercase_ascii name in
      if not (valid_font_feature_values_block name) then
        Cursor.err_invalid outer ("unknown @font-feature-values block: @" ^ name);
      (name, read_font_feature_values_entries inner)

let read_font_feature_values_blocks outer =
  let rec loop inner acc =
    Cursor.ws inner;
    if Cursor.is_done inner then List.rev acc
    else loop inner (read_font_feature_values_block outer inner :: acc)
  in
  Cursor.braces (fun inner -> loop inner []) outer

let read_font_feature_values (r : Cursor.t) : statement =
  Cursor.with_context r "@font-feature-values" @@ fun () ->
  Cursor.expect_at_keyword "font-feature-values" r;
  Cursor.ws r;
  let families =
    Cursor.list ~sep:Cursor.comma ~at_least:1 Properties.read_font_family r
  in
  Cursor.ws r;
  let blocks = read_font_feature_values_blocks r in
  if blocks = [] then
    Cursor.err_invalid r "@font-feature-values requires feature blocks";
  Font_feature_values (families, blocks)

let expect_view_transition_block r =
  match Cursor.peek r with
  | Some (Component.Block { node = { opening = Token.Curly; _ }; _ }) -> ()
  | Some _ -> Cursor.err_invalid r "@view-transition does not take a prelude"
  | None -> Cursor.err_expected r "'{'"

let same_view_transition_descriptor a b =
  match (a, b) with
  | Navigation _, Navigation _ | Types _, Types _ -> true
  | _ -> false

let replace_view_transition_descriptor desc acc =
  desc
  :: List.filter
       (fun existing -> not (same_view_transition_descriptor existing desc))
       acc

let read_view_transition_navigation inner =
  match Cursor.ident ~keep_case:false inner with
  | "auto" -> Navigation `Auto
  | "none" -> Navigation `None
  | _ ->
      Cursor.err_invalid inner "invalid @view-transition navigation descriptor"

let read_view_transition_types inner =
  match Cursor.ident ~keep_case:true inner with
  | "none" -> Types None
  | first ->
      let rec names acc =
        Cursor.ws inner;
        match Cursor.ident_opt inner with
        | Some name -> names (name :: acc)
        | None -> List.rev acc
      in
      Types (Some (names [ first ]))

let read_view_transition_descriptor outer inner =
  Cursor.ws inner;
  if Cursor.is_done inner then None
  else if Cursor.peek_semicolon inner then (
    Cursor.skip inner;
    None)
  else
    let desc_name = Cursor.ident ~keep_case:false inner in
    Cursor.ws inner;
    if not (Cursor.colon inner) then Cursor.err_expected inner "':'";
    Cursor.ws inner;
    let desc =
      match desc_name with
      | "navigation" -> read_view_transition_navigation inner
      | "types" -> read_view_transition_types inner
      | name ->
          Cursor.err_invalid outer
            ("unknown @view-transition descriptor: " ^ name)
    in
    Cursor.ws inner;
    if Cursor.peek_semicolon inner then Cursor.skip inner;
    Some desc

let read_view_transition_descriptors outer =
  Cursor.braces
    (fun inner ->
      let rec loop acc =
        match read_view_transition_descriptor outer inner with
        | Some desc -> loop (replace_view_transition_descriptor desc acc)
        | None -> continue_descriptor_loop inner acc loop
      in
      loop [])
    outer

let read_view_transition (r : Cursor.t) : statement =
  Cursor.with_context r "@view-transition" @@ fun () ->
  Cursor.expect_at_keyword "view-transition" r;
  Cursor.ws r;
  expect_view_transition_block r;
  let descriptors = read_view_transition_descriptors r in
  if descriptors = [] then
    Cursor.err_invalid r "@view-transition requires descriptors";
  View_transition descriptors

let declaration_order_rank decl =
  match Declaration.property_name decl with
  | "top" -> 0
  | "right" -> 1
  | "bottom" -> 2
  | "left" -> 3
  | "width" -> 4
  | "height" -> 5
  | "inset-inline-start" -> 6
  | "inset-inline-end" -> 7
  | "inset-block-start" -> 8
  | "inset-block-end" -> 9
  | "margin-inline" -> 10
  | _ -> 100

let read_position_try (r : Cursor.t) : statement =
  Cursor.with_context r "@position-try" @@ fun () ->
  Cursor.expect_at_keyword "position-try" r;
  Cursor.ws r;
  let name = Cursor.ident ~keep_case:true r in
  validate_dashed_ident r name "@position-try";
  Cursor.ws r;
  let declarations =
    Cursor.braces
      (fun inner ->
        let declarations = Declaration.read_declarations inner in
        Cursor.ws inner;
        Cursor.expect_eof inner;
        declarations)
      r
    |> List.stable_sort (fun a b ->
        compare (declaration_order_rank a) (declaration_order_rank b))
  in
  if declarations = [] then
    Cursor.err_invalid r "@position-try requires descriptors";
  Position_try (name, declarations)

let rec read_viewport_descriptor (r : Cursor.t) : viewport_descriptor option =
  Cursor.ws r;
  match Cursor.peek r with
  | None -> None
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
      ignore (Cursor.next_raw r);
      read_viewport_descriptor r
  | Some (Component.Preserved { kind = Token.Ident name; _ }) ->
      ignore (Cursor.next_raw r);
      Cursor.ws r;
      if not (Cursor.colon r) then Cursor.err_expected r "':'";
      Cursor.ws r;
      let value = String.trim (Cursor.consume_until_semicolon ~trim:true r) in
      Some { name; value }
  | Some _ -> Cursor.err_expected r "<viewport-descriptor>"

let read_viewport_descriptors r =
  Cursor.braces
    (fun inner ->
      let rec loop acc =
        match read_viewport_descriptor inner with
        | None -> List.rev acc
        | Some d -> loop (d :: acc)
      in
      let descriptors = loop [] in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      descriptors)
    r

let read_viewport_with_prefix prefix at_keyword (r : Cursor.t) : statement =
  Cursor.with_context r ("@" ^ at_keyword) @@ fun () ->
  Cursor.expect_at_keyword at_keyword r;
  Cursor.ws r;
  Viewport (prefix, read_viewport_descriptors r)

let read_viewport = read_viewport_with_prefix Standard "viewport"
let read_ms_viewport = read_viewport_with_prefix Ms_prefixed "-ms-viewport"

let trim_unknown_block_body body =
  let rec trim_end i =
    if i < 0 then 0
    else
      match body.[i] with
      | '}' | ' ' | '\t' | '\n' | '\r' -> trim_end (i - 1)
      | _ -> i + 1
  in
  let n = String.length body in
  String.sub body 0 (trim_end (n - 1))

let unknown_block_body slice value =
  match value with
  | [] -> ""
  | _ ->
      let first = Component.source_loc (List.hd value) in
      let last =
        Component.source_loc (List.nth value (List.length value - 1))
      in
      slice first.Loc.start_pos last.Loc.end_pos |> trim_unknown_block_body

(* CSS Syntax 3 §5.4.2 "consume an at-rule": after the at-keyword has been
   consumed, walk components until we hit [;] (no block) or [{...}] (block). Raw
   prelude/block strings are sliced from the original source so the at-rule
   round-trips byte-for-byte even when its grammar is unknown. *)
let read_unknown_at_rule name (r : Cursor.t) : statement =
  let source = Option.value (Cursor.source r) ~default:"" in
  let source_len = String.length source in
  let slice start stop =
    if start < 0 || stop > source_len || start > stop then ""
    else String.sub source start (stop - start)
  in
  let prelude_start = ref (-1) in
  let prelude_end = ref (-1) in
  let block = ref Option.None in
  let rec gather () =
    match Cursor.peek r with
    | None -> ()
    | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
        ignore (Cursor.next_raw r)
    | Some (Component.Block { node = { opening = Token.Curly; value; _ }; _ })
      ->
        (* CSS Syntax 3 section 5.4.2: when an unterminated nested block
           ([(...], [[...]) inside the at-rule's body extends to EOF, the
           Parser's source slice carries the close [}] and any trailing
           whitespace from outside the block - trim them so the serializer
           re-adds its own [}] without stacking. *)
        block := Option.Some (unknown_block_body slice value);
        ignore (Cursor.next_raw r)
    | Some comp ->
        let loc = Component.source_loc comp in
        if !prelude_start < 0 then prelude_start := loc.Loc.start_pos;
        prelude_end := loc.Loc.end_pos;
        ignore (Cursor.next_raw r);
        gather ()
  in
  gather ();
  let prelude =
    if !prelude_start < 0 then "" else slice !prelude_start !prelude_end
  in
  Unknown_at_rule { name; prelude; block = !block }

type property_reader_state = {
  syntax : Variables.any_syntax option;
  inherits : bool option;
  initial_value : string option;
}

let css_wide_keyword s =
  match String.lowercase_ascii (String.trim s) with
  | "initial" | "inherit" | "unset" | "revert" | "revert-layer" -> true
  | _ -> false

let read_property_initial_value r syntax str =
  if css_wide_keyword str then
    Cursor.err_invalid r "@property: initial-value cannot be CSS-wide keyword";
  let value_reader = Cursor.of_string str in
  let value = Variables.read_value value_reader syntax in
  Cursor.ws value_reader;
  Cursor.expect_eof value_reader;
  value

let conditional_args (fn : Component.func Component.node) =
  if not fn.node.terminated then failwith "unterminated conditional function";
  Cursor.string_of_components ~trim:true fn.node.arguments

let conditional_atom (fn : Component.func Component.node) =
  match String.lowercase_ascii fn.Component.node.name with
  | "media" -> Media_condition (Media.of_function_body (conditional_args fn))
  | "supports" ->
      Supports_condition_test
        (Supports.of_string ~allow_unwrapped_decl:true (conditional_args fn))
  | name -> failwith ("unknown conditional function: " ^ name)

let conditional_components components =
  let cursor = Cursor.of_components components in
  let peek_ident () =
    match Cursor.peek cursor with
    | Some (Component.Preserved { kind = Token.Ident name; _ }) ->
        Some (String.lowercase_ascii name)
    | _ -> None
  in
  let read_atom () =
    Cursor.ws cursor;
    match Cursor.peek cursor with
    | Some (Component.Func fn) ->
        Cursor.skip cursor;
        conditional_atom fn
    | _ -> failwith "expected conditional function"
  in
  let rec chain op acc =
    Cursor.ws cursor;
    match peek_ident () with
    | Some "and" ->
        (match op with
        | Some `Or -> failwith "mixed @when condition operators"
        | _ -> ());
        Cursor.skip cursor;
        chain (Some `And) (And (acc, read_atom ()))
    | Some "or" ->
        (match op with
        | Some `And -> failwith "mixed @when condition operators"
        | _ -> ());
        Cursor.skip cursor;
        chain (Some `Or) (Or (acc, read_atom ()))
    | _ -> acc
  in
  let condition = chain None (read_atom ()) in
  Cursor.ws cursor;
  if not (Cursor.is_done cursor) then failwith "trailing @when condition";
  condition

let follows_conditional = function When _ | Else _ -> true | _ -> false

let scope_prelude r prelude_components =
  let prelude = Cursor.string_of_components ~trim:true prelude_components in
  let rec split_at_to seen rest =
    match rest with
    | [] -> (List.rev seen, [])
    | Component.Preserved { kind = Token.Ident s; _ } :: tail
      when String.lowercase_ascii s = "to" ->
        (List.rev seen, tail)
    | hd :: tail -> split_at_to (hd :: seen) tail
  in
  let strip_parens cvs =
    match Cursor.string_of_components ~trim:true cvs with
    | s
      when String.length s >= 2 && s.[0] = '(' && s.[String.length s - 1] = ')'
      ->
        (true, String.sub s 1 (String.length s - 2) |> String.trim)
    | s -> (false, s)
  in
  if prelude = "" then (None, None)
  else
    let start_cvs, end_cvs = split_at_to [] prelude_components in
    let start_parens, start = strip_parens start_cvs in
    let end_parens, end_ = strip_parens end_cvs in
    if start_parens && start = "" then
      Cursor.err_invalid r "@scope start selector cannot be empty";
    if end_cvs <> [] && end_parens && end_ = "" then
      Cursor.err_invalid r "@scope end selector cannot be empty";
    let opt s = if s = "" then None else Some s in
    (opt start, opt end_)

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
      ("-moz-document", read_moz_document);
      ("when", read_when);
      ("else", read_else);
      ("supports-condition", read_supports_condition);
      ("starting-style", read_starting_style);
      ("scope", read_scope);
      ("keyframes", read_keyframes);
      ("-webkit-keyframes", read_webkit_keyframes);
      ("-moz-keyframes", read_moz_keyframes);
      ("font-face", read_font_face);
      ("counter-style", read_counter_style);
      ("page", read_page);
      ("font-palette-values", read_font_palette_values);
      ("font-feature-values", read_font_feature_values);
      ("view-transition", read_view_transition);
      ("position-try", read_position_try);
      ("viewport", read_viewport);
      ("-ms-viewport", read_ms_viewport);
      ("property", read_property_rule);
    ]
  in
  match Cursor.peek r with
  | Some (Component.Preserved { kind = Token.At_keyword name; loc; _ }) -> (
      match List.assoc_opt name table with
      | Some p -> p r
      | None ->
          (* CSS Syntax 3 §5.4.1: an at-rule with no registered handler is
             reported via a typed warning so [Css.of_string] partial-recovery
             can surface it to callers. The prelude/block stay in the AST as
             [Unknown_at_rule], and [Optimize.drop_unknown] removes them under
             minify when the user opted into spec-strict canonicalization. *)
          ignore (Cursor.next_raw r);
          let stmt = read_unknown_at_rule name r in
          Cursor.push_warning r (Error.unknown_at_rule loc name);
          stmt)
  | _ -> Rule (read_rule r)

and read_block (r : Cursor.t) : block =
  let rec read_statements acc =
    Cursor.ws r;
    if Cursor.is_done r then List.rev acc
    else
      let loc = Cursor.position r in
      let stmt = read_statement r in
      match stmt with
      | Import _ ->
          (* CSS Cascade L6 §2: @import is only valid at the top of the
             stylesheet. Drop a misplaced one rather than emitting it. *)
          Cursor.push_warning r
            (Error.bad_value loc ~property:"stylesheet"
               ~reason:"@import is only valid at the top of a stylesheet");
          read_statements acc
      | Else _ when not (List.exists follows_conditional acc) ->
          Cursor.err_invalid r "@else without preceding @when"
      | _ -> read_statements (stmt :: acc)
  in
  read_statements []

and read_starting_style (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "starting-style" r;
  Cursor.ws r;
  let content = Cursor.braces (fun inner -> read_block inner) r in
  Starting_style content

and read_when (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "when" r;
  Cursor.ws r;
  let prelude = Cursor.drain_until_block r in
  if List.for_all (fun cv -> Component.to_string cv |> String.trim = "") prelude
  then Cursor.err_invalid r "@when: missing condition";
  let condition =
    try conditional_components prelude
    with Failure msg -> Cursor.err_invalid r ("@when: " ^ msg)
  in
  let content = Cursor.braces (fun inner -> read_block inner) r in
  When (condition, content)

and read_else (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "else" r;
  Cursor.ws r;
  let prelude = Cursor.drain_until_block r in
  let condition =
    match
      List.filter
        (function
          | Component.Preserved { kind = Token.Whitespace; _ } -> false
          | _ -> true)
        prelude
    with
    | [] -> None
    | _ -> (
        match conditional_components prelude with
        | condition -> Some condition
        | exception Failure msg -> Cursor.err_invalid r ("@else: " ^ msg))
  in
  let content = Cursor.braces (fun inner -> read_block inner) r in
  Else (condition, content)

and read_supports_condition (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "supports-condition" r;
  Cursor.ws r;
  let name = Cursor.ident ~keep_case:true r in
  if not (String.length name >= 2 && String.sub name 0 2 = "--") then
    Cursor.err_invalid r "@supports-condition: name must start with '--'";
  Cursor.ws r;
  let declarations = Cursor.braces Declaration.read_declarations r in
  Supports_condition (name, declarations)

and read_media (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "media" r;
  Cursor.ws r;
  let condition_str = Cursor.drain_until_block_as_string ~trim:true r in
  let content = Cursor.braces (fun inner -> read_block inner) r in
  let condition =
    if String.length condition_str = 0 then Media.List []
    else
      try Media.of_string_strict condition_str
      with Failure reason ->
        Cursor.err_invalid r ("invalid @media condition: " ^ reason)
  in
  Media (condition, content)

and read_supports (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "supports" r;
  Cursor.ws r;
  let cond_loc = Cursor.position r in
  let condition = Cursor.drain_until_block_as_string ~trim:true r in
  if String.length condition = 0 then
    Cursor.err r "@supports rule requires a condition";
  let content = Cursor.braces (fun inner -> read_block inner) r in
  Supports (supports_condition ~loc:cond_loc condition, content)

and read_moz_document (r : Cursor.t) : statement =
  Cursor.with_context r "@-moz-document" @@ fun () ->
  Cursor.expect_at_keyword "-moz-document" r;
  Cursor.ws r;
  let prelude = Cursor.drain_until_block_as_string ~trim:true r in
  let prelude_cursor = Cursor.of_string prelude in
  let conditions =
    Cursor.list ~sep:Cursor.comma ~at_least:1 read_moz_document_condition
      prelude_cursor
  in
  Cursor.ws prelude_cursor;
  Cursor.expect_eof prelude_cursor;
  let content = Cursor.braces (fun inner -> read_block inner) r in
  Moz_document (conditions, content)

and read_scope (r : Cursor.t) : statement =
  (* CSS Scoping section 3: [@scope <start> to <end> { ... }]. The two selectors
     are kept as raw strings; the block is consumed normally. *)
  Cursor.expect_at_keyword "scope" r;
  Cursor.ws r;
  let prelude_components = Cursor.drain_until_block r in
  let scope_start, scope_end = scope_prelude r prelude_components in
  let content = Cursor.braces (fun inner -> read_block inner) r in
  Scope (scope_start, scope_end, content)

and read_container (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "container" r;
  Cursor.ws r;
  (* CSS Containment 3 section 4: [<container-name>] is a [<custom-ident>] that
     excludes the keywords [none], [and], [not], [or]. Without this filter,
     [@container not (width)] would parse [not] as the name and accept the
     malformed [(width)] as the query. *)
  let container_name =
    let snap = Cursor.save r in
    match Cursor.option Cursor.ident r with
    | Some name
      when List.mem (String.lowercase_ascii name) [ "none"; "and"; "not"; "or" ]
      ->
        Cursor.restore r snap;
        None
    | other -> other
  in
  Cursor.ws r;
  let condition_str = Cursor.drain_until_block_as_string ~trim:true r in
  let content = Cursor.braces (fun inner -> read_block inner) r in
  let condition =
    if condition_str = "" then None
    else
      match Container.of_string condition_str with
      | condition -> Some condition
      | exception Failure msg -> Cursor.err_invalid r msg
  in
  (* CSS Containment 3 section 4: [@container] requires a query (with an
     optional [<container-name>] in front). Bare [@container { ... }] is a parse
     error. *)
  if container_name = None && condition = None then
    Cursor.err_invalid r "@container requires a container query";
  Container (container_name, condition, content)

and read_layer_name (r : Cursor.t) : string = read_layer_name_component r

and read_layer (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "layer" r;
  Cursor.ws r;
  match Cursor.peek r with
  | Some (Component.Block { node = { opening = Token.Curly; _ }; _ }) ->
      let content = Cursor.braces (fun inner -> read_block inner) r in
      Layer (None, content)
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
      Cursor.err_invalid r "@layer requires at least one name"
  | _ -> (
      let first = read_layer_name r in
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
                read_layer_name r)
              r
          in
          Cursor.ws r;
          (match Cursor.peek r with
          | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
              Cursor.skip r
          | Some (Component.Block { node = { opening = Token.Curly; _ }; _ }) ->
              Cursor.err_invalid r
                "@layer with multiple names cannot have a block"
          | _ -> ());
          Layer_decl (first :: rest)
      | Some (Component.Block { node = { opening = Token.Curly; _ }; _ }) ->
          let content = Cursor.braces (fun inner -> read_block inner) r in
          Layer (Some first, content)
      | _ -> Cursor.err_invalid r "expected ';' or '{' after @layer name")

(* Helper: Read a block that can contain either bare declarations or statements.
   Used for CSS nesting contexts where content inside @media/@supports/etc can
   be either bare declarations (inheriting the parent selector) or nested
   rules. *)
and read_nesting_block (r : Cursor.t) : block =
  let rec read_items acc =
    Cursor.ws r;
    match read_nesting_item r with
    | `Done -> List.rev acc
    | `Skip -> read_items acc
    | `Stmt stmt -> read_items (stmt :: acc)
  in
  read_items []

and read_nesting_item r =
  match Cursor.peek r with
  | None -> `Done
  | Some (Component.Preserved { kind = Token.At_keyword _; _ }) ->
      `Stmt (read_statement r)
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
      Cursor.skip r;
      `Skip
  | _ -> read_nesting_declaration_or_statement r

and read_nesting_declaration_or_statement r =
  match Declaration.read_declaration r with
  | Some decl ->
      Cursor.ws r;
      `Stmt (Declarations (read_more_nested_decls r [ decl ]))
  | None -> `Stmt (read_statement r)

and read_more_nested_decls r acc =
  match Cursor.peek r with
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
      Cursor.skip r;
      Cursor.ws r;
      read_nested_decl_after_semicolon r acc
  | _ -> List.rev acc

and read_nested_decl_after_semicolon r acc =
  match Cursor.peek r with
  | None | Some (Component.Preserved { kind = Token.At_keyword _; _ }) ->
      List.rev acc
  | _ -> (
      match Declaration.read_declaration r with
      | Some d ->
          Cursor.ws r;
          read_more_nested_decls r (d :: acc)
      | None -> List.rev acc)

(* Helper: Read nested at-rule with declarations content *)
and read_nested_at_rule (r : Cursor.t) (at_rule : string)
    (_selector : Selector.t) : statement =
  Cursor.with_context r at_rule @@ fun () ->
  let name = String.sub at_rule 1 (String.length at_rule - 1) in
  Cursor.expect_at_keyword name r;
  Cursor.ws r;
  match at_rule with
  | "@container" -> read_nested_container_rule r
  | "@supports" -> read_nested_supports_rule r
  | "@media" -> read_nested_media_rule r
  | "@scope" -> read_nested_scope_rule r
  | _ -> Cursor.err_invalid r ("Unexpected nested at-rule: " ^ at_rule)

and read_nested_container_rule r =
  let container_name = Cursor.option Cursor.ident r in
  Cursor.ws r;
  let condition_str = Cursor.drain_until_block_as_string ~trim:true r in
  let content = Cursor.braces (fun inner -> read_nesting_block inner) r in
  let condition =
    if condition_str = "" then None
    else Some (Container.of_string condition_str)
  in
  Container (container_name, condition, content)

and read_nested_supports_rule r =
  let cond_loc = Cursor.position r in
  let condition = Cursor.drain_until_block_as_string ~trim:true r in
  let content = Cursor.braces (fun inner -> read_nesting_block inner) r in
  Supports (supports_condition ~loc:cond_loc condition, content)

and read_nested_media_rule r =
  let condition_str = Cursor.drain_until_block_as_string ~trim:true r in
  let content = Cursor.braces (fun inner -> read_nesting_block inner) r in
  Media (read_nested_media_condition condition_str, content)

and read_nested_media_condition condition_str =
  if String.length condition_str = 0 then Media.List []
  else
    try Media.of_string_strict condition_str
    with Failure _ -> Media.of_string "not all"

and read_nested_scope_rule r =
  let prelude_components = Cursor.drain_until_block r in
  let scope_start, scope_end = scope_prelude r prelude_components in
  let content = Cursor.braces (fun inner -> read_nesting_block inner) r in
  Scope (scope_start, scope_end, content)

and read_nested_at_within_rule (r : Cursor.t) (selector : Selector.t) :
    statement =
  match Cursor.peek r with
  | Some (Component.Preserved { kind = Token.At_keyword name; _ })
    when name = "supports" || name = "media" || name = "container"
         || name = "scope" ->
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
  | Some
      (Component.Preserved
         {
           kind =
             Token.At_keyword (("charset" | "import" | "namespace") as name);
           _;
         }) ->
      Cursor.err_invalid r ("Unexpected nested at-rule: @" ^ name)
  | _ -> read_statement r

and read_rule_selector r =
  let prelude = Cursor.drain_until_block r in
  (* Anchor selector-level EOF errors at the block's opening delimiter so a
     prelude like [.a >] reports its error just before the [{], not at the end
     of the whole stylesheet. The block itself stays in the parent cursor. *)
  let eof_loc =
    match Cursor.peek r with
    | Some (Component.Block { loc; _ }) ->
        Some (Loc.v ~start_pos:loc.start_pos ~end_pos:loc.start_pos)
    | _ -> None
  in
  let c = Cursor.sub ?eof_loc r prelude in
  (* Re-raise the original [Parse_error] so its loc/kind/path/snippet reach the
     caller intact. Previously we rewrapped via [Cursor.err r (Error.to_string
     e)], which erased the structured error and relocated it to the parent
     cursor's current position. *)
  Cursor.with_context c "selector" (fun () ->
      Selector.read_strict_selector_list c)

and is_bare_nesting_selector : Selector.t -> bool = function
  | Selector.Nesting -> true
  | _ -> false

and read_rule_item selector inner decls nested =
  match Cursor.peek inner with
  | None -> `Done (List.rev decls, List.rev nested)
  | Some (Component.Preserved { kind = Token.At_keyword _; _ }) ->
      let stmt = read_nested_at_within_rule inner selector in
      `Continue (decls, stmt :: nested)
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
      Cursor.skip inner;
      `Continue (decls, nested)
  | _ -> read_rule_decl_or_nested selector inner decls nested

and read_rule_decl_or_nested selector inner decls nested =
  match Declaration.read_declaration inner with
  | Some d ->
      Cursor.ws inner;
      if Cursor.peek_semicolon inner then Cursor.skip inner;
      `Continue (d :: decls, nested)
  | None -> read_nested_rule_or_done selector inner decls nested

and read_nested_rule_or_done selector inner decls nested =
  if Cursor.is_done inner then `Done (List.rev decls, List.rev nested)
  else
    let nr = read_rule inner in
    validate_nested_rule_selector inner selector nr.selector;
    `Continue (decls, Rule nr :: nested)

and validate_nested_rule_selector inner selector nested_selector =
  if
    is_bare_nesting_selector selector
    && is_bare_nesting_selector nested_selector
  then
    Cursor.err_invalid inner
      "bare nesting selector cannot directly nest another bare nesting selector"

and read_rule_body selector inner =
  let rec loop decls nested =
    Cursor.ws inner;
    if Cursor.recover inner then
      read_recovering_rule_item selector inner loop decls nested
    else
      match read_rule_item selector inner decls nested with
      | `Done result -> result
      | `Continue (decls, nested) -> loop decls nested
  in
  loop [] []

and read_recovering_rule_item selector inner loop decls nested =
  match read_rule_item selector inner decls nested with
  | `Done result -> result
  | `Continue (decls, nested) -> loop decls nested
  | exception Error.Parse_error e ->
      Cursor.push_warning inner e;
      skip_bad_rule_item inner;
      loop decls nested

and skip_bad_rule_item inner =
  match Cursor.next_raw inner with
  | None -> ()
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) -> ()
  | Some _ -> skip_bad_rule_item inner

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
  if not (String.length name >= 2 && String.sub name 0 2 = "--") then
    Cursor.err_invalid r ("@property: name must start with '--', got: " ^ name);
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
        | Some str -> Some (read_property_initial_value r syntax str)
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
          let value_str = Cursor.consume_until_semicolon ~trim:true r in
          state := { !state with initial_value = Some value_str }
      | _ -> Cursor.err_invalid r "unknown property descriptor");
      Cursor.ws r;
      if Cursor.peek_semicolon r then Cursor.skip r;
      loop ()
  in
  loop ()

type prelude_seen = {
  mutable charset_seen : bool;
  mutable import_seen : bool;
  mutable namespace_seen : bool;
  mutable body_seen : bool;
}

let new_prelude_seen () =
  {
    charset_seen = false;
    import_seen = false;
    namespace_seen = false;
    body_seen = false;
  }

let validate_stylesheet_prelude r state stmt =
  match stmt with
  | Charset _ ->
      if
        state.charset_seen || state.import_seen || state.namespace_seen
        || state.body_seen
      then Cursor.err_invalid r "@charset must precede all rules";
      state.charset_seen <- true
  | Layer_decl _ ->
      if state.import_seen || state.namespace_seen then state.body_seen <- true
  | Import _ ->
      if state.namespace_seen || state.body_seen then
        Cursor.err_invalid r "@import must precede style rules";
      state.import_seen <- true
  | Namespace _ ->
      if state.body_seen then
        Cursor.err_invalid r "@namespace must precede style rules";
      state.namespace_seen <- true
  | _ -> state.body_seen <- true

let validate_stylesheet_else r acc stmt =
  match stmt with
  | Else _ when not (List.exists follows_conditional acc) ->
      Cursor.err_invalid r "@else without preceding @when"
  | _ -> ()

let rec read_stylesheet_statements r state acc =
  Cursor.ws r;
  if Cursor.is_done r then List.rev acc
  else
    let stmt = read_statement r in
    validate_stylesheet_else r acc stmt;
    validate_stylesheet_prelude r state stmt;
    read_stylesheet_statements r state (stmt :: acc)

let read_stylesheet (r : Cursor.t) : stylesheet =
  Cursor.with_context r "stylesheet" (fun () ->
      read_stylesheet_statements r (new_prelude_seen ()) [])

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
      let tail_cv =
        match block with
        | Some b -> [ Component.Block b ]
        | None ->
            (* Section 5.4.2 "consume an at-rule" terminates on [;] or EOF and
               drops the terminator. Readers like [read_layer] and
               [read_charset] then expect a trailing semicolon; synthesize one
               so the replayed cursor matches the original token shape
               regardless of whether [;] or EOF ended the rule. *)
            [ Component.Preserved (Token.v ~kind:Token.Semicolon ~loc) ]
      in
      Cursor.of_components ?source ?meta ~recover:true
        ((at_cv :: prelude) @ tail_cv)

(* Validate one Parser-recovered rule to a typed statement, or convert the
   validator's [Parse_error] into a rule-level error and drop the rule. Per-
   declaration warnings accumulated on the cursor are drained separately by the
   caller. *)
let read_statement_of_rule ?source ?meta (rule : Component.rule) :
    Cursor.t * (statement, Error.t) result =
  let r = cursor_of_rule ?source ?meta rule in
  let result =
    match read_statement r with
    | stmt -> Ok stmt
    | exception Error.Parse_error e -> Error e
  in
  (r, result)

let rule_loc : Component.rule -> Loc.t = function
  | Qualified { loc; _ } | At { loc; _ } -> loc

let validate_partial_prelude () =
  let charset_seen = ref false in
  let import_seen = ref false in
  let namespace_seen = ref false in
  let body_seen = ref false in
  fun loc stmt ->
    let warning reason =
      Some (Error.bad_value loc ~property:"stylesheet" ~reason)
    in
    match stmt with
    | Charset _ ->
        let error =
          if !charset_seen || !import_seen || !namespace_seen || !body_seen then
            warning "@charset must precede all rules"
          else None
        in
        charset_seen := true;
        error
    | Layer_decl _ ->
        if !import_seen || !namespace_seen then body_seen := true;
        None
    | Import _ ->
        let error =
          if !namespace_seen || !body_seen then
            warning "@import must precede style rules"
          else None
        in
        import_seen := true;
        error
    | Namespace _ ->
        let error =
          if !body_seen then warning "@namespace must precede style rules"
          else None
        in
        namespace_seen := true;
        error
    | _ ->
        body_seen := true;
        None

let validate_partial_statement loc = function
  | Rule { selector; _ } when Selector.matches_nothing selector ->
      Some (Error.bad_selector loc "selector matches nothing")
  | Font_face descriptors when not (font_face_participates descriptors) ->
      Some
        (Error.bad_value loc ~property:"@font-face"
           ~reason:"missing font-family or src descriptor")
  | Font_palette_values (_, descriptors)
    when not
           (List.exists
              (function Base_palette _ -> true | _ -> false)
              descriptors) ->
      Some
        (Error.bad_value loc ~property:"@font-palette-values"
           ~reason:"missing base-palette descriptor")
  | (Keyframes (name, _) | Webkit_keyframes (name, _) | Moz_keyframes (name, _))
    when List.mem
           (String.lowercase_ascii name)
           [ "none"; "initial"; "inherit"; "unset"; "revert"; "revert-layer" ]
    ->
      Some
        (Error.bad_value loc ~property:"@keyframes"
           ~reason:"forbidden keyframes name")
  | _ -> None

(* @page descriptors, @font-face descriptors, etc. flow through
   [Declaration.read_declaration] just like ordinary rule declarations and show
   up as [Unknown_property "marks"] / [Unknown_property "src"] in the AST. The
   reader already enforces each at-rule's allowed-descriptor list, so an
   [Unknown_property] arriving in a descriptor block is by construction a known
   descriptor - skip the [is_invalid] check that would otherwise flag it as a
   spec-noncompliant unknown property. *)
let descriptor_has_typed_invalid_value = function
  | Declaration.Declaration { property = Properties.Unknown_property _; _ } ->
      false
  | decl -> Declaration.is_invalid decl

let rec statement_has_invalid_declaration = function
  | Rule { declarations; nested; _ } ->
      List.exists Declaration.is_invalid declarations
      || List.exists statement_has_invalid_declaration nested
  | Declarations decls -> List.exists Declaration.is_invalid decls
  | Media (_, block)
  | Container (_, _, block)
  | Supports (_, block)
  | Moz_document (_, block)
  | When (_, block)
  | Else (_, block)
  | Layer (_, block)
  | Starting_style block
  | Origin (_, block)
  | Scope (_, _, block) ->
      List.exists statement_has_invalid_declaration block
  | Page (_, decls) -> List.exists descriptor_has_typed_invalid_value decls
  | Page_with_margins (_, descs, margins) ->
      List.exists descriptor_has_typed_invalid_value descs
      || List.exists
           (fun { margin_descriptors; _ } ->
             List.exists descriptor_has_typed_invalid_value margin_descriptors)
           margins
  | Position_try (_, decls) | Supports_condition (_, decls) ->
      List.exists descriptor_has_typed_invalid_value decls
  | _ -> false

let validate_partial_invalid_declarations loc stmt =
  if statement_has_invalid_declaration stmt then
    Some
      (Error.bad_value loc ~property:"stylesheet"
         ~reason:"invalid declaration value")
  else None

let rec declaration_has_strict_warning = function
  | Declaration.Declaration { property = Properties.Color; value; _ } ->
      Values.color_has_specified_hue value
  | Declaration.Theme_guarded { decl; _ } -> declaration_has_strict_warning decl
  | _ -> false

let rec statement_has_strict_warning = function
  | Rule { declarations; nested; _ } ->
      List.exists declaration_has_strict_warning declarations
      || List.exists statement_has_strict_warning nested
  | Declarations decls -> List.exists declaration_has_strict_warning decls
  | Media (_, block)
  | Container (_, _, block)
  | Supports (_, block)
  | Moz_document (_, block)
  | When (_, block)
  | Else (_, block)
  | Layer (_, block)
  | Starting_style block
  | Origin (_, block)
  | Scope (_, _, block) ->
      List.exists statement_has_strict_warning block
  | Page (_, decls) -> List.exists declaration_has_strict_warning decls
  | Page_with_margins (_, descs, margins) ->
      List.exists declaration_has_strict_warning descs
      || List.exists
           (fun { margin_descriptors; _ } ->
             List.exists declaration_has_strict_warning margin_descriptors)
           margins
  | Position_try (_, decls) | Supports_condition (_, decls) ->
      List.exists declaration_has_strict_warning decls
  | _ -> false

let validate_partial_strict_warnings loc stmt =
  if statement_has_strict_warning stmt then
    Some
      (Error.bad_value loc ~property:"stylesheet"
         ~reason:"strict stylesheet warning")
  else None

let read_stylesheet_of_rules ?source ?meta (rules : Component.rule list) :
    stylesheet * Error.t list =
  let warnings = ref [] in
  let validate_prelude = validate_partial_prelude () in
  (* CSS Conditional Rules 5 section 4.6: [@else] is a continuation rule and
     must follow [@when] or another [@else]. A bare top-level [@else] is a parse
     error. *)
  let previous = ref None in
  let validate_else_orphan rule stmt =
    match stmt with
    | Else _
      when not (Option.fold ~none:false ~some:follows_conditional !previous) ->
        Some
          (Error.bad_value (rule_loc rule) ~property:"stylesheet"
             ~reason:"@else without preceding @when")
    | _ -> None
  in
  let statements =
    List.filter_map
      (fun rule ->
        let cursor, result = read_statement_of_rule ?source ?meta rule in
        (* Drain declaration-level warnings first so source order is preserved:
           decl warnings come from inside the rule, the rule-level error (if
           any) comes after them. *)
        List.iter
          (fun w -> warnings := w :: !warnings)
          (Cursor.drain_warnings cursor);
        match result with
        | Ok stmt -> (
            Option.iter
              (fun w -> warnings := w :: !warnings)
              (validate_prelude (rule_loc rule) stmt);
            Option.iter
              (fun w -> warnings := w :: !warnings)
              (validate_partial_statement (rule_loc rule) stmt);
            Option.iter
              (fun w -> warnings := w :: !warnings)
              (validate_partial_invalid_declarations (rule_loc rule) stmt);
            Option.iter
              (fun w -> warnings := w :: !warnings)
              (validate_partial_strict_warnings (rule_loc rule) stmt);
            match validate_else_orphan rule stmt with
            | Some w ->
                warnings := w :: !warnings;
                previous := Some stmt;
                None
            | None ->
                previous := Some stmt;
                Some stmt)
        | Error e ->
            warnings := e :: !warnings;
            None)
      rules
  in
  (statements, List.rev !warnings)

(* Top-level partial-recovery entry point: combine section 5.3 syntax warnings
   from [Parser.stylesheet] with per-rule typed-validation warnings. *)
let parse_stylesheet_partial ?(meta = Loc.default_meta_level) (source : string)
    : stylesheet * Error.t list =
  let reader = Reader.of_string source in
  let out = Parser.stylesheet ~meta reader in
  (* Snippets must be sliced from the preprocessed buffer so their offsets line
     up with the locs the lexer produced (see Cursor.of_string). *)
  let sheet, typed_warnings =
    read_stylesheet_of_rules ~source:(Reader.source reader) ~meta out.value
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
  | Rule rule ->
      Variables.vars_of_declarations rule.declarations
      @ vars_of_block rule.nested
  | Declarations decls -> Variables.vars_of_declarations decls
  | Media (_, block)
  | Container (_, _, block)
  | Supports (_, block)
  | Moz_document (_, block)
  | When (_, block)
  | Else (_, block)
  | Layer (_, block)
  | Starting_style block
  | Origin (_, block)
  | Scope (_, _, block) ->
      vars_of_block block
  | Font_face _ | Counter_style _ ->
      [] (* At-rule descriptors don't contribute CSS variables *)
  | Page (_, decls) -> Variables.vars_of_declarations decls
  | Page_with_margins (_, _, _) -> []
  | Position_try (_, decls) | Supports_condition (_, decls) ->
      Variables.vars_of_declarations decls
  | Viewport _ | Font_palette_values _ | Font_feature_values _
  | View_transition _ | Charset _ | Import _ | Namespace _ | Property _
  | Layer_decl _ | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _
  | Unknown_at_rule _ ->
      []

and vars_of_block (block : block) : Variables.any_var list =
  List.concat_map vars_of_statement block

let vars_of_stylesheet (ss : stylesheet) : Variables.any_var list =
  vars_of_block ss

(* Alias for API consistency *)
let read = read_stylesheet

(* Pretty-printer for import_rule *)
let pp_import_rule : import_rule Pp.t =
 fun ctx rule ->
  Pp.string ctx "@import";
  pp_import_components ctx rule;
  Pp.string ctx ";"

(* Reader for import_rule *)
let read_import_rule (r : Cursor.t) : import_rule =
  Cursor.ws r;
  read_import_prelude ~keep_url_repr:false r

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
type config_state = {
  mutable cfg_minify : bool;
  mutable cfg_mode : mode;
  mutable cfg_optimize : bool;
  mutable cfg_newline : bool;
}

let set_config_field state field value =
  match field with
  | "minify" -> state.cfg_minify <- value = "true"
  | "mode" -> state.cfg_mode <- (if value = "Inline" then Inline else Variables)
  | "optimize" -> state.cfg_optimize <- value = "true"
  | "newline" -> state.cfg_newline <- value = "true"
  | _ -> ()

let rec read_config_fields state inner =
  if Cursor.is_done inner then ()
  else
    let field_name = Cursor.ident inner in
    Cursor.expect '=' inner;
    let value = Cursor.ident inner in
    set_config_field state field_name value;
    if Cursor.peek_semicolon inner then Cursor.skip inner;
    read_config_fields state inner

let read_config (r : Cursor.t) : config =
  Cursor.braces
    (fun inner ->
      let state =
        {
          cfg_minify = false;
          cfg_mode = Variables;
          cfg_optimize = false;
          cfg_newline = false;
        }
      in
      read_config_fields state inner;
      {
        minify = state.cfg_minify;
        mode = state.cfg_mode;
        optimize = state.cfg_optimize;
        newline = state.cfg_newline;
      })
    r
