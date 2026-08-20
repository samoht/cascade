(** CSS stylesheet types and construction functions *)

include Stylesheet_intf

type t = stylesheet

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

let rec index_of x i : string list -> int option = function
  | [] -> None
  | y :: ys -> if x = y then Some i else index_of x (i + 1) ys

let cascade_layer_precedence_rank ~layer_order ~important
    (layer : string option) =
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
    (candidates : cascade_layer_candidate list) : cascade_layer_candidate option
    =
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
    (candidates : cascade_origin_candidate list) :
    cascade_origin_candidate option =
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

let scope_proximity_rank : int option -> int = function
  | None -> min_int
  | Some hops -> -hops

let compare_cascade_candidate ~layer_order (a : cascade_candidate)
    (b : cascade_candidate) =
  let key (c : cascade_candidate) =
    ( origin_importance_rank ~important:c.important c.origin,
      cascade_layer_precedence_rank ~layer_order ~important:c.important c.layer,
      c.specificity,
      scope_proximity_rank c.scope_hops,
      c.source_order )
  in
  compare (key a) (key b)

let winning_cascade_candidate ~layer_order (candidates : cascade_candidate list)
    : cascade_candidate option =
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

let value ~inherits ~initial ~inherited ~cascaded =
  let inherited_or_initial () = Option.value ~default:initial inherited in
  match cascaded with
  | Some "initial" -> { value = initial; value_source = Initial_keyword }
  | Some "inherit" ->
      { value = inherited_or_initial (); value_source = Inherit_keyword }
  | Some "unset" when inherits ->
      { value = inherited_or_initial (); value_source = Unset_inherited }
  | Some "unset" -> { value = initial; value_source = Unset_initial }
  | Some value -> { value; value_source = Cascaded }
  | None when inherits ->
      { value = inherited_or_initial (); value_source = Inherited_default }
  | None -> { value = initial; value_source = Initial_default }

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
  value ~inherits ~initial ~inherited ~cascaded

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
      value ~inherits ~initial ~inherited ~cascaded:(Some winner.value)
  | None -> value ~inherits ~initial ~inherited ~cascaded:None

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

(* Listed one by one rather than closed with a wildcard: a traversal that
   descends through this function is only as complete as this match, and a
   wildcard would silently hide the next block at-rule from every caller. *)
let statement_children = function
  | Rule rule -> rule.nested
  | Layer (_, block)
  | Media (_, block)
  | Container (_, _, block)
  | Supports (_, block)
  | Moz_document (_, block)
  | When (_, block)
  | Else (_, block)
  | Starting_style block
  | Origin (_, block)
  | Scope (_, _, block) ->
      block
  | Property _ -> []
  | Declarations _ | Bang_comment _ | Charset _ | Import _ | Namespace _
  | Layer_decl _ | Supports_condition _ | Keyframes _ | Webkit_keyframes _
  | Moz_keyframes _ | Font_face _ | Counter_style _ | Page _
  | Page_with_margins _ | Font_palette_values _ | Font_feature_values _
  | View_transition _ | Position_try _ | Viewport _ | Unknown_at_rule _ ->
      []

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
  Pp.string ctx name;
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
   The reader normalizes either form to a plain OCaml string; on output we
   prefer the bare identifier when the value is a syntactically valid CSS ident
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
  Pp.braced_semicolon_list Declaration.pp_declaration ctx kf.declarations

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
  Pp.braced_semicolon_list Declaration.pp_declaration ctx rule.descriptors

let pp_page_with_margins_body ctx descriptors margins =
  Pp.braces
    (fun ctx () ->
      Pp.list ~sep:Pp.semicolon_cut
        (fun ctx (i, d) ->
          if i = 0 then Declaration.pp_declaration ctx d
          else Pp.indent Declaration.pp_declaration ctx d)
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

let pp_unknown_at_rule_statement ctx name prelude (block : string option) =
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
  (* CSS Syntax 3 sec. 4.3.1: the at-keyword consumes an ident sequence, so the
     whitespace before the prelude is the only thing keeping them apart. It is a
     hard space, not layout - minifying [@foo bar] to [@foobar] names a
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
  (* CSS Syntax 3 sec. 2.2: [@charset] is an encoding-declaration byte pattern
     recognised before tokenization, not a stylesheet at-rule after parsing. In
     minified output the serializer emits UTF-8, so [@charset "UTF-8"] is
     redundant; keep at most the first non-UTF-8 declaration for byte-level
     compatibility. *)
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
  let var_name_of_custom_property name =
    if String.length name >= 2 && name.[0] = '-' && name.[1] = '-' then
      String.sub name 2 (String.length name - 2)
    else name
  in
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
  let declarations names decls = List.fold_left declaration names decls in
  let rec statement names = function
    | Rule rule ->
        let names = declarations names rule.declarations in
        List.fold_left statement names rule.nested
    | Declarations decls -> declarations names decls
    | Layer (_, block)
    | Media (_, block)
    | Container (_, _, block)
    | Supports (_, block)
    | Moz_document (_, block)
    | When (_, block)
    | Else (_, block)
    | Starting_style block
    | Origin (_, block)
    | Scope (_, _, block) ->
        List.fold_left statement names block
    | Page (_, decls) | Position_try (_, decls) | Supports_condition (_, decls)
      ->
        declarations names decls
    | Page_with_margins (_, descs, margins) ->
        let names = declarations names descs in
        List.fold_left
          (fun names margin -> declarations names margin.descriptors)
          names margins
    | _ -> names
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
      ({ property = Properties.Box_shadow; value; _ } as decl) ->
      Declaration.Declaration
        { decl with value = rewrite_shadow_value color_vars value }
  | Declaration.Declaration
      ({
         property = Properties.Custom_property _;
         value =
           Properties.Custom_value
             {
               value =
                 Properties.Typed { kind = Properties.Shadow; value = shadow };
               layer;
               meta;
             };
         _;
       } as decl) ->
      Declaration.Declaration
        {
          decl with
          value =
            Properties.Custom_value
              {
                value =
                  Properties.Typed
                    {
                      kind = Properties.Shadow;
                      value = rewrite_shadow_value color_vars shadow;
                    };
                layer;
                meta;
              };
        }
  | decl -> decl

let normalise_shadows stylesheet =
  let color_vars = color_custom_property_names stylesheet in
  if String_set.is_empty color_vars then stylesheet
  else
    let declarations = List.map (rewrite_shadow_decl color_vars) in
    let rec statement = function
      | Rule rule ->
          Rule
            {
              rule with
              declarations = declarations rule.declarations;
              nested = List.map statement rule.nested;
            }
      | Declarations decls -> Declarations (declarations decls)
      | Layer (name, block) -> Layer (name, List.map statement block)
      | Media (query, block) -> Media (query, List.map statement block)
      | Container (name, query, block) ->
          Container (name, query, List.map statement block)
      | Supports (query, block) -> Supports (query, List.map statement block)
      | Moz_document (query, block) ->
          Moz_document (query, List.map statement block)
      | When (query, block) -> When (query, List.map statement block)
      | Else (query, block) -> Else (query, List.map statement block)
      | Starting_style block -> Starting_style (List.map statement block)
      | Origin (origin, block) -> Origin (origin, List.map statement block)
      | Scope (start, end_, block) ->
          Scope (start, end_, List.map statement block)
      | Page (selector, decls) -> Page (selector, declarations decls)
      | Page_with_margins (selector, descs, margins) ->
          Page_with_margins
            ( selector,
              declarations descs,
              List.map
                (fun margin ->
                  { margin with descriptors = declarations margin.descriptors })
                margins )
      | Position_try (name, decls) -> Position_try (name, declarations decls)
      | Supports_condition (name, decls) ->
          Supports_condition (name, declarations decls)
      | other -> other
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

let pp_import_url ctx url =
  let len = String.length url in
  if Pp.minified ctx then pp_import_url_minified ctx url len
  else if
    len > 0
    && (url.[0] = '"' || url.[0] = '\'' || import_url_starts_with_url url len)
  then Pp.string ctx url
  else Pp.quoted_string ctx url

let pp_import_layer ctx layer =
  Pp.sp ctx ();
  if layer = "" then Pp.string ctx "layer"
  else (
    Pp.string ctx "layer(";
    Pp.string ctx layer;
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
      | None when layer = Some "" -> Pp.space ctx ()
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

let pp_moz_document_condition ctx = function
  | Url_prefix None -> Pp.string ctx "url-prefix()"
  | Url_prefix (Some prefix) ->
      Pp.string ctx "url-prefix(";
      Pp.quoted_string ctx prefix;
      Pp.char ctx ')'

let pp_declarations_statement ctx raw_decls =
  (* Bare declarations for CSS nesting - no selector/braces, just declarations.
     No extra indent since the containing block handles it *)
  let decls = raw_decls in
  Pp.list ~sep:Pp.semicolon_cut Declaration.pp_declaration ctx decls;
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

let pp_font_variant_descriptor ctx = function
  | Normal -> Pp.string ctx "normal"
  | None -> Pp.string ctx "none"
  | Values values ->
      Pp.list ~sep:Pp.space pp_font_variant_descriptor_value ctx values

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
  | Font_variant value ->
      pp_descriptor "font-variant" pp_font_variant_descriptor value
  | Font_feature_settings value ->
      pp_descriptor "font-feature-settings" Properties.pp_font_feature_settings
        value
  | Font_variation_settings value ->
      pp_descriptor "font-variation-settings"
        Properties.pp_font_variation_settings value
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
      Pp.string ctx name

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
            Pp.to_string ~minify:(Pp.minified ctx) ~lossless:ctx.Pp.lossless
              Values.pp_color color
          in
          if (not (Pp.minified ctx)) || rendered = "" || rendered.[0] <> '#'
          then Pp.space ctx ();
          Pp.string ctx rendered)
        ctx entries

let pp_font_feature_value ctx (name, indexes) =
  Pp.string ctx name;
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

let rec pp_rule : rule Pp.t =
 fun ctx rule ->
  Selector.pp ctx rule.selector;
  Pp.sp ctx ();
  Pp.block_open ctx ();
  let decls = rule.declarations in
  (match (decls, rule.nested) with
  | [], [] -> ()
  | decls, nested ->
      let ctx = { ctx with level = ctx.level + 1 } in
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
      Pp.string ctx n
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
      Pp.string ctx (if Pp.minified ctx then "to (" else " to (");
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
      Pp.braced_semicolon_list Declaration.pp_declaration ctx raw_declarations
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
      Pp.braced_list
        ~sep:Pp.(cut ++ cut)
        pp_font_feature_values_block ctx blocks
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
  let statements =
    if Pp.minified ctx then normalise statements else statements
  in
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

(* Measure the output size with [Pp.size] and presize the buffer exactly. *)
let to_string ?(minify = false) ?indent ?lossless ?enforce_spec (statements : t)
    =
  let pp ctx () = pp_stylesheet ctx statements in
  let size = Pp.size ~minify ?indent ?lossless ?enforce_spec pp () in
  let buf = Buffer.create size in
  Pp.to_buffer ~minify ?indent ?lossless ?enforce_spec buf pp ();
  Buffer.contents buf

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
  let selector =
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
  { selector; declarations }

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

(* In the [@import] prelude [layer(...)] and [supports(...)] are structural, and
   the grammar allows each once, before the media query list. Reaching the media
   position still holding one means a duplicate or a misordered prelude. They
   are function tokens, so the media grammar would otherwise take them as a
   [<general-enclosed>] query and quietly accept the rule. *)
let starts_with_import_keyword components =
  let cursor = Cursor.of_components components in
  match Cursor.peek cursor with
  | Some (Component.Func { node = { name; _ }; _ }) ->
      let name = String.lowercase_ascii name in
      String.equal name "layer" || String.equal name "supports"
  | Some (Component.Preserved { kind = Token.Ident name; _ }) ->
      String.equal (String.lowercase_ascii name) "layer"
  | Some (Component.Block _ | Component.Preserved _) | None -> false

let drain_until_semicolon r =
  let rec loop acc =
    match Cursor.peek_raw r with
    | None | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
        List.rev acc
    | Some component ->
        ignore (Cursor.next_raw r);
        loop (component :: acc)
  in
  loop []

let read_import_media (r : Cursor.t) : Media.t option =
  if Cursor.peek_semicolon r || Cursor.is_done r then None
  else
    let loc = Cursor.position r in
    let components = drain_until_semicolon r in
    if starts_with_import_keyword components then
      Error.fail_bad_condition loc ~at_rule:"@media"
        ~reason:"layer()/supports() must precede the media query, and once only"
    else
      match Media.of_components ~recover:false components with
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
              if String.contains raw '\'' then (Quoted '\'' : url_form)
              else if String.contains raw '"' then (Quoted '"' : url_form)
              else (Bare : url_form)
          | _ -> (Bare : url_form)
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

(* CSS Animations 1 sec. 3: [@keyframes <keyframes-name>], [<keyframes-name> =
   <custom-ident> | <string>]. The reserved spellings ([none], CSS-wide
   keywords, [default]) are excluded from [<custom-ident>], but every mainstream
   minifier accepts them as [<string>], so cascade keeps them too rather than
   leak input that downstream tools preserve verbatim. *)
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

let read_moz_document_condition r : moz_document_condition =
  Cursor.ws r;
  match Cursor.next r with
  | Some (Component.Func { node = { name = "url-prefix"; arguments; _ }; _ }) ->
      let arg_cursor = Cursor.of_components arguments in
      Cursor.ws arg_cursor;
      let prefix =
        match Cursor.string_opt arg_cursor with
        | Some "" -> Option.None
        | Some s -> Some s
        | None ->
            Cursor.ws arg_cursor;
            if Cursor.is_done arg_cursor then Option.None
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

(* CSS Fonts 4 sec. 11.2 wants the first bound of a descriptor range <= the
   second. Browsers keep a descending range, so the readers accept it but record
   a warning here; [Css.of_string ~strict] then turns the warning into an
   error. *)
let warn_descending_range r property =
  Cursor.push_warning r
    (Error.bad_value (Cursor.position r) ~property
       ~reason:
         "range must run from the smaller value to the larger (CSS Fonts 4 \
          \u{00a7}11.2)")

let font_weight_num = function
  | (Properties.Weight n : Properties.font_weight) -> Some n
  | Properties.Normal -> Some 400
  | Properties.Bold -> Some 700
  | _ -> None

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
        (match (font_weight_num first, font_weight_num second) with
        | Some a, Some b when a > b -> warn_descending_range r "font-weight"
        | _ -> ());
        Font_weight_range (first, second))
    r

let read_font_style_descriptor r =
  read_descriptor_value Declaration.read_property_value
    (fun value ->
      let c = Cursor.of_string value in
      let first = Properties.read_font_style c in
      Cursor.ws c;
      let descriptor =
        if Cursor.is_done c then Font_style first
        else
          let second = Properties.read_font_style c in
          Cursor.ws c;
          Cursor.expect_eof c;
          Font_style_range (first, second)
      in
      (* [read_font_style] warns on a descending oblique range; the value is
         parsed in [c], so surface its warnings on the drained cursor [r]. *)
      List.iter (Cursor.push_warning r) (Cursor.drain_warnings c);
      descriptor)
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

let font_stretch_pct_opt = function
  | (Properties.Pct value : Properties.font_stretch) -> Some value
  | Properties.Ultra_condensed -> Some 50.
  | Properties.Extra_condensed -> Some 62.5
  | Properties.Condensed -> Some 75.
  | Properties.Semi_condensed -> Some 87.5
  | Properties.Normal -> Some 100.
  | Properties.Semi_expanded -> Some 112.5
  | Properties.Expanded -> Some 125.
  | Properties.Extra_expanded -> Some 150.
  | Properties.Ultra_expanded -> Some 200.
  | _ -> None

let read_font_stretch_descriptor r =
  read_descriptor_value Declaration.read_property_value
    (fun value ->
      let c = Cursor.of_string value in
      let first = Properties.read_font_stretch c in
      Cursor.ws c;
      if Cursor.is_done c then Font_stretch first
      else begin
        let second = Properties.read_font_stretch c in
        Cursor.ws c;
        Cursor.expect_eof c;
        (match (font_stretch_pct_opt first, font_stretch_pct_opt second) with
        | Some a, Some b when a > b -> warn_descending_range r "font-stretch"
        | _ -> ());
        Font_stretch_range value
      end)
    r

let read_unicode_range_descriptor r =
  read_descriptor_value
    (fun cur ->
      Cursor.list ~at_least:1 ~sep:Cursor.comma Properties.read_unicode_range
        cur)
    (fun vs -> Unicode_range vs)
    r

let font_variant_desc_value = function
  | "common-ligatures" -> Some (Ligature Properties.Common_ligatures)
  | "no-common-ligatures" -> Some (Ligature Properties.No_common_ligatures)
  | "discretionary-ligatures" ->
      Some (Ligature Properties.Discretionary_ligatures)
  | "no-discretionary-ligatures" ->
      Some (Ligature Properties.No_discretionary_ligatures)
  | "historical-ligatures" -> Some (Ligature Properties.Historical_ligatures)
  | "no-historical-ligatures" ->
      Some (Ligature Properties.No_historical_ligatures)
  | "contextual" -> Some (Ligature Properties.Contextual)
  | "no-contextual" -> Some (Ligature Properties.No_contextual)
  | "small-caps" -> Some (Caps Properties.Small_caps)
  | "all-small-caps" -> Some (Caps Properties.All_small_caps)
  | "petite-caps" -> Some (Caps Properties.Petite_caps)
  | "all-petite-caps" -> Some (Caps Properties.All_petite_caps)
  | "unicase" -> Some (Caps Properties.Unicase)
  | "titling-caps" -> Some (Caps Properties.Titling_caps)
  | "lining-nums" -> Some (Numeric Properties.Lining_nums)
  | "oldstyle-nums" -> Some (Numeric Properties.Oldstyle_nums)
  | "proportional-nums" -> Some (Numeric Properties.Proportional_nums)
  | "tabular-nums" -> Some (Numeric Properties.Tabular_nums)
  | "diagonal-fractions" -> Some (Numeric Properties.Diagonal_fractions)
  | "stacked-fractions" -> Some (Numeric Properties.Stacked_fractions)
  | "ordinal" -> Some (Numeric Properties.Ordinal)
  | "slashed-zero" -> Some (Numeric Properties.Slashed_zero)
  | "jis78" -> Some (East_asian Properties.Jis78)
  | "jis83" -> Some (East_asian Properties.Jis83)
  | "jis90" -> Some (East_asian Properties.Jis90)
  | "jis04" -> Some (East_asian Properties.Jis04)
  | "simplified" -> Some (East_asian Properties.Simplified)
  | "traditional" -> Some (East_asian Properties.Traditional)
  | "full-width" -> Some (East_asian Properties.Full_width)
  | "proportional-width" -> Some (East_asian Properties.Proportional_width)
  | "ruby" -> Some (East_asian Properties.Ruby)
  | _ -> None

let font_variant_ligature_slot = function
  | Properties.Common_ligatures | Properties.No_common_ligatures -> `Common
  | Properties.Discretionary_ligatures | Properties.No_discretionary_ligatures
    ->
      `Discretionary
  | Properties.Historical_ligatures | Properties.No_historical_ligatures ->
      `Historical
  | Properties.Contextual | Properties.No_contextual -> `Contextual

let font_variant_numeric_slot = function
  | Properties.Lining_nums | Properties.Oldstyle_nums -> `Figure
  | Properties.Proportional_nums | Properties.Tabular_nums -> `Spacing
  | Properties.Diagonal_fractions | Properties.Stacked_fractions -> `Fraction
  | Properties.Ordinal -> `Ordinal
  | Properties.Slashed_zero -> `Slashed_zero
  | Properties.Normal | Properties.Var _ -> `Invalid

let font_variant_east_asian_slot = function
  | Properties.Jis78 | Properties.Jis83 | Properties.Jis90 | Properties.Jis04
  | Properties.Simplified | Properties.Traditional ->
      `Variant
  | Properties.Full_width | Properties.Proportional_width -> `Width
  | Properties.Ruby -> `Ruby

let validate_font_variant_descriptor_values r values =
  let seen_ligatures = ref [] in
  let seen_numeric = ref [] in
  let seen_east_asian = ref [] in
  let seen_caps = ref false in
  let duplicate slot seen =
    if List.mem slot !seen then true
    else (
      seen := slot :: !seen;
      false)
  in
  let invalid =
    List.exists
      (function
        | Ligature value ->
            duplicate (font_variant_ligature_slot value) seen_ligatures
        | Numeric value ->
            duplicate (font_variant_numeric_slot value) seen_numeric
        | East_asian value ->
            duplicate (font_variant_east_asian_slot value) seen_east_asian
        | Caps _ ->
            let duplicate = !seen_caps in
            seen_caps := true;
            duplicate)
      values
  in
  if invalid then Cursor.err_invalid r "font-variant descriptor"

let read_font_variant_descriptor_value r =
  let ident = Cursor.ident ~keep_case:false r in
  match font_variant_desc_value (String.lowercase_ascii ident) with
  | Some value -> value
  | None -> Cursor.err_invalid r ("font-variant descriptor value: " ^ ident)

let read_font_variant_descriptor r =
  let at_value_end () = Cursor.is_done r || Cursor.peek_semicolon r in
  let snap = Cursor.save r in
  let first = Cursor.ident ~keep_case:false r in
  Cursor.ws r;
  match (String.lowercase_ascii first, at_value_end ()) with
  | "normal", true -> Normal
  | "none", true -> None
  | _ ->
      Cursor.restore r snap;
      let rec loop acc =
        Cursor.ws r;
        if at_value_end () then List.rev acc
        else loop (read_font_variant_descriptor_value r :: acc)
      in
      let values = loop [] in
      if values = [] then Cursor.err_invalid r "font-variant descriptor";
      validate_font_variant_descriptor_values r values;
      Values values

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
      read_descriptor_value read_font_variant_descriptor
        (fun v -> Font_variant v)
        r
  | "font-feature-settings" ->
      read_descriptor_value Properties.read_font_feature_settings
        (fun v -> Font_feature_settings v)
        r
  | "font-variation-settings" ->
      read_descriptor_value Properties.read_font_variation_settings
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
    match read_font_face_desc name r with
    | descriptor ->
        Cursor.ws r;
        if Cursor.peek_semicolon r then Cursor.skip r;
        Some descriptor
    | exception Error.Parse_error e ->
        (* CSS Fonts 4 sec. 11.2 / CSS Syntax 3 sec. 5.4.4: a descriptor that
           does not parse - an unknown name (Fontsource's [font-named-instance])
           or an invalid value of a known one ([font-display:maybe]) - is
           dropped and the rest of the @font-face is kept, matching browsers. *)
        Cursor.push_warning r e;
        let rec skip_to_semicolon () =
          match Cursor.next_raw r with
          | None | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
              ()
          | Some _ -> skip_to_semicolon ()
        in
        skip_to_semicolon ();
        None

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
  (* CSS Fonts 4 sec. 11.2.1: missing [font-family] / [src] is a semantic
     mismatch, not a syntax one. [validate_partial_statement] flags it; the
     syntactic reader accepts. *)
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
      System system)
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
      Symbols symbols)
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
      | "suffix" -> read_counter_symbol_descriptor (fun s -> Suffix s) r
      | "prefix" -> read_counter_symbol_descriptor (fun s -> Prefix s) r
      | "fallback" -> read_counter_string_descriptor (fun s -> Fallback s) r
      | "range" -> read_counter_string_descriptor (fun s -> Range s) r
      | "pad" -> read_counter_string_descriptor (fun s -> Pad s) r
      | "negative" -> read_counter_string_descriptor (fun s -> Negative s) r
      | "additive-symbols" ->
          read_counter_string_descriptor (fun s -> Additive_symbols s) r
      | "speak-as" -> read_counter_string_descriptor (fun s -> Speak_as s) r
      | _ -> Cursor.err_invalid r ("unknown counter-style descriptor: " ^ name)
    in
    Cursor.ws r;
    if Cursor.peek_semicolon r then Cursor.skip r;
    Some descriptor

let counter_style_descriptor_rank = function
  | System _ -> 0
  | Symbols _ -> 1
  | Additive_symbols _ -> 2
  | Prefix _ -> 3
  | Suffix _ -> 4
  | Range _ -> 5
  | Pad _ -> 6
  | Negative _ -> 7
  | Fallback _ -> 8
  | Speak_as _ -> 9

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
    (function System system -> Some system | _ -> None)
    descriptors

let counter_style_has_symbols descriptors =
  List.exists (function Symbols _ -> true | _ -> false) descriptors

let counter_style_has_additive_symbols descriptors =
  List.exists (function Additive_symbols _ -> true | _ -> false) descriptors

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

(* CSS Paged Media 3 sec. 3.1: a page selector is an optional page name followed
   by zero or more pseudo-pages from [:first | :left | :right | :blank]. *)
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

(* CSS Paged Media 3 section 3.1: [<page-selector-list> = <page-selector>#],
   [<page-selector> = <ident-token>? <pseudo-page>*], with each pseudo-page from
   the closed set [first | left | right | blank], so [@page invoice:blank:first]
   is well-formed. *)
let parse_page_selectors r selector =
  let s = String.trim selector in
  let len = String.length s in
  let consume_ident = page_selector_consume_ident s len in
  let skip_ws = page_selector_skip_ws s len in
  let pseudo_of_name name =
    match name with
    | "first" -> First
    | "left" -> Left
    | "right" -> Right
    | "blank" -> Blank
    | _ -> page_selector_error r s
  in
  let consume_pseudo_page i : (page_pseudo * int) option =
    if i >= len || s.[i] <> ':' then None
    else
      let start = i + 1 in
      let stop = consume_ident start in
      if stop = start then page_selector_error r s;
      Some (pseudo_of_name (String.sub s start (stop - start)), stop)
  in
  let rec consume_pseudo_pages acc i =
    match consume_pseudo_page i with
    | None -> (List.rev acc, i)
    | Some (p, stop) -> consume_pseudo_pages (p :: acc) stop
  in
  let rec consume_selectors acc i =
    let i = skip_ws i in
    if i >= len then List.rev acc
    else
      let after_ident = consume_ident i in
      let name =
        if after_ident = i then Option.None
        else Some (String.sub s i (after_ident - i))
      in
      let pseudos, after_pseudo = consume_pseudo_pages [] after_ident in
      let sel = { name; pseudos } in
      let after_pseudo = skip_ws after_pseudo in
      if after_pseudo >= len then List.rev (sel :: acc)
      else if s.[after_pseudo] = ',' then
        consume_selectors (sel :: acc) (after_pseudo + 1)
      else page_selector_error r s
  in
  consume_selectors [] 0

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
      (* CSS Paged Media sec. 5: a page-margin box accepts every descriptor
         valid in [@page] plus [content]. *)
      let descriptors =
        Cursor.braces
          (fun inner ->
            read_descriptor_block (replace_page_margin_descriptor inner) inner)
          r
      in
      if descriptors = [] then
        Cursor.err_invalid r "page margin rule requires descriptors";
      { name; descriptors }
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
    if s = "" then [] else parse_page_selectors r s
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

let read_font_palette_descriptor outer inner : font_palette_descriptor option =
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

let read_font_feature_value_entry outer inner : (string * int list) option =
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

let read_view_transition_descriptor outer inner :
    view_transition_descriptor option =
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

(* CSS Syntax 3 sec. 5.4.2 "consume an at-rule": after the at-keyword has been
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

let conditional_arguments (fn : Component.func Component.node) =
  if not fn.node.terminated then failwith "unterminated conditional function";
  fn.node.arguments

let conditional_atom (fn : Component.func Component.node) =
  match String.lowercase_ascii fn.Component.node.name with
  | "media" ->
      Media_condition (Media.of_function_components (conditional_arguments fn))
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

let scope_prelude r prelude_components : Selector.t option * Selector.t option =
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
  if prelude = "" then (Option.None, Option.None)
  else
    let start_cvs, end_cvs = split_at_to [] prelude_components in
    let start_parens, start = strip_parens start_cvs in
    let end_parens, end_ = strip_parens end_cvs in
    if start_parens && start = "" then
      Cursor.err_invalid r "@scope start selector cannot be empty";
    if end_cvs <> [] && end_parens && end_ = "" then
      Cursor.err_invalid r "@scope end selector cannot be empty";
    (* Parse each bound into a selector; an unparseable bound is
       [Selector.Invalid]. *)
    let opt s : Selector.t option =
      if s = "" then None
      else
        match Selector.of_string s with
        | sel -> Some sel
        | exception (Cursor.Parse_error _ | Invalid_argument _) ->
            Some Selector.Invalid
    in
    (opt start, opt end_)

let read_supports_condition (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "supports-condition" r;
  Cursor.ws r;
  let name = Cursor.ident ~keep_case:true r in
  if not (String.length name >= 2 && String.sub name 0 2 = "--") then
    Cursor.err_invalid r "@supports-condition: name must start with '--'";
  Cursor.ws r;
  let declarations = Cursor.braces Declaration.read_declarations r in
  Supports_condition (name, declarations)

let read_layer_name (r : Cursor.t) : string = read_layer_name_component r

let read_nested_media_condition components =
  if Cursor.of_components components |> Cursor.is_done then Media.List []
  else
    try Media.of_components ~recover:false components
    with Failure _ -> Media.of_string "not all"

let read_rule_selector ?(nested = false) r =
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
     caller intact, rather than rewrapping (which erases the structured error
     and relocates it). CSS Nesting 1 sec. 2: a nested rule's prelude is a
     [<relative-selector-list>], so it may start with a combinator ([> .bar])
     implicitly relative to the parent [&]. *)
  Cursor.with_context c "selector" (fun () ->
      if nested then Selector.read_relative c
      else Selector.read_strict_selector_list c)

let is_bare_nesting_selector : Selector.t -> bool = function
  | Selector.Nesting -> true
  | _ -> false

let validate_nested_rule_selector inner selector nested_selector =
  if
    is_bare_nesting_selector selector
    && is_bare_nesting_selector nested_selector
  then
    Cursor.err_invalid inner
      "bare nesting selector cannot directly nest another bare nesting selector"

let rec skip_bad_rule_item inner =
  match Cursor.next_raw inner with
  | None -> ()
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) -> ()
  | Some _ -> skip_bad_rule_item inner

let read_property_descriptors (r : Cursor.t) : property_reader_state =
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

let read_property_rule (r : Cursor.t) : statement =
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
        | None -> Option.None
        | Some str -> Some (read_property_initial_value r syntax str)
      in
      Property { name; syntax; inherits; initial_value }

(* Does the item ahead hold a curly block before its terminating [;]? Then it is
   a nested rule, not a declaration, however much its prelude looks like one.
   Leaves the cursor where it found it. *)
let item_opens_block inner =
  let start = Cursor.save inner in
  let rec scan () =
    match Cursor.peek inner with
    | None -> false
    | Some (Component.Preserved { kind = Token.Semicolon; _ }) -> false
    | Some (Component.Block { node = { opening = Token.Curly; _ }; _ }) -> true
    | Some _ ->
        Cursor.skip inner;
        scan ()
  in
  let found = scan () in
  Cursor.restore inner start;
  found

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
          (* CSS Syntax 3 sec. 5.4.1: an at-rule with no registered handler is
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
  (* Skip a statement that failed to parse: consume to the end of its block
     ([{...}]) or its terminating [;], leaving the cursor at the next rule. *)
  let rec skip_bad_statement () =
    match Cursor.next_raw r with
    | None -> ()
    | Some (Component.Block _)
    | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
        ()
    | Some _ -> skip_bad_statement ()
  in
  let rec read_statements acc =
    Cursor.ws r;
    if Cursor.is_done r then List.rev acc
    else
      let loc = Cursor.position r in
      let snap = Cursor.save r in
      match read_statement r with
      (* CSS Syntax 3 sec. 5.4.1: a rule that fails to parse (e.g. an invalid
         selector) is dropped, and parsing resumes at the next rule - one bad
         rule must not take the rest of the [@layer] / [@media] block with it.
         Strict mode ([not (Cursor.recover r)]) still raises. *)
      | exception Error.Parse_error e when Cursor.recover r ->
          Cursor.restore r snap;
          Cursor.push_warning r e;
          skip_bad_statement ();
          read_statements acc
      | Import _ ->
          (* CSS Cascade L6 sec. 2: @import is only valid at the top of the
             stylesheet. Drop a misplaced one rather than emitting it. *)
          Cursor.push_warning r
            (Error.bad_value loc ~property:"stylesheet"
               ~reason:"@import is only valid at the top of a stylesheet");
          read_statements acc
      | Else _ when not (List.exists follows_conditional acc) ->
          Cursor.err_invalid r "@else without preceding @when"
      | stmt -> read_statements (stmt :: acc)
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
  let condition : conditional =
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
    | [] -> Option.None
    | _ -> (
        match conditional_components prelude with
        | condition -> Some condition
        | exception Failure msg -> Cursor.err_invalid r ("@else: " ^ msg))
  in
  let content = Cursor.braces (fun inner -> read_block inner) r in
  Else (condition, content)

and read_media (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "media" r;
  Cursor.ws r;
  let condition_components = Cursor.drain_until_block r in
  let content = Cursor.braces (fun inner -> read_block inner) r in
  let condition =
    if Cursor.of_components condition_components |> Cursor.is_done then
      Media.List []
    else
      try Media.of_components ~recover:false condition_components
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
  let container_name : string option =
    let snap = Cursor.save r in
    match Cursor.option Cursor.ident r with
    | Some name
      when List.mem (String.lowercase_ascii name) [ "none"; "and"; "not"; "or" ]
      ->
        Cursor.restore r snap;
        Option.None
    | other -> other
  in
  Cursor.ws r;
  let condition_components = Cursor.drain_until_block r in
  let content = Cursor.braces (fun inner -> read_block inner) r in
  let condition : Container.t option =
    if Cursor.of_components condition_components |> Cursor.is_done then
      Option.None
    else
      match Container.of_components condition_components with
      | condition -> Some condition
      | exception Failure msg -> Cursor.err_invalid r msg
  in
  (* CSS Containment 3 section 4: [@container] requires a query (with an
     optional [<container-name>] in front). Bare [@container { ... }] is a parse
     error. *)
  if container_name = None && condition = None then
    Cursor.err_invalid r "@container requires a container query";
  Container (container_name, condition, content)

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
  let container_name : string option = Cursor.option Cursor.ident r in
  Cursor.ws r;
  let condition_str = Cursor.drain_until_block_as_string ~trim:true r in
  let content = Cursor.braces (fun inner -> read_nesting_block inner) r in
  let condition : Container.t option =
    if condition_str = "" then Option.None
    else
      match Container.of_string condition_str with
      | condition -> Some condition
      | exception Failure msg -> Cursor.err_invalid r msg
  in
  Container (container_name, condition, content)

and read_nested_supports_rule r =
  let cond_loc = Cursor.position r in
  let condition = Cursor.drain_until_block_as_string ~trim:true r in
  let content = Cursor.braces (fun inner -> read_nesting_block inner) r in
  Supports (supports_condition ~loc:cond_loc condition, content)

and read_nested_media_rule r =
  let condition_components = Cursor.drain_until_block r in
  let content = Cursor.braces (fun inner -> read_nesting_block inner) r in
  Media (read_nested_media_condition condition_components, content)

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
  let start = Cursor.save inner in
  match Declaration.read_declaration inner with
  | Some d ->
      Cursor.ws inner;
      if Cursor.peek_semicolon inner then Cursor.skip inner;
      `Continue (d :: decls, nested)
  | None -> read_nested_rule_or_done selector inner decls nested
  (* CSS Nesting 1 sec. 2 lets a nested rule start with an identifier, so
     [h2:where(...) { ... }] reads as a declaration up to the [{]. Rewind and
     take it as a rule; a genuine bad declaration has no block and still reports
     as one. *)
  | exception Error.Parse_error _
    when Cursor.restore inner start;
         item_opens_block inner ->
      read_nested_rule_or_done selector inner decls nested

and read_nested_rule_or_done selector inner decls nested =
  if Cursor.is_done inner then `Done (List.rev decls, List.rev nested)
  else
    let nr = read_rule ~nested:true inner in
    validate_nested_rule_selector inner selector nr.selector;
    `Continue (decls, Rule nr :: nested)

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

and read_rule ?(nested = false) (r : Cursor.t) : rule =
  Cursor.with_context r "rule" @@ fun () ->
  let selector = read_rule_selector ~nested r in
  let declarations, nested = Cursor.braces (read_rule_body selector) r in
  { selector; declarations; nested; merge_key = None }

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

(* @page / @font-face descriptors flow through [Declaration.read_declaration]
   like ordinary declarations and show up as [Unknown_property "marks"] / "src".
   The reader already enforces each at-rule's allowed-descriptor list, so an
   [Unknown_property] in a descriptor block is by construction a known
   descriptor - skip the [is_invalid] check that would flag it. *)
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
           (fun { descriptors; _ } ->
             List.exists descriptor_has_typed_invalid_value descriptors)
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
           (fun { descriptors; _ } ->
             List.exists declaration_has_strict_warning descriptors)
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

let add_warning warnings warning = warnings := warning :: !warnings

let drain_statement_warnings warnings cursor =
  List.iter (add_warning warnings) (Cursor.drain_warnings cursor)

let validate_else_orphan previous rule stmt =
  match stmt with
  | Else _
    when not (Option.fold ~none:false ~some:follows_conditional !previous) ->
      Some
        (Error.bad_value (rule_loc rule) ~property:"stylesheet"
           ~reason:"@else without preceding @when")
  | _ -> None

let validate_partial_statement_warnings warnings validate_prelude rule stmt =
  let loc = rule_loc rule in
  Option.iter (add_warning warnings) (validate_prelude loc stmt);
  Option.iter (add_warning warnings) (validate_partial_statement loc stmt);
  Option.iter (add_warning warnings)
    (validate_partial_invalid_declarations loc stmt);
  Option.iter (add_warning warnings) (validate_partial_strict_warnings loc stmt)

let read_stylesheet_of_rules ?source ?meta (rules : Component.rule list) :
    stylesheet * Error.t list =
  let warnings = ref [] in
  let validate_prelude = validate_partial_prelude () in
  (* CSS Conditional Rules 5 section 4.6: [@else] is a continuation rule and
     must follow [@when] or another [@else]. A bare top-level [@else] is a parse
     error. *)
  let previous : statement option ref = ref Option.None in
  let statements =
    List.filter_map
      (fun rule ->
        let cursor, result = read_statement_of_rule ?source ?meta rule in
        (* Drain declaration-level warnings first so source order is preserved:
           decl warnings come from inside the rule, the rule-level error (if
           any) comes after them. *)
        drain_statement_warnings warnings cursor;
        match result with
        | Ok stmt -> (
            validate_partial_statement_warnings warnings validate_prelude rule
              stmt;
            match validate_else_orphan previous rule stmt with
            | Some w ->
                add_warning warnings w;
                previous := Some stmt;
                None
            | None ->
                previous := Some stmt;
                Some stmt)
        | Error e ->
            add_warning warnings e;
            None)
      rules
  in
  (statements, List.rev !warnings)

(* Scan [source] for top-level [/*! ... */] bang comments. The lexer drops
   ordinary comments per CSS Syntax 3 sec. 4.3.2; bang comments are the minifier
   convention for license headers and need to round-trip. Returns pairs
   [(start_offset, body)] in source order; nested comments inside strings or
   other comments are not handled because CSS comments don't nest. *)
let extract_bang_comments (source : string) : (int * string) list =
  let len = String.length source in
  let acc = ref [] in
  let i = ref 0 in
  while !i + 2 < len do
    if
      String.unsafe_get source !i = '/'
      && String.unsafe_get source (!i + 1) = '*'
      && String.unsafe_get source (!i + 2) = '!'
    then (
      let start_offset = !i in
      let body_start = !i + 2 in
      let j = ref (!i + 3) in
      let stop : int option ref = ref Option.None in
      while !stop = Option.None && !j + 1 < len do
        if
          String.unsafe_get source !j = '*'
          && String.unsafe_get source (!j + 1) = '/'
        then stop := Some !j
        else incr j
      done;
      match !stop with
      | Some end_ ->
          let body = String.sub source body_start (end_ - body_start) in
          acc := (start_offset, body) :: !acc;
          i := end_ + 2
      | None -> i := len)
    else incr i
  done;
  List.rev !acc

(* Top-level partial-recovery entry point: combine section 5.3 syntax warnings
   from [Parser.stylesheet] with per-rule typed-validation warnings. *)
let parse_stylesheet_partial ?(meta = Loc.default_meta_level)
    ?(enforce_spec = false) (source : string) : stylesheet * Error.t list =
  let reader = Reader.of_string ~enforce_spec source in
  let out = Parser.stylesheet ~meta reader in
  let sheet, typed_warnings =
    read_stylesheet_of_rules ~source:(Reader.source reader) ~meta out.value
  in
  (* Interleave preserved [/*! ... */] bang comments at their source position by
     walking the original rules and the bang-comment list in parallel; any bang
     comments after the last rule are appended at the end. The typed [sheet] may
     be shorter than [out.value] when validation drops a rule, but each typed
     statement still corresponds to the next unconsumed rule, so the position
     mapping survives. *)
  let bangs = extract_bang_comments source in
  (* Compare a comment's offset against each rule's END, not its start: a rule's
     start absorbs an immediately-preceding comment ([/*!x*/a{}] starts at 0),
     pushing a leading comment after its rule, but the closing-brace end is
     unaffected by leading trivia, so leading and between-rule comments order
     before their rule. *)
  let rule_ends = List.map (fun r -> (rule_loc r).Loc.end_pos) out.value in
  let rec interleave bangs rule_ends sheet =
    match (bangs, rule_ends, sheet) with
    | [], _, _ -> sheet
    | (_, body) :: rest_b, [], _ ->
        Bang_comment body :: interleave rest_b [] sheet
    | (offset, body) :: rest_b, end_ :: _, _ when offset < end_ ->
        Bang_comment body :: interleave rest_b rule_ends sheet
    | _, _ :: rest_s, [] -> interleave bangs rest_s []
    | _, _ :: rest_s, stmt :: rest_sheet ->
        stmt :: interleave bangs rest_s rest_sheet
  in
  let sheet = interleave bangs rule_ends sheet in
  (sheet, out.warnings @ typed_warnings)

(** {1 Inline Styles} *)

let pp_important ~minify pp_ctx =
  if minify then Pp.string pp_ctx "!important"
  else (
    Pp.space pp_ctx ();
    Pp.string pp_ctx "!important")

let pp_decl_inline ~minify ~mode pp_ctx decl =
  let name = Declaration.property_name decl in
  let value =
    Declaration.string_of_value ~minify ~inline:(mode = Inline) decl
  in
  let is_important = Declaration.is_important decl in
  Pp.string pp_ctx name;
  Pp.char pp_ctx ':';
  if not minify then Pp.space pp_ctx ();
  Pp.string pp_ctx value;
  if is_important then pp_important ~minify pp_ctx

let inline_style_of_declarations ?(minify = false) ?(mode : mode = Inline) props
    =
  let buf = Buffer.create 128 in
  let pp_ctx = Pp.ctx ~minify ~inline:(mode = Inline) buf in
  let first = ref true in
  List.iter
    (fun decl ->
      if !first then first := false
      else (
        Pp.semicolon pp_ctx ();
        if not minify then Pp.space pp_ctx ());
      pp_decl_inline ~minify ~mode pp_ctx decl)
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
  | Unknown_at_rule _ | Bang_comment _ ->
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

(* A cheap discriminating hash for a rule, folding the hash each declaration
   already caches. The stdlib structural hash reads only the first few nodes of
   a rule, so rules sharing a selector collide and any table keyed by one then
   compares whole rule subtrees on every probe. *)
let rule_hash (r : rule) =
  List.fold_left
    (fun acc d -> (acc * 31) + Declaration.hash d)
    (Selector.hash r.selector) r.declarations
