(** Structured [@supports] conditions for type-safe feature query construction.

    Implements the grammar from CSS Conditional Rules Level 3/4/5:
    {v
    <supports-condition> = not <supports-in-parens>
                         | <supports-in-parens> [ and <supports-in-parens> ]*
                         | <supports-in-parens> [ or <supports-in-parens> ]*

    <supports-in-parens> = ( <supports-condition> )
                         | <supports-feature>
                         | <general-enclosed>

    <supports-feature> = <supports-decl>
                       | <supports-selector-fn>
                       | <supports-font-tech-fn>
                       | <supports-font-format-fn>

    <supports-decl> = ( <declaration> )
    <supports-selector-fn> = selector( <complex-selector> )
    <supports-font-tech-fn> = font-tech( <font-tech> )
    <supports-font-format-fn> = font-format( <font-format> )
    <general-enclosed> = <function-token> <any-value>? )
                       | ( <any-value> )
    v} *)

open Syntax

type property_name = Property_name of string

type declaration_feature =
  | Declaration of Declaration.t
  | Empty of property_name
  | Unsupported of property_name * string
  | Vendor_flag_enabled

type font_format =
  | Collection
  | Embedded_opentype
  | Opentype
  | Svg
  | Truetype
  | Woff
  | Woff2

type font_tech =
  | Features_opentype
  | Features_aat
  | Features_graphite
  | Color_colrv0
  | Color_colrv1
  | Color_svg
  | Color_sbix
  | Color_cbdt
  | Variations
  | Palettes
  | Incremental

type function_feature =
  | Selector of Selector.t
  | Font_format of font_format
  | Font_tech of font_tech
  | At_rule of string
  | Named_feature of string
  | Env of string
  | General of string * string

type t =
  | Property of declaration_feature
      (** [(property: value)] declaration feature test *)
  | Function of function_feature
      (** Function feature test (selector, font-format, font-tech, at-rule,
          named-feature, or env). *)
  | Not of t  (** [not (condition)] negation *)
  | And of t * t  (** [(cond1) and (cond2)] conjunction *)
  | Or of t * t  (** [(cond1) or (cond2)] disjunction *)

let font_format_of_string = function
  | "collection" -> Some Collection
  | "embedded-opentype" -> Some Embedded_opentype
  | "opentype" -> Some Opentype
  | "svg" -> Some Svg
  | "truetype" -> Some Truetype
  | "woff" -> Some Woff
  | "woff2" -> Some Woff2
  | _ -> None

let string_of_font_format = function
  | Collection -> "collection"
  | Embedded_opentype -> "embedded-opentype"
  | Opentype -> "opentype"
  | Svg -> "svg"
  | Truetype -> "truetype"
  | Woff -> "woff"
  | Woff2 -> "woff2"

let font_tech_of_string = function
  | "features-opentype" -> Some Features_opentype
  | "features-aat" -> Some Features_aat
  | "features-graphite" -> Some Features_graphite
  | "color-colrv0" -> Some Color_colrv0
  | "color-colrv1" -> Some Color_colrv1
  | "color-svg" -> Some Color_svg
  | "color-sbix" -> Some Color_sbix
  | "color-cbdt" -> Some Color_cbdt
  | "variations" -> Some Variations
  | "palettes" -> Some Palettes
  | "incremental" -> Some Incremental
  | _ -> None

let string_of_font_tech = function
  | Features_opentype -> "features-opentype"
  | Features_aat -> "features-aat"
  | Features_graphite -> "features-graphite"
  | Color_colrv0 -> "color-COLRv0"
  | Color_colrv1 -> "color-COLRv1"
  | Color_svg -> "color-svg"
  | Color_sbix -> "color-sbix"
  | Color_cbdt -> "color-cbdt"
  | Variations -> "variations"
  | Palettes -> "palettes"
  | Incremental -> "incremental"

let starts_with ~prefix s =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let property_name name =
  let reader = Cursor.of_string name in
  let parsed =
    try Cursor.ident ~keep_case:true reader
    with Cursor.Parse_error _ ->
      failwith ("invalid supports declaration property name: " ^ name)
  in
  if not (Cursor.is_done reader) then
    failwith ("invalid supports declaration property name: " ^ name);
  let name =
    if starts_with ~prefix:"--" parsed then parsed
    else String.lowercase_ascii parsed
  in
  Property_name name

let string_of_property_name (Property_name name) = name

let declaration_feature prop value =
  match (String.lowercase_ascii prop, String.lowercase_ascii value) with
  | _, "" -> Empty (property_name prop)
  | "-vendor-flag", "enabled" -> Vendor_flag_enabled
  | _ -> (
      let name = property_name prop in
      try Declaration (Declaration.of_string (prop ^ ":" ^ value))
      with Cursor.Parse_error _ | Failure _ ->
        Unsupported (name, String.trim value))

let property prop value = Property (declaration_feature prop value)

let single_ident name args =
  let cursor = Cursor.of_string args in
  let ident = Cursor.ident ~keep_case:false cursor |> String.lowercase_ascii in
  Cursor.ws cursor;
  Cursor.expect_eof cursor;
  if ident = "" then failwith ("empty " ^ name ^ "() in @supports");
  ident

let func name args =
  let lower_name = String.lowercase_ascii name in
  let feature =
    match lower_name with
    | "selector" ->
        let cursor = Cursor.of_string args in
        let selector = Selector.read cursor in
        Cursor.ws cursor;
        Cursor.expect_eof cursor;
        Selector selector
    | "font-format" -> (
        match font_format_of_string (single_ident name args) with
        | Some format -> Font_format format
        | None -> failwith "invalid font-format() in @supports")
    | "font-tech" -> (
        match font_tech_of_string (single_ident name args) with
        | Some tech -> Font_tech tech
        | None -> failwith "invalid font-tech() in @supports")
    | "at-rule" ->
        let cursor = Cursor.of_string args in
        let at_rule =
          match Cursor.at_keyword_opt cursor with
          | Some name -> name
          | None -> failwith "invalid at-rule() in @supports"
        in
        Cursor.ws cursor;
        Cursor.expect_eof cursor;
        At_rule at_rule
    | "named-feature" -> Named_feature (single_ident name args)
    | "env" -> Env (single_ident name args)
    | _ -> General (lower_name, String.trim args)
  in
  Function feature

(* ===== Pretty printing ===== *)

let render_declaration_feature = function
  | Declaration decl -> Declaration.string_of_declaration ~minify:false decl
  | Empty name -> string_of_property_name name ^ ":"
  | Unsupported (name, value) -> string_of_property_name name ^ ": " ^ value
  | Vendor_flag_enabled -> "-vendor-flag: enabled"

let render_function_feature = function
  | Selector selector -> "selector(" ^ Selector.to_string selector ^ ")"
  | Font_format format -> "font-format(" ^ string_of_font_format format ^ ")"
  | Font_tech tech -> "font-tech(" ^ string_of_font_tech tech ^ ")"
  | At_rule rule -> "at-rule(@" ^ rule ^ ")"
  | Named_feature feature -> "named-feature(" ^ feature ^ ")"
  | Env name -> "env(" ^ name ^ ")"
  | General (name, args) -> name ^ "(" ^ args ^ ")"

let rec render context = function
  | Property feature -> "(" ^ render_declaration_feature feature ^ ")"
  | Function feature -> render_function_feature feature
  | Not cond ->
      let rendered = "not " ^ render_not_operand cond in
      if context = `Operand then "(" ^ rendered ^ ")" else rendered
  | And (a, b) -> render_branch `And a ^ " and " ^ render_branch `And b
  | Or (a, b) -> render_branch `Or a ^ " or " ^ render_branch `Or b

and render_not_operand = function
  | (And _ | Or _) as cond -> "(" ^ render `Root cond ^ ")"
  | cond -> render `Root cond

and render_branch operator = function
  | Or _ as cond when operator = `And -> "(" ^ render `Root cond ^ ")"
  | And _ as cond when operator = `Or -> "(" ^ render `Root cond ^ ")"
  | Not _ as cond -> render `Operand cond
  | cond -> render `Root cond

let to_string condition = render `Root condition

let pp_declaration_feature ctx = function
  | Declaration decl ->
      (* The declaration is a capability predicate for this exact value, so
         suppress lossy value rewrites (e.g. static colour folding). *)
      Declaration.pp_declaration (Pp.enter_feature_query ctx) decl
  | Empty name ->
      Pp.string ctx (string_of_property_name name);
      Pp.char ctx ':'
  | Unsupported (name, value) ->
      Pp.string ctx (string_of_property_name name);
      Pp.char ctx ':';
      Pp.space_if_pretty ctx ();
      Pp.string ctx value
  | Vendor_flag_enabled ->
      Pp.string ctx "-vendor-flag:";
      Pp.space_if_pretty ctx ();
      Pp.string ctx "enabled"

let pp_function_feature ctx = function
  | Selector selector -> Pp.call "selector" Selector.pp ctx selector
  | Font_format format ->
      Pp.call "font-format" Pp.string ctx (string_of_font_format format)
  | Font_tech tech ->
      Pp.call "font-tech" Pp.string ctx (string_of_font_tech tech)
  | At_rule rule -> Pp.call "at-rule" Pp.string ctx ("@" ^ rule)
  | Named_feature feature -> Pp.call "named-feature" Pp.string ctx feature
  | Env name -> Pp.call "env" Pp.string ctx name
  | General (name, args) -> Pp.call name Pp.string ctx args

let rec pp_aux ~in_and ctx = function
  | Property feature ->
      Pp.char ctx '(';
      pp_declaration_feature ctx feature;
      Pp.char ctx ')'
  | Function feature -> pp_function_feature ctx feature
  | Not cond -> pp_not ~in_and ctx cond
  | And (a, b) -> pp_and ctx a b
  | Or (a, b) -> pp_or ctx a b

and pp_not ~in_and ctx cond =
  if in_and then Pp.char ctx '(';
  Pp.string ctx "not ";
  (match cond with
  | And _ | Or _ ->
      Pp.char ctx '(';
      pp_aux ~in_and ctx cond;
      Pp.char ctx ')'
  | _ -> pp_aux ~in_and ctx cond);
  if in_and then Pp.char ctx ')'

and pp_and_branch ctx = function
  | Or _ as branch ->
      Pp.char ctx '(';
      pp_aux ~in_and:true ctx branch;
      Pp.char ctx ')'
  | branch -> pp_aux ~in_and:true ctx branch

and pp_and ctx a b =
  pp_and_branch ctx a;
  (* CSS Conditional 5 sec. 4.4: a [)and ] sequence is unambiguous so the
     leading space is droppable under minify; the trailing space is required to
     keep [and(] from re-tokenising as a function call. *)
  Pp.sp ctx ();
  Pp.string ctx "and ";
  pp_and_branch ctx b

and pp_or_branch ~is_left ctx = function
  | And (a, b) ->
      Pp.char ctx '(';
      pp_or_and_left ~is_left ctx a;
      Pp.string ctx " and ";
      pp_aux ~in_and:true ctx b;
      Pp.char ctx ')'
  | Not _ as branch -> pp_aux ~in_and:true ctx branch
  | branch -> pp_aux ~in_and:false ctx branch

and pp_or_and_left ~is_left ctx = function
  | Property _ as branch when Pp.minified ctx && is_left ->
      Pp.char ctx '(';
      pp_aux ~in_and:true ctx branch;
      Pp.char ctx ')'
  | branch -> pp_aux ~in_and:true ctx branch

and pp_or ctx a b =
  pp_or_branch ~is_left:true ctx a;
  Pp.sp ctx ();
  Pp.string ctx "or ";
  pp_or_branch ~is_left:false ctx b

let pp ctx t = pp_aux ~in_and:false ctx t

(* ===== Component parser ===== *)

let is_ws = function
  | Component.Preserved { kind = Token.Whitespace; _ } -> true
  | _ -> false

let strip_components = List.filter (fun cv -> not (is_ws cv))

let closed_block = function
  | Component.Block { node = { closed; _ }; _ }
  | Component.Func { node = { terminated = closed; _ }; _ } ->
      closed
  | _ -> true

let rec components_are_closed cvs =
  List.for_all
    (function
      | Component.Block { node = { value; closed; _ }; _ } ->
          closed && components_are_closed value
      | Component.Func { node = { arguments; terminated; _ }; _ } ->
          terminated && components_are_closed arguments
      | Component.Preserved _ -> true)
    cvs

let contains_top_level_semicolon =
  List.exists (function
    | Component.Preserved { kind = Token.Semicolon; _ } -> true
    | _ -> false)

let property_ident = function
  | [ Component.Preserved { kind = Token.Ident name; _ } ] -> Some name
  | _ -> None

let declaration_of_components prop value =
  if contains_top_level_semicolon value then
    failwith "Invalid declaration in @supports";
  match property_ident (strip_components prop) with
  | Some prop ->
      let value = Cursor.string_of_components ~trim:true value in
      property prop value
  | None -> failwith "Invalid declaration in @supports"

let function_call (fn : Component.func Component.node) =
  let name = fn.node.name in
  let args = fn.node.arguments in
  if not (fn.node.terminated && components_are_closed args) then
    failwith ("Unterminated " ^ name ^ "() in @supports");
  func name (Cursor.string_of_components ~trim:true args)

let peek_ident t =
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Ident name; _ }) ->
      Some (String.lowercase_ascii name)
  | _ -> None

let rec condition t =
  Cursor.ws t;
  match peek_ident t with
  | Some "not" ->
      Cursor.skip t;
      Not (in_parens ~allow_unwrapped_decl:false t)
  | _ ->
      let left = in_parens ~allow_unwrapped_decl:false t in
      chain t None left

and chain t op acc =
  Cursor.ws t;
  match peek_ident t with
  | Some "and" ->
      (match op with
      | Some `Or ->
          failwith "Cannot mix and/or without parentheses in @supports"
      | _ -> ());
      Cursor.skip t;
      let right = in_parens ~allow_unwrapped_decl:false t in
      chain t (Some `And) (And (acc, right))
  | Some "or" ->
      (match op with
      | Some `And ->
          failwith "Cannot mix and/or without parentheses in @supports"
      | _ -> ());
      Cursor.skip t;
      let right = in_parens ~allow_unwrapped_decl:false t in
      chain t (Some `Or) (Or (acc, right))
  | _ -> acc

and in_parens ~allow_unwrapped_decl t =
  let unwrapped_declaration t =
    let components = Cursor.remaining t in
    match split_top_level_colon components with
    | Some (prop, value) ->
        let decl = declaration_of_components prop value in
        ignore (Cursor.consume_remaining_as_string t : string);
        decl
    | None -> failwith "Expected supports feature"
  in
  Cursor.ws t;
  match Cursor.peek t with
  | Some
      (Component.Block { node = { opening = Token.Paren; value; _ }; _ } as cv)
    ->
      if not (closed_block cv) then
        failwith "Unmatched parenthesis in @supports condition";
      Cursor.skip t;
      paren_components value
  | Some (Component.Func fn) ->
      Cursor.skip t;
      function_call fn
  | _ when allow_unwrapped_decl -> unwrapped_declaration t
  | _ -> failwith "Expected supports feature"

and paren_components value =
  if strip_components value = [] then failwith "Empty parentheses in @supports";
  if not (components_are_closed value) then
    failwith "Unmatched parenthesis in @supports condition";
  match split_top_level_colon value with
  | Some (prop, value) -> declaration_of_components prop value
  | None ->
      let inner = Cursor.of_components value in
      let condition = condition inner in
      Cursor.ws inner;
      if not (Cursor.is_done inner) then
        failwith "trailing content in @supports group";
      condition

let of_string ?(allow_unwrapped_decl = false) s =
  let cursor = ref (Cursor.of_string s) in
  let raise_bad reason =
    Error.fail_bad_condition (Cursor.position !cursor) ~at_rule:"@supports"
      ~reason
  in
  try
    let cond =
      try condition !cursor
      with Failure _ when allow_unwrapped_decl ->
        cursor := Cursor.of_string s;
        in_parens ~allow_unwrapped_decl !cursor
    in
    Cursor.ws !cursor;
    if not (Cursor.is_done !cursor) then raise_bad "trailing content";
    cond
  with
  | Cursor.Parse_error _ as exn -> raise exn
  | Failure reason -> raise_bad reason

(* ===== Comparison ===== *)

let compare_declaration d1 d2 =
  let c =
    String.compare (Declaration.property_name d1) (Declaration.property_name d2)
  in
  if c <> 0 then c
  else
    String.compare
      (Declaration.string_of_value ~minify:true d1)
      (Declaration.string_of_value ~minify:true d2)

let compare_declaration_feature d1 d2 =
  let order = function
    | Empty _ -> 0
    | Vendor_flag_enabled -> 1
    | Declaration _ -> 2
    | Unsupported _ -> 3
  in
  match (d1, d2) with
  | Empty n1, Empty n2 ->
      String.compare (string_of_property_name n1) (string_of_property_name n2)
  | Vendor_flag_enabled, Vendor_flag_enabled -> 0
  | Declaration d1, Declaration d2 -> compare_declaration d1 d2
  | Unsupported (n1, v1), Unsupported (n2, v2) ->
      let c =
        String.compare (string_of_property_name n1) (string_of_property_name n2)
      in
      if c <> 0 then c else String.compare v1 v2
  | _ -> Stdlib.compare (order d1) (order d2)

let compare_function_feature a b =
  String.compare (render_function_feature a) (render_function_feature b)

let rec compare t1 t2 =
  match (t1, t2) with
  | Property d1, Property d2 -> compare_declaration_feature d1 d2
  | Function f1, Function f2 -> compare_function_feature f1 f2
  | Not a, Not b -> compare a b
  | And (a1, b1), And (a2, b2) ->
      let c = compare a1 a2 in
      if c <> 0 then c else compare b1 b2
  | Or (a1, b1), Or (a2, b2) ->
      let c = compare a1 a2 in
      if c <> 0 then c else compare b1 b2
  (* Order: Property < Function < Not < And < Or *)
  | Property _, _ -> -1
  | _, Property _ -> 1
  | Function _, _ -> -1
  | _, Function _ -> 1
  | Not _, _ -> -1
  | _, Not _ -> 1
  | And _, _ -> -1
  | _, And _ -> 1

let equal a b = compare a b = 0

(* An [@supports (prop: value)] test is written to detect a feature a browser
   might not support, so a guard for a not-yet-Baseline feature is load-bearing
   and must not be unwrapped. The feature lists live in {!Baseline}, generated
   from the web-features dataset. *)
let is_greenfield_property name =
  List.mem (String.lowercase_ascii name) Baseline.greenfield_properties

let contains_sub ~needle s =
  let nl = String.length needle and sl = String.length s in
  let rec go i = i + nl <= sl && (String.sub s i nl = needle || go (i + 1)) in
  nl > 0 && go 0

(* A [(prop: value)] test can pin a not-yet-Baseline feature through a value
   function ([anchor()], [calc-size()], ...) even on a Baseline property, so
   [@supports (width: anchor-size(--x))] keeps its guard. *)
let value_uses_greenfield value =
  let v = String.lowercase_ascii value in
  List.exists
    (fun fn -> contains_sub ~needle:(fn ^ "(") v)
    Baseline.greenfield_value_functions

let is_greenfield_feature decl =
  is_greenfield_property (Declaration.property_name decl)
  || value_uses_greenfield (Declaration.string_of_value ~minify:true decl)

(* Baseline classification of one declaration feature. A [(prop: value)] test
   whose property and value Cascade recognizes as Baseline is treated as
   Baseline-true; a not-yet-Baseline property or a greenfield value function
   keeps its guard ([`Unknown]). Properties Cascade does not model parse as
   [Unknown_property] and stay unknown, as do empty/unsupported/vendor-flag
   features. *)
let declaration_feature_truth = function
  | Declaration (Declaration.Declaration { property = Unknown_property _; _ })
    ->
      `Unknown
  | Declaration (Declaration.Theme_guarded _) -> `Unknown
  | Declaration decl when is_greenfield_feature decl -> `Unknown
  | Declaration _ -> `True
  | Empty _ | Unsupported _ | Vendor_flag_enabled -> `Unknown

(* Classify a feature query against the evergreen baseline and simplify it.
   [`True] / [`False] mean the whole condition is statically known; [`Cond c]
   keeps the residual condition with known-true conjuncts and known-false
   disjuncts removed. Function features (selector(), font-tech(), env(), ...)
   are never baseline facts, so they stay in the residual. *)
let rec simplify_baseline (cond : t) : [ `True | `False | `Cond of t ] =
  match cond with
  | Property feature -> (
      match declaration_feature_truth feature with
      | `True -> `True
      | `Unknown -> `Cond cond)
  | Function _ -> `Cond cond
  | Not inner -> (
      match simplify_baseline inner with
      | `True -> `False
      | `False -> `True
      | `Cond c -> `Cond (Not c))
  | And (a, b) -> (
      match (simplify_baseline a, simplify_baseline b) with
      | `False, _ | _, `False -> `False
      | `True, other | other, `True -> other
      | `Cond x, `Cond y -> `Cond (And (x, y)))
  | Or (a, b) -> (
      match (simplify_baseline a, simplify_baseline b) with
      | `True, _ | _, `True -> `True
      | `False, other | other, `False -> other
      | `Cond x, `Cond y -> `Cond (Or (x, y)))
