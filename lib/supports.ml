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

type property_name = Property_name of string

type declaration_feature =
  | Declaration of Declaration.t
  | Empty of property_name
  | Vendor_flag_enabled
  | Unparseable of { property : property_name; value : Component.t list }
      (** A [(<property>: <value>)] feature whose [<value>] does not parse as a
          typed declaration value for [<property>]. CSS Conditional Rules 4 §3.5
          routes this through the [<general-enclosed>] production: it is
          preserved verbatim and always evaluates to [false], rather than being
          a parse error in the API surface. *)

type t =
  | Property of declaration_feature
      (** [(property: value)] declaration feature test *)
  | Func of string * Component.t list
      (** [name(args)] function test (selector, font-format, font-tech, var,
          etc.) *)
  | Not of t  (** [not (condition)] negation *)
  | And of t * t  (** [(cond1) and (cond2)] conjunction *)
  | Or of t * t  (** [(cond1) or (cond2)] disjunction *)

let component_values s = Cursor.of_string s |> Cursor.remaining

let is_font_format = function
  | "collection" | "embedded-opentype" | "opentype" | "svg" | "truetype"
  | "woff" | "woff2" ->
      true
  | _ -> false

let is_font_tech = function
  | "features-opentype" | "features-aat" | "features-graphite" | "color-colrv0"
  | "color-colrv1" | "color-svg" | "color-sbix" | "color-cbdt" | "variations"
  | "palettes" | "incremental" ->
      true
  | _ -> false

let starts_with ~prefix s =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let property_name name =
  let reader = Cursor.of_string name in
  let parsed =
    try Cursor.ident ~keep_case:true reader
    with Cursor.Parse_error _ ->
      invalid_arg ("invalid supports declaration property name: " ^ name)
  in
  if not (Cursor.is_done reader) then
    invalid_arg ("invalid supports declaration property name: " ^ name);
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
      match Declaration.of_string (prop ^ ":" ^ value) with
      | Declaration.Declaration _ as decl -> Declaration decl
      | _ ->
          Unparseable
            { property = property_name prop; value = component_values value }
      | exception Error.Parse_error _ ->
          Unparseable
            { property = property_name prop; value = component_values value })

let property prop value = Property (declaration_feature prop value)
let func name args = Func (name, component_values args)

(* Selector is subsumed by [Func ("selector", ...)] for simplicity. The argument
   is still a CSS component stream, because arbitrary selector text is not a
   declaration value. *)

(* ===== Pretty printing ===== *)

let rec to_string condition = render `Root condition

and render context = function
  | Property feature -> "(" ^ render_declaration_feature feature ^ ")"
  | Func (name, args) -> name ^ "(" ^ Parser.to_string args ^ ")"
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

and render_declaration_feature = function
  | Declaration decl -> Declaration.string_of_declaration ~minify:false decl
  | Empty name -> string_of_property_name name ^ ":"
  | Vendor_flag_enabled -> "-vendor-flag: enabled"
  | Unparseable { property; value } ->
      string_of_property_name property ^ ": " ^ Parser.to_string value

let pp_declaration_feature ctx = function
  | Declaration decl -> Declaration.pp_declaration ctx decl
  | Empty name ->
      Pp.string ctx (string_of_property_name name);
      Pp.char ctx ':'
  | Vendor_flag_enabled ->
      Pp.string ctx "-vendor-flag:";
      Pp.space_if_pretty ctx ();
      Pp.string ctx "enabled"
  | Unparseable { property; value } ->
      Pp.string ctx (string_of_property_name property);
      Pp.char ctx ':';
      Pp.space_if_pretty ctx ();
      Pp.string ctx
        (if Pp.minified ctx then Parser.to_string_minified value
         else Parser.to_string value)

let rec pp_aux ~in_and ctx = function
  | Property feature ->
      Pp.char ctx '(';
      pp_declaration_feature ctx feature;
      Pp.char ctx ')'
  | Func (name, args) ->
      Pp.string ctx name;
      Pp.char ctx '(';
      Pp.string ctx
        (if Pp.minified ctx then Parser.to_string_minified args
         else Parser.to_string args);
      Pp.char ctx ')'
  | Not cond -> pp_not ~in_and ctx cond
  | And (a, b) -> pp_and ctx a b
  | Or (a, b) -> pp_or ctx a b

and pp_not ~in_and ctx cond =
  let extra_parens = Pp.minified ctx && not in_and in
  Pp.string ctx "(not ";
  if extra_parens then Pp.char ctx '(';
  (match cond with
  | And _ | Or _ ->
      Pp.char ctx '(';
      pp_aux ~in_and ctx cond;
      Pp.char ctx ')'
  | _ -> pp_aux ~in_and ctx cond);
  if extra_parens then Pp.char ctx ')';
  Pp.char ctx ')'

and pp_and_branch ctx = function
  | Or _ as branch ->
      Pp.char ctx '(';
      pp_aux ~in_and:true ctx branch;
      Pp.char ctx ')'
  | branch -> pp_aux ~in_and:true ctx branch

and pp_and ctx a b =
  pp_and_branch ctx a;
  Pp.string ctx " and ";
  pp_and_branch ctx b

and pp_or_branch ~is_left ctx = function
  | And (a, b) ->
      Pp.char ctx '(';
      pp_or_and_left ~is_left ctx a;
      Pp.string ctx " and ";
      pp_aux ~in_and:true ctx b;
      Pp.char ctx ')'
  | branch -> pp_aux ~in_and:false ctx branch

and pp_or_and_left ~is_left ctx = function
  | Property _ as branch when Pp.minified ctx && is_left ->
      Pp.char ctx '(';
      pp_aux ~in_and:true ctx branch;
      Pp.char ctx ')'
  | branch -> pp_aux ~in_and:true ctx branch

and pp_or ctx a b =
  pp_or_branch ~is_left:true ctx a;
  Pp.string ctx " or ";
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

let split_top_level_colon cvs =
  let rec loop before = function
    | [] -> None
    | Component.Preserved { kind = Token.Colon; _ } :: after ->
        Some (List.rev before, after)
    | cv :: rest -> loop (cv :: before) rest
  in
  loop [] cvs

let contains_top_level_semicolon =
  List.exists (function
    | Component.Preserved { kind = Token.Semicolon; _ } -> true
    | _ -> false)

let property_ident = function
  | [ Component.Preserved { kind = Token.Ident name; _ } ] -> Some name
  | _ -> None

let declaration_from_components prop value =
  if contains_top_level_semicolon value then
    failwith "Invalid declaration in @supports";
  match property_ident (strip_components prop) with
  | Some prop ->
      let value = Cursor.components_to_string ~trim:true value in
      property prop value
  | None -> failwith "Invalid declaration in @supports"

let validate_ident_components name args is_valid =
  match strip_components args with
  | [ Component.Preserved { kind = Token.Ident ident; _ } ]
    when is_valid (String.lowercase_ascii ident) ->
      ()
  | _ -> failwith ("Invalid " ^ name ^ "() in @supports")

let validate_single_ident_components name args =
  match strip_components args with
  | [ Component.Preserved { kind = Token.Ident _; _ } ] -> ()
  | _ -> failwith ("Invalid " ^ name ^ "() in @supports")

let validate_at_rule_components args =
  match strip_components args with
  | [ Component.Preserved { kind = Token.At_keyword _; _ } ] -> ()
  | _ -> failwith "Invalid at-rule() in @supports"

let validate_selector_components args =
  try
    let cursor = Cursor.of_components args in
    ignore (Selector.read cursor : Selector.t)
  with Error.Parse_error _ -> failwith "Invalid selector() in @supports"

let function_call (fn : Component.func Component.node) =
  let name = fn.node.name in
  let args = fn.node.arguments in
  if not (fn.node.terminated && components_are_closed args) then
    failwith ("Unterminated " ^ name ^ "() in @supports");
  let lower_name = String.lowercase_ascii name in
  if
    strip_components args = []
    &&
    (lower_name = "selector" || lower_name = "font-format"
   || lower_name = "font-tech" || lower_name = "at-rule"
   || lower_name = "named-feature" || lower_name = "env")
  then failwith ("Empty " ^ name ^ "() in @supports");
  if lower_name = "selector" then validate_selector_components args;
  if lower_name = "font-format" then
    validate_ident_components name args is_font_format;
  if lower_name = "font-tech" then validate_ident_components name args is_font_tech;
  if lower_name = "at-rule" then validate_at_rule_components args;
  if lower_name = "named-feature" || lower_name = "env" then
    validate_single_ident_components name args;
  Func (name, args)

let peek_ident t =
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Ident name; _ }) ->
      Some (String.lowercase_ascii name)
  | _ -> None

let rec parse_condition t =
  Cursor.ws t;
  match peek_ident t with
  | Some "not" ->
      Cursor.skip t;
      Not (parse_in_parens ~allow_unwrapped_decl:false t)
  | _ ->
      let left = parse_in_parens ~allow_unwrapped_decl:false t in
      parse_chain t None left

and parse_chain t op acc =
  Cursor.ws t;
  match peek_ident t with
  | Some "and" ->
      (match op with
      | Some `Or -> failwith "Cannot mix and/or without parentheses in @supports"
      | _ -> ());
      Cursor.skip t;
      let right = parse_in_parens ~allow_unwrapped_decl:false t in
      parse_chain t (Some `And) (And (acc, right))
  | Some "or" ->
      (match op with
      | Some `And -> failwith "Cannot mix and/or without parentheses in @supports"
      | _ -> ());
      Cursor.skip t;
      let right = parse_in_parens ~allow_unwrapped_decl:false t in
      parse_chain t (Some `Or) (Or (acc, right))
  | _ -> acc

and parse_in_parens ~allow_unwrapped_decl t =
  Cursor.ws t;
  match Cursor.peek t with
  | Some (Component.Block { node = { opening = Token.Paren; value; _ }; _ } as cv)
    ->
      if not (closed_block cv) then
        failwith "Unmatched parenthesis in @supports condition";
      Cursor.skip t;
      parse_paren_components value
  | Some (Component.Func fn) ->
      Cursor.skip t;
      function_call fn
  | _ when allow_unwrapped_decl -> parse_unwrapped_declaration t
  | _ -> failwith "Expected supports feature"

and parse_paren_components value =
  if strip_components value = [] then failwith "Empty parentheses in @supports";
  if not (components_are_closed value) then
    failwith "Unmatched parenthesis in @supports condition";
  match split_top_level_colon value with
  | Some (prop, value) -> declaration_from_components prop value
  | None ->
      let inner = Cursor.of_components value in
      let condition = parse_condition inner in
      Cursor.ws inner;
      if not (Cursor.is_done inner) then
        failwith "trailing content in @supports group";
      condition

and parse_unwrapped_declaration t =
  let components = Cursor.remaining t in
  match split_top_level_colon components with
  | Some (prop, value) ->
      let decl = declaration_from_components prop value in
      ignore (Cursor.consume_remaining_to_string t : string);
      decl
  | None -> failwith "Expected supports feature"

let of_string ?(allow_unwrapped_decl = false) s =
  let cursor, cond =
    let cursor = Cursor.of_string s in
    try (cursor, parse_condition cursor)
    with Failure _ when allow_unwrapped_decl ->
      let cursor = Cursor.of_string s in
      (cursor, parse_in_parens ~allow_unwrapped_decl cursor)
  in
  Cursor.ws cursor;
  if not (Cursor.is_done cursor) then failwith "trailing content in @supports";
  cond

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
    | Unparseable _ -> 3
  in
  match (d1, d2) with
  | Empty n1, Empty n2 ->
      String.compare (string_of_property_name n1) (string_of_property_name n2)
  | Vendor_flag_enabled, Vendor_flag_enabled -> 0
  | Declaration d1, Declaration d2 -> compare_declaration d1 d2
  | ( Unparseable { property = p1; value = v1 },
      Unparseable { property = p2; value = v2 } ) ->
      let cp =
        String.compare (string_of_property_name p1) (string_of_property_name p2)
      in
      if cp <> 0 then cp
      else String.compare (Parser.to_string v1) (Parser.to_string v2)
  | _ -> Stdlib.compare (order d1) (order d2)

let rec compare t1 t2 =
  match (t1, t2) with
  | Property d1, Property d2 -> compare_declaration_feature d1 d2
  | Func (n1, a1), Func (n2, a2) ->
      let c = String.compare n1 n2 in
      if c <> 0 then c
      else
        String.compare
          (Parser.to_string_minified a1)
          (Parser.to_string_minified a2)
  | Not a, Not b -> compare a b
  | And (a1, b1), And (a2, b2) ->
      let c = compare a1 a2 in
      if c <> 0 then c else compare b1 b2
  | Or (a1, b1), Or (a2, b2) ->
      let c = compare a1 a2 in
      if c <> 0 then c else compare b1 b2
  (* Order: Property < Func < Not < And < Or *)
  | Property _, _ -> -1
  | _, Property _ -> 1
  | Func _, _ -> -1
  | _, Func _ -> 1
  | Not _, _ -> -1
  | _, Not _ -> 1
  | And _, _ -> -1
  | _, And _ -> 1

let equal a b = compare a b = 0
