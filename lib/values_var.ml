(** [var()], [env()] and [attr()] references, and preserved component values. *)

open Values_intf
open Values_color_name

let var_ref ?fallback ?default ?layer ?meta ?(runtime = false) name =
  let fallback : _ fallback =
    match fallback with None -> None | Some x -> x
  in
  { name; fallback; default; layer; meta; runtime }

let syntax_fallback s = Syntax_fallback (Cursor.remaining (Cursor.of_string s))

let string_of_number_percentage (np : number_percentage) =
  match np with Num f | Pct f -> Pp.string_of_float f | _ -> "initial"

(* Opens [var(] and writes the referenced name. [name] is the custom property's
   name without its [--] prefix, and CSS Syntax 3 (ED) sec. 4.3.7 lets an escape
   carry a [;] or a [}] into it, so the tail is written with the escapes that
   read the same name back. *)
let pp_var_open ctx name =
  Pp.string ctx "var(--";
  Pp.string ctx (Parser.escape_name (Pp.rename_custom_property ctx name))

(* Prints the [var(--fallback_name)] used as another var's fallback, then the
   outer var's closing paren. Theme resolution is a transform, not a print
   concern, so the reference is emitted structurally. *)
let pp_var_fallback ctx fallback_name =
  pp_var_open ctx fallback_name;
  Pp.string ctx "))"

(* A [Syntax_fallback] is the [<declaration-value>] a [var()] in a typed slot
   falls back to, so the slot's grammar reads it once substituted and the
   whitespace beside a [*] or [/] is spelling, not content: two shadows whose
   colour fallbacks differ only around the alpha slash paint the same. Only a
   custom property's own value keeps that separator, and that stream never
   prints through here. *)
let string_of_syntax_fallback ctx value =
  if Pp.minified ctx then
    Parser.to_string_custom_minified ~fold_ident:fold_custom_value_ident
      ~opaque:false value
  else Parser.string_of_components value

let pp_syntax_fallback ctx value =
  Pp.string ctx (string_of_syntax_fallback ctx value)

let pp_var_ref ctx name =
  pp_var_open ctx name;
  Pp.char ctx ')'

let pp_empty_var ctx name =
  pp_var_open ctx name;
  Pp.char ctx ',';
  Pp.char ctx ')'

let pp_empty2_var ctx name =
  pp_var_open ctx name;
  Pp.string ctx ",  )"

let pp_typed_var_fallback pp_value ctx name value =
  pp_var_open ctx name;
  Pp.comma ctx ();
  pp_value { ctx with in_function = true } value;
  Pp.char ctx ')'

let pp_syntax_var_fallback ctx name value =
  pp_var_open ctx name;
  Pp.comma ctx ();
  pp_syntax_fallback { ctx with in_function = true } value;
  Pp.char ctx ')'

let first_top_level_comma_segment s =
  let len = String.length s in
  let quote_after_char i quote =
    if s.[i] = quote && (i = 0 || s.[i - 1] <> '\\') then Option.None
    else Some quote
  in
  let rec loop i depth (quote : char option) =
    if i >= len then s
    else
      match (quote, s.[i]) with
      | Some quote, _ -> loop (i + 1) depth (quote_after_char i quote)
      | Option.None, ('"' | '\'') -> loop (i + 1) depth (Some s.[i])
      | Option.None, '(' -> loop (i + 1) (depth + 1) Option.None
      | Option.None, ')' when depth > 0 -> loop (i + 1) (depth - 1) Option.None
      | Option.None, ',' when depth = 0 -> String.sub s 0 i
      | Option.None, _ -> loop (i + 1) depth Option.None
  in
  loop 0 0 Option.None

let pp_inline_var : type a. a Pp.t -> a var Pp.t =
 fun pp_value ctx v ->
  match v.default with
  | Some value -> pp_value ctx value
  | Option.None -> (
      match v.fallback with
      | Fallback value -> pp_value ctx value
      | Var_fallback _ -> pp_var_ref ctx v.name
      | Syntax_fallback value ->
          pp_syntax_fallback { ctx with in_function = true } value
      | Empty | Empty2 -> ()
      | None -> pp_var_ref ctx v.name)

(* Print the [var()] reference structurally. Theme resolution (keep / inline a
   default) is an AST transform that runs before printing, so the printer never
   consults a theme set or resolver. *)
let pp_stylesheet_var : type a. a Pp.t -> a var Pp.t =
 fun pp_value ctx v ->
  match v.fallback with
  | None -> pp_var_ref ctx v.name
  | Empty -> pp_empty_var ctx v.name
  | Empty2 -> pp_empty2_var ctx v.name
  | Fallback value -> pp_typed_var_fallback pp_value ctx v.name value
  | Syntax_fallback value -> pp_syntax_var_fallback ctx v.name value
  | Var_fallback fallback_name ->
      pp_var_open ctx v.name;
      Pp.comma ctx ();
      pp_var_fallback ctx fallback_name

let pp_var : type a. a Pp.t -> a var Pp.t =
 fun pp_value ctx v ->
  if ctx.inline then pp_inline_var pp_value ctx v
  else pp_stylesheet_var pp_value ctx v

let pp_env : type a. a Pp.t -> a env Pp.t =
 fun pp_value ctx (env : a env) ->
  Pp.string ctx "env(";
  Pp.string ctx env.name;
  List.iter
    (fun i ->
      Pp.sp ctx ();
      Pp.string ctx (string_of_int i))
    env.indices;
  (match env.fallback with
  | Some fallback ->
      Pp.comma ctx ();
      pp_value ctx fallback
  | Option.None -> ());
  Pp.char ctx ')'

(* CSS Custom Properties 1 (ED) sec. 4.1 forbids normalizing the whitespace of
   the streams this prints, so the runs come back as they were read. *)
let pp_component_values ctx values =
  let value =
    if Pp.minified ctx then
      Parser.to_string_custom_minified ~fold_ident:fold_custom_value_ident
        values
    else Parser.to_string_verbatim values
  in
  Pp.string ctx value

let read_component_values t = Cursor.remaining t
let pp_invalid_value = pp_component_values
let read_invalid_value = read_component_values

let string_of_attr_syntax : attr_syntax -> string = function
  | Length -> "<length>"
  | Length_percentage -> "<length-percentage>"
  | Color -> "<color>"
  | Number -> "<number>"
  | Percentage -> "<percentage>"

let pp_attr_syntax ctx syntax = Pp.string ctx (string_of_attr_syntax syntax)

let pp_attr_type ctx (type_ : attr_type) =
  match type_ with
  | Type syntax ->
      Pp.string ctx "type(";
      pp_attr_syntax ctx syntax;
      Pp.char ctx ')'
  | Unit unit_ -> Pp.string ctx unit_
  | Raw_string -> Pp.string ctx "raw-string"
  | Number_type -> Pp.string ctx "number"

let pp_attr_call pp_value ctx (attr : _ attr_call) =
  Pp.string ctx attr.name;
  Option.iter
    (fun type_ ->
      Pp.space ctx ();
      pp_attr_type ctx type_)
    attr.type_;
  match attr.fallback with
  | No_fallback -> ()
  | Empty_fallback -> Pp.comma ctx ()
  | Attr_fallback value ->
      Pp.comma ctx ();
      pp_value ctx value

let attr_syntax_of_string : string -> attr_syntax option = function
  | "<length>" -> Some Length
  | "<length-percentage>" -> Some Length_percentage
  | "<color>" -> Some Color
  | "<number>" -> Some Number
  | "<percentage>" -> Some Percentage
  | _ -> None

let read_attr_syntax t : attr_syntax =
  Cursor.ws t;
  let body = Cursor.consume_remaining_as_string ~trim:true t in
  match attr_syntax_of_string body with
  | Some syntax -> syntax
  | None -> Cursor.err_invalid t ("attr() type: " ^ body)

let read_attr_type t : attr_type =
  Cursor.ws t;
  if Cursor.looking_at_func "type" t then
    Type (Cursor.call "type" t read_attr_syntax)
  else
    match Cursor.ident_opt t with
    | Some "raw-string" -> Raw_string
    | Some "number" -> Number_type
    | Some unit_ -> Unit unit_
    | None when Cursor.peek_delim t = Some '%' ->
        Cursor.expect '%' t;
        Unit "%"
    | None -> Cursor.err_expected t "attr() type"

(** Read the body of a var(name[, fallback]) call, given a cursor over the
    function arguments. *)
let read_var_body : type a. (Cursor.t -> a) -> Cursor.t -> a var =
 fun read_value t ->
  Cursor.ws t;
  let name = Cursor.ident ~keep_case:true t in
  (* CSS Custom Properties 1: a [var()] reference must name a [<dashed-ident>]
     other than the bare reserved [--] keyword. *)
  if not (Custom_property_name.is_valid name) then
    Cursor.err_invalid t ("var() requires a custom-property name: " ^ name);
  let var_name = Custom_property_name.strip_prefix name in
  Cursor.ws t;
  let fallback : _ fallback =
    if not (Cursor.comma_opt t) then None
    else (
      Cursor.ws t;
      if Cursor.is_done t then Empty
      else
        match Cursor.try_parse_full_err read_value t with
        (* The fallback is a [<declaration-value>]: it swallows the rest of the
           argument list whether or not it parses as the target syntax, so it is
           consumed either way. *)
        | Ok fb -> Fallback fb
        | Error _ -> Syntax_fallback (Cursor.consume_remaining t))
  in
  var_ref ~fallback var_name

(** Generic [var(...)] parser. Consumes the [var(...)] [Func] and applies
    [read_value] to the fallback, if any. *)
let read_var : type a. (Cursor.t -> a) -> Cursor.t -> a var =
 fun read_value t ->
  Cursor.call "var" t (fun inner -> read_var_body read_value inner)

let read_body : type a. (Cursor.t -> a) -> Cursor.t -> a env =
 fun read_value t ->
  Cursor.ws t;
  let name = Cursor.ident ~keep_case:true t in
  let rec indices acc =
    Cursor.ws t;
    match Cursor.integer_opt t with
    | Some i -> indices (i :: acc)
    | Option.None -> List.rev acc
  in
  let indices = indices [] in
  Cursor.ws t;
  let fallback =
    if Cursor.comma_opt t then (
      Cursor.ws t;
      if Cursor.is_done t then Option.None else Some (read_value t))
    else Option.None
  in
  Cursor.ws t;
  Cursor.expect_eof t;
  ({ name; indices; fallback } : a env)

let read_env : type a. (Cursor.t -> a) -> Cursor.t -> a env =
 fun read_value t ->
  Cursor.call "env" t (fun inner -> read_body read_value inner)

(* Var helper functions *)
let var_name (v : 'a var) = v.name
let var_layer (v : 'a var) = v.layer
let var_meta (v : 'a var) = v.meta

let with_fallback (v : 'a var) fallback_value : 'a var =
  { v with fallback = Fallback fallback_value }
