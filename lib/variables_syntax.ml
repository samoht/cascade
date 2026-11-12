open Values
open Properties
open Declaration
open Syntax
open Variables_intf

let custom_value_ident name =
  [ Component.Preserved (Token.synthetic (Token.Ident name)) ]

let custom_value_var_empty_fallback name =
  let loc = Loc.dummy in
  let ident =
    Component.Preserved (Token.synthetic (Token.Ident ("--" ^ name)))
  in
  let comma = Component.Preserved (Token.synthetic Token.Comma) in
  [
    Component.Func
      {
        node = { name = "var"; arguments = [ ident; comma ]; terminated = true };
        loc;
      };
  ]

let string_of_custom_value = Parser.string_of_components

(* The first dashed-ident argument of a [var()] is the referenced custom
   property; leading whitespace is skipped. *)
let rec first_var_ref_ident = function
  | [] -> Option.None
  | Component.Preserved { Token.kind = Token.Ident n; _ } :: _
    when Custom_property_name.is_valid n ->
      Option.Some n
  | _ :: rest -> first_var_ref_ident rest

(* Custom-property names referenced through a real [var()] function anywhere in
   a component stream, recursing into function arguments and bracketed blocks. A
   [var(] inside a string or url is an atomic [Preserved] token, never a [Func],
   so it is correctly ignored - the false positive a text scan would produce. *)
let rec var_refs_in_components acc (components : Component.t list) =
  List.fold_left
    (fun acc (c : Component.t) ->
      match c with
      | Component.Func { node = { name; arguments; _ }; _ } ->
          let acc =
            if String.lowercase_ascii name = "var" then
              match first_var_ref_ident arguments with
              | Option.Some n -> n :: acc
              | Option.None -> acc
            else acc
          in
          var_refs_in_components acc arguments
      | Component.Block { node = { value; _ }; _ } ->
          var_refs_in_components acc value
      | Component.Preserved _ -> acc)
    acc components

let components_of_value_string value =
  let p = Parser.of_string value in
  let rec collect acc =
    match Parser.next p with
    | Component.Preserved { Token.kind = Token.Eof; _ } -> List.rev acc
    | c -> collect (c :: acc)
  in
  collect []

let var_refs_in_value_string value =
  var_refs_in_components [] (components_of_value_string value)

(* CSS Values 5 calls var(), attr() and env() arbitrary substitution functions:
   what each stands for is known only while an element is being styled. Walks
   into arguments and blocks, so one nested in another's fallback still counts,
   and a [var(] inside a string or url stays an atomic [Preserved] token rather
   than a [Func], as it does for the reference walk above. *)
let rec substitution_fn_in_components (components : Component.t list) =
  List.find_map
    (fun (c : Component.t) ->
      match c with
      | Component.Func { node = { name; arguments; _ }; _ } -> (
          match String.lowercase_ascii name with
          | ("var" | "attr" | "env") as fn -> Option.Some fn
          | _ -> substitution_fn_in_components arguments)
      | Component.Block { node = { value; _ }; _ } ->
          substitution_fn_in_components value
      | Component.Preserved _ -> Option.None)
    components

let substitution_fn_in_value_string value =
  substitution_fn_in_components (components_of_value_string value)

(* The same walk for a dimension whose unit resolves against an element. Goes
   into arguments and blocks, so one inside a [calc()] counts as much as one
   written on its own. *)
let rec element_relative_in_components (components : Component.t list) =
  List.find_map
    (fun (c : Component.t) ->
      match c with
      | Component.Preserved { Token.kind = Token.Dimension { unit_; _ }; _ }
        when Values.is_element_relative_unit unit_ ->
          Option.Some unit_
      | Component.Preserved _ -> Option.None
      | Component.Func { node = { arguments; _ }; _ } ->
          element_relative_in_components arguments
      | Component.Block { node = { value; _ }; _ } ->
          element_relative_in_components value)
    components

let element_relative_in_value_string value =
  element_relative_in_components (components_of_value_string value)

(** Pretty-print a syntax descriptor to CSS syntax string *)
let rec pp_syntax_inner : type a. a syntax Pp.t =
 fun ctx syn ->
  match syn with
  | Length -> Pp.string ctx "<length>"
  | Color -> Pp.string ctx "<color>"
  | Number -> Pp.string ctx "<number>"
  | Integer -> Pp.string ctx "<integer>"
  | Percentage -> Pp.string ctx "<percentage>"
  | Length_percentage -> Pp.string ctx "<length-percentage>"
  | Angle -> Pp.string ctx "<angle>"
  | Time -> Pp.string ctx "<time>"
  | Resolution -> Pp.string ctx "<resolution>"
  | Custom_ident -> Pp.string ctx "<custom-ident>"
  | String -> Pp.string ctx "<string>"
  | Url -> Pp.string ctx "<url>"
  | Image -> Pp.string ctx "<image>"
  | Transform_function -> Pp.string ctx "<transform-function>"
  | Transform_list -> Pp.string ctx "<transform-list>"
  | Universal -> Pp.string ctx "*"
  | Or (syn1, syn2) ->
      pp_syntax_inner ctx syn1;
      if Pp.minified ctx then Pp.char ctx '|' else Pp.string ctx " | ";
      pp_syntax_inner ctx syn2
  | Plus syn ->
      pp_syntax_inner ctx syn;
      Pp.string ctx "+"
  | Hash syn ->
      pp_syntax_inner ctx syn;
      Pp.string ctx "#"
  | Ident_keyword name -> Pp.string ctx name

let pp_syntax : type a. a syntax Pp.t =
 fun ctx syn ->
  (* Syntax descriptors should be printed with quotes per CSS spec *)
  Pp.char ctx '"';
  pp_syntax_inner ctx syn;
  Pp.char ctx '"'

(** Pretty-print a value according to its syntax type *)
let pp_registered_angle ctx = function
  | Deg 0. when Pp.minified ctx -> Pp.string ctx "0deg"
  | angle -> Values.pp_angle ctx angle

let rec pp_value : type a. a syntax -> a Pp.t =
 fun syntax ctx value ->
  match syntax with
  | Length -> Values.pp_length ~always:true ctx value
  | Color -> Values.pp_color ctx value
  | Number -> Pp.float ctx value
  | Integer -> Pp.int ctx value
  | Percentage -> Values.pp_percentage ~always:true ctx value
  | Length_percentage -> Values.pp_length_percentage ~always:true ctx value
  | Angle -> pp_registered_angle ctx value
  | Time -> Values.pp_duration ctx value
  | Resolution -> Pp.string ctx value
  | Custom_ident -> Pp.string ctx value
  | String -> Pp.quoted ctx value
  | Url -> Pp.url ctx value
  | Image ->
      let value =
        if Pp.minified ctx then Properties.minify_background_image value
        else value
      in
      Properties.pp_background_image ctx value
  | Transform_function -> Pp.string ctx value
  | Transform_list -> Pp.string ctx value
  | Universal -> Pp.string ctx value
  | Or (syn1, syn2) -> (
      match value with
      | Left v -> pp_value syn1 ctx v
      | Right v -> pp_value syn2 ctx v)
  | Plus syn ->
      (* The [+] separator is the whole of it: drop the space and [10px 20px] is
         one dimension whose unit is [px20px]. *)
      List.iteri
        (fun i v ->
          if i > 0 then Pp.space ctx ();
          pp_value syn ctx v)
        value
  | Hash syn ->
      List.iteri
        (fun i v ->
          if i > 0 then Pp.comma ctx ();
          pp_value syn ctx v)
        value
  | Ident_keyword name -> Pp.string ctx name

(* A value already typed by [syntax] is already spelled for the substitution
   site (CSS Variables 1 sec. 2); [custom_property] still checks the name and
   parses the printed text into the token stream a declaration holds. *)
let typed_custom_property ?layer name syntax value =
  custom_property ?layer name
    (Pp.to_string ~minify:true (pp_value syntax) value)

(* CSS Properties and Values API 1 (ED) sec. 5.1 lists the named [<...>] type
   references. Bare ident keywords match the [<custom-ident>] shape so a leading
   letter followed by ident-continue characters counts; this rejects stray
   punctuation. *)
let is_ident_keyword s =
  String.length s > 0
  && is_ascii_ident_start s.[0]
  &&
  let rec loop i =
    if i = String.length s then true
    else if is_ascii_ident_continue s.[i] then loop (i + 1)
    else false
  in
  loop 1

(** Read a CSS syntax descriptor from input *)
let read_simple_syntax_component r s : any_syntax =
  match s with
  | "<length>" -> Syntax Length
  | "<color>" -> Syntax Color
  | "<number>" -> Syntax Number
  | "<integer>" -> Syntax Integer
  | "<percentage>" -> Syntax Percentage
  | "<length-percentage>" -> Syntax Length_percentage
  | "<angle>" -> Syntax Angle
  | "<time>" -> Syntax Time
  | "<resolution>" -> Syntax Resolution
  | "<custom-ident>" -> Syntax Custom_ident
  | "<string>" -> Syntax String
  | "<url>" -> Syntax Url
  | "<image>" -> Syntax Image
  | "<transform-function>" -> Syntax Transform_function
  | "<transform-list>" -> Syntax Transform_list
  | s when is_ident_keyword s -> Syntax (Ident_keyword s)
  | s -> Cursor.err_invalid r ("Unsupported CSS syntax: " ^ s)

(* CSS Properties and Values API 1 (ED) sec. 5.2: only [+] and [#] are valid
   syntax multipliers. *)
let apply_syntax_modifier r (Syntax inner) (modifier : char option) : any_syntax
    =
  match (inner, modifier) with
  | Transform_list, Some ('+' | '#') ->
      Cursor.err_invalid r
        "a pre-multiplied CSS syntax component cannot take a multiplier"
  | inner, None -> Syntax inner
  | inner, Some '+' -> Syntax (Plus inner)
  | inner, Some '#' -> Syntax (Hash inner)
  | _, Some c ->
      Cursor.err_invalid r
        (String.concat ""
           [ "Unsupported CSS syntax modifier: '"; String.make 1 c; "'" ])

(* CSS Properties and Values API 1 (ED) sec. 5.4.3 sets a component's multiplier
   from a single [+] or [#] and returns, so a component carries at most one.
   Sec. 5.4.2 accepts only EOF or [|] after a component, which fails the whole
   syntax definition on a second multiplier; here that one is left in the body
   for [read_simple_syntax_component] to reject, the route [++], [##] and [#+]
   already take. *)
let split_syntax_modifier s : string * char option =
  let n = String.length s in
  if n = 0 then (s, None)
  else
    let last = s.[n - 1] in
    if last <> '+' && last <> '#' then (s, None)
    else (String.sub s 0 (n - 1), Some last)

let read_syntax (r : Cursor.t) : any_syntax =
  (* CSS @property syntax values must be quoted strings per spec *)
  let s = String.trim (Cursor.string r) in
  let read_component part =
    let body, modifier = split_syntax_modifier (String.trim part) in
    if body = "" then Cursor.err_invalid r "empty CSS syntax component";
    apply_syntax_modifier r (read_simple_syntax_component r body) modifier
  in
  (* Sec. 5.4.2 strips the surrounding whitespace, then returns the universal
     syntax definition only for a string that is [*] and nothing else. As a
     component [*] is none of the shapes sec. 5.4.3 accepts, so it carries no
     multiplier and joins no alternation. *)
  if s = "*" then Syntax Universal
  else if String.contains s '|' then
    let parts = String.split_on_char '|' s in
    let components = List.map read_component parts in
    match components with
    | [] | [ _ ] ->
        (* split_on_char never returns empty list; the single-element case
           cannot reach here because '|' is in [s]. *)
        Cursor.err_invalid r "invalid CSS syntax disjunction"
    | first :: rest ->
        List.fold_left
          (fun (Syntax left) (Syntax right) -> Syntax (Or (left, right)))
          first rest
  else read_component s

(* These types have no reader of their own and take whatever is left. None of
   them spells a value as nothing, and a reader that succeeded on an empty
   stream would leave the [+] and [#] repetitions below with no way to stop. *)
let read_remaining_typed reader what =
  let value = Cursor.consume_remaining_as_string ~trim:true reader in
  if value = "" then Cursor.err_expected reader what;
  value

(** Read a value according to its syntax type *)
let rec read_value : type a. Cursor.t -> a syntax -> a =
 fun reader syntax ->
  match syntax with
  | Universal ->
      (* For universal syntax "*", accept any CSS value -- consume the remaining
         components and serialise them back to source text so the surrounding
         [expect_eof] sees an empty cursor. *)
      Cursor.consume_remaining_as_string ~trim:true reader
  | String -> Cursor.string ~trim:true reader
  | Custom_ident -> Cursor.ident ~keep_case:true reader
  | Url -> Cursor.url reader
  | Image -> Properties.read_background_image reader
  | Transform_function -> read_remaining_typed reader "<transform-function>"
  | Transform_list -> read_remaining_typed reader "<transform-list>"
  | Resolution -> read_remaining_typed reader "<resolution>"
  | Length -> Values.read_length reader
  | Color -> Values.read_color reader
  | Number -> Cursor.number reader
  | Integer -> Cursor.int reader
  | Percentage -> Values.read_percentage reader
  | Length_percentage -> Values.read_length_percentage reader
  | Angle -> Values.read_angle_unit_required reader
  | Time -> Values.read_duration reader
  | Or (syn1, syn2) -> (
      (* Try the left branch with backtracking; fall back to the right. *)
      match Cursor.option (fun r -> read_value r syn1) reader with
      | Some v -> Either.Left v
      | None -> Either.Right (read_value reader syn2))
  | Plus syn ->
      (* Read space-separated list - use Cursor.many for proper error
         handling *)
      let values, _error_opt = Cursor.many (fun r -> read_value r syn) reader in
      if values = [] then
        Cursor.err_invalid reader "expected at least one value for '+' syntax"
      else values
  | Hash syn ->
      (* Read comma-separated list - use Cursor.list for proper parsing *)
      let values =
        Cursor.list ~sep:Cursor.comma ~at_least:1
          (fun r -> read_value r syn)
          reader
      in
      values
  | Ident_keyword name ->
      let got = Cursor.ident reader in
      if got <> name then
        Cursor.err_invalid reader
          (String.concat ""
             [ "expected keyword '"; name; "', got '"; got; "'" ])

open Common

let list_map_preserve = List.map_preserve

let rec normalize_value : type a. ?lossless:bool -> a syntax -> a -> a =
 fun ?(lossless = false) syntax value ->
  match syntax with
  | Color -> Values.normalize_color ~lossless value
  | Or (left, right) -> (
      match value with
      | Either.Left v ->
          let v' = normalize_value ~lossless left v in
          if v' == v then value else Either.Left v'
      | Either.Right v ->
          let v' = normalize_value ~lossless right v in
          if v' == v then value else Either.Right v')
  | Plus syntax -> list_map_preserve (normalize_value ~lossless syntax) value
  | Hash syntax -> list_map_preserve (normalize_value ~lossless syntax) value
  (* A registered [<length>] or [<length-percentage>] reads a unitless zero as
     the length it names, as a declaration of that type does, so it takes the
     shorter [0] a declaration's zero takes; a zero [<percentage>] keeps its
     [%]. The value comes back as it was when nothing shortens, so the caller's
     physical-equality check still reports no change. *)
  | Length ->
      let value' = Values.normalize_length value in
      if Values.equal_length value' value then value else value'
  | Length_percentage ->
      let value' = Values.normalize_length_percentage value in
      if Values.equal_length_percentage value' value then value else value'
  | Number | Integer | Percentage | Angle | Time | Resolution | Custom_ident
  | String | Url | Image | Transform_function | Transform_list | Universal
  | Ident_keyword _ ->
      value

let meta (type t) () =
  let module M = struct
    type meta += V : t -> meta
  end in
  let inj x = M.V x in
  let proj = function M.V v -> Some v | _ -> None in
  (inj, proj)

let var : type a.
    ?default:a ->
    ?fallback:a fallback ->
    ?layer:string ->
    ?meta:meta ->
    ?runtime:bool ->
    string ->
    a kind ->
    a ->
    declaration * a var =
 fun ?default ?fallback ?layer ?meta ?(runtime = false) name kind value ->
  (* Create the declaration directly with the value *)
  let decl =
    Declaration.v
      (Custom_property (String.concat "" [ "--"; name ]))
      (Custom_value { value = Typed { kind; value }; layer; meta })
  in
  let fallback : a fallback =
    match fallback with None -> None | Some v -> v
  in
  (* Use the value as default if no explicit default provided *)
  let default_value =
    match default with Some d -> Some d | None -> Some value
  in
  let var_handle =
    { name; fallback; default = default_value; layer; meta; runtime }
  in
  (decl, var_handle)

(* Pretty-printer for any_syntax *)
let pp_any_syntax : any_syntax Pp.t = fun ctx (Syntax syn) -> pp_syntax ctx syn

(* Reader for any_syntax *)
let read_any_syntax (r : Cursor.t) : any_syntax =
  (* Reuse the main read_syntax function *)
  read_syntax r

(* CSS Custom Properties sec. 3 requires [var()] fallbacks to round-trip
   including author-written comments. The token stream silently drops comments,
   so slice the original source between the first and last fallback component
   instead of re-serialising tokens. Falls back to component-based serialisation
   when the source is not retained on the cursor. *)
let string_of_fallback inner =
  let cvs = Cursor.consume_remaining inner in
  match (cvs, Cursor.source inner) with
  | [], _ -> ""
  | _, None -> Cursor.string_of_components ~trim:true cvs
  | first :: _, Some src ->
      let start_pos = (Component.source_loc first).start_pos in
      let last = List.nth cvs (List.length cvs - 1) in
      let end_pos = (Component.source_loc last).end_pos in
      let len = max 0 (end_pos - start_pos) in
      let slice =
        String.sub src start_pos (min len (String.length src - start_pos))
      in
      String.trim slice

(* CSS Custom Properties 1 sec. 3: [var( <custom-property-name> ,
   <declaration-value>? )], read from a cursor already positioned at the
   argument list. The string-returning counterpart of [read_var_body], for a
   caller with no value type to read the fallback at. *)
let read_reference_body_as_string (r : Cursor.t) : string * string option =
  Cursor.ws r;
  let raw_name = Cursor.ident ~keep_case:true r in
  (* css-variables-1: a <custom-property-name> is a <dashed-ident> other than
     the bare reserved [--] keyword. A further leading dash is part of the
     name. *)
  if not (Custom_property_name.is_valid raw_name) then
    Cursor.err_invalid r ("not a custom property: " ^ raw_name);
  let name = Custom_property_name.strip_prefix raw_name in
  Cursor.ws r;
  let fallback =
    if Cursor.comma_opt r then Some (string_of_fallback r) else None
  in
  (name, fallback)

(** Parse a CSS variable reference with optional fallback value. This creates a
    variable handle for parsing purposes only - it doesn't have type or layer
    information which would need to be resolved from a variable registry or
    context. *)
let read_reference (r : Cursor.t) : string * string option =
  (* CSS Syntax 3 (ED) sec. 4.3.6: EOF inside a function is a parse error,
     tolerated only when the fallback list opened with a comma (a sec. 4.3.5
     [<string-token>] may have eaten the closing [)]) so a recoverable name +
     fallback survives. Without a fallback there is no recovery signal, so it is
     rejected. *)
  let terminated =
    match Cursor.peek r with
    | Some (Component.Func fn) -> fn.node.terminated
    | _ -> true
  in
  let result = Cursor.call "var" r read_reference_body_as_string in
  (match (terminated, snd result) with
  | false, None -> Cursor.err_invalid r "unterminated var()"
  | _ -> ());
  Cursor.ws r;
  if not (Cursor.is_done r) then
    Cursor.err_invalid r "trailing tokens after var()";
  result

let read_reference_body = read_var_body
