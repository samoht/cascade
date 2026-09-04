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

(* CSS Syntax 3 (ED) sec. 4.3.7 lets an escape carry a [;] or a [}] into a
   custom property's name, so the name is checked against the spelling it
   serializes to (CSS Syntax 3 (ED) sec. 9) rather than against its own bytes:
   read raw, such a name ends the declaration early and never looks like the
   single ident it is. *)
let property_name name =
  let reader = Cursor.of_string (Parser.escape_ident name) in
  let parsed =
    try Cursor.ident ~keep_case:true reader
    with Cursor.Parse_error _ ->
      failwith ("invalid supports declaration property name: " ^ name)
  in
  if not (Cursor.is_done reader) then
    failwith ("invalid supports declaration property name: " ^ name);
  let name =
    if Custom_property_name.has_prefix parsed then parsed
    else String.lowercase_ascii parsed
  in
  Property_name name

let string_of_property_name (Property_name name) = name

let declaration_feature prop value =
  match (String.lowercase_ascii prop, String.lowercase_ascii value) with
  | _, "" -> Empty (property_name prop)
  | "-vendor-flag", "enabled" -> Vendor_flag_enabled
  | _ -> (
      (* The name and the value stay apart: read as one text, a name carrying a
         [;] or a [}] (CSS Syntax 3 (ED) sec. 4.3.7 puts either there through an
         escape) would end its own declaration.

         The value is read opaquely. CSS Conditional 3 sec. 6.1 answers a
         declaration feature by running that exact declaration through the
         browser's own parser, so the spelling is the question and a typed
         reading would hand back a different one: [rgba(0,0,0,.5)] and
         [#ff0000ff] name grammars a browser can accept one of and refuse the
         other. *)
      let name = property_name prop in
      match Declaration.parse_opaque_declaration prop value with
      | Some decl -> Declaration decl
      | None -> Unsupported (name, String.trim value))

(* [property] takes authored CSS text and writes it between the feature's own
   parentheses, so the value has to be a [<declaration-value>] (CSS Syntax 3
   (ED) sec. 7.2): an unmatched closing bracket closes those parentheses and the
   tail becomes a second branch of the condition. The reader below hands
   [declaration_feature] a value rendered from balanced components, so it needs
   no check. Rendering through the component stream closes a [<url-token>] the
   caller left open, the way the reader's own path already does. *)
let property prop value =
  if not (String.equal value "" || Declaration.is_declaration_value value) then
    failwith
      (String.concat ""
         [
           "supports property: ";
           prop;
           ": ";
           value;
           " is not a declaration value";
         ]);
  let value =
    if String.equal value "" then value
    else
      Cursor.string_of_components ~trim:true
        (Cursor.remaining (Cursor.of_string value))
  in
  Property (declaration_feature prop value)

let single_ident name args =
  let cursor = Cursor.of_string args in
  let ident = Cursor.ident ~keep_case:false cursor in
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

let escaped_property_name name =
  Parser.escape_ident (string_of_property_name name)

let pp_declaration_feature ctx = function
  | Declaration decl -> Declaration.pp_opaque ctx decl
  | Empty name ->
      Pp.string ctx (escaped_property_name name);
      Pp.char ctx ':'
  | Unsupported (name, value) ->
      Pp.string ctx (escaped_property_name name);
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
  | At_rule rule ->
      let pp_at_rule ctx name =
        Pp.char ctx '@';
        Pp.string ctx name
      in
      Pp.call "at-rule" pp_at_rule ctx rule
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
  (* CSS Conditional 3 sec. 6: a [)and ] sequence is unambiguous so the leading
     space is droppable under minify; the trailing space is required to keep
     [and(] from re-tokenising as a function call. *)
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
let to_string t = Pp.to_string ~minify:false pp t

(* ===== Component parser ===== *)

let strip_components = List.filter (fun cv -> not (Component.is_whitespace cv))

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

(* Anchoring a failure on the components that failed puts the caret on that
   slice of the condition. [t] anchors the smallest enclosing construct, for a
   failure that has no components of its own. *)
let err t cvs reason =
  let at = match cvs with [] -> t | _ :: _ -> Cursor.sub t cvs in
  Cursor.err_condition at ~at_rule:"@supports" reason

let declaration_of_components t prop value =
  if contains_top_level_semicolon value then
    err t value "Invalid declaration in @supports";
  match property_ident (strip_components prop) with
  | Some name -> (
      let text = Cursor.string_of_components ~trim:true value in
      (* [declaration_feature] is the shared constructor, so it reports through
         [Failure]; re-raise it against the declaration's own components. *)
      match declaration_feature name text with
      | feature -> Property feature
      | exception Failure msg -> err t (prop @ value) msg)
  | None -> err t prop "Invalid declaration in @supports"

let function_call t (fn : Component.func Component.node) =
  let name = fn.node.name in
  let args = fn.node.arguments in
  if not (Component.is_any_value [ Component.Func fn ]) then
    err t [ Component.Func fn ] "Invalid general-enclosed function in @supports";
  (* The feature grammar has priority over the general-enclosed fallback. *)
  match func name (Cursor.string_of_components ~trim:true args) with
  | feature -> feature
  | exception (Failure _ | Cursor.Parse_error _) ->
      Function
        (General
           ( String.lowercase_ascii name,
             Cursor.string_of_components ~trim:true args ))

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
          err t [] "Cannot mix and/or without parentheses in @supports"
      | _ -> ());
      Cursor.skip t;
      let right = in_parens ~allow_unwrapped_decl:false t in
      chain t (Some `And) (And (acc, right))
  | Some "or" ->
      (match op with
      | Some `And ->
          err t [] "Cannot mix and/or without parentheses in @supports"
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
        let decl = declaration_of_components t prop value in
        ignore (Cursor.consume_remaining_as_string t : string);
        decl
    | None -> err t [] "Expected supports feature"
  in
  Cursor.ws t;
  match Cursor.peek t with
  | Some
      (Component.Block { node = { opening = Token.Paren; value; _ }; _ } as cv)
    ->
      if not (closed_block cv) then
        err t [ cv ] "Unmatched parenthesis in @supports condition";
      Cursor.skip t;
      paren_components (Cursor.sub t [ cv ]) value
  | Some (Component.Func fn) ->
      let group = Cursor.sub t [ Component.Func fn ] in
      Cursor.skip t;
      function_call group fn
  | _ when allow_unwrapped_decl -> unwrapped_declaration t
  | _ -> err t [] "Expected supports feature"

and paren_components t value =
  if strip_components value = [] then
    err t value "Empty parentheses in @supports";
  if not (components_are_closed value) then
    err t value "Unmatched parenthesis in @supports condition";
  match split_top_level_colon value with
  | Some (prop, value) -> declaration_of_components t prop value
  | None ->
      let inner = Cursor.sub t value in
      let condition = condition inner in
      Cursor.ws inner;
      if not (Cursor.is_done inner) then
        err inner [] "trailing content in @supports group";
      condition

let read ?(allow_unwrapped_decl = false) t =
  let cond =
    if not allow_unwrapped_decl then condition t
    else
      (* A bare declaration is only reachable once the wrapped grammar has been
         ruled out, so the cursor is rewound and read again. *)
      let snapshot = Cursor.save t in
      try condition t
      with Cursor.Parse_error _ ->
        Cursor.restore t snapshot;
        in_parens ~allow_unwrapped_decl t
  in
  Cursor.ws t;
  if not (Cursor.is_done t) then
    Cursor.err_condition t ~at_rule:"@supports" "trailing content";
  cond

let of_string ?allow_unwrapped_decl s =
  read ?allow_unwrapped_decl (Cursor.of_string s)

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
  String.compare
    (Pp.to_string ~minify:false pp_function_feature a)
    (Pp.to_string ~minify:false pp_function_feature b)

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

(* ===== Entailment against the enclosing conditions ===== *)

(* CSS Conditional 3 sec. 6 gives every term of a [<supports-condition>] a
   two-valued result - of [<general-enclosed>] it says "The result is false",
   not unknown - so a condition is a propositional formula over its feature
   tests and classical entailment holds over it. A user agent answers one
   feature test the same way everywhere in a sheet, so the conditions enclosing
   a nested [@supports] are facts about whichever user agent reaches it. Nothing
   below reads a support table.

   Atom identity is syntactic: two feature tests are one variable exactly when
   {!compare} calls them equal. Two spellings a user agent in fact answers alike
   stay two variables, which enumerates worlds that do not exist; entailment
   over that superset holds over the real world set too, so the approximation
   only ever concludes less. *)

(* A containment chain carries few distinct feature tests, so the truth table is
   decided outright rather than searched. Past the cap the condition is left as
   the author wrote it. *)
let max_atoms = 16

let rec collect_atoms acc = function
  | (Property _ | Function _) as atom ->
      if List.exists (fun a -> equal a atom) acc then acc
      else if List.length acc >= max_atoms then raise Exit
      else atom :: acc
  | Not a -> collect_atoms acc a
  | And (a, b) | Or (a, b) -> collect_atoms (collect_atoms acc a) b

(* A truth vector holds one bit per assignment: bit [j] is the formula's value
   under the assignment that reads bit [i] of [j] as the value of atom [i]. So
   [and], [or] and [not] are bitwise, and one pass over [2^n / 8] bytes settles
   a whole truth table. *)
let vector_len n = if n <= 3 then 1 else 1 lsl (n - 3)

let all_true n =
  if n >= 3 then Bytes.make (vector_len n) '\xff'
  else
    (* Under three atoms the whole table fits in one byte's low bits. *)
    Bytes.make 1 (Char.unsafe_chr [| 0b1; 0b11; 0b1111 |].(n))

let map2 f a b =
  Bytes.init (Bytes.length a) (fun i ->
      Char.unsafe_chr
        (f (Char.code (Bytes.get a i)) (Char.code (Bytes.get b i)) land 0xff))

let v_and = map2 ( land )
let v_or = map2 ( lor )
let v_not ~all v = map2 ( lxor ) all v

(* Atom [i] is true under assignment [j] when bit [i] of [j] is set. Below three
   atoms that bit varies inside a byte, giving a fixed pattern; above it, whole
   bytes alternate. *)
let atom_vector n i =
  let len = vector_len n in
  if i < 3 then
    v_and (all_true n)
      (Bytes.make len (Char.unsafe_chr [| 0xaa; 0xcc; 0xf0 |].(i)))
  else
    Bytes.init len (fun b ->
        if (b lsr (i - 3)) land 1 = 1 then '\xff' else '\000')

let for_all_bytes f a b =
  let rec loop i =
    i >= Bytes.length a
    || f (Char.code (Bytes.get a i)) (Char.code (Bytes.get b i))
       && loop (i + 1)
  in
  loop 0

let implies a b = for_all_bytes (fun x y -> x land lnot y land 0xff = 0) a b
let disjoint a b = for_all_bytes (fun x y -> x land y = 0) a b
let never v = Bytes.for_all (fun c -> c = '\000') v

let rec eval ~atom ~all = function
  | (Property _ | Function _) as leaf -> atom leaf
  | Not a -> v_not ~all (eval ~atom ~all a)
  | And (a, b) -> v_and (eval ~atom ~all a) (eval ~atom ~all b)
  | Or (a, b) -> v_or (eval ~atom ~all a) (eval ~atom ~all b)

(* Rewriting stays conservative where deciding is complete: a subformula the
   context settles is replaced by its value and absorbed, and the author's shape
   survives everywhere else. A minimised sum of products is usually longer as
   CSS text and is not what the author wrote. *)
let rec reduce ~atom ~world ~all cond =
  let decide v residual =
    if implies world v then (v, `True)
    else if disjoint world v then (v, `False)
    else (v, residual)
  in
  match cond with
  | Property _ | Function _ -> decide (atom cond) (`Cond cond)
  | Not a ->
      let va, ra = reduce ~atom ~world ~all a in
      let residual =
        match ra with
        | `True -> `False
        | `False -> `True
        | `Cond a' -> `Cond (if a' == a then cond else Not a')
      in
      decide (v_not ~all va) residual
  | And (a, b) ->
      let va, ra = reduce ~atom ~world ~all a in
      let vb, rb = reduce ~atom ~world ~all b in
      let residual =
        match (ra, rb) with
        | `False, _ | _, `False -> `False
        | `True, r | r, `True -> r
        | `Cond a', `Cond b' ->
            `Cond (if a' == a && b' == b then cond else And (a', b'))
      in
      decide (v_and va vb) residual
  | Or (a, b) ->
      let va, ra = reduce ~atom ~world ~all a in
      let vb, rb = reduce ~atom ~world ~all b in
      let residual =
        match (ra, rb) with
        | `True, _ | _, `True -> `True
        | `False, r | r, `False -> r
        | `Cond a', `Cond b' ->
            `Cond (if a' == a && b' == b then cond else Or (a', b'))
      in
      decide (v_or va vb) residual

let simplify_under ~context cond =
  match List.fold_left collect_atoms [] (cond :: context) with
  | exception Exit -> `Cond cond
  | atoms -> (
      let atoms = Array.of_list atoms in
      let n = Array.length atoms in
      let all = all_true n in
      let vectors = Array.init n (atom_vector n) in
      let atom leaf =
        let rec find i =
          if equal atoms.(i) leaf then vectors.(i) else find (i + 1)
        in
        find 0
      in
      let world =
        List.fold_left (fun w c -> v_and w (eval ~atom ~all c)) all context
      in
      if never world then `False
      else
        let size c = Pp.size ~minify:true pp c in
        match snd (reduce ~atom ~world ~all cond) with
        | `Cond c when (not (c == cond)) && size c >= size cond -> `Cond cond
        | decision -> decision)
