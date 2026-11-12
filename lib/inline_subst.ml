open Stylesheet
open Inline_scope

(** {1 Pass 2 - substitute var() in every declaration} *)

(* The customs a consumer can see, with an index from name to the declarations
   that answer to it. Both lookups below used to scan the whole list, and the
   walk asks once per declaration, so the pass cost a square in the declaration
   count. The index is built once per consumer scope instead. *)
type visible = {
  decls : Declaration.declaration list;
  by_name : (string, (int * Declaration.declaration) list) Hashtbl.t;
  declared : (string, unit) Hashtbl.t;
}

let visible_of_decls ~declared decls =
  let by_name = Hashtbl.create 64 in
  List.iteri
    (fun idx decl ->
      match decl with
      | Declaration.Declaration { property = Properties.Custom_property n; _ }
        ->
          let prev = Option.value ~default:[] (Hashtbl.find_opt by_name n) in
          Hashtbl.replace by_name n ((idx, decl) :: prev)
      | _ -> ())
    decls;
  (* Consed while walking, so put each bucket back in source order once rather
     than appending to its end per entry. *)
  Hashtbl.iter
    (fun name entries -> Hashtbl.replace by_name name (List.rev entries))
    (Hashtbl.copy by_name);
  { decls; by_name; declared }

(* A name reaches its declarations under the spelling it was written with, so
   ask for both: [--x] is stored as written and [x] is the bare form callers
   hand in. *)
let named visible name =
  let dashed = Custom_property_name.add_prefix name in
  match Hashtbl.find_opt visible.by_name name with
  | Some entries when name = dashed -> entries
  | Some entries -> (
      match Hashtbl.find_opt visible.by_name dashed with
      | None -> entries
      | Some more ->
          List.merge (fun (a, _) (b, _) -> Int.compare a b) entries more)
  | None -> Option.value ~default:[] (Hashtbl.find_opt visible.by_name dashed)

let visible_customs ~(scopes : scope_set) ~at_path ~selector =
  visible_of_decls ~declared:scopes.declared
    (List.concat_map
       (fun s ->
         if
           at_path_prefix ~outer:s.at_path ~inner:at_path
           && selector_covers ~ancestor:s.selector ~consumer:selector
         then s.customs
         else [])
       scopes.all)

(* [kept] names carry the [--] prefix; [Context.runtime_vars] expects the bare
   custom-property name. *)
let runtime_var_names kept = List.map Custom_property_name.strip_prefix kept

let context_for ?(kept = []) visible =
  Context.v ~custom_properties:(List.rev visible.decls)
    ~runtime_vars:(runtime_var_names kept) ()

let read_custom_components ?(unicode_ranges = false) read = function
  | Declaration.Declaration
      {
        property = Properties.Custom_property _;
        value = Properties.Custom_value { value; _ };
        _;
      } -> (
      try
        let components = Properties.components_of_custom_property_value value in
        let cursor =
          (* CSS Syntax 3 (ED) sec. 5.5.11: a custom property holds the tokens
             it was written with, lexed without unicode ranges. A descriptor
             that asks for them reads the substituted text afresh. *)
          if unicode_ranges then
            Cursor.of_string ~unicode_ranges:true
              (Parser.to_string_minified components)
          else Cursor.of_components components
        in
        let parsed = read cursor in
        Cursor.ws cursor;
        Cursor.expect_eof cursor;
        Some parsed
      with Cursor.Parse_error _ -> None)
  | _ -> None

let lookup_visible_custom ?unicode_ranges visible name read =
  List.find_map
    (fun (_, decl) -> read_custom_components ?unicode_ranges read decl)
    (named visible name)

(* CSS Variables 1 sec. 2.1 gives a CSS-wide keyword written to a custom
   property "its usual meaning", so the binding is what the cascade makes of the
   keyword and not the keyword's own tokens: with nothing above to inherit from,
   [--x: unset] is the guaranteed-invalid initial value and a reference takes
   its fallback. What it is depends on the element the sheet is read against, so
   the binding is one this pass does not fold. *)
let is_css_wide_keyword_value components =
  match List.filter (fun c -> not (Component.is_whitespace c)) components with
  | [ Component.Preserved { kind = Token.Ident name; _ } ] -> (
      match String.lowercase_ascii name with
      | "initial" | "inherit" | "unset" | "revert" | "revert-layer" -> true
      | _ -> false)
  | _ -> false

let custom_value_components = function
  | Declaration.Declaration
      {
        property = Properties.Custom_property _;
        value = Properties.Custom_value { value; _ };
        _;
      } ->
      Some (Properties.components_of_custom_property_value value)
  | _ -> None

let better_custom_candidate ~important ~idx
    (best : (bool * int * Component.t list) option) =
  match best with
  | None -> true
  | Some (best_important, best_idx, _) ->
      (important && not best_important)
      || (important = best_important && idx > best_idx)

let consider_custom_candidate idx best decl value =
  let important = Declaration.is_important decl in
  let candidate = (important, idx, value) in
  if better_custom_candidate ~important ~idx best then Some candidate else best

(* The candidate that wins the cascade is the one that answers, so a keyword
   winning over a token stream stops the fold rather than handing the fold the
   runner-up. *)
let lookup_visible_custom_components visible name =
  List.fold_left
    (fun best (idx, decl) ->
      match custom_value_components decl with
      | None -> best
      | Some value -> consider_custom_candidate idx best decl value)
    None (named visible name)
  |> fun best ->
  Option.bind best (fun (_, _, value) ->
      if is_css_wide_keyword_value value then None else Some value)

let trim_components components =
  let is_ws = function
    | Component.Preserved { kind = Token.Whitespace _; _ } -> true
    | _ -> false
  in
  let rec drop = function hd :: tl when is_ws hd -> drop tl | xs -> xs in
  List.rev (drop (List.rev (drop components)))

type subst_result = Components of Component.t list | Cycle

let rec components_contain preserved components =
  List.exists
    (function
      | Component.Preserved token -> preserved token
      | Component.Func { node = { arguments; _ }; _ } ->
          components_contain preserved arguments
      | Component.Block { node = { value; _ }; _ } ->
          components_contain preserved value)
    components

let parse_var_components args : (string * Component.t list option) option =
  try
    let cursor = Cursor.of_components args in
    let raw_name = Cursor.ident ~keep_case:true cursor in
    if not (Custom_property_name.is_valid raw_name) then None
    else
      let name = Custom_property_name.strip_prefix raw_name in
      let fallback =
        Cursor.ws cursor;
        if Cursor.comma_opt cursor then Some (Cursor.remaining cursor) else None
      in
      Some (name, Option.map trim_components fallback)
  with Cursor.Parse_error _ -> None

let rec substitute_components ~kept visible ~visited components =
  let one = function
    | Component.Func ({ node = { name; arguments; _ }; _ } as fn)
      when String.lowercase_ascii name = "var" -> (
        match parse_var_components arguments with
        | None -> Components [ Component.Func fn ]
        | Some (name, fallback) ->
            substitute_var ~kept visible ~visited fn name fallback)
    | Component.Func fn -> (
        match
          substitute_components ~kept visible ~visited fn.node.arguments
        with
        | Components arguments ->
            Components
              [ Component.Func { fn with node = { fn.node with arguments } } ]
        | Cycle -> Cycle)
    | Component.Block block -> (
        match substitute_components ~kept visible ~visited block.node.value with
        | Components value ->
            Components
              [
                Component.Block { block with node = { block.node with value } };
              ]
        | Cycle -> Cycle)
    | Component.Preserved _ as cv -> Components [ cv ]
  in
  let rec loop acc = function
    | [] -> Components (List.rev acc)
    | cv :: rest -> (
        match one cv with
        | Components cvs -> loop (List.rev_append cvs acc) rest
        | Cycle -> Cycle)
  in
  loop [] components

and substitute_var ~kept visible ~visited original name fallback =
  (* An unresolved var() with no fallback is kept verbatim at the top level, but
     inside another custom property's value it is guaranteed-invalid and
     propagates failure to the nearest enclosing fallback. *)
  let keep_or_fail () =
    if visited = [] then Components [ Component.Func original ] else Cycle
  in
  let fallback_or_original () =
    match fallback with
    | None -> keep_or_fail ()
    | Some components -> substitute_components ~kept visible ~visited components
  in
  let resolved_or_fallback value =
    match
      substitute_components ~kept visible ~visited:(name :: visited) value
    with
    | Components _ as resolved -> resolved
    | Cycle -> (
        match fallback with None -> Cycle | Some _ -> fallback_or_original ())
  in
  (* A var() forming a cycle is invalid at computed-value time; its own fallback
     does not rescue it, so propagate failure to a consumer fallback. *)
  if List.mem name visited then Cycle
  else
    match lookup_visible_custom_components visible name with
    | None ->
        (* CSS Variables 1 sec. 3 puts the fallback in only where the custom
           property holds its guaranteed-invalid initial value. A definition
           this consumer cannot see is not an absent one: the element it styles
           may sit under the element the definition is written for, so the
           reference stays live. *)
        let dashed = Custom_property_name.add_prefix name in
        if List.mem dashed kept || Hashtbl.mem visible.declared dashed then
          keep_wrapper ~kept visible ~visited original fallback
        else fallback_or_original ()
    | Some value -> resolved_or_fallback value

(* A kept var stays a live [var(--name, ...)], but its fallback may hold
   resolvable vars ([var(--tw-ease, var(--default-ease))] -> [var(--tw-ease,
   ease)]), so substitute inside the fallback and rebuild the wrapper. *)
and keep_wrapper ~kept visible ~visited original fallback =
  match fallback with
  | None -> Components [ Component.Func original ]
  | Some components -> (
      match substitute_components ~kept visible ~visited components with
      | Cycle -> Components [ Component.Func original ]
      | Components subst ->
          let rec name_prefix acc = function
            | (Component.Preserved { kind = Token.Comma; _ } as c) :: _ ->
                List.rev (c :: acc)
            | x :: rest -> name_prefix (x :: acc) rest
            | [] -> List.rev acc
          in
          let prefix = name_prefix [] original.Component.node.arguments in
          Components
            [
              Component.Func
                {
                  original with
                  node =
                    { original.Component.node with arguments = prefix @ subst };
                };
            ])

(* The property is carried through as the tag [decl] already holds, never as its
   rendered name: [Declaration.property_name] minifies, and [page-break-*]
   minifies to the CSS Fragmentation 3 sec. 3.4 [break-*] alias of a different
   property. *)
let declaration_with_components decl components : Declaration.declaration option
    =
  let value =
    Parser.to_string_custom_minified ~fold_ident:Values.fold_custom_value_ident
      components
  in
  if String.trim value = "" then None
  else
    let has_string =
      components_contain (function
        | { kind = Token.String _; _ } -> true
        | _ -> false)
    in
    (* [font-family] reaches here typed or, when its value never parsed as one,
       as the unknown property of that name; both are the same property. *)
    let is_font_family =
      match Declaration.property_key decl with
      | Declaration.Key Properties.Font_family -> true
      | Declaration.Key (Properties.Unknown_property name) ->
          String.equal name "font-family"
      | _ -> false
    in
    let opaque () = Some (Declaration.with_opaque_value decl value) in
    match Declaration.with_value decl value with
    | decl -> Some decl
    | exception Cursor.Parse_error _ ->
        (* [font-family] takes a list of names this reader does not model as
           one, so the substituted text is written back as it stands. Anywhere
           else a refused substitution is CSS Variables 1 sec. 3's invalid at
           computed-value time, which is the property's inherited or initial
           value: writing the refused text back hands the slot to the
           declaration before it instead, and dropping it does the same. *)
        if is_font_family && has_string components then opaque () else None

let should_use_typed_default ~kept visible vars =
  vars <> []
  && List.for_all
       (fun (Variables.V var) ->
         let dashed = Custom_property_name.add_prefix var.Values.name in
         Option.is_some var.Values.default
         && Option.is_none
              (lookup_visible_custom_components visible var.Values.name)
         (* A kept var must keep its live [var()] reference, so do not collapse
            it to its typed default, and neither may a name the sheet defines
            out of this consumer's sight: the fallback stands in only for a
            custom property that holds its guaranteed-invalid initial value. *)
         && (not (List.mem dashed kept))
         && not (Hashtbl.mem visible.declared dashed))
       vars

(* [Context.eval] leaves a kept var's [var()] intact (it is in [runtime_vars])
   while still applying value-independent simplifications like calc identities,
   so the substituted declaration is always evaluated, kept var or not. *)
let apply_substituted_components ctx decl ~original_components components =
  if List.equal Component.equal components original_components then
    Some (Context.eval ctx decl)
  else
    match declaration_with_components decl components with
    | None ->
        (* The substitution is no value for this property, so the declaration is
           invalid at computed-value time and neither the refused text nor the
           declaration's absence says that. The reference stays, and the browser
           answers it. *)
        Some decl
    | Some decl -> Some (Context.eval ctx decl)

(* A name the sheet defines out of this consumer's sight is live here for the
   same reason a kept one is: the evaluator resolves a name it does not hold to
   the reference's fallback, and CSS Variables 1 sec. 3 puts the fallback in
   only where the custom property holds its guaranteed-invalid initial value.
   Only the names this declaration references have to be named, so the list
   stays as short as the declaration is. *)
let rec var_names_in acc components =
  List.fold_left
    (fun acc (c : Component.t) ->
      match c with
      | Component.Func { node = { name; arguments; _ }; _ } ->
          let acc = var_names_in acc arguments in
          if String.equal (String.lowercase_ascii name) "var" then
            match
              List.filter (fun c -> not (Component.is_whitespace c)) arguments
            with
            | Component.Preserved { kind = Token.Ident n; _ } :: _ -> n :: acc
            | _ -> acc
          else acc
      | Component.Block { node = { value; _ }; _ } -> var_names_in acc value
      | Component.Preserved _ -> acc)
    acc components

let out_of_sight visible components =
  var_names_in [] components
  |> List.filter_map (fun name ->
      let dashed = Custom_property_name.add_prefix name in
      let bare = Custom_property_name.strip_prefix name in
      if
        Hashtbl.mem visible.declared dashed
        && Option.is_none (lookup_visible_custom_components visible bare)
      then Some dashed
      else None)
  |> List.sort_uniq String.compare

let substitute_non_custom ~kept visible ctx decl =
  let vars = Variables.vars_of_declarations [ decl ] in
  let value = Declaration.string_of_value ~minify:false decl in
  let original_components = Cursor.remaining (Cursor.of_string value) in
  let ctx =
    match out_of_sight visible original_components with
    | [] -> ctx
    | live -> context_for ~kept:(kept @ live) visible
  in
  if should_use_typed_default ~kept visible vars then
    Some (Context.eval ctx decl)
  else
    match
      substitute_components ~kept visible ~visited:[] original_components
    with
    | Cycle -> Some (Context.eval ctx decl)
    | Components components ->
        apply_substituted_components ctx decl ~original_components components

(* A custom property's own value: fold the inlinable (non-kept) variables it
   references in place so they can be deleted, while a [kept] variable it
   references stays a live [var()]. *)
let fold_custom_value ~kept visible decl =
  match custom_value_components decl with
  | None -> Some decl
  | Some original -> (
      match substitute_components ~kept visible ~visited:[] original with
      | Cycle -> Some decl
      | Components components -> (
          if List.equal Component.equal components original then Some decl
          else
            match declaration_with_components decl components with
            | Some decl' -> Some decl'
            | None -> Some decl))

let substitute_declaration ~kept visible ctx decl =
  match custom_name decl with
  | Some _ -> fold_custom_value ~kept visible decl
  | None -> substitute_non_custom ~kept visible ctx decl

(** {1 Fallbacks for references whose definition lives elsewhere} *)

(* [var(--name, answer)] read back through the tokeniser, which is what decides
   that [answer] is a [<declaration-value>]: anything that does not come back as
   one [var()] carrying a fallback is no value for it. *)
let var_with_fallback name answer =
  let text =
    String.concat ""
      [ "var("; Custom_property_name.add_prefix name; ","; answer; ")" ]
  in
  match Cursor.remaining (Cursor.of_string text) with
  | [ (Component.Func { node = { name = fn; arguments; _ }; _ } as var) ]
    when String.lowercase_ascii fn = "var" -> (
      match parse_var_components arguments with
      | Some (_, Some _) -> Some var
      | Some (_, Option.None) | Option.None -> Option.None)
  | _ | (exception Cursor.Parse_error _) -> Option.None

let rec fallback_components ~lookup components =
  List.map
    (function
      | Component.Func ({ node = { name; arguments; _ }; _ } as fn)
        when String.lowercase_ascii name = "var" -> (
          match parse_var_components arguments with
          | Some (var_name, Option.None) -> (
              match lookup (Custom_property_name.add_prefix var_name) with
              | Option.None -> Component.Func fn
              | Some answer ->
                  Option.value ~default:(Component.Func fn)
                    (var_with_fallback var_name answer))
          | Some (_, Some _) | Option.None ->
              Component.Func
                {
                  fn with
                  node =
                    {
                      fn.node with
                      arguments = fallback_components ~lookup arguments;
                    };
                })
      | Component.Func fn ->
          Component.Func
            {
              fn with
              node =
                {
                  fn.node with
                  arguments = fallback_components ~lookup fn.node.arguments;
                };
            }
      | Component.Block block ->
          Component.Block
            {
              block with
              node =
                {
                  block.node with
                  value = fallback_components ~lookup block.node.value;
                };
            }
      | Component.Preserved _ as cv -> cv)
    components

let declaration_with_fallbacks ~lookup decl =
  let original =
    match custom_value_components decl with
    | Some components -> components
    | Option.None ->
        Cursor.remaining
          (Cursor.of_string (Declaration.string_of_value ~minify:false decl))
  in
  let components = fallback_components ~lookup original in
  if List.equal Component.equal components original then decl
  else Option.value ~default:decl (declaration_with_components decl components)

let var_fallbacks lookup stylesheet =
  Stylesheet.map_declarations
    (List.map (declaration_with_fallbacks ~lookup))
    stylesheet

let font_src_var_fallback ~simplify ~visited (var : Font_face.src Values.var) =
  match var.Values.fallback with
  | Values.Fallback value -> simplify ~visited value
  | _ -> [ Font_face.Var var ]

let simplify_font_src_descriptor visible entries =
  let normalize_entry = function
    | Font_face.Quoted_url { url; format; tech; _ } ->
        Font_face.Url { url; format; tech }
    | entry -> entry
  in
  let rec simplify ~visited entries =
    List.concat_map (simplify_entry ~visited) entries
  and simplify_entry ~visited = function
    | Font_face.Var var when not (List.mem var.Values.name visited) -> (
        match
          lookup_visible_custom visible var.Values.name Font_face.read_src
        with
        | Some value -> simplify ~visited:(var.Values.name :: visited) value
        | None -> font_src_var_fallback ~simplify ~visited var)
    | entry -> [ normalize_entry entry ]
  in
  simplify ~visited:[] entries

(* Every descriptor whose value type carries a [Var] arm resolves the same way:
   follow the reference to the custom it names, then the references that value
   holds in turn, and stop on a name already seen so a cycle terminates. What
   differs per descriptor is only how to read the value, how to see a [Var], and
   what a value that is no [Var] still holds, so take those and share the walk.
   [leaf] receives the walk so a value nesting its own type carries the same
   [visited] set inwards; without that a reference reachable both directly and
   through the nesting would not terminate. *)
let simplify_nested_var ?unicode_ranges visible ~read
    ~(as_var : 'a -> 'a Values.var option) ~(of_var : 'a Values.var -> 'a)
    ~(leaf :
       (visited:string list -> 'a -> 'a) -> visited:string list -> 'a -> 'a)
    (value : 'a) : 'a =
  let rec simplify ~visited (value : 'a) : 'a =
    match as_var value with
    | Some var when not (List.mem var.Values.name visited) -> (
        match
          lookup_visible_custom ?unicode_ranges visible var.Values.name read
        with
        | Some found -> simplify ~visited:(var.Values.name :: visited) found
        | None -> (
            (* Nothing defines the name: CSS Variables 1 sec. 3 falls back to
               the second argument, and with none the reference stands. *)
            match (var.Values.fallback : 'a Values.fallback) with
            | Values.Fallback fallback -> simplify ~visited fallback
            | Values.Empty | Values.Empty2 | Values.None
            | Values.Syntax_fallback _ | Values.Var_fallback _ ->
                of_var var))
    | Some _ | None -> leaf simplify ~visited value
  in
  simplify ~visited:[] value

(* The common case: the value is flat, so a non-[Var] holds no reference. *)
let simplify_typed_var ?unicode_ranges visible ~read ~as_var ~of_var value =
  simplify_nested_var ?unicode_ranges visible ~read ~as_var ~of_var
    ~leaf:(fun _simplify ~visited:_ value -> value)
    value

let simplify_unicode_range_descriptor visible =
  simplify_typed_var ~unicode_ranges:true visible
    ~read:Properties.read_unicode_range
    ~as_var:(function Properties.Var v -> Some v | _ -> None)
    ~of_var:(fun v -> (Properties.Var v : Properties.unicode_range))

(* Read a referenced custom value with the property parser so a property-style
   stack remains visible as [List]. [substitute_font_face] validates the result
   against the descriptor's single-name grammar below. Using the narrower reader
   here would conflate an invalid referenced value with a missing custom
   property and retain the unresolved [var()] instead. *)
let simplify_font_family_descriptor visible =
  simplify_nested_var visible ~read:Properties.read_font_family
    ~as_var:(function Properties.Var v -> Some v | _ -> None)
    ~of_var:(fun v -> (Properties.Var v : Properties.font_family))
    ~leaf:(fun simplify ~visited value ->
      match (value : Properties.font_family) with
      | Properties.List items ->
          (Properties.List (List.map (simplify ~visited) items)
            : Properties.font_family)
      | value -> value)

let simplify_font_style_descriptor visible =
  simplify_typed_var visible ~read:Properties.read_font_style
    ~as_var:(function Properties.Var v -> Some v | _ -> None)
    ~of_var:(fun v -> (Properties.Var v : Properties.font_style))

let simplify_font_weight_descriptor visible =
  simplify_typed_var visible ~read:Properties.read_font_weight
    ~as_var:(function Properties.Var v -> Some v | _ -> None)
    ~of_var:(fun v -> (Properties.Var v : Properties.font_weight))

let simplify_font_stretch_descriptor visible =
  simplify_typed_var visible ~read:Properties.read_font_stretch
    ~as_var:(function Properties.Var v -> Some v | _ -> None)
    ~of_var:(fun v -> (Properties.Var v : Properties.font_stretch))

let simplify_font_display_descriptor visible =
  simplify_typed_var visible ~read:Properties.read_font_display
    ~as_var:(function Properties.Var v -> Some v | _ -> None)
    ~of_var:(fun v -> (Properties.Var v : Properties.font_display))

let simplify_font_feature_settings_descriptor visible =
  simplify_typed_var visible ~read:Properties.read_font_feature_settings
    ~as_var:(function Properties.Var v -> Some v | _ -> None)
    ~of_var:(fun v -> (Properties.Var v : Properties.font_feature_settings))

let simplify_font_variation_settings_descriptor visible =
  simplify_typed_var visible ~read:Properties.read_font_variation_settings
    ~as_var:(function Properties.Var v -> Some v | _ -> None)
    ~of_var:(fun v -> (Properties.Var v : Properties.font_variation_settings))

let simplify_font_variant_descriptor visible =
  simplify_typed_var visible ~read:read_font_variant_descriptor
    ~as_var:(function Var v -> Some v | _ -> Option.None)
    ~of_var:(fun v -> (Var v : font_variant_descriptor))

(* One resolver covers [ascent-override], [descent-override] and
   [line-gap-override]: CSS Fonts 4 sec. 4.10 gives the three the same
   grammar. *)
let simplify_metric_override_descriptor visible =
  simplify_typed_var visible ~read:Font_face.read_metric_override
    ~as_var:(function Font_face.Var v -> Some v | _ -> Option.None)
    ~of_var:(fun v -> (Font_face.Var v : Font_face.metric_override))

let simplify_size_adjust_descriptor visible =
  simplify_typed_var visible ~read:Font_face.read_size_adjust
    ~as_var:(function
      | Font_face.Var v -> Some v | Font_face.Pct _ -> Option.None)
    ~of_var:(fun v -> (Font_face.Var v : Font_face.size_adjust))

(* [resolve_font_face_var] names the descriptors whose var() survives the parse
   and asks for one resolver each, so this pass and the parser cannot grow
   apart: a descriptor added to that table takes a resolver argument the call
   below has to supply. *)
let simplify_font_face_descriptor visible descriptor =
  match
    resolve_font_face_var
      ~src:(simplify_font_src_descriptor visible)
      ~unicode_range:(List.map (simplify_unicode_range_descriptor visible))
      ~font_family:(List.map (simplify_font_family_descriptor visible))
      ~font_style:(simplify_font_style_descriptor visible)
      ~font_weight:(simplify_font_weight_descriptor visible)
      ~font_stretch:(simplify_font_stretch_descriptor visible)
      ~font_display:(simplify_font_display_descriptor visible)
      ~font_variant:(simplify_font_variant_descriptor visible)
      ~font_feature_settings:(simplify_font_feature_settings_descriptor visible)
      ~font_variation_settings:
        (simplify_font_variation_settings_descriptor visible)
      ~metric_override:(simplify_metric_override_descriptor visible)
      ~size_adjust:(simplify_size_adjust_descriptor visible)
      descriptor
  with
  | Some resolved -> resolved
  | Option.None -> descriptor

let map_keyframe_decls f frames =
  List.map
    (fun frame -> { frame with declarations = List.map f frame.declarations })
    frames

let universal_selector = Selector.Universal None
let selector_for_parents = function [] -> universal_selector | p :: _ -> p

let universal_visible_customs ~scopes ~at_path =
  visible_customs ~scopes ~at_path ~selector:universal_selector

let eval_decls ~kept ~scopes ~at_path selector decls =
  let visible = visible_customs ~scopes ~at_path ~selector in
  let ctx = context_for ~kept visible in
  List.filter_map (substitute_declaration ~kept visible ctx) decls

(* @page, @keyframes, and @position-try declarations apply to elements whose
   effective selector is universal at this at-path. Customs declared on [:root],
   [html], or [*] cover any element and remain reachable. *)
let eval_universal_decls ~scopes ~at_path decls =
  let visible = universal_visible_customs ~scopes ~at_path in
  List.map (Context.eval (context_for visible)) decls

let universal_eval ~scopes ~at_path =
  let visible = universal_visible_customs ~scopes ~at_path in
  Context.eval (context_for visible)

let substitute_font_face ~scopes ~at_path descriptors =
  let visible = universal_visible_customs ~scopes ~at_path in
  List.filter_map
    (fun descriptor ->
      match simplify_font_face_descriptor visible descriptor with
      | Font_family [ family ] as descriptor
        when Properties.is_font_family_name_value family ->
          Some descriptor
      | Font_family _ -> None
      | descriptor -> Some descriptor)
    descriptors

let substitute_page_with_margins ~scopes ~at_path sel descriptors margins =
  let visible = universal_visible_customs ~scopes ~at_path in
  let eval_page = Context.eval (context_for visible) in
  let update_margin (m : page_margin_rule) =
    { m with descriptors = List.map eval_page m.descriptors }
  in
  Page_with_margins
    (sel, List.map eval_page descriptors, List.map update_margin margins)

let rec substitute ~kept ~scopes ~parents ~at_path stmts =
  List.map (substitute_stmt ~kept ~scopes ~parents ~at_path) stmts

and substitute_stmt ~kept ~scopes ~parents ~at_path stmt =
  match at_wrapper stmt with
  | Some (node, body, rebuild) ->
      rebuild
        (substitute ~kept ~scopes ~parents ~at_path:(node :: at_path) body)
  | None -> (
      match stmt with
      | Rule rule ->
          let eff = effective_selector ~parents rule.selector in
          Rule
            {
              rule with
              declarations =
                eval_decls ~kept ~scopes ~at_path eff rule.declarations;
              nested =
                substitute ~kept ~scopes ~parents:(eff :: parents) ~at_path
                  rule.nested;
            }
      | Declarations decls ->
          Declarations
            (eval_decls ~kept ~scopes ~at_path
               (selector_for_parents parents)
               decls)
      | Page (sel, decls) ->
          let visible = universal_visible_customs ~scopes ~at_path in
          let ctx = context_for visible in
          Page (sel, List.map (Context.eval ctx) decls)
      | Position_try (n, decls) ->
          Position_try (n, eval_universal_decls ~scopes ~at_path decls)
      | Keyframes (n, frames) ->
          Keyframes
            (n, map_keyframe_decls (universal_eval ~scopes ~at_path) frames)
      | Webkit_keyframes (n, frames) ->
          Webkit_keyframes
            (n, map_keyframe_decls (universal_eval ~scopes ~at_path) frames)
      | Moz_keyframes (n, frames) ->
          Moz_keyframes
            (n, map_keyframe_decls (universal_eval ~scopes ~at_path) frames)
      | Font_face descriptors ->
          Font_face (substitute_font_face ~scopes ~at_path descriptors)
      | Page_with_margins (sel, descriptors, margins) ->
          substitute_page_with_margins ~scopes ~at_path sel descriptors margins
      | other -> other)
