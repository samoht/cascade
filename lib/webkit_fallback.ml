(** The WebKit compatibility fallbacks: which standard property has a prefixed
    spelling, what the prefixed declaration for a value is, and whether a set of
    targets still needs one.

    This sits apart from {!Optimize} because two passes have to agree about it.
    The prefix synthesis reads it to write a declaration, and the rule factoring
    reads it to refuse a rewrite that would separate the two halves of a pair it
    wrote. A second table spelling the same relation is how they came to
    disagree: the factoring used to ask {!Shorthand.vendor_alias_pair}, which
    has no [mask-*] arm, so the two halves were split and the synthesis wrote
    the missing one back into a rule the group above it already served. *)

open Declaration

(** The families with a prefixed spelling. Sealed: a property that grows one
    stops every site that decides about the set from compiling. *)
type t =
  | User_select_fallback
  | Backdrop_filter_fallback
  | Hyphens_fallback
  | Text_decoration_color_fallback
  | Mask_fallback
  | Mask_image_fallback
  | Mask_position_fallback
  | Mask_size_fallback
  | Mask_repeat_fallback
  | Mask_clip_fallback
  | Mask_origin_fallback

(* A target that cannot read the standard property at all needs the fallback for
   every value of it. A target that reads both spellings needs it only where the
   value is not settled at parse time. *)
type fallback_condition = Any_value | Unresolved_value

type spec =
  | Typed_fallback : {
      kind : t;
      property : 'a Properties.property;
      webkit_property : 'b Properties.property;
      convert : 'a -> 'b option;
      name : string;
      webkit_name : string;
      condition : fallback_condition;
    }
      -> spec
  | Mask_fallback_spec of { name : string; webkit_name : string }

let typed_fallback ?(condition = Any_value) kind property webkit_property name
    webkit_name =
  Typed_fallback
    {
      kind;
      property;
      webkit_property;
      convert = Option.some;
      name;
      webkit_name;
      condition;
    }

(* The same, where the prefixed property does not take the standard one's
   vocabulary and a value outside the overlap gets no fallback. *)
let converted_fallback ?(condition = Any_value) ~convert kind property
    webkit_property name webkit_name =
  Typed_fallback
    { kind; property; webkit_property; convert; name; webkit_name; condition }

(* The prefixed mask box properties take WebKit's older vocabulary, which meets
   the [<coord-box>] of CSS Masking 1 sec. 6.4 and 6.5 on the three CSS box
   names alone. A value with no prefixed spelling gets no fallback rather than
   one the browser drops. *)
let rec prefixed_mask_box :
    Properties.mask_box -> Properties.webkit_mask_box option = function
  | Border_box -> Some Border_box
  | Content_box -> Some Content_box
  | Padding_box -> Some Padding_box
  | Inherit -> Some Inherit
  | Initial -> Some Initial
  | Unset -> Some Unset
  | Revert -> Some Revert
  | Revert_layer -> Some Revert_layer
  | Layers layers ->
      (* One layer outside the overlap costs the whole fallback: the prefixed
         property reads the list or none of it. *)
      let mapped = List.filter_map prefixed_mask_box layers in
      if List.length mapped = List.length layers then
        Some (Layers mapped : Properties.webkit_mask_box)
      else None
  | Fill_box | Stroke_box | View_box | No_clip | Var _ -> None

let specs =
  [
    typed_fallback User_select_fallback User_select Webkit_user_select
      "user-select" "-webkit-user-select";
    typed_fallback Backdrop_filter_fallback Backdrop_filter
      Webkit_backdrop_filter "backdrop-filter" "-webkit-backdrop-filter";
    typed_fallback Hyphens_fallback Hyphens Webkit_hyphens "hyphens"
      "-webkit-hyphens";
    typed_fallback ~condition:Unresolved_value Text_decoration_color_fallback
      Text_decoration_color Webkit_text_decoration_color "text-decoration-color"
      "-webkit-text-decoration-color";
    Mask_fallback_spec { name = "mask"; webkit_name = "-webkit-mask" };
    typed_fallback Mask_image_fallback Mask_image Webkit_mask_image "mask-image"
      "-webkit-mask-image";
    typed_fallback Mask_position_fallback Mask_position Webkit_mask_position
      "mask-position" "-webkit-mask-position";
    typed_fallback Mask_size_fallback Mask_size Webkit_mask_size "mask-size"
      "-webkit-mask-size";
    typed_fallback Mask_repeat_fallback Mask_repeat Webkit_mask_repeat
      "mask-repeat" "-webkit-mask-repeat";
    converted_fallback ~convert:prefixed_mask_box Mask_clip_fallback Mask_clip
      Webkit_mask_clip "mask-clip" "-webkit-mask-clip";
    converted_fallback ~convert:prefixed_mask_box Mask_origin_fallback
      Mask_origin Webkit_mask_origin "mask-origin" "-webkit-mask-origin";
  ]

let fallback_spec_kind = function
  | Typed_fallback { kind; _ } -> kind
  | Mask_fallback_spec _ -> Mask_fallback

let fallback_spec_condition = function
  | Typed_fallback { condition; _ } -> condition
  | Mask_fallback_spec _ -> Any_value

let fallback_spec_names = function
  | Typed_fallback { name; webkit_name; _ } -> (name, webkit_name)
  | Mask_fallback_spec { name; webkit_name } -> (name, webkit_name)

let fallback_spec_by_kind kind =
  List.find_opt (fun spec -> fallback_spec_kind spec = kind) specs

let fallback_spec_by_name select_name name =
  List.find_opt
    (fun spec -> String.equal (select_name (fallback_spec_names spec)) name)
    specs

let fallback_spec_by_standard_name = fallback_spec_by_name fst

(* The target contract is deliberately owned here rather than by the printer:
   adding a fallback changes the AST and must therefore be explicit to API
   callers. Which of these the targets read unprefixed is a fact about browsers,
   so {!Support} answers it from the generated web-features table and a browser
   that catches up moves the answer at the next regeneration. [mask-mode] and
   [mask-composite] are excluded because their prefixed forms have different
   grammars. *)
let required_fallback kind targets =
  let lacks key = Support.unimplemented_by targets key in
  match kind with
  | User_select_fallback -> lacks "css.properties.user-select"
  | Backdrop_filter_fallback -> lacks "css.properties.backdrop-filter"
  | Hyphens_fallback -> lacks "css.properties.hyphens"
  | Text_decoration_color_fallback ->
      (* Not a support gap: Safari/iOS answer the standard property under both
         spellings through 26.1, which no dataset records, so this stays a
         measured boundary. It pairs with [Unresolved_value], since a settled
         colour is served by the standard longhand on every declared target. *)
      let at_most (major, minor) (target_major, target_minor) =
        target_major < major || (target_major = major && target_minor <= minor)
      in
      at_most (26, 1) targets.safari || at_most (26, 1) targets.ios_safari
  | Mask_fallback -> lacks "css.properties.mask"
  | Mask_image_fallback -> lacks "css.properties.mask-image"
  | Mask_position_fallback -> lacks "css.properties.mask-position"
  | Mask_size_fallback -> lacks "css.properties.mask-size"
  | Mask_repeat_fallback -> lacks "css.properties.mask-repeat"
  | Mask_clip_fallback -> lacks "css.properties.mask-clip"
  | Mask_origin_fallback -> lacks "css.properties.mask-origin"

let webkit_compatible_mask : Properties.mask -> Properties.mask =
  let strip_layer (layer : Properties.mask_layer) =
    { layer with mode = Option.none; composite = Option.none }
  in
  function
  | Layer layer -> Layer (strip_layer layer)
  | Layers layers -> Layers (List.map strip_layer layers)
  | value -> value

let of_declaration targets decl : Declaration.declaration option =
  let fallback : type a.
      t -> a Properties.property -> a -> bool -> Declaration.declaration option
      =
   fun kind property value important ->
    if required_fallback kind targets then
      Some (Declaration.v ~important property value)
    else None
  in
  let opaque_fallback kind property source important =
    if required_fallback kind targets then
      match
        Declaration.parse_opaque_declaration property
          (Declaration.string_of_value ~minify:true source)
      with
      | Some prefixed ->
          Some (if important then Declaration.important prefixed else prefixed)
      | None -> None
    else None
  in
  let condition_holds spec =
    match fallback_spec_condition spec with
    | Any_value -> true
    | Unresolved_value -> Variables.declaration_uses_var decl
  in
  let fallback_from_spec spec =
    match (spec, decl) with
    | ( Typed_fallback { kind; property; webkit_property; convert; _ },
        Declaration { property = actual; value; important; _ } ) -> (
        match Properties.eq_property actual property with
        | Some Equal -> (
            match convert value with
            | Some value -> fallback kind webkit_property value important
            | None -> None)
        | None -> None)
    | ( Mask_fallback_spec { webkit_name; _ },
        Declaration { property = Mask; value; important; _ } ) ->
        let source = Declaration.v Mask (webkit_compatible_mask value) in
        opaque_fallback Mask_fallback webkit_name source important
    | _, (Theme_guarded _ | Declaration _) -> None
  in
  match decl with
  | Theme_guarded _ -> None
  | Declaration
      { property = Unknown_property name; value = components; important; _ }
    -> (
      match fallback_spec_by_standard_name name with
      | Some (Mask_fallback_spec { webkit_name; _ }) -> (
          let source = Declaration.v (Unknown_property name) components in
          let rendered = Declaration.string_of_value ~minify:false source in
          match Declaration.parse_declaration "mask" rendered with
          | Some (Declaration { property = Mask; value; _ }) ->
              let source = Declaration.v Mask (webkit_compatible_mask value) in
              opaque_fallback Mask_fallback webkit_name source important
          | Some _ | None ->
              fallback Mask_fallback (Unknown_property webkit_name) components
                important)
      | Some spec when condition_holds spec ->
          let kind = fallback_spec_kind spec in
          let _, webkit_name = fallback_spec_names spec in
          fallback kind (Unknown_property webkit_name) components important
      | Some _ | None -> None)
  | Declaration _ -> (
      match fallback_spec_by_standard_name (Declaration.property_name decl) with
      | Some spec when condition_holds spec -> fallback_from_spec spec
      | Some _ | None -> None)

let is_prefixed kind decl =
  match (fallback_spec_by_kind kind, decl) with
  | Some spec, Declaration _ ->
      let _, webkit_name = fallback_spec_names spec in
      String.equal (Declaration.property_name decl) webkit_name
  | None, Declaration _ -> false
  | (None | Some _), Theme_guarded _ -> false

(* The (standard, prefixed) names of the family [decl]'s property heads, or none
   where the property has no prefixed spelling. *)
let fallback_spec_names_of_declaration decl =
  match decl with
  | Theme_guarded _ -> None
  | Declaration _ ->
      Option.map fallback_spec_names
        (fallback_spec_by_standard_name (Declaration.property_name decl))

(* The standard property name is what names a family everywhere else, so it is
   what a log line or a test failure should show. *)
let pp ctx kind =
  match fallback_spec_by_kind kind with
  | Some spec -> Pp.string ctx (fst (fallback_spec_names spec))
  | None -> Pp.string ctx "unknown"

let kind_of decl : t option =
  match decl with
  | Theme_guarded _ -> None
  | Declaration _ ->
      Option.map fallback_spec_kind
        (fallback_spec_by_standard_name (Declaration.property_name decl))

(* The relation with neither side named the prefixed one, and with no [targets]
   to consult: a caller asking whether two declarations are the two halves of
   one fallback pair does not know which order they arrived in, and by the time
   it asks the synthesis has already decided the targets needed the pair.

   Same value is asked of the minified text rather than of the values
   themselves. The two sides are two property types wherever a prefixed
   spelling takes WebKit's older vocabulary, so nothing compares them directly,
   and the text is what the pair was written to make a browser read. *)
(* Every prefixed spelling in the table above is a [-webkit-] name, so a
   declaration list holding none of those holds no pair. The scan below is
   quadratic in a rule's declarations and runs on every rewrite candidate, so it
   is worth answering the cheap question first. *)
let holds_a_prefixed_spelling decls =
  List.exists
    (fun decl ->
      String.starts_with ~prefix:"-webkit-" (Declaration.property_name decl))
    decls

let is_pair a b =
  let names decl = fallback_spec_names_of_declaration decl in
  let paired standard prefixed =
    match names standard with
    | Some (_, webkit_name) ->
        String.equal (Declaration.property_name prefixed) webkit_name
        && Bool.equal
             (Declaration.is_important standard)
             (Declaration.is_important prefixed)
        && String.equal
             (Declaration.string_of_value ~minify:true standard)
             (Declaration.string_of_value ~minify:true prefixed)
    | None -> false
  in
  paired a b || paired b a
