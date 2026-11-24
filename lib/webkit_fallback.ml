(** The WebKit compatibility fallbacks: which standard property has a prefixed
    spelling, what the prefixed declaration for a value is, and whether a set of
    targets still needs one.

    This sits apart from {!Optimize} because two passes have to agree about it.
    The prefix synthesis reads it to write a declaration, and the rule factoring
    reads it to refuse a rewrite that would separate the two halves of a pair it
    wrote. *)

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
  | Mask_composite_fallback
  | Mask_mode_fallback

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

(* CSS Masking 1 sec. 6.8 names the compositing operators over the mask layer
   below; WebKit's prefixed property names the same four with the Compositing 1
   sec. 10.2 operators, source over, source out, source in and xor. A list maps
   element by element and a [var()] has no prefixed spelling. *)
let rec prefixed_mask_composite :
    Properties.mask_composite -> Properties.webkit_mask_composite option =
  function
  | Add -> Some Source_over
  | Subtract -> Some Source_out
  | Intersect -> Some Source_in
  | Exclude -> Some Xor
  | Inherit -> Some Inherit
  | Initial -> Some Initial
  | Unset -> Some Unset
  | Revert -> Some Revert
  | Revert_layer -> Some Revert_layer
  | Composites layers ->
      let mapped = List.filter_map prefixed_mask_composite layers in
      if List.length mapped = List.length layers then
        Some (Composites mapped : Properties.webkit_mask_composite)
      else None
  | Var _ -> None

(* CSS Masking 1 sec. 6.3 [mask-mode] against WebKit's
   [-webkit-mask-source-type]: [alpha] and [luminance] are the same words,
   [match-source] is the [auto] that reads the source's own type. The prefixed
   property takes one value, so a layer list has no prefixed spelling. *)
let prefixed_mask_mode :
    Properties.mask_mode -> Properties.webkit_mask_source_type option = function
  | Alpha -> Some Alpha
  | Luminance -> Some Luminance
  | Match_source -> Some Auto
  | Inherit -> Some Inherit
  | Initial -> Some Initial
  | Unset -> Some Unset
  | Revert -> Some Revert
  | Revert_layer -> Some Revert_layer
  | Modes _ | Var _ -> None

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
    converted_fallback ~convert:prefixed_mask_composite Mask_composite_fallback
      Mask_composite Webkit_mask_composite "mask-composite"
      "-webkit-mask-composite";
    converted_fallback ~convert:prefixed_mask_mode Mask_mode_fallback Mask_mode
      Webkit_mask_source_type "mask-mode" "-webkit-mask-source-type";
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

(* How a declaration names its property: typed, or as an unknown property the
   reader kept opaque. *)
type spelling =
  | Typed of prop_key
  | Unknown of Properties.untyped_name
  | Guarded

let spelling = function
  | Theme_guarded _ -> Guarded
  | Declaration { property = Unknown_property u; _ } -> Unknown u
  | Declaration _ as decl -> Typed (Declaration.property_key decl)

(* A family's spelling: a typed property, written typed or read untyped because
   its value defeated the typed reader, or a vendor's prefix on a typed property
   that has no constructor of its own. Only custom and unknown properties carry
   a payload and neither is a family's, so a family's property is an immediate
   and pointer equality is its identity. *)
type spelled = Own of prop_key | Prefixed of Properties.vendor * prop_key

let spells spelled spelling =
  match (spelled, spelling) with
  | Own own, Typed key -> own == key
  | Own own, Unknown { own = Some (Prop p); _ } -> own == Key p
  | ( Prefixed (vendor, key),
      Unknown { own = None; vendor = Some v; spells = Some (Prop p); _ } ) ->
      Properties.equal_vendor vendor v && key == Key p
  | (Own _ | Prefixed _), (Typed _ | Unknown _ | Guarded) -> false

type family = { spec : spec; standard : spelled; prefixed : spelled }

let families =
  List.map
    (fun spec ->
      match spec with
      | Typed_fallback { property; webkit_property; _ } ->
          {
            spec;
            standard = Own (Key property);
            prefixed = Own (Key webkit_property);
          }
      | Mask_fallback_spec _ ->
          (* [-webkit-mask] has no typed property: it is only ever unknown *)
          {
            spec;
            standard = Own (Key Properties.Mask);
            prefixed = Prefixed (Properties.Webkit, Key Properties.Mask);
          })
    specs

(* The family [decl] heads, read off its property. *)
let spec_of_standard decl =
  let spelling = spelling decl in
  List.find_map
    (fun family ->
      if spells family.standard spelling then Some family.spec else None)
    families

(* The target contract is deliberately owned here rather than by the printer:
   adding a fallback changes the AST and must therefore be explicit to API
   callers. Which of these the targets read unprefixed is a fact about browsers,
   so {!Support} answers it from the generated web-features table and a browser
   that catches up moves the answer at the next regeneration. [mask-mode] and
   [mask-composite] take WebKit's own vocabulary, which the converters above
   map. *)
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
  | Mask_composite_fallback -> lacks "css.properties.mask-composite"
  | Mask_mode_fallback -> lacks "css.properties.mask-mode"

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
      { property = Unknown_property untyped; value = components; important; _ }
    -> (
      match spec_of_standard decl with
      | Some (Mask_fallback_spec { webkit_name; _ }) -> (
          let source = Declaration.v (Unknown_property untyped) components in
          let rendered = Declaration.string_of_value ~minify:false source in
          match Declaration.parse_declaration "mask" rendered with
          | Some (Declaration { property = Mask; value; _ }) ->
              let source = Declaration.v Mask (webkit_compatible_mask value) in
              opaque_fallback Mask_fallback webkit_name source important
          | Some _ | None ->
              fallback Mask_fallback
                (Unknown_property (Properties.untyped webkit_name))
                components important)
      | Some spec when condition_holds spec ->
          let kind = fallback_spec_kind spec in
          let _, webkit_name = fallback_spec_names spec in
          fallback kind
            (Unknown_property (Properties.untyped webkit_name))
            components important
      | Some _ | None -> None)
  | Declaration _ -> (
      match spec_of_standard decl with
      | Some spec when condition_holds spec -> fallback_from_spec spec
      | Some _ | None -> None)

let is_prefixed kind decl =
  let spelling = spelling decl in
  List.exists
    (fun family ->
      fallback_spec_kind family.spec = kind && spells family.prefixed spelling)
    families

(* The standard property name is what names a family everywhere else, so it is
   what a log line or a test failure should show. *)
let pp ctx kind =
  match fallback_spec_by_kind kind with
  | Some spec -> Pp.string ctx (fst (fallback_spec_names spec))
  | None -> Pp.string ctx "unknown"

let kind_of decl : t option =
  Option.map fallback_spec_kind (spec_of_standard decl)

(* The relation with neither side named the prefixed one, and with no [targets]
   to consult: a caller asking whether two declarations are the two halves of
   one fallback pair does not know which order they arrived in, and by the time
   it asks the synthesis has already decided the targets needed the pair.

   Same value is asked of the minified text rather than of the values
   themselves. The two sides are two property types wherever a prefixed
   spelling takes WebKit's older vocabulary, so nothing compares them directly,
   and the text is what the pair was written to make a browser read. *)
(* A pair's prefixed half is read through a theme guard; its standard half is
   not. *)
let rec unguarded = function
  | Theme_guarded { decl; _ } -> unguarded decl
  | Declaration _ as decl -> decl

let spells_prefixed spelling =
  List.exists (fun family -> spells family.prefixed spelling) families

let holds_a_prefixed_spelling decls =
  List.exists (fun decl -> spells_prefixed (spelling (unguarded decl))) decls

let is_pair a b =
  let paired standard prefixed =
    let standard_spelling = spelling standard in
    match
      List.find_opt
        (fun family -> spells family.standard standard_spelling)
        families
    with
    | Some family ->
        spells family.prefixed (spelling (unguarded prefixed))
        && Bool.equal
             (Declaration.is_important standard)
             (Declaration.is_important prefixed)
        && String.equal
             (Declaration.string_of_value ~minify:true standard)
             (Declaration.string_of_value ~minify:true prefixed)
    | None -> false
  in
  paired a b || paired b a

let pairs decls =
  let rec walk acc = function
    | [] -> acc
    | decl :: rest ->
        let acc =
          List.fold_left
            (fun acc other ->
              if is_pair decl other then (decl, other) :: acc else acc)
            acc rest
        in
        walk acc rest
  in
  if holds_a_prefixed_spelling decls then walk [] decls else []
