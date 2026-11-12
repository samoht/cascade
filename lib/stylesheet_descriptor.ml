open Stylesheet_intf
open Stylesheet_at_rule

(* Read a font-face descriptor *)
(* Helper to read descriptor value after colon *)
(* CSS Fonts 4 (ED) sec. 4.4 writes each font property descriptor's grammar out
   in full, and sec. 4.6 gives the settings descriptors the corresponding
   property's values "except that the CSS-wide keywords are omitted". No
   descriptor grammar takes one. The shared property readers a descriptor
   delegates to do take them, so the refusal belongs at this boundary, next to
   the var() one. *)
let refuse_css_wide_descriptor r =
  let save = Cursor.save r in
  (match Cursor.peek r with
  | Some (Component.Preserved { kind = Token.Ident name; _ })
    when Properties.is_css_wide_keyword name ->
      Cursor.skip r;
      Cursor.ws r;
      let alone = Cursor.is_done r || Cursor.peek_semicolon r in
      Cursor.restore r save;
      if alone then
        Cursor.err_invalid r
          ("CSS-wide keyword in @font-face descriptor: " ^ name)
  | _ -> ());
  Cursor.restore r save

let read_descriptor_value read_fn constructor r =
  Cursor.ws r;
  if not (Cursor.colon r) then Cursor.err_expected r "':'";
  Cursor.ws r;
  refuse_css_wide_descriptor r;
  constructor (read_fn r)

(* One item of a descriptor body: a descriptor, a stray [;] that CSS Syntax 3
   (ED) sec. 5.5.5 discards with no declaration to validate, or the end of the
   body. [Declaration.read_declaration] answers [None] for an item that opens a
   block instead; a descriptor body holds no rules, so that item is invalid and
   is reported rather than left for the caller to loop on. *)
let read_descriptor_item (r : Cursor.t) =
  Cursor.ws r;
  if Cursor.is_done r then `Done
  else if Cursor.peek_semicolon r then (
    Cursor.skip r;
    `Skip)
  else
    match Declaration.read_declaration r with
    | Some desc -> `Descriptor desc
    | None -> Cursor.err_expected r "descriptor"

(* CSS Cascade 5 sec. 6.2 ranks an important author declaration above a normal
   one whatever their order, and sec. 6.4 breaks a tie between two of the same
   importance by order of appearance. A descriptor read later therefore takes
   the slot one already read holds, unless the one held is important and it is
   not: then the later declaration is the one that is dropped, and the survivor
   keeps the place it was written in. An opaque descriptor is the conservative
   exception: Cascade cannot know whether a browser accepts it, so neither it
   nor a same-name typed descriptor can erase the other. *)
let rec descriptor_has_opaque_value = function
  | Declaration.Declaration { property = Properties.Unknown_property _; _ } ->
      true
  | Declaration.Declaration _ -> false
  | Declaration.Theme_guarded { decl; _ } -> descriptor_has_opaque_value decl

let replace_descriptor desc acc =
  let same_slot held =
    (not (descriptor_has_opaque_value held || descriptor_has_opaque_value desc))
    && String.equal
         (Declaration.property_name held)
         (Declaration.property_name desc)
  in
  match List.find_opt same_slot acc with
  | Some held
    when Declaration.is_important held && not (Declaration.is_important desc) ->
      acc
  | Some _ -> desc :: List.filter (fun held -> not (same_slot held)) acc
  | None -> desc :: acc

let read_descriptor_step normalize inner acc =
  match read_descriptor_item inner with
  | `Done -> `Done (List.rev acc)
  | `Skip -> `More acc
  | `Descriptor desc -> `More (normalize desc acc)

let read_descriptor_block normalize inner =
  read_items_with_recovery ~skip:skip_invalid_item
    ~construct:Error.Recovery.Declaration
    (read_descriptor_step normalize)
    inner []

(* CSS Fonts 4 sec. 4.4: a descriptor range whose startpoint is larger than its
   endpoint is well defined, the user agent swapping the two endpoints for font
   matching. The swap is on the computed value, so the descriptor keeps the
   order it was written in. *)
(* CSS Fonts 4 (ED) sec. 4.4 opens font-style, font-weight and font-width with
   [auto] and gives it as their initial value, outside the [{1,2}] the rest of
   the grammar allows: it is the whole value or it is not there. The properties
   of the same name have no [auto], so the shared readers cannot answer for
   it. *)
let descriptor_is_auto value =
  String.equal (String.lowercase_ascii (String.trim value)) "auto"

(* Sec. 4.6's "except that the CSS-wide keywords are omitted" answers for an
   endpoint of the [{1,2}] range as much as for the whole value.
   [refuse_css_wide_descriptor] cannot: it stands at the boundary, where a
   [<family-name>] sequence reads a keyword as an ordinary ident, so it fires
   only on one standing alone. *)
let refuse_css_wide_endpoint c =
  match Cursor.peek c with
  | Some (Component.Preserved { kind = Token.Ident name; _ })
    when Properties.is_css_wide_keyword name ->
      Cursor.err_invalid c ("CSS-wide keyword in @font-face descriptor: " ^ name)
  | Some _ | None -> ()

let read_font_weight_descriptor r =
  read_descriptor_value Declaration.read_property_value
    (fun value ->
      if descriptor_is_auto value then Font_weight_auto
      else
        let c = Cursor.of_string value in
        let absolute () =
          refuse_css_wide_endpoint c;
          match Properties.read_font_weight c with
          | Bolder | Lighter ->
              Cursor.err_invalid c
                "relative weight in an @font-face font-weight descriptor"
          | weight -> weight
        in
        let first = absolute () in
        Cursor.ws c;
        if Cursor.is_done c then Font_weight first
        else
          let second = absolute () in
          Cursor.ws c;
          Cursor.expect_eof c;
          Font_weight_range (first, second))
    r

let read_font_style_descriptor r =
  read_descriptor_value Declaration.read_property_value
    (fun value ->
      if descriptor_is_auto value then Font_style_auto
      else
        let c = Cursor.of_string value in
        let style = Properties.read_font_style c in
        Cursor.ws c;
        Cursor.expect_eof c;
        Font_style style)
    r

let validate_nonempty_descriptor r name value =
  if String.trim value = "" then
    Cursor.err_invalid r ("empty " ^ name ^ " descriptor")

let read_font_family_descriptor r =
  read_descriptor_value
    (fun r ->
      let family = Properties.read_font_family_name r in
      Cursor.ws r;
      if not (Cursor.is_done r || Cursor.peek_semicolon r) then
        Cursor.err_invalid r "trailing tokens after @font-face font-family";
      [ family ])
    (fun v -> Font_family v)
    r

let read_font_stretch_descriptor r =
  read_descriptor_value Declaration.read_property_value
    (fun value ->
      if descriptor_is_auto value then Font_stretch_auto
      else
        let c = Cursor.of_string value in
        let endpoint () =
          refuse_css_wide_endpoint c;
          Properties.read_font_stretch c
        in
        let first = endpoint () in
        Cursor.ws c;
        if Cursor.is_done c then Font_stretch first
        else
          let second = endpoint () in
          Cursor.ws c;
          Cursor.expect_eof c;
          Font_stretch_range (first, second))
    r

(* CSS Syntax 3 (ED) sec. 4.3.14: this descriptor's value is the one place in
   the language the tokenizer is asked for unicode ranges, so re-lex it with the
   flag the rest of the sheet is read without. *)
let read_unicode_range_descriptor r =
  read_descriptor_value
    (fun cur ->
      let raw = Cursor.consume_to_decl_end ~trim:true cur in
      let inner = Cursor.of_string ~unicode_ranges:true raw in
      let values =
        Cursor.list ~at_least:1 ~sep:Cursor.comma Properties.read_unicode_range
          inner
      in
      Cursor.ws inner;
      if not (Cursor.is_done inner) then
        Cursor.err_invalid cur "unicode-range trailing tokens";
      values)
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
  match font_variant_desc_value ident with
  | Some value -> value
  | None -> Cursor.err_invalid r ("font-variant descriptor value: " ^ ident)

let read_font_variant_keywords r : font_variant_descriptor =
  let at_value_end () = Cursor.is_done r || Cursor.peek_semicolon r in
  let snap = Cursor.save r in
  let first = Cursor.ident ~keep_case:false r in
  Cursor.ws r;
  match (first, at_value_end ()) with
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

let rec read_font_variant_descriptor r : font_variant_descriptor =
  Cursor.ws r;
  match Cursor.peek r with
  | Some (Component.Func { node = { name; _ }; _ })
    when String.lowercase_ascii name = "var" ->
      Var (Values.read_var read_font_variant_descriptor r)
  | Some _ | None -> read_font_variant_keywords r

let read_font_face_desc name r =
  match name with
  (* FONT_FACE_DESCRIPTOR_START - Used by test/spec/browser *)
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
  | "size-adjust" ->
      read_descriptor_value Font_face.read_size_adjust
        (fun v -> Size_adjust v)
        r
  | "ascent-override" ->
      read_descriptor_value Font_face.read_metric_override
        (fun v -> Ascent_override v)
        r
  | "descent-override" ->
      read_descriptor_value Font_face.read_metric_override
        (fun v -> Descent_override v)
        r
  | "line-gap-override" ->
      read_descriptor_value Font_face.read_metric_override
        (fun v -> Line_gap_override v)
        r
  (* FONT_FACE_DESCRIPTOR_END - Used by test/spec/browser *)
  | _ -> Cursor.err_invalid r ("unknown font-face descriptor: " ^ name)

let rec components_upto_semicolon = function
  | [] | Component.Preserved { kind = Token.Semicolon; _ } :: _ -> []
  | component :: rest -> component :: components_upto_semicolon rest

(* CSS Variables 1 sec. 3 substitutes [var()] in a property value; an @font-face
   descriptor is not a property, so no descriptor grammar accepts a var() and
   browsers drop the whole declaration. The descriptor readers delegate to the
   shared property readers, which accept var() legitimately in property
   position, so the rejection belongs at the descriptor boundary. *)
let descriptor_value_has_var r =
  Component.has_var (components_upto_semicolon (Cursor.remaining r))

(* Whether the descriptor called [name] keeps a var() for the inline pass, asked
   of the typed AST rather than of a second list of names: read a value only
   such a descriptor accepts, then put the descriptor to
   {!resolve_font_face_var}, the one table [Inline] fills in. A descriptor with
   no resolution path either refuses the probe or lands on an arm that table
   answers [None] for. Only a value carrying a var() asks, so an ordinary
   descriptor never pays for the probe. *)
let descriptor_resolves_var name =
  let probe = Cursor.of_string ":var(--x)" in
  match read_font_face_desc name probe with
  | descriptor ->
      Option.is_some
        (resolve_font_face_var ~src:Fun.id ~unicode_range:Fun.id
           ~font_family:Fun.id ~font_style:Fun.id ~font_weight:Fun.id
           ~font_stretch:Fun.id ~font_display:Fun.id ~font_variant:Fun.id
           ~font_feature_settings:Fun.id ~font_variation_settings:Fun.id
           ~metric_override:Fun.id ~size_adjust:Fun.id descriptor)
  | exception Error.Parse_error _ -> false

(* CSS Syntax 3 (ED) sec. 5.5.5 gives an [<at-keyword-token>] to "consume an
   at-rule" rather than to the declaration reader, and keeps the declarations
   written on either side of it. No @font-face descriptor is an at-rule, so the
   rule read here is dropped - and it alone, because sec. 5.5.2 ends an at-rule
   at its block or at its [;], never at the descriptor after it. A cursor that
   does not recover raises instead, as the other descriptor bodies do. *)
let skip_font_face_at_rule r name loc =
  let error = Error.unknown_at_rule loc name in
  if not (Cursor.recover r) then Error.fail error;
  let start = Cursor.save r in
  skip_past_rule r;
  Cursor.push_warning r
    ~recovery:(Cursor.dropped_since r start Error.Recovery.Rule)
    error

let read_font_face_descriptor (r : Cursor.t) : font_face_descriptor option =
  Cursor.ws r;
  if Cursor.is_done r then None
  else if Cursor.peek_semicolon r then (
    Cursor.skip r;
    None)
  else
    match Cursor.peek r with
    | Some (Component.Preserved { kind = Token.At_keyword name; loc; _ }) ->
        skip_font_face_at_rule r name loc;
        None
    | _ -> (
        let start = Cursor.save r in
        match
          (* The name is read inside the recovered region: a descriptor with no
             name at all ([@font-face { : url(a.woff2) }]) fails here, and it is
             dropped like any other bad one rather than taking the rule. *)
          let name = Cursor.ident ~keep_case:false r in
          if descriptor_value_has_var r && not (descriptor_resolves_var name)
          then Cursor.err_invalid r ("var() in @font-face descriptor: " ^ name)
          else read_font_face_desc name r
        with
        | descriptor ->
            Cursor.ws r;
            if Cursor.peek_semicolon r then Cursor.skip r;
            Some descriptor
        | exception Error.Parse_error e ->
            (* CSS Fonts 4 sec. 4.1 / CSS Syntax 3 (ED) sec. 5.5.5: a descriptor
               that does not parse - one with no name at all, an unknown name
               (Fontsource's [font-named-instance]) or an invalid value of a
               known one ([font-display:maybe]) - is dropped and the rest of the
               @font-face is kept, matching browsers. *)
            Cursor.skip_past_semicolon r;
            Cursor.push_warning r
              ~recovery:
                (Cursor.dropped_since r start Error.Recovery.Declaration)
              e;
            None)

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
  (* CSS Fonts 4 sec. 4.1: missing [font-family] / [src] is a semantic mismatch,
     not a syntax one. [validate_partial_statement] flags it; the syntactic
     reader accepts. *)
  let descriptors = Cursor.braces read_font_face_block r in
  Font_face descriptors

let read_counter_style_system_value r =
  let system = Cursor.ident ~keep_case:false r in
  match system with
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

(* CSS Counter Styles 3 (ED) sec. 3.2: <symbol> = <string> | <image> |
   <custom-ident>. The image arm is read for its grammar and kept as the text it
   was written with, as the string and ident arms are. *)
let read_counter_symbol r =
  match Cursor.string_opt r with
  | Some symbol -> symbol
  | None -> (
      match Cursor.peek r with
      | Some (Component.Func _)
      | Some (Component.Preserved { kind = Token.Url _; _ }) ->
          Pp.to_string ~minify:true Properties.pp_background_image
            (Properties.read_background_image r)
      | Some _ | None -> Cursor.custom_ident "counter style symbol" r)

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

(* CSS Counter Styles 3 (ED) sec. 3.7: <counter-style-name> is a <custom-ident>
   excluding [none], on top of what CSS Values 4 sec. 4.2 excludes from every
   one of them. Blink 151 refuses [revert-rule] here too, though it takes it as
   a [list-style-type] name. *)
let read_counter_style_name c =
  Cursor.custom_ident ~reserved:[ "none"; "revert-rule" ] "counter style name" c

(* Validates the value against the descriptor's grammar and keeps the text: the
   AST carries the authored spelling, and what this adds is the refusal of a
   value no section grants. Each reader names the section that decides it. *)
let read_counter_validated_descriptor ~what ~check constructor r =
  read_descriptor_value
    (fun r ->
      let value = Declaration.read_property_value r in
      validate_nonempty_descriptor r "counter-style" value;
      let c = Cursor.of_string value in
      check c;
      Cursor.ws c;
      if not (Cursor.is_done c) then
        Cursor.err_invalid r
          (String.concat "" [ "trailing tokens in @counter-style "; what ]);
      value)
    constructor r

(* sec. 3.5: [[<integer> | infinite]{2}]# | auto. *)
let check_counter_range c =
  let bound c =
    match Cursor.peek_keyword c with
    | Some "infinite" -> ignore (Cursor.ident c)
    | Some _ | None -> ignore (Cursor.int c)
  in
  let pair c =
    bound c;
    Cursor.ws c;
    bound c
  in
  match Cursor.peek_keyword c with
  | Some "auto" -> ignore (Cursor.ident c)
  | Some _ | None -> ignore (Cursor.list ~at_least:1 ~sep:Cursor.comma pair c)

(* sec. 3.6: <integer [0,inf]> && <symbol>, so the two come in either order. *)
let check_counter_pad c =
  let non_negative c =
    let n = Cursor.int c in
    if n < 0 then Cursor.err_invalid c "@counter-style pad takes no negative"
  in
  match Cursor.option non_negative c with
  | Some () ->
      Cursor.ws c;
      ignore (read_counter_symbol c)
  | None ->
      ignore (read_counter_symbol c);
      Cursor.ws c;
      non_negative c

(* sec. 3.4: <symbol> <symbol>?. *)
let check_counter_negative c =
  ignore (read_counter_symbol c);
  Cursor.ws c;
  if not (Cursor.is_done c) then ignore (read_counter_symbol c)

(* sec. 3.3: [<integer [0,inf]> && <symbol>]#. *)
let check_counter_additive_symbols c =
  ignore (Cursor.list ~at_least:1 ~sep:Cursor.comma check_counter_pad c)

(* sec. 3.8: auto | bullets | numbers | words | spell-out |
   <counter-style-name>. *)
let check_counter_speak_as c =
  match Cursor.peek_keyword c with
  | Some ("auto" | "bullets" | "numbers" | "words" | "spell-out") ->
      ignore (Cursor.ident c)
  | Some _ | None -> ignore (read_counter_style_name c)

(* CSS Counter Styles 3 sec. 3: "unknown descriptors are invalid and ignored".
   One descriptor of the body, the caller looping over the rest, so a descriptor
   that fails leaves the ones already read untouched. The declaration is the
   whole of the descriptor's value: an [!important] flag, which no descriptor
   takes, invalidates the declaration it follows rather than being left over as
   an item of its own, as it is in Blink 146. *)
let read_counter_style_descriptor (r : Cursor.t) : counter_style_descriptor =
  let name = Cursor.ident ~keep_case:false r in
  let descriptor =
    match name with
    (* COUNTER_STYLE_DESCRIPTOR_START - Used by test/spec/browser *)
    | "system" -> read_counter_style_system_descriptor r
    | "symbols" -> read_counter_symbols_descriptor r
    | "suffix" -> read_counter_symbol_descriptor (fun s -> Suffix s) r
    | "prefix" -> read_counter_symbol_descriptor (fun s -> Prefix s) r
    | "fallback" ->
        read_counter_validated_descriptor ~what:"fallback"
          ~check:(fun c -> ignore (read_counter_style_name c))
          (fun s -> Fallback s)
          r
    | "range" ->
        read_counter_validated_descriptor ~what:"range"
          ~check:check_counter_range
          (fun s -> Range s)
          r
    | "pad" ->
        read_counter_validated_descriptor ~what:"pad" ~check:check_counter_pad
          (fun s -> Pad s)
          r
    | "negative" ->
        read_counter_validated_descriptor ~what:"negative"
          ~check:check_counter_negative
          (fun s -> Negative s)
          r
    | "additive-symbols" ->
        read_counter_validated_descriptor ~what:"additive-symbols"
          ~check:check_counter_additive_symbols
          (fun s -> Additive_symbols s)
          r
    | "speak-as" ->
        read_counter_validated_descriptor ~what:"speak-as"
          ~check:check_counter_speak_as
          (fun s -> Speak_as s)
          r
    (* COUNTER_STYLE_DESCRIPTOR_END - Used by test/spec/browser *)
    | _ -> Cursor.err_invalid r ("unknown counter-style descriptor: " ^ name)
  in
  Cursor.ws r;
  if not (Cursor.is_done r || Cursor.peek_semicolon r) then
    Cursor.err_invalid r ("trailing tokens after @counter-style " ^ name);
  if Cursor.peek_semicolon r then Cursor.skip r;
  descriptor

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

(* One item of a descriptor body: a descriptor, a stray [;] that CSS Syntax 3
   (ED) sec. 5.5.5 discards with no declaration to validate, or the end of the
   body. *)
let read_descriptor_body_step read_one replace inner acc =
  Cursor.ws inner;
  if Cursor.is_done inner then `Done (List.rev acc)
  else if Cursor.peek_semicolon inner then (
    Cursor.skip inner;
    `More acc)
  else `More (replace (read_one inner) acc)

let read_counter_style_descriptors r =
  Cursor.braces
    (fun inner ->
      read_items_with_recovery ~skip:skip_invalid_item
        ~construct:Error.Recovery.Declaration
        (read_descriptor_body_step read_counter_style_descriptor
           replace_counter_style_descriptor)
        inner [])
    r

let read_counter_style (r : Cursor.t) : statement =
  Cursor.with_context r "@counter-style" @@ fun () ->
  Cursor.expect_at_keyword "counter-style" r;
  Cursor.ws r;
  (* Sec. 2: the prelude is the same <counter-style-name> the fallback and
     speak-as descriptors take. *)
  let name = read_counter_style_name r in
  Cursor.ws r;
  let descriptors =
    read_counter_style_descriptors r
    |> List.stable_sort (fun a b ->
        compare
          (counter_style_descriptor_rank a)
          (counter_style_descriptor_rank b))
  in
  Counter_style (name, descriptors)

(* CSS Paged Media 3 sec. 4.2: a page selector is an optional page name followed
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

(* CSS Paged Media 3 section 4.3: [<page-selector-list> = <page-selector>#],
   [<page-selector> = <ident-token>? <pseudo-page>*], with each pseudo-page from
   the closed set [first | left | right | blank], so [@page invoice:blank:first]
   is well-formed. *)
(* CSS Paged Media 3 sec. 4.3 closes [<pseudo-page>] over four names, so each is
   a keyword and not the page name beside it. *)
let page_pseudo_of_name r s name =
  match Common.String.lowercase_ascii_preserve name with
  | "first" -> First
  | "left" -> Left
  | "right" -> Right
  | "blank" -> Blank
  | _ -> page_selector_error r s

let parse_page_selectors r selector =
  let s = String.trim selector in
  let len = String.length s in
  let consume_ident = page_selector_consume_ident s len in
  let skip_ws = page_selector_skip_ws s len in
  let pseudo_of_name = page_pseudo_of_name r s in
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
    (* Reaching the end here is the tail of a [,]: the list is
       [<page-selector>#] and has no empty item, so a trailing comma is no more
       a selector list than a lone one. *)
    if i >= len then (
      if acc <> [] then page_selector_error r s;
      List.rev acc)
    else
      let after_ident = consume_ident i in
      let name =
        if after_ident = i then Option.None
        else Some (String.sub s i (after_ident - i))
      in
      let pseudos, after_pseudo = consume_pseudo_pages [] after_ident in
      (* sec. 4.2 writes a page selector as a name, one or more pseudo-pages, or
         both, so an item with neither is no selector: [@page ,] names two of
         them and Chrome 151 drops the rule. The prelude may be absent
         altogether, which the caller answers before reaching here. *)
      if Option.is_none name && pseudos = [] then page_selector_error r s;
      let sel = { name; pseudos } in
      let after_pseudo = skip_ws after_pseudo in
      if after_pseudo >= len then List.rev (sel :: acc)
      else if s.[after_pseudo] = ',' then
        consume_selectors (sel :: acc) (after_pseudo + 1)
      else page_selector_error r s
  in
  consume_selectors [] 0

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

(* CSS Paged Media 3 sec. 6: Appendix A lists the CSS 2.1 properties that apply
   in a page and in a margin context, and behaviour for a property outside CSS
   2.1 is left undefined "to allow the gradual addition of appropriate CSS3
   properties as they emerge" - undefined, not invalid. Blink 146 duly keeps
   every property it knows in both contexts. A name filter here would drop text
   browsers keep, so the page contexts take the declarations a style rule takes:
   a value its property's grammar rejects is still rejected, and so is an item
   that is no declaration at all. *)
let read_page_margin_rule r =
  match Cursor.peek r with
  | Some (Component.Preserved { kind = Token.At_keyword raw; _ }) ->
      (* Sec. 5.2 names the margin boxes, so each name is a keyword. *)
      let name = Common.String.lowercase_ascii_preserve raw in
      if not (List.mem name allowed_page_margin_names) then
        Cursor.err_invalid r ("unknown page margin rule: @" ^ raw);
      Cursor.skip r;
      Cursor.ws r;
      (* Sec. 5 gives the margin at-rule a [<declaration-list>], which CSS
         Syntax 3 (ED) sec. 5.5.5 consumes even when it holds nothing, so an
         empty box is valid and Blink 146 keeps one. That it paints nothing is
         [Block.drop_empty_rules]'s call to make, not the reader's. *)
      let descriptors =
        Cursor.braces (read_descriptor_block replace_descriptor) r
      in
      { name; descriptors }
  | _ -> Cursor.err_expected r "page margin rule"

(* CSS Paged Media 3 sec. 2: a [@page] body holds page properties and margin
   at-rules, so its items end two different ways and are read apart. *)
let read_page_step inner (descriptors, margins) =
  match Cursor.peek inner with
  | Some (Component.Preserved { kind = Token.At_keyword _; _ }) ->
      `More (descriptors, read_page_margin_rule inner :: margins)
  | _ -> (
      match read_descriptor_item inner with
      | `Done -> `Done (List.rev descriptors, List.rev margins)
      | `Skip -> `More (descriptors, margins)
      | `Descriptor desc -> `More (replace_descriptor desc descriptors, margins)
      )

let read_page_body inner =
  read_items_with_recovery ~skip:skip_invalid_item
    ~construct:Error.Recovery.Declaration read_page_step inner ([], [])

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
  if not (Custom_property_name.is_valid name) then
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
  match Cursor.int inner with
  | n ->
      if n < 0 then
        Cursor.err_invalid inner "base-palette index must be non-negative";
      Index n
  | exception Cursor.Parse_error _ -> (
      (* [light] and [dark] are keywords, so CSS Values 4 sec. 4.1 reads them
         case-insensitively; any other ident is the author's own. *)
      let ident = Cursor.ident inner in
      match Common.String.lowercase_ascii_preserve ident with
      | "light" -> Light
      | "dark" -> Dark
      | _ when ident <> "" -> Palette_ident ident
      | _ -> Cursor.err_expected inner "base-palette value")

let read_override_color_entry c =
  let index = Cursor.int c in
  if index < 0 then
    Cursor.err_invalid c "override-colors index must be non-negative";
  Cursor.ws c;
  let color = Values.read_color c in
  Cursor.ws c;
  (index, color)

(* CSS Fonts 4 sec. 12.1 gives each [@font-palette-values] descriptor a grammar
   of its own, and the declaration is the whole of it: a trailing [!important]
   or a stray ident makes the declaration invalid rather than the leftover
   alone, as it does in Blink 146. *)
let read_font_palette_descriptor outer inner : font_palette_descriptor =
  let desc_name = Cursor.ident ~keep_case:false inner in
  Cursor.ws inner;
  if not (Cursor.colon inner) then Cursor.err_expected inner "':'";
  Cursor.ws inner;
  let desc =
    match desc_name with
    | "font-family" ->
        Palette_font_family
          (Cursor.list ~sep:Cursor.comma Properties.read_font_family_name inner)
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
  if not (Cursor.is_done inner || Cursor.peek_semicolon inner) then
    Cursor.err_invalid outer
      ("trailing tokens after @font-palette-values " ^ desc_name);
  if Cursor.peek_semicolon inner then Cursor.skip inner;
  desc

let read_font_palette_descriptors outer =
  Cursor.braces
    (fun inner ->
      read_items_with_recovery ~skip:skip_invalid_item
        ~construct:Error.Recovery.Declaration
        (read_descriptor_body_step
           (read_font_palette_descriptor outer)
           replace_font_palette_descriptor)
        inner [])
    outer

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

let read_font_feature_values_entries outer =
  let rec loop inner acc =
    let start = Cursor.save inner in
    match read_font_feature_value_entry outer inner with
    | Some entry -> loop inner (replace_font_feature_value entry acc)
    | None ->
        Cursor.ws inner;
        if Cursor.is_done inner then List.rev acc else loop inner acc
    | exception Error.Parse_error e ->
        Cursor.skip_past_semicolon inner;
        Cursor.push_warning inner
          ~recovery:
            (Cursor.dropped_since inner start Error.Recovery.Declaration)
          e;
        loop inner acc
  in
  Cursor.braces (fun inner -> loop inner []) outer

let read_font_feature_values_block outer inner =
  match Cursor.at_keyword_opt inner with
  | None -> Cursor.err_expected inner "@font-feature-values nested at-rule"
  | Some name ->
      let name = String.lowercase_ascii name in
      if not (valid_font_feature_values_block name) then
        Cursor.err_invalid outer ("unknown @font-feature-values block: @" ^ name);
      (name, read_font_feature_values_entries inner)

(* CSS Fonts 4 sec. 11.1 fills the body with feature value blocks, so it is a
   list of rules rather than of declarations: a block the body rejects ends at
   its own [{}] or at a [;] (CSS Syntax 3 (ED) sec. 5.5.2), which leaves the
   blocks written around it in the rule. A [;] with nothing before it has no
   declaration to validate and is discarded (sec. 5.5.5). *)
let read_font_feature_values_step outer inner acc =
  Cursor.ws inner;
  if Cursor.is_done inner then `Done (List.rev acc)
  else if Cursor.peek_semicolon inner then (
    Cursor.skip inner;
    `More acc)
  else `More (read_font_feature_values_block outer inner :: acc)

let read_font_feature_values_blocks outer =
  Cursor.braces
    (fun inner ->
      read_items_with_recovery ~skip:skip_past_rule
        ~construct:Error.Recovery.Rule
        (read_font_feature_values_step outer)
        inner [])
    outer

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

(* CSS View Transitions 2 sec. 2.4 gives each [@view-transition] descriptor a
   grammar of its own, and the declaration is the whole of it. *)
let read_view_transition_descriptor outer inner : view_transition_descriptor =
  let desc_name = Cursor.ident ~keep_case:false inner in
  Cursor.ws inner;
  if not (Cursor.colon inner) then Cursor.err_expected inner "':'";
  Cursor.ws inner;
  let desc =
    match desc_name with
    | "navigation" -> read_view_transition_navigation inner
    | "types" -> read_view_transition_types inner
    | name ->
        Cursor.err_invalid outer ("unknown @view-transition descriptor: " ^ name)
  in
  Cursor.ws inner;
  if not (Cursor.is_done inner || Cursor.peek_semicolon inner) then
    Cursor.err_invalid outer
      ("trailing tokens after @view-transition " ^ desc_name);
  if Cursor.peek_semicolon inner then Cursor.skip inner;
  desc

let read_view_transition_descriptors outer =
  Cursor.braces
    (fun inner ->
      read_items_with_recovery ~skip:skip_invalid_item
        ~construct:Error.Recovery.Declaration
        (read_descriptor_body_step
           (read_view_transition_descriptor outer)
           replace_view_transition_descriptor)
        inner [])
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
