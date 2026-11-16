open Common
open Values
open Properties_intf
open Prop_common

let rec pp_font_optical_sizing : font_optical_sizing Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_optical_sizing ctx v

let rec read_font_optical_sizing t : font_optical_sizing =
  Cursor.enum_or_var "font-optical-sizing"
    [
      ("auto", (Auto : font_optical_sizing));
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_optical_sizing t))
    t

let rec pp_font_kerning : font_kerning Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Normal -> Pp.string ctx "normal"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_kerning ctx v

let rec read_font_kerning t : font_kerning =
  Cursor.enum_or_var "font-kerning"
    [
      ("auto", (Auto : font_kerning));
      ("normal", Normal);
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_kerning t))
    t

let rec pp_font_language_override : font_language_override Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | String s -> Pp.quoted_string ctx s
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_language_override ctx v

(* An OpenType tag is four printable ASCII characters. CSS Fonts 4 sec. 6.13
   pads a shorter font-language-override tag with spaces before matching it, so
   there the tag may be one to four. *)
let read_opentype_tag ?(padded = false) t =
  let tag = Cursor.string t in
  let printable_ascii c =
    let code = Char.code c in
    code >= 0x20 && code <= 0x7E
  in
  let length = String.length tag in
  let fits = if padded then length >= 1 && length <= 4 else length = 4 in
  if (not fits) || not (String.for_all printable_ascii tag) then
    Cursor.err t
      (if padded then
         "OpenType tag must contain one to four printable ASCII characters"
       else "OpenType tag must contain exactly four printable ASCII characters");
  tag

let rec read_font_language_override t : font_language_override =
  Cursor.enum_or_calls "font-language-override"
    [
      ("normal", (Normal : font_language_override));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (read_var read_font_language_override t)) ]
    ~default:(fun t ->
      (String (read_opentype_tag ~padded:true t) : font_language_override))
    t

let rec pp_font_synthesis_style : font_synthesis_style Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Oblique_only -> Pp.string ctx "oblique-only"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_synthesis_style ctx v

let rec read_font_synthesis_style t : font_synthesis_style =
  Cursor.enum_or_var "font-synthesis-style"
    [
      ("auto", (Auto : font_synthesis_style));
      ("none", None);
      ("oblique-only", Oblique_only);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_synthesis_style t))
    t

let rec pp_font_synthesis_weight : font_synthesis_weight Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_synthesis_weight ctx v

let rec read_font_synthesis_weight t : font_synthesis_weight =
  Cursor.enum_or_var "font-synthesis-weight"
    [
      ("auto", (Auto : font_synthesis_weight));
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_synthesis_weight t))
    t

let rec pp_font_synthesis_small_caps : font_synthesis_small_caps Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_synthesis_small_caps ctx v

let rec read_font_synthesis_small_caps t : font_synthesis_small_caps =
  Cursor.enum_or_var "font-synthesis-small-caps"
    [
      ("auto", (Auto : font_synthesis_small_caps));
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_synthesis_small_caps t))
    t

let rec pp_font_synthesis_position : font_synthesis_position Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_synthesis_position ctx v

let rec read_font_synthesis_position t : font_synthesis_position =
  Cursor.enum_or_var "font-synthesis-position"
    [
      ("auto", (Auto : font_synthesis_position));
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_synthesis_position t))
    t

let pp_font_variant_ligature ctx = function
  | Common_ligatures -> Pp.string ctx "common-ligatures"
  | No_common_ligatures -> Pp.string ctx "no-common-ligatures"
  | Discretionary_ligatures -> Pp.string ctx "discretionary-ligatures"
  | No_discretionary_ligatures -> Pp.string ctx "no-discretionary-ligatures"
  | Historical_ligatures -> Pp.string ctx "historical-ligatures"
  | No_historical_ligatures -> Pp.string ctx "no-historical-ligatures"
  | Contextual -> Pp.string ctx "contextual"
  | No_contextual -> Pp.string ctx "no-contextual"

let rec pp_font_variant_ligatures : font_variant_ligatures Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | None -> Pp.string ctx "none"
  | Ligatures ligatures ->
      Pp.list ~sep:Pp.space pp_font_variant_ligature ctx ligatures
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_variant_ligatures ctx v

let read_font_variant_ligature t =
  Cursor.enum "font-variant-ligature"
    [
      ("common-ligatures", Common_ligatures);
      ("no-common-ligatures", No_common_ligatures);
      ("discretionary-ligatures", Discretionary_ligatures);
      ("no-discretionary-ligatures", No_discretionary_ligatures);
      ("historical-ligatures", Historical_ligatures);
      ("no-historical-ligatures", No_historical_ligatures);
      ("contextual", Contextual);
      ("no-contextual", No_contextual);
    ]
    t

let font_variant_ligature_slot = function
  | Common_ligatures | No_common_ligatures -> `Common
  | Discretionary_ligatures | No_discretionary_ligatures -> `Discretionary
  | Historical_ligatures | No_historical_ligatures -> `Historical
  | Contextual | No_contextual -> `Contextual

let has_duplicate_ligature_slot ligatures =
  List.exists
    (fun ligature ->
      let slot = font_variant_ligature_slot ligature in
      List.length
        (List.filter
           (fun other -> font_variant_ligature_slot other = slot)
           ligatures)
      > 1)
    ligatures

let rec read_font_variant_ligatures t : font_variant_ligatures =
  Cursor.enum_or_var "font-variant-ligatures"
    [
      ("normal", (Normal : font_variant_ligatures));
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_variant_ligatures t))
    ~default:(fun t ->
      match Cursor.many read_font_variant_ligature t with
      | [], _ -> Cursor.err_invalid t "font-variant-ligatures"
      | ligatures, _ ->
          if has_duplicate_ligature_slot ligatures then
            Cursor.err_invalid t "font-variant-ligatures";
          Ligatures ligatures)
    t

let rec pp_font_variant_caps : font_variant_caps Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Small_caps -> Pp.string ctx "small-caps"
  | All_small_caps -> Pp.string ctx "all-small-caps"
  | Petite_caps -> Pp.string ctx "petite-caps"
  | All_petite_caps -> Pp.string ctx "all-petite-caps"
  | Unicase -> Pp.string ctx "unicase"
  | Titling_caps -> Pp.string ctx "titling-caps"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_variant_caps ctx v

let pp_feature_value_names ctx names = Pp.list ~sep:Pp.comma pp_ident ctx names

let pp_font_variant_alternates_item ctx (item : font_variant_alternates_item) =
  let call name arg =
    Pp.string ctx name;
    Pp.char ctx '(';
    pp_ident ctx arg;
    Pp.char ctx ')'
  in
  let call_list name args =
    Pp.string ctx name;
    Pp.char ctx '(';
    pp_feature_value_names ctx args;
    Pp.char ctx ')'
  in
  match item with
  | Stylistic name -> call "stylistic" name
  | Historical_forms -> Pp.string ctx "historical-forms"
  | Styleset names -> call_list "styleset" names
  | Character_variant names -> call_list "character-variant" names
  | Swash name -> call "swash" name
  | Ornaments name -> call "ornaments" name
  | Annotation name -> call "annotation" name

let rec pp_font_variant_alternates : font_variant_alternates Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Alternates items ->
      Pp.list ~sep:Pp.space pp_font_variant_alternates_item ctx items
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_variant_alternates ctx v

(* A feature value name is a [<custom-ident>], so it is neither a CSS-wide
   keyword nor [default] (CSS Values 4 sec. 3.2). *)
let read_feature_value_name t =
  let name = Cursor.ident t in
  (match String.lowercase_ascii name with
  | "inherit" | "initial" | "unset" | "revert" | "revert-layer" | "default" ->
      Cursor.err_invalid t "font-variant-alternates reserved feature name"
  | _ -> ());
  name

let read_feature_value_names t =
  Cursor.list ~sep:Cursor.comma ~at_least:1 read_feature_value_name t

let read_font_variant_alternates_item t : font_variant_alternates_item =
  Cursor.ws t;
  match Cursor.peek_keyword t with
  | Some "historical-forms" ->
      let _ = Cursor.ident t in
      Historical_forms
  | _ ->
      Cursor.one_of
        [
          (fun t ->
            Cursor.call "stylistic" t (fun t ->
                Stylistic (read_feature_value_name t)));
          (fun t ->
            Cursor.call "styleset" t (fun t ->
                Styleset (read_feature_value_names t)));
          (fun t ->
            Cursor.call "character-variant" t (fun t ->
                Character_variant (read_feature_value_names t)));
          (fun t ->
            Cursor.call "swash" t (fun t -> Swash (read_feature_value_name t)));
          (fun t ->
            Cursor.call "ornaments" t (fun t ->
                Ornaments (read_feature_value_name t)));
          (fun t ->
            Cursor.call "annotation" t (fun t ->
                Annotation (read_feature_value_name t)));
        ]
        t

(* [||] takes one or more of its options, each at most once. *)
let font_variant_alternates_tag : font_variant_alternates_item -> int = function
  | Stylistic _ -> 0
  | Historical_forms -> 1
  | Styleset _ -> 2
  | Character_variant _ -> 3
  | Swash _ -> 4
  | Ornaments _ -> 5
  | Annotation _ -> 6

let read_font_variant_alternates_items t =
  let items =
    Cursor.list ~sep:Cursor.ws ~at_least:1 read_font_variant_alternates_item t
  in
  let tags = List.map font_variant_alternates_tag items in
  if List.length (List.sort_uniq compare tags) <> List.length tags then
    Cursor.err_invalid t "font-variant-alternates duplicate component";
  (Alternates items : font_variant_alternates)

let rec read_font_variant_alternates t : font_variant_alternates =
  Cursor.enum_or_var "font-variant-alternates"
    [
      ("normal", (Normal : font_variant_alternates));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_variant_alternates t))
    ~default:read_font_variant_alternates_items t

let rec read_font_variant_caps t : font_variant_caps =
  Cursor.enum_or_var "font-variant-caps"
    [
      ("normal", (Normal : font_variant_caps));
      ("small-caps", Small_caps);
      ("all-small-caps", All_small_caps);
      ("petite-caps", Petite_caps);
      ("all-petite-caps", All_petite_caps);
      ("unicase", Unicase);
      ("titling-caps", Titling_caps);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_variant_caps t))
    t

let rec pp_font_variant_position : font_variant_position Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Sub -> Pp.string ctx "sub"
  | Super -> Pp.string ctx "super"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_variant_position ctx v

let rec read_font_variant_position t : font_variant_position =
  Cursor.enum_or_var "font-variant-position"
    [
      ("normal", (Normal : font_variant_position));
      ("sub", Sub);
      ("super", Super);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_variant_position t))
    t

let pp_east_asian_feature ctx = function
  | Jis78 -> Pp.string ctx "jis78"
  | Jis83 -> Pp.string ctx "jis83"
  | Jis90 -> Pp.string ctx "jis90"
  | Jis04 -> Pp.string ctx "jis04"
  | Simplified -> Pp.string ctx "simplified"
  | Traditional -> Pp.string ctx "traditional"
  | Full_width -> Pp.string ctx "full-width"
  | Proportional_width -> Pp.string ctx "proportional-width"
  | Ruby -> Pp.string ctx "ruby"

let rec pp_font_variant_east_asian : font_variant_east_asian Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Features features ->
      Pp.list ~sep:Pp.space pp_east_asian_feature ctx features
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_variant_east_asian ctx v

let read_east_asian_feature t =
  Cursor.enum "font-variant-east-asian-feature"
    [
      ("jis78", Jis78);
      ("jis83", Jis83);
      ("jis90", Jis90);
      ("jis04", Jis04);
      ("simplified", Simplified);
      ("traditional", Traditional);
      ("full-width", Full_width);
      ("proportional-width", Proportional_width);
      ("ruby", Ruby);
    ]
    t

(* sec. 6.5 gives the property one variant option, one width option and [ruby],
   each at most once. *)
let invalid_east_asian_feature_set features =
  let variant_count = ref 0 in
  let width_count = ref 0 in
  let seen = ref [] in
  List.exists
    (fun feature ->
      let duplicate = List.mem feature !seen in
      seen := feature :: !seen;
      (match feature with
      | Jis78 | Jis83 | Jis90 | Jis04 | Simplified | Traditional ->
          incr variant_count
      | Full_width | Proportional_width -> incr width_count
      | Ruby -> ());
      duplicate || !variant_count > 1 || !width_count > 1)
    features

let rec read_font_variant_east_asian t : font_variant_east_asian =
  let invalid_feature_set = invalid_east_asian_feature_set in
  Cursor.enum_or_var "font-variant-east-asian"
    [
      ("normal", (Normal : font_variant_east_asian));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_variant_east_asian t))
    ~default:(fun t ->
      match Cursor.many read_east_asian_feature t with
      | [], _ -> Cursor.err_invalid t "font-variant-east-asian"
      | features, _ when invalid_feature_set features ->
          Cursor.err_invalid t "font-variant-east-asian"
      | features, _ -> (Features features : font_variant_east_asian))
    t

let pp_font_feature_value ctx (value : font_feature_value) =
  match value with
  | On -> Pp.string ctx "on"
  | Off -> Pp.string ctx "off"
  | Index value -> Pp.int ctx value

let pp_font_feature_setting ctx ({ tag; value } : font_feature_setting) =
  Pp.quoted_string ctx tag;
  match value with
  | None -> ()
  | Some value ->
      Pp.space ctx ();
      pp_font_feature_value ctx value

let rec pp_font_feature_settings : font_feature_settings Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Feature_list settings ->
      Pp.list ~sep:Pp.comma pp_font_feature_setting ctx settings
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_feature_settings ctx v

let pp_font_variation_setting ctx ({ tag; value } : font_variation_setting) =
  Pp.quoted_string ctx tag;
  Pp.space ctx ();
  Pp.float ctx value

let rec pp_font_variation_settings : font_variation_settings Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Axis_list settings ->
      Pp.list ~sep:Pp.comma pp_font_variation_setting ctx settings
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_variation_settings ctx v

let rec pp_font_palette : font_palette Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_font_palette ctx v
  | Normal -> Pp.string ctx "normal"
  | Light -> Pp.string ctx "light"
  | Dark -> Pp.string ctx "dark"
  | Palette name -> pp_ident ctx name
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_font_synthesis_feature : font_synthesis_feature Pp.t =
 fun ctx -> function
  | Weight -> Pp.string ctx "weight"
  | Style -> Pp.string ctx "style"
  | Small_caps -> Pp.string ctx "small-caps"
  | Position -> Pp.string ctx "position"

let rec pp_font_synthesis : font_synthesis Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_font_synthesis ctx v
  | None -> Pp.string ctx "none"
  | Features features ->
      Pp.list ~sep:Pp.space pp_font_synthesis_feature ctx features
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_font_variant_emoji : font_variant_emoji Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Text -> Pp.string ctx "text"
  | Emoji -> Pp.string ctx "emoji"
  | Unicode -> Pp.string ctx "unicode"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_variant_emoji ctx v

let rec pp_font_variant_numeric_token : font_variant_numeric_token Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Lining_nums -> Pp.string ctx "lining-nums"
  | Oldstyle_nums -> Pp.string ctx "oldstyle-nums"
  | Proportional_nums -> Pp.string ctx "proportional-nums"
  | Tabular_nums -> Pp.string ctx "tabular-nums"
  | Diagonal_fractions -> Pp.string ctx "diagonal-fractions"
  | Stacked_fractions -> Pp.string ctx "stacked-fractions"
  | Ordinal -> Pp.string ctx "ordinal"
  | Slashed_zero -> Pp.string ctx "slashed-zero"
  | Var v -> pp_var pp_font_variant_numeric_token ctx v

let rec pp_font_variant_numeric : font_variant_numeric Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Tokens tokens ->
      Pp.list ~sep:Pp.space pp_font_variant_numeric_token ctx tokens
  | Var v -> pp_var pp_font_variant_numeric ctx v
  | Composed
      {
        ordinal;
        slashed_zero;
        numeric_figure;
        numeric_spacing;
        numeric_fraction;
      } ->
      (* Print all 5 variables, including None values The Empty fallback in vars
         will produce var(--name,) *)
      let tokens =
        List.filter_map Fun.id
          [
            ordinal;
            slashed_zero;
            numeric_figure;
            numeric_spacing;
            numeric_fraction;
          ]
      in
      Pp.list ~sep:Pp.space pp_font_variant_numeric_token ctx tokens

let rec read_font_palette (t : Cursor.t) : font_palette =
  let keywords : (string * font_palette) list =
    [
      ("normal", Normal);
      ("light", Light);
      ("dark", Dark);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "font-palette" keywords
     ~var:(fun t -> (Var (Values.read_var read_font_palette t) : font_palette))
     ~default:(fun t -> (Palette (read_dashed_ident t) : font_palette))
     t
    : font_palette)

let read_font_synthesis_feature t : font_synthesis_feature =
  Cursor.enum "font-synthesis feature"
    [
      ("weight", Weight);
      ("style", Style);
      ("small-caps", Small_caps);
      ("position", Position);
    ]
    t

let rec read_font_synthesis (t : Cursor.t) : font_synthesis =
  let keywords : (string * font_synthesis) list =
    [
      ("none", None);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  let read_features t =
    let features =
      Cursor.list ~sep:Cursor.ws ~at_least:1 read_font_synthesis_feature t
    in
    let rec duplicates = function
      | [] -> false
      | x :: xs -> List.mem x xs || duplicates xs
    in
    if duplicates features then
      Cursor.err_invalid t "duplicate font-synthesis feature";
    (Features features : font_synthesis)
  in
  (Cursor.enum_or_var "font-synthesis" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_font_synthesis t) : font_synthesis))
     ~default:read_features t
    : font_synthesis)

let rec read_font_variant_numeric_token t : font_variant_numeric_token =
  let read_var t : font_variant_numeric_token =
    Var (read_var read_font_variant_numeric_token t)
  in
  Cursor.enum_or_var "font-variant-numeric-token"
    [
      ("normal", (Normal : font_variant_numeric_token));
      ("lining-nums", Lining_nums);
      ("oldstyle-nums", Oldstyle_nums);
      ("proportional-nums", Proportional_nums);
      ("tabular-nums", Tabular_nums);
      ("diagonal-fractions", Diagonal_fractions);
      ("stacked-fractions", Stacked_fractions);
      ("ordinal", Ordinal);
      ("slashed-zero", Slashed_zero);
    ]
    ~var:read_var t

let font_variant_numeric_token_family : font_variant_numeric_token -> _ =
  function
  | Lining_nums | Oldstyle_nums -> `Numeric_figure
  | Proportional_nums | Tabular_nums -> `Numeric_spacing
  | Diagonal_fractions | Stacked_fractions -> `Numeric_fraction
  | Ordinal -> `Ordinal
  | Slashed_zero -> `Slashed_zero
  | Normal | Var _ -> `Other

let numeric_token_is_normal : font_variant_numeric_token -> bool = function
  | Normal -> true
  | _ -> false

let reject_duplicate_numeric_families t
    (tokens : font_variant_numeric_token list) =
  let seen = Hashtbl.create 5 in
  List.iter
    (fun token ->
      match font_variant_numeric_token_family token with
      | `Other -> ()
      | family ->
          if Hashtbl.mem seen family then
            err_invalid_value t "font-variant-numeric" "duplicate token";
          Hashtbl.add seen family ())
    tokens

let read_font_variant_numeric_tokens t : font_variant_numeric =
  let tokens, _ = Cursor.many read_font_variant_numeric_token t in
  match tokens with
  | [] -> err_invalid_value t "font-variant-numeric" "<empty>"
  | tokens ->
      (* CSS Fonts 4 section 6.7: [normal] resets all sub-properties and must
         stand alone; it can't be mixed with other numeric tokens. *)
      if List.exists numeric_token_is_normal tokens && List.length tokens > 1
      then
        err_invalid_value t "font-variant-numeric"
          "[normal] cannot be mixed with other tokens";
      reject_duplicate_numeric_families t tokens;
      Tokens tokens

let rec read_font_variant_numeric t : font_variant_numeric =
  Cursor.enum_or_var "font-variant-numeric"
    [
      ("normal", (Normal : font_variant_numeric));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_variant_numeric t))
    ~default:read_font_variant_numeric_tokens t

(* CSS Fonts 4 (ED) sec. 6.10: the shorthand's components are the seven
   longhands' own values, so each component reads through its longhand's item
   reader and lands in that longhand's slot. [||] takes each option at most
   once, which the slot being filled already says. *)
let empty_font_variant_shorthand : font_variant_shorthand =
  {
    ligatures = [];
    alternates = [];
    caps = Option.none;
    numeric = [];
    east_asian = [];
    position = Option.none;
    emoji = Option.none;
  }

let font_variant_is_empty (v : font_variant_shorthand) =
  v.ligatures = [] && v.alternates = [] && Option.is_none v.caps
  && v.numeric = [] && v.east_asian = [] && Option.is_none v.position
  && Option.is_none v.emoji

let read_font_variant_caps_component t : font_variant_caps =
  Cursor.enum "font-variant caps"
    [
      ("small-caps", (Small_caps : font_variant_caps));
      ("all-small-caps", All_small_caps);
      ("petite-caps", Petite_caps);
      ("all-petite-caps", All_petite_caps);
      ("unicase", Unicase);
      ("titling-caps", Titling_caps);
    ]
    t

let read_font_variant_position_component t : font_variant_position =
  Cursor.enum "font-variant position"
    [ ("sub", (Sub : font_variant_position)); ("super", Super) ]
    t

let read_font_variant_emoji_component t : font_variant_emoji =
  Cursor.enum "font-variant emoji"
    [
      ("text", (Text : font_variant_emoji));
      ("emoji", Emoji);
      ("unicode", Unicode);
    ]
    t

let font_variant_component acc t =
  let once what = function
    | Option.Some _ -> Cursor.err_invalid t what
    | Option.None -> ()
  in
  Cursor.one_of
    [
      (fun t ->
        let ligature = read_font_variant_ligature t in
        let ligatures = acc.ligatures @ [ ligature ] in
        if has_duplicate_ligature_slot ligatures then
          Cursor.err_invalid t "font-variant duplicate ligature";
        { acc with ligatures });
      (fun t ->
        let item = read_font_variant_alternates_item t in
        let alternates = acc.alternates @ [ item ] in
        let tags = List.map font_variant_alternates_tag alternates in
        if List.length (List.sort_uniq compare tags) <> List.length tags then
          Cursor.err_invalid t "font-variant duplicate alternate";
        { acc with alternates });
      (fun t ->
        let caps = read_font_variant_caps_component t in
        once "font-variant duplicate caps" acc.caps;
        { acc with caps = Some caps });
      (fun t ->
        let token = read_font_variant_numeric_token t in
        let numeric = acc.numeric @ [ token ] in
        reject_duplicate_numeric_families t numeric;
        { acc with numeric });
      (fun t ->
        let feature = read_east_asian_feature t in
        let east_asian = acc.east_asian @ [ feature ] in
        if invalid_east_asian_feature_set east_asian then
          Cursor.err_invalid t "font-variant duplicate east-asian";
        { acc with east_asian });
      (fun t ->
        let position = read_font_variant_position_component t in
        once "font-variant duplicate position" acc.position;
        { acc with position = Some position });
      (fun t ->
        let emoji = read_font_variant_emoji_component t in
        once "font-variant duplicate emoji" acc.emoji;
        { acc with emoji = Some emoji });
    ]
    t

let read_font_variant_components t : font_variant =
  let rec loop acc =
    Cursor.ws t;
    if Cursor.is_done t then acc
    else
      match Cursor.option (font_variant_component acc) t with
      | Some acc -> loop acc
      | Option.None -> acc
  in
  let value = loop empty_font_variant_shorthand in
  if font_variant_is_empty value then
    Cursor.err_expected t "font-variant component";
  Shorthand value

let rec read_font_variant t : font_variant =
  Cursor.enum_or_var "font-variant"
    [
      ("normal", (Normal : font_variant));
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_variant t))
    ~default:read_font_variant_components t

let pp_font_variant_shorthand ctx (v : font_variant_shorthand) =
  let first = ref true in
  let sep () = if !first then first := false else Pp.space ctx () in
  let item pp x =
    sep ();
    pp ctx x
  in
  List.iter (item pp_font_variant_ligature) v.ligatures;
  List.iter (item pp_font_variant_alternates_item) v.alternates;
  Option.iter (item pp_font_variant_caps) v.caps;
  List.iter (item pp_font_variant_numeric_token) v.numeric;
  List.iter (item pp_east_asian_feature) v.east_asian;
  Option.iter (item pp_font_variant_position) v.position;
  Option.iter (item pp_font_variant_emoji) v.emoji;
  (* Every slot empty declares the seven initials, which is what [normal]
     declares; the empty string is not a value any parser reads back. *)
  if !first then Pp.string ctx "normal"

let rec pp_font_variant : font_variant Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | None -> Pp.string ctx "none"
  | Shorthand v -> pp_font_variant_shorthand ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_variant ctx v

let read_font_feature_value t : font_feature_value =
  match Cursor.option Cursor.int t with
  | Some value ->
      if value < 0 then
        Cursor.err t "font-feature-settings value must be non-negative";
      Index value
  | None ->
      Cursor.enum "font-feature-settings value"
        [ ("on", (On : font_feature_value)); ("off", Off) ]
        t

let read_font_feature_setting t : font_feature_setting =
  let tag = read_opentype_tag t in
  Cursor.ws t;
  let value =
    match Cursor.peek t with
    | Some
        (Component.Preserved { kind = Token.Number_tok _ | Token.Ident _; _ })
      ->
        Some (read_font_feature_value t)
    | _ -> None
  in
  { tag; value }

let rec read_font_feature_settings t : font_feature_settings =
  let read_var t : font_feature_settings =
    Var (read_var read_font_feature_settings t)
  in
  let read_feature_list t =
    let items =
      Cursor.list ~sep:Cursor.comma ~at_least:1 read_font_feature_setting t
    in
    Feature_list items
  in
  Cursor.enum_or_calls "font-feature-settings"
    [
      ("normal", (Normal : font_feature_settings));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:read_feature_list t

let read_font_variation_setting t : font_variation_setting =
  let tag = read_opentype_tag t in
  Cursor.ws t;
  let value = Cursor.number t in
  { tag; value }

let rec read_font_variation_settings t : font_variation_settings =
  let read_var t : font_variation_settings =
    Var (read_var read_font_variation_settings t)
  in
  let read_axis_list t =
    let items =
      Cursor.list ~sep:Cursor.comma ~at_least:1 read_font_variation_setting t
    in
    Axis_list items
  in
  Cursor.enum_or_calls "font-variation-settings"
    [
      ("normal", (Normal : font_variation_settings));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:read_axis_list t

let rec read_font_variant_emoji t : font_variant_emoji =
  Cursor.enum_or_var "font-variant-emoji"
    [
      ("normal", (Normal : font_variant_emoji));
      ("text", Text);
      ("emoji", Emoji);
      ("unicode", Unicode);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_font_variant_emoji t))
    t
