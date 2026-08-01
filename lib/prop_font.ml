(* css-fonts-4: the font shorthand and every font-* longhand, font-variant and
   font-synthesis, feature and variation settings, font-family, line-height,
   unicode-range, @font-face src and display, and the vendor font-smoothing
   properties.

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name here stays visible under [Css.Properties] exactly as when
   it lived in properties.ml. *)

open Common
open Values
open Syntax
open Properties_intf
open Prop_common

let read_line_height_length t : line_height =
  let n, repr, unit = Cursor.number_repr_with_unit t in
  let n, repr = normalize_signed_zero n repr in
  if n < 0. then Cursor.err_invalid t "line-height cannot be negative"
  else
    let authored () : line_height = Number { value = n; unit; repr } in
    match unit with
    | Some "px" -> if Pp.string_of_float n = repr then Px n else authored ()
    | Some "rem" -> if Pp.string_of_float n = repr then Rem n else authored ()
    | Some "em" -> if Pp.string_of_float n = repr then Em n else authored ()
    | Some "%" -> if Pp.string_of_float n = repr then Pct n else authored ()
    | None -> if Pp.string_of_float n = repr then Num n else authored ()
    | Some _ -> authored ()

let rec numeric_line_height_calc_leaves : line_height calc -> line_height calc =
  function
  | Val (Num n) | Val (Number { value = n; unit = None; _ }) -> Num n
  | Nested inner -> Nested (numeric_line_height_calc_leaves inner)
  | Parens inner -> Parens (numeric_line_height_calc_leaves inner)
  | Expr (left, op, right) ->
      Expr
        ( numeric_line_height_calc_leaves left,
          op,
          numeric_line_height_calc_leaves right )
  | leaf -> leaf

let rec read_font_weight t : font_weight =
  let read_var t : font_weight = Var (read_var read_font_weight t) in
  Cursor.ws t;
  Cursor.enum_or_calls "font-weight"
    [
      ("normal", Normal);
      ("bold", Bold);
      ("bolder", Bolder);
      ("lighter", Lighter);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:(fun t ->
      let n = Cursor.number t in
      let weight = int_of_float n in
      if weight >= 1 && weight <= 1000 then (Weight weight : font_weight)
      else err_invalid_value t "font-weight" (string_of_int weight))
    t

let rec read_font_style t : font_style =
  Cursor.enum_or_var "font-style"
    [
      ("normal", (Normal : font_style));
      ("italic", (Italic : font_style));
      ("inherit", (Inherit : font_style));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_style t))
    ~default:(fun t ->
      Cursor.expect_string "oblique" t;
      Cursor.ws t;
      if Cursor.is_done t || Cursor.peek_semicolon t then Oblique
      else
        let first = read_angle t in
        Cursor.ws t;
        if Cursor.is_done t || Cursor.peek_semicolon t then Oblique_angle first
        else
          (* CSS Fonts 4 sec. 11.2 wants the first oblique angle <= the second.
             Browsers keep a descending range ([oblique 20deg 10deg]), so accept
             it but warn, leaving [Css.of_string ~strict] free to reject it. *)
          let second = read_angle t in
          (match (angle_degrees_opt first, angle_degrees_opt second) with
          | Some a, Some b when a > b ->
              Cursor.push_warning t
                (Error.bad_value (Cursor.position t) ~property:"font-style"
                   ~reason:
                     "oblique angle range must run from the smaller angle to \
                      the larger (CSS Fonts 4 \u{00a7}11.2)")
          | _ -> ());
          Oblique_range (first, second))
    t

let rec read_font_size t : font_size =
  let read_var t : font_size = Var (read_var read_font_size t) in
  let read_calc t : font_size = Calc (read_calc read_font_size t) in
  let read_length t : font_size =
    let len = read_non_negative_length ~with_keywords:false t in
    Length len
  in
  let read_pct t : font_size =
    let n = Cursor.number t in
    if n < 0. then Cursor.err_invalid t "negative font-size percentage";
    Cursor.expect '%' t;
    Pct n
  in
  Cursor.enum_or_calls "font-size"
    [
      ("xx-small", (Xx_small : font_size));
      ("x-small", X_small);
      ("small", Small);
      ("medium", Medium);
      ("large", Large);
      ("x-large", X_large);
      ("xx-large", Xx_large);
      ("xxx-large", Xxx_large);
      ("larger", Larger);
      ("smaller", Smaller);
      ("math", Math);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var); ("calc", read_calc) ]
    ~default:(fun t ->
      (* Try percentage first, then length *)
      Cursor.one_of [ read_pct; read_length ] t)
    t

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
    ~default:(fun t -> (String (Cursor.string t) : font_language_override))
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

let rec read_font_variant_east_asian t : font_variant_east_asian =
  let invalid_feature_set features =
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
  in
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

let is_font_family_ident_word s =
  let len = String.length s in
  let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false in
  let is_digit = function '0' .. '9' -> true | _ -> false in
  let is_ident_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true
    | _ -> false
  in
  len > 0
  && (is_alpha s.[0] || s.[0] = '_' || s.[0] = '-')
  && (not (len >= 2 && s.[0] = '-' && is_digit s.[1]))
  && String.for_all is_ident_char s

let pp_font_family_name ctx s =
  (* A multi-word named family unquotes under minify (shorter, valid), but the
     CSSOM-canonical serialization quotes it, so enforce_spec keeps the
     quotes. *)
  if Pp.minified ctx && not ctx.Pp.enforce_spec then Pp.string ctx s
  else (
    Pp.char ctx '"';
    Pp.string ctx s;
    Pp.char ctx '"')

let can_unquote_font_family_name s =
  match String.split_on_char ' ' s with
  | _ :: _ :: _ as words ->
      (* CSS Fonts 4 sec. 4.1: a [<family-name>] formed of two or more
         [<custom-ident>]s is unambiguous - none of its words can be picked up
         as a property-level CSS-wide keyword once the parser has committed to a
         multi-token value. So [inherit test] / [revert serif] etc. round-trip
         unquoted, just like [Times New Roman]. *)
      List.for_all is_font_family_ident_word words
  | _ -> false

(* Walk a component stream and rewrite each [<string>] token whose content is a
   multi-word identifier sequence (the [can_unquote_font_family_name] guard) as
   an explicit [<ident>] sequence. Used by the [@property]-registered custom
   property promotion path when the registered syntax accepts [<custom-ident>+]
   - the two forms ([custom-ident>+] vs [<string>]) are spec-equivalent there
   (CSS Fonts 4 sec. 15.3), so the rewrite produces a single canonical AST. The
   guard's "two or more words" rule avoids the CSS-wide-keyword trap (a quoted
   ["inherit"] never collapses to the bare keyword). *)
let unquote_font_family_strings components =
  let changed = ref false in
  let words_of s =
    String.split_on_char ' ' s |> List.filter (fun w -> w <> "")
  in
  let rec interleave loc = function
    | [] -> []
    | [ w ] -> [ Component.Preserved (Token.v ~kind:(Token.Ident w) ~loc) ]
    | w :: rest ->
        Component.Preserved (Token.v ~kind:(Token.Ident w) ~loc)
        :: Component.Preserved (Token.v ~kind:Token.Whitespace ~loc)
        :: interleave loc rest
  in
  let result =
    List.concat_map
      (fun c ->
        match c with
        | Component.Preserved { kind = Token.String { value; _ }; loc }
          when can_unquote_font_family_name value ->
            changed := true;
            interleave loc (words_of value)
        | _ -> [ c ])
      components
  in
  if !changed then result else components

let is_generic_family : font_family -> bool = function
  | Sans_serif | Serif | Monospace | Cursive | Fantasy | System_ui
  | Ui_sans_serif | Ui_serif | Ui_monospace | Ui_rounded | Emoji | Math
  | Fangsong ->
      true
  | _ -> false

let rec pp_font_family : font_family Pp.t =
 fun ctx -> function
  (* Generic CSS font families *)
  | Sans_serif -> Pp.string ctx "sans-serif"
  | Serif -> Pp.string ctx "serif"
  | Monospace -> Pp.string ctx "monospace"
  | Cursive -> Pp.string ctx "cursive"
  | Fantasy -> Pp.string ctx "fantasy"
  | System_ui -> Pp.string ctx "system-ui"
  | Ui_sans_serif -> Pp.string ctx "ui-sans-serif"
  | Ui_serif -> Pp.string ctx "ui-serif"
  | Ui_monospace -> Pp.string ctx "ui-monospace"
  | Ui_rounded -> Pp.string ctx "ui-rounded"
  | Emoji -> Pp.string ctx "emoji"
  | Math -> Pp.string ctx "math"
  | Fangsong -> Pp.string ctx "fangsong"
  (* Popular web fonts *)
  | Inter -> Pp.string ctx "Inter"
  | Roboto -> Pp.string ctx "Roboto"
  | Open_sans -> Pp.string ctx "\"Open Sans\""
  | Lato -> Pp.string ctx "Lato"
  | Montserrat -> Pp.string ctx "Montserrat"
  | Poppins -> Pp.string ctx "Poppins"
  | Source_sans_pro -> Pp.string ctx "\"Source Sans Pro\""
  | Raleway -> Pp.string ctx "Raleway"
  | Oswald -> Pp.string ctx "Oswald"
  | Noto_sans -> Pp.string ctx "\"Noto Sans\""
  | Ubuntu -> Pp.string ctx "Ubuntu"
  | Playfair_display -> Pp.string ctx "\"Playfair Display\""
  | Merriweather -> Pp.string ctx "Merriweather"
  | Lora -> Pp.string ctx "Lora"
  | PT_sans -> Pp.string ctx "\"PT Sans\""
  | PT_serif -> Pp.string ctx "\"PT Serif\""
  | Nunito -> Pp.string ctx "Nunito"
  | Nunito_sans -> Pp.string ctx "\"Nunito Sans\""
  | Work_sans -> Pp.string ctx "\"Work Sans\""
  | Rubik -> Pp.string ctx "Rubik"
  | Fira_sans -> Pp.string ctx "\"Fira Sans\""
  | Fira_code -> Pp.string ctx "\"Fira Code\""
  | JetBrains_mono -> Pp.string ctx "\"JetBrains Mono\""
  | IBM_plex_sans -> Pp.string ctx "\"IBM Plex Sans\""
  | IBM_plex_serif -> Pp.string ctx "\"IBM Plex Serif\""
  | IBM_plex_mono -> Pp.string ctx "\"IBM Plex Mono\""
  | Source_code_pro -> Pp.string ctx "\"Source Code Pro\""
  | Space_mono -> Pp.string ctx "\"Space Mono\""
  | DM_sans -> Pp.string ctx "\"DM Sans\""
  | DM_serif_display -> Pp.string ctx "\"DM Serif Display\""
  | Bebas_neue -> Pp.string ctx "\"Bebas Neue\""
  | Barlow -> Pp.string ctx "Barlow"
  | Mulish -> Pp.string ctx "Mulish"
  | Josefin_sans -> Pp.string ctx "\"Josefin Sans\""
  (* Platform-specific fonts. Multi-word names emit unquoted under minify (CSS
     Fonts 4 sec. 4.1: a [<family-name>] of two or more [<custom-ident>] words
     parses without quotes and is the shorter spelling). Pretty mode keeps the
     quoted form for readability. *)
  | Helvetica -> Pp.string ctx "Helvetica"
  | Helvetica_neue -> pp_font_family_name ctx "Helvetica Neue"
  | Arial -> Pp.string ctx "Arial"
  | Verdana -> Pp.string ctx "Verdana"
  | Tahoma -> Pp.string ctx "Tahoma"
  | Trebuchet_ms -> pp_font_family_name ctx "Trebuchet MS"
  | Times_new_roman -> pp_font_family_name ctx "Times New Roman"
  | Times -> Pp.string ctx "Times"
  | Georgia -> Pp.string ctx "Georgia"
  | Cambria -> Pp.string ctx "Cambria"
  | Garamond -> Pp.string ctx "Garamond"
  | Courier_new -> pp_font_family_name ctx "Courier New"
  | Courier -> Pp.string ctx "Courier"
  | Lucida_console -> pp_font_family_name ctx "Lucida Console"
  | SF_pro -> pp_font_family_name ctx "SF Pro"
  | SF_pro_display -> pp_font_family_name ctx "SF Pro Display"
  | SF_pro_text -> pp_font_family_name ctx "SF Pro Text"
  | SF_mono -> pp_font_family_name ctx "SF Mono"
  | NY -> pp_font_family_name ctx "New York"
  | Segoe_ui -> pp_font_family_name ctx "Segoe UI"
  | Segoe_ui_emoji -> pp_font_family_name ctx "Segoe UI Emoji"
  | Segoe_ui_symbol -> pp_font_family_name ctx "Segoe UI Symbol"
  | Apple_color_emoji -> pp_font_family_name ctx "Apple Color Emoji"
  | Noto_color_emoji -> pp_font_family_name ctx "Noto Color Emoji"
  | Android_emoji -> pp_font_family_name ctx "Android Emoji"
  | Twemoji_mozilla -> pp_font_family_name ctx "Twemoji Mozilla"
  (* Developer fonts *)
  | Menlo -> Pp.string ctx "Menlo"
  | Monaco -> Pp.string ctx "Monaco"
  | Consolas -> Pp.string ctx "Consolas"
  | Liberation_mono -> pp_font_family_name ctx "Liberation Mono"
  | SFMono_regular -> Pp.string ctx "SFMono-Regular"
  | Cascadia_code -> pp_font_family_name ctx "Cascadia Code"
  | Cascadia_mono -> pp_font_family_name ctx "Cascadia Mono"
  | Victor_mono -> pp_font_family_name ctx "Victor Mono"
  | Inconsolata -> Pp.string ctx "Inconsolata"
  | Hack -> Pp.string ctx "Hack"
  (* CSS keywords *)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Name s ->
      let safe_ident_char = function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true
        | _ -> false
      in
      (* A single-word [Name] that matches a generic family / CSS-wide keyword
         must stay quoted - dropping the quotes turns the [<family-name>] into
         the generic keyword (different semantics in [@font-face] and in
         [font-family] cascade). *)
      let collides_with_keyword =
        List.mem (String.lowercase_ascii s)
          [
            "serif";
            "sans-serif";
            "monospace";
            "cursive";
            "fantasy";
            "system-ui";
            "ui-serif";
            "ui-sans-serif";
            "ui-monospace";
            "ui-rounded";
            "emoji";
            "math";
            "fangsong";
            "inherit";
            "initial";
            "unset";
            "revert";
            "revert-layer";
            "none";
            "default";
          ]
      in
      if
        Pp.minified ctx && (not ctx.Pp.enforce_spec)
        && can_unquote_font_family_name s
      then Pp.string ctx s
      else if
        s = ""
        || (not (String.for_all safe_ident_char s))
        || collides_with_keyword
      then Pp.quoted_string ctx s
      else Pp.string ctx s
  | Var v -> pp_var pp_font_family ctx v
  | List fonts ->
      let level_chars =
        match ctx.Pp.indent with Some w -> w * ctx.Pp.level | None -> 0
      in
      (* CSS Fonts 4 sec. 4.1: [font-family] is a fallback list, so a duplicate
         entry never wins under cascade resolution - drop it under minify (the
         first occurrence keeps the source position). A bare generic keyword
         (notably [monospace]) takes the UA generic-font size, so the
         [monospace, monospace] idiom opts back into the normal size; a dedup
         must not collapse a list to a single generic, which would shrink the
         text. *)
      let fonts =
        if Pp.minified ctx then
          let seen = Hashtbl.create 8 in
          let deduped =
            List.filter
              (fun f ->
                let key = Pp.to_string ~minify:true pp_font_family f in
                if Hashtbl.mem seen key then false
                else (
                  Hashtbl.add seen key ();
                  true))
              fonts
          in
          match deduped with
          | [ single ] when is_generic_family single && List.length fonts > 1 ->
              fonts
          | _ -> deduped
        else fonts
      in
      Pp.list_wrap ~threshold:90 ~sep:Pp.comma ~wrap_indent:(level_chars + 2)
        pp_font_family ctx fonts
  | Invalid tokens ->
      let rendered =
        if Pp.minified ctx then Parser.to_string_minified tokens
        else Parser.string_of_components tokens
      in
      Pp.string ctx rendered

let normalize_font_style : font_style -> font_style =
  let na = Values.normalize_angle in
  fun value ->
    match value with
    | Oblique_angle a -> preserve_if_equal value (Oblique_angle (na a))
    | Oblique_range (a, b) ->
        preserve_if_equal value (Oblique_range (na a, na b))
    | other -> other

let rec pp_font_style : font_style Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_font_style ctx v
  | Normal -> Pp.string ctx "normal"
  | Italic -> Pp.string ctx "italic"
  | Oblique -> Pp.string ctx "oblique"
  | Oblique_angle angle ->
      Pp.string ctx "oblique";
      Pp.space ctx ();
      pp_angle ctx angle
  | Oblique_range (first, second) ->
      Pp.string ctx "oblique";
      Pp.space ctx ();
      pp_angle ctx first;
      Pp.space ctx ();
      pp_angle ctx second
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_font_feature_settings : font_feature_settings Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Feature_list s ->
      (* Feature list contains quoted tags already in the stored string *)
      Pp.string ctx s
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | String s -> Pp.quoted_string ctx s
  | Var v -> pp_var pp_font_feature_settings ctx v

(* Collapse the optional whitespace after each axis-separator comma in a
   [font-variation-settings] list. Commas inside a quoted axis tag (a 4-char tag
   may contain U+2C) are left untouched. *)
let minify_axis_list s =
  let buf = Buffer.create (String.length s) in
  let n = String.length s in
  let in_quote = ref false in
  let i = ref 0 in
  while !i < n do
    let c = s.[!i] in
    if !in_quote then (
      Buffer.add_char buf c;
      if c = '"' then in_quote := false;
      incr i)
    else if c = '"' then (
      Buffer.add_char buf c;
      in_quote := true;
      incr i)
    else if c = ',' then (
      Buffer.add_char buf ',';
      incr i;
      while !i < n && s.[!i] = ' ' do
        incr i
      done)
    else (
      Buffer.add_char buf c;
      incr i)
  done;
  Buffer.contents buf

let rec pp_font_variation_settings : font_variation_settings Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Axis_list s ->
      Pp.string ctx (if Pp.minified ctx then minify_axis_list s else s)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | String s -> Pp.quoted_string ctx s
  | Var v -> pp_var pp_font_variation_settings ctx v

let rec pp_font_palette : font_palette Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_font_palette ctx v
  | Normal -> Pp.string ctx "normal"
  | Light -> Pp.string ctx "light"
  | Dark -> Pp.string ctx "dark"
  | Palette name -> Pp.string ctx name
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

(* CSS Fonts 4 sec. 5.3 defines each keyword as a percentage, and the percentage
   is never longer, so minified output uses it. *)
let font_stretch_pct = function
  | Ultra_condensed -> Some 50.
  | Extra_condensed -> Some 62.5
  | Condensed -> Some 75.
  | Semi_condensed -> Some 87.5
  | Normal -> Some 100.
  | Semi_expanded -> Some 112.5
  | Expanded -> Some 125.
  | Extra_expanded -> Some 150.
  | Ultra_expanded -> Some 200.
  | _ -> None

let rec pp_font_stretch : font_stretch Pp.t =
 fun ctx v ->
  match if Pp.minified ctx then font_stretch_pct v else None with
  | Some pct -> Pp.pct ctx pct
  | None -> pp_font_stretch_keyword ctx v

and pp_font_stretch_keyword : font_stretch Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_font_stretch ctx v
  | Pct f -> Pp.pct ctx f
  | Ultra_condensed -> Pp.string ctx "ultra-condensed"
  | Extra_condensed -> Pp.string ctx "extra-condensed"
  | Condensed -> Pp.string ctx "condensed"
  | Semi_condensed -> Pp.string ctx "semi-condensed"
  | Normal -> Pp.string ctx "normal"
  | Semi_expanded -> Pp.string ctx "semi-expanded"
  | Expanded -> Pp.string ctx "expanded"
  | Extra_expanded -> Pp.string ctx "extra-expanded"
  | Ultra_expanded -> Pp.string ctx "ultra-expanded"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_font_size_adjust_metric : font_size_adjust_metric Pp.t =
 fun ctx -> function
  | Ex_height -> Pp.string ctx "ex-height"
  | Cap_height -> Pp.string ctx "cap-height"
  | Ch_width -> Pp.string ctx "ch-width"
  | Ic_width -> Pp.string ctx "ic-width"
  | Ic_height -> Pp.string ctx "ic-height"

let rec pp_font_size_adjust : font_size_adjust Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Number f -> Pp.float ctx f
  | From_font -> Pp.string ctx "from-font"
  | Metric_number (metric, f) ->
      pp_font_size_adjust_metric ctx metric;
      Pp.space ctx ();
      Pp.float ctx f
  | Metric_from_font metric ->
      pp_font_size_adjust_metric ctx metric;
      Pp.space ctx ();
      Pp.string ctx "from-font"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_size_adjust ctx v

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

let rec pp_font_display : font_display Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_font_display ctx v
  | Auto -> Pp.string ctx "auto"
  | Block -> Pp.string ctx "block"
  | Swap -> Pp.string ctx "swap"
  | Fallback -> Pp.string ctx "fallback"
  | Optional -> Pp.string ctx "optional"

let pp_unicode_range_range ctx start end_ =
  Pp.string ctx "U+";
  Pp.hex ctx start;
  Pp.char ctx '-';
  Pp.hex ctx end_

let unicode_range_wildcard start end_ : string option =
  let wildcard_for q : string option =
    let size = 1 lsl (4 * q) in
    if start mod size <> 0 || end_ <> start + size - 1 then None
    else
      let prefix = start / size in
      let prefix = if prefix = 0 then "" else hex_string prefix in
      let wildcard = "U+" ^ prefix ^ String.make q '?' in
      let range = "U+" ^ hex_string start ^ "-" ^ hex_string end_ in
      if String.length wildcard < String.length range then
        (Some wildcard : string option)
      else (None : string option)
  in
  let rec loop q =
    if q > 6 then (None : string option)
    else
      match wildcard_for q with
      | (Some _ : string option) as wildcard -> wildcard
      | None -> loop (q + 1)
  in
  loop 1

let rec pp_unicode_range : unicode_range Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_unicode_range ctx v
  | Single hex ->
      Pp.string ctx "U+";
      Pp.hex ctx hex
  | Range (start, end_) -> (
      let pp_range () = pp_unicode_range_range ctx start end_ in
      if not (Pp.minified ctx) then pp_range ()
      else
        match unicode_range_wildcard start end_ with
        | Some wildcard -> Pp.string ctx wildcard
        | None -> pp_range ())
  | Padded_single (value, width) ->
      if Pp.minified ctx then pp_unicode_range ctx (Single value)
      else (
        Pp.string ctx "U+";
        Pp.string ctx (padded_hex width value))
  | Padded_range { start; end_; start_width; end_width } ->
      if Pp.minified ctx then pp_unicode_range ctx (Range (start, end_))
      else (
        Pp.string ctx "U+";
        Pp.string ctx (padded_hex start_width start);
        Pp.char ctx '-';
        Pp.string ctx (padded_hex end_width end_))
  | Wildcard { prefix; prefix_width; wildcards } ->
      let start = prefix lsl (4 * wildcards) in
      let end_ = start + (1 lsl (4 * wildcards)) - 1 in
      if Pp.minified ctx then
        match unicode_range_wildcard start end_ with
        | Some wildcard -> Pp.string ctx wildcard
        | None -> pp_unicode_range_range ctx start end_
      else (
        Pp.string ctx "U+";
        if prefix_width > 0 then Pp.string ctx (padded_hex prefix_width prefix);
        Pp.string ctx (String.make wildcards '?'))

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

let rec pp_webkit_font_smoothing : webkit_font_smoothing Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_webkit_font_smoothing ctx v
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Antialiased -> Pp.string ctx "antialiased"
  | Subpixel_antialiased -> Pp.string ctx "subpixel-antialiased"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_font_size : font_size Pp.t =
 fun ctx -> function
  | Length l -> pp_length ctx l
  | Pct f -> Pp.pct ctx f
  | Var v -> pp_var pp_font_size ctx v
  | Calc c -> (
      let rec to_length_calc : font_size calc -> length calc option = function
        | Val (Length l) -> Some (Val l)
        | Val (Pct n) -> Some (Val (Pct n : length))
        | Num n -> Some (Num n)
        | Math_const c -> Some (Math_const c)
        | Math_fn fn -> Some (Math_fn fn)
        | Var _ | Sibling_index | Sibling_count | Val _ -> None
        | Nested inner -> Option.map (fun c -> Nested c) (to_length_calc inner)
        | Parens inner -> Option.map (fun c -> Parens c) (to_length_calc inner)
        | Expr (left, op, right) -> (
            match (to_length_calc left, to_length_calc right) with
            | Some left, Some right -> Some (Expr (left, op, right))
            | _ -> None)
      in
      match (Pp.minified ctx, to_length_calc c) with
      | true, Some c -> pp_length ctx (Calc c)
      | _ -> pp_calc pp_font_size ctx c)
  | Xx_small -> Pp.string ctx "xx-small"
  | X_small -> Pp.string ctx "x-small"
  | Small -> Pp.string ctx "small"
  | Medium -> Pp.string ctx "medium"
  | Large -> Pp.string ctx "large"
  | X_large -> Pp.string ctx "x-large"
  | Xx_large -> Pp.string ctx "xx-large"
  | Xxx_large -> Pp.string ctx "xxx-large"
  | Larger -> Pp.string ctx "larger"
  | Smaller -> Pp.string ctx "smaller"
  | Math -> Pp.string ctx "math"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_moz_osx_font_smoothing : moz_osx_font_smoothing Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_moz_osx_font_smoothing ctx v
  | Auto -> Pp.string ctx "auto"
  | Grayscale -> Pp.string ctx "grayscale"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_line_height : line_height Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Px f -> Pp.unit ctx f "px"
  | Rem f -> Pp.unit ctx f "rem"
  | Em f -> Pp.unit ctx f "em"
  | Pct p -> Pp.pct ctx p
  | Num n -> Pp.float ctx n
  | Number { value; unit; repr } -> (
      match (ctx.minify, unit) with
      | false, None -> Pp.string ctx repr
      | false, Some unit ->
          Pp.string ctx repr;
          Pp.string ctx unit
      | true, None -> Pp.float ctx value
      | true, Some "%" -> Pp.pct ctx value
      | true, Some unit -> Pp.unit ctx value unit)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_line_height ctx v
  | Calc c -> pp_calc pp_line_height ctx c

let rec pp_font_weight : font_weight Pp.t =
 fun ctx -> function
  | Weight n -> Pp.int ctx n
  | Normal when Pp.minified ctx ->
      (* CSS Fonts 4 5.1.2: [normal] is spec-equivalent to [400]. *)
      Pp.string ctx "400"
  | Normal -> Pp.string ctx "normal"
  | Bold when Pp.minified ctx ->
      (* CSS Fonts 4 5.1.2: [bold] is spec-equivalent to [700]. *)
      Pp.string ctx "700"
  | Bold -> Pp.string ctx "bold"
  | Bolder -> Pp.string ctx "bolder"
  | Lighter -> Pp.string ctx "lighter"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_weight ctx v

(* CSS Fonts 4 sec. 2.7: under minify, drop [<style>? <weight>? <stretch>?]
   components that equal their longhand initial value, and drop [line-height]
   when it's [normal]. The shorthand body itself is always [<size>
   [/<line-height>]? <family>+]; size and family are required. *)
let pp_font_variant_css21 ctx = function
  | (Normal : font_variant_css21) -> Pp.string ctx "normal"
  | Small_caps -> Pp.string ctx "small-caps"

let read_font_variant_css21 t : font_variant_css21 =
  Cursor.enum "font-variant-css21"
    [ ("normal", (Normal : font_variant_css21)); ("small-caps", Small_caps) ]
    t

let drop_font_default ctx (type a) ~(is_default : a -> bool) (opt : a option) :
    a option =
  if Pp.minified ctx then
    match opt with Some v when is_default v -> None | _ -> opt
  else opt

let drop_font_shorthand_defaults ctx style variant weight stretch line_height =
  let style =
    drop_font_default ctx style ~is_default:(function
      | (Normal : font_style) -> true
      | _ -> false)
  in
  let variant =
    drop_font_default ctx variant ~is_default:(function
      | (Normal : font_variant_css21) -> true
      | _ -> false)
  in
  let weight =
    drop_font_default ctx weight ~is_default:(function
      | (Normal : font_weight) | Weight 400 -> true
      | _ -> false)
  in
  let stretch =
    drop_font_default ctx stretch ~is_default:(function
      | (Normal : font_stretch) -> true
      | _ -> false)
  in
  let line_height =
    drop_font_default ctx line_height ~is_default:(function
      | (Normal : line_height) -> true
      | _ -> false)
  in
  (style, variant, weight, stretch, line_height)

let pp_font_prefix ctx style variant weight stretch =
  let first = ref true in
  let emit pp opt =
    Option.iter
      (fun v ->
        if not !first then Pp.space ctx ();
        first := false;
        pp ctx v)
      opt
  in
  emit pp_font_style style;
  emit pp_font_variant_css21 variant;
  emit pp_font_weight weight;
  (* The [font] shorthand's stretch component takes only the keywords, so the
     percentage the standalone property minifies to would be invalid here. *)
  emit pp_font_stretch_keyword stretch;
  !first

let pp_font_shorthand : font_shorthand Pp.t =
 fun ctx { style; variant; weight; stretch; size; line_height; family } ->
  let style, variant, weight, stretch, line_height =
    drop_font_shorthand_defaults ctx style variant weight stretch line_height
  in
  if not (pp_font_prefix ctx style variant weight stretch) then Pp.space ctx ();
  pp_font_size ctx size;
  Option.iter
    (fun lh ->
      Pp.char ctx '/';
      pp_line_height ctx lh)
    line_height;
  Pp.space ctx ();
  pp_font_family ctx family

let rec pp_font : font Pp.t =
 fun ctx -> function
  | Shorthand sh -> pp_font_shorthand ctx sh
  | Caption -> Pp.string ctx "caption"
  | Icon -> Pp.string ctx "icon"
  | Menu -> Pp.string ctx "menu"
  | Message_box -> Pp.string ctx "message-box"
  | Small_caption -> Pp.string ctx "small-caption"
  | Status_bar -> Pp.string ctx "status-bar"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font ctx v

let rec read_line_height t : line_height =
  let read_var t : line_height = Var (read_var read_line_height t) in
  let read_calc t : line_height =
    Calc (read_calc read_line_height t |> numeric_line_height_calc_leaves)
  in
  Cursor.enum_or_calls "line-height"
    [
      ("normal", Normal);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var); ("calc", read_calc) ]
    ~default:read_line_height_length t

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

let rec read_webkit_font_smoothing t : webkit_font_smoothing =
  Cursor.enum_or_var "webkit-font-smoothing"
    [
      ("auto", (Auto : webkit_font_smoothing));
      ("none", None);
      ("antialiased", Antialiased);
      ("subpixel-antialiased", Subpixel_antialiased);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_webkit_font_smoothing t))
    t

let rec read_moz_osx_font_smoothing t : moz_osx_font_smoothing =
  Cursor.enum_or_var "moz-osx-font-smoothing"
    [
      ("auto", (Auto : moz_osx_font_smoothing));
      ("grayscale", Grayscale);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_moz_osx_font_smoothing t))
    t

let font_family_generic_css =
  [
    ("sans-serif", Sans_serif);
    ("serif", Serif);
    ("monospace", Monospace);
    ("cursive", Cursive);
    ("fantasy", Fantasy);
    ("system-ui", System_ui);
    ("ui-sans-serif", Ui_sans_serif);
    ("ui-serif", Ui_serif);
    ("ui-monospace", Ui_monospace);
    ("ui-rounded", Ui_rounded);
    ("emoji", Emoji);
    ("math", Math);
    ("fangsong", Fangsong);
  ]

let font_family_css_keywords : (string * font_family) list =
  [
    ("inherit", Inherit);
    ("initial", Initial);
    ("unset", Unset);
    ("revert", Revert);
    ("revert-layer", Revert_layer);
  ]

let font_family_popular_web =
  [
    ("inter", Inter);
    ("roboto", Roboto);
    ("open-sans", Open_sans);
    ("lato", Lato);
    ("montserrat", Montserrat);
    ("poppins", Poppins);
    ("source-sans-pro", Source_sans_pro);
    ("raleway", Raleway);
    ("oswald", Oswald);
    ("noto-sans", Noto_sans);
    ("ubuntu", Ubuntu);
    ("playfair-display", Playfair_display);
    ("merriweather", Merriweather);
    ("lora", Lora);
    ("pt-sans", PT_sans);
    ("pt-serif", PT_serif);
    ("nunito", Nunito);
    ("nunito-sans", Nunito_sans);
    ("work-sans", Work_sans);
    ("rubik", Rubik);
    ("fira-sans", Fira_sans);
    ("fira-code", Fira_code);
    ("jetbrains-mono", JetBrains_mono);
    ("ibm-plex-sans", IBM_plex_sans);
    ("ibm-plex-serif", IBM_plex_serif);
    ("ibm-plex-mono", IBM_plex_mono);
    ("source-code-pro", Source_code_pro);
    ("space-mono", Space_mono);
    ("dm-sans", DM_sans);
    ("dm-serif-display", DM_serif_display);
    ("bebas-neue", Bebas_neue);
    ("barlow", Barlow);
    ("mulish", Mulish);
    ("josefin-sans", Josefin_sans);
  ]

let font_family_platform =
  [
    ("helvetica", Helvetica);
    ("helvetica-neue", Helvetica_neue);
    ("arial", Arial);
    ("verdana", Verdana);
    ("tahoma", Tahoma);
    ("trebuchet-ms", Trebuchet_ms);
    ("times-new-roman", Times_new_roman);
    ("times", Times);
    ("georgia", Georgia);
    ("cambria", Cambria);
    ("garamond", Garamond);
    ("courier-new", Courier_new);
    ("courier", Courier);
    ("lucida-console", Lucida_console);
    ("sf-pro", SF_pro);
    ("sf-pro-display", SF_pro_display);
    ("sf-pro-text", SF_pro_text);
    ("sf-mono", SF_mono);
    ("ny", NY);
    ("segoe-ui", Segoe_ui);
    ("segoe-ui-emoji", Segoe_ui_emoji);
    ("segoe-ui-symbol", Segoe_ui_symbol);
    ("apple-color-emoji", Apple_color_emoji);
    ("noto-color-emoji", Noto_color_emoji);
    ("android-emoji", Android_emoji);
    ("twemoji-mozilla", Twemoji_mozilla);
  ]

let font_family_developer =
  [
    ("menlo", Menlo);
    ("monaco", Monaco);
    ("consolas", Consolas);
    ("liberation-mono", Liberation_mono);
    ("sfmono-regular", SFMono_regular);
    ("cascadia-code", Cascadia_code);
    ("cascadia-mono", Cascadia_mono);
    ("victor-mono", Victor_mono);
    ("inconsolata", Inconsolata);
    ("hack", Hack);
  ]

let font_family_all_enums : (string * font_family) list =
  font_family_generic_css @ font_family_css_keywords @ font_family_popular_web
  @ font_family_platform @ font_family_developer

let font_family_lookup_key name =
  name |> String.lowercase_ascii |> String.map (function ' ' -> '-' | c -> c)

(* The unquoted single-word lookup matches generic family keywords; a quoted
   name is always a [<custom-ident>] by spec, so it preserves the user's intent
   even when its text matches a generic keyword ([font-family: "serif"] is a
   custom family named "serif", not the [serif] generic). Multi-word quoted
   names still match the platform-name table since those entries are not
   keywords. *)
let font_family_of_quoted_name name =
  match List.assoc_opt (font_family_lookup_key name) font_family_all_enums with
  | Some family when String.contains name ' ' -> family
  | _ -> Name name

let rec read_font_family_single t : font_family =
  let read_var t : font_family = Var (read_var read_font_family t) in
  (* CSS Fonts 4 sec. 2.1 / CSS Cascade 5 sec. 7.3: the CSS-wide keywords and
     the reserved [default] are excluded from [<custom-ident>], so none may
     appear as any word of an unquoted family name. *)
  let is_reserved_word word =
    List.mem
      (String.lowercase_ascii word)
      [ "inherit"; "initial"; "unset"; "revert"; "revert-layer"; "default" ]
  in
  (* Read unquoted multi-word font names, e.g., "arial rounded" *)
  let rec read_unquoted_name_words acc =
    let word = Cursor.ident ~keep_case:true t in
    if is_reserved_word word then
      Cursor.err_invalid t
        "font-family: reserved word cannot appear in an unquoted family name";
    let acc = word :: acc in
    Cursor.ws t;
    if Option.is_some (Cursor.peek_ident t) then read_unquoted_name_words acc
    else String.concat " " (List.rev acc)
  in
  let read_single_word t : font_family =
    (* For single-word names, try enum match first *)
    (Cursor.enum_or_calls "font-family" font_family_all_enums
       ~calls:[ ("var", read_var) ]
       ~default:(fun t ->
         let name = Cursor.ident ~keep_case:true t in
         (* CSS Fonts 4 sec. 2.1: [default] is reserved and is not a valid
            unquoted [<custom-ident>] family name; it must be quoted. *)
         if String.lowercase_ascii name = "default" then
           Cursor.err_invalid t
             "font-family: 'default' is reserved and must be quoted"
         else (Name name : font_family))
       t
      : font_family)
  in
  Cursor.ws t;
  match Cursor.string_opt t with
  | Some name -> font_family_of_quoted_name name
  | None when Cursor.looking_at_func "var" t -> read_var t
  | None when Option.is_some (Cursor.peek_ident t) ->
      (* Peek ahead to see if this is multi-word or single-word *)
      let is_multi_word =
        Cursor.lookahead
          (fun t ->
            let _ = Cursor.ident t in
            Cursor.ws t;
            Option.is_some (Cursor.peek_ident t))
          t
      in
      if is_multi_word then
        (* Multi-word unquoted name; [read_unquoted_name_words] rejects any
           reserved word in the sequence. *)
        Name (read_unquoted_name_words [])
      else
        (* Single word - try enum match *)
        read_single_word t
  | None -> Cursor.err t "expected font-family value"

and read_font_family t : font_family =
  (* CSS Cascade 5 sec. 7.3: a CSS-wide keyword ([inherit] / [initial] / [unset]
     / [revert] / [revert-layer]) must stand alone; mixed inside a
     [<custom-ident>#] list it makes the whole declaration invalid. *)
  let rec loop acc =
    Cursor.ws t;
    if Cursor.comma_opt t then (
      Cursor.ws t;
      loop (read_font_family_single t :: acc))
    else List.rev acc
  in
  let first = read_font_family_single t in
  let items = loop [ first ] in
  let is_css_wide = function
    | (Inherit : font_family) | Initial | Unset | Revert | Revert_layer -> true
    | _ -> false
  in
  match items with
  | [ x ] -> x
  | _ when List.exists is_css_wide items ->
      (* CSS Cascade 5 sec. 7.3: a CSS-wide keyword must be the sole value; in a
         [<family-name>#] list it makes the whole declaration invalid. *)
      Cursor.err_invalid t
        "font-family: a CSS-wide keyword cannot appear in a family list"
  | l -> List l

let read_shorthand_line_height_typed r : line_height =
  let before = Cursor.save r in
  match read_line_height r with
  | lh -> lh
  | exception Cursor.Parse_error _ ->
      Cursor.restore r before;
      Cursor.err_invalid r "invalid line-height in font shorthand"

let generic_font_family_keywords =
  [
    "sans-serif";
    "serif";
    "monospace";
    "cursive";
    "fantasy";
    "system-ui";
    "ui-sans-serif";
    "ui-serif";
    "ui-monospace";
    "ui-rounded";
    "emoji";
    "math";
    "fangsong";
  ]

(* A bare ident matching a generic family ([sans-serif], [ui-monospace], ...) is
   only valid inside a font-family list, so its presence proves the whole token
   stream is a font-family value (the same "the type is obviously correct"
   reasoning that folds a colour function in an opaque stream). *)
let components_have_generic_family components =
  List.exists
    (function
      | Component.Preserved { kind = Token.Ident name; _ } ->
          List.mem (String.lowercase_ascii name) generic_font_family_keywords
      | _ -> false)
    components

let long_generic_family_start r =
  let is_ws = function
    | Component.Preserved { kind = Token.Whitespace; _ } -> true
    | _ -> false
  in
  let is_comma = function
    | Component.Preserved { kind = Token.Comma; _ } -> true
    | _ -> false
  in
  let rec drop_while p = function
    | x :: rest when p x -> drop_while p rest
    | l -> l
  in
  let item =
    Cursor.remaining r |> drop_while is_ws |> List.to_seq
    |> Seq.take_while (fun cv -> not (is_comma cv))
    |> List.of_seq
    |> List.filter (fun cv -> not (is_ws cv))
  in
  match item with
  | Component.Preserved { kind = Token.Ident first; _ } :: _ :: _ ->
      List.mem (String.lowercase_ascii first) generic_font_family_keywords
  | _ -> false

let font_shorthand_prefix_ident = function
  | Some
      ( "italic" | "oblique" | "normal" | "small-caps" | "bold" | "bolder"
      | "lighter" | "condensed" | "expanded" ) ->
      true
  | _ -> false

(* Keyword -> which prefix slot it fills in the [font] shorthand. [Normal] is
   the absence keyword (style / variant / weight / stretch each have their own
   [normal]); we accept it and move on. *)
type font_prefix_slot =
  | Style of font_style
  | Variant of font_variant_css21
  | Weight of font_weight
  | Stretch of font_stretch
  | No_op

let font_prefix_slot_of = function
  | "italic" -> Style Italic
  | "oblique" -> Style Oblique
  | "small-caps" -> Variant Small_caps
  | "bold" -> Weight Bold
  | "bolder" -> Weight Bolder
  | "lighter" -> Weight Lighter
  | "condensed" -> Stretch Condensed
  | "expanded" -> Stretch Expanded
  | "normal" | _ -> No_op

let assign_font_prefix_slot ~(style : font_style option ref)
    ~(variant : font_variant_css21 option ref)
    ~(weight : font_weight option ref) ~(stretch : font_stretch option ref) =
  function
  | Style s -> if !style = None then style := Some s
  | Variant v -> if !variant = None then variant := Some v
  | Weight w -> if !weight = None then weight := Some w
  | Stretch st -> if !stretch = None then stretch := Some st
  | No_op -> ()

let try_numeric_font_weight r (weight : font_weight option ref) =
  let before = Cursor.save r in
  match read_font_weight r with
  | w when !weight = None ->
      weight := Some w;
      true
  | _ ->
      Cursor.restore r before;
      false
  | exception Cursor.Parse_error _ ->
      Cursor.restore r before;
      false

let read_optional_line_height r =
  Cursor.ws r;
  match Cursor.peek_delim r with
  | Some '/' ->
      Cursor.skip r;
      Cursor.ws r;
      Some (read_shorthand_line_height_typed r)
  | _ -> None

(* Parse the [font] shorthand body: a keyword prefix loop fills style / variant
   / weight / stretch (a [normal] binds no slot), then the required [size
   [/<line-height>]? <family>+] tail. *)
let read_font_shorthand r : font_shorthand =
  let style : font_style option ref = ref Option.None in
  let variant : font_variant_css21 option ref = ref Option.None in
  let weight : font_weight option ref = ref Option.None in
  let stretch : font_stretch option ref = ref Option.None in
  let assign = assign_font_prefix_slot ~style ~variant ~weight ~stretch in
  let rec consume_prefix () =
    Cursor.ws r;
    if Cursor.is_done r then ()
    else if font_shorthand_prefix_ident (Cursor.peek_ident r) then (
      assign (font_prefix_slot_of (Cursor.ident r));
      consume_prefix ())
    else if try_numeric_font_weight r weight then consume_prefix ()
  in
  consume_prefix ();
  Cursor.ws r;
  let size = read_font_size r in
  let line_height = read_optional_line_height r in
  if long_generic_family_start r then
    Cursor.err_invalid r "generic font family must be a standalone family item";
  let family = read_font_family r in
  Cursor.ws r;
  Cursor.expect_eof r;
  {
    style = !style;
    variant = !variant;
    weight = !weight;
    stretch = !stretch;
    size;
    line_height;
    family;
  }

let rec read_font t : font =
  let raw = Cursor.consume_to_decl_end ~trim:true t in
  let lower = String.lowercase_ascii (String.trim raw) in
  match lower with
  | "inherit" -> Inherit
  | "initial" -> Initial
  | "unset" -> Unset
  | "revert" -> Revert
  | "revert-layer" -> Revert_layer
  | "caption" -> Caption
  | "icon" -> Icon
  | "menu" -> Menu
  | "message-box" -> Message_box
  | "small-caption" -> Small_caption
  | "status-bar" -> Status_bar
  | _ ->
      let is_valid_var () =
        let r = Cursor.of_string raw in
        match Values.read_var (fun r -> read_font r) r with
        | (_ : font var) ->
            Cursor.ws r;
            Cursor.is_done r
        | exception Cursor.Parse_error _ -> false
      in
      if is_valid_var () then
        let r = Cursor.of_string raw in
        Var (Values.read_var (fun r -> read_font r) r)
      else if value_has_css_wide_mix raw then
        Cursor.err_invalid t "CSS-wide keyword mixed with other values"
      else
        let body =
          try read_font_shorthand (Cursor.of_string raw)
          with Cursor.Parse_error _ ->
            Cursor.err_invalid t "invalid font shorthand"
        in
        Shorthand body

let rec read_font_stretch t : font_stretch =
  let read_percentage t : font_stretch =
    let n = Cursor.pct t in
    (* CSS Fonts 4 sec. 6.1.2: font-stretch percentage is non-negative. *)
    if n < 0. then err_invalid_value t "font-stretch" (string_of_float n);
    Pct n
  in
  Cursor.enum_or_var "font-stretch"
    [
      ("ultra-condensed", Ultra_condensed);
      ("extra-condensed", Extra_condensed);
      ("condensed", Condensed);
      ("semi-condensed", Semi_condensed);
      ("normal", Normal);
      ("semi-expanded", Semi_expanded);
      ("expanded", Expanded);
      ("extra-expanded", Extra_expanded);
      ("ultra-expanded", Ultra_expanded);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_stretch t))
    ~default:read_percentage t

let read_font_display t : font_display =
  Cursor.enum "font-display"
    [
      ("auto", (Auto : font_display));
      ("block", Block);
      ("swap", Swap);
      ("fallback", Fallback);
      ("optional", Optional);
    ]
    t

let read_unicode_single start_value width =
  if width > String.length (hex_string start_value) then
    (Padded_single (start_value, width) : unicode_range)
  else Single start_value

let read_unicode_range_pair start_value end_value start_width end_width =
  if
    start_width > String.length (hex_string start_value)
    || end_width > String.length (hex_string end_value)
  then
    Padded_range
      { start = start_value; end_ = end_value; start_width; end_width }
  else Range (start_value, end_value)

let read_unicode_wildcard start_value prefix_width wildcards =
  let prefix = start_value lsr (4 * wildcards) in
  Wildcard { prefix; prefix_width; wildcards }

let read_unicode_token_form start_value end_value = function
  | Token.Single { width } -> read_unicode_single start_value width
  | Token.Range { start_width; end_width } ->
      read_unicode_range_pair start_value end_value start_width end_width
  | Token.Wildcard { prefix_width; wildcards } ->
      read_unicode_wildcard start_value prefix_width wildcards

let read_unicode_token t start_value end_value form =
  Cursor.skip t;
  if start_value > end_value then
    Cursor.err_invalid t "unicode range: start > end";
  if end_value > 0x10FFFF then
    Cursor.err_invalid t "unicode range: code point out of range";
  read_unicode_token_form start_value end_value form

let rec read_unicode_range t : unicode_range =
  (* The lexer emits a single [Unicode_range] token for [U+...] forms (CSS
     Syntax section 4.3.14); we just translate it to the [unicode_range] ADT. *)
  Cursor.with_context t "unicode-range" @@ fun () ->
  match Cursor.peek t with
  | Some (Component.Func { node = { name = "var"; _ }; _ }) ->
      (Var (Values.read_var read_unicode_range t) : unicode_range)
  | Some
      (Component.Preserved
         { kind = Token.Unicode_range { start_value; end_value; form }; _ }) ->
      read_unicode_token t start_value end_value form
  | _ -> Cursor.err_expected t "unicode-range"

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
      (* CSS Fonts 4 section 6.6: [normal] resets all sub-properties and must
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

let rec read_font_feature_settings t : font_feature_settings =
  let read_var t : font_feature_settings =
    Var (read_var read_font_feature_settings t)
  in
  let read_feature t =
    let tag_content = Cursor.string t in
    if String.length tag_content <> 4 then
      Cursor.err t "font-feature-settings tag must be exactly 4 characters";
    let tag = "\"" ^ tag_content ^ "\"" in
    Cursor.ws t;
    match Cursor.option Cursor.number t with
    | Some n ->
        if n <> 0.0 && n <> 1.0 then
          Cursor.err t "font-feature-settings value must be 0 or 1";
        tag ^ " " ^ string_of_int (int_of_float n)
    | None -> (
        match Cursor.option Cursor.ident t with
        | Some "on" -> tag ^ " on"
        | Some "off" -> tag ^ " off"
        | Some _ -> Cursor.err t "font-feature-settings value must be on/off"
        | None -> tag)
  in
  let read_feature_list t =
    let items = Cursor.list ~sep:Cursor.comma ~at_least:1 read_feature t in
    Feature_list (String.concat ", " items)
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

let rec read_font_variation_settings t : font_variation_settings =
  let read_var t : font_variation_settings =
    Var (read_var read_font_variation_settings t)
  in
  let read_axis t =
    let tag_content = Cursor.string t in
    if String.length tag_content <> 4 then
      Cursor.err t
        "font-variation-settings axis tag must be exactly 4 characters";
    String.iter
      (fun c ->
        let code = Char.code c in
        if code < 0x20 || code > 0x7E then
          Cursor.err t
            "font-variation-settings axis tag must contain only ASCII \
             characters (U+20 - U+7E)")
      tag_content;
    let tag = "\"" ^ tag_content ^ "\"" in
    Cursor.ws t;
    let value = Cursor.number t in
    tag ^ " " ^ string_of_int (int_of_float value)
  in
  let read_axis_list t =
    let items = Cursor.list ~sep:Cursor.comma ~at_least:1 read_axis t in
    Axis_list (String.concat ", " items)
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

let pp_font_url ctx s =
  Pp.string ctx "url(";
  if url_needs_quotes s then (
    Pp.char ctx '"';
    Pp.string ctx s;
    Pp.char ctx '"')
  else Pp.string ctx s;
  Pp.char ctx ')'

let pp_quoted_font_url ctx quote s =
  Pp.string ctx "url(";
  Pp.char ctx quote;
  Pp.string ctx s;
  Pp.char ctx quote;
  Pp.char ctx ')'

let pp_font_src_modifiers ctx (format : string option) (tech : string option) =
  (match format with
  | None -> ()
  | Some value ->
      Pp.space ctx ();
      Pp.string ctx "format(";
      Pp.string ctx value;
      Pp.char ctx ')');
  match tech with
  | None -> ()
  | Some value ->
      Pp.space ctx ();
      Pp.string ctx "tech(";
      Pp.string ctx value;
      Pp.char ctx ')'

let rec pp_font_src_entry ctx : Font_face.src_entry -> unit = function
  | Local name ->
      Pp.string ctx "local(";
      Pp.char ctx '"';
      Pp.string ctx name;
      Pp.char ctx '"';
      Pp.char ctx ')'
  | Url { url; format; tech } ->
      pp_font_url ctx url;
      pp_font_src_modifiers ctx format tech
  | Quoted_url { url; quote; format; tech } ->
      pp_quoted_font_url ctx quote url;
      pp_font_src_modifiers ctx format tech
  | Var var -> pp_var pp_font_src ctx var

and pp_font_src ctx entries =
  let first = ref true in
  List.iter
    (fun entry ->
      if !first then first := false
      else (
        Pp.char ctx ',';
        Pp.space_if_pretty ctx ());
      pp_font_src_entry ctx entry)
    entries

let read_font_src = Font_face.read_src

let normalize_font_size (fs : font_size) : font_size =
  match fs with
  | Length l -> preserve_if_equal fs (Length (Values.normalize_length l))
  | Calc c -> (
      match Values.eval_calc c with Values.Val v -> v | folded -> Calc folded)
  | _ -> fs

let normalize_line_height (lh : line_height) : line_height =
  match lh with
  | Calc c -> (
      match Values.eval_calc c with
      | Values.Num f -> Num f
      | Values.Val v -> v
      | folded -> Calc folded)
  | _ -> lh

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

let read_font_size_adjust_metric t : font_size_adjust_metric =
  Cursor.enum "font-size-adjust metric"
    [
      ("ex-height", (Ex_height : font_size_adjust_metric));
      ("cap-height", Cap_height);
      ("ch-width", Ch_width);
      ("ic-width", Ic_width);
      ("ic-height", Ic_height);
    ]
    t

let rec read_font_size_adjust t : font_size_adjust =
  let read_non_negative_number t =
    let n = Cursor.number t in
    if n < 0. then Cursor.err_invalid t "font-size-adjust must be non-negative";
    n
  in
  let read_metric_value t =
    let metric = read_font_size_adjust_metric t in
    Cursor.ws t;
    match Cursor.peek_ident t with
    | Some "from-font" ->
        let _ = Cursor.ident t in
        Metric_from_font metric
    | _ -> Metric_number (metric, read_non_negative_number t)
  in
  Cursor.enum_or_var "font-size-adjust"
    [
      ("none", (None : font_size_adjust));
      ("from-font", From_font);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_font_size_adjust t))
    ~default:(fun t ->
      match Cursor.peek_ident t with
      | Some _ -> read_metric_value t
      | None -> Number (read_non_negative_number t))
    t
