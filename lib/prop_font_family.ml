open Common
open Values
open Properties_intf

(* CSS Syntax 3 (ED) sec. 4.3.9: an ident sequence starts with a name-start code
   point, or with a [-] followed by a name-start code point or by a second [-].
   Sec. 4.2 counts a letter, [_] and a non-ASCII code point as name-start, and
   adds the digits and [-] to the name code points that may follow. A name
   needing an escape has no plain ident spelling, so [\] and every other
   non-name byte fails here and the name is written as a [<string>] instead.
   Non-ASCII is held to the ASCII rule and stays quoted too. *)
let is_font_family_ident_word s =
  let len = String.length s in
  let is_name_start = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false
  in
  let is_name_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true
    | _ -> false
  in
  let starts_ident =
    len > 0
    && (is_name_start s.[0]
       || Char.equal s.[0] '-'
          && len >= 2
          && (is_name_start s.[1] || Char.equal s.[1] '-'))
  in
  starts_ident && String.for_all is_name_char s

(* CSS Fonts 4 sec. 2.1.2 spells the bare [<generic-font-family>] names, the
   script-specific generics being functional instead ([generic(fangsong)]). Sec.
   2.1.1 keeps them out of [<font-family-name>], and the property reads a
   [<generic-font-family>] only where the alternative starts, so a generic is
   turned away as the first word of a sequence and is an ordinary
   [<custom-ident>] after it: Chrome 153 refuses [serif serif] and [serif Foo]
   and takes [Foo serif] and [Cambria Math], which are installed font names. *)
let font_family_generic_words =
  [
    "serif";
    "sans-serif";
    "monospace";
    "cursive";
    "fantasy";
    "system-ui";
    "math";
    "ui-serif";
    "ui-sans-serif";
    "ui-monospace";
    "ui-rounded";
  ]

(* CSS Values 4 sec. 4.2 excludes the CSS-wide keywords and the reserved
   [default] from [<custom-ident>] itself, so no word of a sequence is one
   wherever it stands. Chrome takes [inherit inherit] as a name; the exclusion
   is on the type rather than on the position, so cascade is the strict side. *)
let font_family_css_wide_words =
  [ "inherit"; "initial"; "unset"; "revert"; "revert-layer"; "default" ]

let font_family_reserved_words =
  font_family_generic_words @ font_family_css_wide_words

let is_font_family_reserved_word w =
  let w = String.lowercase_ascii w in
  List.exists (String.equal w) font_family_reserved_words

let is_font_family_css_wide w =
  let w = String.lowercase_ascii w in
  List.exists (String.equal w) font_family_css_wide_words

(* A lone word has to clear more than the excluded idents: [read_font_family]
   below also maps a bare [emoji], [fangsong] or [none] to a keyword rather than
   a name, so unquoting one changes the value. Inside a sequence they are
   ordinary [<custom-ident>]s, which is what keeps [Noto Color Emoji]
   unquoted. *)
let is_font_family_keyword_name s =
  let s = String.lowercase_ascii s in
  is_font_family_reserved_word s
  || List.exists (String.equal s) [ "emoji"; "fangsong"; "none" ]

let can_unquote_font_family_name s =
  match String.split_on_char ' ' s with
  | [] -> false
  | [ w ] ->
      (* A [<custom-ident>+] of one word is still a [<custom-ident>+], so a lone
         name unquotes as a sequence does, against the wider exclusion: the
         words only a lone position reads as a keyword are excluded too. *)
      is_font_family_ident_word w && not (is_font_family_keyword_name w)
  | _ :: _ :: _ as words ->
      (* A reserved word anywhere in the sequence keeps the quotes, which is
         stricter than what the reader takes back: sec. 2.1.1 reads flat as
         excluding [Cambria Math], and only some browsers go on to take it, so
         the quoted spelling is the one every UA reads. Unquoting is a size win
         the printer declines rather than a spelling it owes. *)
      List.for_all
        (fun w ->
          is_font_family_ident_word w && not (is_font_family_reserved_word w))
        words

(* Walk a component stream and rewrite each [<string>] token whose content is an
   identifier sequence (the [can_unquote_font_family_name] guard) as an explicit
   [<ident>] sequence. Only for a stream a generic family has proven to be a
   font stack, where CSS Fonts 4 sec. 2.1.1 makes the two forms one name. The
   guard keeps a name holding an excluded word quoted, so no rewrite turns a
   word of the sequence into a keyword. *)
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
        :: Component.Preserved (Token.v ~kind:(Token.Whitespace " ") ~loc)
        :: interleave loc rest
  in
  let result =
    List.concat_map
      (fun c ->
        match c with
        | Component.Preserved { kind = Token.String { value; _ }; loc; _ }
          when can_unquote_font_family_name value ->
            changed := true;
            interleave loc (words_of value)
        | _ -> [ c ])
      components
  in
  if !changed then result else components

(* CSS Fonts 4 sec. 2.1: a [<family-name>] is a [<string>] or a
   [<custom-ident>+], and the two spell the same name. Emit the bare ident
   sequence when it reads back as that same name and quote otherwise: a name
   that is no ident sequence at all keeps its quotes, and so does one whose
   words include a reserved word, since dropping the quotes there turns the word
   into the keyword. The unquoted multi-word form is shorter but is not the
   CSSOM-canonical serialization, so [enforce_spec] keeps the quotes. *)
let pp_family_name_with_keywords is_keyword ctx s =
  if
    Pp.minified ctx && (not ctx.Pp.enforce_spec)
    && can_unquote_font_family_name s
  then Pp.string ctx s
  else if is_keyword s || not (is_font_family_ident_word s) then
    Pp.quoted_string ctx s
  else Pp.string ctx s

let pp_family_name = pp_family_name_with_keywords is_font_family_keyword_name

let pp_descriptor_family_name =
  pp_family_name_with_keywords is_font_family_reserved_word

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
  | Inter -> pp_family_name ctx "Inter"
  | Roboto -> pp_family_name ctx "Roboto"
  | Open_sans -> pp_family_name ctx "Open Sans"
  | Lato -> pp_family_name ctx "Lato"
  | Montserrat -> pp_family_name ctx "Montserrat"
  | Poppins -> pp_family_name ctx "Poppins"
  | Source_sans_pro -> pp_family_name ctx "Source Sans Pro"
  | Raleway -> pp_family_name ctx "Raleway"
  | Oswald -> pp_family_name ctx "Oswald"
  | Noto_sans -> pp_family_name ctx "Noto Sans"
  | Ubuntu -> pp_family_name ctx "Ubuntu"
  | Playfair_display -> pp_family_name ctx "Playfair Display"
  | Merriweather -> pp_family_name ctx "Merriweather"
  | Lora -> pp_family_name ctx "Lora"
  | PT_sans -> pp_family_name ctx "PT Sans"
  | PT_serif -> pp_family_name ctx "PT Serif"
  | Nunito -> pp_family_name ctx "Nunito"
  | Nunito_sans -> pp_family_name ctx "Nunito Sans"
  | Work_sans -> pp_family_name ctx "Work Sans"
  | Rubik -> pp_family_name ctx "Rubik"
  | Fira_sans -> pp_family_name ctx "Fira Sans"
  | Fira_code -> pp_family_name ctx "Fira Code"
  | JetBrains_mono -> pp_family_name ctx "JetBrains Mono"
  | IBM_plex_sans -> pp_family_name ctx "IBM Plex Sans"
  | IBM_plex_serif -> pp_family_name ctx "IBM Plex Serif"
  | IBM_plex_mono -> pp_family_name ctx "IBM Plex Mono"
  | Source_code_pro -> pp_family_name ctx "Source Code Pro"
  | Space_mono -> pp_family_name ctx "Space Mono"
  | DM_sans -> pp_family_name ctx "DM Sans"
  | DM_serif_display -> pp_family_name ctx "DM Serif Display"
  | Bebas_neue -> pp_family_name ctx "Bebas Neue"
  | Barlow -> pp_family_name ctx "Barlow"
  | Mulish -> pp_family_name ctx "Mulish"
  | Josefin_sans -> pp_family_name ctx "Josefin Sans"
  (* Platform-specific fonts *)
  | Helvetica -> pp_family_name ctx "Helvetica"
  | Helvetica_neue -> pp_family_name ctx "Helvetica Neue"
  | Arial -> pp_family_name ctx "Arial"
  | Verdana -> pp_family_name ctx "Verdana"
  | Tahoma -> pp_family_name ctx "Tahoma"
  | Trebuchet_ms -> pp_family_name ctx "Trebuchet MS"
  | Times_new_roman -> pp_family_name ctx "Times New Roman"
  | Times -> pp_family_name ctx "Times"
  | Georgia -> pp_family_name ctx "Georgia"
  | Cambria -> pp_family_name ctx "Cambria"
  | Garamond -> pp_family_name ctx "Garamond"
  | Courier_new -> pp_family_name ctx "Courier New"
  | Courier -> pp_family_name ctx "Courier"
  | Lucida_console -> pp_family_name ctx "Lucida Console"
  | SF_pro -> pp_family_name ctx "SF Pro"
  | SF_pro_display -> pp_family_name ctx "SF Pro Display"
  | SF_pro_text -> pp_family_name ctx "SF Pro Text"
  | SF_mono -> pp_family_name ctx "SF Mono"
  | NY -> pp_family_name ctx "New York"
  | Segoe_ui -> pp_family_name ctx "Segoe UI"
  | Segoe_ui_emoji -> pp_family_name ctx "Segoe UI Emoji"
  | Segoe_ui_symbol -> pp_family_name ctx "Segoe UI Symbol"
  | Apple_color_emoji -> pp_family_name ctx "Apple Color Emoji"
  | Noto_color_emoji -> pp_family_name ctx "Noto Color Emoji"
  | Android_emoji -> pp_family_name ctx "Android Emoji"
  | Twemoji_mozilla -> pp_family_name ctx "Twemoji Mozilla"
  (* Developer fonts *)
  | Menlo -> pp_family_name ctx "Menlo"
  | Monaco -> pp_family_name ctx "Monaco"
  | Consolas -> pp_family_name ctx "Consolas"
  | Liberation_mono -> pp_family_name ctx "Liberation Mono"
  | SFMono_regular -> pp_family_name ctx "SFMono-Regular"
  | Cascadia_code -> pp_family_name ctx "Cascadia Code"
  | Cascadia_mono -> pp_family_name ctx "Cascadia Mono"
  | Victor_mono -> pp_family_name ctx "Victor Mono"
  | Inconsolata -> pp_family_name ctx "Inconsolata"
  | Hack -> pp_family_name ctx "Hack"
  (* CSS keywords *)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Name s -> pp_family_name ctx s
  | Var v -> pp_var pp_font_family ctx v
  | List fonts ->
      let level_chars =
        match ctx.Pp.indent with Some w -> w * ctx.Pp.level | None -> 0
      in
      Pp.list_wrap ~threshold:90 ~sep:Pp.comma ~wrap_indent:(level_chars + 2)
        pp_font_family ctx fonts
  | Invalid tokens ->
      let rendered =
        if Pp.minified ctx then Parser.to_string_minified tokens
        else Parser.string_of_components tokens
      in
      Pp.string ctx rendered

let rec pp_font_family_name : font_family Pp.t =
 fun ctx -> function
  | Name name -> pp_descriptor_family_name ctx name
  | Var var -> pp_var pp_font_family_name ctx var
  | family -> pp_font_family ctx family

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

(* CSS Fonts 4 sec. 2.1: [font-family] has exactly two sets of keywords, the
   generic families and the CSS-wide keywords. Every other name is an
   author-chosen [<family-name>], and sec. 5.1 matches those with Default
   Caseless Matching - a caseless string comparison, which folds no hyphen to a
   space and no name to another - so a name is read verbatim and printed back as
   authored. *)
let font_family_keywords : (string * font_family) list =
  font_family_generic_css @ font_family_css_keywords

(* CSS Fonts 4 sec. 2.1.1: descriptors whose grammar is [<font-family-name>]
   accept strings and non-empty custom-ident sequences, but not the
   generic-family or CSS-wide keywords accepted by the [font-family] property.
   Quoting keeps a keyword available as a family name. *)
let rec read_font_family_name t : font_family =
  let read_var t : font_family = Var (read_var read_font_family_name t) in
  Cursor.ws t;
  match Cursor.string_opt t with
  | Some name -> Name name
  | None when Cursor.looking_at_func "var" t -> read_var t
  | None ->
      let rec read_words acc : font_family =
        match Cursor.peek_ident t with
        | Some _ ->
            let word = Cursor.ident ~keep_case:true t in
            (* A generic family name is the alternative the grammar reads
               instead of a name, so it is turned away where that alternative
               starts, at the first word, and is an ordinary [<custom-ident>]
               after it: [Cambria Math] and [Foo serif] are installed fonts,
               [serif Foo] is not. A CSS-wide keyword or [default] is excluded
               from [<custom-ident>] itself (CSS Values 4 sec. 4.2), so it is
               turned away at every word. This is the rule
               [read_unquoted_family_name] applies to the property, over the
               same production. *)
            let reserved =
              match acc with
              | [] -> is_font_family_reserved_word word
              | _ :: _ -> is_font_family_css_wide word
            in
            if reserved then
              Cursor.err_invalid t
                "reserved word in an unquoted font-family name";
            Cursor.ws t;
            read_words (word :: acc)
        | None -> (
            match acc with
            | [] -> Cursor.err_expected t "font-family name"
            | _ -> Name (String.concat " " (List.rev acc)))
      in
      read_words []

let is_font_family_name_value : font_family -> bool = function
  | Sans_serif | Serif | Monospace | Cursive | Fantasy | System_ui
  | Ui_sans_serif | Ui_serif | Ui_monospace | Ui_rounded | Math | Inherit
  | Initial | Unset | Revert | Revert_layer | List _ | Invalid _ ->
      false
  | Emoji | Fangsong | Inter | Roboto | Open_sans | Lato | Montserrat | Poppins
  | Source_sans_pro | Raleway | Oswald | Noto_sans | Ubuntu | Playfair_display
  | Merriweather | Lora | PT_sans | PT_serif | Nunito | Nunito_sans | Work_sans
  | Rubik | Fira_sans | Fira_code | JetBrains_mono | IBM_plex_sans
  | IBM_plex_serif | IBM_plex_mono | Source_code_pro | Space_mono | DM_sans
  | DM_serif_display | Bebas_neue | Barlow | Mulish | Josefin_sans | Helvetica
  | Helvetica_neue | Arial | Verdana | Tahoma | Trebuchet_ms | Times_new_roman
  | Times | Georgia | Cambria | Garamond | Courier_new | Courier
  | Lucida_console | SF_pro | SF_pro_display | SF_pro_text | SF_mono | NY
  | Segoe_ui | Segoe_ui_emoji | Segoe_ui_symbol | Apple_color_emoji
  | Noto_color_emoji | Android_emoji | Twemoji_mozilla | Menlo | Monaco
  | Consolas | Liberation_mono | SFMono_regular | Cascadia_code | Cascadia_mono
  | Victor_mono | Inconsolata | Hack | Name _ | Var _ ->
      true

(* An unquoted multi-word family name, e.g. [arial rounded]. The two exclusions
   have different reaches. A [<generic-font-family>] is the alternative the
   property reads instead of a name, so it is turned away where that alternative
   starts, at the first word, and reads as an ordinary [<custom-ident>] after
   it: [Cambria Math] and [Foo serif] are installed font names, [serif Foo] is
   not. A CSS-wide keyword or [default] is excluded from [<custom-ident>] itself
   (CSS Values 4 sec. 4.2), so it is turned away at every word. *)
let read_unquoted_family_name t =
  let rec loop acc =
    let word = Cursor.ident ~keep_case:true t in
    let reserved =
      match acc with
      | [] -> is_font_family_reserved_word word
      | _ :: _ -> is_font_family_css_wide word
    in
    if reserved then
      Cursor.err_invalid t
        "font-family: reserved word cannot appear in an unquoted family name";
    let acc = word :: acc in
    Cursor.ws t;
    if Option.is_some (Cursor.peek_ident t) then loop acc
    else String.concat " " (List.rev acc)
  in
  loop []

let rec read_font_family_single t : font_family =
  let read_var t : font_family = Var (read_var read_font_family t) in
  let read_single_word t : font_family =
    (* A single word is a keyword before it is a name *)
    (Cursor.enum_or_calls "font-family" font_family_keywords
       ~calls:[ ("var", read_var) ]
       ~default:(fun t ->
         let name = Cursor.ident ~keep_case:true t in
         (* CSS Fonts 4 sec. 2.1.1: [default] is reserved and is not a valid
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
  | Some name -> Name name
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
      if is_multi_word then Name (read_unquoted_family_name t)
      else
        (* Single word - try the keyword match *)
        read_single_word t
  | None -> Cursor.err t "expected font-family value"

and read_font_family t : font_family =
  let source = Cursor.remaining t in
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
         [<family-name>#] list it makes the whole declaration invalid. Keep the
         source for the declaration-level invalid-value recovery pass. *)
      Invalid source
  | l -> List l

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
    | Component.Preserved { kind = Token.Whitespace _; _ } -> true
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

(* sec. 2.1 has the user agent walk the family list until one matches, so an
   entry repeating an earlier one is never reached and names nothing: drop it,
   keeping the first occurrence's position. The key is the minified spelling,
   which is what makes [Arial] and ["Arial"] one entry. A one-entry list is that
   entry, so the fold lands on the node the same text parses to. A bare generic
   keyword (notably [monospace]) takes the UA generic-font size, so the
   [monospace, monospace] idiom opts back into the normal size: a list that
   would collapse to a lone generic keeps its duplicate. *)
let normalize_font_family (value : font_family) : font_family =
  match value with
  | List fonts -> (
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
      if List.compare_lengths deduped fonts = 0 then
        match fonts with [ single ] -> single | _ -> value
      else
        match deduped with
        | [ single ] when is_generic_family single -> value
        | [ single ] -> single
        | deduped -> List deduped)
  | other -> other
