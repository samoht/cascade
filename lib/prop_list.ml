(* List-style printers, normalisation and readers. *)

open Common
open Values
open Properties_intf
open Prop_common
open Prop_image

let pp_list_style_symbol_sep ctx first (symbol : list_style_symbol) =
  if !first then first := false
  else
    match symbol with
    | String _ when Pp.minified ctx -> ()
    | _ -> Pp.space ctx ()

let pp_symbols_type ctx (kind : symbols_type) =
  match kind with
  | Cyclic -> Pp.string ctx "cyclic"
  | Numeric -> Pp.string ctx "numeric"
  | Alphabetic -> Pp.string ctx "alphabetic"
  | Symbolic -> Pp.string ctx "symbolic"
  | Fixed -> Pp.string ctx "fixed"

let pp_list_style_symbol ctx (symbol : list_style_symbol) =
  match symbol with
  | String symbol -> Pp.quoted_string ctx symbol
  | Url url -> Pp.url ctx url

let pp_list_style_symbols ctx (kind, symbols) =
  let first = ref true in
  let sep symbol = pp_list_style_symbol_sep ctx first symbol in
  let kind =
    match kind with
    | Option.Some Symbolic when Pp.minified ctx -> Option.None
    | kind -> (kind : symbols_type option)
  in
  Option.iter
    (fun kind ->
      sep (String "");
      pp_symbols_type ctx kind)
    kind;
  List.iter
    (fun symbol ->
      sep symbol;
      pp_list_style_symbol ctx symbol)
    symbols

let rec pp_list_style_type : list_style_type Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Disc -> Pp.string ctx "disc"
  | Circle -> Pp.string ctx "circle"
  | Square -> Pp.string ctx "square"
  | Decimal -> Pp.string ctx "decimal"
  | Lower_alpha -> Pp.string ctx "lower-alpha"
  | Upper_alpha -> Pp.string ctx "upper-alpha"
  | Lower_roman -> Pp.string ctx "lower-roman"
  | Upper_roman -> Pp.string ctx "upper-roman"
  | Decimal_leading_zero -> Pp.string ctx "decimal-leading-zero"
  | Arabic_indic -> Pp.string ctx "arabic-indic"
  | Armenian -> Pp.string ctx "armenian"
  | Upper_armenian -> Pp.string ctx "upper-armenian"
  | Lower_armenian -> Pp.string ctx "lower-armenian"
  | Bengali -> Pp.string ctx "bengali"
  | Cambodian -> Pp.string ctx "cambodian"
  | Khmer -> Pp.string ctx "khmer"
  | Cjk_decimal -> Pp.string ctx "cjk-decimal"
  | Devanagari -> Pp.string ctx "devanagari"
  | Georgian -> Pp.string ctx "georgian"
  | Gujarati -> Pp.string ctx "gujarati"
  | Gurmukhi -> Pp.string ctx "gurmukhi"
  | Hebrew -> Pp.string ctx "hebrew"
  | Kannada -> Pp.string ctx "kannada"
  | Lao -> Pp.string ctx "lao"
  | Malayalam -> Pp.string ctx "malayalam"
  | Mongolian -> Pp.string ctx "mongolian"
  | Myanmar -> Pp.string ctx "myanmar"
  | Oriya -> Pp.string ctx "oriya"
  | Persian -> Pp.string ctx "persian"
  | Tamil -> Pp.string ctx "tamil"
  | Telugu -> Pp.string ctx "telugu"
  | Thai -> Pp.string ctx "thai"
  | Tibetan -> Pp.string ctx "tibetan"
  | Lower_latin -> Pp.string ctx "lower-latin"
  | Upper_latin -> Pp.string ctx "upper-latin"
  | Cjk_earthly_branch -> Pp.string ctx "cjk-earthly-branch"
  | Cjk_heavenly_stem -> Pp.string ctx "cjk-heavenly-stem"
  | Lower_greek -> Pp.string ctx "lower-greek"
  | Hiragana -> Pp.string ctx "hiragana"
  | Hiragana_iroha -> Pp.string ctx "hiragana-iroha"
  | Katakana -> Pp.string ctx "katakana"
  | Katakana_iroha -> Pp.string ctx "katakana-iroha"
  | Disclosure_open -> Pp.string ctx "disclosure-open"
  | Disclosure_closed -> Pp.string ctx "disclosure-closed"
  | Cjk_ideographic -> Pp.string ctx "cjk-ideographic"
  | Japanese_informal -> Pp.string ctx "japanese-informal"
  | Japanese_formal -> Pp.string ctx "japanese-formal"
  | Korean_hangul_formal -> Pp.string ctx "korean-hangul-formal"
  | Korean_hanja_informal -> Pp.string ctx "korean-hanja-informal"
  | Korean_hanja_formal -> Pp.string ctx "korean-hanja-formal"
  | Simp_chinese_informal -> Pp.string ctx "simp-chinese-informal"
  | Simp_chinese_formal -> Pp.string ctx "simp-chinese-formal"
  | Trad_chinese_informal -> Pp.string ctx "trad-chinese-informal"
  | Trad_chinese_formal -> Pp.string ctx "trad-chinese-formal"
  | Ethiopic_numeric -> Pp.string ctx "ethiopic-numeric"
  | Name name -> pp_ident ctx name
  | String s -> Pp.quoted_string ctx s
  | Symbols (kind, symbols) ->
      Pp.call "symbols" pp_list_style_symbols ctx (kind, symbols)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_list_style_type ctx v

let rec pp_list_style_position : list_style_position Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_list_style_position ctx v
  | Inside -> Pp.string ctx "inside"
  | Outside -> Pp.string ctx "outside"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_list_style_image : list_style_image Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Image i -> pp_background_image ctx i
  | Var v -> pp_var pp_list_style_image ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_list_style_shorthand : list_style_shorthand Pp.t =
 fun ctx { type_; position; image } ->
  let is_none_type = type_ = Some (None : list_style_type) in
  let is_none_image = image = Some (None : list_style_image) in
  (* CSS Lists 3 (ED) sec. 3.6: "a value of none in the shorthand must be
     applied to whichever of the two properties aren't otherwise set by the
     shorthand", so a lone [none] with no position reads back as the same pair
     of nones the two keywords spell out. *)
  if is_none_type && is_none_image && position = Option.None && Pp.minified ctx
  then Pp.string ctx "none"
  else
    let first = ref true in
    let emit pp = function
      | Option.None -> ()
      | Some v ->
          if !first then first := false else Pp.space ctx ();
          pp ctx v
    in
    let ambiguous_type =
      match type_ with
      | Some (Name name) ->
          let lower = String.lowercase_ascii name in
          lower = "inside" || lower = "outside"
      | _ -> false
    in
    (* A position-shaped counter name must follow an explicit position, even
       when normalisation has removed the default [outside] slot. *)
    if ambiguous_type then begin
      emit pp_list_style_position
        (Some (Option.value ~default:Outside position));
      emit pp_list_style_type type_
    end
    else begin
      emit pp_list_style_type type_;
      emit pp_list_style_position position
    end;
    emit pp_list_style_image image;
    (* Everything was an initial value and got dropped: the shorthand still
       needs one token. Emit the type initial [disc] (the shortest spelling of
       the all-initial value), not the position initial [outside] - a lone
       [outside] would set the position, changing nothing, but [disc] is shorter
       and is the canonical single-value form. *)
    if !first then Pp.string ctx "disc"

(* CSS Lists 3 (ED) sec. 3.6: [list-style] is [<'list-style-position'> ||
   <'list-style-image'> || <'list-style-type'>], so a component left out of the
   shorthand takes its longhand initial - [outside] (sec. 3.5), [none] (sec.
   3.3) and [disc] (sec. 3.4). Writing an initial out names what leaving it out
   names, and leaving it out is the shorter spelling. *)
(* CSS Variables 1 sec. 3 syntax-checks a shorthand carrying a [var()] only
   after substitution, so which slot the substituted tokens fill is not known
   until computed-value time and a slot holding its initial is not spare. The
   reasoning above holds for a value with no [var()] in it and fails here:
   [list-style: disc outside var(--x)] with [--x: circle] substitutes to two
   [<list-style-type>] values and is invalid at computed-value time, leaving the
   longhands unset, where dropping the initials leaves [list-style: var(--x)],
   which substitutes to a valid [circle] and sets the type. Reordering stays
   sound -- CSS Lists 3 sec. 2 spells the shorthand with [||], so its components
   commute -- but nothing may leave. *)
let list_style_slot_is_var (s : list_style_shorthand) =
  (match s.type_ with Some (Var _ : list_style_type) -> true | _ -> false)
  || (match s.position with
    | Some (Var _ : list_style_position) -> true
    | _ -> false)
  || match s.image with Some (Var _ : list_style_image) -> true | _ -> false

let normalize_list_style_shorthand (s : list_style_shorthand) :
    list_style_shorthand =
  if list_style_slot_is_var s then s
  else
    let type_ =
      drop_default ~is_default:(fun (t : list_style_type) -> t = Disc) s.type_
    in
    let position =
      drop_default
        ~is_default:(fun (p : list_style_position) -> p = Outside)
        s.position
    in
    let image =
      drop_default ~is_default:(fun (i : list_style_image) -> i = None) s.image
    in
    if
      option_is_phys_same type_ s.type_
      && option_is_phys_same position s.position
      && option_is_phys_same image s.image
    then s
    else { type_; position; image }

let normalize_list_style : list_style -> list_style = function
  | Shorthand s as value ->
      let s' = normalize_list_style_shorthand s in
      if s' == s then value else Shorthand s'
  | value -> value

let rec pp_list_style : list_style Pp.t =
 fun ctx -> function
  | Shorthand sh -> pp_list_style_shorthand ctx sh
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_list_style ctx v

let read_symbols_type t : symbols_type =
  Cursor.enum "symbols type"
    [
      ("cyclic", (Cyclic : symbols_type));
      ("numeric", Numeric);
      ("alphabetic", Alphabetic);
      ("symbolic", Symbolic);
      ("fixed", Fixed);
    ]
    t

let read_list_style_symbol t : list_style_symbol =
  Cursor.one_of
    [
      (fun t -> (Url (Cursor.url t) : list_style_symbol));
      (fun t -> String (Cursor.string t));
    ]
    t

let list_style_type_keywords : (string * list_style_type) list =
  [
    ("none", (None : list_style_type));
    ("disc", Disc);
    ("circle", Circle);
    ("square", Square);
    ("decimal", Decimal);
    ("lower-alpha", Lower_alpha);
    ("upper-alpha", Upper_alpha);
    ("lower-roman", Lower_roman);
    ("upper-roman", Upper_roman);
    ("decimal-leading-zero", (Decimal_leading_zero : list_style_type));
    ("arabic-indic", (Arabic_indic : list_style_type));
    ("armenian", (Armenian : list_style_type));
    ("upper-armenian", (Upper_armenian : list_style_type));
    ("lower-armenian", (Lower_armenian : list_style_type));
    ("bengali", (Bengali : list_style_type));
    ("cambodian", (Cambodian : list_style_type));
    ("khmer", (Khmer : list_style_type));
    ("cjk-decimal", (Cjk_decimal : list_style_type));
    ("devanagari", (Devanagari : list_style_type));
    ("georgian", (Georgian : list_style_type));
    ("gujarati", (Gujarati : list_style_type));
    ("gurmukhi", (Gurmukhi : list_style_type));
    ("hebrew", (Hebrew : list_style_type));
    ("kannada", (Kannada : list_style_type));
    ("lao", (Lao : list_style_type));
    ("malayalam", (Malayalam : list_style_type));
    ("mongolian", (Mongolian : list_style_type));
    ("myanmar", (Myanmar : list_style_type));
    ("oriya", (Oriya : list_style_type));
    ("persian", (Persian : list_style_type));
    ("tamil", (Tamil : list_style_type));
    ("telugu", (Telugu : list_style_type));
    ("thai", (Thai : list_style_type));
    ("tibetan", (Tibetan : list_style_type));
    ("lower-latin", (Lower_latin : list_style_type));
    ("upper-latin", (Upper_latin : list_style_type));
    ("cjk-earthly-branch", (Cjk_earthly_branch : list_style_type));
    ("cjk-heavenly-stem", (Cjk_heavenly_stem : list_style_type));
    ("lower-greek", (Lower_greek : list_style_type));
    ("hiragana", (Hiragana : list_style_type));
    ("hiragana-iroha", (Hiragana_iroha : list_style_type));
    ("katakana", (Katakana : list_style_type));
    ("katakana-iroha", (Katakana_iroha : list_style_type));
    ("disclosure-open", (Disclosure_open : list_style_type));
    ("disclosure-closed", (Disclosure_closed : list_style_type));
    ("cjk-ideographic", (Cjk_ideographic : list_style_type));
    ("japanese-informal", (Japanese_informal : list_style_type));
    ("japanese-formal", (Japanese_formal : list_style_type));
    ("korean-hangul-formal", (Korean_hangul_formal : list_style_type));
    ("korean-hanja-informal", (Korean_hanja_informal : list_style_type));
    ("korean-hanja-formal", (Korean_hanja_formal : list_style_type));
    ("simp-chinese-informal", (Simp_chinese_informal : list_style_type));
    ("simp-chinese-formal", (Simp_chinese_formal : list_style_type));
    ("trad-chinese-informal", (Trad_chinese_informal : list_style_type));
    ("trad-chinese-formal", (Trad_chinese_formal : list_style_type));
    ("ethiopic-numeric", (Ethiopic_numeric : list_style_type));
    ("inherit", Inherit);
    ("initial", Initial);
    ("unset", Unset);
    ("revert", Revert);
    ("revert-layer", Revert_layer);
  ]

let rec read_list_style_type t : list_style_type =
  let read_var t : list_style_type = Var (read_var read_list_style_type t) in
  let read_symbols_body t : list_style_type =
    let kind = Cursor.option read_symbols_type t in
    Cursor.ws t;
    let symbols =
      Cursor.list ~sep:Cursor.ws ~at_least:1 read_list_style_symbol t
    in
    Symbols (kind, symbols)
  in
  Cursor.enum_or_var "list-style-type" list_style_type_keywords ~var:read_var
    ~default:
      (Cursor.one_of
         [
           (fun t -> Cursor.call "symbols" t read_symbols_body);
           (fun t -> (String (Cursor.string t) : list_style_type));
           (fun t ->
             (Name (Cursor.custom_ident "counter style name" t)
               : list_style_type));
         ])
    t

let rec read_list_style_position t : list_style_position =
  Cursor.enum_or_var "list-style-position"
    [
      ("inside", (Inside : list_style_position));
      ("outside", Outside);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_list_style_position t))
    t

(* CSS Lists 3 sec. 3.5 gives [list-style-image] an [<image>], so every image
   the [background-image] vocabulary reads goes here: the comma list is what
   only that property takes. *)
let rec read_list_style_image t : list_style_image =
  let read_var t : list_style_image = Var (read_var read_list_style_image t) in
  Cursor.one_of
    [
      (fun t ->
        Cursor.enum_or_calls "list-style-image"
          [
            ("none", (None : list_style_image));
            ("inherit", Inherit);
            ("initial", Initial);
            ("unset", Unset);
            ("revert", Revert);
            ("revert-layer", Revert_layer);
          ]
          ~calls:[ ("var", read_var) ]
          t);
      (fun t -> (Image (read_bg_image t) : list_style_image));
    ]
    t

(* Parse the [list-style] shorthand into a typed [list_style_shorthand] record.
   Each slot is recognised by the longhand reader; a single bare [none]
   populates both [type_] and [image] per CSS Lists 3 sec. 3.6. *)
let try_list_style_slot r read_fn (slot : 'a option ref) =
  if !slot <> Option.None then false
  else
    let pos = Cursor.save r in
    match read_fn r with
    | v ->
        slot := Some v;
        true
    | exception Cursor.Parse_error _ ->
        Cursor.restore r pos;
        false

(* CSS Values 4 sec. 2.2 takes each option of a [||] at most once, and sec. 3.6
   lands a [none] on whichever of the image and the type the shorthand does not
   otherwise set, so the two of them hold every [none] there is. *)
let resolve_list_style_nones r nones (type_ : list_style_type option ref)
    (image : list_style_image option ref) =
  let free =
    (if !type_ = Option.None then 1 else 0)
    + if !image = Option.None then 1 else 0
  in
  if nones > free then Cursor.err_invalid r "too many none in list-style";
  if nones > 0 then begin
    if !type_ = Option.None then type_ := Some (None : list_style_type);
    if !image = Option.None then image := Some (None : list_style_image)
  end

let read_list_style_shorthand r : list_style_shorthand =
  let type_ : list_style_type option ref = ref Option.None in
  let position : list_style_position option ref = ref Option.None in
  let image : list_style_image option ref = ref Option.None in
  let nones = ref 0 in
  let try_one () =
    try_list_style_slot r read_list_style_position position
    || try_list_style_slot r read_list_style_image image
    || try_list_style_slot r read_list_style_type type_
  in
  let rec consume () =
    Cursor.ws r;
    if Cursor.is_done r then ()
    else
      let saved = Cursor.save r in
      let kw = Cursor.peek_keyword r in
      if kw = Some "none" then begin
        let _ = Cursor.ident r in
        incr nones;
        consume ()
      end
      else if try_one () then consume ()
      else Cursor.restore r saved
  in
  consume ();
  Cursor.ws r;
  if not (Cursor.is_done r) then
    Cursor.err_invalid r "invalid list-style shorthand";
  resolve_list_style_nones r !nones type_ image;
  if
    !type_ = Option.None && !position = Option.None && !image = Option.None
    && !nones = 0
  then Cursor.err_invalid r "invalid list-style shorthand";
  { type_ = !type_; position = !position; image = !image }

let rec read_list_style t : list_style =
  let raw = Cursor.lookahead (Cursor.consume_to_decl_end ~trim:true) t in
  let lower = String.lowercase_ascii (String.trim raw) in
  match lower with
  | "inherit" ->
      ignore (Cursor.consume_to_decl_end ~trim:true t);
      Inherit
  | "initial" ->
      ignore (Cursor.consume_to_decl_end ~trim:true t);
      Initial
  | "unset" ->
      ignore (Cursor.consume_to_decl_end ~trim:true t);
      Unset
  | "revert" ->
      ignore (Cursor.consume_to_decl_end ~trim:true t);
      Revert
  | "revert-layer" ->
      ignore (Cursor.consume_to_decl_end ~trim:true t);
      Revert_layer
  | _ ->
      let is_valid_var () =
        let r = Cursor.of_string raw in
        match
          Values.read_var (fun r -> Cursor.consume_to_decl_end ~trim:true r) r
        with
        | (_ : string var) ->
            Cursor.ws r;
            Cursor.is_done r
        | exception Cursor.Parse_error _ -> false
      in
      if is_valid_var () then (
        let r = Cursor.of_string raw in
        let var = Values.read_var (fun r -> read_list_style r) r in
        ignore (Cursor.consume_to_decl_end ~trim:true t);
        Var var)
      else
        let body =
          try read_list_style_shorthand (Cursor.of_string raw)
          with Cursor.Parse_error _ ->
            Cursor.err_invalid t "invalid list-style shorthand"
        in
        ignore (Cursor.consume_to_decl_end ~trim:true t);
        Shorthand body
