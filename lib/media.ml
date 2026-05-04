(** Structured media conditions for type-safe media query construction.

    Implements CSS Media Queries Level 4/5:
    {v
    <media-query> = <media-condition>
                  | [ not | only ]? <media-type> [ and <media-condition-without-or> ]?
    <media-condition> = <media-not> | <media-in-parens> [ <media-and>* | <media-or>* ]
    <media-condition-without-or> = <media-not> | <media-in-parens> <media-and>*
    <media-not> = not <media-in-parens>
    <media-and> = and <media-in-parens>
    <media-or>  = or  <media-in-parens>
    <media-in-parens> = ( <media-condition> ) | <media-feature> | <general-enclosed>
    <media-feature> = ( <mf-plain> | <mf-boolean> | <mf-range> )
    <mf-range> = <mf-name> <op> <mf-value>
               | <mf-value> <op> <mf-name>
               | <mf-value> <lt-op> <mf-name> <lt-op> <mf-value>
               | <mf-value> <gt-op> <mf-name> <gt-op> <mf-value>
    v} *)

type cmp = Lt | Le | Eq | Gt | Ge

type value =
  | Length of Values_intf.length
  | Integer of int
  | Number of float
  | Ratio of int * int
  | Resolution_value of float * string
  | Ident of string

type feature =
  | Plain of string * value
  | Boolean of string
  | Range of string * cmp * value
  | Range_rev of value * cmp * string
  | Interval of value * cmp * string * cmp * value

type condition =
  | Feature of feature
  | Not of condition
  | And of condition * condition
  | Or of condition * condition

type medium = All | Screen | Print | Other of string
type prefix = Not | Only

type query =
  | Cond of condition
  | Type of {
      prefix : prefix option;
      type_ : medium;
      trailing : condition option;
    }
  | List of query list  (** Comma-separated media query list. *)

type t =
  | Width of Values_intf.length
  | Height of Values_intf.length
  | Min_width of float
  | Max_width of float
  | Not_min_width of float
  | Min_width_rem of float
  | Not_min_width_rem of float
  | Min_width_length of Values_intf.length
  | Not_min_width_length of Values_intf.length
  | Aspect_ratio of int * int
  | Resolution of float * string
  | Color of int
  | Color_index of int
  | Monochrome of int
  | Color_gamut of [ `Srgb | `P3 | `Rec2020 ]
  | Video_color_gamut of [ `Srgb | `P3 | `Rec2020 ]
  | Dynamic_range of [ `Standard | `High ]
  | Video_dynamic_range of [ `Standard | `High ]
  | Scan of [ `Interlace | `Progressive ]
  | Update of [ `None | `Slow | `Fast ]
  | Overflow_block of [ `None | `Scroll | `Optional_paged | `Paged ]
  | Overflow_inline of [ `None | `Scroll ]
  | Prefers_reduced_motion of [ `No_preference | `Reduce ]
  | Prefers_reduced_transparency of [ `No_preference | `Reduce ]
  | Prefers_reduced_data of [ `No_preference | `Reduce ]
  | Prefers_contrast of [ `No_preference | `Less | `More | `Custom ]
  | Prefers_color_scheme of [ `Dark | `Light ]
  | Forced_colors of [ `Active | `None ]
  | Inverted_colors of [ `Inverted | `None ]
  | Pointer of [ `None | `Coarse | `Fine ]
  | Any_pointer of [ `None | `Coarse | `Fine ]
  | Hover of [ `None | `Hover ]
  | Any_hover of [ `None | `Hover ]
  | Scripting of [ `None | `Initial_only | `Enabled ]
  | Nav_controls of [ `None | `Back_button ]
  | Print
  | Orientation of [ `Portrait | `Landscape ]
  | And of t * t
  | Or of t * t
  | Negated of t
  | Range of string * cmp * value
  | Range_rev of value * cmp * string
  | Interval of value * cmp * string * cmp * value
  | Type_query of {
      prefix : prefix option;
      type_ : medium;
      trailing : t option;
    }
  | Plain of string * value
  | Boolean of string
  | List of t list

(* ===== Formatting helpers ===== *)

let format_float f =
  if Float.is_integer f then Int.to_string (Float.to_int f)
  else Float.to_string f

let format_px = format_float
let format_rem = format_float

let cmp_to_string = function
  | Lt -> "<"
  | Le -> "<="
  | Eq -> "="
  | Gt -> ">"
  | Ge -> ">="

let medium_to_string : medium -> string = function
  | All -> "all"
  | Screen -> "screen"
  | Print -> "print"
  | Other s -> s

(* ===== Pretty printing ===== *)

let pp_length ctx l = Values.pp_length ~always:true ctx l

let pp_value : value Pp.t =
 fun ctx -> function
  | Length l -> pp_length ctx l
  | Integer i -> Pp.string ctx (Int.to_string i)
  | Number f -> Pp.string ctx (format_float f)
  | Ratio (n, d) ->
      Pp.string ctx (Int.to_string n);
      Pp.char ctx '/';
      Pp.string ctx (Int.to_string d)
  | Resolution_value (n, unit) ->
      Pp.string ctx (format_float n);
      Pp.string ctx unit
  | Ident s -> Pp.string ctx s

let pp_feature : feature Pp.t =
 fun ctx -> function
  | Plain (name, value) ->
      Pp.char ctx '(';
      Pp.string ctx name;
      Pp.char ctx ':';
      Pp.space_if_pretty ctx ();
      pp_value ctx value;
      Pp.char ctx ')'
  | Boolean name ->
      Pp.char ctx '(';
      Pp.string ctx name;
      Pp.char ctx ')'
  | Range (name, op, value) ->
      (* CSS Media Queries 4 §3.2: relational operators need whitespace so the
         tokenizer doesn't merge them with adjacent idents/numbers. *)
      Pp.char ctx '(';
      Pp.string ctx name;
      Pp.space ctx ();
      Pp.string ctx (cmp_to_string op);
      Pp.space ctx ();
      pp_value ctx value;
      Pp.char ctx ')'
  | Range_rev (value, op, name) ->
      Pp.char ctx '(';
      pp_value ctx value;
      Pp.space ctx ();
      Pp.string ctx (cmp_to_string op);
      Pp.space ctx ();
      Pp.string ctx name;
      Pp.char ctx ')'
  | Interval (a, op1, name, op2, b) ->
      Pp.char ctx '(';
      pp_value ctx a;
      Pp.space ctx ();
      Pp.string ctx (cmp_to_string op1);
      Pp.space ctx ();
      Pp.string ctx name;
      Pp.space ctx ();
      Pp.string ctx (cmp_to_string op2);
      Pp.space ctx ();
      pp_value ctx b;
      Pp.char ctx ')'

let rec pp_condition : condition Pp.t =
 fun ctx -> function
  | Feature f -> pp_feature ctx f
  | Not c ->
      Pp.string ctx "not ";
      pp_condition ctx c
  | And (a, b) ->
      pp_condition ctx a;
      Pp.string ctx " and ";
      pp_condition ctx b
  | Or (a, b) ->
      pp_condition ctx a;
      Pp.string ctx " or ";
      pp_condition ctx b

let rec pp_query : query Pp.t =
 fun ctx -> function
  | Cond c -> pp_condition ctx c
  | Type { prefix; type_; trailing } -> (
      (match prefix with
      | None -> ()
      | Some Not -> Pp.string ctx "not "
      | Some Only -> Pp.string ctx "only ");
      Pp.string ctx (medium_to_string type_);
      match trailing with
      | None -> ()
      | Some c ->
          Pp.string ctx " and ";
          pp_condition ctx c)
  | List qs ->
      let rec loop = function
        | [] -> ()
        | [ q ] -> pp_query ctx q
        | q :: rest ->
            pp_query ctx q;
            Pp.char ctx ',';
            Pp.space_if_pretty ctx ();
            loop rest
      in
      loop qs

let pp_named_feature ctx name value =
  Pp.char ctx '(';
  Pp.string ctx name;
  Pp.char ctx ':';
  Pp.space_if_pretty ctx ();
  Pp.string ctx value;
  Pp.char ctx ')'

let pp_min_width_length ctx l =
  Pp.char ctx '(';
  Pp.string ctx "min-width";
  Pp.char ctx ':';
  Pp.space_if_pretty ctx ();
  pp_length ctx l;
  Pp.char ctx ')'

let pp_length_feature ctx name l =
  Pp.char ctx '(';
  Pp.string ctx name;
  Pp.char ctx ':';
  Pp.space_if_pretty ctx ();
  pp_length ctx l;
  Pp.char ctx ')'

let rec pp ctx = function
  | Width l -> pp_length_feature ctx "width" l
  | Height l -> pp_length_feature ctx "height" l
  | Min_width px -> pp_named_feature ctx "min-width" (format_px px ^ "px")
  | Max_width px -> pp_named_feature ctx "max-width" (format_px px ^ "px")
  | Not_min_width px ->
      Pp.string ctx "not all and ";
      pp_named_feature ctx "min-width" (format_px px ^ "px")
  | Min_width_rem rem ->
      pp_named_feature ctx "min-width" (format_rem rem ^ "rem")
  | Not_min_width_rem rem ->
      Pp.string ctx "not all and ";
      pp_named_feature ctx "min-width" (format_rem rem ^ "rem")
  | Min_width_length l -> pp_min_width_length ctx l
  | Not_min_width_length l ->
      Pp.string ctx "not all and ";
      pp_min_width_length ctx l
  | Aspect_ratio (a, b) ->
      pp_named_feature ctx "aspect-ratio"
        (Int.to_string a ^ "/" ^ Int.to_string b)
  | Resolution (n, unit) ->
      pp_named_feature ctx "resolution" (format_float n ^ unit)
  | Color n -> pp_named_feature ctx "color" (Int.to_string n)
  | Color_index n -> pp_named_feature ctx "color-index" (Int.to_string n)
  | Monochrome n -> pp_named_feature ctx "monochrome" (Int.to_string n)
  | Color_gamut `Srgb -> pp_named_feature ctx "color-gamut" "srgb"
  | Color_gamut `P3 -> pp_named_feature ctx "color-gamut" "p3"
  | Color_gamut `Rec2020 -> pp_named_feature ctx "color-gamut" "rec2020"
  | Video_color_gamut `Srgb -> pp_named_feature ctx "video-color-gamut" "srgb"
  | Video_color_gamut `P3 -> pp_named_feature ctx "video-color-gamut" "p3"
  | Video_color_gamut `Rec2020 ->
      pp_named_feature ctx "video-color-gamut" "rec2020"
  | Dynamic_range `Standard -> pp_named_feature ctx "dynamic-range" "standard"
  | Dynamic_range `High -> pp_named_feature ctx "dynamic-range" "high"
  | Video_dynamic_range `Standard ->
      pp_named_feature ctx "video-dynamic-range" "standard"
  | Video_dynamic_range `High ->
      pp_named_feature ctx "video-dynamic-range" "high"
  | Scan `Interlace -> pp_named_feature ctx "scan" "interlace"
  | Scan `Progressive -> pp_named_feature ctx "scan" "progressive"
  | Update `None -> pp_named_feature ctx "update" "none"
  | Update `Slow -> pp_named_feature ctx "update" "slow"
  | Update `Fast -> pp_named_feature ctx "update" "fast"
  | Overflow_block `None -> pp_named_feature ctx "overflow-block" "none"
  | Overflow_block `Scroll -> pp_named_feature ctx "overflow-block" "scroll"
  | Overflow_block `Optional_paged ->
      pp_named_feature ctx "overflow-block" "optional-paged"
  | Overflow_block `Paged -> pp_named_feature ctx "overflow-block" "paged"
  | Overflow_inline `None -> pp_named_feature ctx "overflow-inline" "none"
  | Overflow_inline `Scroll -> pp_named_feature ctx "overflow-inline" "scroll"
  | Prefers_reduced_motion `No_preference ->
      pp_named_feature ctx "prefers-reduced-motion" "no-preference"
  | Prefers_reduced_motion `Reduce ->
      pp_named_feature ctx "prefers-reduced-motion" "reduce"
  | Prefers_reduced_transparency `No_preference ->
      pp_named_feature ctx "prefers-reduced-transparency" "no-preference"
  | Prefers_reduced_transparency `Reduce ->
      pp_named_feature ctx "prefers-reduced-transparency" "reduce"
  | Prefers_reduced_data `No_preference ->
      pp_named_feature ctx "prefers-reduced-data" "no-preference"
  | Prefers_reduced_data `Reduce ->
      pp_named_feature ctx "prefers-reduced-data" "reduce"
  | Prefers_contrast `No_preference ->
      pp_named_feature ctx "prefers-contrast" "no-preference"
  | Prefers_contrast `More -> pp_named_feature ctx "prefers-contrast" "more"
  | Prefers_contrast `Less -> pp_named_feature ctx "prefers-contrast" "less"
  | Prefers_contrast `Custom -> pp_named_feature ctx "prefers-contrast" "custom"
  | Prefers_color_scheme `Dark ->
      pp_named_feature ctx "prefers-color-scheme" "dark"
  | Prefers_color_scheme `Light ->
      pp_named_feature ctx "prefers-color-scheme" "light"
  | Forced_colors `Active -> pp_named_feature ctx "forced-colors" "active"
  | Forced_colors `None -> pp_named_feature ctx "forced-colors" "none"
  | Inverted_colors `Inverted ->
      pp_named_feature ctx "inverted-colors" "inverted"
  | Inverted_colors `None -> pp_named_feature ctx "inverted-colors" "none"
  | Pointer `None -> pp_named_feature ctx "pointer" "none"
  | Pointer `Coarse -> pp_named_feature ctx "pointer" "coarse"
  | Pointer `Fine -> pp_named_feature ctx "pointer" "fine"
  | Any_pointer `None -> pp_named_feature ctx "any-pointer" "none"
  | Any_pointer `Coarse -> pp_named_feature ctx "any-pointer" "coarse"
  | Any_pointer `Fine -> pp_named_feature ctx "any-pointer" "fine"
  | Hover `None -> pp_named_feature ctx "hover" "none"
  | Hover `Hover -> pp_named_feature ctx "hover" "hover"
  | Any_hover `None -> pp_named_feature ctx "any-hover" "none"
  | Any_hover `Hover -> pp_named_feature ctx "any-hover" "hover"
  | Scripting `None -> pp_named_feature ctx "scripting" "none"
  | Scripting `Initial_only -> pp_named_feature ctx "scripting" "initial-only"
  | Scripting `Enabled -> pp_named_feature ctx "scripting" "enabled"
  | Nav_controls `None -> pp_named_feature ctx "nav-controls" "none"
  | Nav_controls `Back_button -> pp_named_feature ctx "nav-controls" "back"
  | Print -> Pp.string ctx "print"
  | Orientation `Portrait -> pp_named_feature ctx "orientation" "portrait"
  | Orientation `Landscape -> pp_named_feature ctx "orientation" "landscape"
  | And (a, b) ->
      pp ctx a;
      Pp.string ctx " and ";
      pp ctx b
  | Or (a, b) ->
      pp ctx a;
      Pp.string ctx " or ";
      pp ctx b
  | Negated Print -> Pp.string ctx "not print"
  | Negated inner ->
      Pp.string ctx "not all and ";
      pp ctx inner
  | Range (name, op, value) -> pp_feature ctx (Range (name, op, value))
  | Range_rev (value, op, name) -> pp_feature ctx (Range_rev (value, op, name))
  | Interval (a, op1, name, op2, b) ->
      pp_feature ctx (Interval (a, op1, name, op2, b))
  | Type_query { prefix; type_; trailing } -> (
      (match prefix with
      | None -> ()
      | Some Not -> Pp.string ctx "not "
      | Some Only -> Pp.string ctx "only ");
      Pp.string ctx (medium_to_string type_);
      match trailing with
      | None -> ()
      | Some cond -> (
          Pp.string ctx " and ";
          (* CSS Media Queries 4 §3 [media-condition-without-or]: Or
             sub-expressions need explicit parens in this context. *)
          match cond with
          | Or _ ->
              Pp.char ctx '(';
              pp ctx cond;
              Pp.char ctx ')'
          | _ -> pp ctx cond))
  | Plain (name, value) -> pp_feature ctx (Plain (name, value))
  | Boolean name -> pp_feature ctx (Boolean name)
  | List qs ->
      let rec loop = function
        | [] -> ()
        | [ q ] -> pp ctx q
        | q :: rest ->
            pp ctx q;
            Pp.char ctx ',';
            Pp.space_if_pretty ctx ();
            loop rest
      in
      loop qs

let to_string t = Pp.to_string pp t

(* ===== Parser ===== *)

(* A lightweight character scanner sufficient for media-query syntax. *)
type scanner = { s : string; mutable pos : int }

type recovery_scope = Branch | Query_list

exception Parse_error of recovery_scope * string

let parse_error ?(scope = Branch) reason = raise (Parse_error (scope, reason))

let mk_scanner s = { s = String.trim s; pos = 0 }
let at_end sc = sc.pos >= String.length sc.s
let peek sc = if at_end sc then None else Some sc.s.[sc.pos]
let advance sc = sc.pos <- sc.pos + 1

let skip_ws sc =
  while
    (not (at_end sc))
    &&
    let c = sc.s.[sc.pos] in
    c = ' ' || c = '\t' || c = '\n' || c = '\r' || c = '\012'
  do
    advance sc
  done

let is_ident_start c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '-'

let is_ident_cont c = is_ident_start c || (c >= '0' && c <= '9')

let read_ident sc =
  skip_ws sc;
  let start = sc.pos in
  if at_end sc then ""
  else if not (is_ident_start sc.s.[sc.pos]) then ""
  else (
    advance sc;
    while (not (at_end sc)) && is_ident_cont sc.s.[sc.pos] do
      advance sc
    done;
    String.sub sc.s start (sc.pos - start))

let lookahead_ident sc kw =
  let kw_len = String.length kw in
  let s_len = String.length sc.s in
  if sc.pos + kw_len > s_len then false
  else
    let ok = ref true in
    for k = 0 to kw_len - 1 do
      if Char.lowercase_ascii sc.s.[sc.pos + k] <> Char.lowercase_ascii kw.[k]
      then ok := false
    done;
    !ok
    && (sc.pos + kw_len >= s_len
       ||
       let c = sc.s.[sc.pos + kw_len] in
       not (is_ident_cont c))

let consume_keyword sc kw = sc.pos <- sc.pos + String.length kw

(* Parse an mf-value: number/integer + optional unit, ratio, or ident. *)
let is_digit c = c >= '0' && c <= '9'

let consume_digits sc =
  let saw_digit = ref false in
  while (not (at_end sc)) && is_digit sc.s.[sc.pos] do
    saw_digit := true;
    advance sc
  done;
  !saw_digit

let consume_sign sc =
  match peek sc with Some ('+' | '-') -> advance sc | _ -> ()

let consume_exponent sc =
  match peek sc with
  | Some ('e' | 'E') ->
      let mark = sc.pos in
      advance sc;
      consume_sign sc;
      if not (consume_digits sc) then sc.pos <- mark
  | _ -> ()

let read_number_lit sc =
  let start = sc.pos in
  consume_sign sc;
  let saw_int = consume_digits sc in
  let saw_dot =
    match peek sc with
    | Some '.' ->
        advance sc;
        true
    | _ -> false
  in
  let saw_frac = saw_dot && consume_digits sc in
  consume_exponent sc;
  if not (saw_int || saw_frac) then (
    sc.pos <- start;
    None)
  else
    let txt = String.sub sc.s start (sc.pos - start) in
    let is_int =
      (not saw_dot) && not (String.contains txt 'e' || String.contains txt 'E')
    in
    if is_int then Some (`Int (int_of_string txt), txt)
    else Some (`Float (float_of_string txt), txt)

let read_unit sc =
  let start = sc.pos in
  if at_end sc then ""
  else if sc.s.[sc.pos] = '%' then (
    advance sc;
    "%")
  else if is_ident_start sc.s.[sc.pos] then (
    advance sc;
    while (not (at_end sc)) && is_ident_cont sc.s.[sc.pos] do
      advance sc
    done;
    String.sub sc.s start (sc.pos - start))
  else ""

let length_of_value f unit =
  let module L = Values_intf in
  match String.lowercase_ascii unit with
  | "px" -> Some (L.Px f)
  | "em" -> Some (L.Em f)
  | "rem" -> Some (L.Rem f)
  | "%" -> Some (L.Pct f)
  | "vw" -> Some (L.Vw f)
  | "vh" -> Some (L.Vh f)
  | "vmin" -> Some (L.Vmin f)
  | "vmax" -> Some (L.Vmax f)
  | "ch" -> Some (L.Ch f)
  | "ex" -> Some (L.Ex f)
  | "cm" -> Some (L.Cm f)
  | "mm" -> Some (L.Mm f)
  | "in" -> Some (L.In f)
  | "pt" -> Some (L.Pt f)
  | "pc" -> Some (L.Pc f)
  | "lh" -> Some (L.Lh f)
  | "q" -> Some (L.Q f)
  | "cap" -> Some (L.Cap f)
  | "ic" -> Some (L.Ic f)
  | "rlh" -> Some (L.Rlh f)
  | "vi" -> Some (L.Vi f)
  | "vb" -> Some (L.Vb f)
  | "dvh" -> Some (L.Dvh f)
  | "dvw" -> Some (L.Dvw f)
  | "dvmin" -> Some (L.Dvmin f)
  | "dvmax" -> Some (L.Dvmax f)
  | "lvh" -> Some (L.Lvh f)
  | "lvw" -> Some (L.Lvw f)
  | "lvmin" -> Some (L.Lvmin f)
  | "lvmax" -> Some (L.Lvmax f)
  | "svh" -> Some (L.Svh f)
  | "svw" -> Some (L.Svw f)
  | "svmin" -> Some (L.Svmin f)
  | "svmax" -> Some (L.Svmax f)
  | _ -> None

let resolution_units = [ "dpi"; "dpcm"; "dppx"; "x" ]

let read_value sc =
  skip_ws sc;
  match peek sc with
  | None -> None
  | Some c when (c >= '0' && c <= '9') || c = '.' || c = '+' || c = '-' -> (
      match read_number_lit sc with
      | None -> None
      | Some (num, _) -> (
          let unit = read_unit sc in
          if unit = "" then (
            (* Could be a ratio: "n / m" *)
            let mark = sc.pos in
            skip_ws sc;
            match peek sc with
            | Some '/' -> (
                advance sc;
                skip_ws sc;
                match read_number_lit sc with
                | Some (`Int d, _) ->
                    let n =
                      match num with `Int n -> n | `Float f -> int_of_float f
                    in
                    Some (Ratio (n, d))
                | _ ->
                    sc.pos <- mark;
                    Some
                      (match num with
                      | `Int n -> Integer n
                      | `Float f -> Number f))
            | _ ->
                sc.pos <- mark;
                Some
                  (match num with `Int n -> Integer n | `Float f -> Number f))
          else
            let f = match num with `Int n -> float_of_int n | `Float f -> f in
            match length_of_value f unit with
            | Some l -> Some (Length l)
            | None ->
                if List.mem (String.lowercase_ascii unit) resolution_units then
                  Some (Resolution_value (f, unit))
                else None))
  | Some _ ->
      let id = read_ident sc in
      if id = "" then None else Some (Ident id)

let value_of_string s =
  let sc = mk_scanner s in
  match read_value sc with
  | Some value ->
      skip_ws sc;
      if at_end sc then value else failwith ("invalid media value: " ^ s)
  | None -> failwith ("invalid media value: " ^ s)

let boolean_feature name : feature = Boolean name

(* Read balanced parens content into a string. Assumes '(' already consumed. *)
let read_balanced sc =
  let buf = Buffer.create 32 in
  let depth = ref 1 in
  let continue = ref true in
  while !continue do
    match peek sc with
    | None -> failwith "Unmatched parenthesis in @media condition"
    | Some '(' ->
        incr depth;
        Buffer.add_char buf '(';
        advance sc
    | Some ')' ->
        decr depth;
        if !depth = 0 then (
          advance sc;
          continue := false)
        else (
          Buffer.add_char buf ')';
          advance sc)
    | Some c ->
        Buffer.add_char buf c;
        advance sc
  done;
  Buffer.contents buf

let read_cmp sc =
  skip_ws sc;
  match peek sc with
  | Some '<' ->
      advance sc;
      if peek sc = Some '=' then (
        advance sc;
        Some Le)
      else Some Lt
  | Some '>' ->
      advance sc;
      if peek sc = Some '=' then (
        advance sc;
        Some Ge)
      else Some Gt
  | Some '=' ->
      advance sc;
      Some Eq
  | _ -> None

let interval_ops_compatible op1 op2 =
  match (op1, op2) with
  | (Lt | Le), (Lt | Le) | (Gt | Ge), (Gt | Ge) -> true
  | _ -> false

let starts_with ~prefix s =
  let len = String.length prefix in
  String.length s >= len && String.sub s 0 len = prefix

let range_feature_name name =
  match String.lowercase_ascii name with
  | "width" | "height" | "inline-size" | "block-size" | "aspect-ratio"
  | "resolution" | "color" | "color-index" | "monochrome"
  | "horizontal-viewport-segments" | "vertical-viewport-segments" ->
      true
  | _ -> false

let prefixed_range_feature_name name =
  let name = String.lowercase_ascii name in
  if starts_with ~prefix:"min-" name || starts_with ~prefix:"max-" name then
    let base = String.sub name 4 (String.length name - 4) in
    Some base
  else None

let validate_plain_feature name value =
  let plain_name =
    match prefixed_range_feature_name name with
    | Some base when range_feature_name base -> base
    | Some _ -> name
    | None -> name
  in
  let valid_numeric_value name value =
    match (String.lowercase_ascii name, value) with
    | ("width" | "height" | "inline-size" | "block-size"), Length _ -> true
    | "aspect-ratio", Ratio _ -> true
    | "resolution", Resolution_value _ -> true
    | "resolution", Ident s -> String.lowercase_ascii s = "infinite"
    | ("color" | "color-index" | "monochrome"), Integer n -> n >= 0
    | ( "horizontal-viewport-segments" | "vertical-viewport-segments" ),
      Integer n ->
        n >= 0
    | _ -> false
  in
  let valid_plain_numeric_value name value =
    match (String.lowercase_ascii name, value) with
    | ( "width" | "height" | "min-width" | "max-width" | "inline-size"
      | "block-size" ),
      Length _ ->
        true
    | "aspect-ratio", Ratio _ -> true
    | "resolution", Resolution_value _ -> true
    | "resolution", Ident s -> String.lowercase_ascii s = "infinite"
    | ("color" | "color-index" | "monochrome"), Integer n -> n >= 0
    | "grid", Integer (0 | 1) -> true
    | ( "horizontal-viewport-segments" | "vertical-viewport-segments" ),
      Integer n ->
        n >= 0
    | _ -> false
  in
  let ident_value =
    match value with Ident s -> Some (String.lowercase_ascii s) | _ -> None
  in
  let one_of values =
    match ident_value with Some s -> List.mem s values | None -> false
  in
  match prefixed_range_feature_name name with
  | Some base when range_feature_name base -> valid_numeric_value base value
  | Some _ -> false
  | None -> (
  match String.lowercase_ascii plain_name with
  | "width" | "height" | "min-width" | "max-width" | "inline-size"
  | "block-size" | "aspect-ratio" | "resolution" | "color" | "color-index"
  | "monochrome" | "grid" | "horizontal-viewport-segments"
  | "vertical-viewport-segments" ->
      valid_plain_numeric_value name value
  | "orientation" -> one_of [ "portrait"; "landscape" ]
  | "hover" | "any-hover" -> one_of [ "none"; "hover" ]
  | "pointer" | "any-pointer" -> one_of [ "none"; "coarse"; "fine" ]
  | "update" -> one_of [ "none"; "slow"; "fast" ]
  | "overflow-block" -> one_of [ "none"; "scroll"; "paged" ]
  | "overflow-inline" -> one_of [ "none"; "scroll" ]
  | "scan" -> one_of [ "interlace"; "progressive" ]
  | "color-gamut" | "video-color-gamut" -> one_of [ "srgb"; "p3"; "rec2020" ]
  | "dynamic-range" | "video-dynamic-range" -> one_of [ "standard"; "high" ]
  | "display-mode" ->
      one_of
        [
          "fullscreen";
          "standalone";
          "minimal-ui";
          "browser";
          "picture-in-picture";
        ]
  | "environment-blending" -> one_of [ "opaque"; "additive"; "subtractive" ]
  | "prefers-color-scheme" -> one_of [ "light"; "dark" ]
  | "prefers-reduced-motion" | "prefers-reduced-transparency"
  | "prefers-reduced-data" ->
      one_of [ "no-preference"; "reduce" ]
  | "prefers-contrast" -> one_of [ "no-preference"; "less"; "more"; "custom" ]
  | "forced-colors" -> one_of [ "none"; "active" ]
  | "inverted-colors" -> one_of [ "none"; "inverted" ]
  | "nav-controls" -> one_of [ "none"; "back" ]
  | "scripting" -> one_of [ "none"; "initial-only"; "enabled" ]
  | _ -> true)

let validate_range_feature name value =
  range_feature_name name && validate_plain_feature name value

(* Smart constructor for [Plain] features: rejects values that
   {!validate_plain_feature} reports as outside the feature's grammar (e.g.
   [feature "orientation" (Ident "sideways")]). *)
let plain_feature name value : feature =
  if not (validate_plain_feature name value) then
    invalid_arg
      ("Media.feature: value rejected by " ^ name ^ "'s grammar (see "
     ^ "Media.validate_plain_feature)");
  Plain (name, value)

(* Parse content already inside parens (no surrounding parens). *)
let parse_inside_parens content : feature option =
  let sc = mk_scanner content in
  skip_ws sc;
  (* Try value-first form: V op name [op V] *)
  let mark = sc.pos in
  match read_value sc with
  | Some v1 -> (
      match read_cmp sc with
      | Some op1 -> (
          skip_ws sc;
          let name = read_ident sc in
          if name = "" then (
            sc.pos <- mark;
            None)
          else
            match read_cmp sc with
            | Some op2 -> (
                skip_ws sc;
                  match read_value sc with
                  | Some v2 ->
                    skip_ws sc;
                    if
                      at_end sc && interval_ops_compatible op1 op2
                      && validate_range_feature name v1
                      && validate_range_feature name v2
                    then
                      Some (Interval (v1, op1, name, op2, v2))
                    else None
                | None -> None)
            | None ->
                skip_ws sc;
                if at_end sc && validate_range_feature name v1 then
                  Some (Range_rev (v1, op1, name))
                else None)
      | None ->
          sc.pos <- mark;
          None)
  | None -> None

let parse_feature_in_parens content =
  let sc = mk_scanner content in
  skip_ws sc;
  if at_end sc then None
  else
    let id = read_ident sc in
    if id <> "" then (
      skip_ws sc;
      match peek sc with
      | None ->
          if starts_with ~prefix:"min-" id || starts_with ~prefix:"max-" id then
            None
          else Some (boolean_feature id)
      | Some ':' -> (
          advance sc;
          skip_ws sc;
          let value_start = sc.pos in
          let v = read_value sc in
          match v with
          | Some value ->
              let raw_value =
                String.sub sc.s value_start (sc.pos - value_start)
                |> String.trim
              in
              ignore raw_value;
              skip_ws sc;
              if at_end sc && validate_plain_feature id value then
                Some (plain_feature id value)
              else None
          | None -> None)
      | Some _ -> (
          match read_cmp sc with
          | Some op -> (
              skip_ws sc;
              match read_value sc with
              | Some v2 -> (
                  match read_cmp sc with
                  | None ->
                      skip_ws sc;
                      if at_end sc && validate_range_feature id v2 then
                        Some (Range (id, op, v2))
                      else None
                  | Some _ -> None)
              | None -> None)
          | None -> None))
    else parse_inside_parens content

(* Parser for media-condition (sequence of (...) with and/or/not). *)
let rec parse_in_parens sc =
  skip_ws sc;
  match peek sc with
  | Some '(' -> (
      advance sc;
      let content = read_balanced sc in
      let trimmed = String.trim content in
      (* Could be either ( <condition> ) or ( <feature> ). *)
      let inner = mk_scanner trimmed in
      skip_ws inner;
      if
        lookahead_ident inner "not"
        || lookahead_ident inner "and"
        || lookahead_ident inner "or"
      then Feature (extract_feature_or_fail trimmed)
      else
        let starts_with_paren = peek inner = Some '(' in
        if starts_with_paren then parse_condition_str trimmed
        else
          match parse_feature_in_parens trimmed with
          | Some f -> Feature f
          | None -> failwith ("invalid media feature: " ^ trimmed))
  | _ -> failwith "expected '(' in media condition"

and extract_feature_or_fail content =
  match parse_feature_in_parens content with
  | Some f -> f
  | None -> failwith ("invalid media feature: " ^ content)

and parse_condition_str s =
  let sc = mk_scanner s in
  parse_condition sc

and parse_condition sc =
  skip_ws sc;
  if lookahead_ident sc "not" then (
    consume_keyword sc "not";
    skip_ws sc;
    let inner = parse_in_parens sc in
    skip_ws sc;
    if at_end sc then Not inner
    else failwith "trailing content after 'not <media-in-parens>'")
  else
    let left = parse_in_parens sc in
    chain sc left

and chain sc (acc : condition) =
  let rec loop op (acc : condition) : condition =
    skip_ws sc;
    if at_end sc then acc
    else if lookahead_ident sc "and" then (
      (match op with
      | Some `Or -> failwith "mixed 'and'/'or' media condition"
      | _ -> ());
      consume_keyword sc "and";
      let right = parse_in_parens sc in
      loop (Some `And) (And (acc, right) : condition))
    else if lookahead_ident sc "or" then (
      (match op with
      | Some `And -> failwith "mixed 'and'/'or' media condition"
      | _ -> ());
      consume_keyword sc "or";
      let right = parse_in_parens sc in
      loop (Some `Or) (Or (acc, right) : condition))
    else acc
  in
  loop None acc

let medium_of_ident s : medium =
  match String.lowercase_ascii s with
  | "all" -> All
  | "screen" -> Screen
  | "print" -> Print
  | other -> Other other

let is_reserved_media_type_keyword s =
  match String.lowercase_ascii s with
  | "and" | "or" -> true
  | _ -> false

(* [<media-query>] starts as [<media-condition>] (rather than as a media
   type) when its first non-space token is '(' or "not (". *)
let starts_with_condition sc =
  if peek sc = Some '(' then true
  else if lookahead_ident sc "not" then
    let p = sc.pos + 3 in
    let len = String.length sc.s in
    let rec next_non_ws i =
      if i >= len then None
      else
        let c = sc.s.[i] in
        if c = ' ' || c = '\t' then next_non_ws (i + 1) else Some c
    in
    next_non_ws p = Some '('
  else false

let read_query_prefix sc =
  if lookahead_ident sc "not" then (
    consume_keyword sc "not";
    skip_ws sc;
    Some Not)
  else if lookahead_ident sc "only" then (
    consume_keyword sc "only";
    skip_ws sc;
    Some Only)
  else None

let read_media_type_query sc =
  let prefix = read_query_prefix sc in
  (match prefix with
  | Some _ when lookahead_ident sc "not" || lookahead_ident sc "only" ->
      parse_error ~scope:Query_list "duplicate media query prefix"
  | _ -> ());
  let id = read_ident sc in
  if id = "" then failwith "expected media type or condition"
  else if is_reserved_media_type_keyword id then
    failwith "reserved media condition keyword cannot be a media type"
  else
    let type_ = medium_of_ident id in
    skip_ws sc;
    if at_end sc || peek sc = Some ',' then
      Type { prefix; type_; trailing = None }
    else if lookahead_ident sc "and" then (
      consume_keyword sc "and";
      let cond = parse_in_parens sc in
      let cond = chain sc cond in
      Type { prefix; type_; trailing = Some cond })
    else
      failwith
        (String.concat ""
           [
             "expected 'and' or end of query after media type, got: ";
             String.sub sc.s sc.pos (String.length sc.s - sc.pos);
           ])

let parse_single_query sc =
  skip_ws sc;
  if at_end sc then failwith "empty media query"
  else if starts_with_condition sc then Cond (parse_condition sc)
  else read_media_type_query sc

let not_all_query : query = Type { prefix = Some Not; type_ = All; trailing = None }

let skip_recovery_branch sc =
  let depth = ref 0 in
  let continue = ref true in
  while (not (at_end sc)) && !continue do
    match peek sc with
    | Some '(' ->
        incr depth;
        advance sc
    | Some ')' ->
        if !depth > 0 then decr depth;
        advance sc
    | Some ',' when !depth = 0 -> continue := false
    | Some _ -> advance sc
    | None -> continue := false
  done

let parse_query_branch ~recover ~recovered_at_eof sc =
  let mark = sc.pos in
  try
    let query = parse_single_query sc in
    skip_ws sc;
    match peek sc with
    | None | Some ',' -> query
    | _ -> failwith "trailing content in media query branch"
  with
  | Parse_error (scope, reason) ->
      if not recover then raise (Failure reason);
      if scope = Query_list then sc.pos <- String.length sc.s
      else (
        sc.pos <- mark;
        skip_recovery_branch sc);
      skip_ws sc;
      if at_end sc then recovered_at_eof := true;
      not_all_query
  | Failure _ ->
      if not recover then raise (Failure "invalid media query branch");
      sc.pos <- mark;
      skip_recovery_branch sc;
      skip_ws sc;
      if at_end sc then recovered_at_eof := true;
      not_all_query

let trailing_content_failure sc =
  failwith
    (String.concat ""
       [
         "trailing content in media query: ";
         String.sub sc.s sc.pos (String.length sc.s - sc.pos);
       ])

let parse_query_str ?(recover = true) s : query =
  let sc = mk_scanner s in
  if at_end sc then (List [] : query)
  else
    let recovered_at_eof = ref false in
    let branch () = parse_query_branch ~recover ~recovered_at_eof sc in
    let first = branch () in
    skip_ws sc;
    if at_end sc then first
    else if peek sc = Some ',' then (
      let rec loop acc =
        match peek sc with
        | Some ',' ->
            advance sc;
            skip_ws sc;
            let q = branch () in
            skip_ws sc;
            loop (q :: acc)
        | _ -> List.rev acc
      in
      let rest = loop [] in
      skip_ws sc;
      if not (at_end sc) then trailing_content_failure sc;
      if !recovered_at_eof then not_all_query else List (first :: rest))
    else trailing_content_failure sc

let normalise_preference_value name s =
  match name with
  | "prefers-reduced-motion" -> (
      match s with
      | "no-preference" -> Some (Prefers_reduced_motion `No_preference)
      | "reduce" -> Some (Prefers_reduced_motion `Reduce)
      | _ -> None)
  | "prefers-contrast" -> (
      match s with
      | "more" -> Some (Prefers_contrast `More)
      | "less" -> Some (Prefers_contrast `Less)
      | _ -> None)
  | "prefers-color-scheme" -> (
      match s with
      | "dark" -> Some (Prefers_color_scheme `Dark)
      | "light" -> Some (Prefers_color_scheme `Light)
      | _ -> None)
  | "prefers-reduced-transparency" -> (
      match s with
      | "no-preference" -> Some (Prefers_reduced_transparency `No_preference)
      | "reduce" -> Some (Prefers_reduced_transparency `Reduce)
      | _ -> None)
  | "prefers-reduced-data" -> (
      match s with
      | "no-preference" -> Some (Prefers_reduced_data `No_preference)
      | "reduce" -> Some (Prefers_reduced_data `Reduce)
      | _ -> None)
  | "forced-colors" -> (
      match s with
      | "active" -> Some (Forced_colors `Active)
      | "none" -> Some (Forced_colors `None)
      | _ -> None)
  | "inverted-colors" -> (
      match s with
      | "inverted" -> Some (Inverted_colors `Inverted)
      | "none" -> Some (Inverted_colors `None)
      | _ -> None)
  | _ -> None

let normalise_capability_value name s =
  match name with
  | "pointer" -> (
      match s with
      | "none" -> Some (Pointer `None)
      | "coarse" -> Some (Pointer `Coarse)
      | "fine" -> Some (Pointer `Fine)
      | _ -> None)
  | "any-pointer" -> (
      match s with
      | "none" -> Some (Any_pointer `None)
      | "coarse" -> Some (Any_pointer `Coarse)
      | "fine" -> Some (Any_pointer `Fine)
      | _ -> None)
  | "hover" -> (
      match s with
      | "none" -> Some (Hover `None)
      | "hover" -> Some (Hover `Hover)
      | _ -> None)
  | "any-hover" -> (
      match s with
      | "none" -> Some (Any_hover `None)
      | "hover" -> Some (Any_hover `Hover)
      | _ -> None)
  | "scripting" -> (
      match s with
      | "none" -> Some (Scripting `None)
      | "initial-only" -> Some (Scripting `Initial_only)
      | "enabled" -> Some (Scripting `Enabled)
      | _ -> None)
  | "nav-controls" -> (
      match s with
      | "none" -> Some (Nav_controls `None)
      | "back" -> Some (Nav_controls `Back_button)
      | _ -> None)
  | "orientation" -> (
      match s with
      | "portrait" -> Some (Orientation `Portrait)
      | "landscape" -> Some (Orientation `Landscape)
      | _ -> None)
  | _ -> None

let normalise_display_value name s =
  match name with
  | "color-gamut" -> (
      match s with
      | "srgb" -> Some (Color_gamut `Srgb)
      | "p3" -> Some (Color_gamut `P3)
      | "rec2020" -> Some (Color_gamut `Rec2020)
      | _ -> None)
  | "video-color-gamut" -> (
      match s with
      | "srgb" -> Some (Video_color_gamut `Srgb)
      | "p3" -> Some (Video_color_gamut `P3)
      | "rec2020" -> Some (Video_color_gamut `Rec2020)
      | _ -> None)
  | "dynamic-range" -> (
      match s with
      | "standard" -> Some (Dynamic_range `Standard)
      | "high" -> Some (Dynamic_range `High)
      | _ -> None)
  | "video-dynamic-range" -> (
      match s with
      | "standard" -> Some (Video_dynamic_range `Standard)
      | "high" -> Some (Video_dynamic_range `High)
      | _ -> None)
  | "scan" -> (
      match s with
      | "interlace" -> Some (Scan `Interlace)
      | "progressive" -> Some (Scan `Progressive)
      | _ -> None)
  | "update" -> (
      match s with
      | "none" -> Some (Update `None)
      | "slow" -> Some (Update `Slow)
      | "fast" -> Some (Update `Fast)
      | _ -> None)
  | "overflow-block" -> (
      match s with
      | "none" -> Some (Overflow_block `None)
      | "scroll" -> Some (Overflow_block `Scroll)
      | "optional-paged" -> Some (Overflow_block `Optional_paged)
      | "paged" -> Some (Overflow_block `Paged)
      | _ -> None)
  | "overflow-inline" -> (
      match s with
      | "none" -> Some (Overflow_inline `None)
      | "scroll" -> Some (Overflow_inline `Scroll)
      | _ -> None)
  | _ -> None

let normalise_ident_value name s =
  let s = String.lowercase_ascii s in
  match normalise_preference_value name s with
  | Some _ as v -> v
  | None -> (
      match normalise_capability_value name s with
      | Some _ as v -> v
      | None -> normalise_display_value name s)

(* Smart constructor: try to recognise a known shorthand, otherwise wrap as
   Custom. *)
let normalise_value name value =
  match (String.lowercase_ascii name, value) with
  | "min-width", Length (Values_intf.Px f) -> Some (Min_width f)
  | "max-width", Length (Values_intf.Px f) -> Some (Max_width f)
  | "min-width", Length (Values_intf.Rem f) -> Some (Min_width_rem f)
  | "min-width", Length l -> Some (Min_width_length l)
  | "width", Length l -> Some (Width l)
  | "height", Length l -> Some (Height l)
  | "aspect-ratio", Ratio (a, b) -> Some (Aspect_ratio (a, b))
  | "resolution", Resolution_value (n, u) -> Some (Resolution (n, u))
  | "color", Integer n -> Some (Color n)
  | "color-index", Integer n -> Some (Color_index n)
  | "monochrome", Integer n -> Some (Monochrome n)
  | name, Ident s -> normalise_ident_value name s
  | _ -> None

let rec feature_to_t : feature -> t = function
  | Plain (name, value) -> (
      match normalise_value name value with
      | Some t -> t
      | None -> Plain (name, value))
  | Boolean name -> (Boolean name : t)
  | Range (name, op, value) -> Range (name, op, value)
  | Range_rev (value, op, name) -> Range_rev (value, op, name)
  | Interval (a, op1, name, op2, b) -> Interval (a, op1, name, op2, b)

and condition_to_t : condition -> t = function
  | Feature f -> feature_to_t f
  | Not cond -> Negated (condition_to_t cond)
  | And (a, b) -> And (condition_to_t a, condition_to_t b)
  | Or (a, b) -> Or (condition_to_t a, condition_to_t b)

let rec collapse_query (q : query) : t =
  match q with
  | Cond cond -> condition_to_t cond
  | Type { prefix = Some Not; type_ = All; trailing = Some cond } -> (
      match condition_to_t cond with
      | Min_width f -> Not_min_width f
      | Min_width_rem f -> Not_min_width_rem f
      | Min_width_length l -> Not_min_width_length l
      | other -> Negated other)
  | Type { prefix = Some Not; type_ = Print; trailing = None } -> Negated Print
  | Type { prefix = None; type_ = Print; trailing = None } -> Print
  | Type { prefix; type_; trailing } ->
      Type_query
        { prefix; type_; trailing = Option.map condition_to_t trailing }
  | List qs -> List (List.map collapse_query qs)

let of_string s = collapse_query (parse_query_str s)
let of_string_strict s = collapse_query (parse_query_str ~recover:false s)

let of_function_body s =
  match parse_feature_in_parens s with
  | Some feature -> feature_to_t feature
  | None -> of_string s

let feature name value : t = feature_to_t (plain_feature name value)
let boolean name : t = feature_to_t (Boolean name : feature)

(* ===== Sorting / classification ===== *)

type kind =
  | Hover
  | Responsive of int * float
  | Responsive_max of int * float
  | Preference_accessibility
  | Preference_appearance
  | Other

let length_sort_key (l : Values_intf.length) =
  match l with
  | Calc _ -> (-2, 0.)
  | Em v -> (-1, v)
  | Px v -> (0, v)
  | Rem v -> (1, v)
  | Vh v -> (2, v)
  | Vw v -> (3, v)
  | Cm v -> (4, v)
  | Mm v -> (5, v)
  | In v -> (6, v)
  | Pt v -> (7, v)
  | _ -> (100, 0.)

let value_sort_key = function
  | Length l -> length_sort_key l
  | Integer i -> (0, float_of_int i)
  | Number f -> (0, f)
  | Ratio (n, d) when d <> 0 -> (0, float_of_int n /. float_of_int d)
  | Ratio _ -> (0, 0.)
  | Resolution_value (f, _) -> (0, f)
  | Ident _ -> (100, 0.)

let rec kind : t -> kind = function
  | Hover _ | Any_hover _ -> Hover
  | Min_width px -> Responsive (0, px)
  | Max_width px -> Responsive_max (0, px)
  | Not_min_width px -> Responsive_max (0, px)
  | Min_width_rem rem -> Responsive (0, rem *. 16.)
  | Not_min_width_rem rem -> Responsive_max (0, rem *. 16.)
  | Min_width_length l ->
      let u, v = length_sort_key l in
      Responsive (u, v)
  | Not_min_width_length l ->
      let u, v = length_sort_key l in
      Responsive_max (u, v)
  | Prefers_reduced_motion _ | Prefers_contrast _ | Forced_colors _
  | Inverted_colors _ | Pointer _ | Any_pointer _ | Scripting _ ->
      Preference_accessibility
  | Prefers_color_scheme _ -> Preference_appearance
  | Width l | Height l ->
      let u, v = length_sort_key l in
      Responsive (u, v)
  | Aspect_ratio _ | Resolution _ | Color _ | Color_index _ | Monochrome _
  | Color_gamut _ | Video_color_gamut _ | Dynamic_range _
  | Video_dynamic_range _ | Scan _ | Update _ | Overflow_block _
  | Overflow_inline _ | Prefers_reduced_transparency _ | Prefers_reduced_data _
  | Nav_controls _ | Print | Orientation _ | Type_query _ | Boolean _ | List _
    ->
      Other
  | Plain (name, value) -> (
      match String.lowercase_ascii name with
      | "min-width" | "max-width" | "width" ->
          let u, v = value_sort_key value in
          Responsive (u, v)
      | "prefers-color-scheme" -> Preference_appearance
      | "prefers-reduced-motion" | "prefers-contrast" | "forced-colors"
      | "inverted-colors" | "pointer" | "any-pointer" | "scripting" ->
          Preference_accessibility
      | "hover" -> Hover
      | _ -> Other)
  | Range (name, _, value) | Range_rev (value, _, name) -> (
      match String.lowercase_ascii name with
      | "width" | "min-width" | "max-width" ->
          let u, v = value_sort_key value in
          Responsive (u, v)
      | _ -> Other)
  | Interval (lo, _, name, _, _) -> (
      match String.lowercase_ascii name with
      | "width" | "min-width" | "max-width" ->
          let u, v = value_sort_key lo in
          Responsive (u, v)
      | _ -> Other)
  | And (a, b) | Or (a, b) -> (
      match (kind a, kind b) with
      | Other, other | other, Other -> other
      | ka, _ -> ka)
  | Negated inner -> kind inner

let group_order = function
  | Hover -> (0, 0.)
  | Other -> (500, 0.)
  | Preference_accessibility -> (1000, 0.)
  | Responsive_max (unit_ord, value) ->
      (1999, (Float.of_int unit_ord *. 1e9) +. value)
  | Responsive (unit_ord, value) ->
      (2000, (Float.of_int unit_ord *. 1e9) +. value)
  | Preference_appearance -> (3000, 0.)

let rec preference_order = function
  | Prefers_reduced_motion `No_preference -> 0
  | Prefers_reduced_motion `Reduce -> 1
  | Prefers_contrast `More -> 2
  | Prefers_contrast `Less -> 3
  | Prefers_color_scheme _ -> 4
  | Forced_colors _ -> 5
  | Inverted_colors _ -> 6
  | Pointer `None -> 7
  | Pointer `Coarse -> 8
  | Pointer `Fine -> 9
  | Any_pointer `None -> 10
  | Any_pointer `Coarse -> 11
  | Any_pointer `Fine -> 12
  | Scripting `None -> 13
  | Scripting `Initial_only -> 14
  | Scripting `Enabled -> 15
  | Hover _ | Any_hover _ -> 16
  | Prefers_reduced_transparency `No_preference -> 17
  | Prefers_reduced_transparency `Reduce -> 18
  | Prefers_reduced_data `No_preference -> 19
  | Prefers_reduced_data `Reduce -> 20
  | Nav_controls _ -> 21
  | And (a, _) | Or (a, _) -> preference_order a
  | Negated inner -> preference_order inner
  | _ -> 30

let rec responsive_subkind = function
  | Not_min_width _ | Not_min_width_rem _ | Not_min_width_length _ -> 0
  | Max_width _ -> 1
  | Min_width _ | Min_width_rem _ | Min_width_length _ -> 2
  | And (a, _) | Or (a, _) -> responsive_subkind a
  | Negated inner -> responsive_subkind inner
  | _ -> 2

let compare a b =
  let ka = kind a and kb = kind b in
  let ga, va = group_order ka and gb, vb = group_order kb in
  let group_cmp = Int.compare ga gb in
  if group_cmp <> 0 then group_cmp
  else
    let value_cmp = Float.compare va vb in
    if value_cmp <> 0 then value_cmp
    else
      let sub_cmp = Int.compare (responsive_subkind a) (responsive_subkind b) in
      if sub_cmp <> 0 then sub_cmp
      else
        let pref_cmp = Int.compare (preference_order a) (preference_order b) in
        if pref_cmp <> 0 then pref_cmp
        else String.compare (to_string a) (to_string b)

let equal a b = compare a b = 0
