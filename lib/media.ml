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

open Syntax

type cmp = Lt | Le | Eq | Gt | Ge

type name =
  | Width
  | Height
  | Inline_size
  | Block_size
  | Aspect_ratio
  | Resolution
  | Color
  | Color_index
  | Monochrome
  | Grid
  | Horizontal_viewport_segments
  | Vertical_viewport_segments
  | Orientation
  | Hover
  | Any_hover
  | Pointer
  | Any_pointer
  | Update
  | Overflow_block
  | Overflow_inline
  | Scan
  | Color_gamut
  | Video_color_gamut
  | Dynamic_range
  | Video_dynamic_range
  | Display_mode
  | Environment_blending
  | Prefers_color_scheme
  | Prefers_reduced_motion
  | Prefers_reduced_transparency
  | Prefers_reduced_data
  | Prefers_contrast
  | Forced_colors
  | Inverted_colors
  | Nav_controls
  | Scripting
  | Min of name
  | Max of name
  | Other of string

type ident =
  | Infinite
  | Portrait
  | Landscape
  | None
  | Hover
  | Coarse
  | Fine
  | Slow
  | Fast
  | Interlace
  | Progressive
  | Srgb
  | P3
  | Rec2020
  | Standard
  | High
  | Optional_paged
  | Paged
  | Scroll
  | Fullscreen
  | Standalone
  | Minimal_ui
  | Browser
  | Picture_in_picture
  | Opaque
  | Additive
  | Subtractive
  | Light
  | Dark
  | No_preference
  | Reduce
  | Less
  | More
  | Custom
  | Active
  | Inverted
  | Back
  | Initial_only
  | Enabled
  | Other of string

type value =
  | Length of Values_intf.length
  | Integer of int
  | Number of float
  | Ratio of int * int
  | Resolution_value of float * string
  | Ident of ident
  | Function of string * string
      (** [env(--name)] / [var(--name)] / [attr(...)] / [calc(...)] etc.
          captured as a function name plus its raw argument body for round-trip;
          cascade doesn't yet evaluate them at parse time. *)

type feature =
  | Plain of name * value
  | Boolean of name
  | Range of name * cmp * value
  | Range_rev of value * cmp * name
  | Interval of value * cmp * name * cmp * value

type condition =
  | Feature of feature
  | Not of condition
  | And of condition * condition
  | Or of condition * condition

type medium = All | Screen | Print | Other of string
type prefix = Not | Only

type t =
  | Cond of condition
  | Type of {
      prefix : prefix option;
      type_ : medium;
      trailing : condition option;
    }
  | List of t list  (** Comma-separated media query list. *)

(* ===== Formatting helpers ===== *)

let format_float f =
  if Float.is_integer f then Int.to_string (Float.to_int f)
  else Float.to_string f

let string_of_cmp = function
  | Lt -> "<"
  | Le -> "<="
  | Eq -> "="
  | Gt -> ">"
  | Ge -> ">="

let string_of_medium : medium -> string = function
  | All -> "all"
  | Screen -> "screen"
  | Print -> "print"
  | Other s -> s

let rec string_of_name : name -> string = function
  | Width -> "width"
  | Height -> "height"
  | Inline_size -> "inline-size"
  | Block_size -> "block-size"
  | Aspect_ratio -> "aspect-ratio"
  | Resolution -> "resolution"
  | Color -> "color"
  | Color_index -> "color-index"
  | Monochrome -> "monochrome"
  | Grid -> "grid"
  | Horizontal_viewport_segments -> "horizontal-viewport-segments"
  | Vertical_viewport_segments -> "vertical-viewport-segments"
  | Orientation -> "orientation"
  | Hover -> "hover"
  | Any_hover -> "any-hover"
  | Pointer -> "pointer"
  | Any_pointer -> "any-pointer"
  | Update -> "update"
  | Overflow_block -> "overflow-block"
  | Overflow_inline -> "overflow-inline"
  | Scan -> "scan"
  | Color_gamut -> "color-gamut"
  | Video_color_gamut -> "video-color-gamut"
  | Dynamic_range -> "dynamic-range"
  | Video_dynamic_range -> "video-dynamic-range"
  | Display_mode -> "display-mode"
  | Environment_blending -> "environment-blending"
  | Prefers_color_scheme -> "prefers-color-scheme"
  | Prefers_reduced_motion -> "prefers-reduced-motion"
  | Prefers_reduced_transparency -> "prefers-reduced-transparency"
  | Prefers_reduced_data -> "prefers-reduced-data"
  | Prefers_contrast -> "prefers-contrast"
  | Forced_colors -> "forced-colors"
  | Inverted_colors -> "inverted-colors"
  | Nav_controls -> "nav-controls"
  | Scripting -> "scripting"
  | Min name -> "min-" ^ string_of_name name
  | Max name -> "max-" ^ string_of_name name
  | Other s -> s

let rec name_of_string s : name =
  let s = String.lowercase_ascii s in
  let starts_with ~prefix =
    let len = String.length prefix in
    String.length s > len && String.sub s 0 len = prefix
  in
  if starts_with ~prefix:"min-" then
    Min (name_of_string (String.sub s 4 (String.length s - 4)))
  else if starts_with ~prefix:"max-" then
    Max (name_of_string (String.sub s 4 (String.length s - 4)))
  else
    match s with
    | "width" -> Width
    | "height" -> Height
    | "inline-size" -> Inline_size
    | "block-size" -> Block_size
    | "aspect-ratio" -> Aspect_ratio
    | "resolution" -> Resolution
    | "color" -> Color
    | "color-index" -> Color_index
    | "monochrome" -> Monochrome
    | "grid" -> Grid
    | "horizontal-viewport-segments" -> Horizontal_viewport_segments
    | "vertical-viewport-segments" -> Vertical_viewport_segments
    | "orientation" -> Orientation
    | "hover" -> Hover
    | "any-hover" -> Any_hover
    | "pointer" -> Pointer
    | "any-pointer" -> Any_pointer
    | "update" -> Update
    | "overflow-block" -> Overflow_block
    | "overflow-inline" -> Overflow_inline
    | "scan" -> Scan
    | "color-gamut" -> Color_gamut
    | "video-color-gamut" -> Video_color_gamut
    | "dynamic-range" -> Dynamic_range
    | "video-dynamic-range" -> Video_dynamic_range
    | "display-mode" -> Display_mode
    | "environment-blending" -> Environment_blending
    | "prefers-color-scheme" -> Prefers_color_scheme
    | "prefers-reduced-motion" -> Prefers_reduced_motion
    | "prefers-reduced-transparency" -> Prefers_reduced_transparency
    | "prefers-reduced-data" -> Prefers_reduced_data
    | "prefers-contrast" -> Prefers_contrast
    | "forced-colors" -> Forced_colors
    | "inverted-colors" -> Inverted_colors
    | "nav-controls" -> Nav_controls
    | "scripting" -> Scripting
    | _ -> Other s

let string_of_ident : ident -> string = function
  | Infinite -> "infinite"
  | Portrait -> "portrait"
  | Landscape -> "landscape"
  | None -> "none"
  | Hover -> "hover"
  | Coarse -> "coarse"
  | Fine -> "fine"
  | Slow -> "slow"
  | Fast -> "fast"
  | Interlace -> "interlace"
  | Progressive -> "progressive"
  | Srgb -> "srgb"
  | P3 -> "p3"
  | Rec2020 -> "rec2020"
  | Standard -> "standard"
  | High -> "high"
  | Optional_paged -> "optional-paged"
  | Paged -> "paged"
  | Scroll -> "scroll"
  | Fullscreen -> "fullscreen"
  | Standalone -> "standalone"
  | Minimal_ui -> "minimal-ui"
  | Browser -> "browser"
  | Picture_in_picture -> "picture-in-picture"
  | Opaque -> "opaque"
  | Additive -> "additive"
  | Subtractive -> "subtractive"
  | Light -> "light"
  | Dark -> "dark"
  | No_preference -> "no-preference"
  | Reduce -> "reduce"
  | Less -> "less"
  | More -> "more"
  | Custom -> "custom"
  | Active -> "active"
  | Inverted -> "inverted"
  | Back -> "back"
  | Initial_only -> "initial-only"
  | Enabled -> "enabled"
  | Other s -> s

let ident_of_string s : ident =
  match String.lowercase_ascii s with
  | "infinite" -> Infinite
  | "portrait" -> Portrait
  | "landscape" -> Landscape
  | "none" -> None
  | "hover" -> Hover
  | "coarse" -> Coarse
  | "fine" -> Fine
  | "slow" -> Slow
  | "fast" -> Fast
  | "interlace" -> Interlace
  | "progressive" -> Progressive
  | "srgb" -> Srgb
  | "p3" -> P3
  | "rec2020" -> Rec2020
  | "standard" -> Standard
  | "high" -> High
  | "optional-paged" -> Optional_paged
  | "paged" -> Paged
  | "scroll" -> Scroll
  | "fullscreen" -> Fullscreen
  | "standalone" -> Standalone
  | "minimal-ui" -> Minimal_ui
  | "browser" -> Browser
  | "picture-in-picture" -> Picture_in_picture
  | "opaque" -> Opaque
  | "additive" -> Additive
  | "subtractive" -> Subtractive
  | "light" -> Light
  | "dark" -> Dark
  | "no-preference" -> No_preference
  | "reduce" -> Reduce
  | "less" -> Less
  | "more" -> More
  | "custom" -> Custom
  | "active" -> Active
  | "inverted" -> Inverted
  | "back" -> Back
  | "initial-only" -> Initial_only
  | "enabled" -> Enabled
  | other -> Other other

(* ===== Pretty printing ===== *)

let pp_length ctx l = Values.pp_length ~always:true ctx l

let pp_value : value Pp.t =
 fun ctx -> function
  | Length l -> pp_length ctx l
  | Integer i -> Pp.string ctx (Int.to_string i)
  | Number f -> Pp.string ctx (format_float f)
  | Ratio (n, 1) when Pp.minified ctx -> Pp.string ctx (Int.to_string n)
  | Ratio (n, 1) ->
      Pp.string ctx (Int.to_string n);
      Pp.string ctx "/1"
  | Ratio (n, d) ->
      Pp.string ctx (Int.to_string n);
      Pp.char ctx '/';
      Pp.string ctx (Int.to_string d)
  | Resolution_value (n, unit) ->
      Pp.string ctx (format_float n);
      Pp.string ctx unit
  | Ident s -> Pp.string ctx (string_of_ident s)
  | Function (name, args) ->
      Pp.string ctx name;
      Pp.char ctx '(';
      Pp.string ctx args;
      Pp.char ctx ')'

let pp_feature : feature Pp.t =
 fun ctx -> function
  | Plain (name, value) when Pp.minified ctx ->
      Pp.char ctx '(';
      Pp.string ctx (string_of_name name);
      Pp.char ctx ':';
      pp_value ctx value;
      Pp.char ctx ')'
  | Plain (name, value) ->
      Pp.char ctx '(';
      Pp.string ctx (string_of_name name);
      Pp.char ctx ':';
      Pp.space_if_pretty ctx ();
      pp_value ctx value;
      Pp.char ctx ')'
  | Boolean name ->
      Pp.char ctx '(';
      Pp.string ctx (string_of_name name);
      Pp.char ctx ')'
  | Range (name, op, value) ->
      (* CSS Media Queries 4 3.2: relational operators are their own tokens, so
         the surrounding whitespace is optional and minify drops it. *)
      Pp.char ctx '(';
      Pp.string ctx (string_of_name name);
      Pp.sp ctx ();
      Pp.string ctx (string_of_cmp op);
      Pp.sp ctx ();
      pp_value ctx value;
      Pp.char ctx ')'
  | Range_rev (value, op, name) ->
      Pp.char ctx '(';
      pp_value ctx value;
      Pp.sp ctx ();
      Pp.string ctx (string_of_cmp op);
      Pp.sp ctx ();
      Pp.string ctx (string_of_name name);
      Pp.char ctx ')'
  | Interval (a, op1, name, op2, b) ->
      Pp.char ctx '(';
      pp_value ctx a;
      Pp.sp ctx ();
      Pp.string ctx (string_of_cmp op1);
      Pp.sp ctx ();
      Pp.string ctx (string_of_name name);
      Pp.sp ctx ();
      Pp.string ctx (string_of_cmp op2);
      Pp.sp ctx ();
      pp_value ctx b;
      Pp.char ctx ')'

(* CSS Media Queries 4 sec. 3.3: a lower and an upper bound on the same feature
   combine into the two-sided [<value> <op> <name> <op> <value>] interval.
   [feature_bound] normalises a single-bound feature into a [(name, side, op,
   value)] view so two bounds can be paired. *)
type bound_side = Lower | Upper

let feature_bound (f : feature) : (name * bound_side * cmp * value) option =
  let norm : feature option =
    match f with
    | Plain (Min base, v) -> Some (Range (base, Ge, v))
    | Plain (Max base, v) -> Some (Range (base, Le, v))
    | (Range _ | Range_rev _) as r -> Some r
    | _ -> None
  in
  match norm with
  | Some (Range (name, Ge, v)) -> Some (name, Lower, Le, v)
  | Some (Range (name, Gt, v)) -> Some (name, Lower, Lt, v)
  | Some (Range (name, Le, v)) -> Some (name, Upper, Le, v)
  | Some (Range (name, Lt, v)) -> Some (name, Upper, Lt, v)
  | Some (Range_rev (v, Le, name)) -> Some (name, Lower, Le, v)
  | Some (Range_rev (v, Lt, name)) -> Some (name, Lower, Lt, v)
  | Some (Range_rev (v, Ge, name)) -> Some (name, Upper, Le, v)
  | Some (Range_rev (v, Gt, name)) -> Some (name, Upper, Lt, v)
  | _ -> None

let merge_interval_bounds (a : feature) (b : feature) : feature option =
  match (feature_bound a, feature_bound b) with
  | Some (n1, Lower, lop, lv), Some (n2, Upper, uop, uv) when n1 = n2 ->
      Some (Interval (lv, lop, n1, uop, uv))
  | Some (n1, Upper, uop, uv), Some (n2, Lower, lop, lv) when n1 = n2 ->
      Some (Interval (lv, lop, n1, uop, uv))
  | _ -> None

let rec pp_condition : condition Pp.t =
 fun ctx -> function
  | Feature f -> pp_feature ctx f
  | Not c ->
      Pp.string ctx "not ";
      pp_condition ctx c
  | And (a, b) ->
      pp_condition ctx a;
      Pp.string ctx (if Pp.minified ctx then "and " else " and ");
      pp_condition ctx b
  | Or (a, b) ->
      pp_condition ctx a;
      Pp.string ctx (if Pp.minified ctx then "or " else " or ");
      pp_condition ctx b

let rec pp : t Pp.t =
 fun ctx -> function
  | Cond c -> pp_condition ctx c
  (* CSS Mediaqueries 4 sec. 3: [all and X] without an explicit prefix ([not] /
     [only]) is equivalent to just [X]. *)
  | Type { prefix = None; type_ = All; trailing = Some c } when Pp.minified ctx
    ->
      pp_condition ctx c
  | Type { prefix; type_; trailing } -> (
      (match prefix with
      | None -> ()
      | Some Not -> Pp.string ctx "not "
      | Some Only -> Pp.string ctx "only ");
      Pp.string ctx (string_of_medium type_);
      match trailing with
      | None -> ()
      | Some c -> (
          Pp.string ctx " and ";
          (* CSS Media Queries 4 sec. 3 [media-condition-without-or]: Or
             sub-expressions need explicit parens in this context. *)
          match c with
          | Or _ ->
              Pp.char ctx '(';
              pp_condition ctx c;
              Pp.char ctx ')'
          | _ -> pp_condition ctx c))
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

let to_string ?(minify = false) t = Pp.to_string ~minify pp t

(* ===== Parser ===== *)

(* A lightweight character scanner sufficient for media-query syntax. *)
type scanner = { s : string; mutable pos : int }
type recovery_scope = Branch | Query_list

exception Parse_error of recovery_scope * string

let fail_parse ?(scope = Branch) reason = raise (Parse_error (scope, reason))
let mk_scanner s = { s = String.trim s; pos = 0 }
let at_end sc = sc.pos >= String.length sc.s

let peek sc : char option =
  if at_end sc then Option.None else Some sc.s.[sc.pos]

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

let read_ident sc =
  skip_ws sc;
  let start = sc.pos in
  if at_end sc then ""
  else if not (is_ascii_ident_start sc.s.[sc.pos]) then ""
  else (
    advance sc;
    while (not (at_end sc)) && is_ascii_ident_continue sc.s.[sc.pos] do
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
       not (is_ascii_ident_continue c))

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

let read_number_lit sc : ([ `Int of int | `Float of float ] * string) option =
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
  else if is_ascii_ident_start sc.s.[sc.pos] then (
    advance sc;
    while (not (at_end sc)) && is_ascii_ident_continue sc.s.[sc.pos] do
      advance sc
    done;
    String.sub sc.s start (sc.pos - start))
  else ""

let length_of_value f unit : Values_intf.length option =
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

let typed_function_value name args : value option =
  let raw = name ^ "(" ^ args ^ ")" in
  let cursor = Cursor.of_string raw in
  match Values.read_length cursor with
  | length ->
      Cursor.ws cursor;
      Cursor.expect_eof cursor;
      Some (Length length)
  | exception Cursor.Parse_error _ -> None

let number_as_scalar : [ `Int of int | `Float of float ] -> value = function
  | `Int n -> Integer n
  | `Float f -> Number f

let number_as_int : [ `Int of int | `Float of float ] -> int = function
  | `Int n -> n
  | `Float f -> int_of_float f

let read_value_with_unit num unit : value option =
  let f = match num with `Int n -> float_of_int n | `Float f -> f in
  match length_of_value f unit with
  | Some l -> Some (Length l)
  | None ->
      if List.mem (String.lowercase_ascii unit) resolution_units then
        Some (Resolution_value (f, unit))
      else None

let read_ratio_tail sc num : value option =
  (* Could be a ratio: "n / m" *)
  let mark = sc.pos in
  skip_ws sc;
  match peek sc with
  | Some '/' -> (
      advance sc;
      skip_ws sc;
      match read_number_lit sc with
      | Some (`Int d, _) -> Some (Ratio (number_as_int num, d))
      | _ ->
          sc.pos <- mark;
          Some (number_as_scalar num))
  | _ ->
      sc.pos <- mark;
      Some (number_as_scalar num)

let read_numeric_value sc : value option =
  match read_number_lit sc with
  | None -> None
  | Some (num, _) ->
      let unit = read_unit sc in
      if unit = "" then read_ratio_tail sc num
      else read_value_with_unit num unit

let read_ident_value sc : value option =
  let id = read_ident sc in
  if id = "" then None
  else (
    skip_ws sc;
    match peek sc with
    | Some '(' -> (
        advance sc;
        let args = read_balanced sc in
        match typed_function_value id args with
        | Some _ as value -> value
        | None -> Some (Function (id, args)))
    | _ -> Some (Ident (ident_of_string id)))

let read_value sc : value option =
  skip_ws sc;
  match peek sc with
  | None -> None
  | Some c when (c >= '0' && c <= '9') || c = '.' || c = '+' || c = '-' ->
      read_numeric_value sc
  | Some _ -> read_ident_value sc

let value_of_string s =
  let sc = mk_scanner s in
  match read_value sc with
  | Some value ->
      skip_ws sc;
      if at_end sc then value else failwith ("invalid media value: " ^ s)
  | None -> failwith ("invalid media value: " ^ s)

let boolean_feature name : feature = Boolean name

let read_cmp sc : cmp option =
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

let range_feature_name (name : name) =
  match name with
  | Width | Height | Inline_size | Block_size | Aspect_ratio | Resolution
  | Color | Color_index | Monochrome | Horizontal_viewport_segments
  | Vertical_viewport_segments ->
      true
  | _ -> false

let prefixed_range_feature_name name : name option =
  match name with Min base | Max base -> Some base | _ -> Option.None

let validate_plain_feature (name : name) value =
  let plain_name =
    match prefixed_range_feature_name name with
    | Some base when range_feature_name base -> base
    | Some _ -> name
    | None -> name
  in
  (* [env()] / [var()] / [calc()] resolve their type at use time, so accept them
     wherever a typed numeric is allowed and let the consumer validate. *)
  let is_typed_function = function Function _ -> true | _ -> false in
  let valid_numeric_value (name : name) value =
    is_typed_function value
    ||
    match (name, value) with
    | (Width | Height | Inline_size | Block_size), Length _ -> true
    | Aspect_ratio, (Ratio _ | Integer _) -> true
    | Resolution, Resolution_value _ -> true
    | Resolution, Ident Infinite -> true
    | (Color | Color_index | Monochrome), Integer n -> n >= 0
    | (Horizontal_viewport_segments | Vertical_viewport_segments), Integer n ->
        n >= 0
    | _ -> false
  in
  let valid_plain_numeric_value (name : name) value =
    is_typed_function value
    ||
    match (name, value) with
    | ( (Width | Height | Min Width | Max Width | Inline_size | Block_size),
        Length _ ) ->
        true
    | Aspect_ratio, (Ratio _ | Integer _) -> true
    | Resolution, Resolution_value _ -> true
    | Resolution, Ident Infinite -> true
    | (Color | Color_index | Monochrome), Integer n -> n >= 0
    | Grid, Integer (0 | 1) -> true
    | (Horizontal_viewport_segments | Vertical_viewport_segments), Integer n ->
        n >= 0
    | _ -> false
  in
  let ident_value = match value with Ident s -> Some s | _ -> None in
  let one_of values =
    match ident_value with Some s -> List.mem s values | None -> false
  in
  match prefixed_range_feature_name name with
  | Some base when range_feature_name base -> valid_numeric_value base value
  | Some _ -> false
  | None -> (
      match plain_name with
      | Width | Height | Inline_size | Block_size | Aspect_ratio | Resolution
      | Color | Color_index | Monochrome | Grid | Horizontal_viewport_segments
      | Vertical_viewport_segments
      | Min Width
      | Max Width ->
          valid_plain_numeric_value name value
      | Orientation -> one_of [ Portrait; Landscape ]
      | Hover | Any_hover -> one_of [ None; Hover ]
      | Pointer | Any_pointer -> one_of [ None; Coarse; Fine ]
      | Update -> one_of [ None; Slow; Fast ]
      | Overflow_block -> one_of [ None; Scroll; Paged ]
      | Overflow_inline -> one_of [ None; Scroll ]
      | Scan -> one_of [ Interlace; Progressive ]
      | Color_gamut | Video_color_gamut -> one_of [ Srgb; P3; Rec2020 ]
      | Dynamic_range | Video_dynamic_range -> one_of [ Standard; High ]
      | Display_mode ->
          one_of
            [ Fullscreen; Standalone; Minimal_ui; Browser; Picture_in_picture ]
      | Environment_blending -> one_of [ Opaque; Additive; Subtractive ]
      | Prefers_color_scheme -> one_of [ Light; Dark ]
      | Prefers_reduced_motion | Prefers_reduced_transparency
      | Prefers_reduced_data ->
          one_of [ No_preference; Reduce ]
      | Prefers_contrast -> one_of [ No_preference; Less; More; Custom ]
      | Forced_colors -> one_of [ None; Active ]
      | Inverted_colors -> one_of [ None; Inverted ]
      | Nav_controls -> one_of [ None; Back ]
      | Scripting -> one_of [ None; Initial_only; Enabled ]
      | _ -> true)

let validate_range_feature name value =
  range_feature_name name && validate_plain_feature name value

(* Smart constructor for [Plain] features: rejects values that
   {!validate_plain_feature} reports as outside the feature's grammar (e.g.
   [feature "orientation" (Ident "sideways")]). *)
let plain_feature name value : feature =
  if not (validate_plain_feature name value) then
    invalid_arg
      ("Media.feature: value rejected by " ^ string_of_name name
     ^ "'s grammar (see Media.validate_plain_feature)");
  Plain (name, value)

(* Parse content already inside parens (no surrounding parens). *)
let value_first_interval_tail sc v1 op1 name op2 : feature option =
  skip_ws sc;
  match read_value sc with
  | None -> None
  | Some v2 ->
      skip_ws sc;
      if
        at_end sc
        && interval_ops_compatible op1 op2
        && validate_range_feature name v1
        && validate_range_feature name v2
      then Some (Interval (v1, op1, name, op2, v2))
      else None

let value_first_range_or_interval sc v1 op1 name : feature option =
  match read_cmp sc with
  | Some op2 -> value_first_interval_tail sc v1 op1 name op2
  | None ->
      skip_ws sc;
      if at_end sc && validate_range_feature name v1 then
        Some (Range_rev (v1, op1, name))
      else None

let value_first_after_op sc ~mark v1 op1 : feature option =
  skip_ws sc;
  let name = read_ident sc in
  if name = "" then (
    sc.pos <- mark;
    None)
  else
    let name = name_of_string name in
    value_first_range_or_interval sc v1 op1 name

let value_first_feature content : feature option =
  let sc = mk_scanner content in
  skip_ws sc;
  (* Try value-first form: V op name [op V] *)
  let mark = sc.pos in
  match read_value sc with
  | None -> None
  | Some v1 -> (
      match read_cmp sc with
      | Some op1 -> value_first_after_op sc ~mark v1 op1
      | None ->
          sc.pos <- mark;
          None)

let boolean_or_none_feature id : feature option =
  if Option.is_some (prefixed_range_feature_name (name_of_string id)) then None
  else Some (boolean_feature (name_of_string id))

let plain_feature_after_colon sc id : feature option =
  advance sc;
  skip_ws sc;
  match read_value sc with
  | None -> None
  | Some value ->
      skip_ws sc;
      let name = name_of_string id in
      if at_end sc && validate_plain_feature name value then
        Some (plain_feature name value)
      else None

let range_after_value sc id op v2 : feature option =
  match read_cmp sc with
  | Some _ -> None
  | None ->
      skip_ws sc;
      let name = name_of_string id in
      if at_end sc && validate_range_feature name v2 then
        Some (Range (name, op, v2))
      else None

let range_after_op sc id op : feature option =
  skip_ws sc;
  match read_value sc with
  | None -> None
  | Some v2 -> range_after_value sc id op v2

let name_first_range sc id content : feature option =
  match read_cmp sc with
  | None -> value_first_feature content
  | Some op -> range_after_op sc id op

let feature_after_ident sc id content : feature option =
  skip_ws sc;
  match peek sc with
  | None -> boolean_or_none_feature id
  | Some ':' -> plain_feature_after_colon sc id
  | Some _ -> name_first_range sc id content

let feature_in_parens content : feature option =
  let sc = mk_scanner content in
  skip_ws sc;
  if at_end sc then None
  else
    let id = read_ident sc in
    if id = "" then value_first_feature content
    else feature_after_ident sc id content

let extract_feature_or_fail content =
  match feature_in_parens content with
  | Some f -> f
  | None -> failwith ("invalid media feature: " ^ content)

let rec condition_from_paren_content content =
  let trimmed = String.trim content in
  (* Could be either ( <condition> ) or ( <feature> ). *)
  let inner = mk_scanner trimmed in
  skip_ws inner;
  if lookahead_ident inner "not" then condition_of_string trimmed
  else if lookahead_ident inner "and" || lookahead_ident inner "or" then
    Feature (extract_feature_or_fail trimmed)
  else if peek inner = Some '(' then condition_of_string trimmed
  else Feature (extract_feature_or_fail trimmed)

(* Parser for media-condition (sequence of (...) with and/or/not). *)
and condition_in_parens sc =
  skip_ws sc;
  match peek sc with
  | Some '(' ->
      advance sc;
      condition_from_paren_content (read_balanced sc)
  | _ -> failwith "expected '(' in media condition"

and condition_of_string s =
  let sc = mk_scanner s in
  condition sc

and condition sc =
  skip_ws sc;
  if lookahead_ident sc "not" then (
    consume_keyword sc "not";
    skip_ws sc;
    let inner = condition_in_parens sc in
    skip_ws sc;
    if at_end sc then Not inner
    else failwith "trailing content after 'not <media-in-parens>'")
  else
    let left = condition_in_parens sc in
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
      let right = condition_in_parens sc in
      loop (Some `And) (And (acc, right) : condition))
    else if lookahead_ident sc "or" then (
      (match op with
      | Some `And -> failwith "mixed 'and'/'or' media condition"
      | _ -> ());
      consume_keyword sc "or";
      let right = condition_in_parens sc in
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
  match String.lowercase_ascii s with "and" | "or" -> true | _ -> false

let rec next_non_ws s len i : char option =
  if i >= len then None
  else match s.[i] with ' ' | '\t' -> next_non_ws s len (i + 1) | c -> Some c

(* [<media-query>] starts as [<media-condition>] (rather than as a media type)
   when its first non-space token is '(' or "not (". *)
let starts_with_condition sc =
  match (peek sc, lookahead_ident sc "not") with
  | Some '(', _ -> true
  | _, true -> next_non_ws sc.s (String.length sc.s) (sc.pos + 3) = Some '('
  | _ -> false

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
      fail_parse ~scope:Query_list "duplicate media query prefix"
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
      let cond = condition_in_parens sc in
      let cond = chain sc cond in
      Type { prefix; type_; trailing = Some cond })
    else
      failwith
        (String.concat ""
           [
             "expected 'and' or end of query after media type, got: ";
             String.sub sc.s sc.pos (String.length sc.s - sc.pos);
           ])

let single_query sc =
  skip_ws sc;
  if at_end sc then failwith "empty media query"
  else if starts_with_condition sc then Cond (condition sc)
  else read_media_type_query sc

let not_all_query : t = Type { prefix = Some Not; type_ = All; trailing = None }

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

let query_branch ~recover ~recovered_at_eof sc =
  let mark = sc.pos in
  try
    let query = single_query sc in
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

let query_of_string ?(recover = true) s : t =
  let comma_query_list sc branch first recovered_at_eof =
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
    if !recovered_at_eof then not_all_query else List (first :: rest)
  in
  let sc = mk_scanner s in
  if at_end sc then (List [] : t)
  else
    let recovered_at_eof = ref false in
    let branch () = query_branch ~recover ~recovered_at_eof sc in
    let first = branch () in
    skip_ws sc;
    match (at_end sc, peek sc) with
    | true, _ -> first
    | false, Some ',' -> comma_query_list sc branch first recovered_at_eof
    | false, _ -> trailing_content_failure sc

let of_string s = query_of_string s
let of_string_strict s = query_of_string ~recover:false s

let of_function_body s : t =
  match feature_in_parens s with
  | Some feature -> Cond (Feature feature)
  | None -> of_string s

let feature name value : t =
  Cond (Feature (plain_feature (name_of_string name) value))

let boolean name : t = Cond (Feature (Boolean (name_of_string name)))

(* CSS Media Queries 4 sec. 3.4 / 3.3: the optimizer rewrites [min-X]/[max-X]
   into the range form and pairs a lower and upper bound into the two-sided
   interval. Target-fact grammar upgrades, so they live in optimize (gated by
   [~enforce_spec]), not the printer. *)
let lower_feature : feature -> feature = function
  | Plain (Min base, v) -> Range (base, Ge, v)
  | Plain (Max base, v) -> Range (base, Le, v)
  | f -> f

let rec lower_condition : condition -> condition = function
  | Feature f as cond ->
      let f' = lower_feature f in
      if f' == f then cond else Feature f'
  | Not c as cond ->
      let c' = lower_condition c in
      if c' == c then cond else Not c'
  | And (Feature a, Feature b) -> (
      match merge_interval_bounds a b with
      | Some interval -> Feature interval
      | None ->
          let a' = lower_feature a in
          let b' = lower_feature b in
          if a' == a && b' == b then And (Feature a, Feature b)
          else And (Feature a', Feature b'))
  | And (a, b) as cond ->
      let a' = lower_condition a in
      let b' = lower_condition b in
      if a' == a && b' == b then cond else And (a', b')
  | Or (a, b) as cond ->
      let a' = lower_condition a in
      let b' = lower_condition b in
      if a' == a && b' == b then cond else Or (a', b')

let rec lower_for_minify : t -> t = function
  | Cond c as query ->
      let c' = lower_condition c in
      if c' == c then query else Cond c'
  | Type ({ trailing; _ } as r) as query -> (
      match trailing with
      | None -> query
      | Some c ->
          let c' = lower_condition c in
          if c' == c then query else Type { r with trailing = Some c' })
  | List qs as query ->
      let rec loop changed acc = function
        | [] -> if changed then List (List.rev acc) else query
        | q :: rest ->
            let q' = lower_for_minify q in
            loop (changed || not (q' == q)) (q' :: acc) rest
      in
      loop false [] qs

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
  | Function _ -> (200, 0.)

(* A width feature buckets by which side it bounds: a lower bound ([min-width] /
   [width>=]) is [Responsive], an upper bound ([max-width] / [width<=]) is
   [Responsive_max]. Other width plains and [height] stay length-sorted
   [Responsive]. *)
let width_bound_kind name side value : kind option =
  match name with
  | Width ->
      let u, v = value_sort_key value in
      Some
        (match side with
        | Lower -> Responsive (u, v)
        | Upper -> Responsive_max (u, v))
  | _ -> None

let preference_kind (name : name) : kind option =
  match name with
  | Prefers_reduced_motion | Prefers_contrast | Forced_colors | Inverted_colors
  | Pointer | Any_pointer | Scripting ->
      Some Preference_accessibility
  | Prefers_color_scheme -> Some Preference_appearance
  | _ -> None

let plain_feature_kind name value =
  match preference_kind name with
  | Some k -> k
  | None -> (
      match name with
      | Width | Height ->
          let u, v = value_sort_key value in
          Responsive (u, v)
      | _ -> Other)

let interval_feature_kind name lo =
  match name with
  | Width ->
      let u, v = value_sort_key lo in
      Responsive (u, v)
  | _ -> Other

let feature_kind (f : feature) : kind =
  match f with
  | Boolean (Hover | Any_hover) -> Hover
  | Plain ((Hover | Any_hover), _) -> Hover
  | _ -> (
      match feature_bound f with
      | Some (name, side, _, value) -> (
          match width_bound_kind name side value with
          | Some k -> k
          | None -> Other)
      | None -> (
          match f with
          | Plain (name, value) -> plain_feature_kind name value
          | Boolean name -> Option.value (preference_kind name) ~default:Other
          | Interval (lo, _, name, _, _) -> interval_feature_kind name lo
          | Range _ | Range_rev _ -> Other))

let rec condition_kind (c : condition) : kind =
  match c with
  | Feature f -> feature_kind f
  | Not c -> condition_kind c
  | And (a, b) | Or (a, b) -> (
      match (condition_kind a, condition_kind b) with
      | Other, other | other, Other -> other
      | ka, _ -> ka)

let kind : t -> kind = function
  | Cond c -> condition_kind c
  (* [not all and X] negates a single feature: a negated width lower bound
     covers the complementary upper range, so it classifies as [Responsive_max]
     and vice versa. *)
  | Type { prefix = Some Not; type_ = All; trailing = Some c } -> (
      match condition_kind c with
      | Responsive (u, v) -> Responsive_max (u, v)
      | Responsive_max (u, v) -> Responsive (u, v)
      | other -> other)
  | Type _ | List _ -> Other

let group_order = function
  | Hover -> (0, 0.)
  | Other -> (500, 0.)
  | Preference_accessibility -> (1000, 0.)
  | Responsive_max _ -> (1999, 0.)
  | Responsive _ -> (2000, 0.)
  | Preference_appearance -> (3000, 0.)

let feature_preference_order (f : feature) : int option =
  let by_name name v =
    match (name, v) with
    | Prefers_reduced_motion, No_preference -> Some 0
    | Prefers_reduced_motion, Reduce -> Some 1
    | Prefers_contrast, More -> Some 2
    | Prefers_contrast, Less -> Some 3
    | Prefers_color_scheme, _ -> Some 4
    | Forced_colors, _ -> Some 5
    | Inverted_colors, _ -> Some 6
    | Pointer, None -> Some 7
    | Pointer, Coarse -> Some 8
    | Pointer, Fine -> Some 9
    | Any_pointer, None -> Some 10
    | Any_pointer, Coarse -> Some 11
    | Any_pointer, Fine -> Some 12
    | Scripting, None -> Some 13
    | Scripting, Initial_only -> Some 14
    | Scripting, Enabled -> Some 15
    | (Hover | Any_hover), _ -> Some 16
    | Prefers_reduced_transparency, No_preference -> Some 17
    | Prefers_reduced_transparency, Reduce -> Some 18
    | Prefers_reduced_data, No_preference -> Some 19
    | Prefers_reduced_data, Reduce -> Some 20
    | Nav_controls, _ -> Some 21
    | _ -> None
  in
  match f with
  | Plain (name, Ident v) -> by_name name v
  | Boolean (Hover | Any_hover) -> Some 16
  | _ -> None

let rec condition_preference_order (c : condition) : int =
  match c with
  | Feature f -> (
      match feature_preference_order f with Some n -> n | None -> 30)
  | Not c -> condition_preference_order c
  | And (a, _) | Or (a, _) -> condition_preference_order a

let preference_order : t -> int = function
  | Cond c -> condition_preference_order c
  | Type { trailing = Some c; _ } -> condition_preference_order c
  | Type _ | List _ -> 30

(* Within a responsive bucket, a negated lower bound sorts first, then an upper
   bound, then a lower bound. *)
let condition_subkind (c : condition) : int =
  match condition_kind c with
  | Responsive_max _ -> 1
  | Responsive _ -> 2
  | _ -> 2

let responsive_subkind : t -> int = function
  | Cond c -> condition_subkind c
  | Type { prefix = Some Not; type_ = All; trailing = Some c } -> (
      (* [not all and (min-width)] is a negated lower-bound query. *)
      match condition_kind c with
      | Responsive _ -> 0
      | _ -> 2)
  | Type { trailing = Some c; _ } -> condition_subkind c
  | Type _ | List _ -> 2

(* Within a responsive bucket, sort by decreasing specificity: the largest [min]
   (and smallest [max]) is most specific and sorts last. [Responsive_max] is
   negated so one ascending comparator yields largest-first ([1024, 768,
   640]). *)
let responsive_value k =
  match k with
  | Responsive (unit_ord, value) -> (Float.of_int unit_ord *. 1e9) +. value
  | Responsive_max (unit_ord, value) -> (Float.of_int unit_ord *. 1e9) -. value
  | _ -> 0.

(* Sort by four cheap ordinal keys, then serialized text on a full tie.
   Extracting kinds and serializing allocate, and breakpoint-sharing queries tie
   on every ordinal key so they always reach the text; [sort_key] computes the
   five components once per query and [compare_keys] compares them
   alloc-free. *)
type key = {
  group : int;
  subkind : int;
  responsive : float;
  preference : int;
  text : string;
}

let sort_key t =
  let k = kind t in
  let group, _ = group_order k in
  {
    group;
    subkind = responsive_subkind t;
    responsive = responsive_value k;
    preference = preference_order t;
    text = to_string t;
  }

(* A plain [if c <> 0 then c else ...] chain: it short-circuits like the thunk
   combinator but allocates no closures, which matters because a sort calls this
   O(n log n) times. *)
let compare_keys a b =
  let c = Int.compare a.group b.group in
  if c <> 0 then c
  else
    let c = Int.compare a.subkind b.subkind in
    if c <> 0 then c
    else
      let c = Float.compare a.responsive b.responsive in
      if c <> 0 then c
      else
        let c = Int.compare a.preference b.preference in
        if c <> 0 then c else String.compare a.text b.text

let sort_by project items =
  List.map (fun x -> (sort_key (project x), x)) items
  |> List.stable_sort (fun (k1, _) (k2, _) -> compare_keys k1 k2)
  |> List.map snd

let compare a b =
  let ka, kb = (kind a, kind b) in
  let ga, _ = group_order ka and gb, _ = group_order kb in
  let c = Int.compare ga gb in
  if c <> 0 then c
  else
    let c = Int.compare (responsive_subkind a) (responsive_subkind b) in
    if c <> 0 then c
    else
      let c = Float.compare (responsive_value ka) (responsive_value kb) in
      if c <> 0 then c
      else
        let c = Int.compare (preference_order a) (preference_order b) in
        if c <> 0 then c else String.compare (to_string a) (to_string b)

let equal a b = compare a b = 0
