(* Helpers shared by the property families: CSS-wide keywords, small printing
   and normalisation combinators, and numeric evaluation.

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name here stays visible under [Css.Properties] exactly as when
   it lived in properties.ml. *)

open Common
open Values
open Properties_intf

let err_invalid_value ?loc ?got t prop_name value =
  Cursor.err ?loc ?got t ("invalid " ^ prop_name ^ " value: " ^ value)

let read_auto_color t =
  Cursor.enum "auto or colour"
    [ ("auto", (Auto : color)) ]
    ~default:read_color t

let rec read_css_wide t : css_wide =
  Cursor.enum_or_var "css-wide keyword"
    [
      ("initial", (Initial : css_wide));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_css_wide t))
    t

let rec pp_css_wide : css_wide Pp.t =
 fun ctx -> function
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> Values.pp_var pp_css_wide ctx v

let css_wide_keywords =
  [ "initial"; "inherit"; "unset"; "revert"; "revert-layer"; "revert-rule" ]

let is_css_wide_keyword value =
  List.mem (String.lowercase_ascii value) css_wide_keywords

(* CSS Cascade 5 sec. 7.3: a CSS-wide keyword must stand alone, so it is invalid
   mixed with other tokens (e.g. [font: initial 16px serif]). True when [value]
   is not itself a lone CSS-wide keyword yet contains one as an identifier. *)
let value_has_css_wide_mix value =
  let trimmed = String.trim value in
  (not (is_css_wide_keyword trimmed))
  &&
  let components = Cursor.remaining (Cursor.of_string trimmed) in
  List.exists
    (function
      | Component.Preserved { kind = Token.Ident ident; _ } ->
          is_css_wide_keyword ident
      | _ -> false)
    components

(* Components-form equivalent: skip the round-trip through a string buffer used
   by [value_has_css_wide_mix] when callers already hold the component list. *)
let components_have_css_wide_mix components =
  let non_ws =
    List.filter
      (function
        | Component.Preserved { kind = Token.Whitespace; _ } -> false
        | _ -> true)
      components
  in
  let lone_css_wide =
    match non_ws with
    | [ Component.Preserved { kind = Token.Ident ident; _ } ] ->
        is_css_wide_keyword ident
    | _ -> false
  in
  (not lone_css_wide)
  && List.exists
       (function
         | Component.Preserved { kind = Token.Ident ident; _ } ->
             is_css_wide_keyword ident
         | _ -> false)
       non_ws

let pp_opt_space pp ctx = function
  | Some v ->
      Pp.space ctx ();
      pp ctx v
  | None -> ()

let pp_keyword s ctx = Pp.string ctx s

(* CSS Syntax 3 (ED) sec. 4.3.7 lets an escape carry a [;], a [}] or any other
   code point CSS Syntax 3 (ED) sec. 4.2 keeps out of an ident into a name, so a
   [<custom-ident>] or [<dashed-ident>] is written back with the escapes that
   read it as the same name (see [Properties.pp_property]). Printed raw it ends
   its own declaration. *)
let pp_ident : string Pp.t = fun ctx s -> Pp.string ctx (Parser.escape_ident s)

let is_zero_length : length -> bool = function
  | Zero
  | Px 0.
  | Rem 0.
  | Em 0.
  | Ex 0.
  | Cap 0.
  | Ic 0.
  | Ric 0.
  | Rlh 0.
  | Cm 0.
  | Mm 0.
  | Q 0.
  | In 0.
  | Pt 0.
  | Pc 0.
  | Pct 0.
  | Vw 0.
  | Vh 0.
  | Vmin 0.
  | Vmax 0.
  | Vi 0.
  | Vb 0.
  | Dvh 0.
  | Dvw 0.
  | Dvmin 0.
  | Dvmax 0.
  | Lvh 0.
  | Lvw 0.
  | Lvmin 0.
  | Lvmax 0.
  | Svh 0.
  | Svw 0.
  | Svmin 0.
  | Svmax 0.
  | Cqw 0.
  | Cqh 0.
  | Cqi 0.
  | Cqb 0.
  | Cqmin 0.
  | Cqmax 0.
  | Dimension { value = 0.; _ } ->
      true
  | _ -> false

(* CSS Box 4 (ED) sec. 3.2 fills the four sides from a one-to-four value box
   shorthand: one value goes to all four, two to top-bottom then left-right,
   three to top, left-right, bottom. A list that repeats what those rules
   already supply is the longer spelling of the same declaration, so it
   collapses: [a a a a] -> [a]; [a b a b] -> [a b]; [a b c b] -> [a b c]. sec.
   4.2 says the same for padding, css-logical-1 sec. 4.3 and sec. 4.4 for the
   inset and logical padding/margin shorthands, and CSS Backgrounds 3 (ED) sec.
   3.1 and sec. 4.1 for border-color and border-radius. *)
let collapse_box_shorthand vs =
  match vs with
  | [ a; b; c; d ] when a = b && b = c && c = d -> [ a ]
  | [ a; b; c; d ] when a = c && b = d -> [ a; b ]
  | [ a; b; c; d ] when b = d -> [ a; b; c ]
  | [ a; b; c ] when a = b && b = c -> [ a ]
  | [ a; b; c ] when a = c -> [ a; b ]
  | [ a; b ] when a = b -> [ a ]
  | _ -> vs

let pp_box_shorthand pp ctx vs = Pp.list ~sep:Pp.token_sp pp ctx vs

(* Canonicalise a colour to its shortest spelling. *)
let normalize_color ?(lossless = false) = Values.normalize_color ~lossless
let preserve_if_equal before after = if after == before then before else after
let map_preserve = List.map_preserve

(* CSS Values 5 leaves a value's top-level component sequence unresolved across
   an arbitrary substitution function until computed-value time. A substitution
   nested inside calc() or another typed function stays one top-level component,
   so only these outer AST constructors are cardinality sensitive. *)
let is_length_substitution : length -> bool = function
  | Attr _ | Env _ | Var _ -> true
  | _ -> false

let is_lp_substitution : length_percentage -> bool = function
  | Length length -> is_length_substitution length
  | Env _ | Var _ -> true
  | _ -> false

let is_color_substitution : color -> bool = function
  | Attribute _ | Var _ -> true
  | _ -> false

let is_border_style_substitution : border_style -> bool = function
  | Var _ -> true
  | _ -> false

let is_border_width_substitution : border_width -> bool = function
  | Var _ -> true
  | _ -> false

(* Canonicalise a box shorthand: normalise each side with [f], then pick the
   shortest of the spellings that name those sides. Keep the authored number of
   components when arbitrary substitution defers their computed arity. *)
let normalize_box_shorthand ~is_substitution f vs =
  let normalized = map_preserve f vs in
  if List.exists is_substitution normalized then normalized
  else collapse_box_shorthand normalized

let normalize_length_box ?(non_negative = false) ~ctx =
  normalize_box_shorthand ~is_substitution:is_length_substitution
    (Values.normalize_length ~non_negative ~ctx)

let option_map_preserve f opt =
  match opt with
  | Option.None -> opt
  | Option.Some x ->
      let y = f x in
      if y == x then opt else Option.Some y

let option_is_phys_same a b =
  match (a, b) with
  | Option.None, Option.None -> true
  | Option.Some a, Option.Some b -> a == b
  | _ -> false

let map_var_preserve f (v : 'a var) : 'a var =
  let fallback =
    match v.fallback with
    | Fallback value ->
        let value' = f value in
        if value' == value then v.fallback else Fallback value'
    | (Empty | Empty2 | None | Syntax_fallback _ | Var_fallback _) as fallback
      ->
        fallback
  in
  let default = option_map_preserve f v.default in
  if fallback == v.fallback && default == v.default then v
  else { v with fallback; default }

(* Drop a shorthand component that equals its longhand initial: a shorthand
   resets every component it leaves out to that initial, so the two spellings
   name one value. *)
let drop_default ~is_default v =
  match v with Some x when is_default x -> Option.None | _ -> v

let rec eval_number_value : number -> float option = function
  | Num f -> Some f
  | Var _ -> None
  | Calc c -> eval_number_calc c
  | Round (strategy, value, step) -> (
      match (eval_number_value value, eval_number_value step) with
      | Some value, Some step when step <> 0. ->
          let quotient = value /. step in
          let rounded =
            match strategy with
            | "up" -> Float.ceil quotient
            | "down" -> Float.floor quotient
            | "to-zero" -> Float.trunc quotient
            | _ -> Float.round quotient
          in
          Some (rounded *. step)
      | _ -> None)
  | Mod (a, b) -> (
      match (eval_number_value a, eval_number_value b) with
      | Some a, Some b when b <> 0. -> Some (a -. (Float.floor (a /. b) *. b))
      | _ -> None)
  | Rem (a, b) -> (
      match (eval_number_value a, eval_number_value b) with
      | Some a, Some b when b <> 0. -> Some (Float.rem a b)
      | _ -> None)
  | Hypot (a, b) -> (
      match (eval_number_value a, eval_number_value b) with
      | Some a, Some b -> Some (Float.sqrt ((a *. a) +. (b *. b)))
      | _ -> None)
  | Pow (a, b) -> (
      match (eval_number_value a, eval_number_value b) with
      | Some a, Some b -> Some (a ** b)
      | _ -> None)
  | Sqrt v -> Option.map Float.sqrt (eval_number_value v)
  | Abs v -> Option.map Float.abs (eval_number_value v)
  | Sign v ->
      Option.map
        (fun x -> if x > 0. then 1. else if x < 0. then -1. else 0.)
        (eval_number_value v)
  | Sin _ -> None

and eval_number_calc : number calc -> float option = function
  | Num f -> Some f
  | Math_const c ->
      Some
        (match c with
        | Pi -> Float.pi
        | E -> Float.exp 1.
        | Infinity -> Float.infinity
        | Neg_infinity -> Float.neg_infinity
        | Nan -> Float.nan)
  | Math_fn fn -> Values.eval_math_fn fn
  | Val v -> eval_number_value v
  | Var _ | Sibling_index | Sibling_count -> None
  | Nested inner | Parens inner -> eval_number_calc inner
  | Expr (left, op, right) -> (
      match (eval_number_calc left, eval_number_calc right) with
      | Some left, Some right -> (
          match op with
          | Add -> Some (left +. right)
          | Sub -> Some (left -. right)
          | Mul -> Some (left *. right)
          | Div when right <> 0. -> Some (left /. right)
          | Div -> None)
      | _ -> None)

let hex_string n =
  let rec loop n acc =
    let digit n =
      if n < 10 then Char.chr (Char.code '0' + n)
      else Char.chr (Char.code 'A' + n - 10)
    in
    if n = 0 && acc = [] then "0"
    else if n = 0 then String.of_seq (List.to_seq acc)
    else loop (n / 16) (digit (n mod 16) :: acc)
  in
  loop n []

let padded_hex width n =
  let hex = hex_string n in
  if String.length hex >= width then hex
  else String.make (width - String.length hex) '0' ^ hex

(* RGB color helpers *)
let rgb_black : color = Rgb (Channels { r = Int 0; g = Int 0; b = Int 0 })
let url path : background_image = Url path

(* <dashed-ident>: shared by anchor-name, position-anchor, position-try
   fallbacks, font-palette and the animation timeline names. *)
let read_dashed_ident t =
  let loc = Cursor.position t in
  let ident = Cursor.ident ~keep_case:true t in
  if Custom_property_name.is_valid ident then ident
  else Cursor.err_invalid ~loc t ("expected dashed ident, got: " ^ ident)
