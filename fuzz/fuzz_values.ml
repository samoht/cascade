(** Fuzz tests for the CSS Values module.

    Tests crash safety of value parsers (colors, lengths, angles, etc.) and
    roundtrip consistency for pretty-printed values. *)

open Cascade
open Alcobar

let parse_whole parse input =
  let r = Cursor.of_string input in
  try
    let value = parse r in
    Cursor.ws r;
    if Cursor.is_done r then Some value else None
  with Cursor.Parse_error _ -> None

(** read_color -- must not crash on arbitrary input. *)
let test_read_color buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Values.read_color r) with Cursor.Parse_error _ -> ()

(** read_length -- must not crash. *)
let test_read_length buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Values.read_length r) with Cursor.Parse_error _ -> ()

(** read_angle -- must not crash. *)
let test_read_angle buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Values.read_angle r) with Cursor.Parse_error _ -> ()

(** read_duration -- must not crash. *)
let test_read_duration buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Values.read_duration r) with Cursor.Parse_error _ -> ()

(** read_time -- must not crash (can be negative). *)
let test_read_time buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Values.read_time r) with Cursor.Parse_error _ -> ()

(** read_number -- must not crash. *)
let test_read_number buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Values.read_number r) with Cursor.Parse_error _ -> ()

(** read_percentage -- must not crash. *)
let test_read_percentage buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Values.read_percentage r) with Cursor.Parse_error _ -> ()

(** read_length_percentage -- must not crash. *)
let test_read_length_percentage buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Values.read_length_percentage r)
  with Cursor.Parse_error _ -> ()

(** read_number_percentage -- must not crash. *)
let test_read_number_percentage buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Values.read_number_percentage r)
  with Cursor.Parse_error _ -> ()

(** read_color_name -- must not crash. *)
let test_read_color_name buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Values.read_color_name r) with Cursor.Parse_error _ -> ()

(** read_color_space -- must not crash. *)
let test_read_color_space buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Values.read_color_space r) with Cursor.Parse_error _ -> ()

(** read_system_color -- must not crash. *)
let test_read_system_color buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Values.read_system_color r) with Cursor.Parse_error _ -> ()

(** read_hue -- must not crash. *)
let test_read_hue buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Values.read_hue r) with Cursor.Parse_error _ -> ()

(** read_alpha -- must not crash. *)
let test_read_alpha buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Values.read_alpha r) with Cursor.Parse_error _ -> ()

(** read_hue_interpolation -- must not crash. *)
let test_read_hue_interpolation buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Values.read_hue_interpolation r)
  with Cursor.Parse_error _ -> ()

(** read_calc -- must not crash (with read_length as inner parser). *)
let test_read_calc buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Values.read_calc Css.Values.read_length r)
  with Cursor.Parse_error _ -> ()

(** read_channel -- must not crash. *)
let test_read_channel buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Values.read_channel r) with Cursor.Parse_error _ -> ()

(** read_component -- must not crash. *)
let test_read_component buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Values.read_component r) with Cursor.Parse_error _ -> ()

(** read_rgb -- must not crash. *)
let test_read_rgb buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Values.read_rgb r) with Cursor.Parse_error _ -> ()

(** read_transition_behavior -- must not crash. *)
let test_read_transition_behavior buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Values.read_transition_behavior r)
  with Cursor.Parse_error _ -> ()

(** Color roundtrip: parse -> pp -> parse should not crash. *)
let test_color_roundtrip buf =
  let r = Cursor.of_string buf in
  match
    try Some (Css.Values.read_color r) with Cursor.Parse_error _ -> None
  with
  | None -> ()
  | Some color -> (
      let s = Css.Pp.to_string Css.Values.pp_color color in
      let r2 = Cursor.of_string s in
      try ignore (Css.Values.read_color r2)
      with Cursor.Parse_error _ -> fail "color roundtrip re-parse failed")

(* Allow one canonicalization pass (trailing-zero trim, [1e3] -> [1000], escape
   canonical form, ...) that only fires on re-parse, then require fixed point.
   Skip when initial buf is garbage; serializer output must always re-parse -
   failure there is a lib bug. *)
let assert_value_idempotent ~label read pp buf =
  let serialize v = Css.Pp.to_string ~minify:true pp v in
  let parse s =
    try Some (read (Cursor.of_string s)) with Cursor.Parse_error _ -> None
  in
  let reparse_or_fail step s =
    match parse s with
    | Some v -> v
    | None -> failf "%s serialization did not reparse at %s: %S" label step s
  in
  match parse buf with
  | None -> ()
  | Some v ->
      let once = serialize v in
      let twice = serialize (reparse_or_fail "first reparse" once) in
      let thrice = serialize (reparse_or_fail "second reparse" twice) in
      if twice <> thrice then
        failf "%s serialization drifted past canonicalization: %S -> %S -> %S"
          label once twice thrice

(** Length serialization should reparse to the same canonical form for accepted
    values, including calc()/var() shapes. *)
let test_length_serialization_idempotent buf =
  assert_value_idempotent ~label:"length" Css.Values.read_length
    Css.Values.pp_length buf

let test_angle_serialization_idempotent buf =
  assert_value_idempotent ~label:"angle" Css.Values.read_angle
    Css.Values.pp_angle buf

let test_percentage_serialization_idempotent buf =
  assert_value_idempotent ~label:"percentage" Css.Values.read_percentage
    Css.Values.(pp_percentage ~always:true)
    buf

let test_duration_serialization_idempotent buf =
  assert_value_idempotent ~label:"duration" Css.Values.read_duration
    Css.Values.pp_duration buf

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte_at buf i mod List.length xs)

let test_modern_color_stable buf =
  let input =
    pick
      [
        "lab(50% 10 20)";
        "lch(50% 20 30)";
        "oklab(50% 0.1 0.2)";
        "oklch(50% 0.1 20 / 0.5)";
        "color(display-p3 1 0 0)";
        "color-mix(in oklab, red 40%, blue)";
        "light-dark(black, white)";
      ]
      buf 0
  in
  let r = Cursor.of_string input in
  match
    try Some (Css.Values.read_color r) with Cursor.Parse_error _ -> None
  with
  | None -> ()
  | Some color -> (
      let once = Css.Pp.to_string ~minify:true Css.Values.pp_color color in
      let r2 = Cursor.of_string once in
      match
        try Some (Css.Values.read_color r2) with Cursor.Parse_error _ -> None
      with
      | None -> failf "modern color did not reparse: %S" once
      | Some reparsed ->
          let twice =
            Css.Pp.to_string ~minify:true Css.Values.pp_color reparsed
          in
          if once <> twice then
            failf "modern color changed: %S -> %S" once twice)

let test_modern_math_stable buf =
  let input =
    pick
      [
        "round(nearest, 10px, 3px)";
        "mod(10px, 3px)";
        "rem(10px, 3px)";
        "hypot(3px, 4px)";
        "abs(-10px)";
        "sign(10px)";
        "anchor-size(width)";
        "calc-size(auto, size + 1rem)";
      ]
      buf 0
  in
  let r = Cursor.of_string input in
  match
    try Some (Css.Values.read_length r) with Cursor.Parse_error _ -> None
  with
  | None -> ()
  | Some length -> (
      let once = Css.Pp.to_string ~minify:true Css.Values.pp_length length in
      let r2 = Cursor.of_string once in
      match
        try Some (Css.Values.read_length r2) with Cursor.Parse_error _ -> None
      with
      | None -> failf "modern math length did not reparse: %S" once
      | Some reparsed ->
          let twice =
            Css.Pp.to_string ~minify:true Css.Values.pp_length reparsed
          in
          if once <> twice then
            failf "modern math length changed: %S -> %S" once twice)

let valid_length_vectors =
  [
    "1cqw";
    "1cqh";
    "1cqi";
    "1cqb";
    "1cqmin";
    "1cqmax";
    "anchor-size(width)";
    "anchor(--tooltip width, 10px)";
    "calc-size(auto, size + 1rem)";
  ]

let valid_color_vectors =
  [
    "lab(50% 10 20)";
    "lch(50% 20 30)";
    "oklab(50% 0.1 0.2)";
    "oklch(50% 0.1 20 / 0.5)";
    "color(rec2020 0.1 0.2 0.3)";
    "rgb(from var(--c) r g b / 50%)";
    "color-mix(in lch longer hue, red 30%, blue)";
  ]

let valid_number_vectors =
  [
    "round(up, 1.2, 1)";
    "mod(10, 3)";
    "hypot(3, 4)";
    "pow(2, 3)";
    "sqrt(4)";
    "sin(30deg)";
  ]

let assert_parse_print parse pp input =
  match parse_whole parse input with
  | None -> failf "valid CSS value vector did not parse: %S" input
  | Some value -> (
      let once = Css.Pp.to_string ~minify:true pp value in
      match parse_whole parse once with
      | None ->
          failf "valid CSS value serialization did not reparse: %S -> %S" input
            once
      | Some reparsed ->
          let twice = Css.Pp.to_string ~minify:true pp reparsed in
          if once <> twice then
            failf "valid CSS value serialization drifted: %S -> %S" once twice)

let test_spec_valid_value_vectors buf =
  match byte_at buf 0 mod 6 with
  | 0 ->
      assert_parse_print Css.Values.read_length Css.Values.pp_length
        (pick valid_length_vectors buf 1)
  | 1 ->
      assert_parse_print Css.Values.read_color Css.Values.pp_color
        (pick valid_color_vectors buf 1)
  | 2 ->
      assert_parse_print Css.Values.read_angle Css.Values.pp_angle
        (pick [ "45deg"; ".25turn"; "100grad"; "3.14159rad" ] buf 1)
  | 3 ->
      assert_parse_print Css.Values.read_duration Css.Values.pp_duration
        (pick [ "1s"; "150ms"; ".25s"; "1ms" ] buf 1)
  | 4 ->
      assert_parse_print Css.Values.read_percentage
        Css.Values.(pp_percentage ~always:true)
        (pick [ "0%"; "50%"; ".5%"; "200%" ] buf 1)
  | _ ->
      assert_parse_print Css.Values.read_number Css.Values.pp_number
        (pick valid_number_vectors buf 1)

let test_spec_invalid_value_vectors buf =
  let rejected parse input =
    match parse_whole parse input with
    | None -> ()
    | Some _ -> failf "invalid CSS value vector parsed: %S" input
  in
  match byte_at buf 0 mod 6 with
  | 0 ->
      rejected Css.Values.read_length
        (pick
           [
             "1unknown";
             "calc()";
             "calc(10px +)";
             "anchor()";
             "anchor-size()";
             "calc-size()";
           ]
           buf 1)
  | 1 ->
      rejected Css.Values.read_color
        (pick
           [
             "rgb()";
             "rgb(1 2)";
             "lab(50% 10)";
             "oklch(50% .1 20 /)";
             "color(display-p3 1 0)";
             "color(unknown 1 0 0)";
             "color-mix(in srgb red blue)";
           ]
           buf 1)
  | 2 ->
      rejected Css.Values.read_angle
        (pick [ "45"; "45px"; "deg"; "360.5.5deg" ] buf 1)
  | 3 ->
      rejected Css.Values.read_duration
        (pick [ "1"; "1px"; "-1s"; "10xs" ] buf 1)
  | 4 -> rejected Css.Values.read_percentage (pick [ "50"; "10px"; "%" ] buf 1)
  | _ ->
      rejected Css.Values.read_number
        (pick [ "pow(2)"; "sqrt()"; "sin()"; "round(up)" ] buf 1)

let color_branch_vectors =
  [
    "#fff";
    "#ffff";
    "#112233";
    "#11223344";
    "transparent";
    "currentColor";
    "CanvasText";
    "rgb(255, 0, 0)";
    "rgba(255, 0, 0, .5)";
    "hsl(120, 100%, 50%)";
    "hsla(120, 100%, 50%, .5)";
    "hwb(90 10% 20%)";
    "lab(50% 10 20 / .5)";
    "lch(50% 20 30)";
    "oklab(50% 0.1 0.2)";
    "oklch(50% 0.1 20 / 0.5)";
    "color(display-p3 1 0 0 / .5)";
    "color-mix(in srgb, red 40%, blue)";
    "light-dark(black, white)";
    "rgb(from rebeccapurple r g b / 50%)";
  ]

let assert_color_branch input =
  let r = Cursor.of_string input in
  match
    try Some (Css.Values.read_color r) with Cursor.Parse_error _ -> None
  with
  | None -> failf "valid CSS color branch vector rejected: %S" input
  | Some color -> (
      let serialized =
        Css.Pp.to_string ~minify:true Css.Values.pp_color color
      in
      let r2 = Cursor.of_string serialized in
      match
        try Some (Css.Values.read_color r2) with Cursor.Parse_error _ -> None
      with
      | None ->
          failf "CSS color branch serialization did not reparse: %S -> %S" input
            serialized
      | Some reparsed ->
          (* After one minification pass the color is in its canonical short
             form (CSS Color 4 section 12.1 hex shortening, etc.), so further
             passes must be idempotent at the string level. Direct AST equality
             with [color] does not hold for shortenable inputs like "#112233" ->
             "#123". *)
          let reserialized =
            Css.Pp.to_string ~minify:true Css.Values.pp_color reparsed
          in
          if reserialized <> serialized then
            failf "CSS color branch not idempotent: %S -> %S -> %S" input
              serialized reserialized)

let test_spec_color_branch_vectors buf =
  assert_color_branch (pick color_branch_vectors buf 2)

let test_invalid_color_branches buf =
  let input =
    pick
      [
        "#12";
        "#12345";
        "#ggg";
        "rgb()";
        "rgb(1 2)";
        "rgb(1, 2 3)";
        "hsl(0 50)";
        "hwb(0 0%)";
        "lab(50% 10)";
        "lch(50% 20)";
        "oklab(50% .1)";
        "color(display-p3 1 0)";
        "color(unknown 1 0 0)";
        "color-mix(in srgb red blue)";
        "light-dark(black)";
        "rgb(from red r g)";
        "hsl(0, 50%, 50% 1)";
        "hwb(0 0% 0% 0%)";
        "lab(50% 10 20 30)";
        "color(display-p3 1 0 0 1 2)";
        "color-mix(in srgb, red 20% 30%, blue)";
        "light-dark(black, white, red)";
        "rgb(from red r g b extra)";
        "contrast-color()";
        "transparent()";
        "currentColor()";
        "rgb(255, 0, 0 / 50%)";
        "rgb(255 0 0, 50%)";
        "hsl(0deg, 50% 50%)";
        "hwb(0deg, 0%, 0%)";
        "lab(50% 10px 20)";
        "color(srgb 1 0 0 / / .5)";
        "color-mix(in bogus, red, blue)";
        "light-dark()";
      ]
      buf 3
  in
  let r = Cursor.of_string input in
  match
    try Some (Css.Values.read_color r) with Cursor.Parse_error _ -> None
  with
  | None -> ()
  | Some color ->
      failf "invalid CSS color branch vector parsed: %S -> %S" input
        (Css.Pp.to_string ~minify:true Css.Values.pp_color color)

let generated_number buf i = pick [ "0"; ".5"; "1"; "12"; "-3"; "100" ] buf i

let generated_length buf i =
  let number = generated_number buf i in
  number ^ pick [ "px"; "rem"; "em"; "vw"; "cqw"; "%" ] buf (i + 1)

let generated_value buf =
  match byte_at buf 0 mod 6 with
  | 0 -> (`Length, generated_length buf 1)
  | 1 ->
      ( `Length,
        "calc(" ^ generated_length buf 1 ^ " + " ^ generated_length buf 3 ^ ")"
      )
  | 2 ->
      ( `Length,
        "clamp(" ^ generated_length buf 1 ^ "," ^ generated_length buf 3 ^ ","
        ^ generated_length buf 5 ^ ")" )
  | 3 ->
      ( `Color,
        "rgb(" ^ generated_number buf 1 ^ " " ^ generated_number buf 2 ^ " "
        ^ generated_number buf 3 ^ " / "
        ^ pick [ "50%"; ".5"; "1" ] buf 4
        ^ ")" )
  | 4 ->
      ( `Color,
        "color-mix(in "
        ^ pick [ "srgb"; "oklab"; "lch longer hue" ] buf 1
        ^ ", "
        ^ pick [ "red"; "blue"; "oklch(50% .1 20)" ] buf 2
        ^ " "
        ^ pick [ "25%"; "50%"; "75%" ] buf 3
        ^ ", "
        ^ pick [ "white"; "black"; "transparent" ] buf 4
        ^ ")" )
  | _ ->
      ( `Number,
        pick
          [
            "round(nearest, 10, 3)";
            "mod(10, 3)";
            "hypot(3, 4)";
            "abs(-10)";
            "sign(-1)";
          ]
          buf 1 )

let invalid_value_mutation buf =
  match generated_value buf with
  | `Length, value ->
      ( `Length,
        match byte_at buf 7 mod 4 with
        | 0 -> "calc(" ^ value ^ " +)"
        | 1 -> "clamp(" ^ value ^ "," ^ value ^ ")"
        | 2 -> value ^ " " ^ value
        | _ -> "anchor-size()" )
  | `Color, value ->
      ( `Color,
        match byte_at buf 7 mod 4 with
        | 0 -> "rgb(" ^ value ^ ")"
        | 1 -> "color-mix(in srgb " ^ value ^ ", blue)"
        | 2 -> "light-dark(" ^ value ^ ")"
        | _ -> value ^ " / / .5" )
  | `Number, value ->
      ( `Number,
        match byte_at buf 7 mod 4 with
        | 0 -> "round(up)"
        | 1 -> "pow(" ^ value ^ ")"
        | 2 -> "sqrt()"
        | _ -> value ^ " " ^ value )

let check_roundtrip_once parse pp once =
  match parse_whole parse once with
  | None -> failf "generated CSS value did not reparse: %S" once
  | Some reparsed ->
      let twice = Css.Pp.to_string ~minify:true pp reparsed in
      if once <> twice then
        failf "generated CSS value drifted: %S -> %S" once twice

let assert_value_roundtrip kind input =
  let run parse pp =
    match parse_whole parse input with
    | None -> failf "generated valid CSS value rejected: %S" input
    | Some value ->
        let once = Css.Pp.to_string ~minify:true pp value in
        check_roundtrip_once parse pp once
  in
  match kind with
  | `Length -> run Css.Values.read_length Css.Values.pp_length
  | `Color -> run Css.Values.read_color Css.Values.pp_color
  | `Number -> run Css.Values.read_number Css.Values.pp_number

let assert_value_reject kind input =
  let reject parse =
    match parse_whole parse input with
    | None -> ()
    | Some _ -> failf "generated invalid CSS value parsed: %S" input
  in
  match kind with
  | `Length -> reject Css.Values.read_length
  | `Color -> reject Css.Values.read_color
  | `Number -> reject Css.Values.read_number

let test_generated_value_grammar buf =
  let kind, input = generated_value buf in
  assert_value_roundtrip kind input

(* Pp.size counts the serialized bytes with the same spelling logic as to_string
   but without allocating the string. It must equal the length of what to_string
   would produce, for every formatter and value, in both minified and pretty
   modes. *)
let assert_pp_size_matches pp v =
  List.iter
    (fun minify ->
      let s = Css.Pp.to_string ~minify pp v in
      let n = Css.Pp.size ~minify pp v in
      if n <> String.length s then
        failf
          "Pp.size disagrees with to_string (minify=%b): size=%d, len=%d for %S"
          minify n (String.length s) s)
    [ true; false ]

let test_pp_size_matches_length buf =
  let kind, input = generated_value buf in
  let run parse pp =
    match parse_whole parse input with
    | None -> ()
    | Some v -> assert_pp_size_matches pp v
  in
  match kind with
  | `Length -> run Css.Values.read_length Css.Values.pp_length
  | `Color -> run Css.Values.read_color Css.Values.pp_color
  | `Number -> run Css.Values.read_number Css.Values.pp_number

(* pp must preserve the AST node (pp-purity policy): re-parsing pp's output
   yields a structurally identical value. Equivalent spellings decode to one
   node (e.g. [#ffffff] / [#fff] both decode to the same [Hex] components, like
   [0.5] / [.5] -> [Num 0.5]), so the shortest-spelling choice round-trips. Any
   node-changing fold - calc to a literal, a colour space to hex, a unit
   conversion - belongs to optimize and is caught here in both modes. *)
let assert_pp_preserves_ast read pp eq input =
  match parse_whole read input with
  | None -> ()
  | Some v ->
      List.iter
        (fun minify ->
          let s = Css.Pp.to_string ~minify pp v in
          match parse_whole read s with
          | Some v' when eq v' v -> ()
          | Some _ ->
              failf
                "pp changed the AST node (minify=%b): %S serialized to %S, \
                 which reparses to a different node"
                minify input s
          | None ->
              failf "pp output did not reparse (minify=%b): %S -> %S" minify
                input s)
        [ true; false ]

let test_pp_preserves_ast buf =
  let kind, input = generated_value buf in
  match kind with
  | `Length ->
      assert_pp_preserves_ast Css.Values.read_length Css.Values.pp_length ( = )
        input
  | `Color ->
      assert_pp_preserves_ast Css.Values.read_color Css.Values.pp_color ( = )
        input
  | `Number ->
      assert_pp_preserves_ast Css.Values.read_number Css.Values.pp_number ( = )
        input

let test_invalid_value_mutations buf =
  let kind, input = invalid_value_mutation buf in
  assert_value_reject kind input

let optimize_one css =
  match Css.of_string ~strict:false css with
  | Ok parsed ->
      Some (parsed.stylesheet |> Css.optimize |> Css.to_string ~minify:true)
  | Error _ -> None

(* (property, operand, type-matched zero) triples driving the calc-identity
   invariants below. Spans every value type whose [calc()] folds through the
   optimize pass now that the printer is a pure serialiser: lengths / percentages
   ([width], [padding]), the generic number ([opacity]), and the own-typed
   [line-height] / [<time>] / [font-size] / [vertical-align] / [border-width] /
   [number-percentage] folds. *)
(* The third field is the additive identity element when the type folds [x + 0]
   in the optimize pass: lengths / percentages collapse a typed [0px], numbers a
   unitless [0]. Own-typed lengths reached only through the generic [eval_calc]
   ([font-size], [border-width], [<time>]) fold the multiplicative identity but
   not a typed-zero addition, and a unitless [0] is invalid there, so they carry
   [None]. *)
let calc_identity_targets =
  [
    ("width", "12px", Some "0px");
    ("width", "var(--a)", Some "0px");
    ("width", "50%", Some "0px");
    ("padding", "var(--a)", Some "0px");
    ("padding", ".5rem", Some "0px");
    ("opacity", "var(--a)", Some "0");
    ("opacity", ".5", Some "0");
    ("line-height", "var(--a)", Some "0");
    ("transition-duration", "var(--a)", None);
    ("animation-delay", "var(--a)", None);
    ("font-size", "16px", None);
    ("font-size", "var(--a)", None);
    ("vertical-align", "var(--a)", None);
    ("border-top-width", "var(--a)", None);
    ("scale", "var(--a)", Some "0");
    ("flex-grow", "var(--a)", Some "0");
  ]

(* CSS Values 4 sec. 10.7 identity law: wrapping a value in a value-independent
   identity ([x * 1], [1 * x], [x / 1], [x + 0], [0 + x], [x - 0]) and
   optimising lands on the same normal form as optimising the bare value - the
   [var()] reference and the [calc()] wrapper survive, only the redundant
   arithmetic folds. Drives the shared [Values.calc_identity] through every
   evaluator the optimize pass reaches, so the generic and per-type folds cannot
   drift. *)
let test_calc_identity_law buf =
  let prop, operand, add_zero = pick calc_identity_targets buf 0 in
  let decl v = ".x{" ^ prop ^ ":" ^ v ^ "}" in
  match optimize_one (decl ("calc(" ^ operand ^ ")")) with
  | None -> ()
  | Some bare ->
      let multiplicative =
        [
          "calc(" ^ operand ^ " * 1)";
          "calc(1 * " ^ operand ^ ")";
          "calc(" ^ operand ^ " / 1)";
        ]
      in
      let additive =
        match add_zero with
        | None -> []
        | Some z ->
            [
              "calc(" ^ operand ^ " + " ^ z ^ ")";
              "calc(" ^ z ^ " + " ^ operand ^ ")";
              "calc(" ^ operand ^ " - " ^ z ^ ")";
            ]
      in
      List.iter
        (fun w ->
          match optimize_one (decl w) with
          | None -> failf "calc identity wrapper did not optimise: %S" w
          | Some got ->
              if got <> bare then
                failf "calc identity law broken: %S -> %S (bare %S)" w got bare)
        (multiplicative @ additive)

(* The identity and constant folds both converge, so a second optimize pass over
   a calc is a no-op. *)
let test_calc_optimize_idempotent buf =
  let prop, operand, _ = pick calc_identity_targets buf 0 in
  let css = ".x{" ^ prop ^ ":calc(" ^ operand ^ " * 1)}" in
  match optimize_one css with
  | None -> ()
  | Some once -> (
      match optimize_one once with
      | None -> failf "optimised calc did not reparse: %S" once
      | Some twice ->
          if once <> twice then
            failf "optimise not idempotent: %S -> %S -> %S" css once twice)

let reader_cases =
  [
    test_case "read_color crash safety" [ bytes ] test_read_color;
    test_case "read_length crash safety" [ bytes ] test_read_length;
    test_case "read_angle crash safety" [ bytes ] test_read_angle;
    test_case "read_duration crash safety" [ bytes ] test_read_duration;
    test_case "read_time crash safety" [ bytes ] test_read_time;
    test_case "read_number crash safety" [ bytes ] test_read_number;
    test_case "read_percentage crash safety" [ bytes ] test_read_percentage;
    test_case "read_length_percentage crash safety" [ bytes ]
      test_read_length_percentage;
    test_case "read_number_percentage crash safety" [ bytes ]
      test_read_number_percentage;
    test_case "read_color_name crash safety" [ bytes ] test_read_color_name;
    test_case "read_color_space crash safety" [ bytes ] test_read_color_space;
    test_case "read_system_color crash safety" [ bytes ] test_read_system_color;
    test_case "read_hue crash safety" [ bytes ] test_read_hue;
    test_case "read_alpha crash safety" [ bytes ] test_read_alpha;
    test_case "read_hue_interpolation crash safety" [ bytes ]
      test_read_hue_interpolation;
    test_case "read_calc crash safety" [ bytes ] test_read_calc;
    test_case "read_channel crash safety" [ bytes ] test_read_channel;
    test_case "read_component crash safety" [ bytes ] test_read_component;
    test_case "read_rgb crash safety" [ bytes ] test_read_rgb;
    test_case "read_transition_behavior crash safety" [ bytes ]
      test_read_transition_behavior;
  ]

let roundtrip_cases =
  [
    test_case "color roundtrip" [ bytes ] test_color_roundtrip;
    test_case "length serialization idempotent" [ bytes ]
      test_length_serialization_idempotent;
    test_case "angle serialization idempotent" [ bytes ]
      test_angle_serialization_idempotent;
    test_case "percentage serialization idempotent" [ bytes ]
      test_percentage_serialization_idempotent;
    test_case "duration serialization idempotent" [ bytes ]
      test_duration_serialization_idempotent;
    test_case "modern color stable" [ bytes ] test_modern_color_stable;
    test_case "modern math stable" [ bytes ] test_modern_math_stable;
    test_case "pp size matches serialized length" [ bytes ]
      test_pp_size_matches_length;
    test_case "pp preserves the AST node" [ bytes ] test_pp_preserves_ast;
    test_case "calc identity law" [ bytes ] test_calc_identity_law;
    test_case "calc optimize idempotent" [ bytes ] test_calc_optimize_idempotent;
  ]

let grammar_cases =
  [
    test_case "spec valid value vectors" [ bytes ] test_spec_valid_value_vectors;
    test_case "spec invalid value vectors rejected" [ bytes ]
      test_spec_invalid_value_vectors;
    test_case "spec color branch vectors" [ bytes ]
      test_spec_color_branch_vectors;
    test_case "spec invalid color branch vectors rejected" [ bytes ]
      test_invalid_color_branches;
    test_case "generated value grammar" [ bytes ] test_generated_value_grammar;
    test_case "invalid value mutations rejected" [ bytes ]
      test_invalid_value_mutations;
  ]

let suite = ("values", reader_cases @ roundtrip_cases @ grammar_cases)
