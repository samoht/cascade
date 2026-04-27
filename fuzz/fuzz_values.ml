(** Fuzz tests for the CSS Values module.

    Tests crash safety of value parsers (colors, lengths, angles, etc.) and
    roundtrip consistency for pretty-printed values. *)

open Cascade
open Alcobar

(** read_color — must not crash on arbitrary input. *)
let test_read_color buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Values.read_color r) with Css.Cursor.Parse_error _ -> ()

(** read_length — must not crash. *)
let test_read_length buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Values.read_length r) with Css.Cursor.Parse_error _ -> ()

(** read_angle — must not crash. *)
let test_read_angle buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Values.read_angle r) with Css.Cursor.Parse_error _ -> ()

(** read_duration — must not crash. *)
let test_read_duration buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Values.read_duration r) with Css.Cursor.Parse_error _ -> ()

(** read_time — must not crash (can be negative). *)
let test_read_time buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Values.read_time r) with Css.Cursor.Parse_error _ -> ()

(** read_number — must not crash. *)
let test_read_number buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Values.read_number r) with Css.Cursor.Parse_error _ -> ()

(** read_percentage — must not crash. *)
let test_read_percentage buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Values.read_percentage r)
  with Css.Cursor.Parse_error _ -> ()

(** read_length_percentage — must not crash. *)
let test_read_length_percentage buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Values.read_length_percentage r)
  with Css.Cursor.Parse_error _ -> ()

(** read_number_percentage — must not crash. *)
let test_read_number_percentage buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Values.read_number_percentage r)
  with Css.Cursor.Parse_error _ -> ()

(** read_color_name — must not crash. *)
let test_read_color_name buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Values.read_color_name r)
  with Css.Cursor.Parse_error _ -> ()

(** read_color_space — must not crash. *)
let test_read_color_space buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Values.read_color_space r)
  with Css.Cursor.Parse_error _ -> ()

(** read_system_color — must not crash. *)
let test_read_system_color buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Values.read_system_color r)
  with Css.Cursor.Parse_error _ -> ()

(** read_hue — must not crash. *)
let test_read_hue buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Values.read_hue r) with Css.Cursor.Parse_error _ -> ()

(** read_alpha — must not crash. *)
let test_read_alpha buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Values.read_alpha r) with Css.Cursor.Parse_error _ -> ()

(** read_hue_interpolation — must not crash. *)
let test_read_hue_interpolation buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Values.read_hue_interpolation r)
  with Css.Cursor.Parse_error _ -> ()

(** read_calc — must not crash (with read_length as inner parser). *)
let test_read_calc buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Values.read_calc Css.Values.read_length r)
  with Css.Cursor.Parse_error _ -> ()

(** read_channel — must not crash. *)
let test_read_channel buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Values.read_channel r) with Css.Cursor.Parse_error _ -> ()

(** read_component — must not crash. *)
let test_read_component buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Values.read_component r) with Css.Cursor.Parse_error _ -> ()

(** read_rgb — must not crash. *)
let test_read_rgb buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Values.read_rgb r) with Css.Cursor.Parse_error _ -> ()

(** read_transition_behavior — must not crash. *)
let test_read_transition_behavior buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Values.read_transition_behavior r)
  with Css.Cursor.Parse_error _ -> ()

(** Color roundtrip: parse → pp → parse should not crash. *)
let test_color_roundtrip buf =
  let r = Css.Cursor.of_string buf in
  match
    try Some (Css.Values.read_color r) with Css.Cursor.Parse_error _ -> None
  with
  | None -> ()
  | Some color -> (
      let s = Css.Pp.to_string Css.Values.pp_color color in
      let r2 = Css.Cursor.of_string s in
      try ignore (Css.Values.read_color r2)
      with Css.Cursor.Parse_error _ -> fail "color roundtrip re-parse failed")

(** Length serialization should reparse to the same canonical form for accepted
    values, including calc()/var() shapes. *)
let test_length_serialization_idempotent buf =
  let r = Css.Cursor.of_string buf in
  match
    try Some (Css.Values.read_length r) with Css.Cursor.Parse_error _ -> None
  with
  | None -> ()
  | Some length -> (
      let once = Css.Pp.to_string ~minify:true Css.Values.pp_length length in
      let r2 = Css.Cursor.of_string once in
      match
        try Some (Css.Values.read_length r2)
        with Css.Cursor.Parse_error _ -> None
      with
      | None -> fail (Fmt.str "length serialization did not reparse: %S" once)
      | Some reparsed ->
          let twice =
            Css.Pp.to_string ~minify:true Css.Values.pp_length reparsed
          in
          if once <> twice then
            fail (Fmt.str "length serialization changed: %S -> %S" once twice))

let test_angle_serialization_idempotent buf =
  let r = Css.Cursor.of_string buf in
  match
    try Some (Css.Values.read_angle r) with Css.Cursor.Parse_error _ -> None
  with
  | None -> ()
  | Some angle -> (
      let once = Css.Pp.to_string ~minify:true Css.Values.pp_angle angle in
      let r2 = Css.Cursor.of_string once in
      match
        try Some (Css.Values.read_angle r2)
        with Css.Cursor.Parse_error _ -> None
      with
      | None -> fail (Fmt.str "angle serialization did not reparse: %S" once)
      | Some reparsed ->
          let twice =
            Css.Pp.to_string ~minify:true Css.Values.pp_angle reparsed
          in
          if once <> twice then
            fail (Fmt.str "angle serialization changed: %S -> %S" once twice))

let test_percentage_serialization_idempotent buf =
  let r = Css.Cursor.of_string buf in
  match
    try Some (Css.Values.read_percentage r)
    with Css.Cursor.Parse_error _ -> None
  with
  | None -> ()
  | Some percentage -> (
      let once =
        Css.Pp.to_string ~minify:true
          Css.Values.(pp_percentage ~always:true)
          percentage
      in
      let r2 = Css.Cursor.of_string once in
      match
        try Some (Css.Values.read_percentage r2)
        with Css.Cursor.Parse_error _ -> None
      with
      | None ->
          fail (Fmt.str "percentage serialization did not reparse: %S" once)
      | Some reparsed ->
          let twice =
            Css.Pp.to_string ~minify:true
              Css.Values.(pp_percentage ~always:true)
              reparsed
          in
          if once <> twice then
            fail
              (Fmt.str "percentage serialization changed: %S -> %S" once twice))

let test_duration_serialization_idempotent buf =
  let r = Css.Cursor.of_string buf in
  match
    try Some (Css.Values.read_duration r)
    with Css.Cursor.Parse_error _ -> None
  with
  | None -> ()
  | Some duration -> (
      let once =
        Css.Pp.to_string ~minify:true Css.Values.pp_duration duration
      in
      let r2 = Css.Cursor.of_string once in
      match
        try Some (Css.Values.read_duration r2)
        with Css.Cursor.Parse_error _ -> None
      with
      | None -> fail (Fmt.str "duration serialization did not reparse: %S" once)
      | Some reparsed ->
          let twice =
            Css.Pp.to_string ~minify:true Css.Values.pp_duration reparsed
          in
          if once <> twice then
            fail (Fmt.str "duration serialization changed: %S -> %S" once twice)
      )

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
  let r = Css.Cursor.of_string input in
  match
    try Some (Css.Values.read_color r) with Css.Cursor.Parse_error _ -> None
  with
  | None -> ()
  | Some color -> (
      let once = Css.Pp.to_string ~minify:true Css.Values.pp_color color in
      let r2 = Css.Cursor.of_string once in
      match
        try Some (Css.Values.read_color r2)
        with Css.Cursor.Parse_error _ -> None
      with
      | None -> fail (Fmt.str "modern color did not reparse: %S" once)
      | Some reparsed ->
          let twice =
            Css.Pp.to_string ~minify:true Css.Values.pp_color reparsed
          in
          if once <> twice then
            fail (Fmt.str "modern color changed: %S -> %S" once twice))

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
  let r = Css.Cursor.of_string input in
  match
    try Some (Css.Values.read_length r) with Css.Cursor.Parse_error _ -> None
  with
  | None -> ()
  | Some length -> (
      let once = Css.Pp.to_string ~minify:true Css.Values.pp_length length in
      let r2 = Css.Cursor.of_string once in
      match
        try Some (Css.Values.read_length r2)
        with Css.Cursor.Parse_error _ -> None
      with
      | None -> fail (Fmt.str "modern math length did not reparse: %S" once)
      | Some reparsed ->
          let twice =
            Css.Pp.to_string ~minify:true Css.Values.pp_length reparsed
          in
          if once <> twice then
            fail (Fmt.str "modern math length changed: %S -> %S" once twice))

let test_spec_valid_value_vectors buf =
  let parse_print parse pp input =
    let r = Css.Cursor.of_string input in
    match try Some (parse r) with Css.Cursor.Parse_error _ -> None with
    | None -> fail (Fmt.str "valid CSS value vector did not parse: %S" input)
    | Some value -> (
        let once = Css.Pp.to_string ~minify:true pp value in
        let r2 = Css.Cursor.of_string once in
        match try Some (parse r2) with Css.Cursor.Parse_error _ -> None with
        | None ->
            fail
              (Fmt.str "valid CSS value serialization did not reparse: %S -> %S"
                 input once)
        | Some reparsed ->
            let twice = Css.Pp.to_string ~minify:true pp reparsed in
            if once <> twice then
              fail
                (Fmt.str "valid CSS value serialization drifted: %S -> %S" once
                   twice))
  in
  match byte_at buf 0 mod 6 with
  | 0 ->
      parse_print Css.Values.read_length Css.Values.pp_length
        (pick
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
           buf 1)
  | 1 ->
      parse_print Css.Values.read_color Css.Values.pp_color
        (pick
           [
             "lab(50% 10 20)";
             "lch(50% 20 30)";
             "oklab(50% 0.1 0.2)";
             "oklch(50% 0.1 20 / 0.5)";
             "color(rec2020 0.1 0.2 0.3)";
             "rgb(from var(--c) r g b / 50%)";
             "color-mix(in lch longer hue, red 30%, blue)";
           ]
           buf 1)
  | 2 ->
      parse_print Css.Values.read_angle Css.Values.pp_angle
        (pick [ "45deg"; ".25turn"; "100grad"; "3.14159rad" ] buf 1)
  | 3 ->
      parse_print Css.Values.read_duration Css.Values.pp_duration
        (pick [ "1s"; "150ms"; ".25s"; "1ms" ] buf 1)
  | 4 ->
      parse_print Css.Values.read_percentage
        Css.Values.(pp_percentage ~always:true)
        (pick [ "0%"; "50%"; ".5%"; "200%" ] buf 1)
  | _ ->
      parse_print Css.Values.read_number Css.Values.pp_number
        (pick
           [
             "round(up, 1.2, 1)";
             "mod(10, 3)";
             "hypot(3, 4)";
             "pow(2, 3)";
             "sqrt(4)";
             "sin(30deg)";
           ]
           buf 1)

let test_spec_invalid_value_vectors buf =
  let rejected parse input =
    let r = Css.Cursor.of_string input in
    match try Some (parse r) with Css.Cursor.Parse_error _ -> None with
    | None -> ()
    | Some _ -> fail (Fmt.str "invalid CSS value vector parsed: %S" input)
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

let suite =
  ( "values",
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
      test_case "read_system_color crash safety" [ bytes ]
        test_read_system_color;
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
      test_case "spec valid value vectors" [ bytes ]
        test_spec_valid_value_vectors;
      test_case "spec invalid value vectors rejected" [ bytes ]
        test_spec_invalid_value_vectors;
    ] )
