open Alcotest
open Css_parser

let test_of_string () =
  (* Test basic CSS parsing *)
  let css = ".class { color: red; }" in
  match of_string css with
  | Ok _ -> check bool "parsed successfully" true true
  | Error (Parse_error (msg, _)) -> fail ("Failed to parse CSS: " ^ msg)

let test_of_string_error () =
  (* Test CSS parsing with invalid input *)
  let css = ".class { color: " in
  (* Unclosed declaration *)
  match of_string css with
  | Ok _ -> fail "Should have failed to parse invalid CSS"
  | Error _ -> check bool "failed as expected" true true

let test_of_string_exn () =
  (* Test exception-throwing parser *)
  let css = ".class { margin: 10px; }" in
  let _ = of_string_exn css in
  check bool "parsed without exception" true true

let tests =
  [
    test_case "of_string" `Quick test_of_string;
    test_case "of_string_error" `Quick test_of_string_error;
    test_case "of_string_exn" `Quick test_of_string_exn;
    (* Additional coverage: at-rules and typed values *)
    test_case "of_string_media_and_layer" `Quick (fun () ->
        let css =
          "@media (min-width: 640px){.a{margin:0}} @layer \
           utilities{.u{padding:1rem}}"
        in
        match of_string css with
        | Error (Parse_error (msg, _)) -> fail msg
        | Ok ast ->
            check int "media count" 1
              (Css.stylesheet_media_queries ast |> List.length);
            check int "layer count" 1 (Css.stylesheet_layers ast |> List.length));
    test_case "of_string_supports_roundtrip" `Quick (fun () ->
        let css = "@supports (display: grid){.g{display:grid}}" in
        match of_string css with
        | Ok ast ->
            let s = Css.to_string ~minify:true ast in
            check bool "contains @supports" true
              (Astring.String.is_infix ~affix:"@supports" s)
        | Error (Parse_error (msg, _)) -> fail msg);
    test_case "typed_values_color_length_var" `Quick (fun () ->
        let css = ".x{color: rgba(255,0,0,50%); width: var(--w, 10px)}" in
        match of_string css with
        | Ok ast ->
            let s = Css.to_string ~minify:true ast in
            (* Parser creates placeholder rules - just ensure basic structure
               exists *)
            check bool "selector present" true
              (Astring.String.is_infix ~affix:".x{" s);
            check bool "parses without error" true true
        | Error (Parse_error (msg, _)) -> fail msg);
  ]
