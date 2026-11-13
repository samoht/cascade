open Cascade
open Context_helpers

let check_resolved_url name ~loader ~expected input =
  match Css.Context.resolve_url loader input with
  | Ok actual -> Alcotest.(check string) name expected actual
  | Error _ ->
      Alcotest.failf "%s: expected URL %S to resolve to %S" name input expected

let check_url_error name ~loader input =
  match Css.Context.resolve_url loader input with
  | Ok actual ->
      Alcotest.failf "%s: expected URL %S to be unresolved, got %S" name input
        actual
  | Error _ -> ()

let check_registered_property_valid name ~registry input =
  let decl = Css.Declaration.of_string input in
  match Css.Context.validate_registered_custom_property registry decl with
  | Ok () -> ()
  | Error msg ->
      Alcotest.failf "%s: expected registered property %S to validate, got %S"
        name input msg

let check_registered_property_error name ~registry input =
  let decl = Css.Declaration.of_string input in
  match Css.Context.validate_registered_custom_property registry decl with
  | Ok () ->
      Alcotest.failf "%s: expected registered property %S to fail validation"
        name input
  | Error _ -> ()

let check_import_loaded name ?query ~loader ~expected input =
  let r = Cursor.of_string input in
  let import_rule = Css.Stylesheet.read_import_rule r in
  match Css.Context.load_import ?query loader import_rule with
  | Ok stylesheet ->
      Alcotest.(check string)
        name expected
        (Css.Stylesheet.to_string ~minify:true stylesheet)
  | Error _ -> Alcotest.failf "%s: expected import %S to load" name input

let check_import_error name ?query ~loader input =
  let r = Cursor.of_string input in
  let import_rule = Css.Stylesheet.read_import_rule r in
  match Css.Context.load_import ?query loader import_rule with
  | Ok stylesheet ->
      Alcotest.failf "%s: expected import %S to be blocked, got %S" name input
        (Css.Stylesheet.to_string ~minify:true stylesheet)
  | Error _ -> ()

let check_layered_import_loaded name ?query ~loader ~layer_order ~expected input
    =
  let r = Cursor.of_string input in
  let import_rule = Css.Stylesheet.read_import_rule r in
  match Css.Context.load_import ?query ~layer_order loader import_rule with
  | Ok stylesheet ->
      Alcotest.(check string)
        name expected
        (Css.Stylesheet.to_string ~minify:true stylesheet)
  | Error _ ->
      Alcotest.failf "%s: expected layered import %S to load" name input

let check_layered_import_error name ?query ~loader ~layer_order input =
  let r = Cursor.of_string input in
  let import_rule = Css.Stylesheet.read_import_rule r in
  match Css.Context.load_import ?query ~layer_order loader import_rule with
  | Ok stylesheet ->
      Alcotest.failf "%s: expected layered import %S to be blocked, got %S" name
        input
        (Css.Stylesheet.to_string ~minify:true stylesheet)
  | Error _ -> ()

let test_loader_context_contract () =
  let loader =
    Css.Context.loader ~base_url:"https://example.test/css/app.css"
      ~imports:
        [
          ("https://example.test/css/base.css", ".base{display:block}");
          ("https://example.test/css/theme.css", ".theme{color:red}");
          ("https://example.test/print.css", ".print{display:none}");
        ]
      ()
  in
  check_resolved_url "relative URL uses base directory" ~loader
    ~expected:"https://example.test/css/theme.css" "theme.css";
  check_resolved_url "parent-relative URL normalizes path" ~loader
    ~expected:"https://example.test/img/logo.svg" "../img/logo.svg";
  check_resolved_url "root-relative URL preserves origin" ~loader
    ~expected:"https://example.test/print.css" "/print.css";
  check_resolved_url "absolute URL remains absolute" ~loader
    ~expected:"https://cdn.example.test/site.css"
    "https://cdn.example.test/site.css";
  check_resolved_url "data URL remains absolute" ~loader
    ~expected:"data:image/png;base64,x" "data:image/png;base64,x";
  check_resolved_url "scheme-relative URL inherits scheme" ~loader
    ~expected:"https://cdn.example.test/site.css" "//cdn.example.test/site.css";
  check_resolved_url "query reference keeps base path" ~loader
    ~expected:"https://example.test/css/app.css?v=1" "?v=1";
  check_resolved_url "fragment reference keeps base path" ~loader
    ~expected:"https://example.test/css/app.css#icon" "#icon";
  check_resolved_url "scheme matching is case-insensitive" ~loader
    ~expected:"https://cdn.example.test/site.css"
    "HTTPS://cdn.example.test/site.css";
  (* WHATWG URL sec. 4.4 scheme start state: a digit opens no scheme, and in the
     scheme state a code point outside ASCII alphanumerics, [+], [-] and [.]
     starts over with none, so both are relative paths. *)
  check_resolved_url "a digit before a colon is a relative path" ~loader
    ~expected:"https://example.test/css/1:/x" "1:/x";
  check_resolved_url "an underscore before a colon is a relative path" ~loader
    ~expected:"https://example.test/css/a_b:x" "a_b:x";
  (* Sec. 4.4 scheme state reads [C:] as scheme [c]. *)
  check_resolved_url "a one-letter scheme remains absolute" ~loader
    ~expected:"c:/a.css" "C:/a.css";
  check_url_error "relative URL without base is unresolved"
    ~loader:Css.Context.empty_loader "theme.css";
  check_url_error "a digit before a colon needs a base"
    ~loader:Css.Context.empty_loader "1:/x";
  (* Against a filesystem base, a Windows drive path or UNC path is absolute and
     names a file whatever the base, by the Windows filesystem convention, which
     reads [\\] as [/]. *)
  let files = Css.Context.loader ~base_url:"sub/entry.css" () in
  check_resolved_url "a drive path is absolute" ~loader:files
    ~expected:"C:/dir/a.css" "C:\\dir\\a.css";
  check_resolved_url "a drive-relative path keeps its drive" ~loader:files
    ~expected:"C:a.css" "C:a.css";
  check_resolved_url "a UNC path is absolute" ~loader:files
    ~expected:"//host/share/a.css" "\\\\host\\share\\a.css";
  check_resolved_url "a digit before a colon is relative to the file"
    ~loader:files ~expected:"sub/1:/x" "1:/x";
  check_resolved_url "a file URL remains absolute" ~loader:files
    ~expected:"file:///C:/a.css" "file:///C:/a.css";
  (* RFC 3986 sec. 5.2.2 then resolves an import inside that file against its
     directory; sec. 6.2.2.1 lowercases the scheme. *)
  check_resolved_url "an import beside a drive path" ~expected:"c:/dir/b.css"
    ~loader:(Css.Context.loader ~base_url:"C:/dir/a.css" ())
    "b.css";
  check_resolved_url "an import beside a UNC path"
    ~expected:"//host/share/b.css"
    ~loader:(Css.Context.loader ~base_url:"//host/share/a.css" ())
    "b.css";
  check_import_loaded "loads same-directory import" ~loader
    ~expected:".theme{color:red}" "@import url(theme.css);";
  check_import_loaded "loads root-relative import" ~loader
    ~expected:".print{display:none}" "@import url(/print.css);";
  check_import_error "missing import is unresolved" ~loader
    "@import url(missing.css);"

let test_animation_context_contract () =
  let ctx =
    Css.Context.animation ~timeline_time:"250ms" ~progress:0.25
      ~animated_properties:[ "opacity"; "transform"; "--offset" ]
      ()
  in
  Alcotest.(check bool)
    "standard property animation is detected" true
    (Css.Context.animates_property "opacity" ctx);
  Alcotest.(check bool)
    "custom property animation is detected" true
    (Css.Context.animates_property "--offset" ctx);
  Alcotest.(check bool)
    "unanimated property is absent" false
    (Css.Context.animates_property "color" ctx);
  Alcotest.(check (option string))
    "timeline time is preserved" (Some "250ms") ctx.timeline_time;
  Alcotest.(check (option (float 0.0001)))
    "animation progress is preserved" (Some 0.25) ctx.progress

let test_property_registration_context_contract () =
  let length_reg =
    Css.Context.property_registration "--gap"
      (Css.Variables.Syntax Css.Variables.Length) ~inherits:true
      ~initial_value:"0px"
  in
  let color_reg =
    Css.Context.property_registration "--brand"
      (Css.Variables.Syntax Css.Variables.Color) ~inherits:false
      ~initial_value:"red"
  in
  let universal_reg =
    Css.Context.property_registration "--tokens"
      (Css.Variables.Syntax Css.Variables.Universal) ~inherits:true
  in
  let registry =
    Css.Context.property_registry
      ~property_registrations:[ length_reg; color_reg; universal_reg ]
      ()
  in
  Alcotest.(check (option string))
    "registered property lookup" (Some "--gap")
    (Option.map
       (fun (reg : Css.Context.property_registration) -> reg.name)
       (Css.Context.registered_property "--gap" registry));
  Alcotest.(check bool)
    "registered inherits flag" false
    (let (reg : Css.Context.property_registration) = color_reg in
     reg.inherits);
  Alcotest.(check (option string))
    "registered initial value" (Some "0px")
    (let (reg : Css.Context.property_registration) = length_reg in
     reg.initial_value);
  check_registered_property_valid "registered length accepts length" ~registry
    "--gap: 1rem";
  check_registered_property_valid "registered color accepts color" ~registry
    "--brand: color(display-p3 1 0 0)";
  check_registered_property_valid "universal syntax accepts token stream"
    ~registry "--tokens: { color: red }";
  check_registered_property_valid "unregistered custom property remains valid"
    ~registry "--unknown: red";
  check_registered_property_error "registered length rejects color" ~registry
    "--gap: red";
  check_registered_property_error "registered color rejects length" ~registry
    "--brand: 1rem";
  check_registered_property_error "non-custom declaration is not registered"
    ~registry "color: red"

let animation_time_eval_contract () =
  let ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css.Declaration.of_string "--duration: calc(500ms * 2)";
          Css.Declaration.of_string "--delay: calc(250ms * 2)";
          Css.Declaration.of_string "--distance: calc(1rem + 2px)";
        ]
      ~root_font_size:(Css.Values.Px 16.) ()
  in
  check_eval "eval folds animation-duration time calc" ~ctx
    ~expected:"animation-duration: 1s" "animation-duration: calc(500ms * 2)";
  check_eval "eval folds animation-delay time calc" ~ctx
    ~expected:"animation-delay: 0.5s" "animation-delay: calc(250ms * 2)";
  check_eval "eval folds transition-duration time calc" ~ctx
    ~expected:"transition-duration: 1s" "transition-duration: calc(250ms * 4)";
  check_eval "eval folds transition-delay time var" ~ctx
    ~expected:"transition-delay: 0.5s" "transition-delay: var(--delay)";
  check_eval "eval folds time slots inside animation shorthand" ~ctx
    ~expected:"animation: fade 1s linear 0.5s 2 alternate both running"
    "animation: fade var(--duration) linear var(--delay) 2 alternate both \
     running";
  check_eval "eval folds time slots inside transition shorthand" ~ctx
    ~expected:"transition: opacity 1s ease-in 0.5s"
    "transition: opacity var(--duration) ease-in var(--delay)";
  check_eval_preserves
    "animation-timeline is not sampled without a timeline context" ~ctx
    "animation-timeline: scroll()";
  check_eval_stylesheet "stylesheet eval reaches animation keyframe values" ~ctx
    ~expected:
      "@keyframes slide { from { transform: translateX(0); } to { transform: \
       translateX(18px); } }"
    "@keyframes slide { from { transform: translateX(0); } to { transform: \
     translateX(var(--distance)); } }";
  check_eval_stylesheet "stylesheet eval reaches time declarations in rules"
    ~ctx
    ~expected:
      ".fade{animation-duration:1s;animation-delay:0.5s;transition-duration:1s;transition-delay:0.5s}"
    ".fade{animation-duration:var(--duration);animation-delay:var(--delay);transition-duration:calc(250ms \
     * 4);transition-delay:var(--delay)}"

let test_loader_import_condition_contract () =
  let loader =
    Css.Context.loader ~base_url:"https://example.test/css/app.css"
      ~imports:
        [
          ("https://example.test/css/grid.css", ".grid{display:grid}");
          ("https://example.test/css/print.css", ".print{display:block}");
          ("https://example.test/css/lab.css", ".lab{color:lab(50% 0 0)}");
        ]
      ()
  in
  let query =
    Css.Context.query ~media_type:"screen"
      ~media_features:[ feature "width" (px 1024.) ]
      ~supports:
        [
          Css.Supports.property "display" "grid";
          Css.Supports.property "color" "lab(50% 0 0)";
        ]
      ()
  in
  check_import_loaded "import loads when media and supports match" ~query
    ~loader ~expected:".grid{display:grid}"
    "@import url(grid.css) supports(display: grid) screen and (width >= 40em);";
  check_import_loaded "import loads when supports color function matches"
    ~query
      (* check_import_loaded is to_string ~minify with no optimize, so pp holds
         the lab() unfolded; lab(50%0 0) is valid minified output (% is a token
         boundary). Folding lab to #777 is an optimize transform. *)
    ~loader ~expected:".lab{color:lab(50%0 0)}"
    "@import url(lab.css) supports(color: lab(50% 0 0));";
  check_import_error "import blocked by unmatched media" ~query ~loader
    "@import url(print.css) print;";
  check_import_error "import blocked by unmatched supports" ~query ~loader
    "@import url(grid.css) supports(display: ruby);"

let test_loader_import_layer_contract () =
  let layer_order = [ "theme"; "base"; "components"; "utilities" ] in
  let loader =
    Css.Context.loader ~base_url:"https://example.test/css/app.css"
      ~imports:
        [
          ( "https://example.test/css/theme.css",
            ":root{--color-brand-500:oklch(70% 0.15 250);--spacing:0.25rem}" );
          ("https://example.test/css/base.css", "*,::before,::after{margin:0}");
          ("https://example.test/css/components.css", ".card{padding:1rem}");
          ("https://example.test/css/utilities.css", ".text-brand{color:red}");
        ]
      ()
  in
  let query =
    Css.Context.query ~media_type:"screen"
      ~supports:[ Css.Supports.property "display" "grid" ]
      ~media_features:[ feature "width" (px 1024.) ]
      ()
  in
  check_layered_import_loaded "tailwind theme import enters theme layer" ~loader
    ~layer_order
    ~expected:
      "@layer theme{:root{--color-brand-500:oklch(70%.15 \
       250);--spacing:.25rem}}"
    "@import url(theme.css) layer(theme);";
  check_layered_import_loaded "tailwind base import enters base layer" ~loader
    ~layer_order ~expected:"@layer base{*,:before,:after{margin:0}}"
    "@import url(base.css) layer(base);";
  check_layered_import_loaded
    "tailwind components import keeps layer when query context matches" ~query
    ~loader ~layer_order ~expected:"@layer components{.card{padding:1rem}}"
    "@import url(components.css) layer(components) supports(display: grid) \
     screen and (width >= 40em);";
  check_layered_import_loaded "tailwind utilities import enters final layer"
    ~loader ~layer_order ~expected:"@layer utilities{.text-brand{color:red}}"
    "@import url(utilities.css) layer(utilities);";
  check_layered_import_error
    "conditional layered import blocked before layer insertion" ~query ~loader
    ~layer_order
    "@import url(components.css) layer(components) supports(display: flex);";
  check_layered_import_error "unknown layer name is rejected by layer context"
    ~loader ~layer_order "@import url(theme.css) layer(experimental);"

let suite =
  ( "context_loader",
    [
      Alcotest.test_case "loader context contract" `Quick
        test_loader_context_contract;
      Alcotest.test_case "animation context contract" `Quick
        test_animation_context_contract;
      Alcotest.test_case "property registration context contract" `Quick
        test_property_registration_context_contract;
      Alcotest.test_case "animation time eval contract" `Quick
        animation_time_eval_contract;
      Alcotest.test_case "loader import condition contract" `Quick
        test_loader_import_condition_contract;
      Alcotest.test_case "loader import layer contract" `Quick
        test_loader_import_layer_contract;
    ] )
