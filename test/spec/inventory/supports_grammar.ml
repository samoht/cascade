type expected =
  | Property of string * string
  | Func of string * string
  | Not of expected
  | And of expected * expected
  | Or of expected * expected

type row = { name : string; input : string; expected : expected }

let row name input expected = { name; input; expected }
let property name value = Property (name, value)
let func name value = Func (name, value)

let rows =
  [
    row "declaration feature" "(display: grid)" (property "display" "grid");
    row "selector feature" "selector(:has(+ img))"
      (func "selector" ":has(+ img)");
    row "selector current pseudo" "selector(:popover-open)"
      (func "selector" ":popover-open");
    row "selector focus-visible" "selector(:focus-visible)"
      (func "selector" ":focus-visible");
    row "selector nth child of list" "selector(:nth-child(2n of .item, .card))"
      (func "selector" ":nth-child(2n of .item, .card)");
    row "font format feature" "font-format(woff2)" (func "font-format" "woff2");
    row "font format opentype" "font-format(opentype)"
      (func "font-format" "opentype");
    row "font technology feature" "font-tech(color-COLRv1)"
      (func "font-tech" "color-COLRv1");
    row "font technology variations" "font-tech(variations)"
      (func "font-tech" "variations");
    row "font technology palettes" "font-tech(palettes)"
      (func "font-tech" "palettes");
    row "at-rule feature" "at-rule(@container)" (func "at-rule" "@container");
    row "at-rule layer feature" "at-rule(@layer)" (func "at-rule" "@layer");
    row "at-rule scope feature" "at-rule(@scope)" (func "at-rule" "@scope");
    row "at-rule property feature" "at-rule(@property)"
      (func "at-rule" "@property");
    row "at-rule charset syntax" "at-rule(@charset)" (func "at-rule" "@charset");
    row "named feature function" "named-feature(--compact)"
      (func "named-feature" "--compact");
    row "named feature ident function" "named-feature(color-gamut)"
      (func "named-feature" "color-gamut");
    row "environment variable function" "env(safe-area-inset-top)"
      (func "env" "safe-area-inset-top");
    row "environment variable custom ident" "env(titlebar-area-width)"
      (func "env" "titlebar-area-width");
    row "unknown general-enclosed function" "unknown-feature(foo bar)"
      (func "unknown-feature" "foo bar");
    row "unknown declaration feature" "(-vendor-flag: enabled)"
      (property "-vendor-flag" "enabled");
    row "empty declaration value" "(display:)" (property "display" "");
    row "not feature" "not (display: grid)" (Not (property "display" "grid"));
    row "and feature list" "(display: grid) and selector(:has(img))"
      (And (property "display" "grid", func "selector" ":has(img)"));
    row "or feature list" "font-format(woff2) or font-tech(variations)"
      (Or (func "font-format" "woff2", func "font-tech" "variations"));
    row "grouped mixed operator"
      "((display: grid) and (gap: 1rem)) or (color: red)"
      (Or
         ( And (property "display" "grid", property "gap" "1rem"),
           property "color" "red" ));
    row "not wraps grouped or" "not ((display: grid) or (display: flex))"
      (Not (Or (property "display" "grid", property "display" "flex")));
    row "grouped or branch inside and"
      "((display: grid) or (display: flex)) and (gap: 1rem)"
      (And
         ( Or (property "display" "grid", property "display" "flex"),
           property "gap" "1rem" ));
    row "grouped and branch inside or"
      "((container-type: inline-size) and selector(:has(img))) or (display: \
       grid)"
      (Or
         ( And
             ( property "container-type" "inline-size",
               func "selector" ":has(img)" ),
           property "display" "grid" ));
    row "nested declaration function value"
      "(background-image: image-set(url(a.png) 1x, url(a@2x.png) 2x))"
      (property "background-image" "image-set(url(a.png) 1x, url(a@2x.png) 2x)");
    row "custom property value" "(--theme-color: color(display-p3 1 0 0))"
      (property "--theme-color" "color(display-p3 1 0 0)");
  ]

(* Conditional Rules 3 section 6: unsupported functions remain grammatical. *)
let general_enclosed_functions =
  [
    "selector()";
    "selector(:has())";
    "selector(:nth-child(2n of))";
    "font-format()";
    "font-format(\"woff2\")";
    "font-format(woff2, opentype)";
    "font-tech()";
    "font-tech(\"variations\")";
    "font-tech(color-COLRv1 variations)";
    "at-rule(container)";
    "at-rule()";
    "at-rule(@container @layer)";
    "named-feature()";
    "named-feature(--compact, --wide)";
    "env()";
    "env(\"safe-area-inset-top\")";
    "env(safe-area-inset-top, fallback)";
  ]

let invalid =
  [
    "font-format(])";
    "future(url(a b))";
    "future(\"a\nb\")";
    "";
    "()";
    "display: grid";
    "(display)";
    "(: grid)";
    "(display: grid;)";
    "(display: grid) and";
    "(display: grid) or";
    "(display: grid) and (gap: 1rem) or selector(:has(img))";
    "font-format(woff2) and or (display: grid)";
    "selector(:has(img)) or and (display: grid)";
    "not not (display: grid)";
    "not (display: grid) and (gap: 1rem)";
    "not (display: grid) or (gap: 1rem)";
    "selector(:has(img)";
    "not";
    "not ()";
    "(display: grid) and or (gap: 1rem)";
    "((display: grid)";
    "(display: grid";
  ]

let mutate_invalid row salt =
  match salt mod 6 with
  | 0 -> row.input ^ " and"
  | 1 -> row.input ^ " or"
  | 2 -> "not"
  | 3 -> "(" ^ row.input
  | 4 -> row.input ^ ")"
  | _ -> row.input ^ " and or (color: red)"
