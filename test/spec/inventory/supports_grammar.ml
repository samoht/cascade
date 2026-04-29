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
    row "font format feature" "font-format(woff2)" (func "font-format" "woff2");
    row "font technology feature" "font-tech(color-COLRv1)"
      (func "font-tech" "color-COLRv1");
    row "at-rule feature" "at-rule(@container)" (func "at-rule" "@container");
    row "named feature function" "named-feature(--compact)"
      (func "named-feature" "--compact");
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

let invalid =
  [
    "";
    "()";
    "display: grid";
    "(display)";
    "(: grid)";
    "(display: grid;)";
    "(display: grid) and";
    "(display: grid) or";
    "(display: grid) and (gap: 1rem) or selector(:has(img))";
    "not not (display: grid)";
    "not (display: grid) and (gap: 1rem)";
    "not (display: grid) or (gap: 1rem)";
    "selector()";
    "selector(:has(img)";
    "font-format()";
    "font-tech()";
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
