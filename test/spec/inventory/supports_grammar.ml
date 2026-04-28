type expected =
  | Property of string * string
  | Func of string * string
  | Not of expected
  | And of expected * expected
  | Or of expected * expected

type row = { name : string; input : string; expected : expected }

let row name input expected = { name; input; expected }

let rows =
  [
    row "declaration feature" "(display: grid)" (Property ("display", "grid"));
    row "selector feature" "selector(:has(+ img))"
      (Func ("selector", ":has(+ img)"));
    row "selector current pseudo" "selector(:popover-open)"
      (Func ("selector", ":popover-open"));
    row "font format feature" "font-format(woff2)"
      (Func ("font-format", "woff2"));
    row "font technology feature" "font-tech(color-COLRv1)"
      (Func ("font-tech", "color-COLRv1"));
    row "unknown general-enclosed function" "unknown-feature(foo bar)"
      (Func ("unknown-feature", "foo bar"));
    row "unknown declaration feature" "(-vendor-flag: enabled)"
      (Property ("-vendor-flag", "enabled"));
    row "not feature" "not (display: grid)" (Not (Property ("display", "grid")));
    row "and feature list" "(display: grid) and selector(:has(img))"
      (And (Property ("display", "grid"), Func ("selector", ":has(img)")));
    row "or feature list" "font-format(woff2) or font-tech(variations)"
      (Or (Func ("font-format", "woff2"), Func ("font-tech", "variations")));
    row "grouped mixed operator"
      "((display: grid) and (gap: 1rem)) or (color: red)"
      (Or
         ( And (Property ("display", "grid"), Property ("gap", "1rem")),
           Property ("color", "red") ));
    row "not wraps grouped or" "not ((display: grid) or (display: flex))"
      (Not (Or (Property ("display", "grid"), Property ("display", "flex"))));
    row "grouped or branch inside and"
      "((display: grid) or (display: flex)) and (gap: 1rem)"
      (And
         ( Or (Property ("display", "grid"), Property ("display", "flex")),
           Property ("gap", "1rem") ));
    row "grouped and branch inside or"
      "((container-type: inline-size) and selector(:has(img))) or (display: \
       grid)"
      (Or
         ( And
             ( Property ("container-type", "inline-size"),
               Func ("selector", ":has(img)") ),
           Property ("display", "grid") ));
    row "nested declaration function value"
      "(background-image: image-set(url(a.png) 1x, url(a@2x.png) 2x))"
      (Property
         ("background-image", "image-set(url(a.png) 1x, url(a@2x.png) 2x)"));
    row "custom property value" "(--theme-color: color(display-p3 1 0 0))"
      (Property ("--theme-color", "color(display-p3 1 0 0)"));
  ]

let invalid =
  [
    "";
    "()";
    "display: grid";
    "(display)";
    "(display:)";
    "(: grid)";
    "(display: grid) and";
    "(display: grid) or";
    "(display: grid) and (gap: 1rem) or selector(:has(img))";
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
