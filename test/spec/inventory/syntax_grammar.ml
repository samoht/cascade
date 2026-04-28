type token_row = { branch : string; input : string; expected : string list }
type parser_row = { branch : string; input : string }

let token branch input expected = { branch; input; expected }
let parser branch input = { branch; input }

let token_rows =
  [
    token "unicode range wildcard" "U+4??" [ "<unicode-range U+400-4FF>" ];
    token "bad string newline" "\"oops\n" [ "<bad-string>"; "<ws>" ];
    token "bad url whitespace" "url(a b) foo"
      [ "<bad-url>"; "<ws>"; "<ident foo>" ];
    token "CDC and ident split" "--> -->a --a"
      [ "<CDC>"; "<ws>"; "<CDC>"; "<ident a>"; "<ws>"; "<ident --a>" ];
    token "number signs and delim" ".5 -.5 +.5 .e1"
      [
        "<number .5>";
        "<ws>";
        "<number -.5>";
        "<ws>";
        "<number +.5>";
        "<ws>";
        "<delim '.'>";
        "<ident e1>";
      ];
    token "escape followed by hash" "\\26 #id" [ "<ident &>"; "<#id>" ];
    token "comment skipping" "a/*x*/ b" [ "<ident a>"; "<ws>"; "<ident b>" ];
    token "escaped replacement code point" "\\110000 " [ "<ident \u{FFFD}>" ];
  ]

let parser_rows =
  [
    parser "unclosed function and block" "calc(1px + [2em";
    parser "extra closing braces" "a}}";
    parser "top-level comma groups" "a,(b,c),f(x,y),d";
    parser "bad url with quote" "url(foo\"bar) next";
    parser "nested unclosed blocks" "[a {b (c";
    parser "comment elision" "a/* ignored { color: red } */b";
    parser "comment token boundary" "foo/**/bar";
    parser "unclosed at-rule block" "@media screen { .a { color: red }";
  ]

let mutate_parser_input row salt =
  match salt mod 5 with
  | 0 -> row.input ^ "}"
  | 1 -> "(" ^ row.input
  | 2 -> row.input ^ ",,"
  | 3 -> row.input ^ "/*unterminated"
  | _ -> row.input ^ "\\\n"
