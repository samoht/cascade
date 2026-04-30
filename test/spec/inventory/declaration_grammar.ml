type serialization_row = { branch : string; input : string; expected : string }
type invalid_row = { branch : string; input : string }

let row branch input expected = { branch; input; expected }
let invalid branch input = { branch; input }

let css_wide_keywords =
  [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ]

let css_wide_positive =
  [
    row "ordinary property initial" "display: initial" "display:initial";
    row "ordinary property inherit" "font-size: inherit" "font-size:inherit";
    row "ordinary property unset" "margin: unset" "margin:unset";
    row "ordinary property revert" "color: revert" "color:revert";
    row "ordinary property revert-layer" "width: revert-layer"
      "width:revert-layer";
    row "all initial" "all: initial" "all:initial";
    row "all inherit" "all: inherit" "all:inherit";
    row "all unset" "all: unset" "all:unset";
    row "all revert" "all: revert" "all:revert";
    row "all revert-layer" "all: revert-layer" "all:revert-layer";
    row "all custom property reference" "all: var(--reset)" "all:var(--reset)";
    row "case-insensitive keyword" "display: INITIAL" "display:initial";
  ]

let css_wide_negative =
  [
    invalid "all keyword mix" "all: initial revert";
    invalid "ordinary value plus keyword" "display: block revert";
    invalid "keyword plus component value" "margin: revert-layer 1rem";
    invalid "keyword followed by color" "color: inherit red";
    invalid "all non-keyword" "all: auto";
    invalid "all identifier" "all: none";
  ]

let alias_positive =
  [
    row "page-break-before always" "page-break-before: always"
      "break-before:page";
    row "page-break-before avoid" "page-break-before: avoid"
      "break-before:avoid";
    row "page-break-before left" "page-break-before: left" "break-before:left";
    row "page-break-before right" "page-break-before: right"
      "break-before:right";
    row "page-break-after always" "page-break-after: always" "break-after:page";
    row "page-break-after avoid" "page-break-after: avoid" "break-after:avoid";
    row "page-break-after left" "page-break-after: left" "break-after:left";
    row "page-break-after right" "page-break-after: right" "break-after:right";
    row "page-break-inside auto" "page-break-inside: auto" "break-inside:auto";
    row "page-break-inside avoid" "page-break-inside: avoid"
      "break-inside:avoid";
  ]

let alias_negative =
  [
    invalid "page-break-before unsupported modern value"
      "page-break-before: recto";
    invalid "page-break-before mixed keyword" "page-break-before: revert always";
    invalid "page-break-after mixed keyword" "page-break-after: revert always";
    invalid "page-break-inside avoid-page" "page-break-inside: avoid-page";
    invalid "page-break-inside page" "page-break-inside: page";
  ]
