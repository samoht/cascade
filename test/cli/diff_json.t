CLI: `cascade diff --json`, the comparison as one JSON document.

A CI or parity harness given only the tree report has to parse English to
learn what changed. `--json` writes the whole comparison as one JSON document
on standard output in place of that report, and leaves the exit status alone,
so a script branches on the status and reads the detail from the document.

  $ cat > a.css <<EOF
  > .x { color: red }
  > EOF
  $ cat > b.css <<EOF
  > .x { color: red }
  > EOF

An identical pair. Every count is zero, `identical` is true, and the status
is still 0. `unreadable_declarations` and `unreadable_rules` are what make
`identical` provable: they count, per side, the declarations and the rules the
reader refused and dropped, which the comparison therefore never saw.

  $ cascade diff --json a.css b.css
  {
    "expected": "a.css",
    "actual": "b.css",
    "mode": "auto",
    "identical": true,
    "unreadable_declarations": {
      "expected": 0,
      "actual": 0
    },
    "unreadable_rules": {
      "expected": 0,
      "actual": 0
    },
    "stats": {
      "expected_chars": 18,
      "actual_chars": 18,
      "added_rules": 0,
      "removed_rules": 0,
      "modified_rules": 0,
      "reordered_rules": 0,
      "rearranged_rules": 0,
      "regrouped_rules": 0,
      "container_changes": 0,
      "layer_order_swaps": 0
    },
    "warnings": [],
    "errors": [],
    "changes": []
  }

One modified rule. The entry names the rule, the property that changed and
the value each side gives it, and the status is still 1.

  $ cat > d.css <<EOF
  > .x { color: blue }
  > EOF
  $ cascade diff --json a.css d.css
  {
    "expected": "a.css",
    "actual": "d.css",
    "mode": "auto",
    "identical": false,
    "unreadable_declarations": {
      "expected": 0,
      "actual": 0
    },
    "unreadable_rules": {
      "expected": 0,
      "actual": 0
    },
    "stats": {
      "expected_chars": 18,
      "actual_chars": 19,
      "added_rules": 0,
      "removed_rules": 0,
      "modified_rules": 1,
      "reordered_rules": 0,
      "rearranged_rules": 0,
      "regrouped_rules": 0,
      "container_changes": 0,
      "layer_order_swaps": 0
    },
    "warnings": [],
    "errors": [],
    "changes": [
      {
        "kind": "modified",
        "selector": ".x",
        "old_declarations": [
          {
            "property": "color",
            "value": "red"
          }
        ],
        "new_declarations": [
          {
            "property": "color",
            "value": "blue"
          }
        ],
        "property_changes": [
          {
            "property_name": "color",
            "expected_value": "red",
            "actual_value": "blue"
          }
        ],
        "added_properties": [],
        "removed_properties": []
      }
    ]
  }
  [1]

Both of those are well-formed JSON.

  $ cascade diff --json a.css b.css | helpers/jsonq.exe; echo $?
  0
  $ cascade diff --json a.css d.css | helpers/jsonq.exe; echo $?
  0

A difference inside a container is one entry that nests its own changes,
rather than a rule change hoisted to the top level.

  $ cat > m1.css <<EOF
  > @media (min-width: 40rem) { .x { color: red } }
  > EOF
  $ cat > m2.css <<EOF
  > @media (min-width: 40rem) { .x { color: blue; top: 0 } }
  > EOF
  $ cascade diff --json m1.css m2.css | helpers/jsonq.exe; echo $?
  0
  $ cascade diff --json m1.css m2.css | helpers/jsonq.exe \
  >   '.stats.container_changes .identical' \
  >   '.changes[0].kind .changes[0].change .changes[0].container_type .changes[0].condition' \
  >   '.changes[0].rules .changes[0].actual_rules' \
  >   '.changes[0].changes[0].kind .changes[0].changes[0].selector .changes[0].changes[0].added_properties .changes[0].changes[0].property_changes'
  1 False
  container modified media (min-width: 40rem)
  ['.x{color:red}'] ['.x{color:blue;top:0}']
  modified .x [{'property': 'top', 'value': '0'}] [{'property_name': 'color', 'expected_value': 'red', 'actual_value': 'blue'}]

A parse warning is a member of the document, not a line beside it, so the
document is the only thing on standard output whether or not the parser had
a complaint. A warning both files raise names both sides once.

  $ cat > w1.css <<EOF
  > .x { color: red; float: center; margin: 0 }
  > EOF
  $ cat > w2.css <<EOF
  > .x { color: red; float: center; margin: 1px }
  > EOF
  $ cascade diff --json w1.css w2.css | helpers/jsonq.exe; echo $?
  0
  $ cascade diff --json w1.css w2.css | helpers/jsonq.exe .errors \
  >   --each .warnings '.side | .message:first-line'
  []
  both | read_declaration/float: bad value for float: unknown float-side: center at w1.css:1:25 (in component)

A warning only one file raises names that side.

  $ cat > p1.css <<EOF
  > .x { clear: nope; margin: 0 }
  > EOF
  $ cat > p2.css <<EOF
  > .x { position: nope; margin: 1px }
  > EOF
  $ cascade diff --json p1.css p2.css | helpers/jsonq.exe \
  >   --each .warnings '.side | .message:first-line'
  expected | read_declaration/clear: bad value for clear: unknown clear: nope at p1.css:1:13 (in component)
  actual | read_declaration/position: bad value for position: unknown position: nope at p2.css:1:16 (in component)

The character-diff fallback carries the position and the two lines instead
of a change list.

  $ cascade diff --json --diff=string a.css d.css | helpers/jsonq.exe \
  >   '.mode .changes' \
  >   '.string_diff.position .string_diff.line_expected .string_diff.column_expected' \
  >   .string_diff.diff_lines.expected .string_diff.diff_lines.actual
  string []
  12 0 12
  .x { color: red }
  .x { color: blue }

`--limit` and colour shape the human report, which `--json` replaces, so
passing either is accepted and leaves the document alone.

  $ cascade diff --json a.css d.css > plain.json
  [1]
  $ cascade diff --json --limit=1 --color=always a.css d.css > shaped.json
  [1]
  $ cmp plain.json shaped.json && echo same
  same

A side read from standard input is named the way the report names it.

  $ printf '.x { color: blue }\n' | cascade diff --json - a.css | helpers/jsonq.exe '.expected .actual'
  <stdin> a.css
