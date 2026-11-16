type row = Property_grammar_row.row = {
  property : string;
  positives : string list;
  negatives : string list;
}

let rows =
  Property_grammar_core.rows @ Property_grammar_shared.rows
  @ Property_grammar_text.rows @ Property_grammar_layout.rows
  @ Property_grammar_rendering.rows

let property_names =
  rows |> List.map (fun row -> row.property) |> List.sort_uniq String.compare
