let () =
  let s = ".foo[href]" in
  let c = Cascade.Css.Cursor.of_string s in
  try
    let sel = Cascade.Css.Selector.read c in
    print_endline ("OK: " ^ Cascade.Css.Selector.to_string sel)
  with Cascade.Css.Cursor.Parse_error e ->
    print_endline ("FAIL: " ^ Cascade.Css.Error.to_string e)
