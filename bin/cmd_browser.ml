(* The browser comparison behind [cascade diff --browser --html DOC A B]:
   [Browser_compare] renders the two sheets over the document and reports the
   renders that differ, with the computed values the elements under them
   disagree on. What is left here is reading the files, the JSON document and
   the exit status. A run that reaches no browser, renders nothing or breaks in
   the driver fails rather than reporting equivalence. *)

module J = Cli_json

let json_difference (d : Browser_compare.difference) =
  J.Obj
    [
      ("viewport", J.String d.viewport);
      ("state", J.String d.state);
      ("element", J.String d.element);
      ("pseudo", J.String d.pseudo);
      ("property", J.String d.property);
      ("first", J.String d.first);
      ("second", J.String d.second);
    ]

let json_render (r : Browser_compare.render) =
  J.Obj
    [
      ("viewport", J.String r.viewport);
      ("state", J.String r.state);
      ("x", J.Int r.x);
      ("y", J.Int r.y);
      ("width", J.Int r.width);
      ("height", J.Int r.height);
      ("first_size", J.String r.first_size);
      ("second_size", J.String r.second_size);
    ]

let words s = List.filter (fun w -> w <> "") (String.split_on_char ' ' s)

let json_report ~file1 ~file2 ~html (report : Browser_compare.t) =
  let meta key = Option.value ~default:"" (List.assoc_opt key report.meta) in
  let int key =
    J.Int (Option.value ~default:0 (Browser_compare.meta_int report key))
  in
  let strings key =
    J.List (List.map (fun w -> J.String w) (words (meta key)))
  in
  J.Obj
    [
      ("document", J.String html);
      ("first", J.String file1);
      ("second", J.String file2);
      ( "browser",
        J.Obj
          [
            ("path", J.String (meta "browser_path"));
            ("version", J.String (meta "browser"));
          ] );
      ("viewports", strings "viewports");
      ("states", strings "states");
      ("pseudo_elements", strings "pseudos");
      ("elements", int "elements");
      ("properties", int "properties");
      ("samples", int "samples");
      ("document_styles_removed", int "document_styles_removed");
      ("doctype_added", J.Bool (meta "doctype_added" = "1"));
      ("captures", int "captures");
      ("renders", J.List (List.map json_render report.renders));
      ("differences", int "differences");
      ("truncated", J.Bool (List.mem_assoc "truncated" report.meta));
      ("errors", J.List []);
      ("changes", J.List (List.map json_difference report.differences));
    ]

let compare ~json ~html ~file1 ~file2 =
  let document = In_channel.with_open_bin html In_channel.input_all in
  let css1, _ = Cli_io.read_source file1 in
  let css2, _ = Cli_io.read_source file2 in
  match Browser_compare.run ~html:document [ (file1, css1); (file2, css2) ] with
  | Error msg ->
      Fmt.epr "Error: %s@." msg;
      Stdlib.exit Cli_exit.cannot_determine
  | Ok report ->
      if json then (
        print_string (J.to_string (json_report ~file1 ~file2 ~html report));
        print_newline ())
      else
        print_string
          (Browser_compare.to_string ~first:file1 ~second:file2 ~html report);
      if Browser_compare.identical report then Ok () else Stdlib.exit 1
