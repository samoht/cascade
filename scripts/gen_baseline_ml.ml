(* Emit lib/baseline.ml from lib/baseline.tsv, the web-features data
   scripts/gen_baseline.sh fetches. Each line is a tab-separated record:
   [web-features VERSION], [greenfield PROPERTY] or [support KEY CHROME FIREFOX
   SAFARI SAFARI_IOS], where an engine field is "-" for no support or
   MAJOR.MINOR. *)

let fail_line n msg =
  prerr_endline
    (String.concat "" [ "baseline.tsv:"; string_of_int n; ": "; msg ]);
  exit 1

let read_lines file =
  let ic = open_in file in
  let rec loop n acc =
    match input_line ic with
    | line -> loop (n + 1) ((n, line) :: acc)
    | exception End_of_file ->
        close_in ic;
        List.rev acc
  in
  loop 1 []

let version n = function
  | "-" -> "None"
  | s -> (
      match String.split_on_char '.' s with
      | [ major; minor ] -> (
          match (int_of_string_opt major, int_of_string_opt minor) with
          | Some major, Some minor ->
              String.concat ""
                [
                  "Some ("; string_of_int major; ", "; string_of_int minor; ")";
                ]
          | _ -> fail_line n (String.concat "" [ "bad version "; s ]))
      | _ -> fail_line n (String.concat "" [ "bad version "; s ]))

let string s = String.concat "" [ "\""; String.escaped s; "\"" ]

let () =
  let file = Sys.argv.(1) in
  let dataset = ref None and greenfield = ref [] and support = ref [] in
  List.iter
    (fun (n, line) ->
      match String.split_on_char '\t' line with
      | [ "web-features"; v ] ->
          if !dataset <> None then fail_line n "second web-features line";
          dataset := Some v
      | [ "greenfield"; p ] -> greenfield := p :: !greenfield
      | [ "support"; key; c; f; s; i ] ->
          let row =
            String.concat ""
              [
                "  (";
                string key;
                ", { chrome = ";
                version n c;
                "; firefox = ";
                version n f;
                "; safari = ";
                version n s;
                "; safari_ios = ";
                version n i;
                " });\n";
              ]
          in
          support := row :: !support
      | _ -> fail_line n "unrecognised record")
    (read_lines file);
  let dataset =
    match !dataset with
    | Some v -> v
    | None -> fail_line 0 "no web-features line"
  in
  print_string "(* Generated from lib/baseline.tsv (web-features ";
  print_string dataset;
  print_string
    ")\n\
    \   by scripts/gen_baseline_ml.ml. CSS properties that are not yet\n\
    \   Baseline \"widely available\" (web-features status.baseline other\n\
    \   than \"high\"), so a vendor-prefixed declaration naming one of them\n\
    \   keeps its prefix, and the per-engine first versions for every\n\
    \   css.* BCD key it records. Regenerate the data with\n\
    \   scripts/gen_baseline.sh. *)\n\n\
     let greenfield_properties = [\n";
  List.iter
    (fun p -> print_string (String.concat "" [ "  "; string p; ";\n" ]))
    (List.rev !greenfield);
  print_string
    "]\n\n\
     type version = int * int\n\n\
     type support = {\n\
    \  chrome : version option;\n\
    \  firefox : version option;\n\
    \  safari : version option;\n\
    \  safari_ios : version option;\n\
     }\n\n\
     let dataset_version = ";
  print_string (string dataset);
  print_string "\n\nlet support : (string * support) list = [\n";
  List.iter print_string (List.rev !support);
  print_string "]\n"
