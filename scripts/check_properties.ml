(* Script to validate that all properties in properties_intf.ml are handled in
   properties.ml's read_property function *)

let read_file filename =
  let ic = open_in filename in
  let rec loop acc =
    try
      let line = input_line ic in
      loop (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  String.concat "\n" (loop [])

(* Extract property constructors from properties_intf.ml *)
let extract_property_constructors content =
  let is_static_property_constructor = function
    | "Custom_property" | "Unknown_property" -> false
    | _ -> true
  in
  let lines = String.split_on_char '\n' content in
  let property_start_pattern =
    Re.Perl.compile_pat "^[ ]*type[ ]+'a[ ]+property[ ]*="
  in
  let next_type_pattern = Re.Perl.compile_pat "^[ ]*type[ ]+" in
  let constructor_pattern =
    Re.Perl.compile_pat "^[ ]*\\| ([A-Z][A-Za-z0-9_]*) :"
  in
  let starts_property line = Re.execp property_start_pattern line in
  let starts_next_type line = Re.execp next_type_pattern line in
  let static_constructor line =
    match Re.exec_opt constructor_pattern line with
    | None -> None
    | Some g ->
        let constructor = Re.Group.get g 1 in
        if is_static_property_constructor constructor then Some constructor
        else None
  in
  let next_state in_property acc line =
    if starts_property line then `Continue (true, acc)
    else if in_property && starts_next_type line then `Stop
    else
      match (in_property, static_constructor line) with
      | true, Some constructor -> `Continue (true, constructor :: acc)
      | true, None -> `Continue (true, acc)
      | false, _ -> `Continue (false, acc)
  in
  let rec loop in_property acc = function
    | [] -> List.rev acc
    | line :: rest -> (
        match next_state in_property acc line with
        | `Stop -> List.rev acc
        | `Continue (in_property, acc) -> loop in_property acc rest)
  in
  loop false [] lines

(* Extract handled properties from properties.ml read_property function *)
let extract_handled_properties content =
  (* Find the read_property function with PROPERTY_MATCHING_START marker *)
  let start_pattern = Re.Perl.compile_pat ".*PROPERTY_MATCHING_START.*" in
  let end_pattern = Re.Perl.compile_pat ".*PROPERTY_MATCHING_END.*" in

  (* Split content into lines *)
  let lines = String.split_on_char '\n' content in

  (* Find the region between markers *)
  let rec find_region lines in_region acc =
    match lines with
    | [] -> acc
    | line :: rest ->
        if Re.execp end_pattern line then find_region rest false acc
        else if in_region then find_region rest true (line :: acc)
        else if Re.execp start_pattern line then find_region rest true acc
        else find_region rest false acc
  in

  let region_lines = List.rev (find_region lines false []) in
  (* Joining with a single space rather than a newline lets the regex match [|
     "foo" -> Prop X] whether or not [dune fmt] wrapped it across two lines for
     length. *)
  let region = String.concat " " region_lines in

  (* Extract property names from pattern matches *)
  let pattern =
    Re.Perl.compile_pat "\\| \"[^\"]+\" +-> +Prop +([A-Z][A-Za-z0-9_]*)"
  in
  let extract_from_string str =
    Re.all pattern str |> List.map (fun g -> Re.Group.get g 1)
  in
  extract_from_string region

let () =
  (* dune runs this runtest action from _build/default/scripts with the lib deps
     materialised at ../lib; anchor there, as Sys.executable_name is relative in
     an opam sandbox. *)
  let project_root = ".." in
  let intf_file = Filename.concat project_root "lib/properties_intf.ml" in
  let impl_file = Filename.concat project_root "lib/properties.ml" in

  if not (Sys.file_exists intf_file) then (
    prerr_string ("Cannot find " ^ intf_file ^ "\n");
    exit 1);

  if not (Sys.file_exists impl_file) then (
    prerr_string ("Cannot find " ^ impl_file ^ "\n");
    exit 1);

  let intf_content = read_file intf_file in
  let impl_content = read_file impl_file in

  let all_properties = extract_property_constructors intf_content in
  let handled_properties = extract_handled_properties impl_content in

  (* Convert to sets for comparison *)
  let module StringSet = Set.Make (String) in
  let all_set = StringSet.of_list all_properties in
  let handled_set = StringSet.of_list handled_properties in

  let missing = StringSet.diff all_set handled_set in
  let extra = StringSet.diff handled_set all_set in

  if not (StringSet.is_empty missing) then (
    print_string "ERROR: Missing property mappings in read_property:\n";
    StringSet.iter (fun s -> print_string ("  - " ^ s ^ "\n")) missing;
    exit 1);

  if not (StringSet.is_empty extra) then (
    print_string
      "WARNING: Extra property mappings in read_property (not in interface):\n";
    StringSet.iter (fun s -> print_string ("  - " ^ s ^ "\n")) extra);

  print_string
    ("✓ All "
    ^ string_of_int (StringSet.cardinal all_set)
    ^ " properties are properly mapped in read_property\n")
