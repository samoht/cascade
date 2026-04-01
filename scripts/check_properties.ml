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
  let lines = String.split_on_char '\n' content in
  let constructor_pattern =
    Re.Perl.compile_pat "^[ ]*\\| ([A-Z][A-Za-z0-9_]*) :"
  in
  List.fold_left
    (fun acc line ->
      match Re.exec_opt constructor_pattern line with
      | Some g ->
          let name = Re.Group.get g 1 in
          name :: acc
      | None -> acc)
    [] lines

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
  let region = String.concat "\n" region_lines in

  (* Extract property names from pattern matches *)
  let pattern =
    Re.Perl.compile_pat "\\| \"[^\"]+\" -> Prop ([A-Z][A-Za-z0-9_]*)"
  in
  let extract_from_string str =
    Re.all pattern str |> List.map (fun g -> Re.Group.get g 1)
  in
  extract_from_string region

let () =
  (* Try to find the project root *)
  let rec find_project_root dir =
    if Sys.file_exists (Filename.concat dir "dune-project") then dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then "." else find_project_root parent
  in

  let project_root = find_project_root (Sys.getcwd ()) in
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
