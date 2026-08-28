(* Script to validate that all properties in properties_intf.ml are handled in
   properties.ml's read_property function, and that the tag table ordering a
   property identity lists each of them exactly once under its own tag.

   Two constructors sharing a tag would make [Properties.compare_property]
   answer [0] for two different properties, and every ordered container keyed on
   a property - the shadowing coverage set in [Cover] among them - would treat
   them as one. The compiler cannot see that, so it is pinned here, together
   with the agreement the ordering owes [Declaration.equal_prop_key]. *)

open Cascade

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

(* Extract property constructors from properties_intf.ml. [~static:true] drops
   the two payload-carrying constructors, which [read_any_property] does not
   answer for; the tag table covers them like any other. *)
let extract_property_constructors ~static content =
  let keep_constructor = function
    | "Custom_property" | "Unknown_property" -> not static
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
  let constructor_of line =
    match Re.exec_opt constructor_pattern line with
    | None -> None
    | Some g ->
        let constructor = Re.Group.get g 1 in
        if keep_constructor constructor then Some constructor else None
  in
  let next_state in_property acc line =
    if starts_property line then `Continue (true, acc)
    else if in_property && starts_next_type line then `Stop
    else
      match (in_property, constructor_of line) with
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

(* Read the text between a START/END marker pair in properties.ml *)
let marked_region ~marker content =
  let start_pattern = Re.Perl.compile_pat (".*" ^ marker ^ "_START.*") in
  let end_pattern = Re.Perl.compile_pat (".*" ^ marker ^ "_END.*") in

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
  String.concat " " region_lines

(* Extract the [name -> Prop Constructor] table from read_any_property *)
let extract_name_table content =
  let region = marked_region ~marker:"PROPERTY_MATCHING" content in
  let pattern =
    Re.Perl.compile_pat "\\| \"([^\"]+)\" +-> +Prop +([A-Z][A-Za-z0-9_]*)"
  in
  Re.all pattern region
  |> List.map (fun g -> (Re.Group.get g 1, Re.Group.get g 2))

(* Extract the [Constructor -> tag] table from property_tag *)
let extract_tag_table content =
  let region = marked_region ~marker:"PROPERTY_TAG" content in
  let pattern =
    Re.Perl.compile_pat "\\| ([A-Z][A-Za-z0-9_]*)(?: _)? +-> +([0-9]+)"
  in
  Re.all pattern region
  |> List.map (fun g -> (Re.Group.get g 1, int_of_string (Re.Group.get g 2)))

(* Extract the constructor pairs from eq_property. Both sides are read so that a
   table pairing two different constructors is caught, not just a missing
   one. *)
let extract_eq_table content =
  let region = marked_region ~marker:"PROPERTY_EQ" content in
  let pattern =
    Re.Perl.compile_pat
      "\\| ([A-Z][A-Za-z0-9_]*)(?: [a-z])?, ([A-Z][A-Za-z0-9_]*)(?: [a-z])? ->"
  in
  Re.all pattern region
  |> List.map (fun g -> (Re.Group.get g 1, Re.Group.get g 2))

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

  let all_properties =
    extract_property_constructors ~static:true intf_content
  in
  let name_table = extract_name_table impl_content in
  let handled_properties = List.map snd name_table in

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

  Fmt.pr "\u{2713} All %d properties are properly mapped in read_property@."
    (StringSet.cardinal all_set);

  (* The tag table covers every constructor, [Custom_property] and
     [Unknown_property] included, so it is checked against the full list rather
     than the static one [read_any_property] answers for. *)
  let tag_table = extract_tag_table impl_content in
  let tagged = List.map fst tag_table in
  let tagged_set = StringSet.of_list tagged in
  let declared_set =
    StringSet.of_list (extract_property_constructors ~static:false intf_content)
  in
  let untagged = StringSet.diff declared_set tagged_set in
  let stray = StringSet.diff tagged_set declared_set in
  if not (StringSet.is_empty untagged) then (
    Fmt.pr "ERROR: property constructors missing from property_tag:@.";
    StringSet.iter (fun s -> Fmt.pr "  - %s@." s) untagged;
    exit 1);
  if not (StringSet.is_empty stray) then (
    Fmt.pr "ERROR: property_tag lists names that are not constructors:@.";
    StringSet.iter (fun s -> Fmt.pr "  - %s@." s) stray;
    exit 1);
  if not (Int.equal (List.length tagged) (StringSet.cardinal tagged_set)) then (
    Fmt.pr "ERROR: property_tag lists a constructor twice@.";
    exit 1);

  let module IntMap = Map.Make (Int) in
  let by_tag =
    List.fold_left
      (fun acc (name, tag) ->
        IntMap.update tag
          (function None -> Some [ name ] | Some ns -> Some (name :: ns))
          acc)
      IntMap.empty tag_table
  in
  let shared = IntMap.filter (fun _ names -> List.length names > 1) by_tag in
  if not (IntMap.is_empty shared) then (
    Fmt.pr "ERROR: property_tag gives one tag to several constructors:@.";
    IntMap.iter
      (fun tag names ->
        List.iter (fun name -> Fmt.pr "  - %d: %s@." tag name) names)
      shared;
    exit 1);
  Fmt.pr "\u{2713} All %d property constructors carry a distinct tag@."
    (List.length tagged);

  (* The order the table induces has to answer [0] exactly where
     [Declaration.equal_prop_key] answers [true]. Proved over one key per
     property name the parser accepts, plus two names apiece for the two
     payload-carrying constructors, which the tag alone cannot separate. *)
  let key_of_name name =
    match Properties.read_any_property (Cursor.of_string name) with
    | Properties.Prop p -> Declaration.Key p
  in
  let keys =
    Array.of_list
      (Declaration.Key (Properties.Custom_property "--a")
     :: Declaration.Key (Properties.Custom_property "--b")
     :: Declaration.Key (Properties.Unknown_property "-x-a")
     :: Declaration.Key (Properties.Unknown_property "-x-b")
      :: List.map (fun (name, _) -> key_of_name name) name_table)
  in
  let disagreements = ref 0 in
  let asymmetries = ref 0 in
  let sign n = Int.compare n 0 in
  Array.iter
    (fun a ->
      Array.iter
        (fun b ->
          let c = Declaration.compare_prop_key a b in
          let same = Int.equal c 0 in
          if not (Bool.equal same (Declaration.equal_prop_key a b)) then
            incr disagreements;
          let back = Declaration.compare_prop_key b a in
          if not (Int.equal (sign c) (-sign back)) then incr asymmetries)
        keys)
    keys;
  if !disagreements > 0 then (
    Fmt.pr "ERROR: %d key pairs where compare = 0 and equal_prop_key disagree@."
      !disagreements;
    exit 1);
  if !asymmetries > 0 then (
    Fmt.pr "ERROR: %d key pairs where compare is not antisymmetric@."
      !asymmetries;
    exit 1);
  let n = Array.length keys in
  Fmt.pr "\u{2713} compare_prop_key agrees with equal_prop_key over %d pairs@."
    (n * n);

  (* [eq_property]'s table ends in a wildcard, so the compiler cannot see a
     constructor left out of it: such a property would read back as [None] from
     [Declaration.value_of] for ever. *)
  let eq_table = extract_eq_table impl_content in
  let mispaired = List.filter (fun (x, y) -> not (String.equal x y)) eq_table in
  if not (List.is_empty mispaired) then (
    Fmt.pr "ERROR: eq_property arms pairing two different constructors:@.";
    List.iter (fun (x, y) -> Fmt.pr "  - %s, %s@." x y) mispaired;
    exit 1);
  let eq_set = StringSet.of_list (List.map fst eq_table) in
  let uncovered = StringSet.diff declared_set eq_set in
  let stray_eq = StringSet.diff eq_set declared_set in
  if not (StringSet.is_empty uncovered) then (
    Fmt.pr "ERROR: property constructors missing from eq_property:@.";
    StringSet.iter (fun s -> Fmt.pr "  - %s@." s) uncovered;
    exit 1);
  if not (StringSet.is_empty stray_eq) then (
    Fmt.pr "ERROR: eq_property lists names that are not constructors:@.";
    StringSet.iter (fun s -> Fmt.pr "  - %s@." s) stray_eq;
    exit 1);
  Fmt.pr "\u{2713} eq_property answers for all %d property constructors@."
    (StringSet.cardinal eq_set);

  (* The proof has to arrive exactly where the comparison says the properties
     are the same one. *)
  let eq_disagreements = ref 0 in
  Array.iter
    (fun (Declaration.Key a) ->
      Array.iter
        (fun (Declaration.Key b) ->
          let same = Int.equal (Properties.compare_property a b) 0 in
          if not (Bool.equal same (Option.is_some (Properties.eq_property a b)))
          then incr eq_disagreements)
        keys)
    keys;
  if !eq_disagreements > 0 then (
    Fmt.pr
      "ERROR: %d key pairs where compare_property and eq_property disagree@."
      !eq_disagreements;
    exit 1);
  Fmt.pr "\u{2713} eq_property agrees with compare_property over %d pairs@."
    (n * n)
