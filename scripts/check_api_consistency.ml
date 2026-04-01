(* CSS API Consistency Checker

   Ensures API consistency across interface definitions, implementations, and
   tests.

   For each interface module (lib/*_intf.ml): - Extracts all top-level type
   definitions - Verifies corresponding read_*/pp_* functions in lib/*.mli -
   Verifies corresponding check_* functions in test/test_*.ml

   Reports missing items with actionable suggestions. *)

open Stdlib

(* Types to ignore in consistency checks *)
let ignored_types =
  [
    "meta";
    (* Abstract type without read/pp functions *)
    "mode";
    (* Internal implementation detail *)
    "kind";
    (* Internal implementation detail *)
    "any_var";
    (* Internal type for existential variables *)
  ]

(* Types where neg (negative) tests are not applicable because the parser
   accepts any identifier as valid input (e.g., grid line names, attribute
   names). *)
let neg_exempt_types =
  [
    "grid_line";
    (* Any identifier is a valid grid line name *)
    "attr_name";
    (* Any identifier is a valid attribute name *)
  ]

module Fs = struct
  let read_file path =
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> really_input_string ic (in_channel_length ic))

  let read_lines path =
    try
      let s = read_file path in
      String.split_on_char '\n' s
    with Sys_error _ -> []

  let list_dir path =
    try Array.to_list (Sys.readdir path) with Sys_error _ -> []
end

let ( // ) a b = if a = "" then b else a ^ Filename.dir_sep ^ b

let contains_sub (s : string) (sub : string) : bool =
  let rex = Re.compile (Re.str sub) in
  Re.execp rex s

(* Color constants for terminal output *)
let red = "\027[31m"
let yellow = "\027[33m"
let cyan = "\027[36m"
let bold = "\027[1m"
let reset = "\027[0m"

let colored color text =
  if Unix.isatty Unix.stdout then color ^ text ^ reset else text

(* Type extraction patterns and functions *)
let type_re = Re.Perl.compile_pat "^[\\s]*type[\\s]+([A-Za-z_][A-Za-z0-9_]*)\\b"

let type_blank_re =
  (* Matches: type _ <name> ... -> captures <name> *)
  Re.Perl.compile_pat "^[\\s]*type[\\s]+_+[\\s]+([A-Za-z_][A-Za-z0-9_]*)\\b"

let extract_types path : string list =
  let lines = Fs.read_lines path in
  let rec loop acc = function
    | [] -> List.rev acc
    | l :: tl -> (
        (* Prefer capturing the name in "type _ name" if present *)
        match Re.exec_opt type_blank_re l with
        | Some g ->
            let tname = Re.Group.get g 1 in
            let acc =
              if tname <> "_" && not (List.mem tname acc) then tname :: acc
              else acc
            in
            loop acc tl
        | None -> (
            match Re.exec_opt type_re l with
            | Some g ->
                let tname = Re.Group.get g 1 in
                let acc =
                  if tname <> "_" && not (List.mem tname acc) then tname :: acc
                  else acc
                in
                loop acc tl
            | None -> loop acc tl))
  in
  loop [] lines

(* Extract types that contain Var constructors from interface files *)
let extract_var_types path : string list =
  let lines = Fs.read_lines path in
  let var_type_re =
    Re.Perl.compile_pat "\\|\\s+Var\\s+of\\s+([A-Za-z_][A-Za-z0-9_]*)\\s+var"
  in
  let type_def_re =
    Re.Perl.compile_pat "^[\\s]*type[\\s]+([A-Za-z_][A-Za-z0-9_]*)\\s*="
  in

  let rec loop acc current_type = function
    | [] -> List.rev acc
    | l :: tl -> (
        (* Check if this line starts a new type definition *)
        match Re.exec_opt type_def_re l with
        | Some g ->
            let type_name = Re.Group.get g 1 in
            loop acc (Some type_name) tl
        | None -> (
            (* Check if this line has a Var constructor *)
            match Re.exec_opt var_type_re l with
            | Some _g -> (
                match current_type with
                | Some tname when not (List.mem tname acc) ->
                    loop (tname :: acc) current_type tl
                | _ -> loop acc current_type tl)
            | None -> loop acc current_type tl))
  in
  loop [] None lines

(* Extract test functions from a test file *)
let re_test_header =
  Re.Perl.compile_pat "^[\\s]*let[\\s]+test_([A-Za-z0-9_]+)[\\s]*\\(\\)[\\s]*="

let re_toplevel_let = Re.Perl.compile_pat "^let[\\s]+[A-Za-z_][A-Za-z0-9_]*"

let prev_nonempty lines idx =
  let rec go i =
    if i < 0 then None
    else
      let pl = List.nth lines i in
      if String.trim pl = "" then go (i - 1) else Some pl
  in
  go (idx - 1)

let is_ignored_comment lines idx =
  match prev_nonempty lines idx with
  | Some pl ->
      contains_sub pl "Not a roundtrip test" || contains_sub pl "ignore-test"
  | None -> false

let extract_test_functions test_file =
  if not (Sys.file_exists test_file) then []
  else
    let lines = Fs.read_lines test_file in
    let tests = ref [] in
    let current_name = ref None in
    let current_header = ref "" in
    let current_line = ref 0 in
    let current_ignored = ref false in
    let buf = Buffer.create 4096 in
    let flush_current () =
      match !current_name with
      | None -> ()
      | Some name ->
          tests :=
            ( name,
              Buffer.contents buf,
              (!current_header, !current_line, !current_ignored) )
            :: !tests;
          Buffer.clear buf;
          current_name := None;
          current_header := "";
          current_line := 0;
          current_ignored := false
    in
    List.iteri
      (fun idx l ->
        match Re.exec_opt re_test_header l with
        | Some g ->
            flush_current ();
            current_name := Some (Re.Group.get g 1);
            current_header := l;
            current_line := idx + 1;
            current_ignored := is_ignored_comment lines idx
        | None ->
            (match !current_name with
            | Some _ -> Buffer.add_string buf (l ^ "\n")
            | None -> ());
            if
              Re.execp re_toplevel_let l
              && not (String.trim l |> String.starts_with ~prefix:"let open")
            then flush_current ())
      lines;
    flush_current ();
    !tests

(* Analyze check and neg patterns in test function body *)
let analyze_test_patterns tname body module_name =
  let check_re = Re.Perl.compile_pat "\\bcheck_([A-Za-z0-9_]+)" in
  let neg_read_re = Re.Perl.compile_pat "neg[\\s]+read_([A-Za-z0-9_]+)" in

  let rec collect_checks pos acc =
    if pos >= String.length body then List.rev acc
    else
      match Re.exec_opt ~pos check_re body with
      | None -> List.rev acc
      | Some g ->
          let name = Re.Group.get g 1 in
          collect_checks (Re.Group.stop g 0) (name :: acc)
  in

  let rec collect_neg_reads pos acc =
    if pos >= String.length body then List.rev acc
    else
      match Re.exec_opt ~pos neg_read_re body with
      | None -> List.rev acc
      | Some g ->
          let name = Re.Group.get g 1 in
          collect_neg_reads (Re.Group.stop g 0) (name :: acc)
  in

  let checks = collect_checks 0 [] in
  let neg_reads = collect_neg_reads 0 [] in
  (* For type t, the test function is test_<module> but the read function can be both "read" and "read_<module>" *)
  (* So if tname equals the module name, we're testing type t and should look for both patterns *)
  let has_neg =
    if tname = module_name then
      (* Testing type t - look for both "neg read" (without suffix) and "neg
         read_<module>" *)
      let has_read = Re.execp (Re.Perl.compile_pat "neg[\\s]+read[\\s]") body in
      let has_read_module =
        Re.execp
          (Re.Perl.compile_pat ("neg[\\s]+read_" ^ module_name ^ "\\b"))
          body
      in
      has_read || has_read_module
    else
      (* Testing other types - look for "neg read_<type>" *)
      let neg_re = Re.Perl.compile_pat ("neg[\\s]+read_" ^ tname ^ "\\b") in
      Re.execp neg_re body
  in

  (checks, neg_reads, has_neg)

(* Check if a .mli file has a val declaration with the given name *)
let file_has_val mli_path name : bool =
  let rex = Re.Perl.compile_pat ("^[\\s]*val[\\s]+" ^ name ^ "\\b") in
  List.exists (fun l -> Re.execp rex l) (Fs.read_lines mli_path)

(* Check consistency for a single CSS module *)
let invalid_tests tests valid_types expected_test_name =
  tests
  |> List.filter (fun (n, _body, (_hdr, _ln, ign)) ->
      (not ign)
      && not (List.exists (fun t -> expected_test_name t = n) valid_types))
  |> List.map (fun (n, _, _) -> n)
  |> List.sort_uniq compare

let missing_tests test_names valid_types expected_test_name =
  List.filter
    (fun t ->
      let expected_name = expected_test_name t in
      (not (List.mem t ignored_types))
      && not (List.mem expected_name test_names))
    valid_types
  |> List.map expected_test_name
  |> List.sort_uniq compare

let check_test_patterns ~lib_dir ~mod_name ~valid_types ~expected_test_name
    ~wrong_checks ~missing_neg tname body =
  let checks, neg_reads, has_neg = analyze_test_patterns tname body mod_name in
  let expected_check_name t = if t = "t" then mod_name else t in
  let expected_for_tname = expected_check_name tname in
  let valid_check_names = List.map expected_check_name valid_types in
  let tname_is_valid_type =
    List.exists (fun t -> expected_test_name t = tname) valid_types
  in
  List.iter
    (fun c ->
      let is_wrong =
        c <> expected_for_tname && c <> "value" && c <> "parse_fails"
        && (((not tname_is_valid_type) && List.mem c valid_check_names)
           || tname_is_valid_type)
      in
      if is_wrong then wrong_checks := (tname, c) :: !wrong_checks)
    checks;
  List.iter
    (fun n ->
      if
        n <> expected_for_tname
        && (((not tname_is_valid_type) && List.mem n valid_types)
           || (tname_is_valid_type && List.mem n valid_types))
      then wrong_checks := (tname, "neg read_" ^ n) :: !wrong_checks)
    neg_reads;
  match List.find_opt (fun t -> expected_test_name t = tname) valid_types with
  | Some typename ->
      let mli_path = lib_dir // (mod_name ^ ".mli") in
      let read_name = if typename = "t" then "read" else "read_" ^ typename in
      if
        (not has_neg) && Sys.file_exists mli_path
        && file_has_val mli_path read_name
        && not (List.mem tname neg_exempt_types)
      then missing_neg := tname :: !missing_neg
  | None -> ()

let check_module_consistency lib_dir test_dir mod_name =
  let intf_file = lib_dir // (mod_name ^ "_intf.ml") in
  let test_file = test_dir // ("test_" ^ mod_name ^ ".ml") in
  if not (Sys.file_exists intf_file) then None
  else
    let valid_types = extract_types intf_file |> List.sort_uniq compare in
    let tests = extract_test_functions test_file in
    let test_names = List.map (fun (n, _, _) -> n) tests in
    let expected_test_name t = if t = "t" then mod_name else t in
    let invalid_tests = invalid_tests tests valid_types expected_test_name in
    let missing_tests =
      missing_tests test_names valid_types expected_test_name
    in
    let wrong_checks = ref [] in
    let missing_neg = ref [] in
    List.iter
      (fun (tname, body, (_hdr, _ln, ignored)) ->
        if not ignored then
          check_test_patterns ~lib_dir ~mod_name ~valid_types
            ~expected_test_name ~wrong_checks ~missing_neg tname body)
      tests;
    Some (mod_name, invalid_tests, missing_tests, !wrong_checks, !missing_neg)

let print_warning_section mod_name header items pp_item =
  if items <> [] then (
    print_string
      (colored yellow "Warning -" ^ " " ^ header ^ " in test_" ^ mod_name
     ^ ".ml:\n");
    List.iter pp_item items;
    print_string "\n")

(* Print consistency results for a module *)
let print_module_results
    (mod_name, invalid_tests, missing_tests, wrong_checks, missing_neg) =
  let has_issues =
    invalid_tests <> [] || missing_tests <> [] || wrong_checks <> []
    || missing_neg <> []
  in
  if has_issues then (
    print_string
      ("\n"
      ^ colored bold (String.capitalize_ascii mod_name ^ " Tests Consistency:")
      ^ "\n");

    print_warning_section mod_name "invalid test_ names" invalid_tests (fun n ->
        print_string ("  test_" ^ n ^ " (not in " ^ mod_name ^ "_intf)\n"));

    let pp_wrong (y, x) =
      if String.starts_with ~prefix:"neg read_" x then
        print_string ("  test_" ^ y ^ ": " ^ x ^ "\n")
      else print_string ("  test_" ^ y ^ ": check_" ^ x ^ "\n")
    in
    print_warning_section mod_name "wrong check_x inside test_y"
      (List.sort_uniq compare wrong_checks)
      pp_wrong;

    print_warning_section mod_name "missing test_x functions"
      (List.sort compare missing_tests) (fun n ->
        print_string ("  test_" ^ n ^ "\n"));

    print_warning_section mod_name "missing neg read_x inside test_x"
      (List.sort compare missing_neg) (fun n ->
        print_string ("  test_" ^ n ^ "\n")))

(* Project root detected by looking for dune-project file. *)
let project_root () =
  let rec search dir =
    if Sys.file_exists (dir // "dune-project") then dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then
        failwith "Could not find project root (no dune-project file found)"
      else search parent
  in
  search (Sys.getcwd ())

let root = project_root ()
let lib_dir = root // "lib"
let test_dir = root // "test"
let variables_path = lib_dir // "variables.ml"

let file_has_let test_path name : bool =
  (* Check for multiple patterns: 1. Direct let binding: let check_foo = ... 2.
     Inline check_value call: check_value "foo" pp_foo read_foo 3. Local helper
     within test function: let check_foo ... inside test_foo function *)
  let lines = Fs.read_lines test_path in

  (* First check for direct let binding at any indentation level *)
  let rex = Re.Perl.compile_pat ("[\\s]*let[\\s]+" ^ name ^ "\\b") in
  if List.exists (fun l -> Re.execp rex l) lines then true
  else if
    (* If checking for check_<typename>, also look for inline check_value
       usage *)
    String.starts_with ~prefix:"check_" name
  then
    let typename = String.sub name 6 (String.length name - 6) in
    (* Look for pattern: check_value "typename" pp_typename read_typename *)
    let inline_pattern =
      Re.Perl.compile_pat
        ("check_value[\\s]+\"" ^ typename ^ "\"[\\s]+pp_" ^ typename
       ^ "[\\s]+read_" ^ typename)
    in
    (* Also check if there's a test function that tests this type *)
    let test_func_pattern =
      Re.Perl.compile_pat ("let[\\s]+test_" ^ typename ^ "[\\s]*\\(\\)")
    in
    List.exists
      (fun l -> Re.execp inline_pattern l || Re.execp test_func_pattern l)
      lines
  else false

(* Statistics tracking *)
type stats = {
  mutable total_types : int;
  mutable missing_read : int;
  mutable missing_pp : int;
  mutable missing_check : int;
  mutable missing_vars_of : int;
  mutable missing_modules : string list;
}

let stats =
  {
    total_types = 0;
    missing_read = 0;
    missing_pp = 0;
    missing_check = 0;
    missing_vars_of = 0;
    missing_modules = [];
  }

(* ANSI color codes for better output *)
let () =
  let intf_files =
    Fs.list_dir lib_dir
    |> List.filter (fun f -> Filename.check_suffix f "_intf.ml")
    |> List.sort compare
  in
  if intf_files = [] then (
    print_string
      (colored yellow "Warning:"
      ^ " No interface files found under " ^ lib_dir ^ "/*_intf.ml\n");
    exit 0);

  let all_missing = ref [] in

  (* Collect all issues first without printing *)
  List.iter
    (fun intf_file ->
      let mod_base =
        match Filename.chop_suffix_opt ~suffix:"_intf.ml" intf_file with
        | Some s -> s
        | None -> String.sub intf_file 0 (max 0 (String.length intf_file - 8))
      in
      let intf_path = lib_dir // intf_file in
      let mli_path = lib_dir // (mod_base ^ ".mli") in
      let test_path = test_dir // ("test_" ^ mod_base ^ ".ml") in

      let types = extract_types intf_path in
      let mli_exists = Sys.file_exists mli_path in
      let test_exists = Sys.file_exists test_path in

      if not mli_exists then
        stats.missing_modules <- mod_base :: stats.missing_modules;

      if types <> [] then
        (* Filter out types we should ignore *)
        let filtered_types =
          List.filter (fun t -> not (List.mem t ignored_types)) types
        in
        List.iter
          (fun tname ->
            stats.total_types <- stats.total_types + 1;
            (* Special case: type t uses 'read' and 'pp' instead of 'read_t' and
               'pp_t' *)
            let read_name = if tname = "t" then "read" else "read_" ^ tname in
            let pp_name = if tname = "t" then "pp" else "pp_" ^ tname in
            let check_name =
              if tname = "t" then "check" else "check_" ^ tname
            in
            let read_ok = mli_exists && file_has_val mli_path read_name in
            let pp_ok = mli_exists && file_has_val mli_path pp_name in
            let check_ok = test_exists && file_has_let test_path check_name in

            if not read_ok then stats.missing_read <- stats.missing_read + 1;
            if not pp_ok then stats.missing_pp <- stats.missing_pp + 1;
            if (not check_ok) && not (List.mem tname ignored_types) then
              stats.missing_check <- stats.missing_check + 1;

            if
              not
                (read_ok && pp_ok && (check_ok || List.mem tname ignored_types))
            then (
              let missing_items = ref [] in
              if not read_ok then missing_items := read_name :: !missing_items;
              if not pp_ok then missing_items := pp_name :: !missing_items;
              if (not check_ok) && not (List.mem tname ignored_types) then
                missing_items := check_name :: !missing_items;

              all_missing := (mod_base, tname, !missing_items) :: !all_missing))
          filtered_types)
    intf_files;

  (* Now print the issues *)
  print_string (colored bold "CSS API Consistency Issues" ^ "\n");
  print_string (String.make 50 '=' ^ "\n\n");

  if stats.missing_modules <> [] then (
    print_string (colored red "Critical:" ^ " Missing module interfaces:\n");
    List.iter
      (fun m -> print_string ("  • " ^ m ^ ".mli\n"))
      (List.rev stats.missing_modules);
    print_string "\n");

  (* Group missing items by file for cleaner output *)
  if !all_missing <> [] then (
    let by_file = Hashtbl.create 10 in

    List.iter
      (fun (m, t, items) ->
        List.iter
          (fun item ->
            let file =
              if String.starts_with ~prefix:"check" item then
                "test/test_" ^ m ^ ".ml"
              else "lib/" ^ m ^ ".mli"
            in
            let current =
              try Hashtbl.find by_file file with Not_found -> []
            in
            Hashtbl.replace by_file file ((m, t, item) :: current))
          items)
      !all_missing;

    print_string (colored bold "Missing API Functions:" ^ "\n\n");

    (* Separate API functions from test functions *)
    let api_files = ref [] in
    let test_files = ref [] in

    Hashtbl.iter
      (fun file items ->
        if String.contains file '.' && String.ends_with ~suffix:".mli" file then
          api_files := (file, items) :: !api_files
        else test_files := (file, items) :: !test_files)
      by_file;

    (* Print API functions first (more critical) *)
    if !api_files <> [] then (
      print_string (colored red "Critical - Missing API Functions:" ^ "\n");
      List.iter
        (fun (file, items) ->
          print_string ("\n" ^ colored cyan file ^ "\n");

          (* Group by type and show example implementations *)
          let by_type = Hashtbl.create 10 in
          List.iter
            (fun (m, t, item) ->
              let current = try Hashtbl.find by_type t with Not_found -> [] in
              Hashtbl.replace by_type t ((m, item) :: current))
            items;

          Hashtbl.iter
            (fun t funcs ->
              print_string ("  type " ^ colored yellow t ^ ":\n");
              List.iter
                (fun (_m, f) ->
                  if f = "read" then
                    print_string "    val read : Reader.t -> t\n"
                  else if f = "pp" then print_string "    val pp : t Pp.t\n"
                  else if String.starts_with ~prefix:"read" f then
                    print_string
                      ("    val " ^ f ^ " : Reader.t -> " ^ t ^ "\n")
                  else if String.starts_with ~prefix:"pp" f then
                    print_string ("    val " ^ f ^ " : " ^ t ^ " Pp.t\n")
                  else print_string ("    val " ^ f ^ "\n"))
                (List.sort compare funcs))
            by_type)
        (List.sort compare !api_files);
      print_string "\n");

    (* Print test functions separately (less critical) *)
    if !test_files <> [] then (
      print_string (colored yellow "Warning - Missing Test Functions:" ^ "\n");
      List.iter
        (fun (file, items) ->
          print_string ("\n" ^ colored cyan file ^ "\n");

          (* Group by type *)
          let by_type = Hashtbl.create 10 in
          List.iter
            (fun (m, t, item) ->
              let current = try Hashtbl.find by_type t with Not_found -> [] in
              Hashtbl.replace by_type t ((m, item) :: current))
            items;

          Hashtbl.iter
            (fun t funcs ->
              print_string ("  type " ^ t ^ ":\n");
              List.iter
                (fun (_m, f) ->
                  if f = "check" then
                    print_string
                      "    let check = check_value \"t\" pp read\n"
                  else
                    print_string
                      ("    let " ^ f ^ " = check_value \"" ^ t ^ "\" pp_" ^ t
                     ^ " read_" ^ t ^ "\n"))
                (List.sort compare funcs))
            by_type)
        (List.sort compare !test_files);
      print_string "\n"));

  (* CSS modules test conformance checks *)
  let css_modules =
    [
      "properties";
      "values";
      "declaration";
      "selector";
      "stylesheet";
      "variables";
    ]
  in
  let consistency_warnings = ref 0 in
  List.iter
    (fun mod_name ->
      match check_module_consistency lib_dir test_dir mod_name with
      | Some (_, invalid_tests, missing_tests, wrong_checks, missing_neg) ->
          print_module_results
            (mod_name, invalid_tests, missing_tests, wrong_checks, missing_neg);
          (* Count any consistency issues *)
          let issues_count =
            List.length invalid_tests + List.length missing_tests
            + List.length wrong_checks + List.length missing_neg
          in
          consistency_warnings := !consistency_warnings + issues_count
      | None -> ())
    css_modules;

  (* Summary with counts *)
  print_string (String.make 50 '-' ^ "\n");

  let api_missing = stats.missing_read + stats.missing_pp in
  let test_missing = stats.missing_check in

  if api_missing > 0 then
    print_string
      (colored red "ERROR:" ^ " " ^ string_of_int api_missing
     ^ " missing API functions (read_*/pp_*)\n");

  if test_missing > 0 then
    print_string
      (colored yellow "WARNING:" ^ " " ^ string_of_int test_missing
     ^ " missing test functions (check_*)\n");

  if !consistency_warnings > 0 then
    print_string
      (colored red "ERROR:" ^ " "
      ^ string_of_int !consistency_warnings
      ^ " test consistency issues found\n");

  (* Check for missing property case handling in vars_of_property *)
  let check_property_vars_handling () =
    let properties_intf_path = lib_dir // "properties_intf.ml" in

    (* Extract property cases that use types with Var constructors *)
    let extract_property_cases_with_vars path =
      let lines = Fs.read_lines path in
      let property_re =
        Re.Perl.compile_pat
          "^[\\s]*\\|[\\s]+([A-Za-z_][A-Za-z0-9_]*)[\\s]*:[\\s]*([A-Za-z_][A-Za-z0-9_]*.*?)\\s+property"
      in
      let var_types = extract_var_types path in

      let rec loop acc = function
        | [] -> List.rev acc
        | l :: tl -> (
            match Re.exec_opt property_re l with
            | Some g ->
                let prop_name = Re.Group.get g 1 in
                let prop_type = Re.Group.get g 2 in
                (* Extract the base type name (before any list/option
                   modifiers) *)
                let base_type =
                  prop_type |> String.split_on_char ' ' |> List.hd
                  |> String.trim
                in
                if List.mem base_type var_types then
                  loop ((prop_name, base_type) :: acc) tl
                else loop acc tl
            | None -> loop acc tl)
      in
      loop [] lines
    in

    let property_cases_with_vars =
      extract_property_cases_with_vars properties_intf_path
    in

    (* Check which cases are handled in vars_of_property *)
    let check_property_case_handled prop_name =
      let lines = Fs.read_lines variables_path in
      let pattern = "\\|[\\s]+" ^ prop_name ^ "[\\s]*," in
      let rex = Re.Perl.compile_pat pattern in
      List.exists (fun l -> Re.execp rex l) lines
    in

    let missing_cases = ref [] in
    List.iter
      (fun (prop_name, prop_type) ->
        if not (check_property_case_handled prop_name) then
          missing_cases := (prop_name, prop_type) :: !missing_cases)
      property_cases_with_vars;

    stats.missing_vars_of <- List.length !missing_cases;

    if !missing_cases <> [] then (
      print_string
        ("\n"
        ^ colored bold "Missing Property Case Handling in vars_of_property:"
        ^ "\n");
      print_string
        (colored red "Critical:" ^ " Missing property cases in "
        ^ colored cyan "lib/variables.ml"
        ^ " vars_of_property function:\n");
      List.iter
        (fun (prop_name, prop_type) ->
          print_string
            ("  | " ^ prop_name ^ ", value -> vars_of_"
            ^ String.lowercase_ascii prop_type
            ^ " value\n"))
        (List.sort compare !missing_cases);
      print_string "\n";
      true)
    else false
  in

  let vars_of_issues = check_property_vars_handling () in

  let exit_code =
    if api_missing > 0 || !consistency_warnings > 0 || vars_of_issues then 1
    else 0
  in

  if exit_code = 1 then
    print_string
      ("\n" ^ colored red "Action Required:"
     ^ " Critical issues must be fixed before proceeding.\n")
  else if test_missing > 0 then
    print_string
      ("\n" ^ colored yellow "Recommendation:"
     ^ " Consider adding test functions for complete coverage.\n");

  exit exit_code
