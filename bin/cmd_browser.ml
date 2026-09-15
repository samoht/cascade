(* The browser comparison behind [cascade diff --browser --html DOC A B]: the
   two sheets are rendered over the one supplied document in a headless Chromium
   and every computed-style value they disagree on is reported. The oracle is
   the browser and nothing else: the values are compared as the browser spells
   them, and the one reading cascade adds, whether the two paint the same, is
   the browser's too, taken off a canvas. A run that reaches no browser, samples
   nothing or breaks in the driver fails rather than reporting equivalence. *)

module J = Cli_json

let ( // ) = Filename.concat

type difference = {
  viewport : string;
  state : string;
  element : string;
  pseudo : string;
  property : string;
  first : string;
  second : string;
  paints_same : bool;
}

type report = {
  meta : (string * string) list;
  differences : difference list;
  errors : string list;
}

(* [browser_diff.js] is compiled into the binary, so an installed cascade
   carries its own driver; it is written beside the job it runs on. *)
let write_file path contents =
  Out_channel.with_open_bin path (fun oc -> output_string oc contents)

let job_json ~html ~sheets =
  J.Obj
    [
      ("html", J.String html);
      ( "sheets",
        J.List
          (List.map
             (fun (name, css) ->
               J.Obj [ ("name", J.String name); ("css", J.String css) ])
             sheets) );
    ]

let read_lines ic =
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file -> List.rev acc
  in
  loop []

let parse_output lines =
  List.fold_left
    (fun r line ->
      match String.split_on_char '\t' line with
      | [ "m"; key; value ] -> { r with meta = (key, value) :: r.meta }
      | [
       "d"; viewport; state; element; pseudo; property; first; second; paints;
      ] ->
          {
            r with
            differences =
              {
                viewport;
                state;
                element;
                pseudo;
                property;
                first;
                second;
                paints_same = String.equal paints "1";
              }
              :: r.differences;
          }
      | "x" :: rest -> { r with errors = String.concat "\t" rest :: r.errors }
      | _ -> r)
    { meta = []; differences = []; errors = [] }
    lines
  |> fun r ->
  {
    meta = List.rev r.meta;
    differences = List.rev r.differences;
    errors = List.rev r.errors;
  }

let run_driver ~node ~chrome ~work ~job =
  let script = work // "browser_diff.js" in
  let job_file = work // "job.json" in
  let errors = work // "driver.err" in
  write_file script Browser_diff_js.source;
  write_file job_file (J.to_string job);
  let cmd =
    String.concat " "
      [
        String.concat "" [ "CHROME="; Filename.quote chrome ];
        Filename.quote node;
        Filename.quote script;
        Filename.quote job_file;
        Filename.quote work;
        String.concat "" [ "2>"; Filename.quote errors ];
      ]
  in
  let ic = Unix.open_process_in cmd in
  let lines = read_lines ic in
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> Ok (parse_output lines)
  | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
      let text = In_channel.with_open_bin errors In_channel.input_all in
      Error (String.trim text)

let meta_int report key =
  Option.bind (List.assoc_opt key report.meta) int_of_string_opt

(* ===== The human report ===== *)

let pp_header ~file1 ~file2 ~html report buf =
  let add = Buffer.add_string buf in
  let meta key = Option.value ~default:"?" (List.assoc_opt key report.meta) in
  add (String.concat "" [ "Rendered "; html; " under "; file1; " and "; file2 ]);
  add "\n";
  add
    (String.concat ""
       [
         "Browser: ";
         meta "browser";
         "; viewports: ";
         meta "viewports";
         "; states: ";
         meta "states";
         "\n";
       ]);
  add
    (String.concat ""
       [
         "Sampled ";
         meta "elements";
         " elements and ";
         meta "pseudos";
         " over ";
         meta "properties";
         " properties (";
         meta "samples";
         " values)";
         "\n";
       ]);
  (match meta_int report "document_styles_removed" with
  | Some n when n > 0 ->
      add
        (String.concat ""
           [
             "The document's own ";
             string_of_int n;
             " style element(s) or stylesheet link(s) were removed\n";
           ])
  | _ -> ());
  if List.assoc_opt "doctype_added" report.meta = Some "1" then
    add "The document had no doctype; one was added for standards mode\n";
  let visible =
    List.length (List.filter (fun d -> not d.paints_same) report.differences)
  in
  add
    (String.concat ""
       [
         "Differences: ";
         meta "differences";
         " computed values, ";
         string_of_int visible;
         " of them painting differently";
         (match List.assoc_opt "truncated" report.meta with
         | Some n -> String.concat "" [ " (listing the first "; n; ")" ]
         | None -> "");
         "\n";
       ])

(* One difference is reported once with the contexts it holds in, since most
   hold under every viewport and state. *)
type context = string * string

let key d = (d.element, d.pseudo, d.property, d.first, d.second)

let group_differences report =
  List.fold_left
    (fun acc d ->
      let k = key d in
      let seen = try List.assoc k acc with Not_found -> [] in
      (k, (d.viewport, d.state) :: seen) :: List.remove_assoc k acc)
    [] report.differences
  |> List.rev_map (fun (k, ctxs) -> (k, List.sort_uniq compare ctxs))

(* A context set that is every viewport under some states, or every state at
   some viewports, is named by the states or the viewports alone; the whole set
   is named by nothing. *)
let pp_where ~(contexts : context list) (ctxs : context list) =
  let viewports = List.sort_uniq compare (List.map fst contexts) in
  let states = List.sort_uniq compare (List.map snd contexts) in
  let own_states = List.sort_uniq compare (List.map snd ctxs) in
  let own_viewports = List.sort_uniq compare (List.map fst ctxs) in
  let product xs ys =
    List.concat_map (fun x -> List.map (fun y -> (x, y)) ys) xs
  in
  let is_product pairs =
    List.length ctxs = List.length pairs
    && List.for_all (fun c -> List.mem c ctxs) pairs
  in
  let names =
    if List.length ctxs = List.length contexts then []
    else if is_product (product viewports own_states) then own_states
    else if is_product (product own_viewports states) then own_viewports
    else List.map (fun (v, st) -> v ^ " " ^ st) ctxs
  in
  match names with
  | [] -> ""
  | names -> String.concat "" [ "  ["; String.concat ", " names; "]" ]

(* A pseudo-element repeating its element's difference in the same contexts
   inherits it, and is counted rather than listed. *)
let pp_differences report buf =
  let add = Buffer.add_string buf in
  let contexts =
    List.sort_uniq compare
      (List.map (fun d -> (d.viewport, d.state)) report.differences)
  in
  let grouped = group_differences report in
  let element_has (element, _, property, first, second) ctxs =
    List.exists
      (fun ((e, ps, pr, f, sc), c) ->
        String.equal e element && String.equal ps "" && String.equal pr property
        && String.equal f first && String.equal sc second && c = ctxs)
      grouped
  in
  let inherited = ref 0 in
  let last = ref "" in
  List.iter
    (fun (((element, pseudo, property, first, second) as k), ctxs) ->
      if pseudo <> "" && element_has k ctxs then incr inherited
      else
        let head = element ^ pseudo in
        if head <> !last then (
          last := head;
          add (String.concat "" [ "\n"; head; "\n" ]));
        let paints =
          List.exists (fun d -> key d = k && d.paints_same) report.differences
        in
        add
          (String.concat ""
             [
               "  ";
               property;
               ": ";
               first;
               " -> ";
               second;
               (if paints then "  (paints the same)" else "");
               pp_where ~contexts ctxs;
               "\n";
             ]))
    grouped;
  if !inherited > 0 then
    add
      (String.concat ""
         [
           "\n";
           string_of_int !inherited;
           " pseudo-element value(s) repeat their element's difference\n";
         ])

let pp_report ~file1 ~file2 ~html report =
  let buf = Buffer.create 1024 in
  pp_header ~file1 ~file2 ~html report buf;
  pp_differences report buf;
  Buffer.contents buf

(* ===== The JSON document ===== *)

let json_difference d =
  J.Obj
    [
      ("viewport", J.String d.viewport);
      ("state", J.String d.state);
      ("element", J.String d.element);
      ("pseudo", J.String d.pseudo);
      ("property", J.String d.property);
      ("first", J.String d.first);
      ("second", J.String d.second);
      ("paints_same", J.Bool d.paints_same);
    ]

let words s = List.filter (fun w -> w <> "") (String.split_on_char ' ' s)

let json_report ~file1 ~file2 ~html ~chrome report =
  let meta key = Option.value ~default:"" (List.assoc_opt key report.meta) in
  let int key = J.Int (Option.value ~default:0 (meta_int report key)) in
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
          [ ("path", J.String chrome); ("version", J.String (meta "browser")) ]
      );
      ("viewports", strings "viewports");
      ("states", strings "states");
      ("pseudo_elements", strings "pseudos");
      ("elements", int "elements");
      ("properties", int "properties");
      ("samples", int "samples");
      ("document_styles_removed", int "document_styles_removed");
      ("doctype_added", J.Bool (meta "doctype_added" = "1"));
      ("differences", int "differences");
      ( "visible_differences",
        J.Int
          (List.length
             (List.filter (fun d -> not d.paints_same) report.differences)) );
      ("truncated", J.Bool (List.mem_assoc "truncated" report.meta));
      ("errors", J.List (List.map (fun e -> J.String e) report.errors));
      ("changes", J.List (List.map json_difference report.differences));
    ]

(* ===== Entry point ===== *)

let fail msg =
  Fmt.epr "Error: %s@." msg;
  Stdlib.exit Cli_exit.cannot_determine

let mkdtemp () =
  let base = Filename.get_temp_dir_name () in
  let rec attempt n =
    let dir = base // Fmt.str "cascade-browser-%d-%d" (Unix.getpid ()) n in
    match Unix.mkdir dir 0o700 with
    | () -> dir
    | exception Unix.Unix_error (Unix.EEXIST, _, _) -> attempt (n + 1)
  in
  attempt 0

let compare ~json ~html ~file1 ~file2 =
  let node =
    match Browser.node_binary () with
    | Some n -> n
    | None ->
        fail "no node on PATH (or in NODE); the browser comparison needs it"
  in
  let chrome =
    match Browser.chrome_binary () with
    | Some c -> c
    | None ->
        fail
          "no headless Chromium found (CHROME, PATH, the puppeteer and \
           playwright caches, or the macOS application); the browser \
           comparison needs one"
  in
  let document = In_channel.with_open_bin html In_channel.input_all in
  let css1, _ = Cli_io.read_source file1 in
  let css2, _ = Cli_io.read_source file2 in
  let work = mkdtemp () in
  let job = job_json ~html:document ~sheets:[ (file1, css1); (file2, css2) ] in
  let report =
    match run_driver ~node ~chrome ~work ~job with
    | Ok report -> report
    | Error err ->
        fail (String.concat "" [ "the browser driver failed:\n"; err ])
  in
  let version =
    match Browser.chrome_version chrome with
    | Some (major, minor) -> Fmt.str "%d.%d" major minor
    | None -> "unknown"
  in
  let report = { report with meta = ("browser", version) :: report.meta } in
  (match report.errors with
  | [] -> ()
  | errors -> fail (String.concat "\n" ("the page reported:" :: errors)));
  (match meta_int report "samples" with
  | Some n when n > 0 -> ()
  | _ -> fail "the browser sampled nothing, so nothing was compared");
  if json then (
    print_string (J.to_string (json_report ~file1 ~file2 ~html ~chrome report));
    print_newline ())
  else print_string (pp_report ~file1 ~file2 ~html report);
  match report.differences with [] -> Ok () | _ :: _ -> Stdlib.exit 1
