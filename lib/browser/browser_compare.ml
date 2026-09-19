let ( // ) = Filename.concat

module Css = Cascade.Css
module Html = Cascade_html.Html
module Prune = Cascade.Prune.Make (Cascade_html.Html_node)

type render = {
  viewport : string;
  state : string;
  x : int;
  y : int;
  width : int;
  height : int;
  first_size : string;
  second_size : string;
}

type difference = {
  viewport : string;
  state : string;
  element : string;
  pseudo : string;
  property : string;
  first : string;
  second : string;
}

type t = {
  meta : (string * string) list;
  renders : render list;
  differences : difference list;
}

(* The job the driver reads is JSON. It holds strings alone, so a string escaper
   is the whole writer: a quote, a backslash and a control byte are escaped, and
   bytes at or above 0x80 pass through as the UTF-8 they are part of. *)
let add_json_string buf s =
  let hex = "0123456789abcdef" in
  Buffer.add_char buf '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.code c < 0x20 ->
          Buffer.add_string buf "\\u00";
          Buffer.add_char buf hex.[Char.code c lsr 4];
          Buffer.add_char buf hex.[Char.code c land 0xf]
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"'

let add_sheets buf key sheets =
  Buffer.add_string buf (String.concat "" [ ",\""; key; "\":[" ]);
  List.iteri
    (fun i (name, css) ->
      if i > 0 then Buffer.add_char buf ',';
      Buffer.add_string buf "{\"name\":";
      add_json_string buf name;
      Buffer.add_string buf ",\"css\":";
      add_json_string buf css;
      Buffer.add_char buf '}')
    sheets;
  Buffer.add_char buf ']'

(* [sampling] is what the driver reads the widths and states off; [sheets] is
   what it renders. *)
let job_json ~html ~sampling sheets =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "{\"html\":";
  add_json_string buf html;
  add_sheets buf "sheets" sheets;
  add_sheets buf "sampling" sampling;
  Buffer.add_char buf '}';
  Buffer.contents buf

(* ===== The page and what it can use ===== *)

let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r' || c = '\012'

(* The driver's own test: the source opens, after whitespace, with [<!doctype],
   in any case. *)
let has_doctype html =
  let n = String.length html in
  let rec first i = if i < n && is_space html.[i] then first (i + 1) else i in
  let i = first 0 in
  i + 9 <= n
  && String.equal (String.lowercase_ascii (String.sub html i 9)) "<!doctype"

let is_stylesheet_link (e : Html.element) =
  String.equal e.tag "link"
  &&
  match Html.attribute e "rel" with
  | None -> false
  | Some rel ->
      String.lowercase_ascii rel
      |> String.map (fun c -> if is_space c then ' ' else c)
      |> String.split_on_char ' ' |> List.mem "stylesheet"

(* The document's own [<style>] elements and stylesheet links go, wherever they
   stand, so the sheets compared are the only ones in effect. *)
let rec strip_styles removed nodes =
  List.filter
    (function
      | Html.Element e when String.equal e.tag "style" || is_stylesheet_link e
        ->
          incr removed;
          false
      | Html.Element e ->
          e.children <- strip_styles removed e.children;
          true
      | Html.Text _ | Html.Comment _ | Html.Doctype _ -> true)
    nodes

type page = {
  document : string;  (** What the browser renders. *)
  roots : Html.element list;  (** The same page, as the matcher reads it. *)
  styles_removed : int;
  doctype_added : bool;
}

(* The page is prepared once, here, and the browser is handed that same tree
   printed back: pruning a sheet to the page is only sound over the elements the
   browser builds, so the two must not prepare the page apart. *)
let prepare html =
  let doctype_added = not (has_doctype html) in
  let html =
    if doctype_added then String.concat "" [ "<!DOCTYPE html>\n"; html ]
    else html
  in
  let removed = ref 0 in
  let doc = strip_styles removed (Html.parse html) in
  {
    document = Html.to_string doc;
    roots = Html.roots doc;
    styles_removed = !removed;
    doctype_added;
  }

(* Whether every warning the reader raised cost a declaration and nothing more:
   one it stamped as a dropped declaration, or one raised while reading a
   declaration, which the reader recovers from inside the rule it belongs to. A
   warning raised anywhere else may have cost a whole rule, stamped or not: a
   selector the reader refuses loses its rule under an unstamped warning. *)
let drops_no_rule warnings =
  List.for_all
    (fun (w : Cascade.Error.t) ->
      match (w.recovery, w.path) with
      | Cascade.Error.Recovery.Dropped { construct = Declaration; _ }, _ -> true
      | Dropped { construct = Rule; _ }, _ -> false
      | Recovered, "read_declaration" :: _ -> true
      | Recovered, _ -> false)
    warnings

(* What the driver samples a sheet for: the sheet less every rule no element of
   the page can match, as [cascade prune] removes them. A rule whose selector
   the matcher has no model for is kept, [:hover] among them, so every state a
   kept rule names is still sampled; a width named only by removed rules cannot
   change a computed value, so it is not. A sheet the reader lost a whole rule
   from is sampled as written: a dropped rule leaves nothing to say what it
   would have matched. *)
let sampling_sheet roots css =
  match Css.of_string css with
  | Ok { Css.stylesheet; warnings; _ } when drops_no_rule warnings ->
      let analysis = Prune.analyse ~sheet:stylesheet roots in
      if analysis.elements = 0 then css
      else Css.to_string ~minify:true analysis.sheet
  | Ok _ | Error _ -> css

let write_file path contents =
  Out_channel.with_open_bin path (fun oc -> output_string oc contents)

let read_lines ic =
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file -> List.rev acc
  in
  loop []

(* The driver's TSV: [m] metadata, [r] a render that differs, [d] a computed
   value the elements under it disagree on, and [x] an error the page
   reported. *)
let parse_output lines =
  let meta, renders, differences, errors =
    List.fold_left
      (fun (meta, renders, differences, errors) line ->
        match String.split_on_char '\t' line with
        | [ "m"; key; value ] ->
            ((key, value) :: meta, renders, differences, errors)
        | [ "r"; viewport; state; x; y; width; height; first_size; second_size ]
          -> (
            match
              ( int_of_string_opt x,
                int_of_string_opt y,
                int_of_string_opt width,
                int_of_string_opt height )
            with
            | Some x, Some y, Some width, Some height ->
                let r =
                  {
                    viewport;
                    state;
                    x;
                    y;
                    width;
                    height;
                    first_size;
                    second_size;
                  }
                in
                (meta, r :: renders, differences, errors)
            | _ -> (meta, renders, differences, errors))
        | [ "d"; viewport; state; element; pseudo; property; first; second ] ->
            let d =
              { viewport; state; element; pseudo; property; first; second }
            in
            (meta, renders, d :: differences, errors)
        | "x" :: rest ->
            (meta, renders, differences, String.concat "\t" rest :: errors)
        | _ -> (meta, renders, differences, errors))
      ([], [], [], []) lines
  in
  ( {
      meta = List.rev meta;
      renders = List.rev renders;
      differences = List.rev differences;
    },
    List.rev errors )

(* [browser_diff.js] is compiled into the library, so an installed cascade
   carries its own driver; it is written beside the job it runs on. *)
let run_driver ~node ~chrome ~work ~job =
  let script = work // "browser_diff.js" in
  let job_file = work // "job.json" in
  let errors = work // "driver.err" in
  write_file script Browser_diff_js.source;
  write_file job_file job;
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
      Error
        (String.concat "" [ "the browser driver failed:\n"; String.trim text ])

let mkdtemp () =
  let base = Filename.get_temp_dir_name () in
  let rec attempt n =
    let name =
      String.concat ""
        [
          "cascade-browser-";
          string_of_int (Unix.getpid ());
          "-";
          string_of_int n;
        ]
    in
    let dir = base // name in
    match Unix.mkdir dir 0o700 with
    | () -> dir
    | exception Unix.Unix_error (Unix.EEXIST, _, _) -> attempt (n + 1)
  in
  attempt 0

let meta_int report key =
  Option.bind (List.assoc_opt key report.meta) int_of_string_opt

let identical report = report.renders = []

let version chrome =
  match Browser.chrome_version chrome with
  | Some (major, minor) ->
      String.concat "" [ string_of_int major; "."; string_of_int minor ]
  | None -> "unknown"

let ( let* ) = Result.bind

let run ~html sheets =
  let* () =
    if List.length sheets = 2 then Ok ()
    else
      Error
        "the browser comparison takes exactly two sheets: the report carries \
         one first and one second, so a third has nowhere to go"
  in
  let* node =
    Option.to_result
      ~none:"no node on PATH (or in NODE); the browser comparison needs it"
      (Browser.node_binary ())
  in
  let* chrome =
    Option.to_result
      ~none:
        "no headless Chromium found (CHROME, PATH, the puppeteer and \
         playwright caches, or the macOS application); the browser comparison \
         needs one"
      (Browser.chrome_binary ())
  in
  let page = prepare html in
  let sampling =
    List.map (fun (name, css) -> (name, sampling_sheet page.roots css)) sheets
  in
  let work = mkdtemp () in
  let* report, errors =
    run_driver ~node ~chrome ~work
      ~job:(job_json ~html:page.document ~sampling sheets)
  in
  let meta =
    List.filter
      (fun (key, _) ->
        not (List.mem key [ "document_styles_removed"; "doctype_added" ]))
      report.meta
  in
  let report =
    {
      report with
      meta =
        ("browser", version chrome)
        :: ("browser_path", chrome)
        :: ("document_styles_removed", string_of_int page.styles_removed)
        :: ("doctype_added", if page.doctype_added then "1" else "0")
        :: meta;
    }
  in
  match (errors, meta_int report "captures") with
  | _ :: _, _ -> Error (String.concat "\n" ("the page reported:" :: errors))
  | [], Some n when n > 0 -> Ok report
  | [], _ -> Error "the browser rendered nothing, so nothing was compared"

(* ===== The report for a reader ===== *)

let add_header ~first ~second ~html report buf =
  let add = Buffer.add_string buf in
  let meta key = Option.value ~default:"?" (List.assoc_opt key report.meta) in
  add
    (String.concat "" [ "Rendered "; html; " under "; first; " and "; second ]);
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
         "Captured ";
         meta "captures";
         " renders of ";
         meta "elements";
         " elements";
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
  add
    (String.concat ""
       [
         "Renders that differ: ";
         string_of_int (List.length report.renders);
         "\n";
       ])

(* Where a render differs: the box the differing pixels span, or the two page
   sizes when the page laid out to different extents. *)
let add_renders report buf =
  List.iter
    (fun (r : render) ->
      Buffer.add_string buf
        (String.concat ""
           [
             "  ";
             r.viewport;
             " ";
             r.state;
             ": ";
             (if String.equal r.first_size r.second_size then
                String.concat ""
                  [
                    string_of_int r.width;
                    "x";
                    string_of_int r.height;
                    " pixels differ at (";
                    string_of_int r.x;
                    ",";
                    string_of_int r.y;
                    ")";
                  ]
              else
                String.concat ""
                  [
                    "the page is ";
                    r.first_size;
                    " under the first sheet and ";
                    r.second_size;
                    " under the second";
                  ]);
             "\n";
           ]))
    report.renders

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
let where ~(contexts : context list) (ctxs : context list) =
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
    else List.map (fun (v, st) -> String.concat " " [ v; st ]) ctxs
  in
  match names with
  | [] -> ""
  | names -> String.concat "" [ "  ["; String.concat ", " names; "]" ]

(* The heading over the computed values, with how many there were and, when the
   listing stopped short, how many are listed. *)
let add_differences_heading report buf =
  match report.differences with
  | [] -> ()
  | _ :: _ ->
      Buffer.add_string buf
        (String.concat ""
           [
             "\nComputed values the elements under those pixels disagree on (";
             Option.value ~default:"?"
               (List.assoc_opt "differences" report.meta);
             (match List.assoc_opt "truncated" report.meta with
             | Some n -> String.concat "" [ ", listing the first "; n ]
             | None -> "");
             "):\n";
           ])

(* A pseudo-element repeating its element's difference in the same contexts
   inherits it, and is counted rather than listed. *)
let add_differences report buf =
  let add = Buffer.add_string buf in
  add_differences_heading report buf;
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
        let head = String.concat "" [ element; pseudo ] in
        if head <> !last then (
          last := head;
          add (String.concat "" [ "\n"; head; "\n" ]));
        add
          (String.concat ""
             [
               "  ";
               property;
               ": ";
               first;
               " -> ";
               second;
               where ~contexts ctxs;
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

let pp ppf report =
  let buf = Buffer.create 1024 in
  add_renders report buf;
  add_differences report buf;
  Format.pp_print_string ppf (Buffer.contents buf)

let to_string ~first ~second ~html report =
  let buf = Buffer.create 1024 in
  add_header ~first ~second ~html report buf;
  add_renders report buf;
  add_differences report buf;
  Buffer.contents buf
