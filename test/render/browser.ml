let ( // ) = Filename.concat

let getenv name =
  match Sys.getenv_opt name with Some "" | None -> None | Some v -> Some v

let executable path =
  (try Sys.file_exists path && not (Sys.is_directory path)
   with Sys_error _ -> false)
  &&
    try
      Unix.access path [ Unix.X_OK ];
      true
    with Unix.Unix_error _ | Sys_error _ -> false

let on_path name =
  let dirs =
    String.split_on_char ':' (Option.value ~default:"" (getenv "PATH"))
  in
  List.find_map
    (fun d ->
      if d = "" then None
      else
        let p = d // name in
        if executable p then Some p else None)
    dirs

(* The browser caches keep one directory per version, so the largest path is the
   newest build. *)
let rec search_tree root names depth acc =
  if depth <= 0 then acc
  else
    match Sys.readdir root with
    | exception Sys_error _ -> acc
    | entries ->
        Array.fold_left
          (fun acc entry ->
            let p = root // entry in
            if try Sys.is_directory p with Sys_error _ -> false then
              search_tree p names (depth - 1) acc
            else if List.mem entry names && executable p then p :: acc
            else acc)
          acc entries

let chrome_binary () =
  match getenv "CHROME" with
  | Some c when executable c -> Some c
  | Some _ | None -> (
      let names =
        [
          "chromium";
          "chromium-browser";
          "google-chrome";
          "google-chrome-stable";
        ]
      in
      match List.find_map on_path names with
      | Some c -> Some c
      | None -> (
          let home = Option.value ~default:"" (getenv "HOME") in
          let caches =
            [
              home // ".cache" // "puppeteer";
              home // "Library" // "Caches" // "ms-playwright";
              home // ".cache" // "ms-playwright";
            ]
          in
          let binaries =
            [
              "chrome-headless-shell";
              "headless_shell";
              "Chromium";
              "Google Chrome for Testing";
            ]
          in
          let found =
            List.concat_map (fun c -> search_tree c binaries 6 []) caches
          in
          match List.sort compare found with
          | [] ->
              let app =
                "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
              in
              if executable app then Some app else None
          | l -> Some (List.nth l (List.length l - 1))))

let node_binary () =
  match getenv "NODE" with
  | Some n when executable n -> Some n
  | Some _ | None -> on_path "node"

let skip harness reason =
  print_endline (String.concat "" [ "SKIP: "; harness; " ("; reason; ")" ]);
  exit 0

(* Setting CASCADE_NO_BROWSER silences a gate, and a gate that did not run is
   not a pass: only the value below, which names the run for what it is, exits
   0. A machine with no browser still skips, because there is nothing there to
   silence. *)
let acknowledged = "unchecked"

let suppressed harness =
  match getenv "CASCADE_NO_BROWSER" with
  | None -> ()
  | Some v when String.equal v acknowledged ->
      print_endline
        (String.concat ""
           [
             "SKIP: ";
             harness;
             " (CASCADE_NO_BROWSER=";
             acknowledged;
             ", so this run checks nothing)";
           ]);
      exit 0
  | Some v ->
      prerr_endline
        (String.concat ""
           [
             "FAIL: ";
             harness;
             " is suppressed by CASCADE_NO_BROWSER=";
             v;
             "; a gate that did not run is not a pass. Set CASCADE_NO_BROWSER=";
             acknowledged;
             " to exit 0 and say so.";
           ]);
      exit 1
