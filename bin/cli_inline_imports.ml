(** Filesystem-backed [@import] inlining for the cascade CLI. *)

open Cascade

(* CSS allows [@import url("foo.css?v=1")] and [#fragment]. The host filesystem
   doesn't, so strip them before opening the file. *)
let strip_url_suffix = Syntax.strip_url_suffix
let is_remote = Syntax.is_remote_url

let path_is_within ~root path =
  String.equal root path
  ||
  let root_len = String.length root in
  String.starts_with ~prefix:root path
  &&
  if String.ends_with ~suffix:Filename.dir_sep root then true
  else String.length path > root_len && path.[root_len] = Filename.dir_sep.[0]

let warn (verbosity : Cli_log.verbosity) ~file fmt =
  match verbosity with
  | Quiet -> Format.ifprintf Format.err_formatter fmt
  | Normal | Verbose -> Cli_io.warning ~file fmt

let canonical_file ~verbosity ~import_root resolved =
  let filename = Syntax.url_file_path resolved in
  let canonical = Unix.realpath filename in
  match import_root with
  | None -> Some canonical
  | Some root when path_is_within ~root canonical -> Some canonical
  | Some root ->
      warn verbosity ~file:resolved
        "not inlined: its canonical path %s is outside --import-root %s"
        canonical root;
      None

let warn_cannot_read verbosity resolved msg =
  warn verbosity ~file:resolved "cannot read: %s" msg

let canonical_import_root = function
  | None -> None
  | Some root -> (
      try Some (Unix.realpath root)
      with Unix.Unix_error (error, _, _) ->
        raise
          (Fmt.kstr
             (fun message -> Sys_error message)
             "--import-root %s: %s" root (Unix.error_message error)))

(* Walk the parsed stylesheet, follow every [@import] URL on disk and parse each
   referenced file once, recursing through transitive imports. The resulting
   (resolved-url, content) table is what [Css.inline_imports] consumes. *)
let cache_resolved ~verbosity ~import_root imports resolved =
  if Hashtbl.mem imports resolved then None
  else
    try
      match canonical_file ~verbosity ~import_root resolved with
      | None -> None
      | Some filename ->
          let content = Cli_io.read_file filename in
          Hashtbl.add imports resolved content;
          Some (Cli_io.parse_css verbosity ~filename content)
    with
    | Sys_error msg ->
        warn_cannot_read verbosity resolved msg;
        None
    | Unix.Unix_error (error, _, _) ->
        warn_cannot_read verbosity resolved (Unix.error_message error);
        None

let resolve_import verbosity loader url =
  let path = strip_url_suffix url in
  match Css.Context.resolve_url loader path with
  | Error msg ->
      warn verbosity ~file:url "cannot resolve @import: %s" msg;
      None
  | Ok resolved -> Some resolved

let preload ~verbosity ~base_url ~import_root stylesheet =
  let import_root = canonical_import_root import_root in
  let imports = Hashtbl.create 16 in
  let rec scan_under base sheet =
    let loader = Css.Context.loader ~base_url:base () in
    Css.fold (scan_stmt loader) () sheet
  and scan_stmt loader () stmt =
    match Css.as_import stmt with
    | None -> ()
    | Some import_rule ->
        let url = Css.decode_import_url import_rule.url in
        if not (is_remote url) then handle_import loader url
  and handle_import loader url =
    match resolve_import verbosity loader url with
    | None -> ()
    | Some resolved -> (
        match cache_resolved ~verbosity ~import_root imports resolved with
        | None -> ()
        | Some inner -> scan_under resolved inner)
  in
  scan_under base_url stylesheet;
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) imports []

let run ~verbosity ~base_url ~import_root stylesheet =
  let imports = preload ~verbosity ~base_url ~import_root stylesheet in
  let loader = Css.Context.loader ~base_url ~imports () in
  Css.inline_imports loader stylesheet
