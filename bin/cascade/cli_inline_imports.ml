(** Filesystem-backed [@import] inlining for the cascade CLI. *)

open Cascade

(* CSS allows [@import url("foo.css?v=1")] and [#fragment]. The host filesystem
   doesn't, so strip them before opening the file. *)
let strip_url_suffix url =
  let cut_at c s =
    match String.index_opt s c with Some i -> String.sub s 0 i | None -> s
  in
  url |> cut_at '?' |> cut_at '#'

let is_remote url =
  let starts_with prefix =
    String.length url >= String.length prefix
    && String.sub url 0 (String.length prefix) = prefix
  in
  starts_with "http://" || starts_with "https://" || starts_with "data:"
  || starts_with "//"

(* Walk the parsed stylesheet, follow every [@import] URL on disk and parse each
   referenced file once, recursing through transitive imports. The resulting
   (resolved-url, content) table is what [Css.inline_imports] consumes. *)
let preload ~base_url stylesheet =
  let imports = Hashtbl.create 16 in
  let rec scan_under base sheet =
    let loader = Css.Context.loader ~base_url:base () in
    Css.fold (scan_stmt loader) () sheet
  and scan_stmt loader () stmt =
    match Css.as_import stmt with
    | None -> ()
    | Some import_rule -> (
        let url = Css.decode_import_url import_rule.url in
        if is_remote url then ()
        else
          let path = strip_url_suffix url in
          match Css.Context.resolve_url loader path with
          | Error msg ->
              Fmt.epr "warning: cannot resolve @import %s: %s@." url msg
          | Ok resolved -> (
              if not (Hashtbl.mem imports resolved) then
                try
                  let content = Cli_io.read_file resolved in
                  Hashtbl.add imports resolved content;
                  let inner = Cli_io.parse_css ~filename:resolved content in
                  scan_under resolved inner
                with Sys_error msg ->
                  Fmt.epr "warning: cannot read %s: %s@." resolved msg))
  in
  scan_under base_url stylesheet;
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) imports []

let run ~base_url stylesheet =
  let imports = preload ~base_url stylesheet in
  let loader = Css.Context.loader ~base_url ~imports () in
  Css.inline_imports loader stylesheet
