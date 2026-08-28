(* [cascade prune <page.html>... <style.css>]: remove the rules no element of
   the documents can match, or rank what survives by how little it is used. The
   analysis is {!Cascade.Prune}; this command is the HTML glue and the
   report. *)

open Cmdliner
module Css = Cascade.Css
module Selector = Cascade.Selector
module Prune = Cascade.Prune
module P = Prune.Make (Html_node)

let err_msg fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

(* The stylesheet is the last positional so the documents can be a shell glob,
   and so the order reads the way [cascade apply] takes its two. *)
let split_inputs files =
  match List.rev files with
  | css :: (_ :: _ as rev_docs) -> Ok (List.rev rev_docs, css)
  | _ -> err_msg "expected one or more HTML documents and then a stylesheet"

(* Cmdliner's [file] conv only checks that the path exists, so a directory or a
   file with no read permission reaches the command. Reading nothing where a
   document was named would call its rules unused, so that is a usage error
   rather than a document with no element in it. *)
let read_document path =
  try Ok (Html.roots (Html.parse (Cli_io.read_file path)))
  with Sys_error msg -> err_msg "Error reading %s: %s" path msg

let read_documents paths =
  List.fold_left
    (fun acc path ->
      Result.bind acc (fun roots ->
          Result.map (fun r -> roots @ r) (read_document path)))
    (Ok []) paths

(* The three reasons a rule is where it is, kept apart: what the documents ruled
   out, what they used, and what they could not answer for. *)
let split_entries entries =
  let unused, used, unmodelled =
    List.fold_left
      (fun (unused, used, unmodelled) e ->
        let name = Selector.to_string e.Prune.selector in
        match e.Prune.verdict with
        | Prune.Unused -> (name :: unused, used, unmodelled)
        | Prune.Used n -> (unused, (n, name) :: used, unmodelled)
        | Prune.Unmodelled -> (unused, used, name :: unmodelled))
      ([], [], []) entries
  in
  ( List.rev unused,
    List.rev used |> List.stable_sort (fun (a, _) (b, _) -> Int.compare a b),
    List.rev unmodelled )

let counts ppf ~unused ~used ~unmodelled =
  Fmt.pf ppf
    "rules: %d total, %d removed, %d kept as used, %d kept without a verdict@."
    (unused + used + unmodelled)
    unused used unmodelled

(* A rule kept for want of a model is the measure of what the analysis cannot
   see, so it is reported apart from the rules the documents used rather than
   folded in with them. *)
let report ~documents (analysis : Prune.analysis) =
  let unused, used, unmodelled = split_entries analysis.Prune.entries in
  Fmt.pr "documents: %d, elements: %d@." documents analysis.Prune.elements;
  if unused <> [] then begin
    Fmt.pr "@.unused, removed by a run without --dry-run:@.";
    List.iter (fun name -> Fmt.pr "  %s@." name) unused
  end;
  if used <> [] then begin
    Fmt.pr "@.used, fewest matched elements first:@.";
    List.iter (fun (n, name) -> Fmt.pr "  %4d  %s@." n name) used
  end;
  if unmodelled <> [] then begin
    Fmt.pr "@.no verdict, kept: the matcher has no model for the selector.@.";
    List.iter (fun name -> Fmt.pr "  %s@." name) unmodelled
  end;
  Fmt.pr "@.";
  counts Fmt.stdout ~unused:(List.length unused) ~used:(List.length used)
    ~unmodelled:(List.length unmodelled);
  Fmt.pr
    "@.Unused means unused in these documents: a class a script adds at \
     runtime is not in them.@."

(* [Css.to_buffer] never ends with a newline; a stylesheet on stdout is a text
   file, so it gets one. *)
let emit sheet =
  let buf = Buffer.create 4096 in
  Css.to_buffer buf sheet;
  if Buffer.length buf > 0 then Buffer.add_char buf '\n';
  Buffer.output_buffer stdout buf

let run dry_run files () =
  match
    Result.bind (split_inputs files) (fun (docs, css) ->
        Result.map (fun roots -> (docs, css, roots)) (read_documents docs))
  with
  | Error e -> Error e
  | Ok (docs, css, roots) ->
      let input = Cli_io.read_input css in
      Cli_io.check_not_all_dropped input;
      let analysis = P.analyse ~sheet:input.Cli_io.stylesheet roots in
      (* Nothing to match against makes every rule look unused, which is an
         answer about the documents rather than about the stylesheet. *)
      if analysis.Prune.elements = 0 then begin
        Fmt.epr
          "Error: the documents hold no element, so every rule would look \
           unused@.";
        Stdlib.exit 1
      end;
      if dry_run then report ~documents:(List.length docs) analysis
      else begin
        let unused, used, unmodelled = split_entries analysis.Prune.entries in
        counts Fmt.stderr ~unused:(List.length unused) ~used:(List.length used)
          ~unmodelled:(List.length unmodelled);
        emit analysis.Prune.sheet
      end;
      Ok ()

let dry_run_arg =
  let doc =
    "Write the ranked report instead of the pruned stylesheet: what would go, \
     what stays and how few elements matched it, and what was kept for want of \
     a model."
  in
  Arg.(value & flag & info [ "dry-run" ] ~doc)

let files_arg =
  let doc = "The HTML documents to match against, then the stylesheet." in
  Arg.(value & pos_all file [] & info [] ~docv:"PAGE.html... STYLE.css" ~doc)

let term = Term.(const run $ dry_run_arg $ files_arg $ const ())

let cmd =
  let doc = "Remove the CSS rules a set of documents cannot use" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Match every rule of $(i,STYLE.css) against every element of the given \
         documents and write the stylesheet back without the rules that \
         matched nothing.";
      `P
        "A rule is removed only when the matcher has a model for its selector \
         and every element answers that it does not match. A selector outside \
         what the matcher models ($(b,:hover), a pseudo-element, \
         $(b,:nth-child())) is kept and never counted as unused, and so is a \
         selector list with one such branch. A $(b,@media), $(b,@supports) or \
         $(b,@container) condition is never evaluated: it asks about a device, \
         not about a document, so the rules inside are judged by their own \
         selectors. A statement that names no element ($(b,@keyframes), \
         $(b,@font-face), $(b,@property), $(b,@import), $(b,@layer)) is kept.";
      `P
        "The documents are the whole of what the analysis sees. A class a \
         script adds at runtime is not in them, so a rule waiting for one is \
         removed; check those before shipping the result.";
      `P
        "Nesting is flattened before anything is judged, since a nested \
         selector is written against its parent. Pipe the result through \
         $(b,cascade --minify) to nest it again.";
      `S Manpage.s_exit_status;
      `P "$(tname) exits with:";
      `I ("0", "on success");
      `I
        ( "1",
          "if the documents hold no element, or the stylesheet parsed to \
           nothing" );
    ]
  in
  Cmd.v (Cmd.info "prune" ~doc ~man) Term.(term_result term)
