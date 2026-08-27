(* A caller that does not run the whole optimizer still collapses a run of
   adjacent blocks, through the installed public API alone. *)
open Cascade

let statements css =
  match Css.of_string css with
  | Ok { stylesheet; _ } -> stylesheet
  | Error e -> failwith (Error.to_string e)

let show label merge css =
  print_string label;
  print_string ": ";
  print_endline (Css.to_string ~minify:true (merge (statements css)))

let () =
  show "media" Optimize.merge_consecutive_media
    "@media print{.a{opacity:0}}@media print{.b{opacity:1}}";
  show "supports" Optimize.merge_consecutive_supports
    "@supports (display:grid){.a{opacity:0}}\n\
     @supports (display:grid){.b{opacity:1}}";
  show "starting-style" Optimize.merge_consecutive_starting_style
    "@starting-style{.a{opacity:0}}@starting-style{.b{opacity:1}}"
