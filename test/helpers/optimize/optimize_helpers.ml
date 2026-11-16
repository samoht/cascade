(** Helpers shared by the optimize test suites. *)

open Cascade

let hex_color s = Css.Values.hex s

let media_min_width px : Css.Media.t =
  Css.Media.Cond
    (Css.Media.Feature
       (Css.Media.Plain (Css.Media.Min Css.Media.Width, Css.Media.Length (Px px))))

(* Compose optimize + minified to_string the way [to_string ~minify:true] used
   to behave implicitly. *)
let minify s = s |> Css.optimize |> Css.to_string ~minify:true

let statement_of_rule (rule : Css.Stylesheet.rule) =
  Css.rule ~selector:rule.selector ~nested:rule.nested ?merge_key:rule.merge_key
    rule.declarations

let rule_of_statement stmt =
  match Css.as_rule stmt with
  | Some (selector, declarations, nested) ->
      ({ selector; declarations; nested; merge_key = None }
        : Css.Stylesheet.rule)
  | None -> failwith "Expected Rule"

let optimized_string ?scope ?targets ?(enforce_spec = false) css =
  css |> Cursor.of_string |> Css.Stylesheet.read
  |> Css.Optimize.stylesheet ?scope ?targets ~enforce_spec
  |> Css.Stylesheet.to_string ~minify:true ~enforce_spec
  |> String.trim

let minify_str css =
  match Css.of_string ~strict:false css with
  | Ok { Css.stylesheet; _ } -> minify stylesheet |> String.trim
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)
