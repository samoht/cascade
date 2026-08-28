open Cascade

let _ = Declaration.pp
let _ = Declaration.to_string
let _ = Stylesheet.empty
let _ = Stylesheet.read
let _ = Stylesheet.to_string
let _ = Stylesheet.pp_stylesheet
let _ = Keyframe.to_string
let _ = Css.to_string
let _ = Container.to_string
let _ : Container.t Pp.t = Container.pp
let _ : Stylesheet.t Pp.t = Stylesheet.pp
let _ : Css.t Pp.t = Css.pp
let _ : Optimize.objective = `Transfer
let _ : Css.Optimize.objective = `Raw
let _ :
    ?optimize:bool ->
    ?minify:bool ->
    ?mode:Css.mode ->
    Css.declaration list ->
    string =
  Css.inline_style_of_declarations
let _ = Pp.float
let _ = Css.Gradient_direction.of_string
let _ = Css.shadow
let _ : Css.shadow Css.kind = Css.Shadow

(* The statement-merging passes are callable on their own, so a caller that
   does not run the whole optimizer can still collapse a run of blocks. *)
type merge_block =
  ?optimize_merged_block:
    (Stylesheet.statement list -> Stylesheet.statement list) ->
  Stylesheet.statement list ->
  Stylesheet.statement list

let _ : merge_block = Optimize.merge_consecutive_layers
let _ : merge_block = Optimize.merge_consecutive_media
let _ : merge_block = Optimize.merge_consecutive_supports
let _ : merge_block = Optimize.merge_consecutive_containers
let _ : merge_block = Optimize.merge_consecutive_starting_style
let _ : ?owner:Stylesheet.rule -> merge_block = Optimize.merge_distant_media

let _ : Stylesheet.statement list -> Stylesheet.statement list =
  Optimize.merge_named_layers_by_name

let _ : Stylesheet.statement list -> Stylesheet.statement list =
  Optimize.drop_empty_rules

(* Two statements can be compared and keyed on without rendering either to CSS
   text, and a colour can be given an alpha without re-spelling it by hand. *)
let _ : Css.statement -> Css.statement -> bool = Css.equal_statement
let _ : Css.statement -> int = Css.hash_statement
let _ : Css.Values.color -> Css.Values.color -> bool = Css.Values.equal_color
let _ : Css.Values.color -> int = Css.Values.hash_color

let _ : Css.Values.color -> Css.Values.alpha -> Css.Values.color =
  Css.Values.with_alpha
