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
