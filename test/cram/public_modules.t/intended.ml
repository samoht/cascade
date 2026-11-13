module Css_selector = Cascade.Css.Selector
module Css_selector_summary = Cascade.Css.Selector_summary
module Css_aria = Cascade.Css.Aria
module Css_color_space = Cascade.Css.Color_space
module Css_context = Cascade.Css.Context
module Css_pp = Cascade.Css.Pp
module Css_values = Cascade.Css.Values
module Css_declaration = Cascade.Css.Declaration
module Css_properties = Cascade.Css.Properties
module Css_variables = Cascade.Css.Variables
module Css_optimize = Cascade.Css.Optimize
module Css_stylesheet = Cascade.Css.Stylesheet
module Css_media = Cascade.Css.Media
module Css_container = Cascade.Css.Container
module Css_supports = Cascade.Css.Supports
module Css_keyframe = Cascade.Css.Keyframe
module Css_font_face = Cascade.Css.Font_face
module Css_nest = Cascade.Css.Nest

let _ = Cascade.Cursor.of_string ""
let _ = Cascade.Parser.escape_ident "public"
let _ : Cascade.Token.t option = None

let keep_statement
    (_ : Cascade.Stylesheet.statement) :
    Cascade.Stylesheet.statement Cascade.Stylesheet.edit =
  Cascade.Stylesheet.Keep

let _ = Cascade.Stylesheet.edit_statements keep_statement []
