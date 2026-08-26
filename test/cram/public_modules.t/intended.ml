module Public_apply = Cascade.Apply
module Public_aria = Cascade.Aria
module Public_color_space = Cascade.Color_space
module Public_component = Cascade.Component
module Public_container = Cascade.Container
module Public_context = Cascade.Context
module Public_css = Cascade.Css
module Public_cursor = Cascade.Cursor
module Public_declaration = Cascade.Declaration
module Public_error = Cascade.Error
module Public_font_face = Cascade.Font_face
module Public_keyframe = Cascade.Keyframe
module Public_lexer = Cascade.Lexer
module Public_loc = Cascade.Loc
module Public_media = Cascade.Media
module Public_nest = Cascade.Nest
module Public_optimize = Cascade.Optimize
module Public_parser = Cascade.Parser
module Public_pp = Cascade.Pp
module Public_properties = Cascade.Properties
module Public_reader = Cascade.Reader
module Public_resolve = Cascade.Resolve
module Public_selector = Cascade.Selector
module Public_selector_summary = Cascade.Selector_summary
module Public_sort = Cascade.Sort
module Public_stats = Cascade.Stats
module Public_stylesheet = Cascade.Stylesheet
module Public_supports = Cascade.Supports
module Public_syntax = Cascade.Syntax
module Public_token = Cascade.Token
module Public_values = Cascade.Values
module Public_variables = Cascade.Variables

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
