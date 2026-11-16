open Cascade
open Css

(* Test helper: compose optimize + minified to_string the way [to_string
   ~minify:true] used to behave implicitly. *)
let minify s = s |> Css.optimize |> Css.to_string ~minify:true

(* Helper selectors for tests *)
let btn = Selector.class_ "btn"
let card = Selector.class_ "card"
