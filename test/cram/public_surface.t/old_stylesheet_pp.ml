open Cascade

let _ :
    ?minify:bool ->
    ?indent:int ->
    ?lossless:bool ->
    ?enforce_spec:bool ->
    Stylesheet.t ->
    string =
  Stylesheet.pp
