(* Reduce computed-style differences to the render-real ones. The differential
   test compares getComputedStyle strings, which differ for spellings cascade
   considers equivalent ([0% 0%] vs [0px 0px], [red] vs [rgb(255, 0, 0)]). Each
   line on stdin is [index<TAB>tag<TAB>property<TAB>valueA<TAB>valueB]; a line
   is echoed only when its two values are not [Css_compare]-canonically equal,
   i.e. when inlining actually changed the render. *)

let wrap prop value = String.concat "" [ "x{"; prop; ":"; value; "}" ]

let canon_equal prop a b =
  try
    Cascade_diff.Css_compare.equal ~mode:`Canonical (wrap prop a) (wrap prop b)
  with Cascade.Reader.Parse_error _ | Failure _ | Invalid_argument _ -> false

let () =
  try
    while true do
      let line = input_line stdin in
      match String.split_on_char '\t' line with
      | _ :: _ :: prop :: a :: b :: _ ->
          if not (canon_equal prop a b) then print_endline line
      | _ -> ()
    done
  with End_of_file -> ()
