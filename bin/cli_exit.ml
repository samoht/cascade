(* The verdict a comparison reaches when it finds no difference and could not
   read part of what it compared: it has nothing to report and cannot claim the
   inputs are equivalent either. Cmdliner reserves 123, 124 and 125, so 2 is
   free. *)
let cannot_determine = 2

let by_code a b =
  Int.compare Cmdliner.Cmd.Exit.(info_code a) Cmdliner.Cmd.Exit.(info_code b)

let with_defaults custom =
  let is_overridden default =
    List.exists
      (fun info ->
        Int.equal
          (Cmdliner.Cmd.Exit.info_code info)
          (Cmdliner.Cmd.Exit.info_code default))
      custom
  in
  Cmdliner.Cmd.Exit.defaults
  |> List.filter (fun info -> not (is_overridden info))
  |> List.rev_append custom |> List.sort by_code
