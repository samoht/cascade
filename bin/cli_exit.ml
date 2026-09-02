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
