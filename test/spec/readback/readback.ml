(* Does cascade read back everything it writes, over GENERATED input?

   test/fixpoint asks the same question over the corpora the repo ships, and
   says so in its own header: "Nothing here is generated, and nothing here needs
   regenerating". This asks it over values drawn by
   {!Cascade_spec_inventory.Value_gen}, the generator the accept-set harness
   uses, so the population moves with the seed instead of with the repository.

   No browser is involved. The question is entirely about cascade: a value it
   ACCEPTED, printed, and then could not read, or read as something else, is a
   defect whatever any browser thinks of the value. A value it rejected is not
   this harness's business - the accept-set oracle owns that direction - so a
   declaration that does not parse, or parses with a warning, is skipped.

   Two failures are reported apart:

   unreadable the printed form does not parse unstable it parses, and prints as
   something else

   Both are checked in the two output modes that have to agree with themselves,
   minified and pretty, and across them: pretty and minified are two spellings
   of one AST, so reading either back and minifying must give the same text. *)

let seed = ref 0
let full = ref false
let one_css = ref None

let usage () =
  prerr_endline
    "readback [--seed N] [--full] [--css 'a{color:red}']\n\
    \  --seed N   the generator seed (default 0)\n\
    \  --full     every generated value, rather than a sample per property\n\
    \  --css S    run one sheet and print what each step did";
  exit 2

let rec parse_args i =
  if i < Array.length Sys.argv then
    match Sys.argv.(i) with
    | "--seed" when i + 1 < Array.length Sys.argv ->
        seed := int_of_string Sys.argv.(i + 1);
        parse_args (i + 2)
    | "--full" ->
        full := true;
        parse_args (i + 1)
    | "--css" when i + 1 < Array.length Sys.argv ->
        one_css := Some Sys.argv.(i + 1);
        parse_args (i + 2)
    | _ -> usage ()

(* A sheet cascade accepted without complaint. Anything else is another oracle's
   question. *)
let accepted css =
  match Cascade.Css.of_string css with
  | Error _ -> None
  | Ok { warnings = _ :: _; _ } -> None
  | Ok { stylesheet; warnings = []; _ } -> Some stylesheet

let print ~minify sheet = Cascade.Css.to_string ~minify sheet

type finding = {
  property : string;
  value : string;
  step : string;
  got : string;
}

let unreadable : finding list ref = ref []
let unstable : finding list ref = ref []

let check ~property ~value =
  let css = String.concat "" [ "a{"; property; ":"; value; "}" ] in
  match accepted css with
  | None -> ()
  | Some sheet ->
      let minified = print ~minify:true sheet in
      let pretty = print ~minify:false sheet in
      let step name text =
        match accepted text with
        | None ->
            unreadable :=
              { property; value; step = name; got = text } :: !unreadable;
            None
        | Some again -> Some (print ~minify:true again)
      in
      (* Each printed form has to parse, and to say what the sheet said. *)
      List.iter
        (fun (name, text) ->
          match step name text with
          | None -> ()
          | Some back when String.equal back minified -> ()
          | Some back ->
              unstable :=
                { property; value; step = name; got = back } :: !unstable)
        [ ("minified", minified); ("pretty", pretty) ]

(* The manifest names every property the spec inventory tracks. The reader
   answers for more than that (measured by scripts/check_properties.ml: 455 of
   548), so the names it does not carry are outside this sweep. *)
let properties =
  List.sort_uniq String.compare
    (List.map
       (fun (r : Cascade_spec_inventory.Property_grammar.row) -> r.property)
       Cascade_spec_inventory.Property_grammar.rows)

let sample_per_property = 12

(* A run that reports nothing has to be a run that could have. These put a
   known-bad printer through the same check and require it to be caught, so a
   green sweep means the population was examined rather than that the check went
   quiet. *)
let calibrate () =
  let sheet =
    match accepted "a{color:red}" with
    | Some s -> s
    | None -> failwith "calibration: cascade cannot read a{color:red}"
  in
  let broken_unreadable = Cascade.Css.to_string ~minify:true sheet ^ "@@{" in
  (match accepted broken_unreadable with
  | None -> ()
  | Some _ ->
      failwith "calibration: an unreadable sheet read, so the check is blind");
  let says_something_else = "a{color:blue}" in
  match accepted says_something_else with
  | Some other
    when not
           (String.equal
              (Cascade.Css.to_string ~minify:true other)
              (Cascade.Css.to_string ~minify:true sheet)) ->
      ()
  | Some _ | None -> failwith "calibration: two different sheets compared equal"

let () =
  parse_args 1;
  calibrate ();
  match !one_css with
  | Some css ->
      (match accepted css with
      | None -> print_endline "cascade did not accept it"
      | Some sheet -> (
          let minified = print ~minify:true sheet in
          Fmt.pr "minified  %s@." minified;
          Fmt.pr "pretty    %s@."
            (String.concat " "
               (String.split_on_char '\n' (print ~minify:false sheet)));
          match accepted minified with
          | None -> print_endline "re-read    FAILED"
          | Some again -> Fmt.pr "re-read   %s@." (print ~minify:true again)));
      exit 0
  | None ->
      let started = Unix.gettimeofday () in
      let checked = ref 0 in
      List.iter
        (fun property ->
          let values =
            Cascade_spec_inventory.Value_gen.values_for ~seed:!seed property
          in
          let values =
            if !full then values
            else List.filteri (fun i _ -> i < sample_per_property) values
          in
          List.iter
            (fun value ->
              incr checked;
              check ~property ~value)
            values)
        properties;
      let elapsed = Unix.gettimeofday () -. started in
      Fmt.pr "readback: %d value(s) over %d propert(ies), seed %d, %.1fs@."
        !checked (List.length properties) !seed elapsed;
      let report name findings =
        if findings <> [] then (
          Fmt.pr "@.=== %s (%d) ===@." name (List.length findings);
          List.iter
            (fun f ->
              Fmt.pr "  %s: %s@.    after %s: %s@." f.property f.value f.step
                f.got)
            (List.filteri (fun i _ -> i < 20) findings))
      in
      report "UNREADABLE: cascade cannot read what it printed" !unreadable;
      report "UNSTABLE: the printed form says something else" !unstable;
      let total = List.length !unreadable + List.length !unstable in
      if total = 0 then
        print_endline
          "readback: calibrated, and everything cascade printed, it read back"
      else Fmt.pr "readback: %d finding(s)@." total;
      exit (if total = 0 then 0 else 1)
