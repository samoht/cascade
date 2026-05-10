let read path =
  let ic = open_in_bin path in
  let pairs = ref [] in
  (try
     while true do
       let header = input_line ic in
       Scanf.sscanf header ">>> %d %d" (fun ilen elen ->
           let buf = Bytes.create (ilen + elen) in
           really_input ic buf 0 (ilen + elen);
           let input = Bytes.sub_string buf 0 ilen in
           let expected = Bytes.sub_string buf ilen elen in
           pairs := (input, expected) :: !pairs);
       ignore (input_char ic : char)
     done
   with End_of_file -> ());
  close_in ic;
  List.rev !pairs
