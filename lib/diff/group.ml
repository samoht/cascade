(** Grouping a list into a hash table of buckets. *)

(* Buckets are built by prepending and reversed once at the end, so grouping
   stays linear while each bucket still reads in the order the list did. [size]
   is the table's initial capacity and nothing else: it decides which bucket a
   key lands in, so a caller that needs a defined order over the keys takes it
   from the list it grouped rather than from [Hashtbl.iter]. *)
let by ?(size = 16) key items =
  let tbl = Hashtbl.create size in
  List.iter
    (fun item ->
      let k, v = key item in
      let bucket = Option.value (Hashtbl.find_opt tbl k) ~default:[] in
      Hashtbl.replace tbl k (v :: bucket))
    items;
  Hashtbl.to_seq_keys tbl |> List.of_seq
  |> List.iter (fun k -> Hashtbl.replace tbl k (List.rev (Hashtbl.find tbl k)));
  tbl
