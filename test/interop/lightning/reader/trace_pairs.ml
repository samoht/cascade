type candidate = { tool : string; css : string }
type failed_candidate = { tool : string; command : string; reason : string }

type t = {
  input : string;
  candidates : candidate list;
  failures : failed_candidate list;
}

let starts_with ~prefix s =
  let plen = String.length prefix in
  String.length s >= plen && String.sub s 0 plen = prefix

let read_exact ic len =
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  Bytes.unsafe_to_string buf

let consume_separator ?(allow_eof = false) ic =
  match input_char ic with
  | '\n' -> ()
  | c -> Fmt.failwith "unexpected trace separator: %C" c
  | exception End_of_file when allow_eof -> ()
  | exception End_of_file -> failwith "unexpected EOF in trace record"

let read_ok ?allow_eof ic header =
  Scanf.sscanf header "OK %d %d" (fun tlen clen ->
      let tool = read_exact ic tlen in
      let css = read_exact ic clen in
      consume_separator ?allow_eof ic;
      { tool; css })

let read_fail ?allow_eof ic header =
  Scanf.sscanf header "FAIL %d %d %d" (fun tlen clen rlen ->
      let tool = read_exact ic tlen in
      let command = read_exact ic clen in
      let reason = read_exact ic rlen in
      consume_separator ?allow_eof ic;
      { tool; command; reason })

let read_n n f =
  let rec loop n acc =
    if n = 0 then List.rev acc else loop (n - 1) (f n :: acc)
  in
  loop n []

let read_record ic ilen ok_count fail_count =
  let input = read_exact ic ilen in
  consume_separator ~allow_eof:(ok_count = 0 && fail_count = 0) ic;
  let candidates =
    read_n ok_count (fun remaining ->
        let header = input_line ic in
        if starts_with ~prefix:"OK " header then
          read_ok ~allow_eof:(remaining = 1 && fail_count = 0) ic header
        else failwith ("expected OK trace record, got: " ^ header))
  in
  let failures =
    read_n fail_count (fun remaining ->
        let header = input_line ic in
        if starts_with ~prefix:"FAIL " header then
          read_fail ~allow_eof:(remaining = 1) ic header
        else failwith ("expected FAIL trace record, got: " ^ header))
  in
  { input; candidates; failures }

let read path =
  let ic = open_in_bin path in
  let pairs = ref [] in
  (try
     while true do
       let header = input_line ic in
       if starts_with ~prefix:">>> " header then
         Scanf.sscanf header ">>> %d %d %d" (fun ilen ok_count fail_count ->
             pairs := read_record ic ilen ok_count fail_count :: !pairs)
       else failwith ("expected trace record, got: " ^ header)
     done
   with End_of_file -> ());
  close_in ic;
  List.rev !pairs

let pp_candidate ppf { tool; css } = Fmt.pf ppf "@ %s -> %S" tool css

let pp_failed ppf { tool; command; reason } =
  Fmt.pf ppf "@ %s failed (%s): %s" tool command reason

let pp ppf { input; candidates; failures } =
  Fmt.pf ppf "@[<v 2>%S%a%a@]" input
    (Fmt.list ~sep:Fmt.nop pp_candidate)
    candidates
    (Fmt.list ~sep:Fmt.nop pp_failed)
    failures
