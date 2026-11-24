(* A graph node's reachable set, one bit per node, for {!Rule_graph}. The union
   below runs once per edge of the cone being closed, so it goes a word at a
   time. *)

(* Every set of one graph has the same width, the one its table of sets has room
   for, so two of them union word for word. *)
let capacity count = (max count 1 + 63) / 64 * 64
let table count = Array.make (capacity count) Bytes.empty
let v count = Bytes.make ((count + 7) / 8) '\000'
let row sets = Bytes.make (Array.length sets / 8) '\000'
let is_set set = Bytes.length set > 0

(* [sets] with room for [count] nodes. A rewrite appends a few nodes at a time,
   so the room grows by a quarter and the settled sets are widened once per
   growth rather than once per rewrite. *)
let grow sets count =
  let room = Array.length sets in
  if count <= room then sets
  else begin
    let grown = table (max count (room + (room / 4) + 64)) in
    let width = Array.length grown / 8 in
    Array.iteri
      (fun i set ->
        if is_set set then begin
          let wide = Bytes.make width '\000' in
          Bytes.blit set 0 wide 0 (Bytes.length set);
          grown.(i) <- wide
        end)
      sets;
    grown
  end

let mem set i = Char.code (Bytes.get set (i lsr 3)) land (1 lsl (i land 7)) <> 0

let add set i =
  let byte = i lsr 3 in
  Bytes.set set byte
    (Char.chr (Char.code (Bytes.get set byte) lor (1 lsl (i land 7))))

(* The word loops below run once per edge of every cone closed and once per node
   a commit reaches, and a checked access costs a bounds test per word without
   flambda. [width] proves every word of both sets in range once, so the loops
   read and write unchecked. *)
external get64 : Bytes.t -> int -> int64 = "%caml_bytes_get64u"
external set64 : Bytes.t -> int -> int64 -> unit = "%caml_bytes_set64u"

let width a b =
  let len = Bytes.length a in
  if Bytes.length b <> len then invalid_arg "Reach: unequal widths";
  len

(* Whether [a] and [b], of one width, share a node. *)
let meets a b =
  let len = width a b in
  let words = len / 8 in
  let rec word w =
    w < words
    && ((not
           (Int64.equal 0L (Int64.logand (get64 a (w * 8)) (get64 b (w * 8)))))
       || word (w + 1))
  in
  let rec byte at =
    at < len
    && (Char.code (Bytes.get a at) land Char.code (Bytes.get b at) <> 0
       || byte (at + 1))
  in
  word 0 || byte (words * 8)

(* The nodes of [a] that [b], of one width, lacks; [None] when there are more of
   them than words in a set, past which a union is cheaper than adding them one
   at a time. *)
let missing a b =
  let len = width a b in
  let words = len / 8 in
  let found = ref 0 in
  let rec bits byte at bit acc =
    if bit = 8 then acc
    else if byte land (1 lsl bit) = 0 then bits byte at (bit + 1) acc
    else begin
      incr found;
      bits byte at (bit + 1) (((at * 8) + bit) :: acc)
    end
  in
  let rec bytes at stop acc =
    if at = stop then acc
    else
      let byte =
        Char.code (Bytes.get a at) land lnot (Char.code (Bytes.get b at))
      in
      bytes (at + 1) stop (if byte = 0 then acc else bits byte at 0 acc)
  in
  let rec word w acc =
    if !found > words then None
    else if w = words then Some (bytes (words * 8) len acc)
    else if
      Int64.equal 0L
        (Int64.logand (get64 a (w * 8)) (Int64.lognot (get64 b (w * 8))))
    then word (w + 1) acc
    else word (w + 1) (bytes (w * 8) ((w + 1) * 8) acc)
  in
  word 0 []

let union ~into set =
  let len = width into set in
  let words = len / 8 in
  for w = 0 to words - 1 do
    let at = w * 8 in
    set64 into at (Int64.logor (get64 into at) (get64 set at))
  done;
  for at = words * 8 to len - 1 do
    Bytes.set into at
      (Char.chr
         (Char.code (Bytes.get into at) lor Char.code (Bytes.get set at)))
  done
