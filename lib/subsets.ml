(* The subsets of a bucket's members the candidate search tries. *)

let key ids =
  List.fold_left
    (fun hash id -> Common.mix_int hash (Rule_graph.Node_id.to_int id))
    0x345678 ids

(* The whole of a small bucket, the bucket less one member, then runs of
   neighbours from the widest down, until [max_candidates]. *)
let bounded_subsets ~large_bucket_candidates ~limit ids =
  let n = List.length ids in
  let seen = Hashtbl.create 32 in
  let max_candidates = if n > limit then large_bucket_candidates else 128 in
  let add acc ids =
    match ids with
    | [] | [ _ ] -> acc
    | _ ->
        let key = key ids in
        if Hashtbl.mem seen key then acc
        else begin
          Hashtbl.replace seen key ();
          ids :: acc
        end
  in
  let acc = if n <= limit then add [] ids else [] in
  let acc =
    if n <= 2 || n > limit then acc
    else
      List.fold_left
        (fun acc drop ->
          if List.length acc >= max_candidates then acc
          else
            add acc
              (List.filter
                 (fun id -> Rule_graph.Node_id.compare id drop <> 0)
                 ids))
        acc ids
  in
  let rec take n xs =
    if n <= 0 then []
    else match xs with [] -> [] | x :: xs -> x :: take (n - 1) xs
  in
  let rec drop n xs =
    if n <= 0 then xs else match xs with [] -> [] | _ :: xs -> drop (n - 1) xs
  in
  let window width start = ids |> drop start |> take width in
  let max_width = if n > limit then min 128 n else min limit n in
  let rec widths width acc count =
    if width < 2 || count >= max_candidates then acc
    else
      let rec starts start acc count =
        if start + width > n || count >= max_candidates then (acc, count)
        else starts (start + 1) (add acc (window width start)) (count + 1)
      in
      let acc, count = starts 0 acc count in
      widths (width - 1) acc count
  in
  widths max_width acc (List.length acc) |> List.rev

type indexed_budget = { occurrence_window : int; max_candidates : int }
type t = Exhaustive | Indexed of indexed_budget

let max_span = 128
let indexed_budget = { occurrence_window = 24; max_candidates = 256 }
let v ~aggressive = if aggressive then Exhaustive else Indexed indexed_budget

let shared = function
  | Exhaustive -> Option.None
  | Indexed { max_candidates; _ } -> Option.Some max_candidates

(* The windows of [rows] (in printed order) a bucket offers, each of at most
   [occurrence_window] neighbours, by first member and then by length, or none
   when there are more than [max_candidates]. *)
let indexed_windows budget rows =
  let len = Array.length rows in
  let window_end first = min (len - 1) (first + budget.occurrence_window - 1) in
  let rec count first acc =
    if first >= len then acc
    else count (first + 1) (acc + window_end first - first)
  in
  if count 0 0 > budget.max_candidates then []
  else begin
    let subsets = ref [] in
    for first = len - 2 downto 0 do
      for last = window_end first downto first + 1 do
        let rec loop index subset =
          if index < first then subset
          else loop (index - 1) (rows.(index) :: subset)
        in
        subsets := loop last [] :: !subsets
      done
    done;
    !subsets
  end

let of_bucket budget ids =
  match budget with
  | Exhaustive -> bounded_subsets ~limit:12 ~large_bucket_candidates:32 ids
  | Indexed indexed -> indexed_windows indexed (Array.of_list ids)
