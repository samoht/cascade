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

(* [f] over the first [limit] windows of a bucket of [len] members, each of at
   most [occurrence_window] neighbours, by first member and then by length. *)
let fold_windows budget ~limit len f acc =
  let window_end first = min (len - 1) (first + budget.occurrence_window - 1) in
  let rec from first last n acc =
    if n >= limit || first >= len - 1 then acc
    else if last > window_end first then from (first + 1) (first + 2) n acc
    else from first (last + 1) (n + 1) (f acc first last)
  in
  from 0 1 0 acc

(* An indexed bucket keeps its members and lists its windows when it is taken:
   it is refreshed at every change to its members, and listing them then would
   build and keep up to [max_candidates] lists, taken or not. *)
type bucket =
  | Listed of Rule_graph.node_id list list
  | Windows of {
      budget : indexed_budget;
      rows : Rule_graph.node_id array;
      count : int;
    }

let bucket budget ids =
  match budget with
  | Exhaustive ->
      Listed (bounded_subsets ~limit:12 ~large_bucket_candidates:32 ids)
  | Indexed indexed ->
      let rows = Array.of_list ids in
      let windows =
        fold_windows indexed
          ~limit:(indexed.max_candidates + 1)
          (Array.length rows)
          (fun n _ _ -> n + 1)
          0
      in
      let count = if windows > indexed.max_candidates then 0 else windows in
      Windows { budget = indexed; rows; count }

let count = function
  | Listed subsets -> List.length subsets
  | Windows { count; _ } -> count

let rec take_list n = function
  | [] -> []
  | x :: rest -> if n <= 0 then [] else x :: take_list (n - 1) rest

let take bucket n =
  match bucket with
  | Listed subsets -> take_list n subsets
  | Windows { budget; rows; count } ->
      let rec window first index subset =
        if index < first then subset
        else window first (index - 1) (rows.(index) :: subset)
      in
      fold_windows budget ~limit:(Int.min n count) (Array.length rows)
        (fun acc first last -> window first last [] :: acc)
        []
      |> List.rev
