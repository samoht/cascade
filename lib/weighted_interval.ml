type 'a interval = { start : int; stop : int; weight : int; value : 'a }
type 'a t = { by_stop : 'a interval list array }

let v ~length =
  if length < 0 then invalid_arg "Weighted_interval.v: negative length";
  { by_stop = Array.make length [] }

let length t = Array.length t.by_stop

let validate_range t ~start ~stop =
  if start < 0 then invalid_arg "Weighted_interval.add: negative start";
  if stop < start then invalid_arg "Weighted_interval.add: stop before start";
  if stop >= length t then
    invalid_arg "Weighted_interval.add: stop out of range"

let add t ~start ~stop ~weight value =
  validate_range t ~start ~stop;
  if weight > 0 then
    t.by_stop.(stop) <- { start; stop; weight; value } :: t.by_stop.(stop)

let select t =
  let len = length t in
  let best = Array.make (len + 1) 0 in
  let choice = Array.make (len + 1) None in
  for i = 1 to len do
    best.(i) <- best.(i - 1);
    List.iter
      (fun interval ->
        let value = best.(interval.start) + interval.weight in
        if value > best.(i) then (
          best.(i) <- value;
          choice.(i) <- Some interval))
      t.by_stop.(i - 1)
  done;
  let rec collect i acc =
    if i <= 0 then acc
    else
      match choice.(i) with
      | None -> collect (i - 1) acc
      | Some interval -> collect interval.start (interval :: acc)
  in
  (best.(len), collect len [])
