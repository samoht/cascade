(* Tags are drawn from [0, universe). An element out of the order has tag [-1],
   and [-1] in [prev] or [next] is the end of the order. *)

let bits = 61
let universe = 1 lsl bits
let none = -1

type t = {
  mutable tag : int array;
  mutable prev : int array;
  mutable next : int array;
  mutable first : int;
  mutable relabels : int;
}

let v n =
  let step = universe / (n + 1) in
  {
    tag = Array.init n (fun i -> (i + 1) * step);
    prev = Array.init n (fun i -> i - 1);
    next = Array.init n (fun i -> if i = n - 1 then none else i + 1);
    first = (if n = 0 then none else 0);
    relabels = 0;
  }

let mem t x = x < Array.length t.tag && t.tag.(x) >= 0
let compare t a b = Int.compare t.tag.(a) t.tag.(b)
let option x = if x = none then Option.None else Option.Some x
let first t = option t.first
let next t x = option t.next.(x)
let prev t x = option t.prev.(x)
let relabels t = t.relabels

let exists_between t a b f =
  let rec loop x = x <> none && x <> b && (f x || loop t.next.(x)) in
  loop t.next.(a)

let reserve t x =
  let room = Array.length t.tag in
  if x >= room then begin
    let size = max (x + 1) (2 * room) in
    let grow arr =
      let grown = Array.make size none in
      Array.blit arr 0 grown 0 room;
      grown
    in
    t.tag <- grow t.tag;
    t.prev <- grow t.prev;
    t.next <- grow t.next
  end

(* Thread [x] between [a] and [b], neighbours or ends of the order. *)
let link t a x b =
  t.prev.(x) <- a;
  t.next.(x) <- b;
  if a = none then t.first <- x else t.next.(a) <- x;
  if b <> none then t.prev.(b) <- x

(* A range of [2^i] tags may hold at most [(4/3)^i] elements, the density
   threshold [T^-i] of Bender et al. with [T = 3/2]: with 61 bits the whole
   universe holds 40 million elements. *)
let fits count i = Float.of_int count <= Float.pow (4. /. 3.) (Float.of_int i)

(* Give [x], just linked next to [anchor], a tag: find the smallest aligned
   range around [anchor]'s tag that, counting [x], is sparse enough, and spread
   its elements evenly over it. [x] counts as standing at [anchor]'s tag. *)
let relabel t ~anchor x =
  let base = t.tag.(anchor) in
  let left = ref (if t.next.(x) = anchor then x else anchor) in
  let right = ref (if t.prev.(x) = anchor then x else anchor) in
  let count = ref 2 in
  let rec widen i =
    let size = 1 lsl i in
    let lo = base land lnot (size - 1) in
    let hi = lo + size in
    while t.prev.(!left) <> none && t.tag.(t.prev.(!left)) >= lo do
      left := t.prev.(!left);
      incr count
    done;
    while t.next.(!right) <> none && t.tag.(t.next.(!right)) < hi do
      right := t.next.(!right);
      incr count
    done;
    if i >= bits || fits !count i then begin
      let step = size / !count in
      let rec spread e j =
        t.tag.(e) <- lo + (j * step);
        if e <> !right then spread t.next.(e) (j + 1)
      in
      spread !left 0;
      t.relabels <- t.relabels + !count
    end
    else widen (i + 1)
  in
  widen 1

let insert t a x b =
  reserve t x;
  link t a x b;
  let lo = if a = none then -1 else t.tag.(a) in
  let hi = if b = none then universe else t.tag.(b) in
  if hi - lo >= 2 then t.tag.(x) <- lo + ((hi - lo) / 2)
  else relabel t ~anchor:(if a = none then b else a) x

let insert_after t a x = insert t a x t.next.(a)
let insert_before t b x = insert t t.prev.(b) x b
let insert_first t x = insert t none x t.first

let remove t x =
  let a = t.prev.(x) and b = t.next.(x) in
  if a = none then t.first <- b else t.next.(a) <- b;
  if b <> none then t.prev.(b) <- a;
  t.tag.(x) <- none;
  t.prev.(x) <- none;
  t.next.(x) <- none
