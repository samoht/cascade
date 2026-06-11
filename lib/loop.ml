type action = {
  replacement : Stylesheet.rule list;
  consumed : Pool.node list;
  saving : int;
}

type score = Pool.node -> action option

module Queue =
  Psq.Make
    (struct
      type t = Pool.node

      let compare a b = Int.compare (Pool.id a) (Pool.id b)
    end)
    (struct
      type t = int * int

      let compare (s1, i1) (s2, i2) =
        let c = Int.compare s2 s1 in
        if c <> 0 then c else Int.compare i1 i2
    end)

type candidate = { anchor : Pool.node; action : action }

type t = {
  pool : Pool.t;
  score : score;
  frontier : Queue.t ref;
  candidates : (int, candidate) Hashtbl.t;
  applied : int ref;
}

let action ~replacement ~consumed ~saving = { replacement; consumed; saving }

let live candidate =
  let rec loop previous = function
    | [] -> true
    | node :: rest -> (
        match Pool.next previous with
        | Some next when Pool.is_live node && next == node -> loop node rest
        | _ -> false)
  in
  Pool.is_live candidate.anchor
  && loop candidate.anchor candidate.action.consumed

let forget candidates candidate =
  Hashtbl.remove candidates (Pool.id candidate.anchor);
  List.iter
    (fun node -> Hashtbl.remove candidates (Pool.id node))
    candidate.action.consumed

let enqueue t node =
  if Pool.is_live node then
    match t.score node with
    | Some action when action.saving > 0 ->
        let candidate = { anchor = node; action } in
        Hashtbl.replace t.candidates (Pool.id node) candidate;
        t.frontier := Queue.add node (action.saving, Pool.id node) !(t.frontier)
    | _ ->
        Hashtbl.remove t.candidates (Pool.id node);
        t.frontier := Queue.remove node !(t.frontier)

let apply t candidate =
  incr t.applied;
  let action = candidate.action in
  let added =
    List.map
      (fun rule -> Pool.insert_before t.pool candidate.anchor rule)
      action.replacement
  in
  forget t.candidates candidate;
  Pool.remove t.pool candidate.anchor;
  List.iter (Pool.remove t.pool) action.consumed;
  (added, action.saving)

let rescore t node =
  match t.score node with
  | Some action when action.saving > 0 ->
      let candidate = { anchor = node; action } in
      Hashtbl.replace t.candidates (Pool.id node) candidate;
      t.frontier := Queue.add node (action.saving, Pool.id node) !(t.frontier)
  | _ -> Hashtbl.remove t.candidates (Pool.id node)

let run ?(on_apply = fun _ -> ()) t =
  let rec loop () =
    match Queue.pop !(t.frontier) with
    | None -> ()
    | Some ((node, (stored, _)), rest) -> (
        t.frontier := rest;
        if not (Pool.is_live node) then (
          Hashtbl.remove t.candidates (Pool.id node);
          loop ())
        else
          match Hashtbl.find_opt t.candidates (Pool.id node) with
          | Some candidate
            when candidate.action.saving = stored && live candidate ->
              let added, saving = apply t candidate in
              on_apply saving;
              List.iter (enqueue t) added;
              loop ()
          | _ ->
              rescore t node;
              loop ())
  in
  loop ();
  !(t.applied)

let v pool score =
  let t =
    {
      pool;
      score;
      frontier = ref Queue.empty;
      candidates = Hashtbl.create (Pool.length pool);
      applied = ref 0;
    }
  in
  List.iter (enqueue t) (Pool.nodes pool);
  t
