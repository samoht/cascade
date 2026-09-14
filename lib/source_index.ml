(** See {!Source_index}. *)

type block = { body : string; next : int }
(** A [{ ... }] or [( ... )] body, and the offset just past its close. *)

type header = { prelude : string; brace : int; block : block }
(** An [@name PRELUDE { ... }] header: what stands between the at-keyword and
    the [{], where that [{] is, and the block it opens. *)

type statement = { prelude : string; next : int }
(** A blockless at-rule's prelude and the offset after its semicolon. When a
    closing brace terminates it, [next] points at that brace. *)

type t = {
  call : (int, string * block) Hashtbl.t;
      (** function-token offset -> the name it calls and its arguments *)
  at : (int, string * header) Hashtbl.t;
      (** at-keyword offset -> its at-name and its header *)
  statement : (int, string * statement) Hashtbl.t;
      (** at-keyword offset -> its at-name and blockless statement *)
}

(* Where the contents of a group end. One the source left open ends with the
   source, which is where the parser ends it too. *)
let inner_end ~closed (loc : Loc.t) =
  if closed then loc.end_pos - 1 else loc.end_pos

let group css ~from ~upto ~next =
  { body = String.sub css from (upto - from); next }

let block_of css (b : Component.block Component.node) =
  let upto = inner_end ~closed:b.node.closed b.loc in
  group css ~from:(b.loc.start_pos + 1) ~upto ~next:b.loc.end_pos

(* A call's arguments start where its first one does; one that takes none has an
   empty body against its own closer. *)
let call_of css (f : Component.func Component.node) =
  let upto = inner_end ~closed:f.node.terminated f.loc in
  let from =
    match f.node.arguments with
    | argument :: _ -> (Component.source_loc argument).start_pos
    | [] -> upto
  in
  group css ~from ~upto ~next:f.loc.end_pos

(* The [{] an at-rule's prelude leads to. A group in the prelude is a single
   component, so a [;] inside one does not end the at-rule. *)
let rec brace_of = function
  | [] -> None
  | Component.Block ({ node = { opening = Token.Curly; _ }; _ } as b) :: _ ->
      Some b
  | Component.Preserved { kind = Token.Semicolon | Token.Close _; _ } :: _ ->
      None
  | _ :: rest -> brace_of rest

(* The semicolon or enclosing closer that terminates a blockless at-rule. *)
let rec statement_end ~closer = function
  | [] -> (closer, closer)
  | Component.Preserved { kind = Token.Semicolon; loc; _ } :: _ ->
      (loc.start_pos, loc.end_pos)
  | Component.Preserved { kind = Token.Close Token.Curly; loc; _ } :: _ ->
      (loc.start_pos, loc.start_pos)
  | _ :: rest -> statement_end ~closer rest

let v css =
  let t =
    {
      call = Hashtbl.create 16;
      at = Hashtbl.create 16;
      statement = Hashtbl.create 16;
    }
  in
  let header name (at : Loc.t) rest =
    match brace_of rest with
    | None -> ()
    | Some b ->
        let brace = b.Component.loc.start_pos in
        let prelude =
          String.trim (String.sub css at.end_pos (brace - at.end_pos))
        in
        Hashtbl.replace t.at at.start_pos
          (name, { prelude; brace; block = block_of css b })
  in
  let statement ~closer name (at : Loc.t) rest =
    let upto, next = statement_end ~closer rest in
    let prelude = String.sub css at.end_pos (upto - at.end_pos) in
    Hashtbl.replace t.statement at.start_pos (name, { prelude; next })
  in
  let rec walk ~closer = function
    | [] -> ()
    | item :: rest ->
        (match item with
        | Component.Preserved { kind = Token.At_keyword name; loc; _ } -> (
            (* An at-rule is one or the other. [brace_of] stops at the
               terminator, so finding no brace is exactly "blockless", and
               registering both would answer [at_statement] for a blockful rule
               with a prelude running to the end of the source. *)
            match brace_of rest with
            | Some _ -> header name loc rest
            | None -> statement ~closer name loc rest)
        | Component.Preserved _ -> ()
        | Component.Block b ->
            walk ~closer:(inner_end ~closed:b.node.closed b.loc) b.node.value
        | Component.Func f ->
            Hashtbl.replace t.call f.loc.start_pos (f.node.name, call_of css f);
            walk
              ~closer:(inner_end ~closed:f.node.terminated f.loc)
              f.node.arguments);
        walk ~closer rest
  in
  let parsed = Parser.list_of_component_values (Reader.of_string css) in
  walk ~closer:(String.length css) parsed.value;
  t

(* [name( ... )] starting at [i]. *)
let call t ~name i =
  match Hashtbl.find_opt t.call i with
  | Some (n, block) when n = name -> Some block
  | _ -> None

(* [@name PRELUDE { ... }] starting at [i]. An at-rule with no block of its own
   is not one of these. *)
let at_rule t ~name i =
  match Hashtbl.find_opt t.at i with
  | Some (n, header) when "@" ^ n = name -> Some header
  | _ -> None

(* A blockless [@name PRELUDE;] starting at [i]. *)
let at_statement t ~name i =
  match Hashtbl.find_opt t.statement i with
  | Some (n, statement) when "@" ^ n = name -> Some statement
  | _ -> None

(* Every [name( ... )] in the source, with the offset each starts at. *)
let calls t ~name =
  Hashtbl.fold
    (fun i (n, block) acc -> if n = name then (i, block) :: acc else acc)
    t.call []

(* Every blockless [@name PRELUDE;], the same way. *)
let at_statements t ~name =
  Hashtbl.fold
    (fun i (n, s) acc -> if "@" ^ n = name then (i, s) :: acc else acc)
    t.statement []
