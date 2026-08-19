type item = { decl : Declaration.t; mutable rows : int list }
type table = (int, item list) Hashtbl.t
type seen = (int, Declaration.t list) Hashtbl.t
type t = { same : Declaration.t -> Declaration.t -> bool; table : table }

let bucket table decl =
  Hashtbl.find_opt table (Declaration.hash decl) |> Option.value ~default:[]

let add table ~same row decl =
  let hash = Declaration.hash decl in
  let rec loop acc = function
    | [] ->
        Hashtbl.replace table hash (List.rev ({ decl; rows = [ row ] } :: acc))
    | item :: rest ->
        if same item.decl decl then item.rows <- row :: item.rows
        else loop (item :: acc) rest
  in
  loop [] (bucket table decl)

let seen (seen : seen) ~same decl =
  let bucket =
    Hashtbl.find_opt seen (Declaration.hash decl) |> Option.value ~default:[]
  in
  List.exists (fun d -> same d decl) bucket

let remember seen decl =
  let hash = Declaration.hash decl in
  let bucket = Hashtbl.find_opt seen hash |> Option.value ~default:[] in
  Hashtbl.replace seen hash (decl :: bucket)

let default_same a b =
  a == b
  || Declaration.hash a = Declaration.hash b
     && Declaration.equal_declaration a b

let v ?(same = default_same) ?(keep = fun (_ : Stylesheet.rule) -> true)
    (rules : Stylesheet.rule array) =
  let table = Hashtbl.create 1024 in
  Array.iteri
    (fun row rule ->
      if keep rule then begin
        let seen_in_rule = Hashtbl.create 8 in
        List.iter
          (fun decl ->
            if not (seen seen_in_rule ~same decl) then begin
              remember seen_in_rule decl;
              add table ~same row decl
            end)
          rule.Stylesheet_intf.declarations
      end)
    rules;
  { same; table }

let sort_rows xs =
  let xs = Array.of_list xs in
  Array.sort compare xs;
  let len = Array.length xs in
  if len < 2 then xs
  else
    let next = ref 1 in
    for i = 1 to len - 1 do
      if xs.(i) <> xs.(!next - 1) then begin
        xs.(!next) <- xs.(i);
        incr next
      end
    done;
    Array.sub xs 0 !next

let rows t decl =
  match
    List.find_opt (fun item -> t.same item.decl decl) (bucket t.table decl)
  with
  | None -> [||]
  | Some item -> sort_rows item.rows

let iter t f =
  Hashtbl.iter
    (fun _ items ->
      List.iter (fun item -> f item.decl (sort_rows item.rows)) items)
    t.table
