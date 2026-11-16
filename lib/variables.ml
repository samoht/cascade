(** CSS variables and variable extraction utilities *)

open Values
open Properties
open Declaration
include Variables_intf
include Variables_syntax
include Variables_value
include Variables_text
include Variables_visual
include Variables_motion
include Variables_layout
include Variables_property

let compare_vars_by_name (V x) (V y) = String.compare x.name y.name
let any_var_name (V v) = String.concat "" [ "--"; v.name ]

let rec extract_vars_of_declaration : declaration -> any_var list = function
  | Declaration
      { property = Custom_property _; value = Custom_value { value; _ }; _ } ->
      vars_of_custom_property_value value
  | Declaration { property; value; _ } -> vars_of_property property value
  | Theme_guarded { decl; _ } -> extract_vars_of_declaration decl

(* Stable dedup: preserves first occurrence of each var, removes later
   duplicates. For the (very common) zero / one / two element cases skip the
   hashtable entirely - the work is cheaper than the allocation. *)
let stable_dedup_vars = function
  | [] -> []
  | [ _ ] as xs -> xs
  | [ (V v1 as a); (V v2 as b) ] ->
      if v1.name = v2.name then [ a ] else [ a; b ]
  | vars ->
      let seen = Hashtbl.create (List.length vars) in
      List.filter
        (fun (V v) ->
          if Hashtbl.mem seen v.name then false
          else (
            Hashtbl.add seen v.name ();
            true))
        vars

let vars_of_declarations properties =
  List.concat_map extract_vars_of_declaration properties |> stable_dedup_vars

(* Cheaper than [vars_of_declarations [d] <> []]: skip the dedup hashtable and
   short-circuit on the first matching var. *)
let declaration_uses_var d =
  match extract_vars_of_declaration d with [] -> false | _ -> true

(* Extract only custom property declarations (variable definitions) *)
let custom_declarations ?layer (decls : declaration list) : declaration list =
  List.filter
    (function
      | Declaration
          {
            property = Custom_property _;
            value = Custom_value { layer = decl_layer; _ };
            _;
          } -> (
          match layer with None -> true | Some l -> decl_layer = Some l)
      | _ -> false)
    decls

(* Extract the variable name from a custom declaration *)
let rec custom_declaration_name (decl : declaration) : string option =
  match decl with
  | Declaration { property = Custom_property name; _ } -> Some name
  | Theme_guarded { decl; _ } -> custom_declaration_name decl
  | _ -> None
