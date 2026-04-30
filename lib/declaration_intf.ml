(** Declaration interface types *)

open Properties_intf

type 'a kind = 'a Properties_intf.kind

type declaration =
  | Declaration : {
      property : 'a property;
      value : 'a;
      important : bool;
    }
      -> declaration
  | Theme_guarded : { var_name : string; decl : declaration } -> declaration

type t = declaration
