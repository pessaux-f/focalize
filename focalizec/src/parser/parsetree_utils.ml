(* **************************************************** *)
(*  [Fun] string_of_vname : Parsetree.vname -> string   *)
(** [Descr] : Extracts the inner string of the [vname].

    [Rem] : Exported outside this module.               *)
(* **************************************************** *)
let string_of_vname = function
  | Parsetree.Vlident s | Parsetree.Vuident s | Parsetree.Vpident s
  | Parsetree.Viident s | Parsetree.Vqident s -> s
;;
