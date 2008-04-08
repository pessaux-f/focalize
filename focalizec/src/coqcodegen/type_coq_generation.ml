(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: type_coq_generation.ml,v 1.1 2008-04-08 15:10:55 pessaux Exp $ *)

let type_def_compile ctx env type_def_name type_descr =
  let out_fmter = ctx.Context.rcc_out_fmter in
  (* Build the print context for the methods once for all. *)
  let print_ctx = {
    Types.cpc_current_unit = ctx.Context.rcc_current_unit ;
    Types.cpc_current_species = None ;
    Types.cpc_collections_carrier_mapping =
      ctx.Context.rcc_collections_carrier_mapping } in
  (* Get a fresh instance of the type's identity scheme directly   *)
  (* instanciated with the variables that will serve as parameters *)
  (* of the definition. We keep the list of these variables to be  *)
  (* able to print them in front of the type constructor in the    *)
  (* OCaml definition.                                             *)
  let type_def_params =
    List.map
      (fun _ -> Types.type_variable ())
      type_descr.Env.TypeInformation.type_params in
  let instanciated_body =
    Types.specialize_with_args
      type_descr.Env.TypeInformation.type_identity type_def_params in
  (* Useless, but just in case.... This does not hurt ! *)
  Types.purge_type_simple_to_ml_variable_mapping () ;
  (* Now, generates the type definition's body. *)
  match type_descr.Env.TypeInformation.type_kind with
   | Env.TypeInformation.TK_abstract ->
       (* Since types are toplevel, the way "Self" is printed is non *)
       (* relevant. Indeed, "Self" can only appear inside the scope  *)
       (* of a species, hence never at toplevel, hence never in a    *)
       (* type definition.                                           *)
       Format.fprintf out_fmter "@[<2>Definition %a__t :=@ %a.@\n@]"
         Parsetree_utils.pp_vname_with_operators_expanded type_def_name
         (Types.pp_type_simple_to_coq
            print_ctx ~reuse_mapping: true ~self_as: Types.CSR_self)
         instanciated_body ;
       (* Not an external type definition, so nothing new in the environment. *)
       env
   | _ ->
       (* [Unsure] *)
       Format.eprintf "Types in coq todo !!!!!!!!!!!!!!@." ;
       env
;;
