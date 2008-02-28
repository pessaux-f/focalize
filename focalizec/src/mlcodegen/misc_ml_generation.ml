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

(* $Id: misc_ml_generation.ml,v 1.17 2008-02-28 17:36:46 pessaux Exp $ *)



let pp_to_ocaml_label_ident ppf lab_ident =
  match lab_ident.Parsetree.ast_desc with
   | Parsetree.LI qual_name ->
       let vname =
         (match qual_name with
          | Parsetree.Vname n -> n
          | Parsetree.Qualified (modname, n) ->
              Format.fprintf ppf "%s." (String.capitalize modname) ;
              n) in
       Format.fprintf ppf "%a"
         Parsetree_utils.pp_vname_with_operators_expanded vname
;;



(* ********************************************************************* *)
(** {b Descr} : Data structure to record the various stuff needed to
          generate the OCaml code for various constructs. Passing this
          structure prevents from recursively passing a bunch of
          parameters to the functions. Instead, one pass only one and
          functions use the fields they need. This is mostly to preserve
          the stack and to make the code more readable. In fact,
          information recorded in this structure is semantically pretty
          uninteresting to understand the compilation process: it is more
          utilities.

    {b Rem} Exported outside this module.                                *)
(* ********************************************************************* *)
type reduced_compil_context = {
  (** The name of the currently analysed compilation unit. *)
  rcc_current_unit : Types.fname ;
  (** The list of the current species species parameters if we are in the
      scope of a species and if it has some parameters. *)
  rcc_species_parameters_names : Parsetree.vname list ;
  (** The current correspondance between collection types and type variable
      names representing the carrier of a species type in the OCaml code. *)
  rcc_collections_carrier_mapping : Types.collection_carrier_mapping ;
  (** The current correspondance between method names of Self and their
      extra parameters they must be applied to because of the lambda-lifting
      process. This info is used when generating the OCaml code of a
      method, hence it is only relevant in case of recursive methods to know
      in their own body what they must be applied to in addition to their
      explicit arguments (those given by the FoCaL programmer). *)
  rcc_lambda_lift_params_mapping :
   (Parsetree.vname * ((string * Types.type_simple) list)) list ;
  (** The current output formatter where to send the generated code. *)
  rcc_out_fmter : Format.formatter
} ;;
