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

(* $Id: externals_ml_generation.ml,v 1.1 2007-10-09 08:37:35 pessaux Exp $ *)


(* ************************************************************************ *)
(** {b Descr} : Exception raised when an external value definition does not
              not provides any correspondance with OCaml.

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
exception No_external_value_caml_def of (Parsetree.vname * Location.t) ;;



(* ************************************************************************ *)
(** {b Descr} : Exception raised when an external type definition does not
              not provides any correspondance with OCaml.

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
exception No_external_type_caml_def of (Parsetree.vname * Location.t) ;;



(* ************************************************************************* *)
(* Misc_ml_generation.reduced_compil_context ->                              *)
(*   Parsetree.external_type_def_body -> unit                                *)
(** {b Descr} : Generates the OCaml code to bind a FoCaL type onto an OCaml
        existing type. If the FoCaL type name is the same than the OCaml
        one, then we silently ignore the type definition to avoid OCaml type
        definitions like [type int = int] that would lead to a cyclic type
        abbreviation and would be rejected by OCaml.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let external_type_def_compile ctx external_type_def =
  let external_type_def_desc = external_type_def.Parsetree.ast_desc in
  let out_fmter = ctx.Misc_ml_generation.rcc_out_fmter in
  (* Type definition header. *)
  Format.fprintf out_fmter "@[<2>type" ;
  (* Now, generate the type parameters if some. *)
  (match external_type_def_desc.Parsetree.etd_params with
   | [] -> ()
   | [one] ->
       (* Do not put parentheses because only one parameter. *)
       Format.fprintf out_fmter " %a" Misc_ml_generation.pp_to_ocaml_vname one
   | several ->
       (begin
       (* Enclose by parentheses and separate by commas. *)
       let rec rec_print_params = function
	 | [] -> ()
	 | [last] -> Format.fprintf out_fmter "%a"
	       Misc_ml_generation.pp_to_ocaml_vname last
	 | first :: rem ->
	     Format.fprintf out_fmter "%a,@ "
	       Misc_ml_generation.pp_to_ocaml_vname first ;
	     rec_print_params rem in
       Format.fprintf out_fmter " (@[<1>" ;
       rec_print_params several ;
       Format.fprintf out_fmter "@])"
       end)) ;
  (* Now, the type name, renamed as "_focty_" followed by the original name. *)
  Format.fprintf out_fmter " _focty_%a =@ "
    Misc_ml_generation.pp_to_ocaml_vname
    external_type_def_desc.Parsetree.etd_name ;
  (* And now, bind the FoCaL identifier to the OCaml one. *)
  try
    let (_, ocaml_binding) =
      List.find
	(function
	  | (Parsetree.EL_Caml, _) -> true
	  | (Parsetree.EL_Coq, _)
	  | ((Parsetree.EL_external _), _) -> false)
	external_type_def_desc.Parsetree.etd_body.Parsetree.ast_desc  in
    Format.fprintf out_fmter "%s@]@ ;;@\n" ocaml_binding
  with Not_found ->
    (* We didn't find any correspondance for OCaml. *)
    raise
      (No_external_type_caml_def
	 (external_type_def_desc.Parsetree.etd_name,
	  external_type_def.Parsetree.ast_loc))
;;



(* ********************************************************************** *)
(* Misc_ml_generation.reduced_compil_context ->                           *)
(*   Parsetree.external_value_def_body -> unit                            *)
(** {b Descr} : Generate the OCaml source code for a FoCaL external value
              definition.

    {b Args} :
      - [ctx] : The structure recording the various utilities information
              for the code generation of the external definition.
      - [external_value_def] : The external type definition to compile
           to OCaml source code.

    {b Rem} : Not exported outside this module.                           *)
(* ********************************************************************** *)
let external_value_def_compile ctx external_value_def =
  let external_value_def_desc = external_value_def.Parsetree.ast_desc in
  let out_fmter = ctx.Misc_ml_generation.rcc_out_fmter in
  (* Prints the name of the defined identifier *)
  (* and prepares for its type constraint.     *)
  Format.fprintf out_fmter "@[<2>let (%a : "
    Misc_ml_generation.pp_to_ocaml_vname
    external_value_def_desc.Parsetree.evd_name ;
  let extern_val_type =
    (match external_value_def_desc.Parsetree.evd_type.Parsetree.ast_type with
     | None ->
	 (* This means that during the typechecking pass, we forgot to record *)
	 (* the infered type inside the AST node. If arises, then fix it !    *)
	 assert false
     | Some ty -> ty) in
  (* Prints the defined identifier's type constraint. Because external *)
  (* definitions are always at toplevel, no species parameter is in the *)
  (* scope, hence the collections carrier mapping is trivially empty.   *)
  Format.fprintf out_fmter "%a) =@ "
    (Types.pp_type_simple_to_ml
       ~current_unit: ctx.Misc_ml_generation.rcc_current_unit
       ~reuse_mapping: false []) extern_val_type ;
  (* And now, bind the FoCaL identifier to the OCaml one. *)
  try
    let (_, ocaml_binding) =
      List.find
	(function
	  | (Parsetree.EL_Caml, _) -> true
	  | (Parsetree.EL_Coq, _)
	  | ((Parsetree.EL_external _), _) -> false)
	external_value_def_desc.Parsetree.evd_body.Parsetree.ast_desc  in
    Format.fprintf out_fmter "%s@]@ ;;@\n" ocaml_binding
  with Not_found ->
    (* We didn't find any correspondance for OCaml. *)
    raise
      (No_external_value_caml_def
	 (external_value_def_desc.Parsetree.evd_name,
	  external_value_def.Parsetree.ast_loc))
;;



let external_def_compile ctx extern_def =
  match extern_def.Parsetree.ast_desc with
   | Parsetree.ED_type external_type_def ->
       external_type_def_compile ctx external_type_def
   | Parsetree.ED_value external_value_def ->
       external_value_def_compile ctx external_value_def
;;
