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


(* $Id: core_ml_generation.ml,v 1.4 2007-09-18 10:29:38 pessaux Exp $ *)


type species_compil_context = {
  (** The name of the currently analysed compilation unit. *)
  current_unit : Types.fname ;
  (** The name of the current species. *)
  current_species : Types.species_name ;
  (* The current correspondance between collection types and type variable names
     representing the carrier of a species type in the OCaml code. *)
  collections_carrier_mapping : (Types.type_collection * string) list ;
  (** The current output formatter where to send the generated code. *)
  out_fmter : Format.formatter
} ;;



(* *********************************************************************** *)
(* current_unit: Types.fname -> Env.TypeInformation.species_description -> *)
(*   (Types.type_collection * string) list                                 *)
(** {b Descr} : Create the correspondance between the collection type of
              the species definition parameters and the type variables
              names to be used later during the OCaml translation.

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let build_collections_carrier_mapping ~current_unit species_descr =
    List.map
      (function
	| Env.TypeInformation.SPAR_is (n, _) ->
	    let n_as_string = Parsetree_utils.name_of_vname n in
	    (* Build the name of the type variable that will represent *)
	    (* this parameter's carrier type seen from OCaml. Just     *)
            (* lowerize the parameter name because collection names    *)
            (* are always syntactically capitalized and OCaml type     *)
            (* variable are always syntactically lowercase.            *)
	    let carrier_type_variable_name =
	      "'" ^ (String.lowercase n_as_string) ^ "_as_carrier" in
	    (* Now, build the "collection type" this name will be bound to. *)
	    (* According to how the "collection type" of parameters are     *)
	    (* built, this will be the couple of the current compilation    *)
            (* unit and the name of the parameter.                          *)
	    let type_coll = (current_unit, n_as_string) in
	    (* And now create the binding... *)
	    (type_coll, carrier_type_variable_name)
	| Env.TypeInformation.SPAR_in (n, type_coll) ->
	    (* Build the name of the type variable that will represent *)
	    (* this parameter's carrier type seen from OCaml. Same     *)
            (* remark than above for lowercase/uppercase.              *)
	    let carrier_type_variable_name =
	      "' " ^ (String.lowercase (Parsetree_utils.name_of_vname n)) ^
	      "_as_carrier" in
	    (type_coll, carrier_type_variable_name))
      species_descr.Env.TypeInformation.spe_sig_params
;;



(* ************************************************************************* *)
(* species_compil_context -> Env.TypeInformation.species_field list -> unit  *)
(** {b Descr} : Checks if "rep" is defined. If so, then generate the type
              constraint reflecting its effective structure. We loosely
              iterate on the list of fields, stopping at the first occurence
              of "rep" because, by construction if "rep" is present then it
              is only once. Hence it is safe to ignore the remaining fields.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let generate_rep_constraint_in_record_type ctx fields =
  let rec rec_search = function
    | [] -> ()
    | h :: q ->
	(begin
	match h with
	 | Env.TypeInformation.SF_sig (n, sch) ->
	     (* Check if the si is "rep". *)
	     if (Parsetree_utils.name_of_vname n) = "rep" then
	       (begin
	       let ty = Types.specialize sch in
	       Format.fprintf ctx.out_fmter
		 "@[<2>type me_as_carrier =@ %a@]@\n"
		 (Types.pp_type_simple_to_ml
		    ctx.collections_carrier_mapping) ty
	       end)
	     else rec_search q
	 | _ -> rec_search q
	end) in
  rec_search fields
;;



let generate_record_type ctx species_def species_descr =
  let out_fmter = ctx.out_fmter in
  let collections_carrier_mapping = ctx.collections_carrier_mapping in
  (* First, check if "rep" is defined. If so, then generate  *)
  (* the type constraint reflecting its effective structure. *)
  generate_rep_constraint_in_record_type
    ctx species_descr.Env.TypeInformation.spe_sig_methods ;
  let field_prefix =
    String.lowercase species_def.Parsetree.ast_desc.Parsetree.sd_name in
  (* The header of the OCaml type definition for the species record. *)
  Format.fprintf out_fmter "@[<2>type " ;
  (* Process parameters and "self" type variables names. *)
  if collections_carrier_mapping = [] then
    Format.fprintf out_fmter "'me_as_carrier "
  else
    (begin
    (* If there are several parameters, then enclose them by parentheses. *)
    Format.fprintf out_fmter "(@[<1>" ;
    List.iter
      (fun (_, type_variable_name) ->
	Format.fprintf out_fmter "%s,@ " type_variable_name)
      collections_carrier_mapping ;
    Format.fprintf out_fmter "'me_as_carrier)@] "
    end) ;
  (* The name of the type. *)
  Format.fprintf out_fmter "me_as_species = {@\n" ;
  (* The record's fields types. *)
  List.iter
    (function
      | Env.TypeInformation.SF_sig (n, sch)
      | Env.TypeInformation.SF_let (n, sch, _)
      | Env.TypeInformation.SF_theorem  (n, sch, _, _)
      | Env.TypeInformation.SF_property (n, sch, _) ->
	  (begin
	  (* Skip "rep", because it is a bit different and processed above *)
	  (* c.f. function [generate_rep_constraint_in_record_type].       *)
	  if (Parsetree_utils.name_of_vname n) <> "rep" then
	    (begin
	    let ty = Types.specialize sch in
	    Format.fprintf out_fmter "%s_%a :@ %a ;@\n"
	      field_prefix Sourcify.pp_vname n
	      (Types.pp_type_simple_to_ml collections_carrier_mapping) ty
	    end)
	  end)
      | Env.TypeInformation.SF_let_rec l ->
	  List.iter
	    (fun (n, sch, _) ->
	      let ty = Types.specialize sch in
	      Format.fprintf out_fmter "%s_%a :@ %a ;@\n"
		field_prefix Sourcify.pp_vname n
		(Types.pp_type_simple_to_ml collections_carrier_mapping) ty)
	    l)
    species_descr.Env.TypeInformation.spe_sig_methods ;
  Format.fprintf out_fmter "@]}@\n"
;;



let species_compile ~current_unit out_fmter species_def species_descr
    dep_graph =
  let species_def_desc = species_def.Parsetree.ast_desc in
  (* Start the module encapsulating the species representation. *)
  Format.fprintf out_fmter "@[<2>module %s =@\nstruct@\n"
    species_def_desc.Parsetree.sd_name ;
  (* Now, establish the mapping between collections available *)
  (* and the type variable names representing their carrier.  *)
  let collections_carrier_mapping =
    build_collections_carrier_mapping ~current_unit species_descr in
  (* The record type representing the species' type. *)
  let ctx = {
    current_unit = current_unit ;
    current_species = species_def_desc.Parsetree.sd_name ;
    collections_carrier_mapping = collections_carrier_mapping ;
    out_fmter = out_fmter } in
  generate_record_type ctx species_def species_descr ;
  Format.fprintf out_fmter "@\nend ;;@]@\n@."
;;



let toplevel_compile ~current_unit out_fmter global_env = function
  | Infer.PCM_no_matter -> ()
  | Infer.PCM_external -> Format.eprintf "Infer.PCM_external expr : TODO@."
  | Infer.PCM_species (species_def, species_descr, dep_graph) ->
      species_compile
	~current_unit out_fmter species_def species_descr dep_graph
  | Infer.PCM_collection (coll_def, fields) ->
      Format.eprintf "Infer.PCM_collection expr : TODO@."
  | Infer.PCM_type -> Format.eprintf "Infer.PCM_type expr : TODO@."
  | Infer.PCM_let_def let_def ->
      Format.eprintf "Infer.PCM_let_def expr : TODO@."
  | Infer.PCM_theorem theorem_def ->
      Format.eprintf "Infer.PCM_theorem expr : TODO@."
  | Infer.PCM_expr expr -> Format.eprintf "Infer.PCM_expr expr : TODO@."
;;



let root_compile ~current_unit ~out_file_name global_env stuff =
  let out_hd = open_out_bin out_file_name in
  let out_fmter = Format.formatter_of_out_channel out_hd in
  try
    List.iter (toplevel_compile ~current_unit out_fmter global_env) stuff ;
    close_out out_hd
  with whatever ->
    (* In any error case, close the outfile. *)
    close_out out_hd ;
    (begin
    try
      (* And rename it to prevent an incorrecty OCaml source file   *)
      (* from remaining, but to still keep a trace of what could be *)
      (* generated until the error arose.                           *)
      let trace_filename = out_file_name ^ ".mangled" in
      (* If the file of trace already exists, then first *)
      (* discard it to prevent OS file I/O errors.       *)
      if Sys.file_exists trace_filename then Sys.remove trace_filename ;
      Sys.rename out_file_name trace_filename ;
    with second_error ->
      (begin
      (* Here we want to catch errors that can arise during the trace file  *)
      (* stuff. Because we don't want these errors to hide the real initial *)
      (* problem that made the code generation impossible, we first process *)
      (* here I/O errors, then will be raise again the initial error.       *)
      Format.eprintf "Error@ while@ trying@ to@ keep@ trace@ of@ the@ partially@ generated@ OCaml@ code:@ %s.@\nInitial error follows.@."
	(Printexc.to_string second_error)
      end)
    end) ;
    (* Re-reaise the initial error. *)
    raise whatever
;;
