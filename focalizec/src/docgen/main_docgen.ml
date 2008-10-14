(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Yvan Noyer                                               *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*            François Pessaux                                         *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: main_docgen.ml,v 1.1 2008-10-14 11:36:19 pessaux Exp $ *)


let gendoc_species_field = function
  | Env.TypeInformation.SF_sig (_, n, _) ->
      Format.printf "<LI>Signature: %a</LI>@\n" Sourcify.pp_vname n
  | Env.TypeInformation.SF_let (_, n, _, _, _, _, _) ->
      Format.printf "<LI>Let: %a</LI>@\n" Sourcify.pp_vname n
  | Env.TypeInformation.SF_let_rec let_field_infos ->
      Format.printf "<LI><UL>Let rec@\n" ;
      List.iter
	(fun (_, n, _, _, _, _, _) ->
	  Format.printf "<LI>Let: %a</LI>@\n" Sourcify.pp_vname n)
	let_field_infos ;
      Format.printf "</UL></LI>@\n"
  | Env.TypeInformation.SF_theorem (_, n, _, _, _, _) ->
      Format.printf "<LI>Theorem: %a</LI>@\n" Sourcify.pp_vname n
  | Env.TypeInformation.SF_property (_, n, _, _, _) ->
      Format.printf "<LI>Property: %a</LI>@\n" Sourcify.pp_vname n
;;



let gendoc_species_description species_description =
  Format.printf "<P>I closed ? : %b@\n"
    species_description.Env.TypeInformation.spe_is_closed ;
  Format.printf "<UL>" ;
  List.iter
    gendoc_species_field
    species_description.Env.TypeInformation.spe_sig_methods ;
  Format.printf "</UL>" ;
;;



let gendoc_please_compile_me = function
  | Infer.PCM_use (_, module_name) ->
      Format.printf "<H1>Used module: %s</HY1>@\n" module_name ;
  | Infer.PCM_open (_, module_name) ->
      Format.printf "<H1>Opened module: %s</HY1>@\n" module_name ;
  | Infer.PCM_coq_require module_name ->
      Format.printf
	"<H1>Module required for Coq external definitions: %s</HY1>@\n"
	module_name ;
  | Infer.PCM_species (species_def, species_description, _) ->
      Format.printf "<H1>Species <B>%a<B></H1@\n"
	Sourcify.pp_vname species_def.Parsetree.ast_desc.Parsetree.sd_name ;
	gendoc_species_description species_description
  | Infer.PCM_collection 
      (_collection_def, _species_description, _deps_graph_nodes) -> ()
  | Infer.PCM_type (_vname, _type_description) -> ()
  | Infer.PCM_let_def (_let_def, _type_schemes) -> ()
  | Infer.PCM_theorem (_theorem_def, _) -> ()
  | Infer.PCM_expr _expr -> ()
;;



let gendoc_please_compile_me input_file_name pcms =
  Format.printf "<HTML><BODY>@\n" ;
  Format.printf "<TITLE>%s</TITLE>@\n" input_file_name ;
  List.iter gendoc_please_compile_me pcms ;
  Format.printf "</BODY></HTML>@\n" ;
;;
