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

(* $Id: main_docgen.ml,v 1.3 2008-10-14 13:42:29 pessaux Exp $ *)


let gendoc_species_field out_fmt = function
  | Env.TypeInformation.SF_sig (_, n, sch) ->
      Format.fprintf out_fmt "<LI>Signature: %a : %a</LI>@\n"
	Sourcify.pp_vname n Types.pp_type_scheme sch
  | Env.TypeInformation.SF_let (_, n, _, _, _, _, _) ->
      Format.fprintf out_fmt "<LI>Let: %a</LI>@\n" Sourcify.pp_vname n
  | Env.TypeInformation.SF_let_rec let_field_infos ->
      Format.fprintf out_fmt "<LI><UL>Let rec@\n" ;
      List.iter
	(fun (_, n, _, _, _, _, _) ->
	  Format.fprintf out_fmt "<LI>Let: %a</LI>@\n" Sourcify.pp_vname n)
	let_field_infos ;
      Format.fprintf out_fmt "</UL></LI>@\n"
  | Env.TypeInformation.SF_theorem (_, n, _, _, _, _) ->
      Format.fprintf out_fmt "<LI>Theorem: %a</LI>@\n" Sourcify.pp_vname n
  | Env.TypeInformation.SF_property (_, n, _, _, _) ->
      Format.fprintf out_fmt "<LI>Property: %a</LI>@\n" Sourcify.pp_vname n
;;



let gendoc_species_description out_fmt species_description =
  Format.fprintf out_fmt "<P>I closed ? : %b@\n"
    species_description.Env.TypeInformation.spe_is_closed ;
  Format.fprintf out_fmt "<UL>" ;
  List.iter
    (gendoc_species_field out_fmt)
    species_description.Env.TypeInformation.spe_sig_methods ;
  Format.fprintf out_fmt "</UL>" ;
;;



let gendoc_please_compile_me out_fmt = function
  | Infer.PCM_use (_, module_name) ->
      Format.fprintf out_fmt "<H1>Used module: %s</HY1>@\n" module_name ;
  | Infer.PCM_open (_, module_name) ->
      Format.fprintf out_fmt "<H1>Opened module: %s</HY1>@\n" module_name ;
  | Infer.PCM_coq_require module_name ->
      Format.fprintf out_fmt
	"<H1>Module required for Coq external definitions: %s</HY1>@\n"
	module_name ;
  | Infer.PCM_species (species_def, species_description, _) ->
      Format.fprintf out_fmt "<H1>Species <B>%a<B></H1@\n"
	Sourcify.pp_vname species_def.Parsetree.ast_desc.Parsetree.sd_name ;
	gendoc_species_description out_fmt species_description
  | Infer.PCM_collection 
      (_collection_def, _species_description, _deps_graph_nodes) -> ()
  | Infer.PCM_type (_vname, _type_description) -> ()
  | Infer.PCM_let_def (_let_def, _type_schemes) -> ()
  | Infer.PCM_theorem (_theorem_def, _) -> ()
  | Infer.PCM_expr _expr -> ()
;;



let gendoc_please_compile_me input_file_name pcms =
  let out_filename = (Filename.chop_extension input_file_name) ^ ".html" in
  let out_channel = open_out_bin out_filename in
  let out_fmt = Format.formatter_of_out_channel out_channel in
  Format.fprintf out_fmt "<HTML><BODY>@\n" ;
  Format.fprintf out_fmt "<TITLE>%s</TITLE>@\n" input_file_name ;
  List.iter (gendoc_please_compile_me out_fmt) pcms ;
  Format.fprintf out_fmt "</BODY></HTML>@\n" ;
  close_out out_channel
;;
