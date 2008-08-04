open Parsetree
open Parsetree_utils
open Infer
open Env
open Env.TypeInformation
open Format

type context = 
    { c_file : string;
      c_src : formatter;
      c_hdr : formatter }

let rec compile_species_field ctx = function
    [] -> ()
  | h::t ->
      begin match h with
	SF_sig ((_, vn, ts)) ->
	  let name = vname_as_string_with_operators_expanded vn in
	  fprintf ctx.c_hdr "@\n@[%a;@]"
	    (Types.pp_type_scheme_to_c name) ts
      | SF_let _ ->
	  fprintf err_formatter "@[TODO : SF_let@]@."
      |	SF_let_rec _ ->
	  fprintf err_formatter "@[TODO : SF_let_rec@]@."
      |	SF_theorem _ -> ()
      |	SF_property _ -> ()
      end;
      compile_species_field ctx t

let external_value ee =
  let rec search = function
      [] -> raise Not_found
    | (EL_external "C", c)::_ | (EL_external "c", c)::_ -> c
    | _::t -> search t
  in
  let code = search ee.ast_desc in
  let len = String.length code in
  let off_start = ref 0 in
  let off_end = ref (len-1) in
  while code.[!off_start] = ' ' do
    incr off_start
  done;
  while code.[!off_end] = ' ' do
    decr off_end
  done;
  String.sub code !off_start ((!off_end + 1) - !off_start)

exception C_def_missing of vname * Location.t

let compile_type ctx _ (vn, desc) =
  match desc.type_kind with
    TK_abstract ->
      fprintf err_formatter "TODO: TK_abstract@."
  | TK_external (ee, eb) ->
      begin 
	try
	  let code = external_value ee in
	  let name = vname_as_string_with_operators_expanded vn in
	  if code = name then
	    fprintf ctx.c_hdr "@\n@[/* @[<hov 2>typedef@;@[%s@;%s@];@]@] */@."
	      code name
	  else
	    fprintf ctx.c_hdr "@\n@[<hov 2>typedef@;@[%s@;%s@];@]@."
	      code name;
	  if eb.ast_desc <> [] then
	    fprintf err_formatter "TODO: TK_external (with bindings)@."
	with
	  Not_found -> raise (C_def_missing (vn, desc.type_loc))
      end
  | TK_variant _ ->
      fprintf err_formatter "TODO: TK_variant@."
  | TK_record _ ->
      fprintf err_formatter "TODO: TK_record@."

let rec compile_phrase ctx = function
    [] -> ()
  | (ph, pcm)::t ->
      begin match (ph.ast_desc, pcm) with
	(Ph_use file, PCM_no_matter) ->
	  fprintf ctx.c_hdr "@\n@[#include \"%s.h\"@]@." file
      |	(Ph_open _, PCM_open _) ->
	  ()
      |	(Ph_coq_require _, PCM_coq_require _) ->
	  ()
      |	(Ph_species sd, PCM_species (_, desc, _)) ->
	  fprintf ctx.c_hdr "@\n@[@[<hov 2>typedef struct {";
	  compile_species_field ctx desc.spe_sig_methods;
	  fprintf ctx.c_hdr "@]@\n} %a;@]@."
	    pp_vname_with_operators_expanded sd.ast_desc.sd_name
      |	(Ph_theorem _, PCM_theorem _) ->
	  ()
      | (Ph_collection _, _) ->
	  fprintf err_formatter "@[TODO : Ph_collection@]@."
      |	(Ph_type td, PCM_type (vn, desc)) ->
	  compile_type ctx td (vn, desc)
      |	(Ph_let _, _) ->
	  fprintf err_formatter "@[TODO : Ph_let@]@."
      |	(Ph_theorem _, _) -> ()
      |	(Ph_expr _, _) ->
	  fprintf err_formatter "@[TODO : Ph_expr@]@."
      |	_ -> assert false
      end;
      compile_phrase ctx t

let compile file l =
  let dir = Filename.dirname file in
  let base = Filename.chop_extension (Filename.basename file) in
  let header = Filename.concat dir (base^".h") in
  let source = Filename.concat dir (base^".c") in
  let header_out = open_out_bin header in
  let source_out = open_out_bin source in
  try
    let header_ppf = formatter_of_out_channel header_out in
    let source_ppf = formatter_of_out_channel source_out in
    let write_intro ppf =
      fprintf ppf "@[<hov 3>/*@ This@ file@ is@ automaticaly@ generated.@ Modify@ it@ at@ you@ own@ risks.@ */@]@." in
    write_intro header_ppf;
    write_intro source_ppf;
    fprintf source_ppf "@\n@[#include \"%s.h\"@]@." base;
    (* preventing multiple inclusion *)
    let macro = String.uppercase base in
    fprintf header_ppf "@\n@[#ifndef __%s_H@\n#define __%s_H@]@." macro macro;
    let ctx = { c_file = file;
		c_src = source_ppf;
		c_hdr = header_ppf } in
    compile_phrase ctx l;
    fprintf header_ppf "@\n@[#endif@]@.";
    close_out header_out;
    close_out source_out
  with
    anything ->
      begin
	close_out header_out;
	close_out source_out;
	raise anything
      end
