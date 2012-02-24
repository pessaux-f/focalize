open Pred
open Parsetree
open Parsetree_utils


(* Dummy ast location. *)
let null_pos = {
	Location.pos_fname = "";
	Location.pos_lnum = 0;
	Location.pos_bol = 0;
	Location.pos_cnum = 0;
}

let null_t = { Location.l_beg = null_pos ; Location.l_end = null_pos }

(* Make ast with dummy location. *)
let mk_ast thing = {
	Parsetree.ast_loc = null_t;
	Parsetree.ast_desc = thing;
	Parsetree.ast_annot = [];
	Parsetree.ast_type = Parsetree.ANTI_none
}

(* Focalize identifiers generation. *)

let gen_ident var_name = if String.capitalize var_name = var_name then
    Parsetree.Vuident var_name
	else Parsetree.Vlident var_name
let gen_iident var_name = Parsetree.Viident var_name
let gen_ident_expr var_name = 
  mk_ast (Parsetree.E_var (mk_ast (Parsetree.EI_local (gen_ident var_name))))
let gen_iident_expr var_name = 
  mk_ast (Parsetree.E_var (mk_ast (Parsetree.EI_local (gen_iident var_name))))
let gen_ident_cons var_name = 
  mk_ast (Parsetree.CI ( mk_ast (Parsetree.I_local (gen_ident var_name))))


(* Focalize pattern expressions generation. *)
let rec gen_focalize_pat_expr pat_expr = match pat_expr with
    | LPTrue -> mk_ast (Parsetree.P_const (mk_ast (Parsetree.C_bool "true")))
    | LPFalse -> mk_ast (Parsetree.P_const (mk_ast (Parsetree.C_bool "false")))
    | LPVar v_name -> mk_ast (Parsetree.P_var (gen_ident v_name))
	  | LPConstr (p_name, args) -> 
      mk_ast (Parsetree.P_constr (gen_ident_cons p_name,
		    List.map gen_focalize_pat_expr args))
    | LPWild -> mk_ast (Parsetree.P_wild)
    | LPTuple pat_expr_list -> mk_ast 
       (Parsetree.P_tuple (List.map gen_focalize_pat_expr pat_expr_list))

(* Pattern generation. *)
let rec gen_focalize_pat pat code d =
  (gen_focalize_pat_expr pat, gen_focalize_code d code) 

(* Focalize expression generation. *)
and gen_focalize_code d code = match code with
  | LMatch (t, pat_code_list)-> mk_ast (
		Parsetree.E_match (
		  (gen_focalize_code d t),
			List.map (function (pat,code) -> 
        gen_focalize_pat pat code d) pat_code_list)
    )
  | LLin cond ->
    let cond = List.map
      (fun (x,y) -> mk_ast ( Parsetree.E_app ( (gen_iident_expr "="),
			  [(gen_ident_expr x); (gen_ident_expr y)]))) cond in
    List.fold_left
			(fun exp -> fun c ->
				( mk_ast ( Parsetree.E_app ( (gen_iident_expr "&&"),
				[exp; c]))))
			(List.hd cond) (List.tl cond)
  | LFun (fn, args) -> mk_ast (Parsetree.E_app ((gen_ident_expr fn), 
                         (List.map (gen_focalize_code d) args)))
  | LConstr (cn, args) -> mk_ast (Parsetree.E_constr ((gen_ident_cons cn), 
                            (List.map (gen_focalize_code d) args)))
  | LTuple (args) -> 
     mk_ast (Parsetree.E_tuple (List.map (gen_focalize_code d) args))
  | LVar n -> gen_ident_expr n
  | LTrue -> mk_ast (Parsetree.E_const (mk_ast (Parsetree.C_bool "true")))
  | LFalse -> mk_ast (Parsetree.E_const (mk_ast (Parsetree.C_bool "false")))
  | LDefault -> d
  | LFunNot _ -> raise Not_found
  | LConst _  -> raise Not_found
  | LRecord _ -> raise Not_found
(*  mk_ast (Parsetree.E_app (gen_ident_expr "focalize_error", [mk_ast (Parsetree.E_const (mk_ast (Parsetree.C_string err)))])) *)

(* Focalize code generation. *)
let rec gen_focalize (fn, args, code) d = 
  let fargs = List.map (function x -> (gen_ident x, None)) args in
 Parsetree.SF_let (mk_ast ( {
	Parsetree.ld_rec = Parsetree.RF_rec;
	Parsetree.ld_logical = Parsetree.LF_no_logical;
	Parsetree.ld_local = Parsetree.LF_no_local;
	Parsetree.ld_bindings = [mk_ast ({
		Parsetree.b_name = gen_ident fn;
		Parsetree.b_params = fargs;
		Parsetree.b_type = None;
		Parsetree.b_body = Parsetree.BB_computational ( gen_focalize_code d code );
	})];
	Parsetree.ld_termination_proof = None;
	} ))

(* Name of a focalize let. *)
let field_name f = match f with 
  | Parsetree.SF_let l -> 
    let bind = List.hd (l.Parsetree.ast_desc.Parsetree.ld_bindings) in
    Parsetree_utils.name_of_vname bind.Parsetree.ast_desc.Parsetree.b_name

(* Name of a focalize signture. *)
let sig_field_name f = match f with 
  | Parsetree.SF_sig s -> Parsetree_utils.name_of_vname 
    (s.Parsetree.ast_desc.Parsetree.sig_name)

(* Replace a signture by its definition. *)

let rec replace_sf_in_list fl f = match fl with
	| [] -> []
	| (Parsetree.SF_sig _)::q -> let orig_f = List.hd fl in
	if (field_name f) = (sig_field_name orig_f) then (f)::q
		else orig_f::(replace_sf_in_list q f)
	| x::q -> x::(replace_sf_in_list q f)

let replace_sf_in_s stuff field = 
  List.map (function x -> replace_sf_in_list x field) stuff

let replace_sf stuff sp_name field (*proofs*) = 
  let nphlist = List.map (function x -> match x.Parsetree.ast_desc with
	  | Parsetree.Ph_species species_def -> 
      let name = Parsetree_utils.name_of_vname
			  species_def.Parsetree.ast_desc.Parsetree.sd_name in
		  if name = sp_name then
		(let new_sdlt = replace_sf_in_list
				( List.map (function x -> x.Parsetree.ast_desc) 
            species_def.Parsetree.ast_desc.Parsetree.sd_fields) field
				in let new_sdl = List.map mk_ast new_sdlt
				in let new_species_def = mk_ast ( {
					Parsetree.sd_name = species_def.Parsetree.ast_desc.Parsetree.sd_name;
					Parsetree.sd_params = 
            species_def.Parsetree.ast_desc.Parsetree.sd_params;
					Parsetree.sd_inherits = 
            species_def.Parsetree.ast_desc.Parsetree.sd_inherits;
					Parsetree.sd_fields = new_sdl(*@proofs*);
				} )
				in mk_ast (Parsetree.Ph_species new_species_def)
		) else x
	 | _ -> x
  ) (match stuff.Parsetree.ast_desc with Parsetree.File phlist -> phlist) in 
  mk_ast ( Parsetree.File nphlist )

let inject_code file_ast (sp_name, field) = replace_sf file_ast sp_name field


(* Default case for pattern matching. *)
let default_case spec =
  if List.exists ((=) MOutput) spec.ps_pred.pred_mode then
    mk_ast (Parsetree.E_app (gen_ident_expr "focalize_error", 
      [mk_ast (Parsetree.E_const (mk_ast (Parsetree.C_string "error")))]))
  else  mk_ast (Parsetree.E_const (mk_ast (Parsetree.C_bool "false")))
