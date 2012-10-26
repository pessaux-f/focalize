open Parsetree
open Parsetree_utils
open Pred

exception PredSyntaxError of Location.t
exception PredPropertyError of Location.t

(* Split a string with a delimiter. *)
let rec split_string str c = if String.contains str c then
    let pos = String.index str c in
    (String.sub str 0 pos)::
      (split_string (String.sub str (pos+1) (String.length str - pos - 1)) c)
  else [str]

(* Remove beginning spaces in a string. *)
let rec skip_blanks str = if str = "" || str.[0] = ' ' || str.[0] = '\t' then
    skip_blanks (String.sub str 1 (String.length str - 1))
  else str

(* Remove last spaces in a string. *)
let rec remove_tail_blanks str = let n = String.length str - 1 in
  if str = "" || str.[n] = ' ' || str.[n] = '\t' then
    remove_tail_blanks (String.sub str 0 n)
  else str

(* Parse a tuple in an annotation. *)
let parse_an_tuple str =
  let parts = split_string str ',' in
  List.map (fun s -> skip_blanks (remove_tail_blanks s)) parts

(* Parse an extraction command in an annotation. *)
let parse_annot_part aeloc an = match split_string an ':' with
  | name::tail::[] ->
    let tail = skip_blanks tail in
    let pos_blank = String.index tail ' '
    and ob_mode = String.index tail '(' 
    and cb_mode = String.index tail ')' 
    and ob_props = String.rindex tail '(' 
    and cb_props = String.rindex tail ')' in
    let mode_tuple = String.sub tail (ob_mode+1) (cb_mode-ob_mode-1) 
    and props_tuple = String.sub tail (ob_props+1) (cb_props-ob_props-1) in
    let mode = parse_an_tuple mode_tuple
    and props = parse_an_tuple props_tuple
    and name = remove_tail_blanks (skip_blanks name)
    and pred_name = String.sub tail 0 pos_blank in
      (name, pred_name, mode, props)
  | _ -> raise (PredSyntaxError aeloc)

(* Signatures annotations parsing. *)
let parse_annotations ae =
  if ae.ae_tag <> "Pred" then []
  else let annots = split_string ae.ae_desc '\n' in
  List.map (parse_annot_part ae.ae_loc) annots

(* Get a string from a focalize ident. *)
let name_from_ident ident = match ident.Parsetree.ast_desc with
	| Parsetree.I_local vn -> Parsetree_utils.name_of_vname vn
	| Parsetree.I_global qvn -> (match qvn with
		| Parsetree.Vname vn -> Parsetree_utils.name_of_vname vn
		| Parsetree.Qualified (_,vn) -> Parsetree_utils.name_of_vname vn
	)

(* Get a string from a focalize expr ident. *)
let name_from_expr_ident ident = match ident.Parsetree.ast_desc with
	| Parsetree.EI_local vn -> Parsetree_utils.name_of_vname vn
	| Parsetree.EI_global qvn -> ( match qvn with
		| Parsetree.Vname vn -> Parsetree_utils.name_of_vname vn
		| Parsetree.Qualified (_,vn) -> Parsetree_utils.name_of_vname vn )
	| Parsetree.EI_method (_,vn) -> Parsetree_utils.name_of_vname vn
;;
	
(* Get a function name from a focalize expr. *)
let f_name_from_expr expr = match expr.Parsetree.ast_desc with
	| Parsetree.E_var (expr_id) -> name_from_expr_ident expr_id
	| _ -> raise Not_found
;;

(* Get a constructor's name from a focalize expr. *)
let c_name_from_expr expr =
  match expr.Parsetree.ast_desc with
	| Parsetree.CI ident -> name_from_ident ident
;;

(* Get a focalize species name. *)
let sp_name_from_spexpr spexpr = 
  name_from_ident spexpr.Parsetree.ast_desc.Parsetree.se_name
;;

(* Find the definition of a species from its name. *)
let rec find_species_def phlist name = match phlist with
	| [] -> failwith "Impossible to find the species."
	| p::q -> (match p.Parsetree.ast_desc with
		| Parsetree.Ph_species sp_def -> let sp_name = 
      Parsetree_utils.name_of_vname
			  sp_def.Parsetree.ast_desc.Parsetree.sd_name in
			if sp_name = name then sp_def
      else find_species_def q name
		| _ -> find_species_def q name
	)

(* Test if a property must be extracted. *)
let test_prop_name prop_def prop_names =
  let prop_name = Parsetree_utils.name_of_vname
	  prop_def.Parsetree.ast_desc.Parsetree.prd_name in
	List.exists ((=) prop_name) prop_names

(* Remove a property from a list. *)
let remove_prop_name prop_def prop_names =
  let prop_name = Parsetree_utils.name_of_vname
	  prop_def.Parsetree.ast_desc.Parsetree.prd_name in
	List.filter ((<>) prop_name) prop_names

(* Find properties for extraction in species fields. *)
let rec find_properties_in_fields phlist parent_species_names 
    species_fields prop_names accu =
	match species_fields with
	| [] -> find_properties phlist parent_species_names prop_names accu
	| x::q -> begin match x.Parsetree.ast_desc with
		| Parsetree.SF_property prop_def ->
			if test_prop_name prop_def prop_names then
				let nprop_names = remove_prop_name prop_def prop_names in
				find_properties_in_fields phlist parent_species_names q nprop_names 
          (prop_def::accu)
			else
        find_properties_in_fields phlist parent_species_names q prop_names 
          accu
		| _ -> find_properties_in_fields phlist parent_species_names q prop_names 
          accu
	end

(* Find properties for extraction. *)
and find_properties phlist species_name_list prop_names accu = 
  match species_name_list with
	  | [] -> accu
	  | name::q -> let sp_def = find_species_def phlist name in
		  let fields = sp_def.Parsetree.ast_desc.Parsetree.sd_fields in
		  let parents = List.map
			  sp_name_from_spexpr
			  sp_def.Parsetree.ast_desc.Parsetree.sd_inherits.Parsetree.ast_desc in
		  find_properties_in_fields phlist (parents@q) fields prop_names accu

(* Strings from focalize names. *)
let get_names_of_vnames var_list =
  List.map Parsetree_utils.name_of_vname var_list

(* Extract a FoCaLiZe expression. *)
let rec extract_expr expr =
  match expr.Parsetree.ast_desc with
	| Parsetree.E_app (fn_expr, param_exprs) ->
      let f_name = f_name_from_expr fn_expr in
		  let params = List.map extract_expr param_exprs in
		  TFun (f_name, params)
	| Parsetree.E_var expr_ident -> TVar (name_from_expr_ident expr_ident)
	| Parsetree.E_constr (cstr_ident, param_exprs) ->
      let c_name = c_name_from_expr cstr_ident in
		  let params = List.map extract_expr param_exprs in
		  TConstr (c_name, params, CLNone)
	| Parsetree.E_paren expr -> extract_expr expr
  | _ -> failwith "foc2spec: extract_expr: TODO."
;;


(* Cut the implications in a property. *)
let rec cut_property l_expr = match l_expr.Parsetree.ast_desc with
	| Parsetree.Pr_imply (exp1,exp2) -> (cut_property exp1)@(cut_property exp2)
	| Parsetree.Pr_expr expr -> [expr]
	| _ -> failwith "Bad property structure"

(* Extract quantified variables in a property. *)
let rec catch_forall l_expr = match l_expr.Parsetree.ast_desc with
	| Parsetree.Pr_forall (vars,_,exp) -> let v,last_e = catch_forall exp
		in ((get_names_of_vnames vars)@v, last_e)
	| _ -> ([], l_expr)

(* Get the tail element of a list and the list without it. *)
let rec split_list = function
  | [] -> raise Not_found
  | [x] -> (x, [])
	| e :: q -> let (x, t) = split_list q in
    (x, (e :: t))
;;

(* Get a single predicate from an environment. *)
let get_pred env sig_name =
  match List.assoc sig_name env with
  | [pred] -> pred
  | _ -> raise Not_found
;;

(* Extract a term in a premisse. *)
let extract_prem_term env prem =
  match prem.ast_desc with
	| Parsetree.E_app (fn_expr, param_exprs) -> 
      let f_name = f_name_from_expr fn_expr in
		  let params = List.map extract_expr param_exprs in
      if List.mem_assoc f_name env then
        List.map
          (fun pred -> 
            PMTPred {pdt_pred = pred; pdt_args = params; pdt_not_flag = false}) 
          (List.assoc f_name env)
      else [PMTFun (TFun (f_name, params))]
  | _ -> failwith "foc2spec: extract_prem_term: TODO."
;;

(* Extract a premisse. *)
let extract_prem env prem =
  match extract_prem_term env prem with
  | [premt] -> PMTerm premt
  | premts -> PMChoice (List.map (fun premt -> PMTerm premt) premts)
;;

(* Extract a property. *)
let extract_property env sig_name prop_def =
	let (variables, prop_expr) = 
    catch_forall prop_def.Parsetree.ast_desc.Parsetree.prd_logical_expr in
	let prop_parts = cut_property prop_expr in
	let (concl, prems) = split_list prop_parts in
	let extr_concl =
    (match extract_expr concl with
		| TFun (f_name, expr_list) -> if f_name = sig_name then
        { pdt_pred = (try get_pred env sig_name with Not_found ->
          raise (PredPropertyError prop_def.ast_loc));
          pdt_args = expr_list ;
          pdt_not_flag = false }
		else raise (PredPropertyError prop_def.ast_loc)
		| _ -> raise (PredPropertyError prop_def.ast_loc)) in
	let extr_prems = List.map (extract_prem env) prems in
	{ p_forall = variables ; p_prems = extr_prems ; p_concl = extr_concl }
;;

(* Extract properties. *)
let extract_properties env sig_name prop_defs = 
  List.map (extract_property env sig_name) prop_defs

(*let get_mode mode =
	if List.length mode = 0 then Types_pgen.ModeAll
	else
		Types_pgen.ModeList (List.map
			(function x -> match x.Parsetree.ast_desc with
				| Parsetree.E_const cte -> (match cte.Parsetree.ast_desc with
					| Parsetree.C_int intstr -> int_of_string intstr
					| _ -> failwith "Bad mode"
					)
				| _ -> failwith "Bad mode"
			)
			mode
		)
*)


(*let get_str_from_expr x = match x.Parsetree.ast_desc with
				| Parsetree.E_var expr_id -> (match expr_id.Parsetree.ast_desc with
					| Parsetree.EI_local vn -> Parsetree_utils.name_of_vname vn
					| _ -> failwith "Bad terminaison argument"
					)
				| _ -> failwith "Bad terminaison argument"
*)


(* Parse an extraction mode. *)
let rec make_mode = function
  | [] -> []
  | "I" :: tail_mode -> MInput :: (make_mode tail_mode)
  | "O" :: tail_mode -> MOutput :: (make_mode tail_mode)
  | _ -> failwith "foc2spec: make_mode: TODO."
;;


(* Search for extraction annotations in a species. *)
let rec search_extractions_in_sp phlist species_def fields = match fields with
	| [] -> []
	| f::q ->
		( match f.ast_desc with
			| SF_sig sig_def -> let extractions = List.flatten 
          (List.map parse_annotations sig_def.ast_annot) in
        let extractions = List.map (fun (f,p,m,pr) -> (f,p,make_mode m,pr)) 
          extractions in
        let env = List.map 
          (fun (f,p,m,_) -> 
            (p,[{pred_name = p; pred_mode = m; pred_fun_name = f}])) 
          extractions in
        let sp_name = name_of_vname species_def.ast_desc.sd_name in
        ( List.map (fun (f_name, sig_name, mode, props) ->
          let props = extract_properties env sig_name 
            (find_properties phlist [sp_name] props []) in
          (sp_name, {
            ps_props = props;
            ps_pred_info = [];
            ps_constr_info = [];
            ps_record_info = [];
            ps_const_info = [];
            ps_fun_info = [];
            ps_ind_env = [];
            ps_pred = 
              {pred_name = sig_name; pred_mode = mode; pred_fun_name = f_name};
          })
        ) extractions )@(search_extractions_in_sp phlist species_def q)
      | _ -> search_extractions_in_sp phlist species_def q )
    


(*Printf.printf "toto\n"; List.iter (fun ae -> Printf.printf "annot: %s %s\n" ae.ae_tag ae.ae_desc) sig_def.ast_annot*)(*let extr_info = extract_def.Parsetree.ast_desc
				in let sp_name = Parsetree_utils.name_of_vname species_def.Parsetree.ast_desc.Parsetree.sd_name
				in let f_name = Parsetree_utils.name_of_vname extr_info.Parsetree.extr_name
				in let sig_name = Parsetree_utils.name_of_vname extr_info.Parsetree.extr_pred
				in let props = extract_properties
					sig_name
					(find_properties phlist [sp_name] (get_names_of_vnames extr_info.Parsetree.extr_rules) [])
				in let mode = get_mode extr_info.Parsetree.extr_mode
                in let term = Types_pgen.Struct (List.map get_str_from_expr extr_info.Parsetree.extr_term_arg)
				in (Types_pgen.Extract (sp_name, sig_name, f_name, mode, props, term))
					::*)

(* Search for extraction annotations. *)
let rec search_extractions_sub orig_phlist = function
	| [] -> []
	| ph :: q -> (
      match ph.Parsetree.ast_desc with
			| Parsetree.Ph_species species_def ->
				  (search_extractions_in_sp orig_phlist species_def
             species_def.Parsetree.ast_desc.Parsetree.sd_fields)
				  @ (search_extractions_sub orig_phlist q)
			| _ -> search_extractions_sub orig_phlist q
		 )
;;


(* Search for extraction annotations. *)
let search_extractions file_ast =
  match file_ast.Parsetree.ast_desc with
	| File phlist -> search_extractions_sub phlist phlist
;;


let foc2spec focfile = search_extractions focfile ;;
