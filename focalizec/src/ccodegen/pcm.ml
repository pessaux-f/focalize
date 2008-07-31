(***********************************************************************)
(*                                                                     *)
(*                   FoCaL compiler (C translation)                    *)
(*                                                                     *)
(*            Julien Blond                                             *)
(*                                                                     *)
(*                               LIP6  --  Bertin Technologies         *)
(*                                                                     *)
(*  Copyright 2008 LIP6 and Bertin Technologies                        *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

open Ast;;

type pcm_type = 
  | T_var of int
  | T_ident of ident
  | T_fun of pcm_type * pcm_type
  | T_app of pcm_type * pcm_type list
  | T_tuple of pcm_type list
  | T_prop
  | T_paren of pcm_type
  | T_species of pcm_type option * (ident * pcm_type) list
  | T_none

and ident = (ident_desc, pcm_type) ast
and ident_desc =
    { mutable i_file : string;
      i_spc : string;
      i_name : string }
;;

type type_expr = (type_expr_desc, unit) ast
and type_expr_desc =
  | TE_var of string
  | TE_ident of ident
  | TE_fun of type_expr * type_expr
  | TE_app of ident * type_expr list
  | TE_tuple of type_expr list
  | TE_self
  | TE_prop
  | TE_paren of type_expr
;;

type constant = (constant_desc, pcm_type) ast
and constant_desc =
  | C_int of string
  | C_float of string
  | C_bool of string
  | C_string of string
  | C_char of char
;;

type pattern = (pattern_desc, pcm_type) ast
and pattern_desc =
  | P_const of constant
  | P_var of ident
  | P_as of pattern * ident
  | P_wild
  | P_constr of ident * pattern list
  | P_record of (ident * pattern) list
  | P_tuple of pattern list
  | P_paren of pattern
;;

type 'a binding = ('a binding_desc, unit) ast
and 'a binding_desc = 
    { b_name : ident;
      b_params : ident list;
      b_type : type_expr option list * type_expr option;
      b_body : 'a }
;;

type 'a let_def = ('a let_def_desc, unit) ast
and 'a let_def_desc =
    { ld_rec : bool;
      ld_local : bool;
      ld_logical : bool;
      ld_bindings : ('a binding) list }
;;

type external_expr = (external_expr_desc, unit) ast
and external_expr_desc = (string * string) list
;;

type expr = (expr_desc, pcm_type) ast
and expr_desc =
  | E_self
  | E_constant of constant
  | E_fun of ident list * expr
  | E_var of ident
  | E_app of expr * expr list
  | E_constr of ident * expr list
  | E_match of expr * (pattern * expr) list
  | E_if of expr * expr * expr
  | E_let of expr let_def * expr
  | E_logical_let of logical_expr let_def * expr
  | E_record of (ident * expr) list
  | E_record_access of expr * ident
  | E_record_with of expr * (ident * expr) list
  | E_tuple of expr list
  | E_external of external_expr
  | E_paren of expr

and logical_expr = unit;;

type proof = unit;;

type sig_def = (sig_def_desc, unit) ast
and sig_def_desc =
    { sig_name : ident;
      sig_type : type_expr;
      sig_logical : bool }
;;

type property_def = (property_def_desc, unit) ast
and property_def_desc =
    { pr_name : ident;
      pr_prop : logical_expr }
;;

type theorem_def = (theorem_def_desc, unit) ast
and theorem_def_desc = 
    { t_local : bool;
      t_name : ident;
      t_prop : logical_expr;
      t_proof : proof }
;;

type proof_def = (proof_def_desc, unit) ast
and proof_def_desc =
    { pf_name : ident;
      pf_proof : proof }
;;

type species_field = (species_field_desc, unit) ast
and species_field_desc =
    Sf_rep of type_expr
  | Sf_sig of sig_def 
  | Sf_let of expr let_def
  | Sf_logical_let of logical_expr let_def
  | Sf_theorem of theorem_def
  | Sf_property of property_def
  | Sf_proof of proof_def
;;

type external_binding = (external_binding_desc, unit) ast
and external_binding_desc = (ident * external_expr)
;;

type type_kind = (type_kind_desc, unit) ast
and type_kind_desc =
  | Tk_external of type_kind option * external_expr * external_binding list
  | Tk_inductive of (ident * type_expr list) list
  | Tk_record of (ident * type_expr) list
  | Tk_alias of type_expr
;;

type type_def = (type_def_desc, unit) ast
and type_def_desc =
    { type_name : ident;
      type_params : ident list;
      type_body : type_kind }
;;

type species_param = (species_param_desc, unit) ast
and species_param_desc =
    Sp_in of expr
  | Sp_is of expr
;;

type species_def = (species_def_desc, unit) ast
and species_def_desc =
    { s_name : ident;
      s_params : ident list;
      s_spec : species_param list;
      s_inh : expr list;
      s_fields : species_field list }

type phrase = (phrase_desc, unit) ast
and phrase_desc =
  | Ph_open of string
  | Ph_use of string
  | Ph_require of string * string
  | Ph_species of species_def
  | Ph_collection of ident * expr
  | Ph_type of type_def
  | Ph_let of expr let_def
  | Ph_logical_let of logical_expr let_def
  | Ph_theorem of theorem_def
  | Ph_expr of expr
;;

type file = (file_desc, unit) ast
and file_desc =
    { file_name : string;
      mutable file_body : phrase list }
;;

module PP =
  struct
    open Format

    let ident ppf id =
      fprintf ppf "@[";
      let file = id.ast_desc.i_file in
      if file <> "" then
	fprintf ppf "%s#" file;
      let spc = id.ast_desc.i_spc in
      if spc <> "" then
	fprintf ppf "%s!" spc;
      fprintf ppf "%s@]" id.ast_desc.i_name

  end;;

(* The conversion part *)
type context =
    { mutable file_id : string;
      mutable spc_id : string }
;;

let empty id =
  { file_id = id;
    spc_id = "" }
;;

exception Disapointed of Location.t;;

let mk_vname = Parsetree_utils.vname_as_string_with_operators_expanded
;;

let mk_id ctx ?(file = ctx.file_id) ?(spc = ctx.spc_id) id =
  { i_file = file;
    i_spc = spc;
    i_name = id }
;;

let mk_ident ctx id =
  mk_ast
    id.Parsetree.ast_loc
    T_none
    (match id.Parsetree.ast_desc with
      Parsetree.I_local vn ->
	mk_id ctx ~file:"" ~spc:"" (mk_vname vn)
    | Parsetree.I_global (Parsetree.Vname vn) ->
	mk_id ctx ~file:"" ~spc:"" (mk_vname vn)
    | Parsetree.I_global (Parsetree.Qualified (f, vn)) ->
	mk_id ctx ~file:f ~spc:"" (mk_vname vn))
;;

let mk_constructor_ident ctx id =
  mk_ast
    id.Parsetree.ast_loc
    T_none
    (match id.Parsetree.ast_desc with
      Parsetree.CI (Parsetree.Vname vn) ->
	mk_id ctx ~file:"" ~spc:"" (mk_vname vn)
    | Parsetree.CI (Parsetree.Qualified (f, vn)) ->
	mk_id ctx ~file:f ~spc:"" (mk_vname vn))
;;

let mk_label_ident ctx id =
  mk_ast
    id.Parsetree.ast_loc
    T_none
    (match id.Parsetree.ast_desc with
      Parsetree.LI (Parsetree.Vname vn) ->
	mk_id ctx ~file:"" (mk_vname vn)
    | Parsetree.LI (Parsetree.Qualified (f, vn)) ->
	mk_id ctx ~file:f (mk_vname vn))
;;


let mk_expr_ident ctx ei =
  mk_ast
    ei.Parsetree.ast_loc
    T_none
    (match ei.Parsetree.ast_desc with
      Parsetree.EI_local vn ->
	mk_id ctx ~file:"" ~spc:"" (mk_vname vn)
    | Parsetree.EI_global (Parsetree.Vname vn) ->
	mk_id ctx ~file:"" ~spc:"" (mk_vname vn)
    | Parsetree.EI_global (Parsetree.Qualified (f, vn)) ->
	mk_id ctx ~file:f ~spc:"" (mk_vname vn)
    | Parsetree.EI_method (None, vn) ->
	mk_id ctx ~file:"" ~spc:"" (mk_vname vn)
    | Parsetree.EI_method (Some (Parsetree.Vname spc), vn) ->
	mk_id ctx ~file:"" ~spc:(mk_vname spc) (mk_vname vn)
    | Parsetree.EI_method (Some (Parsetree.Qualified (f, spc)), vn) ->
	mk_id ctx ~file:f ~spc:(mk_vname spc) (mk_vname vn))
;;

let rec mk_type_expr ctx t =
  let loc = t.Parsetree.ast_loc in
  let mk_ast = mk_ast loc () in
  match t.Parsetree.ast_desc with
    Parsetree.TE_ident id ->
      begin match id.Parsetree.ast_desc with
      |	Parsetree.I_local vn
      |	Parsetree.I_global (Parsetree.Vname vn)
      |	Parsetree.I_global (Parsetree.Qualified (_, vn)) ->
	  begin match vn with
	  | Parsetree.Vqident s ->
	      mk_ast (TE_var s) 
	  | Parsetree.Vlident _ | Parsetree.Vuident _
	  | Parsetree.Vpident _ | Parsetree.Viident _ ->
	      mk_ast (TE_ident (mk_ident ctx id))	      
	  end
      end
  | Parsetree.TE_self ->
      mk_ast TE_self
  | Parsetree.TE_prop ->
      mk_ast TE_prop
  | Parsetree.TE_fun (l, r) ->
      mk_ast (TE_fun (mk_type_expr ctx l, mk_type_expr ctx r))
  | Parsetree.TE_app (id, l) ->
      mk_ast (TE_app (mk_ident ctx id, List.map (mk_type_expr ctx) l))
  | Parsetree.TE_prod l ->
      mk_ast (TE_tuple (List.map (mk_type_expr ctx) l))
  | Parsetree.TE_paren t' ->
      mk_ast (TE_paren (mk_type_expr ctx t'))
;;

let mk_language _ lang =
  match lang with
    Parsetree.EL_Caml -> "ocaml"
  | Parsetree.EL_Coq -> "coq"
  | Parsetree.EL_external s -> s
;;

let mk_external_expr ctx ee =
  mk_ast
    ee.Parsetree.ast_loc
    ()
    (List.map
       (function (lang, code) -> (mk_language ctx lang, code))
       ee.Parsetree.ast_desc)
;;

let mk_external_binding ctx eb =
  let loc = eb.Parsetree.ast_loc in 
  let (vn, ee) = eb.Parsetree.ast_desc in
  mk_ast
    loc
    ()
    (mk_ast loc T_none (mk_id ctx (mk_vname vn)), mk_external_expr ctx ee)
;;

let mk_constant _ c =
  let mk_ast = mk_ast c.Parsetree.ast_loc T_none in
  match c.Parsetree.ast_desc with
    Parsetree.C_int s -> mk_ast (C_int s)
  | Parsetree.C_float s -> mk_ast (C_float s)
  | Parsetree.C_bool s -> mk_ast (C_bool s)
  | Parsetree.C_string s -> mk_ast (C_string s)
  | Parsetree.C_char s -> mk_ast (C_char s)
;;

let rec mk_pattern ctx p =
  let loc = p.Parsetree.ast_loc in
  let mk_ast = mk_ast loc T_none in
  match p.Parsetree.ast_desc with
    Parsetree.P_const c ->
      mk_ast (P_const (mk_constant ctx c))
  | Parsetree.P_var vn ->
      mk_ast (P_var (Ast.mk_ast loc T_none (mk_id ctx ~file:"" (mk_vname vn))))
  | Parsetree.P_wild ->
      mk_ast (P_wild)
  | Parsetree.P_as (p', vn) ->
      mk_ast (P_as (mk_pattern ctx p',
		    Ast.mk_ast loc T_none (mk_id ctx ~file:"" (mk_vname vn))))
  | Parsetree.P_constr (ci, l) ->
      mk_ast (P_constr (mk_constructor_ident ctx ci,
			List.map (mk_pattern ctx) l))
  | Parsetree.P_record l ->
      mk_ast (P_record (List.map
			  (function (li, p') ->
			    (mk_label_ident ctx li, mk_pattern ctx p'))
			  l))
  | Parsetree.P_tuple l ->
      mk_ast (P_tuple (List.map (mk_pattern ctx) l))
  | Parsetree.P_paren p' ->
      mk_ast (P_paren (mk_pattern ctx p'))
;;

let mk_flag_rec _ = function
    Parsetree.RF_no_rec -> false
  | Parsetree.RF_rec -> true
;;

let mk_flag_logical _ = function
    Parsetree.LF_no_logical -> false
  | Parsetree.LF_logical -> true
;;

let mk_flag_local _ = function
    Parsetree.LF_no_local -> false
  | Parsetree.LF_local -> true
;;

let rec mk_expr ctx e =
  let loc = e.Parsetree.ast_loc in
  let mk_ast = mk_ast loc T_none in
  match e.Parsetree.ast_desc with
    Parsetree.E_self ->
      mk_ast E_self
  | Parsetree.E_var ei ->
      mk_ast (E_var (mk_expr_ident ctx ei))
  | Parsetree.E_app (a, l) ->
      mk_ast (E_app (mk_expr ctx a,
		     List.map (mk_expr ctx) l))
  | Parsetree.E_const c ->
      mk_ast (E_constant (mk_constant ctx c))
  | Parsetree.E_fun (l, a) ->
      mk_ast (E_fun (List.map
		       (fun x ->
			 Ast.mk_ast
			   loc T_none (mk_id ctx ~file:"" (mk_vname x)))
		       l,
		     mk_expr ctx a))
  | Parsetree.E_constr (id, l) ->
      mk_ast (E_constr (mk_constructor_ident ctx id,
			List.map (mk_expr ctx) l))
  | Parsetree.E_match (a, l) ->
      mk_ast (E_match (mk_expr ctx a,
		       List.map
			 (function (p, x) ->
			   (mk_pattern ctx p, mk_expr ctx x))
			 l))
  | Parsetree.E_if (a, b, c) ->
      mk_ast (E_if (mk_expr ctx a,
		    mk_expr ctx b,
		    mk_expr ctx c))
  | Parsetree.E_let (ld, x) ->
      mk_ast 
	(let mk_ast = Ast.mk_ast in
	let loc = ld.Parsetree.ast_loc in
	let ldd = ld.Parsetree.ast_desc in
	let frec = mk_flag_rec ctx ldd.Parsetree.ld_rec in
	let flog = mk_flag_logical ctx ldd.Parsetree.ld_logical in
	let floc = mk_flag_local ctx ldd.Parsetree.ld_local in
	let bindings = ldd.Parsetree.ld_bindings in
	if flog then
	  E_logical_let 
	    (mk_ast loc ()
	       { ld_rec = frec;
		 ld_local = floc;
		 ld_logical = flog;
		 ld_bindings =
		 List.map
		   (fun x ->
		     let loc = x.Parsetree.ast_loc in
		     let name = x.Parsetree.ast_desc.Parsetree.b_name in
		     let args = x.Parsetree.ast_desc.Parsetree.b_params in
		     let ret = x.Parsetree.ast_desc.Parsetree.b_type in
		     let _ =
		       match x.Parsetree.ast_desc.Parsetree.b_body with
			 Parsetree.BB_logical _ -> ()
		       | Parsetree.BB_computational _ -> assert false in
		     let pcm_name = mk_ast loc T_none (mk_id ctx (mk_vname name)) in
		     let pcm_params = List.map
			 (function (vn, _) ->
			   mk_ast loc T_none (mk_id ctx (mk_vname vn)))
			 args in
		     let pcm_type =
		       (List.map
			  (function
			      (_, None) -> 
				None
			    | (_, Some t) ->
				Some (mk_type_expr ctx t))
			  args,
			match ret with
			  None -> None
			| Some t -> Some (mk_type_expr ctx t)) in
		     mk_ast loc () { b_name = pcm_name;
				     b_params = pcm_params;
				     b_type = pcm_type;
				     b_body = () })
		   bindings },
	     mk_expr ctx x)
	else
	  E_let
	    (mk_ast loc ()
	       { ld_rec = frec;
		 ld_local = floc;
		 ld_logical = flog;
		 ld_bindings =
		 List.map
		   (fun x ->
		     let loc = x.Parsetree.ast_loc in
		     let name = x.Parsetree.ast_desc.Parsetree.b_name in
		     let args = x.Parsetree.ast_desc.Parsetree.b_params in
		     let ret = x.Parsetree.ast_desc.Parsetree.b_type in
		     let body =
		       match x.Parsetree.ast_desc.Parsetree.b_body with
			 Parsetree.BB_logical le ->
			   (match le.Parsetree.ast_desc with
			   | Parsetree.Pr_expr e -> e
			   | Parsetree.Pr_forall (_, _, _)
			   | Parsetree.Pr_exists (_, _, _)
			   | Parsetree.Pr_imply (_, _)
			   | Parsetree.Pr_or (_, _) | Parsetree.Pr_and (_, _)
			   | Parsetree.Pr_equiv (_, _) | Parsetree.Pr_not _
			   | Parsetree.Pr_paren _ ->
			       raise (Disapointed loc))
		       | Parsetree.BB_computational e -> e in
		     let pcm_name = 
		       mk_ast loc T_none (mk_id ctx (mk_vname name)) in
		     let pcm_params = List.map
			 (function (vn, _) ->
			   mk_ast loc T_none (mk_id ctx (mk_vname vn)))
			 args in
		     let pcm_type =
		       (List.map
			  (function
			      (_, None) -> None
			    | (_, Some t) -> Some (mk_type_expr ctx t))
			  args,
			match ret with
			  None -> None
			| Some t -> Some (mk_type_expr ctx t)) in
		     mk_ast loc () { b_name = pcm_name;
				     b_params = pcm_params;
				     b_type = pcm_type;
				     b_body = mk_expr ctx body })
		   bindings },
	     mk_expr ctx x))
  | Parsetree.E_record l ->
      mk_ast (E_record (List.map 
			  (function (l, r) ->
			    (mk_label_ident ctx l, mk_expr ctx r))
			  l))
  | Parsetree.E_record_access (x, id) ->
      mk_ast (E_record_access (mk_expr ctx x,
			       mk_label_ident ctx id))
  | Parsetree.E_record_with (x, l) ->
      mk_ast (E_record_with (mk_expr ctx x,
			     List.map
			       (function (id, y) ->
				 (mk_label_ident ctx id, mk_expr ctx y))
			       l))
  | Parsetree.E_tuple l ->
      mk_ast (E_tuple (List.map (mk_expr ctx) l))
  | Parsetree.E_external ee ->
      mk_ast (E_external (mk_external_expr ctx ee))
  | Parsetree.E_paren x ->
      mk_ast (E_paren (mk_expr ctx x));;

let mk_species_expr ctx se =
  mk_ast
    se.Parsetree.ast_loc
    T_none
    (let id = mk_ident ctx se.Parsetree.ast_desc.Parsetree.se_name in
    match se.Parsetree.ast_desc.Parsetree.se_params with
      [] -> E_var id
    | l -> E_app (mk_ast id.ast_loc T_none (E_var id), 
		  List.map 
		    (fun x ->
		      let Parsetree.SP e = x.Parsetree.ast_desc in
		      mk_expr ctx e)
		    l))
;;

let mk_species_param ctx sp =
  let loc = sp.Parsetree.ast_loc in
  mk_ast
    loc
    ()
    (match sp.Parsetree.ast_desc with
      Parsetree.SPT_in aid ->
	Sp_in (mk_ast
		 aid.Parsetree.ast_loc
		 T_none
		 (E_var (mk_ident ctx aid)))
    | Parsetree.SPT_is spe ->
	Sp_is (mk_species_expr ctx spe))
;;

let rec mk_rep_type_def ctx rtd =
  let mk_ast = mk_ast rtd.Parsetree.ast_loc () in
  match rtd.Parsetree.ast_desc with
    Parsetree.RTE_ident id ->
      mk_ast (TE_ident (mk_ident ctx id))
  | Parsetree.RTE_fun (a, b) ->
      mk_ast (TE_fun (mk_rep_type_def ctx a, mk_rep_type_def ctx b))
  | Parsetree.RTE_app (id, l) ->
      mk_ast (TE_app (mk_ident ctx id, List.map (mk_rep_type_def ctx)  l))
  | Parsetree.RTE_prod l ->
      mk_ast (TE_tuple (List.map (mk_rep_type_def ctx) l))
  | Parsetree.RTE_paren t ->
      mk_ast (TE_paren (mk_rep_type_def ctx t))
;;


let mk_sig_def ctx sd =
  let loc = sd.Parsetree.ast_loc in
  let name = sd.Parsetree.ast_desc.Parsetree.sig_name in
  let ty = sd.Parsetree.ast_desc.Parsetree.sig_type in
  let flog = sd.Parsetree.ast_desc.Parsetree.sig_logical in
  mk_ast loc ()
    { sig_name = mk_ast loc T_none (mk_id ctx (mk_vname name));
      sig_type = mk_type_expr ctx ty;
      sig_logical = mk_flag_logical ctx flog }
;;
	
let mk_property_def ctx pd =
  let loc = pd.Parsetree.ast_loc in
  let name = pd.Parsetree.ast_desc.Parsetree.prd_name in
  mk_ast loc () { pr_name = mk_ast loc T_none (mk_id ctx (mk_vname name));
		  pr_prop = () }
;;

let mk_species_field ctx sf =
  mk_ast
    sf.Parsetree.ast_loc
    ()
    (match sf.Parsetree.ast_desc with
      Parsetree.SF_rep rtd ->
	Sf_rep (mk_rep_type_def ctx rtd)
    | Parsetree.SF_sig sd ->
	Sf_sig (mk_sig_def ctx sd)
    | Parsetree.SF_let ld ->
	let loc = ld.Parsetree.ast_loc in
	let ldd = ld.Parsetree.ast_desc in
	let frec = mk_flag_rec ctx ldd.Parsetree.ld_rec in
	let flog = mk_flag_logical ctx ldd.Parsetree.ld_logical in
	let floc = mk_flag_local ctx ldd.Parsetree.ld_local in
	let bindings = ldd.Parsetree.ld_bindings in
	if flog then
	  Sf_logical_let 
	    (mk_ast loc ()
	       { ld_rec = frec;
		 ld_local = floc;
		 ld_logical = flog;
		 ld_bindings =
		 List.map
		   (fun x ->
		     let loc = x.Parsetree.ast_loc in
		     let name = x.Parsetree.ast_desc.Parsetree.b_name in
		     let args = x.Parsetree.ast_desc.Parsetree.b_params in
		     let ret = x.Parsetree.ast_desc.Parsetree.b_type in
		     let pcm_name = mk_ast loc T_none (mk_id ctx (mk_vname name)) in
		     let pcm_params = List.map
			 (function (vn, _) ->
			   mk_ast loc T_none (mk_id ctx (mk_vname vn)))
			 args in
		     let pcm_type =
		       (List.map
			  (function
			      (_, None) -> 
				None
			    | (_, Some t) ->
				Some (mk_type_expr ctx t))
			  args,
			match ret with
			  None -> None
			| Some t -> Some (mk_type_expr ctx t)) in
		     mk_ast loc () { b_name = pcm_name;
				     b_params = pcm_params;
				     b_type = pcm_type;
				     b_body = () })
		   bindings })
	else
	  Sf_let
	    (mk_ast loc ()
	       { ld_rec = frec;
		 ld_local = floc;
		 ld_logical = flog;
		 ld_bindings =
		 List.map
		   (fun x ->
		     let loc = x.Parsetree.ast_loc in
		     let name = x.Parsetree.ast_desc.Parsetree.b_name in
		     let args = x.Parsetree.ast_desc.Parsetree.b_params in
		     let ret = x.Parsetree.ast_desc.Parsetree.b_type in
		     let body =
		       match x.Parsetree.ast_desc.Parsetree.b_body with
			 Parsetree.BB_logical le ->
			   (match le.Parsetree.ast_desc with
			   | Parsetree.Pr_expr e -> e
			   | Parsetree.Pr_forall (_, _, _)
			   | Parsetree.Pr_exists (_, _, _)
			   | Parsetree.Pr_imply (_, _)
			   | Parsetree.Pr_or (_, _) | Parsetree.Pr_and (_, _)
			   | Parsetree.Pr_equiv (_, _) | Parsetree.Pr_not _
			   | Parsetree.Pr_paren _ ->
			       raise (Disapointed loc))
		       | Parsetree.BB_computational e -> e in
		     let pcm_name = 
		       mk_ast loc T_none (mk_id ctx (mk_vname name)) in
		     let pcm_params = List.map
			 (function (vn, _) ->
			   mk_ast loc T_none (mk_id ctx (mk_vname vn)))
			 args in
		     let pcm_type =
		       (List.map
			  (function
			      (_, None) -> None
			    | (_, Some t) -> Some (mk_type_expr ctx t))
			  args,
			match ret with
			  None -> None
			| Some t -> Some (mk_type_expr ctx t)) in
		     mk_ast loc () { b_name = pcm_name;
				     b_params = pcm_params;
				     b_type = pcm_type;
				     b_body = mk_expr ctx body })
		   bindings })
    | Parsetree.SF_property pd ->
	Sf_property (mk_property_def ctx pd)
    | Parsetree.SF_proof pd ->
	let loc = pd.Parsetree.ast_loc in
	let name = pd.Parsetree.ast_desc.Parsetree.pd_name in
	Sf_proof (mk_ast loc () 
		    { pf_name = mk_ast loc T_none (mk_id ctx (mk_vname name));
		      pf_proof = () })
    | Parsetree.SF_theorem td ->
	let loc = td.Parsetree.ast_loc in
	let name = td.Parsetree.ast_desc.Parsetree.th_name in
	let local = td.Parsetree.ast_desc.Parsetree.th_local in
	Sf_theorem (mk_ast loc ()
		      { t_name = mk_ast loc T_none (mk_id ctx (mk_vname name));
			t_local = mk_flag_local ctx local;
			t_prop = ();
			t_proof = () })
    | Parsetree.SF_termination_proof tp ->
	raise (Disapointed tp.Parsetree.ast_loc))
;;

let mk_simple_type_def_body ctx t =
  let loc = t.Parsetree.ast_loc in
  let mk_ast = mk_ast loc () in
  match t.Parsetree.ast_desc with
    Parsetree.STDB_alias te ->
      mk_ast (Tk_alias (mk_type_expr ctx te))
  | Parsetree.STDB_union l ->
      mk_ast (Tk_inductive (List.map 
			      (function (vn, lt) ->
				(Ast.mk_ast 
				   loc T_none (mk_id ctx (mk_vname vn)),
				 List.map (mk_type_expr ctx) lt))
			      l))
  | Parsetree.STDB_record l ->
      mk_ast (Tk_record (List.map
			   (function (vn, t) ->
			     (Ast.mk_ast loc T_none (mk_id ctx (mk_vname vn)),
			      mk_type_expr ctx t))
			   l))
      
;;

let mk_external_type_def_body ctx t =
  let mk_ast = mk_ast t.Parsetree.ast_loc () in
  let internal = t.Parsetree.ast_desc.Parsetree.etdb_internal in
  let extern = t.Parsetree.ast_desc.Parsetree.etdb_external in
  let binds = t.Parsetree.ast_desc.Parsetree.etdb_bindings in
  mk_ast (Tk_external
	    ((match internal with
	      None -> None
	    | Some ty -> Some (mk_simple_type_def_body ctx ty)),
	     mk_external_expr ctx extern,
	     List.map 
	       (mk_external_binding ctx) 
	       binds.Parsetree.ast_desc))
;;

let mk_type_def_body ctx tdb =
  match tdb.Parsetree.ast_desc with
    Parsetree.TDB_simple t ->
      mk_simple_type_def_body ctx t
  | Parsetree.TDB_external t ->
      mk_external_type_def_body ctx t
;;
  

let mk_type_def ctx td =
  let loc = td.Parsetree.ast_loc in
  mk_ast
    loc
    ()
    (let vn = td.Parsetree.ast_desc.Parsetree.td_name in
    let params = td.Parsetree.ast_desc.Parsetree.td_params in
    let body = td.Parsetree.ast_desc.Parsetree.td_body in
    { type_name = mk_ast loc T_none (mk_id ctx (mk_vname vn));
      type_params = List.map
	(fun t ->
	  mk_ast loc T_none (mk_id ctx ~file:"" (mk_vname t)))
	params;
      type_body = mk_type_def_body ctx body })
;;



let mk_theorem_def ctx td =
  let loc = td.Parsetree.ast_loc in
  let name = td.Parsetree.ast_desc.Parsetree.th_name in
  let floc = mk_flag_local ctx td.Parsetree.ast_desc.Parsetree.th_local in
  mk_ast
    loc
    ()
    { t_local = floc;
      t_name = mk_ast loc T_none (mk_id ctx (mk_vname name));
      t_prop = ();
      t_proof = () }
;;

let mk_phrase ctx ph =
  mk_ast 
    ph.Parsetree.ast_loc
    ()
    (match ph.Parsetree.ast_desc with
      Parsetree.Ph_use id -> Ph_use id
    | Parsetree.Ph_open id -> Ph_open id
    | Parsetree.Ph_coq_require id -> Ph_require ("coq", id)
    | Parsetree.Ph_species sd -> 
	let loc = sd.Parsetree.ast_loc in
	let name = sd.Parsetree.ast_desc.Parsetree.sd_name in
	let pcm_name = mk_ast loc T_none (mk_id ctx (mk_vname name)) in
	let params = sd.Parsetree.ast_desc.Parsetree.sd_params in
	let pcm_params = List.map 
	    (function (vn, ty) ->
	      (mk_ast loc T_none (mk_id ctx ~file:"" (mk_vname vn)),
	       mk_species_param ctx ty))
	    params in
	let inh =
	  sd.Parsetree.ast_desc.Parsetree.sd_inherits.Parsetree.ast_desc in
	let pcm_inh = List.map (mk_species_expr ctx) inh in
	let body = sd.Parsetree.ast_desc.Parsetree.sd_fields in
	ctx.spc_id <- pcm_name.ast_desc.i_name;
	let pcm_body = List.map (mk_species_field ctx) body in
	ctx.spc_id <- "";
	Ph_species (mk_ast ph.Parsetree.ast_loc ()
		      { s_name = pcm_name;
			s_params = List.map fst pcm_params;
			s_spec = List.map snd pcm_params;
			s_inh = pcm_inh;
			s_fields = pcm_body })
    | Parsetree.Ph_collection cd ->
	Ph_collection (mk_ast
			 cd.Parsetree.ast_loc
			 T_none
			 (mk_id ctx 
			    (mk_vname cd.Parsetree.ast_desc.Parsetree.cd_name)),
		       mk_species_expr
			 ctx cd.Parsetree.ast_desc.Parsetree.cd_body)
    | Parsetree.Ph_type td ->
	Ph_type (mk_type_def ctx td)
    | Parsetree.Ph_let ld ->
	let loc = ld.Parsetree.ast_loc in
	let ldd = ld.Parsetree.ast_desc in
	let frec = mk_flag_rec ctx ldd.Parsetree.ld_rec in
	let flog = mk_flag_logical ctx ldd.Parsetree.ld_logical in
	let floc = mk_flag_local ctx ldd.Parsetree.ld_local in
	let bindings = ldd.Parsetree.ld_bindings in
	if flog then
	  Ph_logical_let 
	    (mk_ast loc ()
	       { ld_rec = frec;
		 ld_local = floc;
		 ld_logical = flog;
		 ld_bindings =
		 List.map
		   (fun x ->
		     let loc = x.Parsetree.ast_loc in
		     let name = x.Parsetree.ast_desc.Parsetree.b_name in
		     let args = x.Parsetree.ast_desc.Parsetree.b_params in
		     let ret = x.Parsetree.ast_desc.Parsetree.b_type in
		     let _ =
		       match x.Parsetree.ast_desc.Parsetree.b_body with
			 Parsetree.BB_logical _ -> ()
		       | Parsetree.BB_computational _ -> assert false in
		     let pcm_name = mk_ast loc T_none (mk_id ctx (mk_vname name)) in
		     let pcm_params = List.map
			 (function (vn, _) ->
			   mk_ast loc T_none (mk_id ctx (mk_vname vn)))
			 args in
		     let pcm_type =
		       (List.map
			  (function
			      (_, None) -> 
				None
			    | (_, Some t) ->
				Some (mk_type_expr ctx t))
			  args,
			match ret with
			  None -> None
			| Some t -> Some (mk_type_expr ctx t)) in
		     mk_ast loc () { b_name = pcm_name;
				     b_params = pcm_params;
				     b_type = pcm_type;
				     b_body = () })
		   bindings })
	else
	  Ph_let
	    (mk_ast loc ()
	       { ld_rec = frec;
		 ld_local = floc;
		 ld_logical = flog;
		 ld_bindings =
		 List.map
		   (fun x ->
		     let loc = x.Parsetree.ast_loc in
		     let name = x.Parsetree.ast_desc.Parsetree.b_name in
		     let args = x.Parsetree.ast_desc.Parsetree.b_params in
		     let ret = x.Parsetree.ast_desc.Parsetree.b_type in
		     let body =
		       match x.Parsetree.ast_desc.Parsetree.b_body with
			 Parsetree.BB_logical le ->
			   (match le.Parsetree.ast_desc with
			   | Parsetree.Pr_expr e -> e
			   | Parsetree.Pr_forall (_, _, _)
			   | Parsetree.Pr_exists (_, _, _)
			   | Parsetree.Pr_imply (_, _)
			   | Parsetree.Pr_or (_, _) | Parsetree.Pr_and (_, _)
			   | Parsetree.Pr_equiv (_, _) | Parsetree.Pr_not _
			   | Parsetree.Pr_paren _ ->
			       raise (Disapointed loc))
		       | Parsetree.BB_computational e -> e in
		     let pcm_name =
		       mk_ast loc T_none (mk_id ctx (mk_vname name)) in
		     let pcm_params = List.map
			 (function (vn, _) ->
			   mk_ast loc T_none (mk_id ctx (mk_vname vn)))
			 args in
		     let pcm_type =
		       (List.map
			  (function
			      (_, None) -> None
			    | (_, Some t) -> Some (mk_type_expr ctx t))
			  args,
			match ret with
			  None -> None
			| Some t -> Some (mk_type_expr ctx t)) in
		     mk_ast loc () { b_name = pcm_name;
				     b_params = pcm_params;
				     b_type = pcm_type;
				     b_body = mk_expr ctx body })
		   bindings })
    | Parsetree.Ph_theorem td ->
	Ph_theorem (mk_theorem_def ctx td)
    | Parsetree.Ph_expr e ->
	Ph_expr (mk_expr ctx e))
	    
;;

let mk_file id (f : Parsetree.file) =
  let ctx = empty id in
  let Parsetree.File l = f.Parsetree.ast_desc in
  mk_ast
    f.Parsetree.ast_loc
    T_none
    { file_name = id;
      file_body = List.map (mk_phrase ctx) l }
;;
