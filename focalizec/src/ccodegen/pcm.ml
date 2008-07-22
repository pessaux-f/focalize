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

type type_expr = (type_expr_desc, unit) ast
and type_expr_desc =
  | TE_var of int
  | TE_ident of (unit ident)
  | TE_fun of type_expr * type_expr
  | TE_app of (unit ident) * type_expr list
  | TE_tuple of type_expr list
  | TE_self
  | TE_prop
  | TE_paren of type_expr

and 'a ident = (ident_desc, 'a) ast
and ident_desc =
    { i_file : string;
      i_spc : string;
      i_name : string }
;;

type constant = (constant_desc, type_expr) ast
and constant_desc =
  | C_int of string
  | C_float of string
  | C_bool of string
  | C_string of string
  | C_char of char
;;

type pattern = (pattern_desc, type_expr) ast
and pattern_desc =
  | P_const of constant
  | P_var of (type_expr ident)
  | P_as of pattern * (type_expr ident)
  | P_wild
  | P_constr of (unit ident) * pattern list
  | P_record of ((type_expr ident) * pattern) list
  | P_tuple of pattern list
  | P_paren of pattern
;;

type flags = (flags_desc, unit) ast
and flags_desc =
    { f_rec : bool;
      f_local : bool }
;;

type 'a binding = ('a binding_desc, unit) ast
and 'a binding_desc = ((type_expr ident) * (type_expr ident) list * 'a)
;;

type external_expr = (external_expr_desc, unit) ast
and external_expr_desc = (string * string) list
;;

type expr = (expr_desc, type_expr) ast
and expr_desc =
  | E_self
  | E_constant of constant
  | E_fun of (type_expr ident) list * expr
  | E_var of (type_expr ident)
  | E_app of expr * expr list
  | E_constr of (unit ident) * expr list
  | E_match of expr * (pattern * expr) list
  | E_if of expr * expr * expr
  | E_let of flags * (expr binding) list * expr
  | E_logical_let of flags * (logical_expr binding) list * expr
  | E_record of ((type_expr ident) * expr) list
  | E_record_access of expr * (type_expr ident)
  | E_record_with of expr * ((type_expr ident) * expr) list
  | E_tuple of expr list
  | E_external of external_expr
  | E_paren of expr

and logical_expr = (logical_expr_desc, unit) ast
and logical_expr_desc =
  | Le_forall of (type_expr ident) list * logical_expr
  | Le_exists of (type_expr ident) list * logical_expr
  | Le_imply of logical_expr * logical_expr
  | Le_or of logical_expr * logical_expr
  | Le_and of logical_expr * logical_expr
  | Le_equiv of logical_expr * logical_expr
  | Le_not of logical_expr
  | Le_expr of expr
  | Le_paren of logical_expr
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

type history =
    { h_initial : (unit ident) }
;;

type proof = (proof_desc, unit) ast
and proof_desc = unit (* [julius:] todo. *)
;;

type species_field = (species_field_desc, unit) ast
and species_field_desc =
  | Sf_sig of (type_expr ident) * history
  | Sf_let of flags * ((expr * history) binding) list
  | Sf_logical_let of flags * ((logical_expr * history) binding) list
  | Sf_theorem of (unit ident) * logical_expr * proof * history 
  | Sf_property of (unit ident) * logical_expr * history
;;

type external_binding = (external_binding_desc, unit) ast
and external_binding_desc = ((unit ident) * external_expr)
;;

type type_kind = (type_kind_desc, unit) ast
and type_kind_desc =
  | Tk_external of external_expr * external_binding list
  | Tk_inductive of ((unit ident) * type_expr) list
  | Tk_record of (unit ident * type_expr) list
  | Tk_alias of type_expr
;;

type type_def = (type_def_desc, unit) ast
and type_def_desc =
    { type_name : (unit ident);
      type_params : type_expr list;
      type_body : type_kind }
;;

type phrase = (phrase_desc, unit) ast
and phrase_desc =
  | Ph_open of string
  | Ph_use of string
  | Ph_species of (unit ident) * species_field list
  | Ph_collection of (unit ident) * species_field list
  | Ph_type of type_def
  | Ph_let of flags * (expr binding) list
  | Ph_logical_let of flags * (logical_expr binding) list
  | Ph_theorem of (unit ident) * logical_expr * proof
  | Ph_expr of expr
;;

type file =
    { file_name : string;
      mutable file_body : phrase list }
;;


(* The conversion part *)
type context =
    { mutable file_id : string;
      mutable spc_id : string; }
;;

let empty id =
  { file_id = id;
    spc_id = "" }
;;

let mk_vname = Parsetree_utils.vname_as_string_with_operators_expanded
;;

let mk_ident env ?(file = env.file_id) ?(spc = env.spc_id) id =
  { i_file = file;
    i_spc = spc;
    i_name = id }
;;

let rec mk_local_type_expr env (t : Types.local_type) =
  match t with
  | Types.Lt_var i ->
      mk_uast (TE_var i)
	
  | Types.Lt_fun (a, b) ->
      mk_uast (TE_fun (mk_local_type_expr env a,
		       mk_local_type_expr env b))

  | Types.Lt_tuple l ->
      mk_uast (TE_tuple (List.map (mk_local_type_expr env) l))

  | Types.Lt_constr ((fid, nid), []) ->
      mk_uast (TE_ident (mk_uast (mk_ident env ~file:fid nid)))

  | Types.Lt_constr ((fid, nid), args) ->
      mk_uast (TE_app (mk_uast (mk_ident env ~file:fid nid),
		       List.map (mk_local_type_expr env) args))

  | Types.Lt_self ->
      mk_uast TE_self

  | Types.Lt_species (fid, nid) ->
      mk_uast (TE_ident (mk_uast (mk_ident env ~file:fid nid)))
;;

let mk_type_simple env ts =
  mk_local_type_expr env (Types.type_simple_to_local_type ts)
;;

let mk_type_scheme env ts =
  mk_local_type_expr env (Types.type_scheme_to_local_type ts)
;;

let mk_history (h : Env.from_history) =
  let (fid, nid) = h.Env.fh_initial_apparition in
  { h_initial = { ast_desc = { i_file = fid;
			       i_spc = "";
			       i_name = mk_vname nid };
		  ast_type = ();
		  ast_loc = None } }
;;

let rec build_typed_args env args types = 
  match (args, types.ast_desc) with
  | ([], _) -> []
  | (h::q, TE_fun (t1, t2)) ->
      (mk_ast t1 (mk_ident env (mk_vname h))) :: (build_typed_args env q t2)
  | (_, TE_ident _) | (_, TE_app _)
  | (_, TE_tuple _) | (_, TE_self)
  | (_, TE_paren _) | (_, TE_prop)
  | (_, TE_var _) -> assert false
;;

let mk_external_expr (ee : Parsetree.external_expr) =
  { ast_desc = List.map (function
      (Parsetree.EL_Caml, code) -> ("ocaml", code)
    | (Parsetree.EL_Coq, code) -> ("coq", code)
    | (Parsetree.EL_external l, code) -> (l, code)) ee.Parsetree.ast_desc;
    ast_type = ();
    ast_loc = Some ee.Parsetree.ast_loc }
;;

let mk_external_bindings env (eb : Parsetree.external_bindings) =
  List.map (fun x ->
    let (a, b) = x.Parsetree.ast_desc in
    { ast_desc = ({ ast_desc = { i_file = env.file_id;
				 i_spc = "";
				 i_name = mk_vname a };
		    ast_type = ();
		    ast_loc = None },
		  mk_external_expr b);
      ast_type = ();
      ast_loc = Some x.Parsetree.ast_loc }) eb.Parsetree.ast_desc
;;

exception MissingNodeTypeError of Location.t;;

let mk_anti_type env (loc : Location.t)
    (t : Parsetree.ast_node_type_information) =
  match t with
  | Parsetree.ANTI_type ts -> mk_type_simple env ts
  | Parsetree.ANTI_scheme ts -> mk_type_scheme env ts
  | Parsetree.ANTI_non_relevant
  | Parsetree.ANTI_none ->
      raise (MissingNodeTypeError loc)
;;

let mk_constr_id env cid =
  let loc = cid.Parsetree.ast_loc in
(*   let ty = mk_anti_type env loc cid.Parsetree.ast_type in *)
  mk_uast ~loc:(Some loc)
    (match cid.Parsetree.ast_desc with
    | Parsetree.CI (Parsetree.Vname vn) ->
	mk_ident env (mk_vname vn)
    | Parsetree.CI (Parsetree.Qualified (f, vn)) ->
	mk_ident env ~file:f (mk_vname vn))
;;

let mk_type_kind env (t : Env.TypeInformation.type_kind) alias =
  mk_uast 
    (match t with
    | Env.TypeInformation.TK_abstract ->
	Tk_alias (mk_type_scheme env alias)
	  
    | Env.TypeInformation.TK_external (ee, eb) ->
	Tk_external (mk_external_expr ee,
		     mk_external_bindings env eb)
	  
    | Env.TypeInformation.TK_variant l ->
	Tk_inductive (List.map (function (cn, _, ts) ->
	  (mk_uast (mk_ident env (mk_vname cn)), mk_type_scheme env ts)) l)

    | Env.TypeInformation.TK_record l ->
	(* [julius:] don't know what to do with mutability. *)
	Tk_record (List.map (function (a, _, b) ->
	  (mk_uast (mk_ident env (mk_vname a)), mk_type_scheme env b)) l)
)
;;

let mk_constant env (c : Parsetree.constant) =
  mk_ast
    ~loc:(Some c.Parsetree.ast_loc)
    (mk_anti_type env c.Parsetree.ast_loc c.Parsetree.ast_type)
    (match c.Parsetree.ast_desc with
    | Parsetree.C_int s -> C_int s
    | Parsetree.C_float s -> C_float s
    | Parsetree.C_bool s -> C_bool s
    | Parsetree.C_string s -> C_string s
    | Parsetree.C_char ch -> C_char ch)
;;

let mk_label env (cid : Parsetree.label_ident) =
  let loc = cid.Parsetree.ast_loc in
  let ty = mk_anti_type env loc cid.Parsetree.ast_type in
  mk_ast ~loc:(Some loc) ty
    (match cid.Parsetree.ast_desc with
    | Parsetree.LI (Parsetree.Vname vn) ->
	mk_ident env (mk_vname vn)
    | Parsetree.LI (Parsetree.Qualified (f, vn)) ->
	mk_ident env ~file:f (mk_vname vn))
;;

let rec mk_pattern env (p : Parsetree.pattern) =
  let loc = p.Parsetree.ast_loc in
  let ty = mk_anti_type env loc p.Parsetree.ast_type in
  mk_ast ~loc:(Some loc) ty
    (match p.Parsetree.ast_desc with
    | Parsetree.P_const c ->
	P_const (mk_constant env c)
    | Parsetree.P_var vn ->
	P_var (mk_ast ~loc:(Some loc) ty (mk_ident env (mk_vname vn)))
    | Parsetree.P_as (p', vn) ->
	P_as (mk_pattern env p', mk_ast ty (mk_ident env (mk_vname vn)))
    | Parsetree.P_wild ->
	P_wild
    | Parsetree.P_constr (cid, l) ->
	P_constr (mk_constr_id env cid, List.map (mk_pattern env) l)
    | Parsetree.P_record l ->
	P_record (List.map (fun (a, b) -> (mk_label env a, mk_pattern env b)) l)
    | Parsetree.P_tuple l ->
	P_tuple (List.map (mk_pattern env) l)
    | Parsetree.P_paren p' ->
	P_paren (mk_pattern env p'))
;;

let mk_unit_ident env (id : Parsetree.ident) =
  mk_uast
    ~loc:(Some id.Parsetree.ast_loc)
    (match id.Parsetree.ast_desc with
    | Parsetree.I_local vn ->
	mk_ident env (mk_vname vn)
    | Parsetree.I_global (Parsetree.Vname vn) ->
	mk_ident env (mk_vname vn)
    | Parsetree.I_global (Parsetree.Qualified (f, vn)) ->
	mk_ident env ~file:f (mk_vname vn))
;;

let rec mk_type_expr env (t : Parsetree.type_expr) =
  mk_uast
    ~loc:(Some t.Parsetree.ast_loc)
    (match t.Parsetree.ast_desc with
    | Parsetree.TE_ident id ->
	TE_ident (mk_unit_ident env id)
    | Parsetree.TE_self ->
	TE_self
    | Parsetree.TE_prop ->
	TE_prop
    | Parsetree.TE_fun (l, r) ->
	TE_fun (mk_type_expr env l, mk_type_expr env r)
    | Parsetree.TE_app (id, l) ->
	TE_app (mk_unit_ident env id, List.map (mk_type_expr env) l)
    | Parsetree.TE_prod l ->
	TE_tuple (List.map (mk_type_expr env) l)
    | Parsetree.TE_paren t' ->
	TE_paren (mk_type_expr env t'))
;;

let rec mk_expr env (e : Parsetree.expr) =
  let ty = mk_anti_type env e.Parsetree.ast_loc e.Parsetree.ast_type in
  let loc = Some e.Parsetree.ast_loc in
  mk_ast ~loc:loc ty
    (match e.Parsetree.ast_desc with
    | Parsetree.E_self -> 
	E_self

    | Parsetree.E_const c ->
	E_constant (mk_constant env c)

    | Parsetree.E_fun (a, b) ->
	E_fun (build_typed_args env a ty, mk_expr env b)

    | Parsetree.E_var id ->
	begin match id.Parsetree.ast_desc with
	| Parsetree.EI_local vn ->
	    E_var (mk_ast ~loc:loc ty (mk_ident env (mk_vname vn)))
	| Parsetree.EI_global (Parsetree.Vname vn) ->
	    E_var (mk_ast ~loc:loc ty (mk_ident env (mk_vname vn)))
	| Parsetree.EI_global (Parsetree.Qualified (f, vn)) ->
	    E_var (mk_ast ~loc:loc ty (mk_ident env ~file:f (mk_vname vn)))
	| Parsetree.EI_method (None, vn) ->
	    E_var (mk_ast ~loc:loc ty (mk_ident env (mk_vname vn)))
	| Parsetree.EI_method (Some (Parsetree.Vname n), vn) ->
	    E_var (mk_ast ~loc:loc ty 
		     (mk_ident env ~spc:(mk_vname n) (mk_vname vn)))
	| Parsetree.EI_method (Some (Parsetree.Qualified (f, n)), vn) ->
	    E_var (mk_ast ~loc:loc ty
		     (mk_ident env ~file:f ~spc:(mk_vname n) (mk_vname vn)))
	end

    | Parsetree.E_app (a, l) ->
	E_app (mk_expr env a, List.map (mk_expr env) l)

    | Parsetree.E_constr (q, l) ->
	E_constr (mk_constr_id env q, List.map (mk_expr env) l)

    | Parsetree.E_match (e', l) ->
	E_match (mk_expr env e',
		 List.map (fun (a, b) ->
		   (mk_pattern env a, mk_expr env b)) l)

    | Parsetree.E_if (a, b, c) ->
	E_if (mk_expr env a, mk_expr env b, mk_expr env c)

    | Parsetree.E_let (ld, e') ->
	let logical_flag = match ld.Parsetree.ast_desc.Parsetree.ld_logical with
	| Parsetree.LF_no_logical -> false
	| Parsetree.LF_logical -> true in
	let rec_flag = match ld.Parsetree.ast_desc.Parsetree.ld_rec with
	| Parsetree.RF_no_rec -> false
	| Parsetree.RF_rec -> true in
	let local_flag = match ld.Parsetree.ast_desc.Parsetree.ld_local with
	| Parsetree.LF_no_local -> false
	| Parsetree.LF_local -> true in
	let body = ld.Parsetree.ast_desc.Parsetree.ld_bindings in
	if not logical_flag then
	  E_let (mk_uast { f_rec = rec_flag;
			   f_local = local_flag },
		 List.map
		   (fun x -> mk_binding_expr env
		       (x, mk_anti_type env x.Parsetree.ast_loc
			  x.Parsetree.ast_type))
		   body,
		 mk_expr env e')
	else
	  E_logical_let (mk_uast { f_rec = rec_flag;
				   f_local = local_flag },
			 List.map
			   (fun x -> mk_binding_logical env
			       (x, mk_anti_type env x.Parsetree.ast_loc
				  x.Parsetree.ast_type))
			   body, 
			 mk_expr env e')

    | Parsetree.E_record l ->
	E_record (List.map (fun (a, b) -> (mk_label env a, mk_expr env b)) l)

    | Parsetree.E_record_access (a, id) ->
	E_record_access (mk_expr env a, mk_label env id)

    | Parsetree.E_record_with (a, l) ->
	E_record_with (mk_expr env a,
		       List.map (fun (x, y) ->
			 (mk_label env x, mk_expr env y)) l)

    | Parsetree.E_tuple l ->
	E_tuple (List.map (mk_expr env) l)

    | Parsetree.E_external ee ->
	E_external (mk_external_expr ee)

    | Parsetree.E_paren e' ->
	E_paren (mk_expr env e'))

and mk_binding_expr env ((b, ty) : Parsetree.binding * type_expr) =
  let vnid = b.Parsetree.ast_desc.Parsetree.b_name in
  let args =
    List.map (function (a, _) -> a) b.Parsetree.ast_desc.Parsetree.b_params in
  let body = match b.Parsetree.ast_desc.Parsetree.b_body with
  | Parsetree.BB_computational e -> e
  | Parsetree.BB_logical _ -> assert false in
  mk_uast
    ~loc:(Some b.Parsetree.ast_loc)
    (mk_ast ty (mk_ident env (mk_vname vnid)),
     build_typed_args env args ty,
     mk_expr env body)

and mk_binding_logical env ((b, ty) : Parsetree.binding * type_expr) =
  let vnid = b.Parsetree.ast_desc.Parsetree.b_name in
  let loc = b.Parsetree.ast_loc in
  let args =
    List.map (function (a, _) -> a) b.Parsetree.ast_desc.Parsetree.b_params in
  let body = match b.Parsetree.ast_desc.Parsetree.b_body with
  | Parsetree.BB_computational _ ->
      Format.printf "### %a ###@." Location.pp_location loc;
      assert false
  | Parsetree.BB_logical e -> e in
  mk_uast
    ~loc:(Some loc)
    (mk_ast ty (mk_ident env (mk_vname vnid)),
     build_typed_args env args ty,
     mk_logical_expr env body)

and mk_logical_expr env (l : Parsetree.logical_expr) =
  mk_uast 
    ~loc:(Some l.Parsetree.ast_loc)
    (match l.Parsetree.ast_desc with
    | Parsetree.Pr_forall (args, te, lexp) ->
	Le_forall (List.map
		     (fun x ->
		       mk_ast
			 (mk_type_expr env te)
			 (mk_ident env (mk_vname x)))
		     args,
		   mk_logical_expr env lexp)
    
    | Parsetree.Pr_exists (args, te, lexp) ->
	Le_exists (List.map 
		     (fun x ->
		       mk_ast
			 (mk_type_expr env te)
			 (mk_ident env (mk_vname x)))
		     args,
		   mk_logical_expr env lexp)
	  
    | Parsetree.Pr_imply (a, b) ->
	Le_imply (mk_logical_expr env a, mk_logical_expr env b)

    | Parsetree.Pr_or (a, b) ->
	Le_or (mk_logical_expr env a, mk_logical_expr env b)

    | Parsetree.Pr_and (a, b) ->
	Le_and (mk_logical_expr env a, mk_logical_expr env b)

    | Parsetree.Pr_equiv (a, b) ->
	Le_equiv (mk_logical_expr env a, mk_logical_expr env b)
	  
    | Parsetree.Pr_not e ->
	Le_not (mk_logical_expr env e)

    | Parsetree.Pr_paren e ->
	Le_paren (mk_logical_expr env e)

    | Parsetree.Pr_expr e ->
	Le_expr (mk_expr env e))
;;

let mk_proof _ _ =
  mk_uast ()
;;

let rec mk_species_fields env (l : Env.TypeInformation.species_field list) =
  match l with
  | [] -> []
  
  | (Env.TypeInformation.SF_sig ((hist, name, ts))) :: l' ->
      (mk_uast (Sf_sig (mk_ast
			  (mk_type_scheme env ts)
			  (mk_ident env (mk_vname name)),
			mk_history hist)))
      :: (mk_species_fields env l')

  | (Env.TypeInformation.SF_let ((hist, vn, args, ts, b, _, log))) :: l' ->
      let flags = mk_uast { f_rec = false;
			    f_local = false } in
      let ty = mk_type_scheme env ts in
      let id = mk_ast ty (mk_ident env (mk_vname vn)) in
      (mk_uast 
	 (begin match log with
	 | Parsetree.LF_logical ->
	     Sf_logical_let (flags,
			     [mk_uast (id,
				       build_typed_args env args ty,
				       (begin match b with
				       | Parsetree.BB_logical e ->
					   mk_logical_expr env e
				       | Parsetree.BB_computational _ ->
					   assert false
				       end,
					mk_history hist))])
	 | Parsetree.LF_no_logical ->
	     Sf_let (flags,
		     [mk_uast (id,
			       build_typed_args env args ty,
			       (begin match b with
			       | Parsetree.BB_computational e ->
				   mk_expr env e
			       | Parsetree.BB_logical _ ->
				   assert false
			       end,
				mk_history hist))])
	 end))
      :: (mk_species_fields env l')

  | (Env.TypeInformation.SF_let_rec x) :: l' ->
      let flags = mk_uast { f_rec = true;
			    f_local = false } in
      let (llog, llexp) = (ref [], ref []) in
      List.iter (function
	  (hist, vn, args, ts, b, _, log) ->
	    let ty = mk_type_scheme env ts in
	    let id = mk_ast ty (mk_ident env (mk_vname vn)) in
	    match log with
	    | Parsetree.LF_logical ->
		llog := !llog @ [mk_uast (id,
					 build_typed_args env args ty,
					 (begin match b with
					 | Parsetree.BB_logical e ->
					     mk_logical_expr env e
					 | Parsetree.BB_computational _ ->
					     assert false
					 end,
					  mk_history hist))]
	    | Parsetree.LF_no_logical ->
		llexp := !llexp @ [mk_uast (id,
					   build_typed_args env args ty,
					   (begin match b with
					   | Parsetree.BB_computational e ->
					       mk_expr env e
					   | Parsetree.BB_logical _ ->
					       assert false
					   end,
					    mk_history hist))]) x;
      (match (!llog, !llexp) with
      |	([], _) -> [mk_uast (Sf_let (flags, !llexp))]
      |	(_, []) -> [mk_uast (Sf_logical_let (flags, !llog))]
      |	_ -> assert false)
      @ (mk_species_fields env l')

  | (Env.TypeInformation.SF_theorem ((hist, vn, _, log, prf, _))) :: l' ->
      (mk_uast (Sf_theorem (mk_uast (mk_ident env (mk_vname vn)),
			    mk_logical_expr env log,
			    mk_proof env prf,
			    mk_history hist)))
      :: (mk_species_fields env l')

  | (Env.TypeInformation.SF_property ((hist, vn, _, log, _))) :: l' ->
      (mk_uast (Sf_property (mk_uast (mk_ident env (mk_vname vn)),
			     mk_logical_expr env log,
			     mk_history hist)))
      :: (mk_species_fields env l')
;;

let rec mk_phrases env (l : Infer.please_compile_me list) =
  match l with
  | [] -> []

  | Infer.PCM_no_matter :: l'->
      mk_phrases env l'

  | (Infer.PCM_open (loc, id)) :: l' ->
      { ast_desc = Ph_open id;
	ast_type = ();
	ast_loc = Some loc }
      :: (mk_phrases env l')

  | (Infer.PCM_coq_require id) :: l' ->
      (* [julius:] seems redunctant with PCM_no_matter that originaly meant *)
      (* "use". *)
      { ast_desc = Ph_use id;
	ast_type = ();
	ast_loc = None }
      :: (mk_phrases env l')

  | (Infer.PCM_species (_, _ , _)) :: l' ->
      (* [julius:] no need to traduct species for now since we do not *)
      (* translate them. *)
      mk_phrases env l'

  | (Infer.PCM_collection (coldef, spcdesc, _)) :: l' ->
      let loc = coldef.Parsetree.ast_loc in
      let id = mk_vname coldef.Parsetree.ast_desc.Parsetree.cd_name in
      let fields = spcdesc.Env.TypeInformation.spe_sig_methods in
      { ast_desc =
	Ph_collection ({ ast_desc = { i_file = env.file_id;
				      i_spc = "";
				      i_name = id };
			 ast_type = ();
			 ast_loc = None },
		       mk_species_fields (env.spc_id <- id; env) fields);
	ast_type = ();
	ast_loc = Some loc }
      :: (mk_phrases (env.spc_id <- ""; env) l')

  | (Infer.PCM_type (vnid, info)) :: l' ->
      let args = info.Env.TypeInformation.type_params in
      let type_id = { ast_desc = { i_file = env.file_id;
				   i_spc = "";
				   i_name = mk_vname vnid };
		      ast_type = ();
		      ast_loc = None } in
      let body = info.Env.TypeInformation.type_kind in
      let alias = info.Env.TypeInformation.type_identity in
      let tk = { type_name = type_id;
		 type_params = List.map (mk_type_simple env) args;
		 type_body = mk_type_kind env body alias } in
      (mk_uast (Ph_type (mk_uast tk)))
      :: (mk_phrases env l')

  | (Infer.PCM_let_def (ld, tys)) :: l' ->
      let logical_flag = match ld.Parsetree.ast_desc.Parsetree.ld_logical with
      |	Parsetree.LF_no_logical -> false
      |	Parsetree.LF_logical -> true in
      let rec_flag = match ld.Parsetree.ast_desc.Parsetree.ld_rec with
      | Parsetree.RF_no_rec -> false
      | Parsetree.RF_rec -> true in
      let local_flag = match ld.Parsetree.ast_desc.Parsetree.ld_local with
      | Parsetree.LF_no_local -> false
      | Parsetree.LF_local -> true in
      let body = ld.Parsetree.ast_desc.Parsetree.ld_bindings in
      let loc = ld.Parsetree.ast_loc in
      (mk_uast ~loc:(Some loc)
	 (if not logical_flag then
	   Ph_let (mk_uast { f_rec = rec_flag;
			     f_local = local_flag },
		   List.map
		     (mk_binding_expr env)
		     (List.combine body (List.map (mk_type_scheme env) tys)))
	 else
	   Ph_logical_let (mk_uast { f_rec = rec_flag;
				     f_local = local_flag },
			   List.map
			     (mk_binding_logical env)
			     (List.combine body 
				(List.map (mk_type_scheme env) tys)))))
      :: (mk_phrases env l')

  | (Infer.PCM_theorem td) :: l' ->
      let vnid = td.Parsetree.ast_desc.Parsetree.th_name in
      let lexp = td.Parsetree.ast_desc.Parsetree.th_stmt in
      let prf = td.Parsetree.ast_desc.Parsetree.th_proof in
      { ast_desc = Ph_theorem ({ ast_desc = { i_file = env.file_id;
					      i_spc = "";
					      i_name = mk_vname vnid };
				 ast_type = ();
				 ast_loc = None },
			       mk_logical_expr env lexp,
			       mk_proof env prf);
	ast_type = ();
	ast_loc = Some td.Parsetree.ast_loc } :: (mk_phrases env l')

  | (Infer.PCM_expr e) :: l' ->
      { ast_desc = Ph_expr (mk_expr env e);
	ast_type = ();
	ast_loc = Some e.Parsetree.ast_loc } :: (mk_phrases env l')
;;

let mk_file id (body : Infer.please_compile_me list) =
  let env = empty id in
  { file_name = id;
    file_body = mk_phrases env body }
;;
