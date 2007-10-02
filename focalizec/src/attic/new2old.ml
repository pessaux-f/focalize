(* ********************************************************************* *)
(*  [Exc] : No_mapping of string                                         *)
(** [Descr] : Exception used to pinpoint the fact that a construction in
              the new AST cannot be translated into the old AST.

    [Rem] : Exported outside this module.                                *)
(* ********************************************************************* *)
exception No_mapping of string ;;



(* Debug purpose. Must disapear. *)
exception Dont_know_what_to_do of string ;;



(* ****************************************************************** *)
(*  [Fun] gen_syntactic_type_variable : unit -> string                *)
(** [Descr] : Generate a fresh type variable name. According to what
              was dont in the old AST, names are prefixed by the
              string "parse__". Next, follows the string version of
              a unique integer that gets incremented each time this
              function is called.
              This function is espacially useful when translating a
              construct from the new AST with an optional [type_expr]
              into a construct of the old AST where the [typ] is NOT
              optional. To overcome this problem, we then generate 
              type variable when the option is None.
    [Rem] : Exported outside this module.                             *)
(* ****************************************************************** *)
let gen_syntactic_type_variable =
  let counter = ref 0 in
  (fun () ->
    let tmp = !counter in
    incr counter ;
    "parse__" ^ (string_of_int tmp))
;;



(* *************************************************************** *)
(*  [Fun] process_generic_ast_desc :                               *)
(*         ('a -> 'b) -> ('a, 'c) Parsetree.generic_ast -> 'b      *)
(** [Descr] : Wrapper to apply processing only on the 'desc' field
              of a generic_ast. Ignores all other fields.

    [Rem] : Not exported outside this module.                      *)
(* *************************************************************** *)
let process_generic_ast_desc f ga = f ga.Parsetree.ast_desc ;;



(* ****************************************************** *)
(*  [Fun] downgrade_vname : Parsetree.vname -> string     *)
(** [Descr] : Translates a [vname] into a regular string.

    [Rem] : Not exported outside this module.             *)
(* ****************************************************** *)
let downgrade_vname = function
  | Parsetree.Vlident s | Parsetree.Vuident s | Parsetree.Vpident s
  | Parsetree.Viident s | Parsetree.Vqident s -> s
;;



(* ********************************************************************** *)
(*  [Fun] downgrade_ident_desc_to_string : Parsetree.ident_desc -> string *)
(** [Descr] : Translates a [ident_desc] into a regular string.

    [Rem] : Not exported outside this module.                             *)
(* ********************************************************************** *)
let downgrade_ident_desc_to_string = function
  | Parsetree.I_local vname -> downgrade_vname vname
  | Parsetree.I_global (fname_opt, vname) -> 
      begin
      match fname_opt with
       | None -> downgrade_vname vname
       | Some whatever ->
	   raise
	     (No_mapping
		("downgrade_ident_desc_to_string: Parsetree.I_global Some (" ^
		 whatever ^ ") " ^ (downgrade_vname vname)))
      end
  | Parsetree.I_method (fname_opt, vname) ->
      begin
      match fname_opt with
       | None -> downgrade_vname vname
       | Some whatever ->
	   raise
	     (No_mapping
		("downgrade_ident_desc_to_string: Parsetree.I_method Some (" ^
		 whatever ^ ") " ^ (downgrade_vname vname)))
      end
;;
(* ************************************************************ *)
(*  [Fun] downgrade_ident_to_string : Parsetree.ident -> string *)
(** [Descr] : Translates a [ident] into a regular string.

    [Rem] : Not exported outside this module.                   *)
(* ************************************************************ *)
let downgrade_ident_to_string =
  process_generic_ast_desc downgrade_ident_desc_to_string
;;



(* ******************************************************************* *)
(*  [Fun] downgrade_ident_desc_to_simple_expr :                        *)
(*          Parsetree.ident_desc -> Ast_types.simple_expr              *)
(** [Descr] : Translates a [ident_desc] into an old AST [simple_expr].

    [Rem] : Not exported outside this module.                          *)
(* ******************************************************************* *)
let downgrade_ident_desc_to_simple_expr = function
  | Parsetree.I_local vname -> Ast_types.Simple_Id (downgrade_vname vname)
  | Parsetree.I_global (fname_opt, vname) ->
      Ast_types.Simple_Glob_Id (fname_opt, (downgrade_vname vname))
  | Parsetree.I_method (fname_opt, vname) ->
      raise (Dont_know_what_to_do "Parsetree.I_method")
;;
(* ************************************************************** *)
(*  [Fun] downgrade_ident_to_simple_expr :                        *)
(*          Parsetree.ident -> Ast_types.simple_expr              *)
(** [Descr] : Translates a [ident] into an old AST [simple_expr].

    [Rem] : Not exported outside this module.                     *)
(* ************************************************************** *)
let downgrade_ident_to_simple_expr =
  process_generic_ast_desc downgrade_ident_desc_to_simple_expr
;;



(* ********************************************************************** *)
(*  [Fun] downgrade_constant_desc_to_simple_expr :                        *)
(*          Parsetree.constant_desc -> Ast_types.simple_expr              *)
(** [Descr] : Translates a [constant_desc] into an old AST [simple_expr].

    [Rem] : Not exported outside this module.                             *)
(* ********************************************************************** *)
let downgrade_constant_desc_to_simple_expr = function
  | Parsetree.C_int i -> Ast_types.Simple_Int (int_of_string i)
  | Parsetree.C_float _ ->
      raise
	(No_mapping "downgrade_constant_desc_to_simple_expr: Parsetree.C_float")
  | Parsetree.C_bool _ ->
      raise
	(No_mapping "downgrade_constant_desc_to_simple_expr:Parsetree.C_bool")
  | Parsetree.C_string s -> Ast_types.Simple_String s
  | Parsetree.C_char _ ->
      raise
	(No_mapping "downgrade_constant_desc_to_simple_expr:Parsetree.C_char")
;;
(* ***************************************************************** *)
(*  [Fun] downgrade_constant_to_simple_expr :                        *)
(*          Parsetree.constant -> Ast_types.simple_expr              *)
(** [Descr] : Translates a [constant] into an old AST [simple_expr].

    [Rem] : Not exported outside this module.                        *)
(* ***************************************************************** *)
let downgrade_constant_to_simple_expr =
  process_generic_ast_desc downgrade_constant_desc_to_simple_expr
;;



(* ****************************************************************** *)
(*  [Fun] downgrade_expr_desc_to_simple_expr : Parsetree.expr_desc -> *)
(*          Ast_types.simple_expr                                     *)
(** [Descr] : Translates a [expr_desc] into an old AST [simple_expr].

    [Rem] : Not exported outside this module.                         *)
(* ****************************************************************** *)
let rec downgrade_expr_desc_to_simple_expr = function
  | Parsetree.E_const cst -> downgrade_constant_to_simple_expr cst
  | Parsetree.E_fun (arg_vnames, body) ->
      let arg_vnames' = List.map downgrade_vname arg_vnames in
      let body' = downgrade_expr_to_simple_expr body in
      (* Do not fold left otherwise arguments will be reversed ! *)
      List.fold_right
	(fun arg_name accu -> Ast_types.Simple_Lam (arg_name, accu))
	arg_vnames' body'
  | Parsetree.E_var id -> Ast_types.Simple_Id (downgrade_ident_to_string id)
  | Parsetree.E_app (expr, exprs) ->
      let expr' = downgrade_expr_to_simple_expr expr in
      let exprs' = List.map downgrade_expr_to_simple_expr exprs in
      Ast_types.Simple_App (expr', exprs')
  | Parsetree.E_constr (_, _) ->
      raise
	(No_mapping "downgrade_expr_desc_to_simple_expr: Parsetree.E_constr")
  | Parsetree.E_match (_, _) ->
      raise (No_mapping "downgrade_expr_desc_to_simple_expr: Parsetree.E_match")
  | Parsetree.E_if (_, _, _) ->
      raise (No_mapping "downgrade_expr_desc_to_simple_expr: Parsetree.E_if")
  | Parsetree.E_let (_, _) ->
      raise (No_mapping "downgrade_expr_desc_to_simple_expr: Parsetree.E_let")
  | Parsetree.E_record _ ->
      raise
	(No_mapping "downgrade_expr_desc_to_simple_expr: Parsetree.E_record")
  | Parsetree.E_record_access (_, _) ->
      raise 
	(No_mapping
	   "downgrade_expr_desc_to_simple_expr: Parsetree.E_record_access")
  | Parsetree.E_record_with (_, _) ->
      raise
	(No_mapping
	   "downgrade_expr_desc_to_simple_expr: Parsetree.E_record_with")
  | Parsetree.E_tuple _ ->
      raise (No_mapping "downgrade_expr_desc_to_simple_expr: Parsetree.E_tuple")
  | Parsetree.E_external _ ->
      raise
	(No_mapping "downgrade_expr_desc_to_simple_expr: Parsetree.E_external")
  | Parsetree.E_paren expr -> downgrade_expr_to_simple_expr expr
(* ************************************************************* *)
(*  [Fun] downgrade_expr_to_simple_expr :                        *)
(*          Parsetree.expr -> Ast_types.simple_expr              *)
(** [Descr] : Translates a [expr] into an old AST [simple_expr].

    [Rem] : Not exported outside this module.                    *)
(* ************************************************************* *)
and downgrade_expr_to_simple_expr expr =
  (process_generic_ast_desc downgrade_expr_desc_to_simple_expr) expr
;;



(* ************************************************************** *)
(*  [Fun] downgrade_species_param_desc :                          *)
(*          Parsetree.species_param_desc -> Ast_types.simple_expr *)
(** [Descr] : Translates a [species_param_desc] into an old
              AST [simple_expr].

    [Rem] : Not exported outside this module.                      *)
(* *************************************************************** *)
let downgrade_species_param_desc = function
  | Parsetree.SP expr -> downgrade_expr_to_simple_expr expr
;;
(* ********************************************************* *)
(*  [Fun] downgrade_species_param :                          *)
(*          Parsetree.species_param -> Ast_types.simple_expr *)
(** [Descr] : Translates a [species_param] into an old
              AST [simple_expr].

    [Rem] : Not exported outside this module.                *)
(* ********************************************************* *)
let downgrade_species_param =
  process_generic_ast_desc downgrade_species_param_desc
;;



(* ****************************************************************** *)
(*  [Fun] downgrade_species_expr_desc :                               *)
(*          Parsetree.species_expr_desc -> Ast_types.typ              *)
(** [Descr] : Translates a [species_expr_desc] into an old AST [typ].

    [Rem] : Not exported outside this module.                         *)
(* ****************************************************************** *)
let downgrade_species_expr_desc desc =
  match desc.Parsetree.se_params with
   | [] -> Ast_types.HoAtom (downgrade_ident_to_string desc.Parsetree.se_name)
   | whatever ->
       let params = List.map downgrade_species_param whatever in
       let name = downgrade_ident_to_string desc.Parsetree.se_name in
       Ast_types.HoApp (name, params)
;;
(* *********************************************************************** *)
(*  [Fun] downgrade_species_expr : Parsetree.species_expr -> Ast_types.typ *)
(** [Descr] : Translates a [species_expr] into an old AST [typ].

    [Rem] : Not exported outside this module.                              *)
(* *********************************************************************** *)
let downgrade_species_expr =
  process_generic_ast_desc downgrade_species_expr_desc
;;



(* ************************************************************************ *)
(*  [Fun] downgrade_species_param_type_desc :                               *)
(*          string -> Parsetree.species_param_type_desc -> Ast_types.prm    *)
(** [Descr] : Translates a [species_param_type_desc] into an old AST [prm].

    [Rem] : Not exported outside this module.                               *)
(* ************************************************************************ *)
let downgrade_species_param_type_desc name = function
  | Parsetree.SPT_in ident ->
      (* Entity parameter. *)
      Ast_types.Ent (name, Ast_types.Atom (downgrade_ident_to_string ident))
  | Parsetree.SPT_is species_expr ->
      (* Collection parameter. *)
      Ast_types.Collec (name, (downgrade_species_expr species_expr))
;;
(* ******************************************************************* *)
(*  [Fun] downgrade_species_param_type :                               *)
(*          string -> Parsetree.species_param_type -> Ast_types.prm    *)
(** [Descr] : Translates a [species_param_type] into an old AST [prm].

    [Rem] : Not exported outside this module.                          *)
(* ******************************************************************* *)
let downgrade_species_param_type name = 
  process_generic_ast_desc (downgrade_species_param_type_desc name)
;;



(* ****************************************************************** *)
(*  [Fun] downgrade_rep_type_def_desc :                               *)
(*          Parsetree.rep_type_def_desc -> Ast_types.typ              *)
(** [Descr] : Translates a [rep_type_def_desc] into an old AST [typ].

    [Rem] : Not exported outside this module.                         *)
(* ****************************************************************** *)
let rec downgrade_rep_type_def_desc = function
  | Parsetree.RTE_ident ident ->
      Ast_types.Atom (downgrade_ident_to_string ident)
  | Parsetree.RTE_fun (rtd1, rtd2) ->
      Ast_types.Fct
	((downgrade_rep_type_def rtd1), (downgrade_rep_type_def rtd1))
  | Parsetree.RTE_app (ident, rtds) ->
      let typs = List.map downgrade_rep_type_def rtds in
      Ast_types.Prm ((downgrade_ident_to_string ident) ,typs)
  | Parsetree.RTE_prod (rtd1, rtd2) ->
      Ast_types.Prod
	((downgrade_rep_type_def rtd1), (downgrade_rep_type_def rtd1))
  | Parsetree.RTE_paren rtd -> downgrade_rep_type_def rtd
(* *********************************************************************** *)
(*  [Fun] downgrade_rep_type_def : Parsetree.rep_type_def -> Ast_types.typ *)
(** [Descr] : Translates a [rep_type_def] into an old AST [typ].

    [Rem] : Not exported outside this module.                              *)
(* *********************************************************************** *)
and downgrade_rep_type_def rtd =
  process_generic_ast_desc downgrade_rep_type_def_desc rtd
;;



(* *************************************************************** *)
(*  [Fun] downgrade_type_expr_desc :                               *)
(*          Parsetree.type_expr_desc -> Ast_types.typ              *)
(** [Descr] : Translates a [type_expr_desc] into an old AST [typ].

    [Rem] : Not exported outside this module.                      *)
(* *************************************************************** *)
let rec downgrade_type_expr_desc = function
  | Parsetree.TE_ident ident -> Ast_types.Atom (downgrade_ident_to_string ident)
  | Parsetree.TE_fun (texpr1, texpr2) ->
      Ast_types.Fct ((downgrade_type_expr texpr1), (downgrade_type_expr texpr2))
  | Parsetree.TE_app (ident, texprs) ->
      let typs = List.map downgrade_type_expr texprs in
      Ast_types.Prm ((downgrade_ident_to_string ident) ,typs)
  | Parsetree.TE_prod (texpr1, texpr2) ->
      Ast_types.Prod
	((downgrade_type_expr texpr1), (downgrade_type_expr texpr2))
  | Parsetree.TE_self -> Ast_types.Self
  | Parsetree.TE_prop -> Ast_types.Prop
  | Parsetree.TE_paren texpr -> downgrade_type_expr texpr
(* ***************************************************************** *)
(*  [Fun] downgrade_type_expr : Parsetree.type_expr -> Ast_types.typ *)
(** [Descr] : Translates a [type_expr] into an old AST [typ].

    [Rem] : Not exported outside this module.                        *)
(* ***************************************************************** *)
and downgrade_type_expr texpr =
  (process_generic_ast_desc downgrade_type_expr_desc) texpr
;;



(* *********************************************************************** *)
(*  [Fun] downgrade_sig_def_desc : Parsetree.sig_def_desc -> Typed_elt.def *)
(** [Descr] : Translates a [sig_def_desc] into an old AST [def].

    [Rem] : Not exported outside this module.                              *)
(* *********************************************************************** *)
let downgrade_sig_def_desc desc =
  let name = downgrade_ident_to_string desc.Parsetree.sig_name in
  let typ = downgrade_type_expr desc.Parsetree.sig_type in
  Typed_elt.Sig (name, typ)
;;
(* ************************************************************* *)
(*  [Fun] downgrade_sig_def : Parsetree.sig_def -> Typed_elt.def *)
(** [Descr] : Translates a [sig_def] into an old AST [def].

    [Rem] : Not exported outside this module.                    *)
(* ************************************************************* *)
let downgrade_sig_def = process_generic_ast_desc downgrade_sig_def_desc ;;




let downgrade_ident_desc_to_expr_desc = function
  | Parsetree.I_local vname -> Ast_types.Id (downgrade_vname vname)
  | Parsetree.I_global (fname_opt, vname) ->
      Ast_types.Glob_Id (fname_opt, (downgrade_vname vname))
  | Parsetree.I_method (fname_opt, vname) ->
      raise
	(Dont_know_what_to_do
	   "downgrade_ident_desc_to_expr: Parsetree.I_method")
;;
let downgrade_ident_to_expr_desc =
  process_generic_ast_desc downgrade_ident_desc_to_expr_desc
;;



(* ******************************************************************* *)
(*  [Fun] downgrade_constant_desc_to_expr_desc :                       *)
(*          Parsetree.constant_desc -> Ast_types.exp_desc              *)
(** [Descr] : Translates a [constant_desc] into an old AST [exp_desc].

    [Rem] : Not exported outside this module.                          *)
(* ******************************************************************* *)
let downgrade_constant_desc_to_expr_desc = function
  | Parsetree.C_int i -> Ast_types.Int (int_of_string i)
  | Parsetree.C_float _ ->
      raise (No_mapping "downgrade_constant_desc_to_expr: Parsetree.C_float")
  | Parsetree.C_bool _ ->
      raise (No_mapping "downgrade_constant_desc_to_expr: Parsetree.C_bool")
  | Parsetree.C_string s -> Ast_types.String s
  | Parsetree.C_char _ ->
      raise (No_mapping "downgrade_constant_desc_to_expr: Parsetree.C_char")
;;
(* ************************************************************** *)
(*  [Fun] downgrade_constant_to_expr_desc :                       *)
(*          Parsetree.constant -> Ast_types.exp_desc              *)
(** [Descr] : Translates a [constant] into an old AST [exp_desc].

    [Rem] : Not exported outside this module.                     *)
(* ************************************************************** *)
let downgrade_constant_to_expr_desc =
  process_generic_ast_desc downgrade_constant_desc_to_expr_desc
;;



(* ************************************************************** *)
(* [Fun] downgrade_expr_desc_to_exp_desc :                       *)
(*         Parsetree.expr_desc -> Ast_types.exp_desc              *)
(** [Descr] : Translates a [expr_desc] into an old AST [exp_desc].
              Do not translate locations.

    [Rem] : Not exported outside this module.                     *)
(* ************************************************************** *)
let rec downgrade_expr_desc_to_exp_desc = function
  | Parsetree.E_const cst -> downgrade_constant_to_expr_desc cst
  | Parsetree.E_fun (arg_vnames, body) ->
      let arg_vnames' = List.map downgrade_vname arg_vnames in
      let body' = downgrade_expr_desc_to_exp_desc body.Parsetree.ast_desc in
      (* Do not fold left otherwise arguments will be reversed ! *)
      List.fold_right
	(fun arg_name accu_desc ->
	  (* Since we have to type information here, *)
	  (* we will generate type variable instead. *)
	  let new_var = gen_syntactic_type_variable () in
          let expr_from_accu = {
	    Ast_types.expr_loc = Loc.dummy ;
	    Ast_types.expr_desc = accu_desc } in
	  Ast_types.Lam (arg_name, (Ast_types.Var new_var), expr_from_accu))
	arg_vnames' body'
  | Parsetree.E_var id -> downgrade_ident_to_expr_desc id
  | Parsetree.E_app (expr, exprs) ->
      let expr' = downgrade_expr_to_expr expr in
      let exprs' = List.map downgrade_expr_to_expr exprs in
      Ast_types.App (expr', exprs')
  | Parsetree.E_constr (_, _) ->
      raise (No_mapping "downgrade_expr_desc_to_exp_desc: Parsetree.E_constr")
  | Parsetree.E_match (_, _) -> failwith "TODO14"
  | Parsetree.E_if (expr1, expr2, expr3) ->
      let expr1' = downgrade_expr_to_expr expr1 in
      let expr2' = downgrade_expr_to_expr expr2 in
      let expr3' = downgrade_expr_to_expr expr3 in
      Ast_types.Ifte (expr1', expr2', expr3')
  | Parsetree.E_let (_, _) -> failwith "TODO15"
  | Parsetree.E_record _ ->
      raise (No_mapping "downgrade_expr_desc_to_exp_desc: Parsetree.E_record")
  | Parsetree.E_record_access (_, _) ->
      raise
	(No_mapping
	   "downgrade_expr_desc_to_exp_desc: Parsetree.E_record_access")
  | Parsetree.E_record_with (_, _) ->
      raise
	(No_mapping "downgrade_expr_desc_to_exp_desc: Parsetree.E_record_with")
  | Parsetree.E_tuple _ ->
      raise (No_mapping "downgrade_expr_desc_to_exp_desc: Parsetree.E_tuple")
  | Parsetree.E_external _ -> failwith "TODO16"
  | Parsetree.E_paren expr ->
      downgrade_expr_desc_to_exp_desc expr.Parsetree.ast_desc
(* *************************************************************** *)
(*  [Fun] downgrade_expr_to_expr: Parsetree.expr -> Ast_types.expr *)
(** [Descr] : Translates a [expr] into an old AST [expr].
              Do not translate locations.

    [Rem] : Not exported outside this module.                      *)
(* *************************************************************** *)
and downgrade_expr_to_expr expr =
  { Ast_types.expr_loc = Loc.dummy ;
    Ast_types.expr_desc =
      process_generic_ast_desc downgrade_expr_desc_to_exp_desc expr }
;;



(* (string * typ * expr) *)
let downgrade_binding_desc desc =
  let bound_name = downgrade_ident_to_string desc.Parsetree.b_name in
  (* Recover the list of parameter with their type. If none      *)
  (* explicitly specified, then one get a type variable instead. *)
  let params =
    List.map 
      (fun (ident, texpr_opt) ->
	let param_name = downgrade_ident_to_string ident in
	let typ =
	  (match texpr_opt with
	   | None ->
	       (* Let's generate a fresh type variable. *)
	       Ast_types.Var (gen_syntactic_type_variable ())
	   | Some texpr -> downgrade_type_expr texpr) in
	(param_name, typ))
      desc.Parsetree.b_params in
  let body_desc' =
    downgrade_expr_desc_to_exp_desc desc.Parsetree.b_body.Parsetree.ast_desc in
  let body_typ =
    (match desc.Parsetree.b_type with
     | None ->
         (* Let's generate a fresh type variable. *)
	 Ast_types.Var (gen_syntactic_type_variable ())
     | Some texpr -> downgrade_type_expr texpr) in
  (* Now it's time to create both the functionnal expression *)
  (* and type if the bound variable has some parameters.     *)
  let (bound_expr, bound_type) =
    List.fold_right
      (fun (param_name, param_typ) (accu_expr_desc, accu_typ) ->
	let accu_expr = {
	  Ast_types.expr_loc = Loc.dummy ;
	  Ast_types.expr_desc = accu_expr_desc } in
	let accu_expr_desc' =
	  Ast_types.Lam (param_name, param_typ, accu_expr) in
	let accu_typ' = Ast_types.Fct (param_typ, accu_typ) in
	(accu_expr_desc', accu_typ'))
      params (body_desc', body_typ) in
  (bound_name, bound_expr, bound_type)
;;
let downgrade_binding = process_generic_ast_desc downgrade_binding_desc ;;



let downgrade_let_def_desc desc =
  match desc.Parsetree.ld_rec with
   | Parsetree.RF_no_rec ->
       (* Non-recursive definition. Will generate an old Let. Because there *)
       (* is no recursion, this should lead to only 1 definition. Anyway,   *)
       (* we will process the whole list of bindings without checking this  *)
       (* conjecture.                                                       *)
       failwith "TODO12"
   | Parsetree.RF_rec ->
     (* Recursive definition. Will generate an old Rec. *)
     failwith "TODO13"
;;
let downgrade_let_def = process_generic_ast_desc downgrade_let_def_desc ;;



let downgrade_property_def _ = failwith "TODO2" ;;
let downgrade_theorem_def _ = failwith "TODO3" ;;
let downgrade_proof_def _ = failwith "TODO4" ;;



(* ******************************************************************* *)
(*  [Fun] downgrade_species_field_desc :                               *)
(*          Parsetree.species_field_desc -> Typed_elt.def              *)
(** [Descr] : Translates a [species_field_desc] into an old AST [def].

    [Rem] : Not exported outside this module.                          *)
(* ******************************************************************* *)
let downgrade_species_field_desc = function
  | Parsetree.SF_rep rep_type_def ->
      Typed_elt.Rep_type (downgrade_rep_type_def rep_type_def)
  | Parsetree.SF_sig sig_def -> downgrade_sig_def sig_def
  | Parsetree.SF_let let_def -> downgrade_let_def let_def
  | Parsetree.SF_property property_def -> downgrade_property_def property_def
  | Parsetree.SF_theorem theorem_def -> downgrade_theorem_def theorem_def
  | Parsetree.SF_proof proof_def -> downgrade_proof_def proof_def
;;
(* ************************************************************** *)
(*  [Fun] downgrade_species_field :                               *)
(*          Parsetree.species_field -> Typed_elt.def              *)
(** [Descr] : Translates a [species_field] into an old AST [def].

    [Rem] : Not exported outside this module.                     *)
(* ************************************************************** *)
let downgrade_species_field =
  process_generic_ast_desc downgrade_species_field_desc
;;



(* ***************************************************************** *)
(*  [Fun] downgrade_species_def_desc :                               *)
(*          Parsetree.species_def_desc -> Typed_elt.def              *)
(** [Descr] : Translates a [species_def_desc] into an old AST [def].

    [Rem] : Not exported outside this module.                        *)
(* ***************************************************************** *)
let downgrade_species_def_desc species_def =
  (* Species's parameters. *)
  let prms =
    List.map
      (fun (vname, ptype) ->
	let param_name = downgrade_vname vname in
	downgrade_species_param_type param_name ptype)
      species_def.Parsetree.sd_params in
  (* Species's ancestors. *)
  let inheritance =
    List.map downgrade_species_expr
      species_def.Parsetree.sd_inherits.Parsetree.ast_desc in
  (* Species's fields. *)
  let fields =
    List.map downgrade_species_field species_def.Parsetree.sd_fields in
  Typed_elt.Spec (species_def.Parsetree.sd_name, prms, inheritance, fields)
;;
(* ********************************************************************* *)
(*  [Fun] downgrade_species_def : Parsetree.species_def -> Typed_elt.def *)
(** [Descr] : Translates a [species_def] into an old AST [def].

    [Rem] : Not exported outside this module.                            *)
(* ********************************************************************* *)
let downgrade_species_def = process_generic_ast_desc downgrade_species_def_desc
;;



(* ********************************************************************* *)
(*  [Fun] downgrade_external_desc : *)
let downgrade_external_desc ext_desc = failwith "TODO5" ;;
(* ********************************************************************* *)
(*  [Fun] downgrade_external : *)
let downgrade_external = process_generic_ast_desc downgrade_external_desc ;;



(* ************************************************************************* *)
(*  [Fun] downgrade_phrase_desc : Parsetree.phrase_desc -> Typed_elt.command *)
(** [Descr] : Translates a [phrase_desc] into an old AST [command].

    [Rem] : Not exported outside this module.                                *)
(* ************************************************************************* *)
let downgrade_phrase_desc = function
  | Parsetree.Ph_external external_def -> downgrade_external external_def
  | Parsetree.Ph_use fname -> Typed_elt.Uses fname
  | Parsetree.Ph_open fname -> Typed_elt.Open fname
  | Parsetree.Ph_species species_def ->
      Typed_elt.Comm (downgrade_species_def species_def)
  | Parsetree.Ph_coll coll_def -> failwith "TODO6"
  | Parsetree.Ph_type type_def -> failwith "TODO7"
  | Parsetree.Ph_let let_def -> failwith "TODO8"
  | Parsetree.Ph_theorem theorem -> failwith "TODO9"
  | Parsetree.Ph_expr expr -> failwith "TODO10"
;;
(* *************************************************************** *)
(*  [Fun] downgrade_phrase : Parsetree.phrase -> Typed_elt.command *)
(** [Descr] : Translates a [phrase] into an old AST [command].

    [Rem] : Not exported outside this module.                      *)
(* *************************************************************** *)
let downgrade_phrase = process_generic_ast_desc downgrade_phrase_desc ;;



(* ************************************************************************** *)
(*  [Fun] downgrade_file_desc : Parsetree.file_desc -> Typed_elt.command list *)
(** [Descr] : Translates a [file_desc] into an old AST [command] list.

    [Rem] : Exported outside this module.                                     *)
(* ************************************************************************** *)
let downgrade_file_desc = function
  | Parsetree.File phrases -> List.map downgrade_phrase phrases
 ;;
(* ******************************************************************** *)
(* [Fun] downgrade_file : Parsetree.file_desc -> Typed_elt.command list *)
(** [Descr] : Translates a [file] into an old AST [command] list.

    [Rem] : Exported outside this module.                               *)
(* ******************************************************************** *)
let downgrade_file = process_generic_ast_desc downgrade_file_desc ;;
