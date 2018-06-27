(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            Raphaël Cauderlier                                              *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - ... LIP6 and INRIA                                       *)
(*            2012 - ... ENSTA ParisTech                                      *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)


(* *************************************************************** *)
(** {b Descr} : This module performs the compilation from FoCaLiZe to
    Dedukti of proofs.  *)
(* *************************************************************** *)

exception Attempt_proof_by_def_of_species_param of
  (Location.t * Parsetree.expr_ident) ;;


exception Attempt_proof_by_def_of_declared_method_of_self of
  (Location.t * Parsetree.expr_ident) ;;


exception Attempt_proof_by_def_of_local_ident of
  (Location.t * Parsetree.expr_ident) ;;


exception Attempt_proof_by_prop_of_local_ident of
  (Location.t * Parsetree.expr_ident) ;;


exception Attempt_proof_by_unknown_hypothesis of
  (Location.t * Parsetree.vname) ;;


exception Attempt_proof_by_unknown_step of
  (Location.t * Parsetree.node_label) ;;


let section_gen_sym =
  let cnt = ref 0 in
  (fun () ->
    let tmp = !cnt in
    incr cnt ;
    tmp)
;;

(*
   There is no "Section" mechanism in Dedukti.
   Its use in the Coq backend is to call Zenon in a simple context
   where type information is pretty much discarded and undefined methods
   are free symbols.

   We ask Zenon to output just one term and we put it in the correct context.

   By doing so, we also avoid the dummy theorem produced in the Coq backend
   and needed to abstract over unused section variables.
*)

(* Type describing the possible section variables.
   Section variables are passed to Zenon because it needs them for type inference. *)
type section_variable =
  | SVType of string                                           (* Type *)
  | SVTypeAlias of string * Types.type_simple                  (* Type alias *)
  | SVVar of                                                   (* Variable *)
      string *                  (* Prefix: empty string or module name *)
        Parsetree.vname *       (* Name of the variable *)
        Types.type_simple       (* Type of the variable *)
;;

(* List of variable declarations for current section.
   This is a global variable. *)
let section_variable_list : section_variable list ref = ref [];;

let find_method_type_kind_by_name vname coll_meths =
  let method_info = List.find (fun mi -> mi.Env.mi_name = vname) coll_meths in
  method_info.Env.mi_type_kind
;;

(** {descr} Collects the section variables needed for the proof. *)
let generate_field_definition_prelude_true min_dk_env dependencies_from_params =
  List.iter
    (fun (species_param, (Env.ODFP_methods_list meths_from_param)) ->
      (* Recover the species parameter's name. *)
      let species_param_name =
        match species_param with
         | Env.TypeInformation.SPAR_in (n, _, _) -> n
         | Env.TypeInformation.SPAR_is ((_, n), _, _, _, _) ->
             Parsetree.Vuident n in
      (* Each abstracted method will be named like "_p_", followed by the
         species parameter name, followed by "_", followed by the method's
         name.
         We don't care here about whether the species parameters is "in" or
         "is". *)
      let prefix =
        "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) ^ "_" in
      List.iter
        (fun (meth, meth_ty_kind) ->
         match meth_ty_kind with
         | Parsetree_utils.DETK_computational meth_ty ->
            section_variable_list :=
              SVVar (prefix, meth, meth_ty) :: !section_variable_list
         | Parsetree_utils.DETK_logical _ -> ())
        meths_from_param)
    dependencies_from_params;
  (* Generate the parameters denoting methods of ourselves we depend on
     according the the minimal typing environment. *)
  List.iter
    (function (_, meth_dep) ->
           (* Reason ignored since in Dedukti we take all the kinds of methods in
              account. *)
           match meth_dep with
           | Env.TypeInformation.MDEM_Defined_carrier sch ->
               let ty = Types.specialize sch in
               section_variable_list :=
                 SVTypeAlias ("abst_T", ty)
                 :: !section_variable_list;
           | Env.TypeInformation.MDEM_Defined_computational _
           | Env.TypeInformation.MDEM_Defined_logical _ -> ()
           | Env.TypeInformation.MDEM_Declared_carrier ->
               (* Note that by construction, the carrier is first in the env. *)
              section_variable_list :=
                SVType "abst" :: !section_variable_list;
           | Env.TypeInformation.MDEM_Declared_computational (n, sch) ->
               (* Due to a decl-dependency, hence: abstract. *)
               let ty = Types.specialize sch in
               section_variable_list :=
                 SVVar
                   ("abst_", n, ty) :: !section_variable_list;
           | Env.TypeInformation.MDEM_Declared_logical _ -> ())
         min_dk_env;;

let add_equality_hypothesis_for_rec ctx =
  (* Add the equality between CBV f x and f(x). *)
  let output_string = Format.fprintf ctx.Context.scc_out_fmter "%s@\n" in
  output_string "dk_builtins.cbv_eq : cc.eP (dk_logic.forall_type (A : cc.uT =>";
  output_string "   dk_logic.forall_type (R : cc.uT =>";
  output_string "   dk_logic.forall (cc.Arrow A R) (f : cc.eT (cc.Arrow A R) =>";
  output_string "   dk_logic.forall A (a : cc.eT A =>";
  output_string "   dk_logic.equal R (dk_builtins.call_by_value A R f a) (f a)))))).";;

(* {b Descr} : Generates the postlude of the prototype of a definec method. It
   Prints its arguments with their type (not those induced by lambda-liftings),
   its return type and its body (if some is provided) WITH methods
   abstracted by "abst_xxx". *)
(* Should be places in Species_dk_generation but we need it for
   printing Zenon's "by definition"s. *)
let generate_defined_method_proto_postlude ctx print_ctx env
    ~self_manifest ~rec_status name params scheme body_opt =
  let out_fmter = ctx.Context.scc_out_fmter in
  let (generalized_vars, _) = Types.scheme_split scheme in
  let (params_with_type, ending_ty_opt, _) =
    MiscHelpers.bind_parameters_to_types_from_type_scheme
      ~self_manifest (Some scheme) params in
  let fun_name = match name with
    | Parsetree.Vname v
    | Parsetree.Qualified(_, v) -> v
  in
  let ending_ty =
    (match ending_ty_opt with
     | None ->
         (* Because we always provide a type scheme (a [Some ...]), one must
            always be returned a type, i.e, something [Some ...].  *)
         assert false
     | Some t -> t) in
  (match rec_status with
   | Env.RC_rec _ ->
      let body = match body_opt with
        | Some (Parsetree.BB_computational b) -> b
        | _ -> assert false
      in
      Format.fprintf out_fmter " :@ ";
      List.iter
        (fun var ->
         Format.fprintf out_fmter "%a : cc.uT ->@ "
                        Dk_pprint.pp_type_variable_to_dk var)
        generalized_vars ;
      List.iter
        (fun (_, opt_param_ty) ->
         match opt_param_ty with
         | Some param_ty ->
            Format.fprintf out_fmter "(%a) ->@ "
                           (Dk_pprint.pp_type_simple_to_dk_with_eps print_ctx) param_ty
         | None -> assert false)
        params_with_type ;
      (* Now, we print the ending type of the method. *)
      Format.fprintf out_fmter "%a.@\n"
                     (Dk_pprint.pp_type_simple_to_dk_with_eps print_ctx) ending_ty ;
      add_equality_hypothesis_for_rec ctx;
      Rec_let_dk_gen.generate_recursive_definition
        ctx print_ctx env fun_name params scheme body ~abstract:false ~close_parens:0 ~toplevel:true
        (fun ?sep _ _ -> ignore sep)
   | Env.RC_non_rec ->
      List.iter
        (fun var ->
         Format.fprintf out_fmter "@ (%a : cc.uT)"
                        Dk_pprint.pp_type_variable_to_dk var)
        generalized_vars ;
      List.iter
        (fun (param_vname, opt_param_ty) ->
         match opt_param_ty with
         | Some param_ty ->
            Format.fprintf out_fmter "@ (%a : %a)"
                           Parsetree_utils.pp_vname_with_operators_expanded param_vname
                           (Dk_pprint.pp_type_simple_to_dk_with_eps print_ctx) param_ty
         | None ->
            Format.fprintf out_fmter "@ %a"
                           Parsetree_utils.pp_vname_with_operators_expanded param_vname)
        params_with_type ;
      (* Now, we print the ending type of the method. *)
      Format.fprintf out_fmter " :@ %a "
                     (Dk_pprint.pp_type_simple_to_dk_with_eps print_ctx) ending_ty ;
      (* Generates the body's code of the method if some is provided.
     No local idents in the context because we just enter the scope of a species
     fields and so we are not under a core expression. Since we are generating
     a "let", methods from Self are printed "abst_XXX" since dependencies have
     leaded to "Variables abst_XXX" before this new "Variable". *)
      (match body_opt with
       | None -> ()
       | Some body ->
          Format.fprintf out_fmter ":=@ ";
          (match body with
           | Parsetree.BB_computational e ->
              Expr_dk_generation.generate_expr
                ctx ~local_idents: [] ~in_recursive_let_section_of: []
                ~self_methods_status:
                Expr_dk_generation.SMS_abstracted
                ~recursive_methods_status:
                Expr_dk_generation.RMS_regular
                env e
           | Parsetree.BB_logical p ->
              Species_record_type_dk_generation.generate_logical_expr
                ctx ~local_idents: [] ~in_recursive_let_section_of: []
                ~self_methods_status:
                Expr_dk_generation.SMS_abstracted
                ~recursive_methods_status:
                Expr_dk_generation.RMS_regular env p)));;







(* ************************************************************************* *)
(** {b Descr} : Helper to find, during a Zenon proof by steps, an hypothesis
    by its name among the list of available material to do the proof.

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
let find_hypothesis_by_name name l =
  let rec rec_find = function
    | [] -> raise Not_found
    | h :: q -> (
        match h.Parsetree.ast_desc with
         | Parsetree.H_hypothesis (n, body) ->
             if n = name then body else rec_find q
         | _ -> rec_find q
       ) in
  rec_find l
;;



(** Kind of things that one can "assume" in a Zenon script. *)
type assumed_hypothesis =
  | AH_variable of (Parsetree.vname * Parsetree.type_expr)
  | AH_lemma of Parsetree.logical_expr
;;



(** {b Descr} : Helper. *)
let rec find_assumed_variables_and_lemmas_in_hyps = function
  | [] -> []
  | h :: q -> (
      let found_stuff = find_assumed_variables_and_lemmas_in_hyps q in
      match h.Parsetree.ast_desc with
      | Parsetree.H_variable (n, ty_expr) ->
          (AH_variable (n, ty_expr)) :: found_stuff
      | Parsetree.H_hypothesis (_, body) -> (AH_lemma body) :: found_stuff
      | Parsetree.H_notation (_, _) -> found_stuff
     )
;;



(** {b Descr} : Helper. *)
let rec find_only_PN_subs_in_proof_nodes = function
  | [] -> []
  | n :: q ->
      match n.Parsetree.ast_desc with
      | Parsetree. PN_sub (node_label, _, _) ->
          node_label :: (find_only_PN_subs_in_proof_nodes q)
      | _ -> find_only_PN_subs_in_proof_nodes q
;;

(** To make recursive definitions working with Zenon.

    {b Rem} : For Function. *)
let zenonify_by_recursive_meth_definition ctx print_ctx env
                                          vname params scheme body =
  match body with
  | Parsetree.BB_logical _ -> assert false (* Not implemented *)
  | Parsetree.BB_computational body ->
     add_equality_hypothesis_for_rec ctx;
     Rec_let_dk_gen.generate_recursive_definition
       ctx print_ctx env vname params scheme body ~close_parens:0 ~abstract:true ~toplevel:false
       (fun ?sep _ _ -> ignore sep);
     Format.fprintf ctx.Context.scc_out_fmter ".@\n";;

(** Ensure that the enforced dependencies of a Dedukti script or assumed proof
    are related to entities that are really definitions, and not signatures or
    properties. This task is done for regular dependencies of Zenon proofs while
    generating the Zenon stuff. But for assumed or Dedukti script, since we
    directly emit verbatim code, dependencies must be checked aside.

    This function works like the below [zenonify_by_definition] but only does
    not emit any code. *)
let ensure_enforced_by_definition_is_definition min_dk_env def_expr_ident =
  match def_expr_ident.Parsetree.ast_desc with
   | Parsetree.EI_local _ ->
       raise
         (Attempt_proof_by_def_of_local_ident
            (def_expr_ident.Parsetree.ast_loc, def_expr_ident))
   | Parsetree.EI_global _ ->
       (* Since stuff is at toplevel, it cannot be a declaration and can only
          be a definition. *)
       ()
   | Parsetree.EI_method (qcollname_opt, vname) -> (
       match qcollname_opt with
       | None -> (
           (* The method comes from ourselves (Self). So we will search it
              inside the Dedukti minimal typing environment. *)
           let method_info =
             snd (MinEnv.find_dk_env_element_by_name vname min_dk_env) in
           match method_info with
           | Env.TypeInformation.MDEM_Declared_carrier | Env.TypeInformation.MDEM_Defined_carrier _ ->
               (* Syntax does not allow to mention "Self" as a proof
                  element. *)
               assert false
           | Env.TypeInformation.MDEM_Declared_computational (_, _)
           | Env.TypeInformation.MDEM_Declared_logical (_, _) ->
               (* We can't prove "by definition" of something only declared ! *)
               raise
                 (Attempt_proof_by_def_of_declared_method_of_self
                    (def_expr_ident.Parsetree.ast_loc, def_expr_ident))
           | Env.TypeInformation.MDEM_Defined_computational (_, _, _, _, _, _)
           | Env.TypeInformation.MDEM_Defined_logical (_, _, _) -> ()
          )
       | Some (Parsetree.Qualified (_, _)) ->
           (* The method comes from another module's species. Hence it is for
              sure from a toplevel species. And this is not correct since the
              methods used for proofs must only come from our methods or
              species parameters' ones. [Unsure] *)
           failwith "I think this is a toplevel species method (1)."
       | Some (Parsetree.Vname _) -> (
           (* The method belongs to a species parameters. Since they are
              always abstract, it is forbidden to prove "by definition" of a
              species parameter method. *)
           raise
             (Attempt_proof_by_def_of_species_param
                (def_expr_ident.Parsetree.ast_loc, def_expr_ident))
          )
      )
;;



let ensure_enforced_dependencies_by_definition_are_definitions min_dk_env
    enf_deps =
  List.iter
    (fun enf_dep ->
      match enf_dep.Parsetree.ast_desc with
       | Parsetree.Ed_definition expr_idents ->
           List.iter
             (ensure_enforced_by_definition_is_definition min_dk_env)
             expr_idents
       | Parsetree.Ed_property _ -> ())
    enf_deps
;;

let zenonify_free_ident ctx print_ctx env out ident =
  Format.fprintf out "@[%t : %t.@]@\n"
                 (fun _ -> Expr_dk_generation.generate_expr_ident ctx ident)
                 (fun _ -> Expr_dk_generation.generate_expr_ident_type ctx print_ctx env ident)
;;

let zenonify_all_free_idents ctx print_ctx env out expr =
  List.iter (zenonify_free_ident ctx print_ctx env out)
            (Parsetree_utils.get_free_local_idents_from_expr_desc
               expr.Parsetree.ast_desc)
;;

let zenonify_all_free_idents_from_logical_expr ctx print_ctx env out lexpr =
  List.iter (zenonify_free_ident ctx print_ctx env out)
            (Parsetree_utils.get_free_local_idents_from_logical_expr
               lexpr)
;;

let zenonify_all_free_idents_from_binding_body ctx print_ctx env out params body =
  List.iter (zenonify_free_ident ctx print_ctx env out)
            (Parsetree_utils.get_free_local_idents_from_binding_body
               params body)
;;

let zenonify_by_definition ctx print_ctx env min_dk_env ~self_manifest
    available_hyps by_def_expr_ident =
  let out_fmter = ctx.Context.scc_out_fmter in
  match by_def_expr_ident.Parsetree.ast_desc with
   | Parsetree.EI_local vname ->
       let rec lookup x l =
         match l with
         | { Parsetree.ast_desc =
               Parsetree.H_notation (((Parsetree.Vuident id
                                     | Parsetree.Vlident id) as y), body) }
             :: _ when x = y ->
            (id, body)
         | _ :: t -> lookup x t
         | [] -> raise
                   (Attempt_proof_by_def_of_local_ident
                      (by_def_expr_ident.Parsetree.ast_loc, by_def_expr_ident))
       in
       let (id, body) = lookup vname available_hyps in
       (* Declare possibly free idents *)
       zenonify_all_free_idents ctx print_ctx env out_fmter body;
       Format.fprintf out_fmter
         "(; For notation used via \"by definition of %a\". ;)@\n"
         Sourcify.pp_expr_ident by_def_expr_ident;
       let ty = match body.Parsetree.ast_type with
         | Parsetree.ANTI_type ty -> ty
         | _ -> assert false
       in
       Format.fprintf out_fmter "@[<2>def %s : %a :=@ "
                      id (Dk_pprint.pp_type_simple_to_dk_with_eps print_ctx) ty;
       Expr_dk_generation.generate_expr
         ctx ~local_idents: [] ~in_recursive_let_section_of: []
         ~self_methods_status:
           Expr_dk_generation.SMS_abstracted env
         ~recursive_methods_status:
           Expr_dk_generation.RMS_regular
         body ;
       (* Done... Then, final carriage return. *)
       Format.fprintf out_fmter ".@]@\n"
   | Parsetree.EI_global qvname -> (
       (* The stuff is in fact a toplevel definition, not a species method. We
          must recover its type kind from the environment. *)
       let current_species_name =
         Some
           (Parsetree_utils.name_of_vname
              (snd ctx.Context.scc_current_species)) in
       let value_body =
         Env.DkGenEnv.find_value
           ~loc: by_def_expr_ident.Parsetree.ast_loc
           ~current_unit: ctx.Context.scc_current_unit
           ~current_species_name by_def_expr_ident env in
       (* A bit of comment. *)
       Format.fprintf out_fmter
         "(; For toplevel definition used via \"by definition of %a\". ;)@\n"
         Sourcify.pp_expr_ident by_def_expr_ident ;
       let name_for_zenon =
         Parsetree_utils.make_concatenated_name_with_operators_expanded_from_qualified_vname
           ~current_unit: ctx.Context.scc_current_unit
           ~dont_qualify_if_local: true qvname in
       match value_body with
        | Env.DkGenInformation.VB_non_toplevel -> assert false
        | Env.DkGenInformation.VB_toplevel_let_bound
            (rec_status, params, scheme, body) ->
           (* Declare possibly free idents *)
           zenonify_all_free_idents_from_binding_body
             ctx print_ctx env out_fmter
             params body;
           Format.fprintf out_fmter "@[<2>def %s" name_for_zenon ;
           (* We now generate the sequence of real parameters of the
              method, NOT those induced by abstractions and finally the
              method's body. Anyway, since the used definition is at
              toplevel, there is no abstraction no notion of "Self", no
              dependencies. *)
           generate_defined_method_proto_postlude
             ctx print_ctx env ~self_manifest ~rec_status qvname params scheme (Some body);
           (* Then, final carriage return. *)
           Format.fprintf out_fmter ".@]@\n"
        | Env.DkGenInformation.VB_toplevel_property lexpr ->
           zenonify_all_free_idents_from_logical_expr
             ctx print_ctx env out_fmter lexpr;
           Format.fprintf out_fmter "@[<2>def %s :=@ " name_for_zenon ;
            (* Since the used definition is at toplevel, there is no abstraction
               no notion of "Self", no dependencies. *)
            Species_record_type_dk_generation.generate_logical_expr
              ctx ~local_idents: [] ~in_recursive_let_section_of: []
              ~self_methods_status:
                Expr_dk_generation.SMS_from_record (* Or anything *)
              ~recursive_methods_status:
                Expr_dk_generation.RMS_regular env lexpr ;
            (* Done... Then, final carriage return. *)
            Format.fprintf out_fmter ".@]@\n"
      )
   | Parsetree.EI_method (qcollname_opt, vname) -> (
       match qcollname_opt with
        | None -> (
            (* The method comes from ourselves (Self). So we will search it
               inside the Dedukti minimal typing environment. *)
            let method_info =
              snd (MinEnv.find_dk_env_element_by_name vname min_dk_env) in
            match method_info with
             | Env.TypeInformation.MDEM_Declared_carrier
             | Env.TypeInformation.MDEM_Defined_carrier _ ->
                 (* Syntax does not allow to mention "Self" as a proof
                    element. *)
                 assert false
             | Env.TypeInformation.MDEM_Declared_computational (_, _)
             | Env.TypeInformation.MDEM_Declared_logical (_, _) ->
                 (* We can't prove "by definition" of something only
                    declared ! *)
                 raise
                   (Attempt_proof_by_def_of_declared_method_of_self
                      (by_def_expr_ident.Parsetree.ast_loc, by_def_expr_ident))
             | Env.TypeInformation.MDEM_Defined_computational
                   (_, rec_status, _, params, scheme, body) -> (
               zenonify_all_free_idents_from_binding_body
                 ctx print_ctx env out_fmter params body;
                 (* A bit of comment. *)
                 Format.fprintf out_fmter
                   "(; For method of Self used via \"by definition of \
                   %a\". ;)@\n"
                   Sourcify.pp_expr_ident by_def_expr_ident ;
                 match rec_status with
                 | Env.RC_rec _ ->
                     (* Since we are in the case of a method of Self, we must
                        find the abstraction_info and the abstracted_methods
                        in the already [generated_fields]. *)
                     zenonify_by_recursive_meth_definition
                       ctx print_ctx env vname params scheme body
                 | Env.RC_non_rec  ->
                     Format.fprintf out_fmter "@[<2>def abst_%a"
                       Parsetree_utils.pp_vname_with_operators_expanded
                       vname ;
                     (* We now generate the sequence of real parameters of the
                        method, not those induced by abstraction and finally the
                        method's body. Inside, methods we depend on are
                        abstracted by "abst_xxx". Non recursive, hence not
                        decreasing argument to pass. *)
                     generate_defined_method_proto_postlude
                       ctx print_ctx env ~self_manifest ~rec_status (Parsetree.Vname vname) params scheme
                       (Some body) ;
                     (* Done... Then, final carriage return. *)
                     Format.fprintf out_fmter ".@]@\n"
                 )
             | Env.TypeInformation.MDEM_Defined_logical (_, _, body) ->
                zenonify_all_free_idents_from_logical_expr
                  ctx print_ctx env out_fmter body;
                 (* A bit of comment. *)
                 Format.fprintf out_fmter
                   "(; For method of Self used via \"by definition of \
                   %a\". ;)@\n"
                   Sourcify.pp_expr_ident by_def_expr_ident;
                 Format.fprintf out_fmter "@[<2>def abst_%a :=@ "
                   Parsetree_utils.pp_vname_with_operators_expanded vname ;
                 (* We now generate the sequence of real parameters of the
                    method, not those induced by abstraction and finally the
                    method's body. Inside, methods we depend on are abstracted
                    by "abst_xxx".
                    No recursion problem here since recursive properties are
                    not allowed. *)
                 Species_record_type_dk_generation.generate_logical_expr
                   ctx ~local_idents: [] ~in_recursive_let_section_of: []
                   ~self_methods_status:
                     Expr_dk_generation.SMS_abstracted env
                   ~recursive_methods_status:
                     Expr_dk_generation.RMS_regular body ;
                 (* Done... Then, final carriage return. *)
                 Format.fprintf out_fmter ".@]@\n"
           )
        | Some (Parsetree.Qualified (_, _)) ->
            (* The method comes from another module's species. Hence it is for
               sure from a toplevel species. And this is not correct since the
               methods used for proofs must only come from our methods or
               species parameters' ones. [Unsure] *)
            failwith "I think this is a toplevel species method (2)."
        | Some (Parsetree.Vname _) -> (
            (* The method belongs to a species parameters. Since they are
               always abstract, it is forbidden to prove "by definition" of a
               species parameter method. *)
            raise
              (Attempt_proof_by_def_of_species_param
                 (by_def_expr_ident.Parsetree.ast_loc, by_def_expr_ident))
           )
      )
;;



(* ******************************************************************* *)
(** {b Descr} : Helper type used to discriminate in the function
    [zenonify_by_property_when_qualified_method] the cases where the
    ident refers to a species parameter of the current species or to a
    toplevel species (possiliy in another module).

    {b Rem} : Not exported outside this module.                        *)
(* ******************************************************************* *)
type species_param_or_topleve_species =
  | SPOTS_param of Parsetree.vname  (** The ident was refering to a species
                                        parameter of the current species. *)
  | SPOTS_toplevel_species of
      (Parsetree.module_name * (** The ident was refering to a toplevel species
                                   and here is the module name hosting the
                                   toplevel species. *)
       Parsetree.vname) (** The species name the ident was refering to. *)
;;



(** {b Descr} : Helper... *)
let exists_among_parameters_from_dependencies param_name deps_from_params =
  List.exists
    (fun (p ,_) ->
      match p with
       | Env.TypeInformation.SPAR_in (_, _, _) -> false
       | Env.TypeInformation.SPAR_is ((_, p), _, _, _, _) ->
           (Parsetree.Vuident p) = param_name)
    deps_from_params
;;




(* *********************************************************************** *)
(** {b Descr} : Handle the subcase of [zenonify_by_property] in the case
    where the method used by the proof has a qualified name.
    This decomposition in 2 functions is mostly to ease readability of the
    compiler because otherwise the function  [zenonify_by_property] gets
    really a mess to read !

    {b Args} :
      - [from_qcollname] : The qualified species name hosting the method
          used in the proof under a "by property...".

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let zenonify_by_property_when_qualified_method ctx print_ctx env
    dependencies_from_params by_prop_expr_ident from_qcollname meth_vname =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* We search if the [from_qcollname] has the same name than one of the
     species parameters. For this, either [from_qcollname] has no qualification
     hence is implicitely in the current compilation unit or is qualified with
     the same file name that the current compilation unit. *)
  let param_or_topl_species_to_search_opt =
    (match from_qcollname with
     | Parsetree.Vname param_name ->
         (* Implicitely in the current compilation unit. *)
         if exists_among_parameters_from_dependencies
             param_name dependencies_from_params then
           SPOTS_param param_name
         else
           SPOTS_toplevel_species (ctx.Context.scc_current_unit, param_name)
     | Parsetree.Qualified (mod_name, species_name) ->
         if exists_among_parameters_from_dependencies
             species_name dependencies_from_params then
           SPOTS_param species_name
         else SPOTS_toplevel_species (mod_name, species_name)) in
  match param_or_topl_species_to_search_opt with
   | SPOTS_toplevel_species (mod_name, topl_species_name) ->
       (begin
       (* The method comes from another module's species. Hence it
          is for sure from a toplevel species. We must recover its
          type. *)
       let fake_ident = {
         Parsetree.ast_desc =
           Parsetree.I_global
             (Parsetree.Qualified (mod_name, topl_species_name));
         (* Roughly correction as a location, even is not exact. *)
         Parsetree.ast_loc = by_prop_expr_ident.Parsetree.ast_loc;
         Parsetree.ast_annot = [];
         Parsetree.ast_type = Parsetree.ANTI_none } in
       let (_, coll_info, _, _) =
         Env.DkGenEnv.find_species
           ~loc: by_prop_expr_ident.Parsetree.ast_loc
           ~current_unit: ctx.Context.scc_current_unit fake_ident env in
       (* Now, look for the of the method. *)
       let meth_ty_kind = find_method_type_kind_by_name meth_vname coll_info in
       (* A bit of comment. *)
       Format.fprintf out_fmter
         "(; For toplevel collection's method used via \"by \
         property %a\". ;)@\n"
         Sourcify.pp_expr_ident by_prop_expr_ident;
       match meth_ty_kind with
        | Env.MTK_computational meth_sch ->
            let meth_ty = Types.specialize meth_sch in
            Format.fprintf out_fmter "@[<2>";
            if mod_name <> ctx.Context.scc_current_unit then
              Format.fprintf out_fmter "%s." mod_name;
            Format.fprintf out_fmter "%a__%a"
              Parsetree_utils.pp_vname_with_operators_expanded topl_species_name
              Parsetree_utils.pp_vname_with_operators_expanded meth_vname ;
            Format.fprintf out_fmter
              " :@ %a.@]@\n" (Dk_pprint.pp_type_simple_to_dk_with_eps print_ctx) meth_ty
        | Env.MTK_logical lexpr ->
            zenonify_all_free_idents_from_logical_expr ctx print_ctx env out_fmter lexpr;
            Format.fprintf out_fmter
              "@[<2>";
            if mod_name <> ctx.Context.scc_current_unit then
              Format.fprintf out_fmter "%s." mod_name;
            Format.fprintf out_fmter "%a__%a"
              Parsetree_utils.pp_vname_with_operators_expanded topl_species_name
              Parsetree_utils.pp_vname_with_operators_expanded meth_vname;
            Format.fprintf out_fmter " :@ dk_logic.eP (";
            (* We must substitute Self by the toplevel collection. In effect,
               each method of Self of this toplevel collection must be called
               anyway
                  module.species.meth,
               possibly without module if it is the current one. This is needed
               only when generating in Zenon's Section the methods to used via
               "by property" or "by definition" when they are identified to be
               coming from a toplevel collection. In effect, in the structure we
               keep to be able to generate the text of these methods for Zenon,
               we have the body of the method BUT in its environment, that is,
               with methods of Self referencing the methods of the species.
               Unfortunately, when we generate the code, we are not in the scope
               of this species. Hence we must rename on the fly occurrences of
               Self'methods by explicitely qualifying them. This is hence done
               by applying a substitution of Self in the body of the method. *)
            let lexpr' =
              SubstColl.subst_logical_expr
                ~current_unit: ctx.Context.scc_current_unit
                SubstColl.SRCK_self
                (Types.SBRCK_coll
                   (mod_name,
                    (Parsetree_utils.name_of_vname topl_species_name))) lexpr in
            (* Now, let's dump the code of the modified body. *)
            Species_record_type_dk_generation.generate_logical_expr
              ctx ~local_idents: [] ~in_recursive_let_section_of: []
              ~self_methods_status:
                (* Or whatever since we substituted Self by the effective
                   collection. *)
                Expr_dk_generation.SMS_from_record
              ~recursive_methods_status:
                Expr_dk_generation.RMS_regular env lexpr' ;
            Format.fprintf out_fmter ").@]@\n"
       end)
   | SPOTS_param param_name ->
       (begin
       (* The method belongs to a species parameters. We first get the
          species parameter's bunch of methods. *)
       let (Env.ODFP_methods_list param_meths) =
         Handy.list_assoc_custom_eq
           (fun spe_param searched ->
             match spe_param with
              | Env.TypeInformation.SPAR_in (_, _, _) ->
                  (* Proofs never use methods of "IN" parameters. *)
                  false
              | Env.TypeInformation.SPAR_is ((_, n), _, _, _, _) ->
                  (Parsetree.Vuident n) = searched)
           param_name
           dependencies_from_params in
       (* Now, get the type of the specified method. *)
       let (_, meth_ty_kind) =
         List.find (fun (n, _) -> n = meth_vname) param_meths in
       (* A bit of comment. *)
       Format.fprintf out_fmter
         "(; For species parameter method used via \"by \
         property %a\". ;)@\n"
         Sourcify.pp_expr_ident by_prop_expr_ident;
       (* The method is name by "_p_" + the species parameter's name
          + "_" + the method's name. *)
       match meth_ty_kind with
        | Parsetree_utils.DETK_computational meth_ty ->
            Format.fprintf out_fmter
              "@[<2>_p_%a_%a :@ %a.@]@\n"
              Parsetree_utils.pp_vname_with_operators_expanded param_name
              Parsetree_utils.pp_vname_with_operators_expanded meth_vname
              (Dk_pprint.pp_type_simple_to_dk_with_eps print_ctx)
              meth_ty
        | Parsetree_utils.DETK_logical lexpr ->
            zenonify_all_free_idents_from_logical_expr ctx print_ctx env out_fmter lexpr;
            (* Inside the logical expression of the method of the parameter
               "Self" must be printed as "_p_param_name_T". *)
            let self_map =
              Species_record_type_dk_generation.make_Self_cc_binding_species_param
                ~current_species: ctx.Context.scc_current_species param_name in
            let ctx' = { ctx with
              Context.scc_collections_carrier_mapping =
                self_map :: ctx.Context.scc_collections_carrier_mapping } in
            Format.fprintf out_fmter
              "@[<2>_p_%a_%a :@ dk_logic.eP@ ("
              Parsetree_utils.pp_vname_with_operators_expanded param_name
              Parsetree_utils.pp_vname_with_operators_expanded meth_vname;
            Species_record_type_dk_generation.generate_logical_expr
              ctx' ~local_idents: [] ~in_recursive_let_section_of: []
              ~self_methods_status:
                (Expr_dk_generation.SMS_from_param param_name)
              ~recursive_methods_status:
                Expr_dk_generation.RMS_regular env lexpr ;
            Format.fprintf out_fmter ").@]@\n"
       end)
;;



let zenonify_by_property ctx print_ctx env min_dk_env
    dependencies_from_params by_prop_expr_ident =
  let out_fmter = ctx.Context.scc_out_fmter in
  match by_prop_expr_ident.Parsetree.ast_desc with
   | Parsetree.EI_local _ ->
       raise
         (Attempt_proof_by_prop_of_local_ident
            (by_prop_expr_ident.Parsetree.ast_loc, by_prop_expr_ident))
   | Parsetree.EI_global qvname -> (
       (* The stuff is in fact a toplevel definition, not a species method. We
           must recover its type kind from the environment. *)
       let current_species_name =
         Some
           (Parsetree_utils.name_of_vname
              (snd ctx.Context.scc_current_species)) in
       let value_body =
         Env.DkGenEnv.find_value
           ~loc: by_prop_expr_ident.Parsetree.ast_loc
           ~current_unit: ctx.Context.scc_current_unit
           ~current_species_name by_prop_expr_ident env in
                 (* A bit of comment. *)
       Format.fprintf out_fmter
         "(; For toplevel definition used via \"by property of %a\". ;)@\n"
         Sourcify.pp_expr_ident by_prop_expr_ident;
       let name_for_zenon =
         Parsetree_utils.make_concatenated_name_from_qualified_vname qvname in
       match value_body with
        | Env.DkGenInformation.VB_non_toplevel -> assert false
        | Env.DkGenInformation.VB_toplevel_let_bound (_, _, scheme, _) ->
            (* We just need to print the type of the method. *)
            let meth_ty = Types.specialize scheme in
            Format.fprintf out_fmter "@[<2>%s :@ %a.@]@\n"
              name_for_zenon (Dk_pprint.pp_type_simple_to_dk_with_eps print_ctx) meth_ty
        | Env.DkGenInformation.VB_toplevel_property lexpr ->
            zenonify_all_free_idents_from_logical_expr ctx print_ctx env out_fmter lexpr;
            Format.fprintf out_fmter "@[<2>%s :@ dk_logic.eP (" name_for_zenon;
            (* Since the used definition is at toplevel, there is no abstraction
               no notion of "Self", no dependencies. *)
            Species_record_type_dk_generation.generate_logical_expr
              ctx ~local_idents: [] ~in_recursive_let_section_of: []
              ~self_methods_status:
                Expr_dk_generation.SMS_from_record (* Or anything *)
              ~recursive_methods_status:
                Expr_dk_generation.RMS_regular env lexpr ;
            (* Done... Then, final carriage return. *)
            Format.fprintf out_fmter ").@]@\n"
      )
   | Parsetree.EI_method (qcollname_opt, vname) -> (
       match qcollname_opt with
        | None -> (
            (* The method comes from ourselves (Self). So we will search it
               inside the dk minimal typing environment. *)
            let method_info =
              snd (MinEnv.find_dk_env_element_by_name vname min_dk_env) in
            match method_info with
             | Env.TypeInformation.MDEM_Declared_carrier
             | Env.TypeInformation.MDEM_Defined_carrier _ ->
                 (* Syntax does not allow to mention "Self" as a proof
                    element. *)
                 assert false
             | Env.TypeInformation.MDEM_Declared_computational (_, scheme)
             | Env.TypeInformation.MDEM_Defined_computational (_, _, _, _, scheme, _) ->
                 (* A bit of comment. *)
                 Format.fprintf out_fmter
                   "(; For method of Self used via \"by property %a\". ;)@\n"
                   Sourcify.pp_expr_ident by_prop_expr_ident;
                 (* We just need to print the type of the method. *)
                 let meth_ty = Types.specialize scheme in
                 Format.fprintf out_fmter "@[<2>abst_%a :@ %a.@]@\n"
                   Parsetree_utils.pp_vname_with_operators_expanded vname
                   (Dk_pprint.pp_type_simple_to_dk_with_eps print_ctx) meth_ty
             | Env.TypeInformation.MDEM_Declared_logical (_, body)
             | Env.TypeInformation.MDEM_Defined_logical (_, _, body) ->
                 zenonify_all_free_idents_from_logical_expr ctx print_ctx env out_fmter body;
                 (* A bit of comment. *)
                 Format.fprintf out_fmter
                   "(; For method of Self used via \"by property %a\". ;)@\n"
                   Sourcify.pp_expr_ident by_prop_expr_ident;
                 (* We need to print the logical expression of the method. *)
                 Format.fprintf out_fmter "@[<2>abst_%a :@ dk_logic.eP ("
                   Parsetree_utils.pp_vname_with_operators_expanded vname;
                 (* We now generate the sequence of real parameters of the
                    method, not those induced by abstraction and finally the
                    method's body. Inside, methods we depend on are abstracted
                    by "abst_xxx". *)
                 Species_record_type_dk_generation.generate_logical_expr
                   ctx ~local_idents: [] ~in_recursive_let_section_of: []
                   ~self_methods_status:
                     Expr_dk_generation.SMS_abstracted
                   ~recursive_methods_status:
                     Expr_dk_generation.RMS_regular env body ;
                 (* Done... Then, final carriage return. *)
                 Format.fprintf out_fmter ").@]@\n"
           )
        | Some qcollname ->
            zenonify_by_property_when_qualified_method
              ctx print_ctx env dependencies_from_params by_prop_expr_ident
              qcollname vname
      )
;;



(** We must pass to Zenon the definition of the type in Dk-like syntax. *)
let zenonify_by_type ctx env type_ident =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* We first search for the type definition in the code generation
     environment. *)
  let ty_def =
    Env.DkGenEnv.find_type
      ~loc: type_ident.Parsetree.ast_loc
      ~current_unit: ctx.Context.scc_current_unit type_ident env in
  (* A bit of comment. *)
  Format.fprintf out_fmter
    "(; For type definition used via \"by type %a\". ;)@\n"
    Sourcify.pp_ident type_ident;
  (* Now, generate the definition like we usually do in Dk syntax. *)
  let reduced_ctx = {
    Context.rcc_current_unit = ctx.Context.scc_current_unit ;
    (* Since type definitions are at toplevel, they can never reference
       species parameters, so we can safely leave this list empty. *)
    Context.rcc_species_parameters_names = [] ;
    (* Since type definitions are at toplevel, they can never reference
       species parameters carriers, so we can safely leave this list empty. *)
    Context.rcc_collections_carrier_mapping = [] ;
    (* For the same reason than above, we can leave this list empty. *)
    Context.rcc_lambda_lift_params_mapping = [] ;
    Context.rcc_out_fmter = out_fmter } in
  (* Unqualify the type name since in a type definition, the name is always
     without any qualification. We never write "type foo#t = ..." ! *)
  let type_vname = Parsetree_utils.unqualified_vname_of_ident type_ident in
  (* Throw the resulting environment since we do not to bind any thing
     anymore. The returned value of [type_def_compile] is only useful when
     compiling the type definition the first time it appears, i.e. when we
     encounter it as a FoCaLize phrase. By the way, tell not to enrich the
     environment otherwise, since the type is already defined, when inserting
     it again in the environment, we will have an error telling "already
     bound". We also must qualify the constructors in the definition if
     the type is not hosted in the compilation unit where the proof appears.
     In effect, in functions using this type and on which we depend to make
     proofs, constructors WILL be qualified. Not qualifying constructors in
     the "fake" definition for Zenon would prevent ot from finding proofs. *)
  ignore
    (Type_dk_generation.type_def_compile
       ~as_zenon_fact: true reduced_ctx env type_vname ty_def)
;;



type proof_step_availability = {
  psa_node_label : Parsetree.node_label ;
  psa_lemma_name : Parsetree.vname ;
  psa_base_logical_expr : Parsetree.logical_expr ;
  psa_assumed_variables_and_lemmas : assumed_hypothesis list
}
;;



let add_quantifications_and_implications ctx print_ctx env avail_info =
  let out_fmter = ctx.Context.scc_out_fmter in
  let rec rec_print = function
    | [] -> ()
    | assumed :: q ->
        (begin
        match assumed with
         | AH_variable (var_vname, ty_expr) ->
             (begin
             (* Quantify all the assumed variables. *)
             (match ty_expr.Parsetree.ast_type with
              | Parsetree.ANTI_type ty ->
                  Format.fprintf out_fmter "dk_logic.forall (%a) (%a :@ %a =>@ "
                    (Dk_pprint.pp_type_simple_to_dk print_ctx) ty
                    Parsetree_utils.pp_vname_with_operators_expanded var_vname
                    (Dk_pprint.pp_type_simple_to_dk_with_eps_and_prio print_ctx) ty
              | _ -> assert false) ;
             rec_print q
             end)
         | AH_lemma log_expr ->
             (* Make a string of implications with the assumed logical
                expressions. *)
             Format.fprintf out_fmter "@[<1>dk_logic.imp (";
             Species_record_type_dk_generation.generate_logical_expr
               ctx ~local_idents: [] ~in_recursive_let_section_of: []
               ~self_methods_status:
                 Expr_dk_generation.SMS_abstracted
               ~recursive_methods_status:
                 Expr_dk_generation.RMS_regular env log_expr ;
             Format.fprintf out_fmter ")@ (";
             rec_print q ;
             Format.fprintf out_fmter "@]"
        end) in
  rec_print avail_info.psa_assumed_variables_and_lemmas
;;

let close_quantifications_and_implications ctx avail_info =
  let out_fmter = ctx.Context.scc_out_fmter in
  let rec rec_print = function
    | [] -> ()
    | assumed :: q ->
        (begin
        match assumed with
         | AH_variable (_, ty_expr) ->
             (* Quantify all the assumed variables. *)
             (match ty_expr.Parsetree.ast_type with
              | Parsetree.ANTI_type _ ->
                  Format.fprintf out_fmter ")"
              | _ -> assert false) ;
             rec_print q;
         | AH_lemma _ ->
            Format.fprintf out_fmter ")";
            rec_print q
        end) in
  rec_print avail_info.psa_assumed_variables_and_lemmas
;;


(* *********************************************************************** *)
(** {b Descr} : Generate the Definition and Parameter for Dk that Zenon
    needs to automatically prove the current theorem. Methods used by the
    the proof are present in the minimal typing environment or the
    parameters dependencies since they induce either a def or a
    decl-dependency. Hence, to recover them, we will search inside these 2
    kinds of information.

    {b Exported} : No.                                                     *)
(* *********************************************************************** *)
let zenonify_fact ctx print_ctx env min_dk_env ~self_manifest
    dependencies_from_params available_hyps available_steps
    fact =
  let out_fmter = ctx.Context.scc_out_fmter in
  match fact.Parsetree.ast_desc with
   | Parsetree.F_definition expr_idents ->
       (* Syntax: "by definition ...". This leads to a Dk Definition. *)
       List.iter
         (zenonify_by_definition
           ctx print_ctx env min_dk_env ~self_manifest
           available_hyps)
         expr_idents
   | Parsetree.F_property expr_idents ->
       (* Syntax: "by property ...". This leads to a Dk Parameter. *)
       List.iter
         (zenonify_by_property
            ctx print_ctx env min_dk_env dependencies_from_params)
         expr_idents
   | Parsetree.F_hypothesis vnames ->
       (* Syntax: "by hypothesis ...". *)
       List.iter
         (fun vname ->
           let hyp_logical_expr =
             (try find_hypothesis_by_name vname available_hyps with
             | Not_found ->
                 raise
                   (Attempt_proof_by_unknown_hypothesis
                      (fact.Parsetree.ast_loc, vname))) in
           zenonify_all_free_idents_from_logical_expr ctx print_ctx env out_fmter hyp_logical_expr;
           Format.fprintf out_fmter "(; For hypothesis \"%a\". ;)@\n"
             Sourcify.pp_vname vname;
           Format.fprintf out_fmter "@[<2>%a :@ dk_logic.eP ("
             Parsetree_utils.pp_vname_with_operators_expanded vname ;
           Species_record_type_dk_generation.generate_logical_expr
             ctx ~local_idents: [] ~in_recursive_let_section_of: []
             ~self_methods_status:
               Expr_dk_generation.SMS_abstracted
             ~recursive_methods_status:
                Expr_dk_generation.RMS_regular env hyp_logical_expr ;
           (* Done... Then, final carriage return. *)
           Format.fprintf out_fmter ").@]@\n")
         vnames
   | Parsetree.F_node node_labels ->
       (* Syntax: "by step ...". We must search among the [available_steps] the
          logical expression related to this step and print it in the current
          auto-proof section as a Parameter. This Parameter must be universally
          quantified by all the "assume" [H_variable]s found in the available
          hypotheses and be the consequence of the implications strings
          compound of all the assumed [H_hypothesis]s found in the closed
          Sections since its creation. *)
       List.iter
         (fun node_label ->
           let avail_info =
             (try
               List.find
                 (fun a -> a.psa_node_label = node_label) available_steps with
             | Not_found ->
                 raise
                   (Attempt_proof_by_unknown_step
                      (fact.Parsetree.ast_loc, node_label))) in
           zenonify_all_free_idents_from_logical_expr
             ctx print_ctx env out_fmter avail_info.psa_base_logical_expr;
           Format.fprintf out_fmter "(; For step <%d>%s. ;)@\n"
             (fst node_label) (snd node_label);
           Format.fprintf out_fmter "@[<2>%a :@ dk_logic.eP ("
             Parsetree_utils.pp_vname_with_operators_expanded
             avail_info.psa_lemma_name;
           add_quantifications_and_implications ctx print_ctx env avail_info ;
           (* Now, print the lemma's body. Be careful to enclose it between
              parens to avoid associativity issues. Was bug #62. *)
           Format.fprintf out_fmter "(" ;
           Species_record_type_dk_generation.generate_logical_expr
             ctx ~local_idents: [] ~in_recursive_let_section_of: []
             ~self_methods_status: Expr_dk_generation.SMS_abstracted
             ~recursive_methods_status:
               Expr_dk_generation.RMS_regular
             env avail_info.psa_base_logical_expr ;
           Format.fprintf out_fmter "))" ;
           (* Close parentheses introduced by the function quantifications_and_implications *)
           close_quantifications_and_implications ctx avail_info ;
           (* Done... Then, final carriage return. *)
           Format.fprintf out_fmter ".@]@\n")
         node_labels
   | Parsetree.F_type type_idents ->
       (* Syntax: "by type ...". *)
       List.iter
         (fun type_ident -> zenonify_by_type ctx env type_ident)
         type_idents
;;



let zenonify_hyp ctx print_ctx env ~sep hyp =
  let out_fmter = ctx.Context.scc_out_fmter in
  match hyp.Parsetree.ast_desc with
   | Parsetree.H_variable  (vname, type_expr) ->
       (begin
       match type_expr.Parsetree.ast_type with
        | Parsetree.ANTI_type ty ->
            (* Notation "assume ... in ...". This leads to a Variable in the
               current Dk Section. *)
            Format.fprintf out_fmter "@[<2>%a :@ %a %s@ @]@\n"
              Parsetree_utils.pp_vname_with_operators_expanded vname
              (Dk_pprint.pp_type_simple_to_dk_with_eps_and_prio print_ctx) ty sep;
            section_variable_list :=
              SVVar ("", vname, ty) :: !section_variable_list
        | _ -> assert false
       end)
   | Parsetree.H_hypothesis (vname, logical_expr) ->
       (* Notation "H: all blabla in Self, foo -> bar...". This leads to a
          Variable in the current Dk Section. *)
       Format.fprintf out_fmter "@[<2>%a :@ dk_logic.eP ("
         Parsetree_utils.pp_vname_with_operators_expanded vname;
       Species_record_type_dk_generation.generate_logical_expr
         ~local_idents: [] ~in_recursive_let_section_of: []
         ~self_methods_status: Expr_dk_generation.SMS_abstracted
         ~recursive_methods_status: Expr_dk_generation.RMS_regular
         ctx env logical_expr ;
       Format.fprintf out_fmter ") %s@ @]@\n" sep
   | Parsetree.H_notation (vname, expr) ->
       (* Leads to a local definition. *)
      Format.fprintf out_fmter "@[<2>%a :=@ "
        Parsetree_utils.pp_vname_with_operators_expanded vname;
      Expr_dk_generation.generate_expr
        ctx ~local_idents: [] ~in_recursive_let_section_of: []
        ~self_methods_status: Expr_dk_generation.SMS_abstracted
        ~recursive_methods_status: Expr_dk_generation.RMS_regular
        env expr ;
      Format.fprintf out_fmter " =>@ @]@\n"
;;

(* ************************************************************************* *)
(** {b Descr} : Just allows the function interfacing to zenon to generate
    their logical statement to prove either by a simple [logical_expr] or
    using the recursive calls information.
    In the first case, we are in a proof whose statement is an existing
    [logical_expr] in the AST.
    In the seconbd case, we are in a termination proof. In effect, in this
    case, there is no *real* [logical_expr] in the AST to express the
    termination property/statement. This property is generated in Dk on the
    fly by the function [Rec_let_gen.generate_termination_lemmas]. Hence in
    this case, functions used to interface with zenon don't have any
    [logical_expr] and must  use [Rec_let_gen.generate_termination_lemmas] to
    dump the required Dk code for Zenon.

    {b Rem}: Not exported outside this module.                               *)
(* ************************************************************************* *)
type zenon_statement_dk_generation_method =
  | ZSGM_from_logical_expr of Parsetree.logical_expr
;;

(** section_name_seed : the base of name to use if one need to open a fresh
    Section.
    available_hyps : Assoc list mapping any previously seen step onto its
    related logical expression it demonstrates and the name given to this
    lemma.
    Return new available steps to be added to the already known. Does NOT
    return the concatenation of fresh ones and already know. Hence, returns
    and EXTENSION of the steps that must be manually appended !
 *)
let zenonify_proof_node section_name_seed node aim_gen_method =
  match node.Parsetree.ast_desc with
   | Parsetree.PN_sub ((label_num, label_name), stmt, _) ->
       (begin
       let section_name = section_name_seed ^ "_" ^ label_name in
       let lemma_name = Parsetree.Vlident ("__" ^ section_name ^ "_LEMMA") in
       let stmt_desc = stmt.Parsetree.ast_desc in
       (* Finally, we deal with the conclusion of the statement. *)
       let new_aim =
         (match stmt_desc.Parsetree.s_concl with
          | None ->
              (begin
              match aim_gen_method with
               | ZSGM_from_logical_expr lexpr -> lexpr
              end)
          | Some logical_expr -> logical_expr) in
       let assumed_variables_and_lemmas =
         find_assumed_variables_and_lemmas_in_hyps stmt_desc.Parsetree.s_hyps in
       (* We return the extra step known thanks to the current [PN_sub] This
          extra step will be made available for the rest of the surrounding
          proof and sibling [PN_sub]/[PN_qed]. *)
       [{ psa_node_label = (label_num, label_name);
          psa_lemma_name = lemma_name;
          psa_base_logical_expr = new_aim;
          psa_assumed_variables_and_lemmas = assumed_variables_and_lemmas }]
       end)
   | Parsetree.PN_qed ((_label_num, _label_name), _) ->
       [(* No new extra step available. *)]

(* Assumed proof *)
let admit_zenon_theorem_for_proof ctx env min_dk_env
    aim_gen_method enforced_deps =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* [Unsure] Bad place to make the check. This should be made in something
     like "abstration.ml". Ensure that the *)
  ensure_enforced_dependencies_by_definition_are_definitions
    min_dk_env enforced_deps ;
  (* Now, print the lemma body. Inside, any method of "Self" is abstracted
     (without lambda-lift) and named "abst_xxx". That's why we use the mode
     [SMS_abstracted]. *)
  Format.fprintf out_fmter "(; Assumed proof node. ;)@\n";
  Format.fprintf out_fmter "@[<2>dk_builtins.magic_prove (";
  (* Generate the aim depending on if we are in a regular proof or in the
     initial stage of a termination proof. *)
  match aim_gen_method with
  | ZSGM_from_logical_expr aim ->
      Species_record_type_dk_generation.generate_logical_expr
        ~local_idents: [] ~in_recursive_let_section_of: []
        ~self_methods_status: Expr_dk_generation.SMS_abstracted
        ~recursive_methods_status: Expr_dk_generation.RMS_regular
        ctx env aim;
      Format.fprintf out_fmter ")@]@\n"

let zenonify_proof_node_print_before ctx print_ctx env
    section_name_seed node aim_gen_method =
  let out_fmter = ctx.Context.scc_out_fmter in
  match node.Parsetree.ast_desc with
   | Parsetree.PN_sub ((_, label_name), stmt, _) ->
       (begin
       let section_name = section_name_seed ^ "_" ^ label_name in
       let lemma_name = Parsetree.Vlident ("__" ^ section_name ^ "_LEMMA") in
       Format.fprintf out_fmter "@[<2>(; Section __%s. ;)@\n" section_name;
       Format.fprintf out_fmter "(%a :@ ("
                      Sourcify.pp_vname lemma_name;
       let stmt_desc = stmt.Parsetree.ast_desc in
       (* First, generate the hypotheses of the statement. We also recover the
          "Hypothesis" of the statement because we will need to print them
          again later if they are mentionned as used in a [F_hypothesis] .*)
       List.iter (zenonify_hyp ctx print_ctx env ~sep:"->") stmt_desc.Parsetree.s_hyps;
       (* Finally, we deal with the conclusion of the statement. *)
       let new_aim =
         (match stmt_desc.Parsetree.s_concl with
          | None ->
              (begin
              match aim_gen_method with
               | ZSGM_from_logical_expr lexpr -> lexpr
              end)
          | Some logical_expr -> logical_expr) in
       Format.fprintf out_fmter "dk_logic.eP (";
       Species_record_type_dk_generation.generate_logical_expr
         ~local_idents: [] ~in_recursive_let_section_of: []
         ~self_methods_status: Expr_dk_generation.SMS_abstracted
         ~recursive_methods_status: Expr_dk_generation.RMS_regular
         ctx env new_aim ;
       Format.fprintf out_fmter ")) =>@ ("
       end)
   | Parsetree.PN_qed _ -> ()

let rec zenonify_proof_node_print_after ~in_nested_proof ctx print_ctx env min_dk_env
    ~self_manifest dependencies_from_params generated_fields available_hyps
    available_steps section_name_seed parent_proof_opt node default_aim_name
    aim_gen_method =
  let out_fmter = ctx.Context.scc_out_fmter in
  match node.Parsetree.ast_desc with
   | Parsetree.PN_sub ((_, label_name), stmt, proof) ->
       (begin
           Format.fprintf out_fmter "))";
           let section_name = section_name_seed ^ "_" ^ label_name in
       let lemma_name = Parsetree.Vlident ("__" ^ section_name ^ "_LEMMA") in
       let stmt_desc = stmt.Parsetree.ast_desc in
       let available_hyps' = stmt_desc.Parsetree.s_hyps @ available_hyps in
       (* Finally, we deal with the conclusion of the statement. *)
       let new_aim =
         (match stmt_desc.Parsetree.s_concl with
          | None ->
              (begin
              match aim_gen_method with
               | ZSGM_from_logical_expr lexpr -> lexpr
              end)
          | Some logical_expr -> logical_expr) in
       Format.fprintf out_fmter "(";
       List.iter (zenonify_hyp ctx print_ctx env ~sep:"=>") stmt_desc.Parsetree.s_hyps;
       zenonify_proof
         ~in_nested_proof: true ~qed:false ctx print_ctx env min_dk_env
         ~self_manifest dependencies_from_params generated_fields
         available_hyps' available_steps section_name
         (ZSGM_from_logical_expr new_aim) lemma_name parent_proof_opt proof;
       Format.fprintf out_fmter ")@ ";
       Format.fprintf out_fmter "(; End __%s.@] ;)@\n" section_name
       end)
   | Parsetree.PN_qed ((_label_num, _label_name), proof) ->
       zenonify_proof ~in_nested_proof ~qed:true ctx print_ctx env min_dk_env
         ~self_manifest dependencies_from_params generated_fields available_hyps
         available_steps section_name_seed aim_gen_method default_aim_name
         parent_proof_opt proof


and zenonify_proof ~in_nested_proof ~qed ctx print_ctx env min_dk_env
    ~self_manifest dependencies_from_params generated_fields available_hyps
    available_steps section_name_seed aim_gen_method aim_name parent_proof_opt
    proof =
  let out_fmter = ctx.Context.scc_out_fmter in
  match proof.Parsetree.ast_desc with
   | Parsetree.Pf_coq (enforced_deps, _)
   | Parsetree.Pf_assumed enforced_deps ->
       admit_zenon_theorem_for_proof
         ctx env min_dk_env
         aim_gen_method enforced_deps ;
       (* Proof is assumed, then simply use "magic_prove". *)
       Format.fprintf out_fmter "(; Proof was flagged as assumed. ;)@\n";

   | Parsetree.Pf_dk (_, script) ->
       (* Dump verbatim the Dk code. *)
       Format.fprintf out_fmter "(%s)@\n" script;
   | Parsetree.Pf_node nodes ->
       (* For each successive node, we remember the previously seen **extra**
          steps that will be available for the trailing Qed node. *)
       let rec rec_dump accu_avail_steps = function
         | [] -> ()
         | node :: q ->
            begin
             let extra_avail_steps =
               zenonify_proof_node section_name_seed node aim_gen_method in
             zenonify_proof_node_print_before
                 ctx print_ctx env section_name_seed node aim_gen_method;
             rec_dump
               (* And not not append in the other way otherwise, the newly
                  found steps will be in tail of the list and of we look for
                  a step that has the same name than an older one, we will
                  find the older one and that's wrong ! (Exactly like just
                  above). *)
               (extra_avail_steps @ accu_avail_steps) q;
             zenonify_proof_node_print_after
                 ~in_nested_proof ctx print_ctx env min_dk_env
                 ~self_manifest dependencies_from_params generated_fields
                 available_hyps accu_avail_steps section_name_seed (Some proof)
                 node aim_name aim_gen_method
            end
       in
       rec_dump available_steps nodes
   | Parsetree.Pf_auto facts ->
       (* Generate Zenon's header. *)
       Format.fprintf out_fmter "@\n%%%%begin-auto-proof@\n";
       (* Location is not the while theorem, but its body instead. I think
          this is sufficient *)
       Format.fprintf out_fmter "%%%%location: [%a]@\n"
         Location.pp_location proof.Parsetree.ast_loc;
       Format.fprintf out_fmter "%%%%name: %a@\n"
         Parsetree_utils.pp_vname_with_operators_expanded aim_name;
       (* Tell Zenon to abstract over "Section" variables *)
       (* Each variable has to be printed on its line,
          hence the horizontal printing box *)
       (* Start with Type aliases because they belong to the header *)
       List.iter (
         function
         | SVTypeAlias (s, ty) ->
            Format.fprintf out_fmter "%%%%begin-type-alias: %s := %a@\n%%%%end-type-alias@\n"
                           s
                           (Dk_pprint.pp_type_simple_to_dk print_ctx) ty
         | _ -> ())
       !section_variable_list;
       (* Declare all is-param carrier types *)
       let is_params =
         List.map
           (function
             | _, (param_name, Types.CCMI_is) -> Some param_name
             | _, (_, Types.CCMI_in _) -> None)
           ctx.Context.scc_collections_carrier_mapping in
       List.iter
         (function None -> () | Some param ->
          Format.fprintf out_fmter "%s_T : cc.uT.@\n" param)
         is_params;
       (* Now all other section variables *)
       List.iter (
         function
         | SVTypeAlias _ -> ()
         | SVType vname -> Format.fprintf out_fmter "%s_T : cc.uT.@\n" vname
         | SVVar (prefix, vname, ty) ->
            Format.fprintf out_fmter
              "%s%a :@ %a.@\n"
              prefix Parsetree_utils.pp_vname_with_operators_expanded vname
              (Dk_pprint.pp_type_simple_to_dk_with_eps print_ctx) ty
         )
         (List.rev !section_variable_list);
       Format.fprintf out_fmter
         "@\n@\n@\n(; Methods to use for automated proof. ;)@\n";
       (* Now, print Definition and Hypothesis mentionned in the "by" clause
          without using the method generator. This means that one must
          "inline" the Definitions' bodies (in fact, for Hypothesis, since we
          have no body, only declaration, the inlining has no effect since they
          don't have method generator). If the list of facts is empty, this
          implicitly means "by all the previous steps of THIS proof level",
          i.e. by the [PN_sub]s of the closest [Pf_node] hosting us. And the
          closest [Pf_node] hosting us is in our parent proof. *)
       let real_facts =
         (match facts with
          | [] when qed ->
              let parent_proof_nodes =
                (match parent_proof_opt with
                 | None -> None
                 | Some p ->
                     match p.Parsetree.ast_desc with
                      | Parsetree.Pf_node ns -> Some ns
                      | _ -> assert false) in
              (* Make a pseudo list with all the encountered steps (node
                 labels). *)
              [{ Parsetree.ast_loc = proof.Parsetree.ast_loc;
                 Parsetree.ast_desc =
                   Parsetree.F_node (
                     match parent_proof_nodes with
                     | None -> []
                     | Some x -> find_only_PN_subs_in_proof_nodes x);
                 Parsetree.ast_annot = [];
                 Parsetree.ast_type = Parsetree.ANTI_irrelevant }]
          | _ -> facts) in
       List.iter
         (zenonify_fact
            ctx print_ctx env min_dk_env ~self_manifest
            dependencies_from_params available_hyps
            available_steps)
         real_facts;
       (* Now, print the lemma body. Inside, any method of "Self" is
          abstracted (without lambda-lift) and named "abst_xxx". That's why we
          use the mode [SMS_abstracted]. *)
       let aim = match aim_gen_method with ZSGM_from_logical_expr aim -> aim in
       zenonify_all_free_idents_from_logical_expr
         ctx print_ctx env out_fmter aim;
       Format.fprintf out_fmter "(; Theorem's body. ;)@\n";
       Format.fprintf out_fmter "%a : dk_logic.eP (@\n"
         Parsetree_utils.pp_vname_with_operators_expanded aim_name;
       (* Generate the aim depending on if we are in a regular proof or in the
          initial stage of a termination proof. *)
       Species_record_type_dk_generation.generate_logical_expr
         ~local_idents: [] ~in_recursive_let_section_of: []
         ~self_methods_status:
         Expr_dk_generation.SMS_abstracted
         ~recursive_methods_status:
         Expr_dk_generation.RMS_regular
         ctx env aim;
       Format.fprintf out_fmter ").@\n" ;
       (* End of Zenon stuff. *)
       Format.fprintf out_fmter "%%%%end-auto-proof@\n";
       (* Now, let's print the theorem/lemma and prove it unless we are at
          toplevel (i.e. not in a nested proof). In this last case, this will
          be done directly by [generate_defined_theorem]. *)
;;


(* ************************************************************************* *)
(* {b Descr} : This function generates the Dk Section needed by Zenon ONLY
      if the theorem has to be proved by Zenon. Otherwise, do nothing.
      It first dump Variables, Let and Hypothesis for all the things we
      usually lambda-lift in a regular definition of method. They represent
      thing that were abstracted in the theorem.
      Next, it prints the Zenon header.
      Next, since the current collection carrier mapping is empty, it re-build
      it so that following definitions will map carriers onto Variables
      generated at the previous step.
      Next the theorem's body is printed.
      Finally, we generate definitions required by Zenon for the facts of the
      proof.
      And we end the Section.

   {b Rem} : Not exported outside this module.
      This function generates the Dk Section needed by Zenon ONLY
      if the theorem has to be proved by Zenon. Otherwise, do nothing.       *)
(* ************************************************************************* *)
let generate_theorem_section_if_by_zenon ctx print_ctx env min_dk_env
    ~self_manifest used_species_parameter_tys dependencies_from_params
    generated_fields name logical_expr_or_term_stuff proof =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Local function that prints the common stuff required by Zenon in case
     of proofs done by [Pf_auto] or [Pf_node]. It prints the opening of the
     main Section and the theorem. *)
  let print_common_prelude_for_zenon () =
    Format.fprintf out_fmter "@[<2>(; Section for proof of theorem '%a'. ;)@\n"
      Parsetree_utils.pp_vname_with_operators_expanded name;
    (* Start the Section. *)
    section_variable_list := [];
    (* We must now dump Variables, Let and Hypothesis for all the things we
       usually lambda-lift in a regular definition of method. This is due to
       the fact that here we still use the Section mechanism. Hence, we do
       the same job than for regular field definition prelude but changing
       abstractions that are performed by extra parameters by Variable, Let
       or Hypothesis. *)
    generate_field_definition_prelude_true min_dk_env dependencies_from_params
  in
  (* *********************** *)
  (* Start really the job... *)
  match proof.Parsetree.ast_desc with
   | Parsetree.Pf_assumed _ | Parsetree.Pf_coq _ | Parsetree.Pf_dk _ ->
       () (* No Section needed. *)
   | Parsetree.Pf_node _ | Parsetree.Pf_auto _ ->
       (begin
       (* Generate the common code for proofs done by Zenon either by [Pf_auto]
          of by [Pf_node]. *)
       print_common_prelude_for_zenon () ;
       (* Get the stuff to add to the current collection carrier mapping to
          make so the type expressions representing some species parameter
          carrier types, will be automatically be mapped onto our freshly
          created extra args. The trailing "_T" will be automatically added
          by the type printing routine.
          In fact, thsi process is already done by the function
          [generate_field_definition_prelude] but we don't remind it. So we need
          to do it again. That's not efficient, but it's not a big deal. *)
       let cc_mapping_extension =
         List.map
           (fun species_param_type_name ->
             let as_string =
               Parsetree_utils.vname_as_string_with_operators_expanded
                 species_param_type_name in
             let param_name =  "_p_" ^ as_string in
             (* Return the stuff to extend the collection_carrier_mapping. *)
             ((ctx.Context.scc_current_unit, as_string),
              (param_name, Types.CCMI_is)))
           used_species_parameter_tys in
       (* Overwrite the [ctx] and [print_ctx]. *)
       let ctx = { ctx with
         Context.scc_collections_carrier_mapping =
         cc_mapping_extension @ ctx.Context.scc_collections_carrier_mapping } in
       let print_ctx = {
         print_ctx with
           Dk_pprint.dpc_collections_carrier_mapping =
             cc_mapping_extension @
               print_ctx.Dk_pprint.dpc_collections_carrier_mapping } in
       (* Create a unique name seed for Sections of this theorem. *)
       let section_name_seed =
         Handy.string_uppercase_ascii (Handy.int_to_base_26 (section_gen_sym ())) in
       (* Handle the proof, telling not to print the Theorem's body once the
          auto-proof is ended because this will be done directly by
          [generate_defined_theorem]. *)
       ignore
         (zenonify_proof ~in_nested_proof: false ~qed: true ctx print_ctx env
            min_dk_env ~self_manifest dependencies_from_params generated_fields
            [(* No available hypothesis at the beginning. *)]
            [(* No available steps at the beginning. *)] section_name_seed
            logical_expr_or_term_stuff name
            None (* No parent proof at the beginning. *)
            proof) ;
       (* End the Section. *)
       Format.fprintf out_fmter "@].@\n(; End Proof_of_%a. ;)@\n@\n"
         Parsetree_utils.pp_vname_with_operators_expanded name
       end)
;;



let generate_theorem_body ctx print_ctx env min_dk_env ~self_manifest
    used_species_parameter_tys dependencies_from_params generated_fields
    name logical_expr proof =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* End the proof matter. *)
  (match proof.Parsetree.ast_desc with
   | Parsetree.Pf_assumed _ | Parsetree.Pf_coq _ ->
       (* Proof assumed, then simply use "magic_prove". *)
       Format.fprintf out_fmter "(; Proof was flagged as assumed ;)@\n";
       Format.fprintf out_fmter "dk_builtins.magic_prove@ (";
       (* Print again the theorem *)
       Species_record_type_dk_generation.generate_logical_expr
         ~local_idents: [] ~in_recursive_let_section_of: []
         ~self_methods_status: Expr_dk_generation.SMS_abstracted
         ~recursive_methods_status: Expr_dk_generation.RMS_regular
         ctx env logical_expr ;
       Format.fprintf out_fmter ").@\n"
   | Parsetree.Pf_auto _  | Parsetree.Pf_node _ ->
       (* Proof done by Zenon. *)
       generate_theorem_section_if_by_zenon
         ctx print_ctx env min_dk_env ~self_manifest
         used_species_parameter_tys
         dependencies_from_params generated_fields name
         (ZSGM_from_logical_expr logical_expr) proof;
       Format.fprintf out_fmter "@\n"
   | Parsetree.Pf_dk (_, script) ->
       (* Dump verbatim the Dk code. *)
       Format.fprintf out_fmter "%s@\n" script)
;;

let toplevel_theorem_compile ctx env theorem_def =
  let theorem_desc = theorem_def.Parsetree.ast_desc in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating Dedukti code for toplevel theorem %a@."
      Sourcify.pp_vname theorem_desc.Parsetree.th_name ;
  (* Make a print context with an empty mapping since we are at toplevel. *)
  let print_ctx = {
    Dk_pprint.dpc_current_unit = ctx.Context.scc_current_unit ;
    Dk_pprint.dpc_current_species = None ;
    Dk_pprint.dpc_collections_carrier_mapping =
      ctx.Context.scc_collections_carrier_mapping } in
  let out_fmter = ctx.Context.scc_out_fmter in
  Format.fprintf out_fmter "@[<2>def %a@ "
    Parsetree_utils.pp_vname_with_operators_expanded theorem_desc.Parsetree.th_name;
  Format.fprintf out_fmter " :@ dk_logic.eP@ (" ;
  Species_record_type_dk_generation.generate_logical_expr
    ~local_idents: [] ~in_recursive_let_section_of: []
    ~self_methods_status: Expr_dk_generation.SMS_abstracted
    ~recursive_methods_status: Expr_dk_generation.RMS_regular
    ctx env theorem_desc.Parsetree.th_stmt ;
  Format.fprintf out_fmter ")";
  Format.fprintf out_fmter " :=@\n";
  Format.fprintf out_fmter "@]@\n";
  generate_theorem_body
    ctx print_ctx env []
    ~self_manifest: None [] [] []
    theorem_desc.Parsetree.th_name theorem_desc.Parsetree.th_stmt
    theorem_desc.Parsetree.th_proof
;;
