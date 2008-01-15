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

(* $Id: base_exprs_ml_generation.ml,v 1.16 2008-01-15 13:46:40 pessaux Exp $ *)


(* ************************************************************************** *)
(** {b Descr} : This module performs the compilation from FoCaL to Ocaml
      of basic FoCaL expressions, i.e. of the core constructs that don't
      deal with species and collections. More accurately, this includes
      the let, let rec constructs and the simple expressions
      ([Parsetree.expr]) including their sub-parts (patterns, and constants). *)
(* ************************************************************************** *)



(* ************************************************************************* *)
(* Format.formatter -> Parsetree.constant -> unit                            *)
(** {b Descr} : Generate the OCaml code from a FoCaL constant expression.

    {b Args} :
      - [out_fmter] : The out channel where to generate the OCaml source
                    code.
      - [constant] : The constant expression to translate into OCaml source
          code.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let generate_constant out_fmter constant =
  match constant.Parsetree.ast_desc with
   | Parsetree.C_int i -> Format.fprintf out_fmter "%s" i
   | Parsetree.C_float fl -> Format.fprintf out_fmter "%s" fl
   | Parsetree.C_bool b -> Format.fprintf out_fmter "%s" b
   | Parsetree.C_string s ->
       Format.fprintf out_fmter "\"%s\"" (String.escaped s)
   | Parsetree.C_char c -> Format.fprintf out_fmter "'%c'" c
;;



(* *********************************************************************** *)
(* Misc_ml_generation.reduced_compil_context ->                            *)
(*   local_idents: Parsetree.vname list -> Parsetree.expr_ident -> unit    *)
(** {b Descr} : Generate the OCaml code from a FoCaL [ident] in the
              context of method generator generation.

              The most tricky stuff is dealing with [ident] denoting
              methods (i.e. "!-ed" identifiers).
              If the method [ident] corresponds to a parameter's method
              then we use the lambda-lifted principle. Otherwise, this
              ident is considered as from something not abstracted, coming
              from a toplevel collection. In this case, it is printed as
              a field coming from the collection's effective value
              (i.e. field "effective_collection" of the collection value)
              with its scoping qualification if there is some.

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let generate_expr_ident_for_method_generator ctx ~local_idents ident =
  let out_fmter = ctx.Misc_ml_generation.rcc_out_fmter in
  match ident.Parsetree.ast_desc with
   | Parsetree.EI_local vname ->
       (begin
       (* Thanks to the scoping pass, identifiers remaining "local" are *)
       (* either really let-bound in the context of the expression,     *)
       (* hence have a direct mapping between FoCaL and OCaml code, or  *)
       (* species "in"-parameters and then must be mapped onto the      *)
       (* lambda-lifted parameter introduced for it in the context of   *)
       (* the current species.                                          *)
       (* Be careful, because a recursive method called in its body is  *)
       (* scoped AS LOCAL ! And because recursive methods do not have   *)
       (* dependencies together, there is no way to recover the extra   *)
       (* parameters to apply to them in this configuration. Hence, to  *)
       (* avoid forgetting these extra arguments, we must use here the  *)
       (* information recorded in the context, i.e. the extra arguments *)
       (* of the recursive functions.                                   *)
       (* To check if a "smelling local" identifier is really local or  *)
       (* a "in"-parameter of the species, we use the same reasoning    *)
       (* that in [param_dep_analysis.ml]. Check justification over     *)
       (* there ! *)
       if (List.mem vname
             ctx.Misc_ml_generation.rcc_species_parameters_names) &&
         (not (List.mem vname local_idents)) then
         (begin
         (* In fact, a species "in"-parameter. This parameter was of the *)
         (* form "foo in C". Then it's naming scheme will be "_p_" +     *)
         (* the species parameter's name + the method's name that is     *)
         (* trivially the parameter's name again (because this last one  *)
         (* is computed as the "stuff" a dependency was found on, and in *)
         (* the case of a "in"-parameter, the dependency can only be on  *)
         (* the parameter's value itself, not on any method since there  *)
         (* is none !).                                                  *)
         Format.fprintf out_fmter "_p_%a_%a"
           Parsetree_utils.pp_vname_with_operators_expanded vname
           Parsetree_utils.pp_vname_with_operators_expanded vname
         end)
       else
         (begin
         (* Really a local identifier or a call to a recursive method. *)
         Format.fprintf out_fmter "%a"
           Parsetree_utils.pp_vname_with_operators_expanded vname ;
         (* Because this method can be recursive, we must apply it to     *)
         (* its extra parameters if it has some, except "rep" because     *)
         (* dependencies on the carrier are ignored in OCaml. In the      *)
         (* compilation context's field [rcc_lambda_lift_params_mapping], *)
         (* we directly recorded the positionnal list of extra parameters *)
         (* names of recursive functions. Then we must filter "abst_rep"  *)
         (* to ignore extra parameter induced by a dependency on the      *)
         (* carrier.                                                      *)
         try
           let extra_args =
             List.assoc
               vname ctx.Misc_ml_generation.rcc_lambda_lift_params_mapping in
           List.iter
             (fun (s, _) ->
               (* Don't print types in OCaml to prevent being to verbose. *)
               if s <> "abst_rep" then Format.fprintf out_fmter "@ %s" s)
             extra_args
            with Not_found -> ()
         end)
       end)
   | Parsetree.EI_global (Parsetree.Vname _) ->
       (* In this case, may be there is some scoping process missing. *)
       assert false
   | Parsetree.EI_global (Parsetree.Qualified (mod_name, vname)) ->
       (* Call the OCaml corresponding identifier in the corresponding *)
       (* module (i.e. the capitalized [mod_name]). If the module is   *)
       (* the currently compiled one, then do not qualify the          *)
       (* identifier.                                                  *)
       if mod_name <> ctx.Misc_ml_generation.rcc_current_unit then
         Format.fprintf out_fmter "%s.%a"
           (String.capitalize mod_name)
           Parsetree_utils.pp_vname_with_operators_expanded vname
       else
         Format.fprintf out_fmter "%a"
           Parsetree_utils.pp_vname_with_operators_expanded vname
   | Parsetree.EI_method (coll_specifier_opt, vname) ->
       (begin
       match coll_specifier_opt with
        | None
        | Some (Parsetree.Vname (Parsetree.Vuident "Self")) ->
            (begin
            (* Method call from the current species. This corresponds to *)
            (* a call to the corresponding lambda-lifted method that is  *)
            (* represented as an extra parameter of the OCaml function.  *)
            Format.fprintf out_fmter "abst_%a"
              Parsetree_utils.pp_vname_with_operators_expanded vname
            end)
        | Some coll_specifier ->
            (begin
            match coll_specifier with
             | Parsetree.Vname coll_name ->
                 (begin
                 (* Method call from a species that is not the current but  *)
                 (* is implicitely in the current compilation unit. May be  *)
                 (* either a paramater or a toplevel defined collection.    *)
                 if List.mem
                     coll_name
                     ctx.Misc_ml_generation.rcc_species_parameters_names then
                   (begin
                   (* It comes from a parameter. To retrieve the related *)
                   (* method name we build it the same way we built it   *)
                   (* while generating the extra OCaml function's        *)
                   (* parameters due to depdencencies coming from the    *)
                   (* species parameter. I.e: "_p_", followed by the     *)
                   (* species parameter name, followed by "_", followed  *)
                   (* by the method's name.                              *)
                   let prefix =
                     "_p_" ^ (Parsetree_utils.name_of_vname coll_name) ^
                     "_" in
                   Format.fprintf out_fmter "%s%a"
                     prefix
                     Parsetree_utils.pp_vname_with_operators_expanded vname
                   end)
                 else
                   (begin
                   (* It comes from a toplevel stuff, hence not abstracted *)
                   (* by lambda-lifting.                                   *)
                   Format.fprintf out_fmter
                     "%a.effective_collection.%a.%a"
                     Parsetree_utils.pp_vname_with_operators_expanded coll_name
                     Parsetree_utils.pp_vname_with_operators_expanded coll_name
                     Parsetree_utils.pp_vname_with_operators_expanded vname
                   end)
                 end)
             | Parsetree.Qualified (module_name, coll_name) ->
                 (begin
                 if module_name = ctx.Misc_ml_generation.rcc_current_unit then
                   (begin
                   (* Exactly like when it is method call from a species that *)
                   (* is not the current but is implicitely in the current    *)
                   (* compilation unit : the call is performed to a method    *)
                   (* a species that is EXPLICITELY in the current            *)
                   (* compilation unit.                                       *)
                   if List.mem
                       coll_name
                       ctx.Misc_ml_generation.rcc_species_parameters_names then
                     (begin
                     let prefix =
                       "_p_" ^ (Parsetree_utils.name_of_vname coll_name) ^
                       "_" in
                     Format.fprintf out_fmter "%s%a"
                       prefix Parsetree_utils.pp_vname_with_operators_expanded
                       vname
                       end)
                   else
                     (begin
                     Format.fprintf out_fmter
                       "%a.effective_collection.%a.%a"
                       Parsetree_utils.pp_vname_with_operators_expanded
                       coll_name
                       Parsetree_utils.pp_vname_with_operators_expanded
                       coll_name
                       Parsetree_utils.pp_vname_with_operators_expanded
                       vname
                     end)
                   end)
                 else
                   (begin
                   (* The called method belongs to a species that is not    *)
                   (* ourselves and moreover belongs to another compilation *)
                   (* unit. May be a species from the toplevel of another   *)
                   (* FoCaL source file.                                    *)
                   failwith "generate_expr_ident_for_method_generator foreign species's module TODO"
                   end)
                 end)
            end)
       end)
;;



(* ******************************************************************** *)
(* Misc_ml_generation.reduced_compil_context ->                         *)
(*   Env.MlGenEnv.t -> Parsetree.constructor_ident -> unit              *)
(** {b Descr} : Generate the OCaml code from a FoCaL [constructor_expr]
              in the context of method generator generation.
              The translation of the FoCaL sum constructor is obtained
              from the code generation environment. If this environment
              doesn't contain a binding for the constructor, this means
              that this constructor directly corresponds to its FoCaL
              name in the OCaml code.

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
let generate_constructor_ident_for_method_generator ctx env cstr_expr =
  try
    let mapping_info =
      Env.MlGenEnv.find_constructor
        ~loc: cstr_expr.Parsetree.ast_loc
        ~current_unit: ctx.Misc_ml_generation.rcc_current_unit
        cstr_expr env in
    let (_, ocaml_binding) =
      try
        List.find
          (function
            | (Parsetree.EL_Caml, _) -> true
            | (Parsetree.EL_Coq, _)
            | ((Parsetree.EL_external _), _) -> false)
          mapping_info
      with Not_found ->
        (* No OCaml mapping found. *)
        raise
          (Externals_ml_generation.No_external_constructor_caml_def
             cstr_expr) in
    (* Now directly generate the name the constructor is mapped onto. *)
    Format.fprintf ctx.Misc_ml_generation.rcc_out_fmter "%s" ocaml_binding
  with
  | Env.Unbound_constructor (_, _) ->
      (begin
      match cstr_expr.Parsetree.ast_desc with
       | Parsetree.CI (Parsetree.Vname name) ->
           Format.fprintf ctx.Misc_ml_generation.rcc_out_fmter "%a"
             Parsetree_utils.pp_vname_with_operators_expanded name
       | Parsetree.CI (Parsetree.Qualified (fname, name)) ->
           (* If the constructor belongs to the current      *)
           (* compilation unit then one must not qualify it. *)
           if fname <> ctx.Misc_ml_generation.rcc_current_unit then
             Format.fprintf ctx.Misc_ml_generation.rcc_out_fmter "%s.%a"
               (String.capitalize fname)
               Parsetree_utils.pp_vname_with_operators_expanded name
           else
             Format.fprintf ctx.Misc_ml_generation.rcc_out_fmter "%a"
               Parsetree_utils.pp_vname_with_operators_expanded name
      end)
;;



let generate_pattern ctx env pattern =
  let out_fmter = ctx.Misc_ml_generation.rcc_out_fmter in
  let rec rec_gen_pat pat =
    match pat.Parsetree.ast_desc with
     | Parsetree.P_const constant -> generate_constant out_fmter constant
     | Parsetree.P_var name ->
         Format.fprintf out_fmter "%a"
           Parsetree_utils.pp_vname_with_operators_expanded name
     | Parsetree.P_as (p, name) ->
         Format.fprintf out_fmter "(" ;
         rec_gen_pat p ;
         Format.fprintf out_fmter "as@ %a)"
           Parsetree_utils.pp_vname_with_operators_expanded name
     | Parsetree.P_wild -> Format.fprintf out_fmter "_"
     | Parsetree.P_constr (ident, pats) ->
         (begin
         generate_constructor_ident_for_method_generator ctx env ident ;
         (* Discriminate on the umber of arguments *)
         (* to know if parens are needed.          *)
         match pats with
          | [] -> ()
          | _ ->
              Format.fprintf out_fmter " (" ;
              rec_generate_pats_list pats ;
              Format.fprintf out_fmter ")"
         end)
     | Parsetree.P_record _labs_pats ->
         Format.eprintf "generate_pattern P_record TODO@."
     | Parsetree.P_tuple pats ->
         Format.fprintf out_fmter "(@[<1>" ;
         rec_generate_pats_list pats ;
         Format.fprintf out_fmter ")@]"
     | Parsetree.P_paren p ->
         Format.fprintf out_fmter "(@[<1>" ;
         rec_gen_pat p ;
         Format.fprintf out_fmter ")@]"


  and rec_generate_pats_list = function
    | [] -> ()
    | [last] -> rec_gen_pat last
    | h :: q ->
        rec_gen_pat h ;
        Format.fprintf out_fmter ",@ " ;
        rec_generate_pats_list q in
  (* ********************** *)
  (* Now, let's do the job. *)
  rec_gen_pat pattern
;;



let rec let_binding_compile ctx ~local_idents env bd opt_sch =
  let out_fmter = ctx.Misc_ml_generation.rcc_out_fmter in
  (* Generate the bound name. *)
  Format.fprintf out_fmter "%a"
    Parsetree_utils.pp_vname_with_operators_expanded
    bd.Parsetree.ast_desc.Parsetree.b_name ;
  (* Generate the parameters if some, with their type constraints. *)
  let params_names = List.map fst bd.Parsetree.ast_desc.Parsetree.b_params in
  (* We ignore the result type of the "let" if it's a function because *)
  (* we never print the type constraint on the result of the "let". We *)
  (* only print them in the arguments of the let-bound ident.          *)
  (* We also ignore the variables used to instanciate the polymorphic  *)
  (* ones of the scheme because in OCaml polymorphism is not explicit. *)
  let (params_with_type, _, _) =
    Misc_ml_generation.bind_parameters_to_types_from_type_scheme
      opt_sch params_names in
  (* We are printing each parameter's type. These types in fact belong *)
  (* to a same type scheme. Hence, they may share variables together.  *)
  (* For this reason, we first purge the printing variable mapping and *)
  (* after, activate its persistence between each parameter printing.  *)
  Types.purge_type_simple_to_ml_variable_mapping () ;
  List.iter
    (fun (param_vname, pot_param_ty) ->
      match pot_param_ty with
       | Some param_ty ->
           Format.fprintf out_fmter "@ (%a : %a)"
             Parsetree_utils.pp_vname_with_operators_expanded param_vname
             (Types.pp_type_simple_to_ml
                ~current_unit: ctx.Misc_ml_generation.rcc_current_unit
                ~reuse_mapping: true
                ctx.Misc_ml_generation.rcc_collections_carrier_mapping)
             param_ty
       | None ->
           Format.fprintf out_fmter "@ %a"
             Parsetree_utils.pp_vname_with_operators_expanded param_vname)
    params_with_type ;
  (* Now we don't need anymore the sharing. Hence, clean it. This should not *)
  (* be useful because the other guys usign printing should manage this      *)
  (* themselves (as we did just above by cleaning before activating the      *)
  (* sharing), but anyway, it is safer an not costly. So...                  *)
  Types.purge_type_simple_to_ml_variable_mapping () ;
  (* Output now the "=" sign ending the OCaml function's "header".    *)
  (* With a NON-breakable space before to prevent uggly hyphenation ! *)
  Format.fprintf out_fmter " =@ " ;
  (* Here, each parameter name of the binding may mask a "in"-parameter. *)
  let local_idents' = params_names @ local_idents in
  (* Now, let's generate the bound body. *)
  generate_expr ctx ~local_idents: local_idents' env
    bd.Parsetree.ast_desc.Parsetree.b_body



(* ********************************************************************* *)
(* Misc_ml_generation.reduced_compil_context -> Env.MlGenEnv.t ->        *)
(*   local_idents: Parsetree.vname list -> Parsetree.let_def ->          *)
(*     Types.type_scheme list -> unit                                    *)
(** {b Desrc} : Generates the OCaml code for a FoCaL "let"-definition.

    {b Args} :
      - [out_fmter] : The out channel where to generate the OCaml source
              code.

      - [local_idents] : The list of known local identifiers in the
          scope of this "let" EXPRESSION.

      - [env] : The current compilation environment allowing to know on
          what to map sum constructors and record field names.

      - [let_def] : The [Parsetree.let_def] structure representing the
          "let-definition" for which th generate the OCaml source code.

      - [bound_schemes] : The list of types schemes of the identifiers
          bound to the "let-definition" (i.e. several if the definition
          is a "rec", hence binds several identifiers).
          In effect, because we do not have directly inside the
          [Parsetree.let_def] these schemes, in order to be able to
          generate the type constraints of each components of the
          "let-definition", we must take these schemes aside.
          It is sometimes impossible yet to have this information.
          In this case, no type constraint will be added to the
          parameter of the bound identifiers.

    {b Rem} : Not exported outside this module.                          *)
(* ********************************************************************* *)
and let_def_compile ctx ~local_idents env let_def bound_schemes =
  let out_fmter = ctx.Misc_ml_generation.rcc_out_fmter in
  (* Generates the binder ("rec" or non-"rec"). *)
  Format.fprintf out_fmter "@[<2>let%s@ "
    (match let_def.Parsetree.ast_desc.Parsetree.ld_rec with
     | Parsetree.RF_no_rec -> ""
     | Parsetree.RF_rec -> " rec"   (* NON-breakable space in front. *)) ;
  (* Now generate each bound definition. *)
  (match (let_def.Parsetree.ast_desc.Parsetree.ld_bindings, bound_schemes) with
   | ([], []) ->
       (* The "let" construct should always at least bind one identifier ! *)
       assert false
   | ([one_bnd], [one_scheme]) ->
       let_binding_compile ctx ~local_idents env one_bnd one_scheme
   | ((first_bnd :: next_bnds), (first_scheme :: next_schemes)) ->
       let_binding_compile ctx ~local_idents env first_bnd first_scheme ;
       List.iter2
         (fun binding scheme ->
           Format.fprintf out_fmter "@]@\n@[<2>and " ;
           let_binding_compile ctx ~local_idents env binding scheme)
         next_bnds
         next_schemes
   | (_, _) ->
       (* Because the FoCaL has been parsed and typechecked, we must never    *)
       (* have a different number of bound identifiers and bound-identifiers' *)
       (* type schemes. If this arise, then we have a serious bug somewhere.  *)
       assert false) ;
  Format.fprintf out_fmter "@]"



(* ******************************************************************** *)
(* Misc_ml_generation.reduced_compil_context ->                         *)
(*   local_idents: Parsetree.vname list ->  Env.MlGenEnv.t ->           *)
(*     Parsetree.expr -> unit                                           *)
(** {b Descr} : Generate the OCaml code from a FoCaL expression.

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
and generate_expr ctx ~local_idents env initial_expression =
  let out_fmter = ctx.Misc_ml_generation.rcc_out_fmter in
  let rec rec_generate loc_idents expr =
    (* Generate the source code for the expression. *)
    match expr.Parsetree.ast_desc with
     | Parsetree.E_self ->
         (* [Unsure] D'ailleurs, est-ce possible en fait ? *)
         failwith "generate_expr E_self TODO"
     | Parsetree.E_const cst -> generate_constant out_fmter cst
     | Parsetree.E_fun (args_names, body) ->
         List.iter
           (fun n ->
             Format.fprintf out_fmter "@[<2>fun@ %a@ ->@ "
               Parsetree_utils.pp_vname_with_operators_expanded n)
           args_names ;
         (* Here, the function parameter name may mask a "in"-parameter. *)
         let loc_idents' = args_names @ loc_idents in
         rec_generate loc_idents' body ;
         Format.fprintf out_fmter "@]"
     | Parsetree.E_var ident ->
         generate_expr_ident_for_method_generator
           ctx ~local_idents: loc_idents ident
     | Parsetree.E_app (expr, exprs) ->
         Format.fprintf out_fmter "@[<2>(" ;
         rec_generate loc_idents expr ;
         Format.fprintf out_fmter "@ " ;
         rec_generate_exprs_list ~comma: false loc_idents exprs ;
         Format.fprintf out_fmter ")@]"
     | Parsetree.E_constr (cstr_expr, exprs) ->
         (begin
         generate_constructor_ident_for_method_generator ctx env cstr_expr ;
         match exprs with
          | [] -> ()
          | _ ->
              (* If argument(s), enclose by parens to possibly make a tuple. *)
              Format.fprintf out_fmter "@ @[<1>(" ;
              rec_generate_exprs_list ~comma: true loc_idents exprs ;
              Format.fprintf out_fmter ")@]"
         end)
     | Parsetree.E_match (matched_expr, pats_exprs) ->
         (begin
         Format.fprintf out_fmter "@[<1>match " ;
         rec_generate loc_idents matched_expr ;
         Format.fprintf out_fmter " with" ;
         List.iter
           (fun (pattern, expr) ->
             (* My indentation style: indent of 4 between *)
             (* the pattern and its related processing.   *)
             Format.fprintf out_fmter "@\n@[<4>| " ;
             generate_pattern ctx env pattern ;
             (* Enclose each match-case by begin/end to ensure no confusion. *)
             Format.fprintf out_fmter " ->@\n(begin@\n" ;
             (* Here, each name of the pattern may mask a "in"-parameter. *)
             let loc_idents' =
               (Parsetree_utils.get_local_idents_from_pattern pattern) @
               loc_idents in
             rec_generate loc_idents' expr ;
             Format.fprintf out_fmter "@\nend)@]")
           pats_exprs ;
         Format.fprintf out_fmter "@]"
         end)
     | Parsetree.E_if (expr1, expr2, expr3) ->
         Format.fprintf out_fmter "@[<2>if@ " ;
         rec_generate loc_idents expr1 ;
         Format.fprintf out_fmter "@ @[<2>then@ @]" ;
         rec_generate loc_idents expr2 ;
         Format.fprintf out_fmter "@ @[<2>else@ @]" ;
         rec_generate loc_idents expr3 ;
         Format.fprintf out_fmter "@]"
     | Parsetree.E_let (let_def, in_expr) ->
         (* Here we do not have type contraints under the hand. So give-up  *)
         (* generating such constraints. Just generate the raw code for the *)
         (* "let-definition".                                               *)
         let bound_schemes =
           List.map
             (fun _ -> None)
             let_def.Parsetree.ast_desc.Parsetree.ld_bindings in
         let_def_compile
           ctx ~local_idents: loc_idents env let_def bound_schemes ;
         Format.fprintf out_fmter "@ in@\n" ;
         rec_generate loc_idents in_expr
     | Parsetree.E_record labs_exprs ->
         (begin
         Format.fprintf out_fmter "@[<1>{@ " ;
         rec_generate_record_field_exprs_list loc_idents labs_exprs ;
         Format.fprintf out_fmter "@ }@]"
         end)
     | Parsetree.E_record_access (expr, label_name) ->
         (begin
         Format.fprintf out_fmter "@[<2>" ;
         rec_generate loc_idents expr ;
         Format.fprintf out_fmter ".@," ;
         rec_generate_one_record_field_name label_name ;
         Format.fprintf out_fmter "@]" ;
         end)
     | Parsetree.E_record_with (expr, labs_exprs) ->
         (begin
         (* Because in OCaml the with construct only starts by an ident, we *)
         (* create a temporary ident to bind the expression to an ident.    *)
         Format.fprintf out_fmter "@[<2>let __foc_tmp_with_ =@ " ;
         rec_generate loc_idents expr ;
         Format.fprintf out_fmter "@ in@] " ;
         (* Now really generate the "with"-construct. *)
         Format.fprintf out_fmter "@[<2>{ __foc_tmp_with_ with@\n" ;
         List.iter
           (fun (label_name, field_expr) ->
             Format.fprintf out_fmter "%a =@ "
               Misc_ml_generation.pp_to_ocaml_label_ident label_name ;
             rec_generate loc_idents field_expr ;
             Format.fprintf out_fmter " ;")
           labs_exprs ;
         Format.fprintf out_fmter "@ }@]"
         end)
     | Parsetree.E_tuple exprs ->
         (begin
         match exprs with
          | [] -> assert false
          | [one] -> rec_generate loc_idents one
          | _ ->
              Format.fprintf out_fmter "@[<1>(" ;
              rec_generate_exprs_list ~comma: true loc_idents exprs ;
              Format.fprintf out_fmter ")@]"
         end)
     | Parsetree.E_external external_expr ->
         (begin
         try
           (* Simply a somewhat of verbatim stuff of the OCaml translation. *)
           let (_, ocaml_binding) =
             List.find
               (function
                 | (Parsetree.EL_Caml, _) -> true
                 | (Parsetree.EL_Coq, _)
                 | ((Parsetree.EL_external _), _) -> false)
               external_expr.Parsetree.ast_desc in
           Format.fprintf out_fmter "%s" ocaml_binding
         with Not_found ->
           (* No OCaml mapping found. *)
           raise
             (Externals_ml_generation.No_external_value_caml_def
                ((Parsetree.Vlident "<expr>"), expr.Parsetree.ast_loc))
         end)
     | Parsetree.E_paren e -> rec_generate loc_idents e



  and rec_generate_exprs_list ~comma loc_idents = function
    | [] -> ()
    | [last] -> rec_generate loc_idents last
    | h :: q ->
        rec_generate loc_idents h ;
        if comma then Format.fprintf out_fmter ",@ "
        else Format.fprintf out_fmter "@ " ;
        rec_generate_exprs_list ~comma loc_idents q


  and rec_generate_record_field_exprs_list loc_idents = function
    | [] -> ()
    | [(label, last)] ->
        rec_generate_one_record_field_name label ;
        Format.fprintf out_fmter " =@ " ;
        rec_generate loc_idents last
    | (h_label, h_expr) :: q ->
        rec_generate_one_record_field_name h_label ;
        Format.fprintf out_fmter " =@ " ;
        rec_generate loc_idents h_expr ;
        Format.fprintf out_fmter " ;@ " ;
        rec_generate_record_field_exprs_list loc_idents q


  (* ********************************************************************** *)
  (* Parsetree.label_ident -> unit                                          *)
  (** {b Descr} : Local function to the general expression generation that
         generates one record field name according to whether it has a
         special translation to OCaml because it is external (hence it is
         bound in the code generation environment) or it is straightly
         translated as a regular FoCaL record label.
         Makeking this function prevents from writing several times the
         code to access the environment and determine if we write the
         label with translation of directly depending of the access result.

      {b Rem} : Not exported outside this module. Local to the function
         [generate_expr].                                                   *)
  (* ********************************************************************** *)
  and rec_generate_one_record_field_name label =
    try
      let mapping_info =
        Env.MlGenEnv.find_label
          ~loc: label.Parsetree.ast_loc
          ~current_unit: ctx.Misc_ml_generation.rcc_current_unit
          label env in
      let (_, ocaml_binding) =
        try
          List.find
            (function
              | (Parsetree.EL_Caml, _) -> true
              | (Parsetree.EL_Coq, _)
              | ((Parsetree.EL_external _), _) -> false)
            mapping_info
        with Not_found ->
          (* No OCaml mapping found. *)
          raise (Externals_ml_generation.No_external_field_caml_def label) in
      Format.fprintf out_fmter "%s" ocaml_binding
    with
    | Env.Unbound_label (_, _) ->
        (* If no binding for the field name in the environment, *)
        (* then it get's directly mapped onto its FoCaL name.   *)
        Format.fprintf out_fmter "%a"
          Misc_ml_generation.pp_to_ocaml_label_ident label in


  (* ************************************************ *)
  (* Now, let's really do the job of [generate_expr]. *)
  rec_generate local_idents initial_expression
;;
