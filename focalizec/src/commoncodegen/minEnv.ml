(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 - 2012 LIP6 and INRIA                               *)
(*            2012 ENSTA ParisTech                                     *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)


(* ************************************************************************** *)
(** {b Descr} Elements of the minimal Coq typing environment for methods.
    We can't directly use [Env.TypeInformation.species_field] because they can't
    make appearing the fact that the carrier belongs to the minimal environment
    even if not *defined* (remind that in species fields, if "rep" appears then
    is it *defined* otherwise; it is silently declared and does't appear in the
    list of fields).

    {b Rem} : Not exported outside this module.                               *)
(* ************************************************************************** *)
type min_coq_env_method =
  | MCEM_Declared_carrier    (** The carrier belongs to the environment but
       only via a decl-dependency. Hence it doesn't need to be explicitely
       defined, but need to be in the environment. *)
  | MCEM_Defined_carrier of Types.type_scheme (** The carrier belongs to the
               environment via at least a def-dependency. Then is have to
               be explicitely declared. *)
  | MCEM_Declared_computational of
      (Parsetree.vname * Types.type_scheme) (** Abstract computational method,
         i.e. abstracted Let or abstracted Let_rec or Sig other than "rep". *)
  | MCEM_Defined_computational of
      (Env.from_history *
         (** Tells if the method is recursive and if so which kind of
             termination proof it involves. This is needed when generating
             pseudo-Coq code for Zenon using a "by definition" of this method.
             In effect, the body of the method contains the ident of this
             method, but when generating the Zenon stuff, the definition of the
             method will be named "abst_xxx".
             So the internal recursive call to print when generating the
             method's body must be replaced by "abst_xxx". This is related to
             the bug report #199. Moreover, since currently structural
             recursion is compiled with "Fixpoint" and other kinds with
             "Function" in Coq, we need to remind what is the compilation scheme
             used depending on the recursion kind. *)
       Env.CoqGenInformation.rec_status *
       Parsetree.vname * (Parsetree.vname list) * Types.type_scheme *
       Parsetree.binding_body)  (** Defined computational method, i.e. Let or
          Let_rec. *)
  | MCEM_Declared_logical of
      (Parsetree.vname * Parsetree.logical_expr)  (** Abstract logical
          property, i.e. Property or abstracted Theorem. *)
  | MCEM_Defined_logical of      (** Defined logical property, i.e. Theorem. *)
      (Env.from_history * Parsetree.vname * Parsetree.logical_expr)
;;



(** {b Descr}: Tells by which kind of construct (i.e. only logical or logical
    and/or computational) the method to add as dependency arrived. In other
    words, this tag tells if only logical target languages must take this
    dependency into account or if logical ANN also computational target
    languages are also impacted.
    This allows to compute the dependency calculus once for all, and not once
    for each target language. After thi common pass of calculus, each backend
    will select either [MCER_only_logical] AND [MCER_even_comput] dependencies
    for logical targets or only [MCER_even_comput] dependencies for
    computational targets.
    Clearly, [MCER_even_comput] is absorbant, this means that if a method
    is initiall present as dependency tagged by [MCER_only_logical], if it
    appear to be also required for computational stuff, it will be added
    with the tag [MCER_even_comput] whicb subsumes [MCER_only_logical].
    Said again differently, [MCER_even_comput] concerns both computational
    and logical targets although [MCER_only_logical] concerns only logical
    targets. *)
type min_coq_env_reason =
  | MCER_only_logical   (** The method is only induced by logical stuff and
                            must not be taken into account by only-computational
                            targets backend. *)
  | MCER_even_comput    (** The method is induced by at least computational
                            stuff and must be taken into account by
                            only-computational and also logical targets
                            backend. *)
;;

type min_coq_env_element = (min_coq_env_reason * min_coq_env_method) ;;


let find_coq_env_element_by_name name min_coq_env =
  List.find
    (fun (_, meth) ->
      match meth with
      | MCEM_Declared_carrier
      | MCEM_Defined_carrier _ -> name = (Parsetree.Vuident "rep")
      | MCEM_Declared_computational (n, _)
      | MCEM_Defined_computational (_, _, n, _, _, _)
      | MCEM_Declared_logical (n, _)
      | MCEM_Defined_logical (_, n, _) -> n = name)
    min_coq_env
;;



(* *********************************************************************** *)
(* in_the_universe_because Universe.t ->                                   *)
(*   Env.TypeInformation.species_field list ->                             *)
(*     min_coq_env_element list                                            *)
(** {b Descr} Compute the minimal Coq typing environment for a field whose
    visible universe is passed as [universe]. Proceeds following Virgile
    Prevosto's Phd, page 116, definition 58, section 6.4.4.
    If the carrier belongs to this list, then it ALWAYS is in head.

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let minimal_typing_environment universe species_fields =
  (* A local function to process one let-binding. Handy to factorize code for
     both [Let] and [Let_rec] fields. *)
  let process_one_let_binding is_rec l_binding =
    try (
      let (from, n, params, sch, body, opt_proof, _, _) = l_binding in
      match VisUniverse.Universe.find n universe with
      | VisUniverse.IU_decl_comput ->
          (* Keep in the environment, but as abstracted. *)
          [(MCER_even_comput, MCEM_Declared_computational (n, sch))]
      | VisUniverse.IU_decl_logic ->
          (* Keep in the environment, but as abstracted. *)
          [(MCER_only_logical, MCEM_Declared_computational (n, sch))]
      | VisUniverse.IU_trans_def ->
          (* Otherwise, keep the full definition. *)
          let rec_status =
            if is_rec then
              (match opt_proof with
              | None ->
                  Env.CoqGenInformation.RC_rec Env.CoqGenInformation.RPK_other
              | Some proof ->
                  Env.CoqGenInformation.RC_rec (
                  match proof.Parsetree.ast_desc with
                  | Parsetree.TP_structural decr_arg_name ->
                      Env.CoqGenInformation.RPK_struct decr_arg_name
                  | Parsetree.TP_lexicographic _
                  | Parsetree.TP_measure (_, _, _)
                  | Parsetree.TP_order (_, _, _) ->
                      Env.CoqGenInformation.RPK_other
                 ))
            else Env.CoqGenInformation.RC_non_rec in
          [(MCER_even_comput,
            MCEM_Defined_computational
              (from, rec_status, n, params, sch, body))]
     )
    with Not_found ->
      (* Not in the universe. Hence not in the minimal typing env. *)
      [] in
  (* We make a temporary flag that will remind if "rep" appears among the
     fields. This is a bit casual, but this avoid searching again in the list
     for this field. We need this because "rep" will be processed aside the
     other Sigs. See below. *)
  let found_rep_field = ref None in
  (* Now the local recursive function that will examine each species field. *)
  let rec build = function
   | [] -> []
   | h :: q ->
       let h' =
         (match h with
          | Env.TypeInformation.SF_sig (_, n, sch) -> (
              (* We will process "rep" specially because it may belong to
                 the universe although not present among the fields. See
                 below. *)
              if n <> (Parsetree.Vlident "rep") then (
                try (
                  match VisUniverse.Universe.find n universe with
                  | VisUniverse.IU_decl_logic | VisUniverse.IU_trans_def ->
                      [(MCER_only_logical,
                        MCEM_Declared_computational (n, sch))]
                  | VisUniverse.IU_decl_comput ->
                      [(MCER_even_comput,
                        MCEM_Declared_computational (n, sch))]
                 )
                with Not_found -> []
               )
              else (found_rep_field := Some sch ; [])
             )
          | Env.TypeInformation.SF_let l_binding ->
              process_one_let_binding false l_binding
          | Env.TypeInformation.SF_let_rec l ->
              List.flatten (List.map (process_one_let_binding true) l)
          | Env.TypeInformation.SF_theorem (_, n, _, body, _, _)
          | Env.TypeInformation.SF_property (_, n, _, body, _) -> (
              try (
                match VisUniverse.Universe.find n universe with
                | VisUniverse.IU_decl_logic ->
                    (* Keep in the environment, but as abstracted. *)
                    [(MCER_only_logical, MCEM_Declared_logical (n, body))]
                | VisUniverse.IU_decl_comput ->
                    (* How could it be since computational stuff can't depend
                       on logical ones ? *)
                    assert false
                | VisUniverse.IU_trans_def ->
                    (* How could it be since there are not def-dependencies
                       on proofs ? *)
                    assert false
               )
              with Not_found ->
                (* Not in the universe. Hence not in the minimal typing env. *)
                []
             )) in
       h' @ (build q) in
  (* *************************** *)
  (* Now, let do the real job... *)
  let env_without_carrier = build species_fields in
  (* Now, we need to handle "rep" aside. In effect, it may belong to the
     universe although not present among the fields. Moreover, if present
     among the fields, one must make it "declared" or "defined" depending
     on if it is in the universe via [IU_decl_xxx] or via [IU_trans_def].
     The regular processing of Sigs doesn't take this last point in account.
     So we proces "rep" aside here. If "rep" has to be added, then, always put
     it in head ! *)
  try (
    let reason = VisUniverse.Universe.find (Parsetree.Vlident "rep") universe in
    match !found_rep_field with
     | None -> (
         match reason with
         | VisUniverse.IU_decl_comput ->
             (* A decl-dependency was found even if "rep" is not defined. *)
             (MCER_even_comput, MCEM_Declared_carrier) :: env_without_carrier
         | VisUniverse.IU_decl_logic ->
             (MCER_only_logical, MCEM_Declared_carrier) :: env_without_carrier
         | VisUniverse.IU_trans_def ->
             (* Impossible to have a def-dependency if the carrier's
                structure is unknown ! *)
             assert false
        )
     | Some sch -> (
         match reason with
         | VisUniverse.IU_decl_comput ->
             (* A decl-dependency was found. No matter what "rep" is. *)
             (MCER_even_comput, MCEM_Declared_carrier) :: env_without_carrier
         | VisUniverse.IU_decl_logic ->
             (MCER_only_logical, MCEM_Declared_carrier) :: env_without_carrier
         | VisUniverse.IU_trans_def ->
             (* A def-dependency was found. So, record the carrier's
                structure.
              ATTENTION: We tag it [MCER_even_comput] and it is very subtil!
              The question is won't it bring a non-existing def-dep on rep
              in computational code although this dep was initially brought
              by a dependency on a logical method?
              The answer is no. In effect, either the currently processed
              method is really computational and in this case there have been
              some where a real unification since computational can't depend
              on logical methods. Or we are processing a logical method, and
              in this case anyway all the kind of dependencies must be taken
              into account! *)
             (MCER_even_comput, (MCEM_Defined_carrier sch)) ::
             env_without_carrier
        )
   )
  with Not_found ->
    (* Not in the universe. Hence not in the minimal typing env. *)
    env_without_carrier
;;
