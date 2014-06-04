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


let find_coq_env_element_by_name name min_coq_env =
  List.find
    (fun (_, meth) ->
      match meth with
      | Env.TypeInformation.MCEM_Declared_carrier
      | Env.TypeInformation.MCEM_Defined_carrier _ ->
          name = (Parsetree.Vuident "rep")
      | Env.TypeInformation.MCEM_Declared_computational (n, _)
      | Env.TypeInformation.MCEM_Defined_computational (_, _, n, _, _, _)
      | Env.TypeInformation.MCEM_Declared_logical (n, _)
      | Env.TypeInformation.MCEM_Defined_logical (_, n, _) -> n = name)
    min_coq_env
;;

let find_dk_env_element_by_name name min_dk_env =
  List.find
    (fun (_, meth) ->
      match meth with
      | Env.TypeInformation.MCEM_Declared_carrier
      | Env.TypeInformation.MCEM_Defined_carrier _ ->
          name = (Parsetree.Vuident "rep")
      | Env.TypeInformation.MCEM_Declared_computational (n, _)
      | Env.TypeInformation.MCEM_Defined_computational (_, _, n, _, _, _)
      | Env.TypeInformation.MCEM_Declared_logical (n, _)
      | Env.TypeInformation.MCEM_Defined_logical (_, n, _) -> n = name)
    min_dk_env
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
let minimal_coq_typing_environment universe species_fields =
  (* A local function to process one let-binding. Handy to factorize code for
     both [Let] and [Let_rec] fields. *)
  let process_one_let_binding is_rec l_binding =
    try (
      let (from, n, params, sch, body, opt_proof, _, _) = l_binding in
      match VisUniverse.Universe.find n universe with
      | VisUniverse.IU_decl_comput ->
          (* Keep in the environment, but as abstracted. *)
          [(Env.TypeInformation.MCER_even_comput,
            Env.TypeInformation.MCEM_Declared_computational (n, sch))]
      | VisUniverse.IU_decl_logic ->
          (* Keep in the environment, but as abstracted. *)
          [(Env.TypeInformation.MCER_only_logical,
            Env.TypeInformation.MCEM_Declared_computational (n, sch))]
      | VisUniverse.IU_trans_def ->
          (* Otherwise, keep the full definition. *)
          let rec_status =
            if is_rec then
              (match opt_proof with
              | None -> Env.RC_rec Env.RPK_other
              | Some proof ->
                  Env.RC_rec (
                  match proof.Parsetree.ast_desc with
                  | Parsetree.TP_structural decr_arg_name ->
                      Env.RPK_struct decr_arg_name
                  | Parsetree.TP_lexicographic _
                  | Parsetree.TP_measure (_, _, _)
                  | Parsetree.TP_order (_, _, _) -> Env.RPK_other
                 ))
            else Env.RC_non_rec in
          [(Env.TypeInformation.MCER_even_comput,
            Env.TypeInformation.MCEM_Defined_computational
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
                      [(Env.TypeInformation.MCER_only_logical,
                        Env.TypeInformation.MCEM_Declared_computational
                          (n, sch))]
                  | VisUniverse.IU_decl_comput ->
                      [(Env.TypeInformation.MCER_even_comput,
                        Env.TypeInformation.MCEM_Declared_computational
                          (n, sch))]
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
                    [(Env.TypeInformation.MCER_only_logical,
                      Env.TypeInformation.MCEM_Declared_logical (n, body))]
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
             (Env.TypeInformation.MCER_even_comput,
              Env.TypeInformation.MCEM_Declared_carrier) :: env_without_carrier
         | VisUniverse.IU_decl_logic ->
             (Env.TypeInformation.MCER_only_logical,
              Env.TypeInformation.MCEM_Declared_carrier) :: env_without_carrier
         | VisUniverse.IU_trans_def ->
             (* Impossible to have a def-dependency if the carrier's
                structure is unknown ! *)
             assert false
        )
     | Some sch -> (
         match reason with
         | VisUniverse.IU_decl_comput ->
             (* A decl-dependency was found. No matter what "rep" is. *)
             (Env.TypeInformation.MCER_even_comput,
              Env.TypeInformation.MCEM_Declared_carrier) :: env_without_carrier
         | VisUniverse.IU_decl_logic ->
             (Env.TypeInformation.MCER_only_logical,
              Env.TypeInformation.MCEM_Declared_carrier) :: env_without_carrier
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
             (Env.TypeInformation.MCER_even_comput,
              (Env.TypeInformation.MCEM_Defined_carrier sch)) ::
             env_without_carrier
        )
   )
  with Not_found ->
    (* Not in the universe. Hence not in the minimal typing env. *)
    env_without_carrier
;;

(* *********************************************************************** *)
(* in_the_universe_because Universe.t ->                                   *)
(*   Env.TypeInformation.species_field list ->                             *)
(*     min_dk_env_element list                                            *)
(** {b Descr} Compute the minimal Dk typing environment for a field whose
    visible universe is passed as [universe]. Proceeds following Virgile
    Prevosto's Phd, page 116, definition 58, section 6.4.4.
    If the carrier belongs to this list, then it ALWAYS is in head.

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let minimal_dk_typing_environment universe species_fields =
  (* A local function to process one let-binding. Handy to factorize code for
     both [Let] and [Let_rec] fields. *)
  let process_one_let_binding is_rec l_binding =
    try (
      let (from, n, params, sch, body, opt_proof, _, _) = l_binding in
      match VisUniverse.Universe.find n universe with
      | VisUniverse.IU_decl_comput ->
          (* Keep in the environment, but as abstracted. *)
          [(Env.TypeInformation.MCER_even_comput,
            Env.TypeInformation.MCEM_Declared_computational (n, sch))]
      | VisUniverse.IU_decl_logic ->
          (* Keep in the environment, but as abstracted. *)
          [(Env.TypeInformation.MCER_only_logical,
            Env.TypeInformation.MCEM_Declared_computational (n, sch))]
      | VisUniverse.IU_trans_def ->
          (* Otherwise, keep the full definition. *)
          let rec_status =
            if is_rec then
              (match opt_proof with
              | None -> Env.RC_rec Env.RPK_other
              | Some proof ->
                  Env.RC_rec (
                  match proof.Parsetree.ast_desc with
                  | Parsetree.TP_structural decr_arg_name ->
                      Env.RPK_struct decr_arg_name
                  | Parsetree.TP_lexicographic _
                  | Parsetree.TP_measure (_, _, _)
                  | Parsetree.TP_order (_, _, _) -> Env.RPK_other
                 ))
            else Env.RC_non_rec in
          [(Env.TypeInformation.MCER_even_comput,
            Env.TypeInformation.MCEM_Defined_computational
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
                      [(Env.TypeInformation.MCER_only_logical,
                        Env.TypeInformation.MCEM_Declared_computational
                          (n, sch))]
                  | VisUniverse.IU_decl_comput ->
                      [(Env.TypeInformation.MCER_even_comput,
                        Env.TypeInformation.MCEM_Declared_computational
                          (n, sch))]
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
                    [(Env.TypeInformation.MCER_only_logical,
                      Env.TypeInformation.MCEM_Declared_logical (n, body))]
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
             (Env.TypeInformation.MCER_even_comput,
              Env.TypeInformation.MCEM_Declared_carrier) :: env_without_carrier
         | VisUniverse.IU_decl_logic ->
             (Env.TypeInformation.MCER_only_logical,
              Env.TypeInformation.MCEM_Declared_carrier) :: env_without_carrier
         | VisUniverse.IU_trans_def ->
             (* Impossible to have a def-dependency if the carrier's
                structure is unknown ! *)
             assert false
        )
     | Some sch -> (
         match reason with
         | VisUniverse.IU_decl_comput ->
             (* A decl-dependency was found. No matter what "rep" is. *)
             (Env.TypeInformation.MCER_even_comput,
              Env.TypeInformation.MCEM_Declared_carrier) :: env_without_carrier
         | VisUniverse.IU_decl_logic ->
             (Env.TypeInformation.MCER_only_logical,
              Env.TypeInformation.MCEM_Declared_carrier) :: env_without_carrier
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
             (Env.TypeInformation.MCER_even_comput,
              (Env.TypeInformation.MCEM_Defined_carrier sch)) ::
             env_without_carrier
        )
   )
  with Not_found ->
    (* Not in the universe. Hence not in the minimal typing env. *)
    env_without_carrier
;;

