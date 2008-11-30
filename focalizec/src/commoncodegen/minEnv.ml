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
(*  Copyright 2007, 2008 LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: minEnv.ml,v 1.8 2008-11-29 23:41:13 weis Exp $ *)


(* *********************************************************************** *)
(** {b Descr} Elements of the minimal Coq typing environment for methods.
    We can't directly use [Env.TypeInformation.species_field] because they
    can't make appearing the fact that the carrier belongs to the minimal
    environment even if not *defined* (remind that in species fields, if
    "rep" appears then is it *defined* otherwise; it is silently
    declared and does't appear in the list of fields).

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
type min_coq_env_element =
  | MCEE_Declared_carrier    (** The carrier belongs to the environment but
                                 only via a decl-dependency. Hence it doesn't
                                 need to be explicitely defined, but need to
                                 be in the environment. *)
  | MCEE_Defined_carrier of Types.type_scheme (** The carrier belongs to the
               environment via at least a def-dependency. Then is have to
               be explicitely declared. *)
  | MCEE_Declared_computational of
      (Parsetree.vname * Types.type_scheme) (** Abstract computational method,
           i.e. abstracted Let or abstracted Let_rec or Sig other than "rep". *)
  | MCEE_Defined_computational of
      (Env.from_history *
       Parsetree.vname * (Parsetree.vname list) * Types.type_scheme *
       Parsetree.binding_body)  (** Defined computational method, i.e. Let or
                                    Let_rec. *)
  | MCEE_Declared_logical of
      (Parsetree.vname * Parsetree.logical_expr)  (** Abstract logical property,
           i.e. Property or abstracted Theorem. *)
  | MCEE_Defined_logical of      (** Defined logical property, i.e. Theorem. *)
      (Env.from_history * Parsetree.vname * Parsetree.logical_expr)
;;



let find_coq_env_element_by_name name min_coq_env =
  List.find
    (function
      | MCEE_Declared_carrier
      | MCEE_Defined_carrier _ -> name = (Parsetree.Vuident "rep")
      | MCEE_Declared_computational (n, _)
      | MCEE_Defined_computational (_, n, _, _, _)
      | MCEE_Declared_logical (n, _)
      | MCEE_Defined_logical (_, n, _) -> n = name)
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
  let process_one_let_binding l_binding =
    try
      let (from, n, params, sch, body, _, _, _) = l_binding in
      let reason = VisUniverse.Universe.find n universe in
      if reason = VisUniverse.IU_only_decl then
        (* Keep in the environment, but as abstracted. *)
        [MCEE_Declared_computational (n, sch)]
      else
        (* Otherwise, keep the full definition. *)
        [MCEE_Defined_computational (from, n, params, sch, body)]
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
          | Env.TypeInformation.SF_sig (_, n, sch) ->
              (* We will process "rep" specially because it may belong to
                 the universe although not present among the fields. See
                 below. *)
              if n <> (Parsetree.Vlident "rep") then
                (if VisUniverse.Universe.mem n universe then
                  [MCEE_Declared_computational (n, sch)]
                else [])
              else (found_rep_field := Some sch ; [])
          | Env.TypeInformation.SF_let l_binding ->
              process_one_let_binding l_binding
          | Env.TypeInformation.SF_let_rec l ->
              List.flatten (List.map process_one_let_binding l)
          | Env.TypeInformation.SF_theorem (from, n, _, body, _, _) ->
              (begin
              try
                let reason = VisUniverse.Universe.find n universe in
                if reason = VisUniverse.IU_only_decl then
                  (* Keep in the environment, but as abstracted. *)
                  [MCEE_Declared_logical (n, body) ]
                else
                  (* Otherwise, keep the full definition. *)
                  [MCEE_Defined_logical (from, n, body) ]
              with Not_found ->
                (* Not in the universe. Hence not in the minimal typing env. *)
                []
              end)
          | Env.TypeInformation.SF_property (_, n, _, body, _) ->
              if VisUniverse.Universe.mem n universe then
                [MCEE_Declared_logical (n, body)]
              else []) in
       h' @ (build q) in
  (* *************************** *)
  (* Now, let do the real job... *)
  let env_without_carrier = build species_fields in
  (* Now, we need to handle "rep" aside. In effect, it may belong to the
     universe although not present among the fields. Moreover, if present
     among the fields, one must make it "declared" or "defined" depending
     on if it is in the universe via [IU_only_decl] or via [IU_trans_def].
     The regular processing of Sigs doesn't take this last point in account.
     So we proces "rep" aside here. If "rep" has to be added, then, always put
     it in head ! *)
  try
    (begin
    let reason = VisUniverse.Universe.find (Parsetree.Vlident "rep") universe in
    match (!found_rep_field, reason) with
     | (None, VisUniverse.IU_only_decl) ->
         (* A decl-dependency was found even if "rep" is not defined. *)
         MCEE_Declared_carrier :: env_without_carrier
     | (None, VisUniverse.IU_trans_def) ->
         (* Impossible to have a def-dependency if the carrier's structure is
            unknown ! *)
         assert false
     | ((Some _), VisUniverse.IU_only_decl) ->
         (* A decl-dependency was found. No matter what "rep" is. *)
         MCEE_Declared_carrier :: env_without_carrier
     | ((Some sch), VisUniverse.IU_trans_def) ->
         (* A def-dependency was found. So, record the carrier's structure. *)
         (MCEE_Defined_carrier sch) :: env_without_carrier
    end)
  with Not_found ->
    (* Not in the universe. Hence not in the minimal typing env. *)
    env_without_carrier
;;
