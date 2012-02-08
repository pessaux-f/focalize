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

(* $Id: miscHelpers.ml,v 1.7 2012-02-08 16:35:29 pessaux Exp $ *)


(** ****************************************************************************
    {b Descr} : Because methods parameters do not have their type with them
      in the [species_description]s, this function establishes the mapping
      between the parameters names and their related type.
      It dissecates method's the type scheme (more accurately, an instance of
      it), "removing" arrows parameter after parameter.
      Because the typechecking pass is already done, the FoCaLize program is
      well-typed, hence, the type of the method must have "as many arrows as"
      the method has parameters. If this is not the case, then we have a bug
      somewhere else in the previous processes in the compiler.
      This function hence returns the list giving for each parameter name its
      type and the "result" type of the method (i.e. the type remaining after
      having "removed all the arrows" induced by the parameters).
      It also returns the list of type variables that were used to
      instanciate the ones generalized in the type scheme. More accurately, this
      list contains the couples (generalized type_variable * type being a
      variable used to instantiate the former). This is useful for Coq
      generation because polymorphism is explicit, leading for each
      polymorphic parameter to one extra parameter of type "Set" used to type
      the polymorphic parameter. Moreover, keeping in mind also the complete
      binding instead of just the type being a variable allows to keep trace of
      variables marked as generalized but that are bound by a hosting definition
      and are in fact only polymorphic in the context of this hosting
      definition.
      This allows to avoid generating spurious explicit polymorphic parameters
      in case they were already generated because the polymorphism comes from
      identifiers of this hosting definition.
      For instance, in the case of:
        let f(xx, yy) =
          let zz = (xx, yy) in
          yy
      the polymorphism of xx and yy is handled at the point of definition of f.
      Hence, f will need extra arguments, but not zz ! Not taking care of this
      would make us generating wrong code like:
        Let f (__var_a : Set) (__var_b : Set) (xx : __var_a) (yy : __var_b) :
          __var_b :=
          let zz (__var_a : Set) (__var_b : Set) : ((__var_b * __var_a)%type) :=
            (xx, yy) in
          yy.
      where zz has 2 spurious extra parameters, (__var_a : Set) and
      (__var_b : Set).

    {b Rem}: Note that the parameter [gen_vars_in_scope] is only useful when
      generating Coq code.

    {b Visibility}: Exported outside this module.
 **************************************************************************** *)
let bind_parameters_to_types_from_type_scheme ~self_manifest ~gen_vars_in_scope
    opt_scheme params_names =
  match opt_scheme with
   | None ->
       (* Since we are not given any type information, the binding will be
          trivially void and no type constraint will be printed. *)
       ((List.map (fun p_name -> (p_name, None)) params_names), None, [])
   | Some scheme ->
       (begin
       try
         let (type_from_scheme, generalized_instanciated_vars) =
           Types.specialize_n_show_instanciated_generalized_vars
             ~gen_vars_in_scope scheme in

         (* Be careful, the bindings list is built reversed ! We must finally
            reverse it again to keep the right order (i.e. first argument in
            head of the list. *)
         let rec rec_bind accu_bindings ty = function
           | [] -> (accu_bindings, (Some ty))
           | h :: q ->
               (* We split the functionnal type. First, get argument type. *)
               let h_type = Types.extract_fun_ty_arg ~self_manifest ty in
               (* Next, get the result type. *)
               let q_type = Types.extract_fun_ty_result ~self_manifest ty in
               (* We bind the current parameter to the "head-type" and continue
                  with the remaining parameters using the "tail-type". *)
               rec_bind ((h, (Some h_type)) :: accu_bindings) q_type q in

         (* ********************** *)
         (* Now, let's do the job. *)
         let (revd_mapping, result_ty) =
           rec_bind [] type_from_scheme params_names in
         (* Put the resulting mapping in the right order. *)
         ((List.rev revd_mapping), result_ty, generalized_instanciated_vars)
       with _ ->
         (* Because the typechecking was done in the previous passes, the
            program must be well-typed at this point. Then unification must
            always be successfull. If it fails, then there is a bug somewhere
            else before ! *)
         assert false
       end)
;;
