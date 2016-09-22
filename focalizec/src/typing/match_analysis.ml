(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            Pierre-Nicolas Tollitte                                         *)
(*            François Pessaux                                                *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - ... LIP6 and INRIA                                       *)
(*            2012 - ... ENSTA ParisTech                                      *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)



(** {b Descr}: Exceptions for pattern matching analysis. *)
exception Match_not_exhaustive of Location.t ;;
exception Match_useless_case of Location.t ;;



(* ************************************************************************** *)
(** {b Descr}: Get the constructors of a type assumed this type is a construct
    (i.e. named) sum type. Returns some [Parsetree.constructor_name]s.
    {b Rem}: [Unsure] Doesn't handle record types.                            *)
(* ************************************************************************** *)
let get_constructors_of_a_type ~current_unit ty typing_env =
  match Types.of_which_construct_type ty with
  | None -> []       (* Not a named type: so, no constructors. *)
  | Some type_name -> (
      (* Ok, we've got a named type. Now recover its hosting module and its
         constructor name. *)
      let (host_mod, ty_cstr_name) = Types.split_type_constructor type_name in
      (* Build a [Parsetree.ident] from these 2 parts. *)
      let ty_cstr_ident = {
        Parsetree.ast_loc = Location.none ;
        Parsetree.ast_desc = 
          Parsetree.I_global
            (Parsetree.Qualified
               (host_mod, (Parsetree.Vlident ty_cstr_name))) ;
        Parsetree.ast_annot = [] ;
        Parsetree.ast_type = Parsetree.ANTI_none } in
      (* Find this type's definition in the typing environment. Since the
         typechecking pass is done, this should never fail. *)
      try
        let ty_descr =
          Env.TypingEnv.find_type
            ~loc: Location.none ~current_unit ty_cstr_ident typing_env in
        (* Now, inspect the type definition to find its possible constructors we
           only have constructors in case of sum type definition, i.e. in case
           the [type_kind] is [TK_variant] but also [TK_external] since these
           latter can have mapped value constructors. *)
        match ty_descr.Env.TypeInformation.type_kind with
        | Env.TypeInformation.TK_variant val_cstrs ->
            List.map (fun (cn, _, _) -> cn) val_cstrs
        | Env.TypeInformation.TK_external (_, mapping) ->
            (* External types may also have constructors. *)
            List.map
              (fun binding -> fst binding.Parsetree.ast_desc)
              mapping.Parsetree.ast_desc
        | Env.TypeInformation.TK_abstract
        | Env.TypeInformation.TK_record _ -> []
      with  _ -> assert false
     )
;;



(** {b Descr}: Cleans a pattern. *)
let rec clean_pattern pat =
  match pat.Parsetree.ast_desc with
  | Parsetree.P_as (pat, _) | Parsetree.P_paren pat -> clean_pattern pat
  | _ -> pat
;;



(** {b Descr}: Repeats a pattern nb times *)
let rec repeat_pattern pat nb =
  if nb > 0 then pat :: (repeat_pattern pat (nb - 1)) else []
;;



(** {b Descr}: Tests if a matrix is empty. *)
let is_matrix_empty p =
  match p with [] | ([] :: _) -> true | _ -> false
;;



(** {b Descr}: Expands cleaned pattern. *)
let expand_patterns pats =
  let find_max_size nb pat =
    let nb2 =
      (match pat.Parsetree.ast_desc with
      | Parsetree.P_record l -> List.length l
      | Parsetree.P_tuple l -> List.length l
      | _ -> 1) in
    if nb2 > nb then nb2 else nb in
  let pat_size = List.fold_left find_max_size 1 pats in
  let expand_pattern pat =
    (match pat.Parsetree.ast_desc with
    | Parsetree.P_record l -> List.map snd l
    | Parsetree.P_tuple l -> l
    | _ -> repeat_pattern pat pat_size) in
  (pat_size, (List.map expand_pattern pats))
;;



(* ************************************************************************** *)
(** {b Descr}: Extracts a string from a constructor identifier representing
    only it's name without anymore hosting module (or qualification)
    information. This is used to compare the constructors found in patterns
    and the available constructors of the type of matched values.
    {b Rem}: Since typechecking has been done, we are sure that hosting
    modules of constructors in patterns match the ones of matched values.
    For this reason we can safely ignore these hosting modules (i.e. the
    full qualification of the names).                                         *)
(* ************************************************************************** *)
let string_of_constructor_ident ci =
  match ci.Parsetree.ast_desc with
  | Parsetree.CI ident -> (
      match ident.Parsetree.ast_desc with
      | Parsetree.I_local vn -> Parsetree_utils.name_of_vname vn
      | Parsetree.I_global qvn -> (
          match qvn with
          | Parsetree.Vname vn | Parsetree.Qualified (_, vn) ->
              Parsetree_utils.name_of_vname vn
         )
     )
;;



(** {b Descr}: Takes one column out of a matrix. *)
let rec matrix_one_col_out p =
  match p with
  | [] -> ([], [])
  | (hd_row :: tl_row) :: tail_p ->
      let (col, np) = matrix_one_col_out tail_p in
      ((hd_row :: col), (tl_row :: np))
  | _ -> assert false
;;



(** {b Descr}: Splits a matrix by columns. *)
let rec split_matrix p = match p with
  | [] -> []
  | [] :: _ -> []
  | _ ->
      let (c, np) = matrix_one_col_out p in
      c :: (split_matrix np)
;;


(** {b Descr}: Collapses a matrix splitted with split_matrix. *)
let collapse_matrix_2 m1 m2 = List.map2 (@) m1 m2 ;;
let rec collapse_matrix matrix_list = match matrix_list with
  | [] -> []
  | [ m ] -> m
  | m1 :: ml -> collapse_matrix_2 m1 (collapse_matrix ml)
;;



(** {b Descr}: Creates a normalized pattern matrix from a pattern list. *)
let rec normalize pats =
  let pats = List.map clean_pattern pats in
  let (n, p) = expand_patterns pats in
  if n = 1 then List.map (List.map clean_pattern) p
  else let cols = split_matrix p in
  let matrix_list = List.map normalize cols in
  collapse_matrix matrix_list
;;



(** {b Descr}: Normalizes a matrix for the urec algorithm. *)
let normalize_matrix p =
  if is_matrix_empty p then p
  else
    let cols = split_matrix p in
    let matrix_list = List.map normalize cols in
    collapse_matrix matrix_list
;;



(** {b Descr}: Specializes a matrix for the urec algorithm. *)
let spec_matrix ci cargs_count p q =
  let spec_vector rows pi = match (List.hd pi).Parsetree.ast_desc with
  | Parsetree.P_constr (ci', pats') when
        (string_of_constructor_ident ci') = (string_of_constructor_ident ci) ->
      (pats' @ (List.tl pi)) :: rows
  | Parsetree.P_wild | Parsetree.P_const _ | Parsetree.P_var _ ->
      ((repeat_pattern (List.hd pi) cargs_count) @ (List.tl pi)) :: rows
  | _ -> rows in
  let p' = List.fold_left spec_vector [] p in
  let q' = List.hd (spec_vector [] q) in
  (normalize_matrix p'), q'
;;



(** {b Descr}: Gets the list of root constructors of a matrix column.
    {b Rem}: [Unsure] Doesn't process constant patterns ! This means that int and
    so on are considered having no constructors ! This cause analysis to be
    broken on types other than sum types. *)
let rec head_constructors_from_list col =
  match col with
  | [] -> []
  | p :: tl -> (
      match p.Parsetree.ast_desc with
      | Parsetree.P_constr (ci, al) ->
          (ci, List.length al, p) :: (head_constructors_from_list tl)
      | _ -> head_constructors_from_list tl
     )
;;



(** {b Descr}: Get the default matrix for the urec algorithm. *)
let rec default_matrix p = match p with
  | [] -> []
  | (p :: tlr) :: tlp -> (
      match p.Parsetree.ast_desc with
      | Parsetree.P_wild | Parsetree.P_const _ | Parsetree.P_var _ ->
          tlr :: (default_matrix tlp)
      | _ -> default_matrix tlp
     )
  | _ -> assert false
;;



(** {b Descr}: Implements the Urec algorithm. *)
let urec ~current_unit initial_p initial_q typing_env =
  (* Just a local function to save passing each time the current compilation
     unit and the typing environment. *)
  let rec local_urec p q =
    match (p, q) with
    | ([], _) -> true
    | (([] :: _), _) -> false
    | (_, (q1 :: tail_q)) -> (
        match q1.Parsetree.ast_desc with
        | Parsetree.P_constr (ci, pats) -> (* q1 is a constructed pattern *)
            let (p', q') = spec_matrix ci (List.length pats) p q in
            local_urec p' q'
        | Parsetree.P_wild | Parsetree.P_const _
        | Parsetree.P_var _ -> (* q1 is a wildcard *)
            let (first_col, _) = matrix_one_col_out p in
            let patterns_cstrs = head_constructors_from_list first_col in
            (match patterns_cstrs with
            | [] -> local_urec (default_matrix p) tail_q
            | (_, _, some_cstr) :: _ ->
                let ty =
                  (match some_cstr.Parsetree.ast_type with
                  | Parsetree.ANTI_type t -> t
                  | _ -> assert false) in
                let all_ty_cstrs =
                  get_constructors_of_a_type ~current_unit ty typing_env in
                (* For any constructor found in the type definition, check if
                   it belongs to the patterns constructors. If so, the signature
                   is complete. *)
                let complete_sig =
                  List.for_all
                    (fun ci ->
                      let ci_as_string = Parsetree_utils.name_of_vname ci in
                      (* Look for the corresponding constructor in the patterns
                         constructors. *)
                      List.exists
                        (fun (ci', _, _) ->
                          ci_as_string = (string_of_constructor_ident ci'))
                        patterns_cstrs)
                    all_ty_cstrs in
                if complete_sig then
                  List.exists
                    (fun (c, count, _) ->
                      let (p', q') = spec_matrix c count p q in
                      local_urec p' q')
                    patterns_cstrs
                else local_urec (default_matrix p) tail_q
            )
        | _ -> assert false
       )
    | _ -> assert false in
  (* Now, really do the job. *)
  local_urec initial_p initial_q
;;



(** {b Descr}: Normalize the patterns and use the urec algorithm. *)
let urec_norm ~current_unit pats qpat typing_env =
  match normalize (qpat :: pats) with
  | q::p -> urec ~current_unit p q typing_env
  | _ -> assert false
;;



(** {b Descr}: Dummy wildcard pattern for exhaustivity check. *)
let dummy_wild_pattern =
  { Parsetree.ast_loc = Location.none ;
    Parsetree.ast_desc = Parsetree.P_wild ;
    Parsetree.ast_annot = [] ;
    Parsetree.ast_type = Parsetree.ANTI_none }
;;



(** {b Descr}: Patterns usefulness check. *)
let rec check_usefulness ~current_unit pats typing_env =
  match pats with
  | [] -> ()
  | [ _ ] -> ()
  | p :: tl_pats ->
      check_usefulness ~current_unit tl_pats typing_env ;
      if not (urec_norm ~current_unit tl_pats p typing_env) then (
        (* Only print a warning if no error raising was requested. *)
        if (Configuration.get_pmatch_err_as_warn ()) then
          Format.eprintf
            "%a:@\n@[%tWarning:%t Useless@ case@ in@ pattern-matching.@]@."
            Location.pp_location p.Parsetree.ast_loc
            Handy.pp_set_bold Handy.pp_reset_effects
        else raise (Match_useless_case p.Parsetree.ast_loc)
       )
;;



(** {b Descr}: Verify a pattern-matching expression. Checks for exhaustivity
    and absence of useless pattern. Doesn't return any validity result. Instead,
    issues warnings or exception raising in case of problem. *)
let verify ~current_unit typing_env m_expr =
  match m_expr.Parsetree.ast_desc with
  | Parsetree.E_match (_, pattern_expr_list) ->
      let (pats, _) = List.split pattern_expr_list in
      let res = urec_norm ~current_unit pats dummy_wild_pattern typing_env in
      check_usefulness ~current_unit (List.rev pats) typing_env ;
      if res then (
        (* Only print a warning if no error raising was requested. *)
        if (Configuration.get_pmatch_err_as_warn ()) then
          Format.eprintf
            "%a:@\n@[%tWarning:%t Pattern-matching@ is@ not@ exhaustive.@]@."
            Location.pp_location m_expr.Parsetree.ast_loc
            Handy.pp_set_bold Handy.pp_reset_effects
        else raise (Match_not_exhaustive m_expr.Parsetree.ast_loc)
       )
  | _ -> assert false
;;



(* ************************************************************************** *)
(** {b Descr}: Search for pattern matching expressions in the AST.

    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let rec verify_matchings ~current_unit typing_env stuff_to_compile =
  match stuff_to_compile with
  | Infer.PCM_species (species_def, _, _, _) ->
      verify_matchings_species ~current_unit typing_env species_def
  | Infer.PCM_let_def (let_def, _) ->
      verify_matchings_let ~current_unit typing_env let_def
  | Infer.PCM_expr expr -> verify_matchings_expr ~current_unit typing_env expr
  | _ -> ()



and verify_matchings_species ~current_unit typing_env species_def =
  let species_def_desc = species_def.Parsetree.ast_desc in
  if Configuration.get_verbose () then
    Format.eprintf
      "Checking pattern-matching exhaustivity in species '%a'.@."
      Sourcify.pp_vname species_def_desc.Parsetree.sd_name ;
  List.iter
    (fun spe_field ->
      match spe_field.Parsetree.ast_desc with
      | Parsetree.SF_let let_def ->
          verify_matchings_let ~current_unit typing_env let_def
      | _ -> ())
    species_def_desc.Parsetree.sd_fields



and verify_matchings_let ~current_unit typing_env let_def =
  List.iter
    (fun binding ->
      match binding.Parsetree.ast_desc.Parsetree.b_body with
      | Parsetree.BB_logical logical_expr ->
          verify_matchings_logexpr ~current_unit typing_env logical_expr
      | Parsetree.BB_computational expr ->
          verify_matchings_expr ~current_unit typing_env expr)
    let_def.Parsetree.ast_desc.Parsetree.ld_bindings



and verify_matchings_logexpr ~current_unit typing_env logical_expr =
  match logical_expr.Parsetree.ast_desc with
    | Parsetree.Pr_forall (_ ,_ , logical_expr)
    | Parsetree.Pr_exists (_ , _, logical_expr)
    | Parsetree.Pr_not logical_expr | Parsetree.Pr_paren logical_expr ->
        verify_matchings_logexpr ~current_unit typing_env logical_expr
    | Parsetree.Pr_imply (le1, le2) | Parsetree.Pr_or (le1, le2)
    | Parsetree.Pr_and (le1, le2) | Parsetree.Pr_equiv (le1, le2) ->
        verify_matchings_logexpr ~current_unit typing_env le1 ;
        verify_matchings_logexpr ~current_unit typing_env le2
  | Parsetree.Pr_expr expr ->
      verify_matchings_expr ~current_unit typing_env expr



and verify_matchings_expr ~current_unit typing_env expr =
  match expr.Parsetree.ast_desc with
  | Parsetree.E_paren expr | Parsetree.E_record_access (expr, _)
  | Parsetree.E_fun (_, expr) ->
      verify_matchings_expr ~current_unit typing_env expr
  | Parsetree.E_app (expr, expr_list) ->
      verify_matchings_expr ~current_unit typing_env expr ;
      List.iter (verify_matchings_expr ~current_unit typing_env) expr_list
  | Parsetree.E_tuple expr_list
  | Parsetree.E_sequence expr_list
  | Parsetree.E_constr (_, expr_list) ->
      List.iter (verify_matchings_expr ~current_unit typing_env) expr_list
  | Parsetree.E_if (expr1, expr2, expr3) ->
      verify_matchings_expr ~current_unit typing_env expr1 ;
      verify_matchings_expr ~current_unit typing_env expr2 ;
      verify_matchings_expr ~current_unit typing_env expr3
  | Parsetree.E_let (let_def, expr) ->
      verify_matchings_let ~current_unit typing_env let_def ;
      verify_matchings_expr ~current_unit typing_env expr
  | Parsetree.E_record label_expr_list ->
      List.iter
        (fun (_, e) -> verify_matchings_expr ~current_unit typing_env e)
        label_expr_list
  | Parsetree.E_record_with (expr, label_expr_list) ->
      verify_matchings_expr ~current_unit typing_env expr ;
      List.iter
        (fun (_, e) -> verify_matchings_expr ~current_unit typing_env e)
        label_expr_list
  | Parsetree.E_match (expr2, pattern_expr_list) ->
      (* verify ~current_unit typing_env expr ;*)  (* Disabled for release. *)
      verify_matchings_expr ~current_unit typing_env expr2 ;
      List.iter
        (fun (_, e) -> verify_matchings_expr ~current_unit typing_env e)
        pattern_expr_list
  | _ -> ()
;;
