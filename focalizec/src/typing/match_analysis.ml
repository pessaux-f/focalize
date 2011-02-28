
(* Exceptions for pattern matching analysis. *)
exception Match_not_exhaustive of Location.t
exception Match_useless_case of Location.t

(* Make a dummy ast from an ast_desc. *)
let locnone = Location.none
let get_dummy_ast a = { Parsetree.ast_loc = locnone;
                        Parsetree.ast_desc = a;
                        Parsetree.ast_annot = [];
                        Parsetree.ast_type = Parsetree.ANTI_none}

(* Make a dummy constructor ident from a name. *)
let constructor_ident_of_constructor_name n =
  get_dummy_ast (Parsetree.CI (get_dummy_ast (Parsetree.I_local n)))

(* Get all constructors defined in the current environment *)
let focalize_get_all_constructors typing_env =
  let get_constr_def cn =
    let ci = constructor_ident_of_constructor_name cn in
    (ci, Env.TypingEnv.find_constructor ~loc:locnone ~current_unit:"" ci
     typing_env) in
  let constr = Env.get_constructor_list typing_env in
  List.map get_constr_def constr


(* Get the constructors of a special type *)
let get_constructors_of_a_type t typing_env =
  let rec is_ok typ  =
    let nok _ = false in
    let nok2 _ _ = false in
    let farrow _t1 t2 = is_ok t2 in
    let fconstruct _ tn _args = (tn = t) in
      Types.extract_type_simple nok farrow nok nok fconstruct nok nok2 typ in
  let cons = focalize_get_all_constructors typing_env in
  List.filter
    (fun r -> is_ok (Types.specialize (snd r).Env.TypeInformation.cstr_scheme))
    cons




(* Clean a pattern *)
let rec clean_pattern pat = match pat.Parsetree.ast_desc with
  | Parsetree.P_as (pat, _) -> clean_pattern pat
  | Parsetree.P_paren pat -> clean_pattern pat
  | _ -> pat

(* Repeat a pattern nb times *)
let rec repeat_pattern pat nb =
  if nb > 0 then pat::(repeat_pattern pat (nb - 1))
  else []


(* Test if a matrix is empty. *)
let is_matrix_empty p = match p with
  | [] | []::_ -> true
  | _ -> false

(* Expand cleaned pattern *)
let rec expand_patterns pats =
  let find_max_size nb pat = let nb2 = match pat.Parsetree.ast_desc with
    | Parsetree.P_record l -> List.length l
    | Parsetree.P_tuple l -> List.length l
    | _ -> 1 in
    if nb2 > nb then nb2 else nb in
  let pat_size = List.fold_left find_max_size 1 pats in
  let expand_pattern pat = match pat.Parsetree.ast_desc with
    | Parsetree.P_record l -> List.map snd l
    | Parsetree.P_tuple l -> l
    | _ -> repeat_pattern pat pat_size in
  (pat_size, List.map expand_pattern pats)

(* Extract a string from a constructor identifier. *)
let string_of_ci ci = match ci.Parsetree.ast_desc with
  | Parsetree.CI ident -> begin match ident.Parsetree.ast_desc with
    | Parsetree.I_local vn -> Parsetree_utils.name_of_vname vn
    | Parsetree.I_global qvn -> begin match qvn with
      | Parsetree.Vname vn | Parsetree.Qualified (_, vn) ->
        Parsetree_utils.name_of_vname vn
    end
  end

(* Take one column out of a martix. *)
let rec matrix_one_col_out p = match p with
  | [] -> ([], [])
  | (hd_row::tl_row)::tail_p -> let col, np = matrix_one_col_out tail_p in
    (hd_row::col), (tl_row::np)
  | _ -> assert false

(* Split a matrix by columns. *)
let rec split_matrix p = match p with
  | [] -> []
  | []::_ -> []
  | _ -> let c, np = matrix_one_col_out p in
    c::(split_matrix np)

(* Callapse a matrix splitted with split_matrix. *)
let collapse_matrix_2 m1 m2 = List.map2 (@) m1 m2
let rec collapse_matrix matrix_list = match matrix_list with
  | [] -> []
  | [m] -> m
  | m1::ml -> collapse_matrix_2 m1 (collapse_matrix ml)

(* Create a normalized pattern matrix from a pattern list *)
let rec normalize pats =
  let pats = List.map clean_pattern pats in
  let n, p = expand_patterns pats in
  if n = 1 then List.map (List.map clean_pattern) p
  else let cols = split_matrix p in
  let matrix_list = List.map normalize cols in
  collapse_matrix matrix_list

(* Normalize a martix for the urec algorithm. *)
let rec normalize_matrix p =
  if is_matrix_empty p then p else
  let cols = split_matrix p in
  let matrix_list = List.map normalize cols in
  collapse_matrix matrix_list

(* Specialize a matrix for the urec algorithm. *)
let spec_matrix ci cargs_count p q =
  let spec_vector rows pi = match (List.hd pi).Parsetree.ast_desc with
    | Parsetree.P_constr(ci', pats') when
             (string_of_ci ci') = (string_of_ci ci) ->
      (pats' @ (List.tl pi))::rows
    | Parsetree.P_wild | Parsetree.P_const _ | Parsetree.P_var _ ->
      ((repeat_pattern (List.hd pi) cargs_count)@(List.tl pi))::rows
    | _ -> rows in
  let p' = List.fold_left spec_vector [] p in
  let q' = List.hd (spec_vector [] q) in
  (normalize_matrix p'), q'

(* Get the list of root constructors of a matrix column. *)
let rec constructors_list col = match col with
  | [] -> []
  | p::tl -> begin match p.Parsetree.ast_desc with
    | Parsetree.P_constr(ci, al) ->
      (ci, List.length al, p)::(constructors_list tl)
    | _ -> constructors_list tl
  end

(* Get the default matrix for the urec algorithm. *)
let rec default_matrix p = match p with
  | [] -> []
  | (p::tlr)::tlp -> begin match p.Parsetree.ast_desc with
    | Parsetree.P_wild | Parsetree.P_const _
    | Parsetree.P_var _ -> tlr::(default_matrix tlp)
    | _ -> default_matrix tlp
  end
  | _ -> assert false

(* Extract the name of a sum type. *)
let string_of_sum_type t =
  let def _ = "" in
  let def2 _ _ = "" in
  let fconstruct _ s _ = s in
  Types.extract_type_simple def def2 def def fconstruct def def2 t

(* Urec algorithm. *)
let rec urec p q typing_env = match p, q with
  | [], _ -> true
  | []::_, _ -> false
  | _, q1::tail_q -> begin match q1.Parsetree.ast_desc with
    | Parsetree.P_constr (ci, pats) -> (* q1 is a constructed pattern *)
      let p', q' = spec_matrix ci (List.length pats) p q in
      urec p' q' typing_env
    | Parsetree.P_wild | Parsetree.P_const _
    | Parsetree.P_var _ -> (* q1 is a wildcard *)
      let first_col, _ = matrix_one_col_out p in
      let found_cstrs = constructors_list first_col in
      begin match found_cstrs with
        | [] -> 
          urec (default_matrix p) tail_q typing_env
        | (_, _, some_cstr)::_ ->
          let t =  begin match some_cstr.Parsetree.ast_type with
            | Parsetree.ANTI_type st -> string_of_sum_type st
            | _ -> assert false
          end in
          let all_cstrs = get_constructors_of_a_type t typing_env in
          let complete_sig = List.for_all
            (fun (ci, _) -> List.exists
              (fun (ci', _, _) -> (string_of_ci ci) = (string_of_ci ci'))
                found_cstrs)
            all_cstrs in
          if complete_sig then
            List.exists (fun (c, count, _) ->
              let p', q' = spec_matrix c count p q in
              urec p' q' typing_env) found_cstrs
          else 
            urec (default_matrix p) tail_q typing_env
      end
    | _ -> assert false
    end
  | _ -> assert false

(* Normalize the patterns and use the urec algorithm. *)
let urec_norm pats qpat typing_env =
  match normalize (qpat::pats) with
    | q::p -> urec p q typing_env
    | _ -> assert false

(* Dummy Wildcard pattern for exhaustiveness check. *)
let dummy_wild_pattern =
  { Parsetree.ast_loc = Location.none;
    Parsetree.ast_desc = Parsetree.P_wild;
    Parsetree.ast_annot = [];
    Parsetree.ast_type = Parsetree.ANTI_none;
  }

(* Patterns usefulness check. *)
let rec check_usefulness pats typing_env = match pats with
  | [] -> true
  | _::[] -> true
  | p::tl_pats -> check_usefulness tl_pats typing_env &&
    if urec_norm tl_pats p typing_env then true else
    raise (Match_useless_case p.Parsetree.ast_loc)

(* Verify a pattern matching expression. *)
let verify typing_env m_expr = match m_expr.Parsetree.ast_desc with
  | Parsetree.E_match (_, pattern_expr_list) ->
  let pats, _ = List.split pattern_expr_list in
  let res = urec_norm pats dummy_wild_pattern typing_env in
  let _ = check_usefulness (List.rev pats) typing_env in
  if res then raise (Match_not_exhaustive m_expr.Parsetree.ast_loc)
  else ()
  | _ -> assert false

(* Search for pattern matching expressions in the AST. *)
let rec verify_matchings typing_env stuff_to_compile =
  match stuff_to_compile with
    | Infer.PCM_species (species_def, _, _) ->
      verify_matchings_species typing_env species_def
    | Infer.PCM_let_def (let_def, _) -> verify_matchings_let typing_env let_def
    | Infer.PCM_expr (expr) -> verify_matchings_expr typing_env expr
    | _ -> ()
and verify_matchings_species typing_env species_def = List.iter
  (fun spe_field -> match spe_field.Parsetree.ast_desc with
     | Parsetree.SF_let (let_def) -> verify_matchings_let typing_env let_def
     | _ -> ()
  )
  species_def.Parsetree.ast_desc.Parsetree.sd_fields
and verify_matchings_let typing_env let_def = List.iter
  (fun binding -> match binding.Parsetree.ast_desc.Parsetree.b_body with
     | Parsetree.BB_logical (logical_expr) ->
       verify_matchings_logexpr typing_env logical_expr
     | Parsetree.BB_computational (expr) ->
       verify_matchings_expr typing_env expr
  )
  let_def.Parsetree.ast_desc.Parsetree.ld_bindings
and verify_matchings_logexpr typing_env logical_expr =
  match logical_expr.Parsetree.ast_desc with
    | Parsetree.Pr_forall (_,_,logical_expr)
    | Parsetree.Pr_exists (_,_,logical_expr)
    | Parsetree.Pr_not (logical_expr)
    | Parsetree.Pr_paren (logical_expr) ->
      verify_matchings_logexpr typing_env logical_expr
    | Parsetree.Pr_imply (le1, le2)
    | Parsetree.Pr_or (le1, le2)
    | Parsetree.Pr_and (le1, le2)
    | Parsetree.Pr_equiv (le1, le2) -> verify_matchings_logexpr typing_env le1;
      verify_matchings_logexpr typing_env le2
  | Parsetree.Pr_expr (expr) -> verify_matchings_expr typing_env expr
and verify_matchings_expr typing_env expr = match expr.Parsetree.ast_desc with
  | Parsetree.E_paren (expr)
  | Parsetree.E_record_access (expr, _)
  | Parsetree.E_fun (_, expr) -> verify_matchings_expr typing_env expr
  | Parsetree.E_app (expr, expr_list) -> List.iter
      (verify_matchings_expr typing_env) (expr::expr_list)
  | Parsetree.E_tuple (expr_list)
  | Parsetree.E_sequence (expr_list)
  | Parsetree.E_constr (_,expr_list) ->
    List.iter (verify_matchings_expr typing_env) (expr_list)
  | Parsetree.E_if (expr1, expr2, expr3) ->
    verify_matchings_expr typing_env expr1;
    verify_matchings_expr typing_env expr2;
    verify_matchings_expr typing_env expr3
  | Parsetree.E_let (let_def, expr) -> verify_matchings_let typing_env let_def;
    verify_matchings_expr typing_env expr
  | Parsetree.E_record (label_expr_list) ->
    let _, expr_list = List.split label_expr_list in
    List.iter (verify_matchings_expr typing_env) expr_list
  | Parsetree.E_record_with (expr, label_expr_list) ->
    let _, expr_list = List.split label_expr_list in
    List.iter (verify_matchings_expr typing_env) (expr::expr_list)
  | Parsetree.E_match (expr2, pattern_expr_list) -> verify typing_env expr;
    let _, expr_list = List.split pattern_expr_list in
    List.iter (verify_matchings_expr typing_env) (expr2::expr_list)
  | _ -> ();;
