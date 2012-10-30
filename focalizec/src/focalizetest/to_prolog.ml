open Expr_prolog;;
open Own_prop;;
open Own_prolog;;
open Focalize_inter;;
open Context_test;;
open Own_basics;;

(* Convert a focal function name to a prolog predicate name *)
let prolog_predicate cp n =
  let shallow = Context_test.letter_string_of_cp cp ^ n in
  Fresh_variable.get_from_existing_prefix n shallow;;

(** [add_elem e l]

Add the element [e] in the list [l] if the element is not in l.
*)
let add_elem e l = if List.mem e l then l else e::l;;


(** [merge_list l1 l2]

Add all elements of [l2] in the list [l1].
Each element of [l2] which is in [l1] is not added in [l1].
If [l2] is empty then remove all duplicated element of l1.

*)
let merge_list l1 l2 = List.fold_left (fun s e -> add_elem e s) l1 l2;;

let list_fst_del (e, _) l =
  let rec aux l =
    match l with
    | [] -> []
    | e'::r' -> if fst e' = e then r' else e'::aux r' in
  aux l;;

let list_fst_dels l1 l2 =
  List.fold_right list_fst_del l1 l2;;

open Own_types;;

let rec prolog_of_typ t =
  match t with
  | TAtom(_s_o, n) -> prolog_fun n []
  | TSpecPrm(_) -> failwith "prolog_of_typ: Invalid typ" 
  | TFct(_, _) -> prolog_fun "fun" [prolog_int (get_arity t)]
  | TProd(a,b) -> prolog_fun "pair" [prolog_of_typ a; prolog_of_typ b]
  | TPrm(_s_o, n, p) -> prolog_fun n (List.map prolog_of_typ p)
;;

let get_vars =
  let aux_args e =
      match e with
      | FVar (FVInt v) -> [v, TAtom(Some focbasics, foctint)]
      | FVar (FVHer(v, t)) -> [v, t] (* A supprimer *)
      | _ -> [] in
  let rec aux expr = 
  match expr with
  | FFun(v_l, e1) -> List.fold_left (fun s v -> list_fst_del v s) (aux e1) v_l
  | FVarloc(FVInt v, e1, e2)
  | FIfte(v,e1, e2) -> add_elem (v, TAtom(Some focbasics, foctbool)) (merge_list (aux e1) (aux e2))
  | FMethVar(x, t, l) -> (x, t)::List.fold_left (fun s e -> merge_list (aux_args e) s) [] l
  | FMeth(_,_s,l) -> List.fold_left (fun s e -> merge_list (aux_args e) s) [] l
            (** a function is applied to a list of string/integer name/value *)
  | FBasic(_, l) -> List.fold_left (fun s e -> merge_list (aux_args e) s) [] l
  | FMatch(_,l) ->
     List.fold_left (fun s (_,_a,e) -> merge_list s (aux e)) [] l
  | FVarloc(_, e1, e2) -> merge_list (aux e1) (aux e2)
  | FValue(FVar (FVHer(x, t)))
  | FValue(FVar (FVFun(x, t))) ->
      [x, t]
  | FValue(FVar (FVInt t)) -> [t, TAtom(Some focbasics, foctint)]
  | FValue(FInt _)
  | FValue(FConstruct _) -> [] in
  aux
;; (* (** the value of a variable or an integer *) *)

let get_int_vars e =
  let l = get_vars e in
  List.filter (function (_, TAtom(Some _, "int")) -> true | _ -> false) l;;

(*

(*
 match l with
 | [] -> failwith "and_of_list"
 | e::r ->
     List.fold_left
       (fun s e -> prolog_fun "#/\\" [s;e])
       e
       r;;
*)
*)

(** [get_meths_of_expr expr]
 Takes an expression. 
 Returns the list of method's name used in the expression.
*)
let get_meths_of_expr (cp : context_path) : 'a -> (context_path * string) list =
  let rec aux m =
    match m with
    | FFun(_,e1) -> aux e1
    | FVarloc(_,e1,e2)
    | FIfte(_,e1,e2) -> aux e1 @ aux e2
    | FMethVar(_, _, _) -> []
    | FMeth(spec, m, _args) ->
        if spec = focself then [cp,m] else [cp_add cp spec, m]
    | FMatch(_,p_a_e_l) -> List.fold_left (fun s (_,_,e) -> s @ aux e) [] p_a_e_l
    | FBasic(_,_) 
    | FValue _ -> [] in
  aux;;

(**
This prolog_variable is used for storing the prolog policy for the
constraint match and ite.
*)
let env_variable = Fresh_variable.new_prolog_var ();;

(* Le dictionnaire:
    Soit on calcul un résultat (ex: int_plus), soit on quelquechose qui donne des
    contraintes (ex: int_eq) et donc pas de calcul de résulat.

    Dans un cas le résultat est donnée par l'expression, dans l'autre cas il
    faut préciser la variable dans laquelle le résultat doit être rangé.
 *)

let rec prolog_of_focarg v =
  match v with
  | FInt i -> Prolog_int i
  | FVar (FVInt v) -> Prolog_var (Fresh_variable.get_from_existing v)
  | FVar (FVHer(v, _)) -> Prolog_var (Fresh_variable.get_from_existing v)
  | FVar (FVFun(v, _)) -> Prolog_var (Fresh_variable.get_from_existing v)
  | FConstruct(f,l) -> 
      if f = "true" then
        Prolog_int 1
      else if f = "false" then
        Prolog_int 0
      else
        let f = Own_basics.atom_of_cons_name f in
        Prolog_fun(f, prologs_of_focargs l)
and prologs_of_focargs l = List.map prolog_of_focarg l;;

let basics_noresult = (* used only in a decision of a if *)
  ["int_eq",(fun l -> prolog_fun "#=" l)];;

let basics_result = (* used otherwise *)
(*   let and_op v l = prolog_fun "and"  in *)
  let equal_op x op l = (* construct x #= op(l) *)
    prolog_fd_equal (prolog_var x) (prolog_fun op l) in
  ["focalize_error", (fun _l _v -> prolog_fun "fail" []);
   "~0x", (fun l v -> equal_op v "-" (prolog_int 0 :: l));
   "=0x", (fun l v -> prolog_fun "#<=>" [prolog_var v; prolog_fun "#=" l]);
   "<=0x", (fun l v -> prolog_fun "#<=>" [prolog_var v; prolog_fun "#=<" l]);
   ">0x", (fun l v -> prolog_fun "#<=>" [prolog_var v; prolog_fun "#>" l]);
   ">=0x", (fun l v -> prolog_fun "#<=>" [prolog_var v; prolog_fun "#>=" l]);
   "=", (fun l v -> prolog_fun "unifyD" (prolog_var v ::l));
   "=", (fun l v -> prolog_fun "unifyD" (prolog_var v ::l));
   "+", (fun l v -> equal_op v "+" l);
   "True", (fun _ _ -> prolog_int 1);
   "False", (fun _ _ -> prolog_int 0);
   "succ0x", (fun l v -> prolog_fun "succ" (prolog_var v::l));
   "max0x", (fun l v -> prolog_fun "int_max" (prolog_var v ::l));
   "*", (fun l v -> equal_op v "*" l);
   "-", (fun l v -> equal_op v "-" l);
   "/", (fun l v -> equal_op v "/" l);
   "%", (fun l v -> prolog_fun "int_mod" (prolog_var v ::l));
   "&&", (fun l v -> prolog_fun "and_b" (prolog_var v ::l));
   "||", (fun l v -> prolog_fun "or_b" (prolog_var v ::l));
   "~~", (fun l v -> prolog_fun "not_b" (prolog_var v ::l));
   "<0x", (fun l v -> prolog_fun "#<=>" [prolog_var v; prolog_fun "#<" l]);
   "pair", (fun l v -> prolog_fun "pair" (prolog_var v::l));
   "snd", (fun l v -> prolog_fun "scnd" (prolog_var v::l));
   "fst", (fun l v -> prolog_fun "first" (prolog_var v::l))
  ];;

let get_dictionnary_entry f l (res : string option) =
  try
    match res with
    | None -> List.assoc f basics_noresult l
    | Some s -> List.assoc f basics_result l s
  with
  | Not_found -> failwith (f ^ ": n'est pas une fonction de basics connue");;

(* ************************************************************************* *)
(* ************************************************************************* *)
(* ************************************************************************* *)
(* ************************************************************************* *)

let rec expand_self tc cp t = 
  match t with
  | TAtom(m, s) ->
      if s = focself then
        let species = species_of_cp tc cp in
       expand_self tc cp (get_rep species)
      else
        TAtom(m, s)
  | TSpecPrm(p) ->
      let cp = cp_add cp p in
      expand_self tc cp (TAtom(None, focself))
  | TFct(t1,t2) -> TFct(expand_self tc cp t1, expand_self tc cp t2)
  | TProd(t1,t2) -> TProd(expand_self tc cp t1, expand_self tc cp t2)
  | TPrm(m, t, t_l) -> TPrm(m, t, List.map (expand_self tc cp) t_l);;


let create_itematch_vars tc cp lv =
  let rec aux lv =
    match lv with
    | [] -> []
    | (x,t)::r ->
       (match t with
        | TFct(_,_) -> aux r
        | _ -> prolog_fun "var" [prolog_var (Fresh_variable.get_from_existing x);
                                 prolog_of_typ (expand_self tc cp t)]::aux r
       ) in
  aux lv;;


(** [prolog_term_of_pattern var f_args_e res]

convert a focal pattern into a prolog pattern of form :

[pattern(PAT, LIST_EXPR).]
*)
let rec prolog_term_of_pattern tc cp _var (f,args,e) res arity_list =
  prolog_fun "pattern"
      [(* prolog_list_of_list  *)
          (*[*) (* prolog_equal (prolog_var (get_from_existing var)) *)
                        (prolog_fun (Own_basics.atom_of_cons_name f) (List.map prolog_of_focarg args))
          (*]*);
       prolog_list (prolog_term_of_minifoc tc cp e res arity_list)
       ]
and
(** [prolog_term_f_focexpr e res] Takes an expression [e] and returns the prolog
    term, translation of [e]. This prolog term put the result into [res]. *)
  prolog_term_of_minifoc tc cp e res arity_list =
  match e with 
  | FIfte(v,e1,e2) -> 
      [prolog_fun "ite" [prolog_var (Fresh_variable.get_from_existing v);
                         prolog_list 
                          (create_itematch_vars  tc cp
                                    (merge_list (get_vars e1)
                                                (get_vars e2)));
                         prolog_list (prolog_term_of_minifoc tc cp e1 res arity_list);
                         prolog_list (prolog_term_of_minifoc tc cp e2 res arity_list);
                         prolog_var env_variable ]
      ]
  | FMethVar(f, _, s_l) -> (* apply constraint *)
      [prolog_fun "apply" [prolog_var res;
                           prolog_var (Fresh_variable.get_from_existing f);
                           prolog_list (prologs_of_focargs s_l);
                           prolog_var env_variable
                          ]
      ]

  | FMeth(spec, s,s_l) ->
      let ncp = if spec = focself then cp else Context_test.cp_add cp spec in
      let prolog_m = prolog_predicate ncp s in
      let clos_var = prolog_var (Fresh_variable.new_prolog_var ()) in
      let nb_args = try List.assoc (ncp, s) arity_list with | Not_found ->
        failwith ("Internal error O_o : " ^ string_of_cp cp ^ s) in
      let closure =
        prolog_fun "closure_defs" [clos_var;
                                   prolog_fun prolog_m []; (* nom de la fonction *)
                                   prolog_int nb_args; (* Nombre d'arguments *)
                                   prolog_list []; (* Les arguments déjà donnés *)
                                   prolog_list [] (* Les variables libres *)
                                  ]
      in
      closure::[prolog_fun "apply" [prolog_var res;
                           clos_var;
                           prolog_list (prologs_of_focargs s_l);
                           prolog_var env_variable
                          ]
               ]
  | FBasic(s,s_l) -> (* Il faut utiliser ici les contraintes normales *)
     [get_dictionnary_entry s (prologs_of_focargs s_l) (Some res)]
  | FFun(args, body) -> (* We create a new clause *)
      (* The function has its proper return variable *)
      let ret_f =  Fresh_variable.new_prolog_var () in
      let f = res in (* ret is the new defined function *)
      let pbody = prolog_term_of_minifoc tc cp body ret_f arity_list in
      let fv = list_fst_dels args (minifoc_expr_fv body) in
      let fv_prolog =
        List.map (fun e ->
                    prolog_var (Fresh_variable.get_from_existing (fst e)) 
                 ) fv in
      [prolog_fun "fun_eq" [prolog_var f;
                            prolog_var ret_f;
                            prolog_list (List.map (fun e -> prolog_var (Fresh_variable.get_from_existing (fst e)))
                                        args);
                            prolog_list pbody;
                            prolog_list fv_prolog;
                            prolog_var env_variable
                           ]
      ]
  (************************************)
  | FValue(v) -> [prolog_equal (prolog_var res) (prolog_of_focarg v)]
  | FMatch((v,_t),p_l) -> 
      [prolog_fun "match" (prolog_list
      (* A supprimer (remplacer par []) *)
                                (create_itematch_vars tc cp (flatten_no_doublon (List.map (fun (_,_,e) -> get_vars e) p_l)))
     (* ********** *)
       ::
                           prolog_var (Fresh_variable.get_from_existing v) ::
                             (* Liste de   pattern * expression *)
                           [prolog_list
                              (List.map 
                              (fun e -> prolog_term_of_pattern tc cp v e res
                              arity_list) p_l)]
                                @ [prolog_var env_variable]
                          )
      ]
  | FVarloc(FVFun(v, _), FMeth(spec, n, p), e2)
  | FVarloc(FVInt v,     FMeth(spec, n, p), e2)
  | FVarloc(FVHer(v, _), FMeth(spec, n, p), e2) -> 
      prolog_term_of_minifoc tc cp (FMeth(spec, n, p))
                             (Fresh_variable.get_from_existing v) arity_list @
        prolog_term_of_minifoc tc cp e2 res arity_list
  | FVarloc(FVFun(v, _), FBasic(n, p), e2)
  | FVarloc(FVInt v,     FBasic(n, p), e2) 
  | FVarloc(FVHer(v,_),  FBasic(n, p), e2) -> (* Il faut utiliser ici les contraintes réifiées *)
      prolog_term_of_minifoc tc cp (FBasic(n, p))
                             (Fresh_variable.get_from_existing v) arity_list @
      prolog_term_of_minifoc tc cp e2 res arity_list
  | FVarloc(FVInt v,     e1, e2)
  | FVarloc(FVFun(v, _), e1, e2)
  | FVarloc(FVHer(v, _), e1, e2) ->
      prolog_term_of_minifoc tc cp e1 (Fresh_variable.get_from_existing v) arity_list @
      prolog_term_of_minifoc tc cp e2 res arity_list

;;

let prolog_pgm_of_minifoc_function tc
                                   cp
                                   _cur_spec
                                   name
                                   (prm, (minifoc_body, ret_type))
                                   arity_list : prolog_clause =
  let res_var = Fresh_variable.new_prolog_var () in
  let prolog_args = List.map (fun (e, _t) -> prolog_var (Fresh_variable.get_from_existing e)) prm in
  let prolog_m = prolog_predicate cp name in
  let _prolog_head =
    prolog_fun prolog_m
               [prolog_var res_var;
                prolog_list prolog_args; (* les arguments *)
                prolog_list []; (* pas de variable libre *)
                prolog_var env_variable
               ] in
  let to_set = get_vars minifoc_body in
  let set_types = (prolog_fun "set_type" [prolog_var res_var; prolog_of_typ
  (expand_self tc cp ret_type)])::
                  List.map (fun (v,typ) -> prolog_fun "set_type"
                                       [prolog_var (Fresh_variable.get_from_existing v);
                                        prolog_of_typ (expand_self tc cp typ)]) to_set in

(*   print_string
 *   "***********************************************************\n"; *)
(*   List.iter (fun e -> print_string (dbg_string_minifoc_expr e)) s_l; *)
(*   print_string (dbg_string_minifoc_expr minifoc_body); *)
(*   print_newline (); *)
(*   print_string
 *   "***********************************************************\n"; *)
  let var_clos = Fresh_variable.get_from_existing name in
  let l_closure = 
        prolog_fun "closure_defs" [prolog_var var_clos;
                                   prolog_fun prolog_m []; (* nom de la fonction *)
                                   prolog_int (List.length prm); (* Nombre d'arguments *)
                                   prolog_list []; (* Les arguments déjà donnés *)
                                   prolog_list [] (* Les variables libres *)
                                  ] in
  let prolog_body = prolog_term_of_minifoc tc cp minifoc_body res_var arity_list in
  Some (prolog_fun (prolog_m ^ "_add")
                   [prolog_var env_variable]),
  [prolog_fun
    "function_add_atom"
    [prolog_fun prolog_m [];
     prolog_var res_var;
     prolog_list prolog_args;
     prolog_list (set_types @ (l_closure::prolog_body));
     prolog_list [];
     prolog_var env_variable
    ]
  ];;
(*     Some (prolog_head), set_types @ (l_closure:: prolog_body);; *)

let rec add_let l e2 =
  match l with
  | [] -> e2
  | (x,e1)::r ->
      FVarloc(x,e1,add_let r e2)
;;

open Own_expr;;
open Own_types;;

let prolog_of_precond tc _vars sat p arity_list =
 let expr = Expr_prolog.minifoc_expr_of_myexpr p in
 Prolog_comment (string_of_myexpr p)::
 prolog_term_of_minifoc tc (Context_test.empty_path) expr sat arity_list;;

(*
(* From a species name and a proposition name, 
   returns the name of all méthod to convert in prolog, the variable involved in
   the proposition and the precondtion of the property *) 
let prolog_from_prop species prop_name =
  let def = get_prop_def species prop_name in
  let l_precond = get_vars_precond def in
  let meth = get_meths_from_def_list (List.map precond_get_expr (snd l_precond)) in
  let ml = get_all_meths_dep species meth in
  (ml, l_precond);;
*)

(** [get_args_minifoc sn f]
 
 From a species name [sn] and a function name [f], returns a 3-tuple [args,
 (def, t)] where [args] are the formal argument of [f], [def] the definition of
 [f] and [typ] the type of value returned by [f].
*)
let get_args_minifoc sn f =
  let args, (def, typ) = get_meth_def_split sn f in
  let pdef = minifoc_expr_of_myexpr def in
  args, (pdef, typ);;

(** [get_meth_dep spec meth]

 Returns the list of method's name used in the method [method] in species [spec].
*)
let get_meth_dep (tc : test_context) (cp : context_path)
                 meth : (context_path * string) list = 
  let _args, (def, _typ) = get_args_minifoc (species_of_cp tc cp) meth in
  get_meths_of_expr cp def;;


(** [get_meth_dep_closure spec meth]

 Returns the list obtained by taking the closure of get_meth_dep.
*)
let get_meth_dep_closure (tc : test_context) meths cumul : (context_path * string) list=
  let rec aux l1 l2 =
    match l2 with
    | [] -> l1
    | (cp,e)::r ->
        if List.mem (cp,e) l1 then
          aux l1 r
        else
          aux ((cp,e)::l1) (l2 @ (get_meth_dep tc cp e)) in
  aux cumul meths;;


let load_functions_in_environment l : prolog_clause =
  let name = "f_" ^ Fresh_variable.get_from_existing "load_functions" in
  let ev = prolog_var env_variable in
  Some (prolog_fun name [ev]),
  Prolog_fun("true", [])::
  List.map (fun (cp, n) ->
       let n = prolog_predicate cp n ^ "_add" in
       prolog_fun n [ev]) l;;

let create_needing_meths (tc : test_context)
                         _vars
                         (prop : elementaire list) : (prolog_pgm * 'a list) =
       (* Get the predonditions *)
  let preconds = List.map
                   (fun s ->
                     List.map (minifoc_expr_of_myexpr) (get_precond s)) prop in
       (* Collect the list of methods *)
  let meth_needed = List.fold_left (fun s e -> 
                                      get_meth_dep_closure tc (get_meths_of_expr empty_path e) s)
                                     []  (List.flatten preconds) in
       (* Get the arguments of all functions and species name *)
  let aux_list =
     List.map (fun (cp, n) ->
                let cur_spec = species_of_cp tc cp in
(*                print_string (string_of_cp cp); *)
(*                print_string n; *)
(*                print_newline (); *)
                let args = get_args_minifoc cur_spec n in
                  (cur_spec, (cp, n), args), ((cp, n), List.length (fst args))) meth_needed in
  let aux_list, arity_list = List.split aux_list in
       (* and finally get the corresponding clause *)
  let clauses = List.map (function (cur_spec, (cp,n), args)  ->
                            prolog_pgm_of_minifoc_function tc cp cur_spec n args arity_list)
                         aux_list in
  (clauses @ [load_functions_in_environment meth_needed],
      arity_list);;
(** [create_needing_meth tc vars elems]

From a test context [tc], the list ol elementary forms and the list of
quantified variables [vars]. Return the list of clause that is the translation
of the methods involved in all preconditions of the elementary forms.
*)

(* Convert Own_types.typ term to string *) 
(*
let rec prolog_of_typ =
        let rec print prec exp = 
                match exp with
                | TAtom(m, s) -> prolog_fun s []
                | TSpecPrm s -> prolog_fun s []
                | TFct (t1,t2) -> failwith "prolog_of_typ"
                | TProd (t1,t2) -> prolog_fun "pair" [prolog_of_typ t1;
                                                     prolog_of_typ t2]
                | TPrm (m, s, l) -> prolog_fun s (List.map prolog_of_typ l)
        in
         fun cp t -> 
           let tc = get_test_context () in
            print 0 (expand_self tc cp t);;
*)

let rec list_insert i l =
  match l with
  | [] -> []
  | e::r -> e::i:: list_insert i r;;

let create_goal tc vars (elem : elementaire) name i arity_list : prolog_clause * prolog_clause =
  let expand_self = expand_self tc empty_path in
  let sat = Fresh_variable.new_prolog_var () in
  let unsat = Fresh_variable.new_prolog_var () in
  let goal_name = Fresh_variable.prolog_pgm_name name i in
  let state_name = Fresh_variable.prolog_pgm_state_name name i in
  let precond = get_precond elem in
  let precond_truth = List.map (fun e ->
                                  let nv = Fresh_variable.new_prolog_var () in
                                  prolog_of_precond tc vars nv e arity_list, nv) precond in
  let truth_vars = snd (List.split precond_truth) in
  let var_var = variables_map_esc (fun i -> i) vars in
  let var_var = List.filter (fun e -> match get_variable_type e with TFct(_,_) -> false | _ -> true) var_var in
  let set_domains = List.map 
                      (fun var ->
                                prolog_fun "set_type"
                                [prolog_var (Fresh_variable.get_from_existing
                                (get_variable_name var));
                                            prolog_of_typ (expand_self (get_variable_type
                                                              var))] ) var_var in
  (Some (prolog_fun goal_name []),
  set_domains @
  [prolog_fun "#=" [prolog_var sat; prolog_int 1];
   prolog_fun "#=" [prolog_var unsat; prolog_int 0];
   prolog_fun "init_env" [prolog_var env_variable;
                          prolog_int 2;
                          prolog_fun (Whattodo.get_prolog_opt ()) []
                         ];
  ] @ [prolog_fun ("f_" ^Fresh_variable.get_from_existing "load_functions")
      [prolog_var env_variable]] @
  List.flatten (fst (List.split precond_truth)) @
  [prolog_fun "fin_env" [prolog_var env_variable]] @
  (if Whattodo.get_mcdc_number () = 0 then
     list_insert (prolog_fun "fin_env" [prolog_var env_variable])
                 (List.map (fun e -> prolog_fun "=" [prolog_var e; prolog_int 1]) truth_vars) @
     [prolog_fun "label_and_write"
                 [prolog_list (variables_map_esc (fun e -> prolog_var (Fresh_variable.get_from_existing (get_variable_name e))) vars);
                  prolog_int (Whattodo.get_number_of_test ());
                  prolog_fun (Fresh_variable.prolog_pgm_res name i) [];
                  prolog_var (env_variable);
                  prolog_fun name [];
                  prolog_int 1;
                  match Whattodo.get_prolog_stat_file () with
                  | None ->   prolog_fun "none" []
                  | Some e -> prolog_fun "some" [prolog_var e]
                 ]]
  else
     [prolog_fun "label_and_write_mcdc"
                 [prolog_list (variables_map_esc (fun e -> prolog_var (Fresh_variable.get_from_existing (get_variable_name e))) vars);
                  prolog_int (Whattodo.get_mcdc_number ());
                  prolog_list (List.map prolog_var truth_vars);
                  prolog_fun (Fresh_variable.prolog_pgm_res name i) [];
                  prolog_var (env_variable);
                  prolog_fun name [];
                  prolog_int 1;
                  match Whattodo.get_prolog_stat_file () with
                  | None ->   prolog_fun "none" []
                  | Some e -> prolog_fun "some" [prolog_var e]
                 ]]

  ))
  ,
  let file = Fresh_variable.new_prolog_var () in
  (
    Some(prolog_fun state_name []),
    [prolog_fun "save_program" [prolog_fun goal_name [];
              prolog_fun ";" [prolog_fun goal_name [];
              Prolog_conjunction([prolog_fun "open" [ prolog_fun (Fresh_variable.prolog_pgm_res name i) []; prolog_fun "write" []; prolog_fun file []];
                                  prolog_fun "close" [prolog_fun file []];
                                  prolog_fun "halt" []]) 
                                          ]
                               ];
    prolog_fun "halt" []
    ]);;
(** [create_goal (vars, p_l)]

Takes a list of variables and the precondition
Returns a list of clause to call corresponding to the constraints in the
precondition.

*)


(** [create_prolog_file tc normals_forms file_name]
 Takes the test context named [tc] we test, the list of all normals forms to
 test.

 It creates the file [file_name] which contains all the needs to generate test
 data for the normal forms. Its returns the list of prolog predicate to call for
 generating the test data for each normals forms. Each predicate take into
 parameters the numbers of test data to generate.

*)

let create_prolog_goals tc vars name elems arity_list : ((prolog_clause * prolog_clause) * elementaire) list =
  let rec my_map l i=
    match l with
    | [] -> []
    | e::r -> (create_goal tc vars e name i arity_list, e):: my_map r (i+1) in
  my_map elems 0;;

let create_prolog_file (tc : test_context)
                       (name : string)
                       (vars : variables)
                       (elems : elementaire list) =
  if Whattodo.get_use_prolog () then 
    begin
    let sortie = open_out (Whattodo.get_file_output_prolog name) in

(*   print_string ("TEST DE (" ^ string_of_tc tc ^ "). "); *)
(*   print_string("Sur une forme normale de \"" ^ name ^ "\"\n"); *)

    let (file, arity_list) = create_needing_meths tc vars elems in
    let goals = create_prolog_goals tc vars name elems arity_list in
    let output_formatter = Format.formatter_of_out_channel sortie in
    let print_file = List.iter (Format.fprintf output_formatter "%a\n"
                                Print_prolog.print_prolog_clause) in
    print_file [prolog_clause None [prolog_list [prolog_fun 
                               ("'" ^ Whattodo.get_prolog_path () ^ "/all'") []]]];
    print_file [Test_prolog.import_all_types ()];
    print_file file;
    print_file (List.map fst (List.map fst goals));
    print_file (List.map snd (List.map fst goals));
    close_out sortie;
(*   print_string "\nFIN DE TEST.\n"; *)
    let goals = List.map (fun ((e,_t),y) -> e,y) goals in
    file, goals
    end
  else
    let file = [] in
    let goals = List.map (fun e -> prolog_clause None [], e) elems in
    file, goals
;;
