open Rewrite_prop;;
open Random_rep;;

open To_strings;;
open Own_prop;;
open Own_types;;
open Own_basics;;
open Useful_parsing;;
open Own_expr;;

open Whattodo;;
open Print_xml;;
open Focalize_inter;;
open Fresh_variable;;

(* pour avoir le nom des methodes  *)
let pre_cond_string i = "prop_pre_cond_" ^ (string_of_int i);;
let conclu_string i = "prop_conclu_" ^ (string_of_int i);;
let rapport_test i = "rapport_test_elem_" ^ string_of_int i;;
let rapport_test_reinject i = "rapport_test_elem_reinject" ^ string_of_int i;;

let submit_test i = "test_elem_" ^ string_of_int i ^ "_submit";;
let get_variables_string i = "variables_to_string_" ^ string_of_int i;;
let test_from_prolog i = "from_prolog" ^ string_of_int i;;

let test_elem i  = "test_elem_" ^ (string_of_int i);;
let test_elem_reinject i  = "test_elem_reinject_" ^ (string_of_int i);;

let prefix_var e = "_" ^  e;;

(* Create an expression calculing the conjunction of each elements of the list
* *)

let conjonction_call l =
  match l with
  | [] -> expr_glob foctrue
  | e::r -> List.fold_right
              (fun e seed -> expr_app (expr_glob focand) [e;seed])
              r e;;

(* idem with disjunction *)
let disjonction_call l =
  match l with
  | [] -> expr_glob foctrue
  | e::r -> List.fold_right
              (fun e seed -> expr_app (expr_glob focor) [e;seed])
              r e;;

(** [ast_bind_lazy_evaluation n i (e_1::...::e_n) expr] creates the expression :

[
let n_i = fun () -> e_i in
...
let n_{i+m} = fun () -> e_{i+m} in
 expr
]
*)
let rec ast_bind_lazy_evaluation n i l =
  match l with
  | [] -> fun expr -> expr
  | e::r ->
     fun expr ->
       expr_let_notyp (n ^ string_of_int i)
                (expr_fun "x" (TAtom(Some "basics", foctunit)) e)
                (ast_bind_lazy_evaluation n (i+1) r expr);;


let rec ast_call_lazy_evaluation n i l =
  if i >= l then
    expr_glob focnil
  else
    expr_app (expr_glob foccons)
     [expr_var (n ^ string_of_int i);
      ast_call_lazy_evaluation n (i+1) l
     ];;

let rec ast_unfold_args var_l reste =
  match var_l with
  | [] -> reste
  | (n,_t)::[] ->
      expr_let_notyp n (expr_var "__me") reste
  | (n,_t)::r ->
      expr_let_notyp n (expr_basic focfst [expr_var "__me"]) (
      expr_let_notyp "__me" (expr_basic focsnd [expr_var "__me"]) (
      ast_unfold_args r reste))
;;

(* Generate the function testing the precondition *)
let ast_test_pre_cond =
  fun e args i ->
    let c = get_precond e in
    let m = Unique (meth_create (pre_cond_string i)
                        (parse_type "@RESULT*@LIST(@RESULT)")
                           (expr_fun "__me" (variables_to_tuple_type args) (ast_unfold_args (variables_to_list args)
                                 (ast_bind_lazy_evaluation "v" 1 c
                                    (expr_app (expr_glob (prefix "eval_list_expr_and"))
                                              [ast_call_lazy_evaluation "v" 1 (1 +
                                               List.length c)]))
                           ))
                         false) in
    m;;

let ast_parse_one_res_prolog (name : string) =
  let rec decompose_one l err i vars =
    match l with
    | [] -> string_of_myexpr (variables_to_tuple vars)
    | (e,t)::l -> "match l with
                | @NIL -> " ^ err ^ "
                | @CONS(" ^ e ^ ",l) ->
                   let " ^ e ^ " = !" ^ parse_meth (string_of_ttyp t) ^ "(" ^ e ^ ") in " ^  
          decompose_one  l err (i+1) vars in
  fun vars -> 
  let vsl = variables_to_list vars in
  let err = "@FOC_ERROR(\"parse_error : " ^ prolog_pgm_get_res name ^ "\")" in
   Unique (
  parse_foc_meth
     ("let " ^ prolog_pgm_get_res name ^ " in
            @LIST(@STRING) ->   @LIST(" ^ string_of_typ (variables_to_tuple_type vars) ^ ") =
       let rec aux = fun s in (@STRING) ->
                       let parsed = #split_cons(s) in
                       let cons = @FST(parsed) in
                       let l = @SND(parsed) in 
                       if @STRUCT_EQUAL(cons,\"vars\") then
                       " ^ decompose_one vsl err 1 vars ^ "
                       else
                       " ^ err ^ " in
        fun l in (@LIST(@STRING)) ->
          #list_map(aux, l)"));;
      
let ast_test_from_prolog i (name : string) _prolog_pgm =
  Unique (parse_foc_meth
     ("let " ^ test_from_prolog i ^ " in (@BOOL * @LIST(@LIST(@RESULT))) * (float * float) = 
       fun n in ( @UNIT) ->
         let d_f = #call_prolog2(\"" ^ get_file_output_prolog name ^ "\",
                                 \"" ^ prolog_pgm_state_name name i ^ "\", " ^
                                 "\"" ^ prolog_pgm_name name i ^ "\", " ^
                                 "\"" ^ name ^ "\", " ^
                                 "\"" ^ get_prolog_opt () ^ "\") in
         let lines = #get_all_lines(\"" ^ prolog_pgm_res name i ^ "\") in
         let l = !" ^ prolog_pgm_get_res name ^ "(lines) in
           let rec aux = fun l -> fun b -> fun res -> fun nb ->
             match l with
             | @NIL ->
                 (#print_string(\"\\n\");
                  " ^ top_xml_coll_name ^ "!xml_close_elementaire(res, 1, nb, compute_time(d_f), \"constraint\");
                  @CRP(b, res)
                 )
             | @CONS(cur,r) -> 
                 (let pval = !" ^ pre_cond_string i ^ "(cur) in
                  let vars = !" ^ get_variables_string i ^ "(cur) in
                  let cval = !" ^ conclu_string i ^ "(cur) in
                  " ^ top_xml_coll_name ^ "!xml_open_test(@VUNIT);
                  " ^ top_xml_coll_name ^ "!xml_result_test(vars, pval, cval);
                  " ^ top_xml_coll_name ^ "!xml_close_test(@VUNIT);
                  aux(r,
                      @AND(b, #result_ok(@FST(cval))),
                      @CONS(@SND(cval), res),
                      @SUCC(nb))
                 ) in
                 @CRP(aux(l, @TRUE, @NIL, 0), d_f)"));;

(* idem with conclusion *)
let ast_test_conclu =
  fun e args i ->
    let c = get_conclusion e in
    let m = Unique (meth_create (conclu_string i)
                        (parse_type "@RESULT*@LIST(@RESULT)")
                           (expr_fun "__me" (variables_to_tuple_type args) (ast_unfold_args (variables_to_list args)
                                 (ast_bind_lazy_evaluation "v" 1 c
                                    (expr_app (expr_glob (prefix "eval_list_expr_or"))
                                              [ast_call_lazy_evaluation "v" 1 (1 +
                                               List.length c)]))
                           ))
                         false) in
    m;;

let ast_variables_to_list_string vars i =
  let get_print_var t = print_meth (string_of_ttyp t) in
  let get_print_xml_var t = print_xml_meth (string_of_ttyp t) in
  let rec print_vars l =
    match l with
    | [] -> expr_basic focnil []
    | (e,t)::r -> expr_basic foccons
                             [expr_basic foccrp  
                                   [expr_meth focself
                                        (get_print_var t)
                                        [expr_var e];
                                    expr_meth focself
                                        (get_print_xml_var t)
                                        [expr_var e]
                                   ];
                              print_vars r 
                             ] in
  Unique (meth_create (get_variables_string i)
              (parse_type ("@LIST(@STRING*@STRING)"))
              (expr_fun "__me" 
                (variables_to_tuple_type vars)
                (ast_unfold_args
                 (variables_to_list vars)
                 (print_vars (variables_to_list vars)))
              )
              true);;

let ast_test_submit _e vars i =
(*   let vars_call = to_args (fun (n,_) -> n) (variables_to_list vars) in *)
  let mcdc = if get_mcdc_number () = 0 then "@FALSE" else "@TRUE" in
  let vars_tuple = string_of_myexpr (variables_to_tuple vars) in
  let def =
    "let " ^ submit_test i ^ foc_argsdef_of_variables vars ^ " in @LIST(@INT) * " ^ verdict_type ^ "  =
       fun requirement ->
       let pval = !" ^ pre_cond_string i ^ "(" ^ vars_tuple ^ ") in 
       let test_number = list_occur(@SND(pval)) in
       let test_accepted =
         if " ^ mcdc ^ " then
           list_in(test_number, requirement)
         else
           #result_ok(@FST(pval)) in
       if test_accepted then
         " ^ top_xml_coll_name ^ "!xml_open_test(@VUNIT);
         let lval = !" ^ get_variables_string i ^ "(" ^ vars_tuple ^ ") in
         let cval = !" ^ conclu_string i ^ "(" ^ vars_tuple ^ ") in
         " ^ top_xml_coll_name ^ "!xml_result_test(lval, pval, cval);
         " ^ top_xml_coll_name ^ "!xml_close_test(@VUNIT);
         @CRP(#list_del_one(test_number, requirement),
              #" ^ fst verdict_precond_ok ^ "(#result_ok(@FST(cval)), @SND(cval)))
       else
         @CRP(requirement, #" ^ fst verdict_precond_ko ^ ")" in
  Unique (parse_foc_meth def);;


(* Test report *)

(* generate random value of variables *)
let rec ast_random_var lvar =
  match lvar with
  | [] -> ""
  | (e,t)::r -> 
(*       print_string (string_of_ttyp t); *)
(*       print_newline (); *)
      let meth_name = random_meth (string_of_ttyp t) in
      "let " ^ e ^ "= !" ^ meth_name ^ "(" ^ string_of_int (get_size_value_test ()) ^ ") in\n" ^
        ast_random_var r ;;


let rec ast_var_from_strings lvar l_value =
  let get_spec t =
    match t with
    | TAtom(_m, n) when n = focself -> focself
    | TSpecPrm(s) -> s
    | _ -> prerr_string "Variable type not handled\n";exit 1 in
  match lvar,l_value with
  | [], [] -> "@UNIT"
  | (_e,t)::[], v::[]  -> get_spec t ^ "!parse(" ^ v ^ ")"
  | (_e,t)::vars, v::vals  ->
     "@CRP(" ^ get_spec t ^ "!parse(" ^ v ^ ")," ^
      ast_var_from_strings vars vals ^")"
  | _ -> prerr_string "Error in the XML file, number of values mismatch with number of variables\n"; exit 3;;

(* Take an elementary form and return the function testing it *)
let ast_test_elem_i =
  let get_print_var t = print_meth (string_of_ttyp t) in
  let rec print_vars l =
    match l with
      [] -> "@NIL"
    | (e,t)::r -> "@CONS(" ^ get_print_var t ^ "(" ^ e ^ "), " ^ print_vars r ^ ")"  in
  fun (x : elementaire) lvar  elem_num ->
    let test_elem_i = test_elem elem_num in
    let mcdc = if get_mcdc_number () = 0 then "@FALSE" else "@TRUE" in
    let nb_conclu = string_of_int (List.length (list_of_conclusion (get_conclusion x))) in
    Unique (
    parse_foc_meth
    (" let rec " ^ test_elem_i ^ " in (@BOOL * (@LIST(@LIST(@RESULT)) * @INT )) * @FLOAT =
      fun n              in (@INT) ->
      fun requirement    in (@LIST(@INT)) ->
      fun b              in (@BOOL) -> 
      fun j_t            in (@LIST(@LIST(@RESULT))) ->
      fun nb_try         in (@INT) ->
      fun nb_consec_fail in (@INT) ->
      fun deb in (@FLOAT) ->
        if @OR(@AND(@NOT(" ^ mcdc ^ "), @INT_EQUAL(n , 0)),
               @AND(     " ^ mcdc ^ " , @STRUCT_EQUAL(requirement, @NIL))) then
          (
            let fin = #get_time(@VUNIT) in
            let _saute_ligne = #print_string(\"\\n\") in
             " ^ top_xml_coll_name ^ "!xml_close_elementaire(j_t,
                                                             " ^ nb_conclu ^ ",
                                                             nb_try,
                                                             compute_time(@CRP(deb,fin)),
                                                             \"random\");
              @CRP(@CRP(b, @CRP(j_t, nb_try)), fin)
          )
        else 
          " ^ ast_random_var (variables_to_list lvar ) ^ (* Lot of let binding *) "  
            let req_res = !" ^ submit_test elem_num ^ foc_argscall_of_variables lvar ^ "(requirement) in
              match @SND(req_res) with
              | " ^ fst verdict_precond_ok ^ "(test_pass, nj_t) -> 
                 (#print_string(\"*\");
                  #flush_stdout(@VUNIT);
                  !" ^ test_elem_i ^ "( @PRED(n),
                                        @FST(req_res),
                                        @AND(test_pass, b),
                                        @CONS(nj_t, j_t),
                                        @SUCC(nb_try), 0, deb)
                 )
              | " ^ fst verdict_precond_ko ^ " -> 
                  if  @INT_GEQ(nb_consec_fail, 10000000) then
                    (" ^ top_xml_coll_name ^ "!xml_timeout(@VUNIT);
                     let fin = #get_time(@VUNIT) in
                     " ^ top_xml_coll_name ^ "!xml_close_elementaire(j_t,
                                                                     " ^ nb_conclu ^ ",
                                                                     nb_try,
                                                                     compute_time(@CRP(deb,fin)),
                                                                     \"random\"); 
                     @CRP(@CRP(@FALSE, @CRP(j_t, nb_try)), fin)
                    )
                  else
                  !" ^ test_elem_i ^ "(n,requirement, b,j_t, @SUCC(nb_try), @SUCC(nb_consec_fail), deb)"
    ));;

(** ast_call_test_elem i
calculate the conjunction of the i-first-elementary-form's report *)
let rec ast_call_test_elems i args =
  let rec generate_call i =
    if i = 0 then
      [expr_meth focself (rapport_test 0) args]
    else
      expr_meth focself (rapport_test i) args
      ::
      generate_call (i - 1) in
  conjonction_call (List.rev (generate_call i));;

let ast_rapport_test_elem (s_u_t : Own_expr.species_name) (selem : elementaire) vs name i =
    let forme_elem =
      expr_app (expr_glob foccrp)
            [expr_of_caml_list (variables_map_esc
            (fun e -> expr_app (expr_glob foccrp) [MString (get_variable_name e);MString (xml_string_of_typ (get_variable_type e))])
            vs);
      expr_app (expr_glob foccrp)
              [expr_of_caml_list (List.map (fun e -> MString (xml_string_of_expr e)) (get_precond selem));
               expr_of_caml_list (List.map (fun e -> MString (xml_string_of_expr e)) (get_conclusion selem))
              ]
            ] in
    let do_mcdc, mcdc_n = get_mcdc_number () != 0, get_mcdc_number () in
    let mcdc_size = List.length (list_of_precond (get_precond selem)) in
    Unique (
    meth_create (rapport_test i) (TAtom(Some "basics", foctbool))
                (expr_fun "n" (TAtom(Some "basics", foctint))
                  (expr_fun "sicstus" (TAtom(Some "basics", foctbool))
                   (expr_seq
                     (expr_meth top_xml_coll_name "xml_print_elementaire" [forme_elem])
                     (expr_if (expr_var "sicstus")
                              (parse_foc_expr
                               ("
                                let res_d_f = !" ^ test_from_prolog i ^ "(@VUNIT) in
                                (#put_time(\"s_" ^ snd s_u_t ^ "\", \"" ^ get_prolog_opt () ^ "\",
                                          @FST(@SND(res_d_f)),
                                          @SND(@SND(res_d_f)),
                                          \"" ^ name ^ "\", @CRP(@SND(@FST(res_d_f)), 0));
                                 @FST(@FST(res_d_f))
                                )")
                              )
                              (expr_let_notyp "deb"  (expr_basic (prefix "get_time") [expr_basic focunit []])
                              (expr_let_notyp "mcdc" (if do_mcdc then expr_basic
                              (prefix "list_n_mcdc") [expr_int mcdc_n; expr_int mcdc_size] else expr_glob focnil)
                              (expr_let_notyp "res_fin" (expr_meth "Self" (test_elem i)
                                                [expr_var "n"; expr_var "mcdc"; expr_glob foctrue; expr_glob
                                                focnil; expr_int 0; expr_int 0; expr_var "deb"])
                              (expr_let_notyp "fin" (expr_basic focsnd [expr_var "res_fin"])
                              (expr_let_notyp "res" (expr_basic focfst [expr_var "res_fin"])
                              (expr_seq (expr_basic (prefix "put_time")
                                                    [expr_string ("s_" ^ (snd s_u_t)); 
                                                     expr_string "random"; 
                                                     expr_var "deb";
                                                     expr_var "fin";
                                                     expr_string name;
                                                     expr_basic focsnd [expr_var "res"]
                                                    ])
                              (expr_basic focfst [expr_var "res"]) (use_seq_function ())
                              )
                              )))))) (use_seq_function ())
                     
                   )
                  )
                )
                false);;

(*
let rapport_test i = fun n in (@int) ->
    fun sicstus in (@BOOL) ->
      top_xml_coll_name!xml_print_elementaire(forme_elem);
      if sicstus then
        Self!test_from_prolog i(#Null)
      else
        let avant = #get_time(#Null) in
        let mcdc = if do_mcdc then #list_n_mcdc(mcdc_n) else #Nil in
        let res = Self!test_elem i(n, mcdc, #True, #Null, 0, 0) in
        #put_time(avant, #get_time(#Null), name, res);
        #fst(res)
*)


(* Create the entry point of the species, The method is named "test_prop" and
call the methods testing all elementary property, with some output message.  If
the original property contains no variables, create an unique test case. *)
let ast_meth_test_all_elem p_name p_forme nb_var nb_elems =
  let nb_test_case = if nb_var = 0 then expr_int 1 else expr_var "n" in
  let scnd = expr_basic (if get_use_prolog () then foctrue else focfalse) [] in
   Unique (
    meth_create "test_prop"
                (TAtom(Some "basics", foctbool))
                (expr_fun "n" (TAtom(Some "basics", foctint))
                   (expr_seq
                   (expr_meth top_xml_coll_name "xml_print_property"
                                           [expr_string p_name;
                                            expr_string (Print_xml.safe_replace p_forme)]
                   )
                   (expr_seq
                     (expr_app
                         (expr_glob (prefix "print_string"))
                         [MString ("Testing " ^ p_name ^ "...\\n")])
                   (expr_seq
                      (expr_app
                         (expr_glob (prefix "flush_stdout"))
                         [expr_glob focunit])
                   (   expr_let_notyp "res"
                         (ast_call_test_elems
                            (nb_elems - 1)
                            [nb_test_case;scnd])
                   (expr_seq
                         (expr_meth top_xml_coll_name
                                    "xml_close_property"
                                    [expr_glob focunit] )
                         (expr_var "res" )
                         (use_seq_function ())
                   ))
                   (use_seq_function ()))
                   (use_seq_function ()))
                   (use_seq_function ()))
                   )
                false
   );;
(*****)


(* Create the methods testing an elementary forms.
It takes the elementary forms, the number of the elementary forms, the actual
variables on which the elementary form hold, the same variables renamed (to
avoid variable's capture) and the association list (typ, function printing this
type). *)
let ast_test_elem s_u_t name (prolog_pgm,elem) number actual_vars renamed_vars =
    (* methods testing the elem_numth elementary property (elem) *)
  [
   ast_test_pre_cond (map_elementaire_ren prefix_var actual_vars elem) renamed_vars number;
   ast_test_conclu (map_elementaire_ren prefix_var actual_vars elem) renamed_vars number;
   ast_variables_to_list_string actual_vars number;
   ast_test_from_prolog number name prolog_pgm;
   ast_test_submit elem actual_vars number;
   ast_test_elem_i elem renamed_vars number;
   ast_rapport_test_elem s_u_t elem actual_vars name number
       ];;

let ast_species_test (name, actual_vars, cel) property_statement
                     (species_under_test : species_name)
                     species_test
                     params =
(*
  match get_test_context () with
  | None -> 
*)
      (* renames variable (prefix by a "_") to avoid some capture of variable *)
      let rename_vars = variables_map (variable_ren prefix_var) actual_vars in 
      (* If there is no variables, one test case is sufficient *)
      let test_all_elem = ast_meth_test_all_elem name
                                                 property_statement
                                                 (variables_nb actual_vars)
                                                 (List.length cel) in
      let test_elems = snd (List.fold_left (fun (i,seed) e ->
                                              i + 1,
                                              (ast_test_elem species_under_test
                                                             name
                                                             e
                                                             i
                                                             actual_vars
                                                             rename_vars)
                                                             @ seed
                                           )
                                           (0,[test_all_elem])
                                           cel
                           ) in
    spec_create (snd (Species_harness.spec_harness_harness species_test))
                params
                [Species_harness.spec_harness species_under_test, List.map get_name_prm params]
                None
                (ast_parse_one_res_prolog property_statement rename_vars :: test_elems)

(*
  | Some tc ->
      let _file, _clause_elem = To_prolog.create_prolog_file tc nfs in
      failwith "rter";;
*)

(*
(* create the species generating a randoming rep *)
let ast_species_random l_param rep =
  spec_create "random"
              []
              [(Whattodo.species None).specname]
              (Some rep)
              (ast_random l_param rep);;
 *)
(* Create the collection test *)
let ast_collection_test species_test _collection_test coll_implements =
  coll_create (Species_harness.coll_harness_harness species_test)
              (Species_harness.spec_harness_harness species_test,coll_implements);;




(* Take a prop and a species name species_test and return the species and a
 * collection testing the property in species_test.
 * The new species inherits from species_test.
 * We assume here that species_test contains a method named random generating a
 * value of type rep and doesn't expect any parameters.
 *)

let ast_testprop (n, vs, ce_l)
                 prop_stat
                 ((species : Own_expr.species_name), params, coll_implements) =
   (* normalize the property to a list of elementary property *)
   let species_test = spec_test_name species in
   (* and create the species *)
   let species_teste_prop = ast_species_test (n,vs,ce_l) prop_stat
                                             species
                                             species_test
                                             params in
   let collection_test = Species_harness.coll_harness_harness species_test in
     collection_test,
     [ObjSpecies species_teste_prop;
      ObjCollection (ast_collection_test species_test
                                         collection_test
                                         coll_implements)
     ];;

(* ************************************************************************** *)
(* ************************************************************************** *)
(* ************************************************************************** *)

(* generate random value of variables *)
let rec ast_bind_var lvar =
  match lvar with
  | [] -> ""
  | (e,_t)::[] ->
      "let " ^ e ^ " = h in "
  | (e,_t)::(e',_t')::[] -> 
      "let " ^ e ^ "= @FST(h) in " ^
      "let " ^ e' ^ "= @SND(h) in "
  | (e,_t)::r -> 
      "let " ^ e ^ "= @FST(h) in
       let h = @SND(h) in " ^ 
        ast_bind_var r ;;

(** ast_call_test_elem i
calculate the conjunction of the i-first-elementary-form's report *)
let rec ast_call_test_elems_reinject i args =
  let rec generate_call i =
    if i = 0 then
      [expr_meth focself (rapport_test_reinject 0) args]
    else
      expr_meth focself (rapport_test_reinject i) args
      ::
      generate_call (i - 1) in
  conjonction_call (List.rev (generate_call i));;

(* Create the function testing an elementary from a list of value *)
let ast_test_elem_i_reinject =
  let get_print_var t = print_meth (string_of_ttyp t) in
  let rec print_vars l =
    match l with
      [] -> "@NIL"
    | (e,t)::r -> "@CONS(" ^ get_print_var t ^ "(" ^ e ^ "), " ^ print_vars r ^ ")"  in
  fun x lvar elem_num ->
    Unique (
    parse_foc_meth
    (" let rec " ^ test_elem_reinject elem_num ^ " in @BOOL =
        fun l in ( @LIST(" ^ string_of_typ (variables_to_tuple_type lvar)^ ")) ->
        fun b in ( @BOOL ) -> fun j_t in (@LIST(@LIST(@RESULT))) ->
        fun nb_try in ( @INT ) ->
        match l with
        | @NIL ->
           (let saute_ligne = #print_string(\"\\n\") in
             " ^ top_xml_coll_name ^ "!xml_close_elementaire(j_t, " ^
                                                             string_of_int (List.length (list_of_conclusion (get_conclusion x)))  ^ ",
                                                             nb_try,
                                                             0,
                                                             \"replay\");
             @CRP(b, nb_try)
           )
        | @CONS(h,tail) ->
          " ^ ast_bind_var (variables_to_list lvar ) ^ " 
            let result = !" ^ submit_test elem_num ^ foc_argscall_of_variables lvar ^ "(@NIL) in
            let result = #get_verdict(result) in
            let value = @SND(result) in 
              match @FST(result) with
              | @TRUE -> 
                  let rien = #print_string(\"*\") in
                  let rien = #flush_stdout(@VUNIT) in
                  !" ^ test_elem_reinject elem_num ^ "
                          (tail,
                           @AND(#result_ok(@FST(value)), b),
                           @CONS(@SND(value),j_t), @SUCC(nb_try)
                          )
              | @FALSE ->
                  if @INT_GT(nb_try,10000000) then
                    (" ^ top_xml_coll_name ^ "!xml_timeout(@VUNIT);
          " ^ top_xml_coll_name ^ "!xml_close_elementaire(j_t, " ^
                                                          string_of_int (List.length (list_of_conclusion (get_conclusion x)))  ^ ",
                                                          nb_try,
                                                          0, \"replay\"); 
                     @CRP(@FALSE, nb_try)
                  )
                  else
                  !" ^ test_elem_reinject elem_num ^ "(tail,b,j_t, @SUCC(nb_try))"
    ));;

let ast_rapport_test_elem_reinject (selem : elementaire) (tcs : Own_xml.test_case list) vs i =
    let forme_elem =
      expr_app (expr_glob foccrp)
            [expr_of_caml_list (variables_map_esc
            (fun e -> expr_app (expr_glob foccrp) [MString (get_variable_name e);MString (xml_string_of_typ (get_variable_type e))])
            vs);
      expr_app (expr_glob foccrp)
              [expr_of_caml_list (List.map (fun e -> MString (xml_string_of_expr e)) (get_precond selem));
               expr_of_caml_list (List.map (fun e -> MString (xml_string_of_expr e)) (get_conclusion selem))
              ]
            ] in
    let rec my_tuples tcs vs =
     match tcs, vs with
     | [], [] -> expr_basic focunit []
     | tc::[], (_,t)::[] ->
         let m = reinject_value_meth (string_of_ttyp t) in
         expr_meth focself m [tc]
     | tc::r1, (_,t)::r2 ->
          expr_basic foccrp
                     [(let m = reinject_value_meth
                                 (string_of_ttyp t) in
                       expr_meth focself m [tc])
                     ; my_tuples r1 r2
                     ]
     | _ -> failwith ("reinject report : bad number of values") in
    let vs = variables_to_list vs in
    let jeux_de_test = 
      expr_of_caml_list (List.map (fun e -> my_tuples e vs) tcs) in
    Unique (
    meth_create (rapport_test_reinject i) (TAtom(Some "basics", foctbool))
                (expr_fun "n" (TAtom(Some "basics", foctint))
                  (expr_seq
                    (expr_meth top_xml_coll_name "xml_print_elementaire" [forme_elem])
                        (expr_meth "Self"
                        (test_elem_reinject i)
                        [jeux_de_test;expr_glob foctrue; expr_glob
                        focnil; expr_int 0]) (use_seq_function ()))
                   )
                false);;


(* Create the methods testing an elementary forms.
It takes the elementary forms, the number of the ef, the actual variables on
which the elementary form hold, the same variables renamed (to avoid variable's
capture) and the association list (typ, function printing this type). *)
let ast_test_elem_reinject _name ((_, elem), tcs) number actual_vars renamed_vars =
    (* methods testing the elem_numth elementary property (elem) *)
    [ast_test_pre_cond             (map_elementaire_ren prefix_var actual_vars elem) renamed_vars number;
     ast_test_conclu               (map_elementaire_ren prefix_var actual_vars elem) renamed_vars number;
     ast_variables_to_list_string   actual_vars number;
     ast_test_submit                elem actual_vars number;
     ast_test_elem_i_reinject         elem renamed_vars number;
     ast_rapport_test_elem_reinject elem tcs actual_vars number;
    ];;

(* Create the entry point of the species, The method is named "test_prop" and
call the methods testing all elementary property, with some output message.  If
the original property contains no variables, create an unique test case. *)
let ast_meth_test_all_elem_reinject p_name p_forme nb_var nb_elems =
  let nb_test_case = if nb_var = 0 then expr_int 1 else expr_var "n" in
   Unique (
    meth_create "test_prop"
                (TAtom(Some "basics", foctbool))
                (expr_fun "n" (TAtom(Some "basics", foctint))
                   (expr_seq
                   (expr_meth top_xml_coll_name "xml_print_property"
                                           [expr_string p_name ;
                                            expr_string p_forme]
                   )
                   (expr_seq
                     (expr_app
                         (expr_glob (prefix "print_string"))
                         [MString ("Testing " ^ p_name ^ "...\\n")])
                   (expr_seq
                      (expr_app
                         (expr_glob (prefix "flush_stdout"))
                         [expr_glob focunit])
                   (   expr_let_notyp "res"
                         (ast_call_test_elems_reinject
                            (nb_elems - 1)
                            [nb_test_case])
                   (expr_seq
                         (expr_meth top_xml_coll_name
                                    "xml_close_property"
                                    [expr_glob focunit] )
                         (expr_var "res" )
                         (use_seq_function ())
                   ))
                   (use_seq_function ()))
                   (use_seq_function ()))
                   (use_seq_function ()))
                   )
                false);;
(*****)



let ast_species_test_reinject (name, actual_vars, cel) property_statement
                     species_under_test
                     species_test
                     params =
      (* renames variable (prefix by a "_") to avoid some capture of variable *)
      let rename_vars = variables_map (variable_ren prefix_var) actual_vars in 
      (* If there is no variables, one test case is sufficient *)
      let test_all_elem = ast_meth_test_all_elem_reinject name
                                                 (property_statement)
                                                 (variables_nb actual_vars)
                                                 (List.length cel) in
      let test_elems = snd (List.fold_left (fun (i,seed) e ->
                                              i + 1,
                                              (ast_test_elem_reinject
                                                             name
                                                             e
                                                             i
                                                             actual_vars
                                                             rename_vars)
                                                             @ seed
                                           )
                                           (0,[test_all_elem])
                                           cel
                           ) in
    spec_create (snd (Species_harness.spec_harness_harness species_test))
                params [Species_harness.spec_harness species_under_test,List.map get_name_prm params]
                None
                (ast_parse_one_res_prolog property_statement rename_vars :: test_elems)



let ast_testprop_reinject (p : Own_xml.report_property)
                          (species,params,coll_implements) =
   let (n,prop_stat, elem_l) = p in
(*    let liste_elem = List.map (fun ((_,e),_) -> e) elem_l in *)
   let vs = try  fst (fst (List.hd elem_l)) with | Not_found -> failwith "Internal_error :~" in (* TODO: Raise an exception ourself *)
(*    let ce_l = List.map (fun () -> ()) [] in *)
   let species_test = spec_test_name species in
   let species_teste_prop = ast_species_test_reinject
                                             (n,vs,elem_l) prop_stat
                                             species
                                             species_test
                                             params in
   let collection_test = Species_harness.coll_harness_harness species_test in
     collection_test,
     [ObjSpecies species_teste_prop;
      ObjCollection (ast_collection_test species_test
                                         collection_test
                                         coll_implements)
     ];;

let ast_testprop_list nfs_l tc harness =
  let rec aux l c s =
    match l with
    | [] -> c,s
    | (nfs, stat)::r ->
        let name = get_norm_name nfs in
        let vars = get_norm_variables nfs in
        let elems = get_norm_elems nfs in
        let _prolog_file, clause_elem_list = To_prolog.create_prolog_file
                                              tc
                                              name 
                                              vars 
                                              elems in
        let collname,specname = ast_testprop (name, vars, clause_elem_list)
                                             stat
                                             harness in
(*         print_string "Propriété suivante ...\n"; *)
        aux r (collname::c) (specname@s) in
  let all = aux nfs_l [] [] in
(*   print_string "Fin de test toute propriété\n"; *)
  all;;


let ast_testprop_reinject_list report_l harness =
  let rec aux l c s =
    match l with
    | [] -> c,s
    | prop::r ->
        let collname,specname = ast_testprop_reinject prop harness in
        aux r (collname::c) (specname@s) in
  aux report_l [] [];;



