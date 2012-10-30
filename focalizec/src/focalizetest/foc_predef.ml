open Own_basics ;;
open Useful_parsing ;;
open Own_expr ;;
open Own_types ;;
open Print_xml ;;
open Whattodo ;;
open Whattodo2 ;;


(* Fichier ou vont se trouver les predefinitions du fichier fcl *)

let top_coverage_spec_name = "Spec_coverage" ;;
let top_coverage_coll_name = "Coll_coverage" ;;

let top_coverage_meths =  fun () ->
  List.map parse_foc_meth
["let rec all_nok in @BOOL =\n\
  fun l in (@LIST(@RESULT)) ->\n\
    match l with\n\
    | @NIL -> @TRUE\n\
    | @CONS(e,r) -> if #result_ok(e) then\n\
                       @FALSE\n\
                    else\n\
                       !all_nok(r)";
"let rec num_coverage in @INT =\n\
    fun cumul in (@INT) ->\n\
  fun x in (@LIST(@RESULT)) ->\n\
      match x with\n\
      | @NIL -> 0\n\
      | @CONS(e,r) -> if #result_ok(e) then\n\
                        if !all_nok(r) then\n\
                          cumul\n\
                        else\n\
                          0\n\
                      else\n\
                        !num_coverage(@ADD_INT(cumul,1),r)";
"let coverage_conclu in @INT =\n\
   fun l_test in (@LIST(@LIST(@RESULT))) ->\n\
     let nb_concluindep = #list_map(!num_coverage(1), l_test) in\n\
     let nb_indep = #list_fold_left\n\
                      (fun seed in (@LIST(@INT)) ->\n\
                       fun e in (@INT) ->\n\
                         if @OR(#list_is_in(e,seed),@INT_EQUAL(e,0)) then\n\
                           seed\n\
                         else\n\
                           @CONS(e,seed), @NIL, nb_concluindep) in\n\
       #list_length(nb_indep)"
];;



(** It's the list of functions used by the harness for the submit, verdict calculus and
printed out report.  *)
let top_preambule =
  (List.map parse_foc_topexpr
  ["let get_time in @UNIT -> @FLOAT = caml import get_time";
   "let get_tag in @UNIT -> @STRING = caml import get_tag";
   "let rand_int in @INT -> @INT = caml import rand_int";
   "let rand_int_good in @INT -> @INT = caml import rand_int_good";
   "let rand_bool in @INT -> @BOOL = caml import rand_bool";
   "let rand_float in @INT -> @FLOAT = caml import rand_float";
   "let split_cons in @STRING -> @STRING * @LIST(@STRING) = caml import split_cons";
   "type RESULT =\n\
    " ^ fst result_ok ^ ";\n\
    " ^ fst result_ko ^ ";\n\
    " ^ fst result_raise ^ "(@STRING);";
   "let put_time in @STRING -> @STRING -> @FLOAT -> @FLOAT -> @STRING -> (@LIST(@LIST(@RESULT)) * @INT) -> @UNIT = caml import put_time";
   "let compute_time in (@FLOAT * @FLOAT) -> @INT = caml import compute_time";
   "type VERDICT =\n\
    " ^ fst verdict_precond_ok ^ "(@BOOL, @LIST(@RESULT));\n\
    " ^ fst verdict_precond_ko ^ ";";
   "let get_verdict(x in @VERDICT) in @VERDICT -> @BOOL * (@BOOL * @LIST(@RESULT)) =\n\
      match x with\n\
      | " ^ fst verdict_precond_ok ^ "(r, s) -> @CRP(@TRUE, @CRP(r, s))\n\
      | " ^ fst verdict_precond_ko ^ " -> @CRP(@FALSE, @CRP(@FALSE, @NIL))";
   "let seq in 'a -> 'b -> 'a = caml import seq";
   "let flush_stdout in @UNIT -> @UNIT = caml import flush_stdout";
   "let print_close_file in (@STRING -> @UNIT) * (@UNIT -> @UNIT) =\n\
      caml import print_close_file";
   "let get_all_lines in (@STRING -> @LIST(@STRING)) = caml import get_all_line";
   "let print_file in @STRING -> @UNIT = @FST(#print_close_file)";
   "let close_file in @UNIT -> @UNIT = @SND(#print_close_file)";
   "let catch_evaluation in (@UNIT -> @BOOL) -> @BOOL * (@BOOL * @STRING) =\n\
      caml import catch_raise";
   "let get_result in (@UNIT -> @BOOL) -> @RESULT =\n\
      fun f in (@UNIT -> @BOOL) ->\n\
        let eval = #catch_evaluation(f) in\n\
        let catch = @FST(eval) in\n\
        let res = @SND(eval) in\n\
        if catch then\n\
          #" ^ fst result_raise ^ "(@SND(res))\n\
        else if @FST(res) then\n\
          #" ^ fst result_ok ^ "\n\
        else\n\
          #" ^ fst result_ko;
   "let result_ok in @RESULT -> @BOOL = fun r in (@RESULT) ->\n\
     @STRUCT_EQUAL(r, #" ^ fst result_ok ^ ")";
   "let list_map in ('a -> 'b) -> @LIST('a) -> @LIST('b) = caml import list_map";
   "let list_iter in ('a -> @UNIT) -> @LIST('a) -> @UNIT = caml import list_iter";
   "let list_length in @LIST('a) -> @INT = caml import list_length";
   "let list_fold_left in ('a -> 'b -> 'a) -> 'a -> @LIST('b) -> 'a = caml import list_fold_left";
   "let list_is_in in 'a -> @LIST('a) -> @BOOL =\n\
       fun b in ('a) ->\n\
         fun l in (@LIST('a)) ->\n\
           #list_fold_left(fun seed in (@BOOL) -> fun e in ('a) ->\n\
             @OR(@STRUCT_EQUAL(e,b),seed),@FALSE,l)";
   "let eval_list_expr_or = fun l in (@LIST(@UNIT -> @BOOL)) ->\n\
       let l_verdict = #list_map(#get_result, l) in\n\
       let verdict = #list_fold_left(fun s in (@RESULT) -> fun e in (@RESULT) ->\n\
                  if @OR(#result_ok(s),#result_ok(e)) then #" ^ fst result_ok
                  ^ " else #" ^ fst result_ko ^ ",\n\
                   #" ^ fst result_ko ^ ", l_verdict) in\n\
          @CRP(verdict, l_verdict)";
   "let eval_list_expr_and = fun l in (@LIST(@UNIT -> @BOOL)) ->\n\
       let l_verdict = #list_map(#get_result, l) in\n\
       let verdict = #list_fold_left(fun s in (@RESULT) -> fun e in (@RESULT) ->\n\
                  if @AND(#result_ok(s),#result_ok(e)) then #" ^ fst result_ok
                  ^ " else #" ^ fst result_ko ^ ",\n\
                   #" ^ fst result_ok ^ ", l_verdict) in\n\
            @CRP(verdict, l_verdict)";
   "let call_prolog in @STRING -> @STRING -> unit = caml import call_prolog";
   "let call_prolog2 in @STRING -> @STRING -> @STRING -> @STRING -> @STRING -> float * float = caml import call_prolog2";
   "let list_in =\n\
      let rec list_in =\n\
       fun n in (@INT) -> fun l in (@LIST(@INT)) ->\n\
       match l with\n\
       | @NIL -> @FALSE\n\
       | @CONS(e,r) ->\n\
         if @INT_EQUAL(e,n) then\n\
           @TRUE\n\
         else \n\
           list_in(n,r) in\n\
      list_in";
  "let list_append =\n\
       let rec aux =\n\
         fun l1 in (@LIST('a)) -> fun l2 in (@LIST('a)) ->\n\
         match l1 with\n\
         | @NIL -> l2\n\
         | @CONS(e,r) ->\n\
           @CONS(e, aux(r, l2)) in\n\
   aux";
  "let list_del_one = \n\
    let rec list_del_one =\n\
       fun n in (@INT) -> fun l in (@LIST(@INT)) ->\n\
       match l with\n\
       | @NIL -> @NIL\n\
       | @CONS(e,r) ->\n\
         if @INT_EQUAL(e,n) then\n\
           r\n\
         else\n\
           @CONS(e,list_del_one(n, r)) in\n\
      list_del_one";
   "let list_int =\n\
        let rec list_int =\n\
           fun n in (@INT) ->\n\
             if @INT_EQUAL(n, @PRED(0)) then\n\
               @NIL\n\
             else\n\
               @CONS(n, #list_int(@PRED(n))) in\n\
       list_int";
   "let list_one_mcdc =\n\
      fun n in (@INT) ->\n\
       let rec list_mcdc_aux =\n\
          fun m in (@INT) ->\n\
            if @INT_EQUAL(m, 0) then\n\
               @NIL\n\
            else\n\
              list_append(#list_int(n), list_mcdc_aux(@PRED(m))) in\n\
      list_mcdc_aux(n)";
   "let list_n_mcdc =\n\
      fun n in (@INT) -> fun m in (@INT) ->\n\
       let rec list_n_mcdc_aux = fun n in (@INT) ->\n\
         if @INT_LEQ(n, 0) then\n\
           @NIL\n\
         else \n\
           list_append(list_int(m), list_n_mcdc_aux(@PRED(n))) in\n\
      list_n_mcdc_aux(n)";
   "let list_nb_false = fun l in (@LIST(@RESULT)) ->\n\
         list_fold_left(fun s in (@INT) -> fun e in (@RESULT) ->\n\
           if #result_ok(e) then s else @SUCC(s), 0, l)";
   "let list_occur =\n\
      let rec list_nth =\n\
        fun l in (@LIST(@RESULT)) -> fun n in (@INT) ->\n\
        match l with\n\
        | @NIL -> @PRED(0)\n\
        | @CONS(e,r) ->\n\
            if #result_ok(e) then\n\
               list_nth(r, @SUCC(n))\n\
            else\n\
              n in\n\
    fun l in (@LIST(@RESULT)) ->\n\
      let n = list_nb_false(l) in\n\
      if @INT_EQUAL(n, 0) then\n\
        0\n\
      else\n\
        if @INT_EQUAL(n, 1) then\n\
          list_nth(l, 1)\n\
        else\n\
          @PRED(0)"
  ]) @ 
   (* The species which calculate the coverage *)
  [create_toplevel_spec(
    spec_create top_coverage_spec_name [] [] (Some (TAtom(None, foctunit))) (List.map (fun x -> Unique x) (top_coverage_meths ()))
             );
    let m = Whattodo.get_output_module () in 
    create_toplevel_coll(coll_create top_coverage_coll_name
                                     (create_species_name m top_coverage_spec_name,[]))
  ] @
  top_xml_def ()
(*
  @
  (List.map parse_foc_topexpr
  ["let test_from_list =
     fun a_test in (@UNIT) -> fun fun_precond -> fun fun_conclu -> fun fun_to_strings ->
           let rec aux = fun l in (@LIST( )) -> fun b -> fun res -> fun nb ->
             match l with
             | @NIL ->
                 (#print_string(\"\n\");
                  Coll_xml!xml_close_elementaire(res, 1, nb);
                  b
                 )
             | @CONS(cur,r) -> 
                 (let pval = fun_precond(cur) in
                  let vars = fun_to_strings(cur) in
                  let cval = fun_conclu(cur) in
                  Coll_xml!xml_open_test(@VUNIT);
                  Coll_xml!xml_result_test(vars, pval, cval);
                  Coll_xml!xml_close_test(@VUNIT);
                  aux(r,
                      @AND(b, #result_ok(@FST(cval))),
                      @CONS(@SND(cval), res),
                      @SUCC(nb))
                 ) in
               aux(l_test, @TRUE, @NIL, 0)"
  ])
*)
;;


(** Returns an expression of the type string * list(bool * (string * string)) 
    which is the definition of the species under test. *)
let species_test () =
  let expr_of_string_list l =
    expr_of_caml_list (List.map expr_string l) in
  let convert_species_name : Own_expr.species_name -> Own_expr.myexpr =
    expr_of_caml_couple expr_string expr_string in
  let convert_species = (expr_of_caml_couple convert_species_name expr_of_string_list) in
  let convert_collection = expr_of_caml_couple expr_string
                                               convert_species in
  let convert_collections = List.map convert_collection in
  let collections, species =
    match get_test_context () with
    | None -> failwith "We need a test context"
(*
        let spec_name,p_instance = get_species_test () in 
        let p_name = List.map get_name_prmexp (Focalize_inter.get_parameters spec_name) in 
        let coll_xml = List.map (fun e -> is_coll_param e,
                                          xml_string_of_parameters_instance e) p_instance in
        let n_i = try List.combine p_name coll_xml with
                  | Invalid_argument _ ->
                       raise (Bad_parameters_numbers(List.length p_name,
                                                     List.length p_instance)) in
        spec_name, n_i
*)
    | Some tc ->
        let spec = Context_test.tc_get_end_sc tc in
        let spec_name =  Context_test.sc_get_name spec in
        let p_name = Context_test.sc_get_parameters spec in
        let colls = Context_test.tc_get_parameters tc in
        let colls = List.map (fun e -> Context_test.bc_get_name e, 
                             (Context_test.bc_get_name2 e,
                              Context_test.bc_get_parameters e
                             )) colls in
        colls , (spec_name, p_name) in
  expr_of_caml_couple expr_of_caml_list convert_species
                      (convert_collections collections, species)

(** [call_test_prop c l] call the method "test_prop" of the species [c::l]. It
returns the logical and of all returned values. *)
let call_test_prop c l =
  let num_test = string_of_int (get_number_of_test ()) in
  let rec aux e l =
    match l with
    | [] -> parse_foc_expr (e ^ "!test_prop(" ^ num_test ^ ")")
    | e'::r' -> expr_app
                  (expr_glob focand)
                  [parse_foc_expr (e ^ "!test_prop(" ^ num_test ^ ")"); aux e' r'] in
  expr_seq
  (expr_meth top_xml_coll_name "xml_print_header" [expr_glob focunit])
 (expr_seq
 (expr_meth top_xml_coll_name "xml_open_report" [species_test ()])
    (expr_let_notyp "resultat" (aux c l)
 (expr_seq
 (expr_meth top_xml_coll_name "xml_close_report" [expr_glob focunit])
    (expr_var "resultat")
    (use_seq_function ())
 )
 )
    (use_seq_function ())
 )
    (use_seq_function ())
;;

let call_test_prop_from_testset c l =
  let rec aux (n,ts) l =
    match l with
    | [] -> parse_foc_expr (n ^ "!test_prop_ts(" ^ ts ^ ")")
    | e'::r' -> expr_app
                  (expr_glob focand)
                  [parse_foc_expr (n ^ "!test_prop_ts(" ^ ts ^ ")"); aux e' r'] in
  expr_seq
  (expr_meth top_xml_coll_name "xml_print_header" [expr_glob focunit])
 (expr_seq
 (expr_meth top_xml_coll_name "xml_open_report" [species_test ()])
    (expr_let_notyp "resultat" (aux c l)
 (expr_seq
 (expr_meth top_xml_coll_name "xml_close_report" [expr_glob focunit])
    (expr_var "resultat")
    (use_seq_function ())
 )
 )
    (use_seq_function ())
 )
    (use_seq_function ())
;;




(** [top_postambule coll_list] takes a list of collection name. Assuming these
collections exist, it returns the list of expressions calling the harness
implemented within the collections. *)
let top_postambule coll_list =
  match coll_list with
  | [] -> []
  | e::r -> 
      parse_foc_topexpr ("let random in @UNIT = caml import init_rand")::
      ObjToplet("value",Some (TAtom(None, foctbool)),call_test_prop e r) ::
      [parse_foc_topexpr
        "if #value then\n\
           #print_string(\"-> All test cases have successfully passed\\n\")\n\
         else\n\
           #print_string(\"-> At least one test case has failed\\n\")"];;

let top_postambule_ts coll_list =
  match coll_list with
  | [] -> []
  | e::r -> 
      parse_foc_topexpr ("let random in @UNIT = caml import init_rand")::
      ObjToplet("value",Some (TAtom(None, foctbool)),call_test_prop_from_testset e r) ::
      [parse_foc_topexpr
        "if #value then\n\
           #print_string(\"-> All test cases have successfully passed\\n\")\n\
         else\n\
           #print_string(\"-> At least one test case has failed\\n\")"];;
(** [top_postambule_ts coll_ts_list] takes a list of collection name, test set.
Assuming these collections exist, it returns the list of expressions calling the
harness implemented within the collections testing with the corresponding test
set. *)



let setenv_sicstus () =
  let aux s i =
    if i = 0 then
      ""
    else
      "Unix.putenv \"" ^ s ^ "\" \"" ^ string_of_int i ^ "M\";" in
    aux "GLOBALSTKSIZE" (Whattodo.get_globalstk ()) ^
    aux "LOCALSTKSIZE" (Whattodo.get_localstk ())     ^
    aux "CHOICESTKSIZE" (Whattodo.get_choicestk ())   ^
    aux "TRAILSTKSIZE" (Whattodo.get_trailstk ())     ^
    aux "PROLOGMAXSIZE" (Whattodo.get_prologmax ())

(** [top_import xml] takes a xml file name [xml] and return the default import
 section concerning this xml file. *)
let top_import xml : import =
  "toplevel",
  ["init_rand","Random.init (int_of_float (Unix.time ()))";
   (let n = string_of_int (get_int_size ()) in
   "rand_int","fun n -> (Random.int " ^ n ^") - (" ^ n ^ " / 2)");
   "rand_int_good","fun n -> Random.int n";
   "rand_float","fun n -> Random.float 10.0";
   "rand_unit","fun n -> ()";
   "rand_string","fun n -> \"string\"";
   "rand_char","fun n -> char_of_int (Random.int 256)";
   "rand_bool","fun n -> (Random.int 2) = 0";
   "get_tag","fun () -> try Sys.argv.(1) with\n\
                        | _ -> \"\"";
   "rand_big_int",
                "fun n ->\n\
                   let zero = Char.code '0' in\n\
                   let rec entier_string s n =\n\
                     if n <= 0 then\n\
                       s\n\
                     else\n\
                       entier_string (s ^ Char.escaped (Char.chr (zero + Random.int 10))) (n-1) in\n\
                   let generate_chaine = entier_string \"0\" in\n\
               big_int_of_string (generate_chaine n)";
   "seq"  ,"fun a b -> (b;a)";
   "list_map" ,"fun f l -> List.map f l";
   "list_iter" ,"fun f l -> List.iter f l";
   "list_length" ,"fun l -> List.length l";
   "list_fold_left" ,"fun f s l -> List.fold_left f s l";
   "print_close_file",
   "let first = ref true in\n\
    let file = ref stdout in\n\
    (fun s ->\n\
       if !first then\n\
         (first := false;\n\
          file := open_out \"" ^ xml ^ "\"\n\
         );\n\
      output_string !file s;\n\
    ), fun () -> close_out !file";
   "get_all_line",
   "fun s ->\n\
    let file = open_in s in\n\
    let rec get_all () =\n\
      try\n\
        let l = input_line file in\n\
        l :: get_all ()\n\
      with\n\
      | End_of_file -> [] in\n\
        get_all ()";
   "catch_raise","fun f -> try false,(f (),\"\") with | " ^ focexception ^ " s -> true,(false,s)";
   "flush_stdout", "fun () -> Pervasives.flush stdout";
   "call_prolog", "fun n -> fun s -> \n\
                      " ^ setenv_sicstus () ^
                     "match Unix.system (\"sicstus -l \" ^ n ^ \" --goal \" ^ s\n\
                      ^ \".\") with\n\
                      | Unix.WEXITED i -> print_string (\"sicstus returns with code : \" ^\n\
                                                  (string_of_int i) ^ \"\n\")\n\
                      | Unix.WSIGNALED i -> print_string (\"sicstus was killed by signal : \" ^\n\
                                                  (string_of_int i) ^ \"\n\")\n\
                      | Unix.WSTOPPED i -> print_string (\"sicstus was stopped by signal : \" ^\n\
                                                  (string_of_int i) ^ \"\n\")";
   "call_prolog2", "fun n -> fun s -> fun s2 -> fun yy -> fun t -> " ^
                          setenv_sicstus () ^
                     "Unix.system (\"sicstus -l \" ^ n ^ \" --goal \" ^ s ^ \".\");\n\
                      let deb = (Unix.times ()).Unix.tms_cutime in\n\
                      Unix.system (\"sicstus -r \" ^ s2);\n\
                      deb, (Unix.times ()).Unix.tms_cutime";
   "get_time", "fun () -> (Unix.times ()).Unix.tms_utime";
   "compute_time", "fun d_f -> int_of_float ((snd d_f -. fst d_f) *. 1000.0)";
   "put_time", "fun s_u_t meth d f s ok_nb ->\n\
     let fic = open_out_gen [Open_append; Open_creat] 0o600 \"stats\" in\n\
                 let nb_ok = List.length (fst ok_nb) in\n\
                 let nb_gen = snd ok_nb in\n\
                 let meth = meth ^ get_tag () in\n\
                 output_string fic (meth ^ \"(\" ^ s_u_t ^ \", \" ^ s ^ \", \" ^ \n\
                                             string_of_int (int_of_float ((f -.  d) *. 1000.0)) ^ \", \" ^\n\
                                             string_of_int nb_ok ^ \", \" ^\n\
                                             string_of_int nb_gen ^ \").\n\");\n\
                 close_out fic";
   "split_cons",
   "fun s ->\n\
     let rec aux cumul anc pos prof =\n\
       if s.[pos] = ','  && prof = 1 then\n\
         aux ((String.sub s anc (pos - anc))::cumul)\n\
             (pos + 1) (pos + 1) prof\n\
       else if s.[pos] = '(' then\n\
           aux cumul anc (pos+1) (prof + 1)\n\
       else if s.[pos] = ')' then\n\
           let prof = prof - 1 in\n\
             if prof <= 0 then\n\
               List.rev (String.sub s anc (pos -anc)::cumul)\n\
             else\n\
               aux cumul anc (pos + 1) prof\n\
         else\n\
           aux cumul anc (pos + 1) prof in\n\
     let rec get_word pos =\n\
       if pos >= String.length s then\n\
         String.sub s 0 pos, []\n\
       else\n\
         if s.[pos] = '(' then\n\
           String.sub s 0 pos, aux [] (pos+1) (pos+1) 1\n\
         else\n\
           get_word (pos + 1) in\n\
     get_word 0"
  ];;

