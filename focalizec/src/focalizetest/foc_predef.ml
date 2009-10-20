open Parser;;
open Lexer;;

open Own_basics;;
open Useful_parsing;;
open Own_expr;;
open Own_types;;
open Print_xml;;
open Whattodo;;
open Whattodo2;;

open Focalize_inter;;

(* Fichier ou vont se trouver les predefinitions du fichier fcl *)

let top_coverage_spec_name = "Spec_coverage"
let top_coverage_coll_name = "Coll_coverage"

let top_coverage_meths =  fun () ->
  List.map parse_foc_meth
["let rec all_nok in @BOOL =
  fun l in (@LIST(@RESULT)) ->
    match l with
    | @NIL -> @TRUE
    | @CONS(e,r) -> if #result_ok(e) then
                       @FALSE
                    else
                       !all_nok(r)";
"let rec num_coverage in @INT =
    fun cumul in (@INT) ->
  fun x in (@LIST(@RESULT)) ->
      match x with
      | @NIL -> 0
      | @CONS(e,r) -> if #result_ok(e) then
                        if !all_nok(r) then
                          cumul
                        else
                          0
                      else
                        !num_coverage(@ADD_INT(cumul,1),r)";
"let coverage_conclu in @INT =
   fun l_test in (@LIST(@LIST(@RESULT))) ->
     let nb_concluindep = #list_map(!num_coverage(1), l_test) in
     let nb_indep = #list_fold_left
                      (fun seed in (@LIST(@INT)) ->
                       fun e in (@INT) ->
                         if @OR(#list_is_in(e,seed),@INT_EQUAL(e,0)) then
                           seed
                         else
                           @CONS(e,seed), @NIL, nb_concluindep) in
       #list_length(nb_indep)"
];;



(** It's the list of functions used by the harness for the submit, verdict calculus and
printed out report.  *)
let top_preambule =
  (List.map parse_foc_topexpr
  ["let get_time in @UNIT -> @FLOAT = caml import get_time";
   "let put_time in @FLOAT -> @FLOAT -> @STRING -> @BOOL -> @UNIT = caml import put_time";
   "let rand_int in @INT -> @INT = caml import rand_int";
   "let rand_int_good in @INT -> @INT = caml import rand_int_good";
   "let rand_bool in @INT -> @BOOL = caml import rand_bool";
   "let rand_float in @INT -> @FLOAT = caml import rand_float";
   "let split_cons in @STRING -> @STRING * @LIST(@STRING) = caml import split_cons";
   "type RESULT =
    " ^ fst result_ok ^ ";
    " ^ fst result_ko ^ ";
    " ^ fst result_raise ^ "(@STRING);";
   "type VERDICT =
    " ^ fst verdict_precond_ok ^ "(@RESULT, @LIST(@RESULT));
    " ^ fst verdict_precond_ko ^ "(@RESULT, @LIST(@RESULT));";
   "let get_verdict(x in @VERDICT) in @VERDICT -> @BOOL * (@RESULT * @LIST(@RESULT)) =
      match x with
      | " ^ fst verdict_precond_ok ^ "(r,s) -> @CRP(#True, @CRP(r,s))
      | " ^ fst verdict_precond_ko ^ "(r,s) -> @CRP(#False, @CRP(r,s))";
   "let seq in 'a -> 'b -> 'a = caml import seq";
   "let flush_stdout in @UNIT -> @UNIT = caml import flush_stdout";
   "let print_close_file in (@STRING -> @UNIT) * (@UNIT -> @UNIT) =
      caml import print_close_file";
   "let get_all_lines in (@STRING -> @LIST(@STRING)) = caml import get_all_line";
   "let print_file in @STRING -> @UNIT = @FST(#print_close_file)";
   "let close_file in @UNIT -> @UNIT = @SND(#print_close_file)";
   "let catch_evaluation in (@UNIT -> @BOOL) -> @BOOL * (@BOOL * @STRING) =
      caml import catch_raise";
   "let get_result in (@UNIT -> @BOOL) -> @RESULT =
      fun f in (@UNIT -> @BOOL) ->
        let eval = #catch_evaluation(f) in
        let catch = @FST(eval) in 
        let res = @SND(eval) in
        if catch then
          #" ^ fst result_raise ^ "(@SND(res))
        else if @FST(res) then
          #" ^ fst result_ok ^ "
        else
          #" ^ fst result_ko;
   "let result_ok in @RESULT -> @BOOL = fun r in (@RESULT) -> 
     @STRUCT_EQUAL(r, #" ^ fst result_ok ^ ")";
   "let list_map in ('a -> 'b) -> @LIST('a) -> @LIST('b) = caml import list_map";
   "let list_iter in ('a -> @UNIT) -> @LIST('a) -> @UNIT = caml import list_iter";
   "let list_length in @LIST('a) -> @INT = caml import list_length";
   "let list_fold_left in ('a -> 'b -> 'a) -> 'a -> @LIST('b) -> 'a = caml import list_fold_left";
   "let list_is_in in 'a -> @LIST('a) -> @BOOL =
       fun b in ('a) ->
         fun l in (@LIST('a)) ->
           #list_fold_left(fun seed in (@BOOL) -> fun e in ('a) ->
             @OR(@STRUCT_EQUAL(e,b),seed),@FALSE,l)";
   "let eval_list_expr_or = fun l in (@LIST(@UNIT -> @BOOL)) ->
       let l_verdict = #list_map(#get_result, l) in
       let verdict = #list_fold_left(fun s in (@RESULT) -> fun e in (@RESULT) ->
                  if @OR(#result_ok(s),#result_ok(e)) then #" ^ fst result_ok
                  ^ " else #" ^ fst result_ko ^ ",
                   #" ^ fst result_ko ^ ", l_verdict) in
          @CRP(verdict, l_verdict)";
   "let eval_list_expr_and = fun l in (@LIST(@UNIT -> @BOOL)) ->
       let l_verdict = #list_map(#get_result, l) in
       let verdict = #list_fold_left(fun s in (@RESULT) -> fun e in (@RESULT) ->
                  if @AND(#result_ok(s),#result_ok(e)) then #" ^ fst result_ok
                  ^ " else #" ^ fst result_ko ^ ",
                   #" ^ fst result_ok ^ ", l_verdict) in
            @CRP(verdict, l_verdict)";
   "let call_prolog in @STRING -> @STRING -> unit = caml import call_prolog";
   "let call_prolog2 in @STRING -> @STRING -> @STRING -> @STRING -> @STRING -> unit = caml import call_prolog2"
  ]) @ 
   (* The species which calculate the coverage *)
  [create_toplevel_spec(
    spec_create top_coverage_spec_name [] [] (Some (TAtom(None, foctunit))) (top_coverage_meths ()) 
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
        "if #value then
           #print_string(\"-> All test cases have successfully passed\\n\")
         else
           #print_string(\"-> At least one test case has failed\\n\")"];;

let top_postambule_ts coll_list =
  match coll_list with
  | [] -> []
  | e::r -> 
      parse_foc_topexpr ("let random in @UNIT = caml import init_rand")::
      ObjToplet("value",Some (TAtom(None, foctbool)),call_test_prop_from_testset e r) ::
      [parse_foc_topexpr
        "if #value then
           #print_string(\"-> All test cases have successfully passed\\n\")
         else
           #print_string(\"-> At least one test case has failed\\n\")"];;
(** [top_postambule_ts coll_ts_list] takes a list of collection name, test set.
Assuming these collections exist, it returns the list of expressions calling the
harness implemented within the collections testing with the corresponding test
set. *)




(** [top_import xml] takes a xml file name [xml] and return the default import
 section concerning this xml file. *)
let top_import xml : import =
  "toplevel",
  ["init_rand","Random.init (int_of_float (Unix.time ()))";
   "rand_int","fun n -> (Random.int 21) - 10";
   "rand_int_good","fun n -> Random.int n";
   "rand_float","fun n -> Random.float 10.0";
   "rand_unit","fun n -> ()";
   "rand_string","fun n -> \"string\"";
   "rand_char","fun n -> char_of_int (Random.int 256)";
   "rand_bool","fun n -> (Random.int 2) = 0";
   "rand_big_int",
                "fun n ->
                   let zero = Char.code '0' in
                   let rec entier_string s n =
                     if n <= 0 then
                       s
                     else
                       entier_string (s ^ Char.escaped (Char.chr (zero + Random.int 10))) (n-1) in
                   let generate_chaine = entier_string \"0\" in
               big_int_of_string (generate_chaine n)";
   "seq"  ,"fun a b -> (b;a)";
   "list_map" ,"fun f l -> List.map f l";
   "list_iter" ,"fun f l -> List.iter f l";
   "list_length" ,"fun l -> List.length l";
   "list_fold_left" ,"fun f s l -> List.fold_left f s l";
   "print_close_file",
   "let first = ref true in
    let file = ref stdout in
    (fun s ->
       if !first then
         (first := false;
          file := open_out \"" ^ xml ^ "\"
         );
      output_string !file s;
    ), fun () -> close_out !file";
   "get_all_line",
   "fun s ->
    let file = open_in s in
    let rec get_all () =
      try
        let l = input_line file in
        l :: get_all ()
      with
      | End_of_file -> [] in
        get_all ()";
   "catch_raise","fun f -> try false,(f (),\"\") with | " ^ focexception ^ " s -> true,(false,s)";
   "flush_stdout", "fun () -> Pervasives.flush stdout";
   "call_prolog", "fun n -> fun s -> 
                      Unix.putenv \"GLOBALSTKSIZE\" \"" ^ string_of_int (Whattodo.get_globalstk ()) ^ "M\";
                      Unix.putenv \"LOCALSTKSIZE\" \"" ^ string_of_int (Whattodo.get_localstk ()) ^ "M\";
                      Unix.putenv \"CHOICESTKSIZE\" \"" ^ string_of_int (Whattodo.get_choicestk ()) ^ "M\";
                      Unix.putenv \"TRAILSTKSIZE\" \"" ^ string_of_int (Whattodo.get_trailstk ()) ^ "M\";
                      Unix.putenv \"PROLOGMAXSIZE\" \"" ^ string_of_int (Whattodo.get_prologmax ()) ^"M\";
                      match Unix.system (\"sicstus -l \" ^ n ^ \" --goal \" ^ s
                      ^ \".\") with
                      | Unix.WEXITED i -> print_string (\"sicstus returns with code : \" ^
                                                  (string_of_int i) ^ \"\n\")
                      | Unix.WEXITED i -> print_string (\"sicstus was killed by signal : \" ^
                                                  (string_of_int i) ^ \"\n\")
                      | Unix.WSTOPPED i -> print_string (\"sicstus was stopped by signal : \" ^
                                                  (string_of_int i) ^ \"\n\")";
   "call_prolog2", "fun n -> fun s -> fun s2 -> fun yy -> fun t ->
                      Unix.putenv \"GLOBALSTKSIZE\" \"" ^ string_of_int (Whattodo.get_globalstk ()) ^ "M\";
                      Unix.putenv \"LOCALSTKSIZE\" \"" ^ string_of_int (Whattodo.get_localstk ()) ^ "M\";
                      Unix.putenv \"CHOICESTKSIZE\" \"" ^ string_of_int (Whattodo.get_choicestk ()) ^ "M\";
                      Unix.putenv \"TRAILSTKSIZE\" \"" ^ string_of_int (Whattodo.get_trailstk ()) ^ "M\";
                      Unix.putenv \"PROLOGMAXSIZE\" \"" ^ string_of_int (Whattodo.get_prologmax ()) ^"M\";
                      Unix.system (\"sicstus -l \" ^ n ^ \" --goal \" ^ s ^ \".\");
                      let d = (Unix.times ()).Unix.tms_cutime in
 print_float d;
 print_newline ();
                      let truc = Unix.system (\"sicstus -r \" ^ s2) in
                      let f = (Unix.times ()).Unix.tms_cutime in
 print_float f;
 print_newline ();
                      let fic = open_out_gen [Open_append; Open_creat] 0o600 \"stats\" in
                      output_string fic (\"sicstus(\" ^ yy ^ \", \" ^ 
                                             string_of_int (int_of_float ((f -.  d) *. 1000.0)) ^ \", \" ^
                                             t ^ \").\n\");
                      close_out fic";
   "get_time", "fun () -> (Unix.times ()).Unix.tms_utime";
   "put_time", "fun d f s b ->
     let fic = open_out_gen [Open_append; Open_creat] 0o600 \"stats\" in
                 let timeout = if b then \"timeout\" else \"ok\" in
                 output_string fic (\"random(\" ^ s ^ \", \" ^ 
                                             string_of_int (int_of_float ((f -.  d) *. 1000.0)) ^ \", \" ^
                                             timeout ^ \").\n\");
                 close_out fic";
   "split_cons",
   "fun s ->
     let rec aux cumul anc pos prof =
       if s.[pos] = ','  && prof = 1 then
         aux ((String.sub s anc (pos - anc))::cumul)
             (pos + 1) (pos + 1) prof
       else if s.[pos] = '(' then
           aux cumul anc (pos+1) (prof + 1)
       else if s.[pos] = ')' then
           let prof = prof - 1 in
             if prof <= 0 then
               List.rev (String.sub s anc (pos -anc)::cumul)
             else
               aux cumul anc (pos + 1) prof
         else
           aux cumul anc (pos + 1) prof in
     let rec get_word pos =
       if pos >= String.length s then
         String.sub s 0 pos, []
       else
         if s.[pos] = '(' then
           String.sub s 0 pos, aux [] (pos+1) (pos+1) 1
         else
           get_word (pos + 1) in
     get_word 0"
  ];;

