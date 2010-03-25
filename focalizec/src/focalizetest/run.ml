open Focalize_inter;;
open Whattodo;;
open Whattodo2;;
open Own_basics;;
open Useful_parsing;;
open Own_prop;;

open Test_prop;;
open Print_foc;;
open Print_fml;;
open Print_xml;;

open Rewrite_prop;;
open Own_expr;;
(* open Instru_species;; *)
open Foc_predef;;


type tested_proposition =
(*   | Prop_list of (string * proposition) list * Own_expr.species_test  *)
  | Test_context of (string * proposition) list * Context_test.test_context
  | Report of Own_xml.report
;;

let report_from_xml_file file =
  try
    let ch_in = open_in file in
    let lb = Lexing.from_channel ch_in in
    set_input_lexbuf (Some lb);
    let report = Own_parser.expr_xml Lexer.tmp_lexe_xml lb in
    set_input_lexbuf  None;
    Own_xml.xml_report_to_report report
  with
  | Sys_error(_) -> prerr_string ("Error: File " ^ file ^ " cannot be open for read\n");
                    exit 1;;

(*
let ty  = report_from_xml_file "test_report.xml";;
let rec print t =
 match t with
 | Own_xml.Node(e,t) -> 
     print_string "<";
     print_string e;
     print_string ">";
     List.iter print t;
     print_string "<";
     print_string "/";
     print_string e;
     print_string ">"
 | Own_xml.Leave(a,e) ->
     print_string "<";
     print_string a;
     print_string ">";
     (match e with
     | None -> ()
     | Some e -> print_string e
     );
     print_string "<";
     print_string "/";
     print_string a;
     print_string ">";;
let r = Own_xml.xml_report_to_report ty in
print_string (Own_xml.string_of_report r);;
exit 1;;
*)

let parse_string fun_parse s =
  let lb = Lexing.from_string s in
  set_input_lexbuf (Some lb);
  fun_parse Lexer.lexe_string lb;;

let parse_file fun_parse fic =
  try
    let ch_in = open_in fic in
    let lb = Lexing.from_channel ch_in in
    set_input_lexbuf None;
    fun_parse Lexer.lexe_file lb
  with
  | Sys_error(_) -> prerr_string ("Error: File " ^ fic ^ " cannot be open for read\n");;


let test_prop library =
        function
(*
          | Prop_list(p,species) -> (* instrumentate the species *)
              if is_complete (fst species) then
                begin
                  begin
                    try     (* verifying types *)
                      Focalize_inter.good_prms species
                    with
                    | Foc_def_err s -> display_foc_def_err
                       ("Type's error in arguments of " ^ (fst species))
                    s;
                    exit 1
                  end;
                  let type_to_generate =
                    List.fold_left (fun s (_,e) -> get_forall_types e ++ s) [] p in
                  let typ_meth,fichier_fml,spec_harness,specs_colls_instru =
                    instru_species species type_to_generate in 
                  (* obtain the species and collection testing the property *)
                  let l_nfs = List.map (fun p -> normalise p, To_strings.string_of_prop (snd p)) p in
                  let colllist,speccoll_test = ast_testprop_list l_nfs spec_harness typ_meth in
                  let file_foc = fic_create [library] (top_preambule @ specs_colls_instru @
                  speccoll_test @ top_postambule colllist) in
                  file_foc,fichier_fml
                end
              else
                begin
                  prerr_string ("Error : The species " ^ fst species ^ " is not complete\n");
                  exit 32
                end
*)
        | Test_context(p, tc) ->
            let type_to_generate =
              List.fold_left (fun s (_,e) -> get_forall_types e ++ s) [] p in
            let spec_harness,specs_colls_instru =
              Species_harness.species_harness tc type_to_generate in 
            (* obtain the species and collection testing the property *)
            let l_nfs = List.map (fun p -> normalise p, To_strings.string_of_prop (snd p)) p in
            let colllist,speccoll_test = ast_testprop_list l_nfs tc spec_harness in
            let file_foc = fic_create [library] (top_preambule @ specs_colls_instru @
            speccoll_test @ top_postambule colllist) in
                  file_foc
        | Report (test_context,prop_l) ->
            let get_var_l l = List.fold_left (fun s ((e,_),_) ->
                                               snd (List.split (variables_to_list e)) ++ s) [] l in
            let type_to_generate =
              List.fold_left (fun s (_,_,l) -> get_var_l l ++ s) [] prop_l in
            let spec_harness,specs_colls_instru =
              Species_harness.species_harness test_context type_to_generate in 
            let colllist,speccoll_test = ast_testprop_reinject_list prop_l spec_harness in
            let file_foc = fic_create [library] (top_preambule @ specs_colls_instru @
            speccoll_test @ top_postambule colllist) in
               file_foc

(** [test_prop p] tests a property [p] which can be a list of property or a test report (a test
report contain a list of property in elementary forms). *)


(* Take a list of proposition and a file_name, create the file with source code
   testing prop *)
let proceed p library =
      print_verbose "Generating .fcl abstract syntax tree ...\n";
      let file_foc = test_prop library p in
      print_verbose "Writing .fcl file ...\n";
      let file_fml = add_import (top_import (get_file_output_xml())) [] in
      print_foc_file (get_file_output_foc ()) file_foc file_fml;;
(*       print_verbose "Writing .fml file ...\n"; *)
(*       print_fml_file (get_file_output_fml ()) (add_import (top_import (get_file_output_xml ())) []) *)

let remove_suffixe s =
  let len = String.length s in
  if len > 3 && Filename.check_suffix s ".fo" then
    Filename.chop_suffix s ".fo"
  else
    s;;

let generate =
  let first_call = ref true in
  fun file ->
  begin
    try
      if !first_call then
       (first_call := false;
        let library_name = Filename.basename (remove_suffixe file) in
        (* load the environment *)
          focalize_init library_name;
                (*******)
        let prop =
          match get_use_report () with
          | None -> 
              begin
              match get_test_context () with
              | None ->
                  failwith("You should specify a context of test");
(*
                  let species = fst (get_species_test ()) in
                  let property = get_property_test () in
                  Prop_list (List.map (fun n -> n,get_prop_def species n) property,get_species_test ())
*)
              | Some tc ->
                  let species = Context_test.sc_get_name (Context_test.tc_get_end_sc tc) in
                  let property = get_property_test () in
                  Test_context(List.map (fun n -> n,get_prop_def species n) property,tc)
              end
          | Some t ->
              let report = (report_from_xml_file t) in
              set_test_context (fst report);
(*               print_string (Own_xml.string_of_report report); *)
              Report (report_from_xml_file t) in
        proceed prop library_name
       )
      else
       prerr_string ("Not processing " ^ file ^ ": only one file could be processed at a time.")
    with
      | Property_doesnt_exists p ->
          prerr_string ("The property " ^ p ^ " doesn't exist at all\n");
          exit 18
      | Function_doesnt_exists p ->
          prerr_string ("The function " ^ p ^ " doesn't exist at all\n");
          exit 18
      | Species_dont_exists s ->
          prerr_string ("The species " ^ s ^ " doesn't exist at all\n");
          exit 19
(*
      | Foc_def_err s -> display_foc_def_err "" s;
                         exit 1
*)
      | Not_found  -> prerr_string "The property doesn't exists\n";
                      exit 2
(*
      | Foc_Failure s -> prerr_string s;
                         exit 3
*)
      | Not_a_property s -> prerr_string (s ^ " exists but is not a property\n");
                            exit 4
      | Rep_not_defined s -> prerr_string ("The species " ^ s ^ " has an undefined rep\n");
                             exit 5
      | Bad_parameters msg ->
          prerr_string ("An error occured when getting parameters of a species : " ^ msg ^ "\n");
          exit 11
      | Species_not_defined -> 
          prerr_string "You should specify the species to test\n";
          exit 6
      | Property_not_defined ->
          prerr_string "You have to specify the property to test\n";
          exit 7 
      | Not_good_form p ->
          prerr_string ("The property " ^ p ^ " is not in normal form\n");
          exit 8
      | Focalize_inter.Bad_parameters_numbers(test,exp) ->
          prerr_string ("Bad numbers of arguments : expected " ^ (string_of_int exp) ^ ", given " ^
                        string_of_int test ^ "\n");
          exit 8
      | Not_defined(s,l) ->
          prerr_string ("The species " ^ s ^ " is expected to be complete but some methods are only declared :\n");
          List.iter (fun m -> prerr_string (m ^ "\n")) l;
                      exit 9
      | Cant_instanciate_param(e,p) ->
          prerr_string ("The species expects (" ^ string_of_parameters_expect e ^
          ") but you provide " ^ string_of_parameters_instance_verbose p ^ "\n")

  end;;

let usage = 
  "usage: foctest -s spc -p prop [-v] [-o outfile] [-S size] [-t number] [--let | --seq] my_file_fo

Print on outfile (a.out.fcl if not specify) an automatic testing-harness.
This harness generates a Focal source program which submits tests case to the
property prop in species spc.
my_file_fo (without extension) is the binary .fo file containing the species. It
can be either a library from the standard library or a library in the current
directory.
The --seq and --let options specify how to simulate sequences of instructions on 
the generated .fcl file:
  --let makes ocamlc producting some warning messages
  --seq can raise a stackoverflow if the number of test case is to high\n

Example:
  foctest -s 'my_species(integers, field, #int_plus(5,6))' -p 'plus_commutes,mult_commutes' library.fo

will test the property plus_commutes and mult_commutes in the species my_species
with integers, field and #int_plus(5,6) (11) as instance of parameters.";;

let parse_args () = Arg.parse
                      (Arg.align 
                      [
(*
      "-s", Arg.String (fun s ->
                          set_species_test (parse_string Own_parser.expr_species s)),
                 "spc set which species to test";
*)
      "-s", Arg.String (fun s ->
                          set_test_context (parse_test_context s)),
                 "spc Set which species to test";
      "-p", Arg.String (fun s ->
                         set_property_test (parse_string Own_parser.properties_test s)
                       ),
                 "prop Set which property to test";
      "-o", Arg.String set_file_output,
                 "outfile Set output file (default a.out.fcl)";
      "-sicstus", Arg.Unit (fun () -> set_use_prolog true),
                 " We want to use sicstus";
      "-sicstus-globalstk", Arg.Int (fun i -> set_globalstk i),
                 " Size of the global stack in MB";
      "-sicstus-localstk", Arg.Int (fun i -> set_localstk i),
                 " Size of the local stack in MB";
      "-sicstus-choicestk", Arg.Int (fun i -> set_choicestk i),
                 " Size of the choice stack in MB";
      "-sicstus-trailstk", Arg.Int (fun i -> set_trailstk i),
                 " Size of the trail stack in MB";
      "-sicstus-prologmax", Arg.Int (fun i -> set_prologmax i),
                 " Maximum size of the prolog data space in MB";
      "-sicstus-path", Arg.String (fun s -> set_prolog_path s),
                 " Where to find sicstus";
      "-sicstus-opt", Arg.String (fun s -> set_prolog_opt s),
                 " Which option for solving (meta|naive|noback)";
      "-v", Arg.Unit (fun () -> set_verbose_mode true),
                 " Verbose mode";
      "-mcdc", Arg.Int set_mcdc_number,
                 "n Set number of times we want to apply MC/DC on the precondition";
      "-int", Arg.Int set_int_size,
                  "n Set the size of integer. The generated interval is [-n/2; n/2 - 1]";
      "-S", Arg.Int set_size_value_test,
                 "size Set size of element we want to generate";
      "-t", Arg.Int set_number_of_test,
                 "number Set the number of test case";
      "-stat", Arg.String (fun e -> set_prolog_stat_file (Some e)),
                 "filename Set the file where sicstus sould put internal statistics values";
      "--seq", Arg.Unit set_seq,
                 " Use a seq(e2,e1) style for simulate sequence of instructions in the .fcl file"; 
      "-r", Arg.String set_use_report,
                 "report Use this test report as test case set"; 
      "--let", Arg.Unit set_let,
                 " Use a \"let _unused = e1 in e2\" style for simulate sequence of instructions in the .fcl file";
      "-funtop",Arg.String (fun s -> ignore (externfun (Some (Toplevel s)))),
                 "topf Set the random rep function to a toplevel function";
      "-funspec",Arg.String (fun s -> ignore (externfun (Some (Species s)))),
                 "spcf Set the random rep function from the species under test"
                    ])
                      generate
                      usage ;;

parse_args ();;
(* (Global.fd_val None "cr_qrr") *)


(*
let l = Test_prolog.import_all_types ();;
Print_prolog.print_prolog_clause Format.std_formatter l;;
*)

(*
List.iter
 (fun e ->
   print_string (Own_types.string_of_typ_definition e);
   print_newline ()
   ) (get_all_types ());;
*)


(* open Context_test;; *)
(*
               let s1 = create_species_context "length_int" [];;

let t2 = create_binding_context "mon_truc_a_moi" s1
let s1 = create_species_context "triangle" ["mon_truc_a_moi"];;

let tc = create_test_context [t2] s1;; 
*)
(*
let s1 = create_species_context "list_0"   [];;
let c1 = create_binding_context "zero" s1
let s2 = create_species_context "list_1"   ["zero"];;
let c2 = create_binding_context "un" s2
let s3 = create_species_context "list_2"   ["zero";"un"];;
let c3 = create_binding_context "deux" s3
let s4 = create_species_context "list_3"   ["zero";"un";"deux"];;
let c4 = create_binding_context "trois" s4
let s5 = create_species_context "list_4"   ["zero";"un";"deux";"trois"];;
let c5 = create_binding_context "quatre" s5
let s6 = create_species_context "list_5"   ["zero";"un";"deux";"trois";"quatre"];;
let c6 = create_binding_context "cinq" s6
let s7 = create_species_context "list_6"   ["zero";"un";"deux";"trois";"quatre";"cinq"];;
let c7 = create_binding_context "six" s7
let s8 = create_species_context "list_7"   ["zero";"un";"deux";"trois";"quatre";"cinq";"six"];;
let c8 = create_binding_context "sept" s8
let s9 = create_species_context "list_8"   ["zero";"un";"deux";"trois";"quatre";"cinq";"six";"sept"];;
let c9 = create_binding_context "huit" s9
let s10 = create_species_context "list_9"  ["zero";"un";"deux";"trois";"quatre";"cinq";"six";"sept";"huit"];;
let c10 = create_binding_context "neuf" s10
let s11 = create_species_context "list_10" ["zero";"un";"deux";"trois";"quatre";"cinq";"six";"sept";"huit";"neuf"];;
*)

(*
let tc  = parse_test_context 
"
let zero = list_0 in
let un     = list_1(zero) in
let deux   = list_2(zero,un) in
let trois  = list_3(zero,un,deux) in
let quatre = list_4(zero,un,deux,trois) in
let cinq   = list_5(zero,un,deux,trois,quatre) in
let six    = list_6(zero,un,deux,trois,quatre,cinq) in
let sept   = list_7(zero,un,deux,trois,quatre,cinq,six) in
let huit   = list_8(zero,un,deux,trois,quatre,cinq,six,sept) in
let neuf   = list_9(zero,un,deux,trois,quatre,cinq,six,sept,huit) in
list_10(zero,un,deux,trois,quatre,cinq,six,sept,huit,neuf)
";;

let my_add cp a =
  let s,cp = Context_test.cp_add tc cp a in
  print_string s;
  print_string "\n";
  cp;;

let cp = my_add species_test "p5";;
let cp = my_add cp           "p2";;
let cp = my_add cp           "p1";;
let cp = my_add cp           "p";;
*)

(* let n,prm_l = get_species_test ();; *)

