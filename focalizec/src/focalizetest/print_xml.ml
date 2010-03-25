open Rewrite_prop;;
(* open Random_rep;; *)

open To_strings;;
open Own_prop;;
open Own_types;;
open Own_expr;;
open Own_basics;;
open Own_expr;;
open Whattodo;;
open Focalize_inter;;
open Useful_parsing;;

let top_xml_spec_name = "Spec_xml"
let top_xml_coll_name = "Coll_xml"

let rec xml_string_of_typ t =
  match t with
  | TAtom(_m, s) -> 
      "<typeatom>"^ s ^ "</typeatom>" 
  | TSpecPrm(s) ->
      "<typeprmatom>"^ s ^ "</typeprmatom>" 
  | TFct(t1,t2) ->
      "<typefct><left>"^ xml_string_of_typ t1 ^ "</left><right>" ^ xml_string_of_typ t2 ^ "</right></typefct>" 
  | TProd(t1,t2) ->
      "<typeprod><left>"^ xml_string_of_typ t1 ^ "</left><right>" ^ xml_string_of_typ t2 ^ "</right></typeprod>" 
  | TPrm(_m, s,t_l) ->
      "<typeprm><prmname>"^ s ^ "</prmname>" ^
      List.fold_left (fun s e -> s ^ "<prmlist>" ^ xml_string_of_typ e ^ "</prmlist>" ) "" t_l ^
      "</typeprm>" 
      ;;

let special_xml =
  ['&', "&amp;";
   '<', "&lt;";
   '>', "&gt;"
  ];;
let safe_replace s = string_assoc s special_xml;;

let rec xml_string_of_ident =
  fun i ->
  match i with
  | Prefix(None, s) -> "<prefix><name>" ^ safe_replace s ^ "</name></prefix>"
  | Prefix(Some m, s) ->
      "<prefix> <module>" ^ safe_replace m ^ "</module>" ^ 
                "<name>" ^ safe_replace s ^ "</name></prefix>"
  | Infix s ->  "<infix>" ^ safe_replace s ^ "</infix>";;

let rec xml_string_of_expr e =
  match e with
  | MIfte(c,t,e) ->
      "<exprif>" ^
        "<cond>" ^ xml_string_of_expr c ^ "</cond>" ^
        "<then>" ^ xml_string_of_expr t ^ "</then>" ^
        "<else>" ^ xml_string_of_expr e ^ "</else>" ^
      "</exprif>"
  | MApp(e, _, e_l) ->
      "<exprapp>" ^
        "<appleft>" ^ xml_string_of_expr e ^ "</appleft>" ^
        List.fold_right (fun (e,_) s -> "<appright>" ^ xml_string_of_expr e ^ "</appright>" ^ s) 
                      e_l "</exprapp>"
  | MVar(_i, None) ->
      failwith "xml_string_of_expr: MVar no type"
  | MVar(i, Some t) ->
      "<exprvar>" ^
      "<varname>" ^ i ^ "</varname>" ^
      "<vartype>" ^ xml_string_of_typ t ^ "</vartype>" ^
      "</exprvar>"
  | MMeth(None, f) ->
      "<exprmeth><methname>" ^ f ^ "</methname></exprmeth>"
  | MMeth(Some c, f) ->
      "<exprmeth>" ^
        "<collname>" ^ c ^ "</collname>" ^
        "<methname>" ^ f ^ "</methname>" ^
      "</exprmeth>"
  | MFun(_v, None, _e)  ->
      failwith "xml_string_of_expr: MFun no type"
  | MFun(v, Some t, e)  ->
      "<exprfun>" ^
        "<funvar>" ^ v ^ "</funvar>" ^
        "<funtype>" ^ xml_string_of_typ t ^ "</funtype>" ^
        "<funexpr>" ^ xml_string_of_expr e ^ "</funexpr>" ^
      "</exprfun>"
  | MMatch (p,pl) -> (* myexpr * (string * string list * myexpr) list *)
      "<exprmatch>" ^
        "<matchval>" ^ xml_string_of_expr (fst p) ^ "</matchval>" ^
        List.fold_right
          (fun (_r,l,e) s ->
            "<exprpattern>" ^
              List.fold_right (fun e s ->  "<patroot>" ^ (match e with | None ->
                "_" | Some s -> s) ^ "</patroot>" ^ s) l 
              ("<patexpr>" ^ xml_string_of_expr e ^ "</parexpr>" ^
            "</exprpattern>" ^ s))
                      pl  "</exprmatch>"
  | MInt i  ->
      "<exprint>" ^ string_of_int i ^ "</exprint>"
  | MString s ->
      "<exprstring>" ^ s ^ "</exprstring>"
  | MVarloc(_b, s,e1,e2) ->
      "<exprlet>" ^
        "<letvar>" ^ (fst s) ^ "</letvar>" ^
        "<letvarexpr>" ^ xml_string_of_expr e1 ^ "</letvarexpr>" ^
        "<letexpr>" ^ xml_string_of_expr e2 ^ "</letexpr>" ^
      "</exprlet>"
  | MGlob_id(s) -> 
      "<exprglobid>" ^
        xml_string_of_ident s ^
      "</exprglobid>"
  | MCaml_def s -> 
      "<exprcaml_def>" ^ s ^ "</exprcaml_def>"
;;


let xml_string_of_parameters_instance =
  function
    | InstPrmColl(_,s) -> s
    | InstPrmEnt expr -> xml_string_of_expr expr;;

(*
let xml_value_to_string_of_typ  t =
  match t with 
  | TAtom s ->
  | TSpecPrm s -> 
  | TFct(t1,t2) -> failwith "Not yet implemented (and unimplementable)"
  | TProd(t1,t2) -> 
  | TPrm(n,p)  ->
;;
*)
let top_xml_meths () =
  List.map parse_foc_meth
  [
   "let xml_print_newline in @UNIT =
       fun x in (@UNIT) -> #print_file(\"\\n\")";
   "let xml_print_header in @UNIT = fun t in (@UNIT) ->
      #print_file(\"<?xml version=\\\"1.0\\\" encoding=\\\"utf-8\\\" ?>\\n\");
      #print_file( \"<?xml-stylesheet type=\\\"text/xsl\\\" href=\\\"test_report.xslt\\\" ?>\");
      !xml_print_newline(@VUNIT)";
      (* * prints the property * *)
   "let xml_variable_to_string in @STRING =
     fun v in (@STRING * @STRING) ->
         let n = @FST(v) in
         let t = @SND(v) in
         @SC(
           @SC(\"\\n<varname>\",@SC(n,\"</varname>\\n\")),
           @SC(\"<vartype>\",@SC(t,\"</vartype>\\n\"))
            )
     ";
   "let rec xml_aux_print_several in @UNIT =
       fun tag in (@STRING) ->
       fun l in ( list(@STRING)) -> 
     match l with
     | @NIL -> @VUNIT
     | @CONS(e,r) ->
         (#print_file(@SC(\"<\", @SC(tag,\">\")));
          #print_file(e);
          #print_file(@SC(\"</\", @SC(tag,\">\\n\")));
          !xml_aux_print_several(tag,r)
         )";
   "let xml_print_several in @UNIT =
     fun tags in (@STRING) ->
     fun tag in (@STRING) ->
     fun l in (list(@STRING)) ->
       if @STRUCT_EQUAL(l,@NIL) then
         @VUNIT
       else
        (#print_file(@SC(\"<\", @SC(tags,\">\\n\")));
         !xml_aux_print_several(tag,l);
         #print_file(@SC(\"</\", @SC(tags,\">\")));
         !xml_print_newline(@VUNIT)
        )";
         (* *********************** *)
         (* Les formes elementaires *)
   "let xml_print_elementaire in @UNIT =
        fun propr in (list(@STRING * @STRING) * (list(@STRING) * list(@STRING))) ->
          #print_file(\"<elementaire>\\n\");
          #print_file(\"<forme>\\n\");
          !xml_print_several(\"variables\",\"variable\",#list_map(!xml_variable_to_string,@FST(propr)));
          !xml_print_several(\"preconditions\",\"focexpr\",@FST(@SND(propr)));
          !xml_print_several(\"conclusions\",\"focexpr\",@SND(@SND(propr)));
          #print_file(\"</forme>\");
          !xml_print_newline(@VUNIT)";
   "let xml_close_elementaire in @UNIT =
     fun l in (@LIST(@LIST(@RESULT))) ->
     fun nb in (@INT) ->
     fun nb_try in (@INT) ->
       #print_file(\"<stats>\\n\");
       #print_file(\"<conclucoverage>\\n\");
       #print_file(\"<nb_reach>\");
       #print_file(#string_of_int(Coll_coverage!coverage_conclu(l)));
       #print_file(\"</nb_reach>\\n\");
       #print_file(\"<nb_total>\");
       #print_file(#string_of_int(nb));
       #print_file(\"</nb_total>\\n\");
       #print_file(\"</conclucoverage>\\n\");
       #print_file(\"<nb_generated>\");
       #print_file(#string_of_int(nb_try));
       #print_file(\"</nb_generated>\\n\");
       #print_file(\"</stats>\\n\");
       #print_file(\"</elementaire>\\n\")";
         (* * * * * * * * * * * * * * * * *)
          (*   Les proprietes de départ  *)
           (* * * * * * * * * * * * * * *)
   "let xml_print_property in @UNIT =
     fun n in (@STRING) -> fun f in (@STRING) ->
      #print_file(\"<propriete>\\n\");
      #print_file(\"<name>\");
      #print_file(n);
      #print_file(\"</name>\\n\");
      #print_file(\"<forme>\");
      #print_file(f);
      #print_file(\"</forme>\");
      !xml_print_newline(@VUNIT)";
   "let xml_close_property in @UNIT = fun x in (@UNIT) ->
      #print_file(\"</propriete>\");
        !xml_print_newline(@VUNIT)";
         (* *************************************** *)
   "let xml_print_parameter in @UNIT =
     fun p in (@STRING) ->
       #print_file(\"<specparameter>\");
       #print_file(p);
       #print_file(\"</specparameter>\\n\")";
   "let xml_print_species in @UNIT = 
     fun spec in ((@STRING * @STRING) * @LIST(@STRING)) ->
       #print_file(\"<species>\");
       #print_file(@SC(\"<module>\",@SC(@FST(@FST(spec)),\"</module>\\n\")));
       #print_file(@SC(\"<name>\",@SC(@SND(@FST(spec)),\"</name>\\n\")));
       if @INT_EQUAL(#list_length(@SND(spec)), 0) then
         @VUNIT
       else
         (#print_file(\"<specparameters>\\n\");
          #list_map(!xml_print_parameter,@SND(spec));
          #print_file(\"</specparameters>\\n\")
         );
       #print_file(\"</species>\")";
   "let xml_print_collection in @UNIT = 
     fun spec in (@STRING * ((@STRING * @STRING) * @LIST(@STRING))) ->
       #print_file(\"<collection>\");
       #print_file(\"<paramname>\");
       #print_file(@FST(spec));
       #print_file(\"</paramname>\");
       !xml_print_species(@SND(spec));
       #print_file(\"</collection>\")";
   "let xml_open_report in @UNIT =
     fun context in (@LIST(@STRING * ((@STRING * @STRING) * @LIST(@STRING))) * 
                    (@STRING * @STRING) * @LIST(@STRING)) ->
     #print_file(\"<rapport>\\n\");
     #print_file(\"<context_test>\\n\");
     if @INT_EQUAL(#list_length(@FST(context)), 0) then
         @VUNIT
      else
        (#print_file(\"<collections>\\n\");
         #list_map(!xml_print_collection,@FST(context));
         #print_file(\"</collections>\\n\")
        );
     !xml_print_species(@SND(context));
     #print_file(\"</context_test>\");
     !xml_print_newline(@VUNIT)";
   "let xml_close_report in @UNIT = fun x in (@UNIT) ->
       #print_file(\"</rapport>\\n\")";
       (* ******************************** *)
   "let xml_timeout in @UNIT =
     fun x in (@UNIT) -> #print_file(\"<timeout></timeout>\\n\")";
                     (* ** *)
   "let xml_open_test in @UNIT =
     fun x in (@UNIT) -> #print_file(\"<test>\\n\")";
   "let xml_close_test in @UNIT =
      fun x in (@UNIT) ->  #print_file(\"</test>\\n\")";
       (* ******************************** *)
   "let xml_print_value in @UNIT =
      fun v in (@STRING * @STRING) ->
        #print_file(\"<value>\");
        #print_file(\"<valstring>\");
        #print_file(@FST(v));
        #print_file(\"</valstring>\");
        #print_file(\"<valexpr>\");
        #print_file(@SND(v));
        #print_file(\"</valexpr>\");
        #print_file(\"</value>\");
        !xml_print_newline(@VUNIT)";
   "let xml_string_of_verdict in @STRING = fun v in (@RESULT) ->
     match v with
     | " ^ fst result_ok ^ " -> \"<ok></ok>\"
     | " ^ fst result_ko ^ " -> \"<ko></ko>\"
     | " ^ fst result_raise ^ "(s) -> @SC(\"<raise>\",@SC(s,\"</raise>\"))
     ";
   "let xml_print_verdict in @UNIT = fun v in (@RESULT) ->
     #print_file(!xml_string_of_verdict(v))
     ";
   "let xml_print_conclu_one in @UNIT = fun x in (@RESULT) ->
       #print_file(\"<conclu>\");
       !xml_print_verdict(x);
       #print_file(\"</conclu>\");
           !xml_print_newline(@VUNIT)";
   "let xml_result_test in @UNIT =
       fun vars in (@LIST(@STRING * @STRING)) ->
       fun precond in (@RESULT * @LIST(@RESULT)) ->
       fun conclu in (@RESULT * @LIST(@RESULT)) ->
       #list_map(!xml_print_value,vars);
       let ps = @SND(precond) in
       let res = @FST(conclu) in
       let cs = @SND(conclu) in
       !xml_print_several(\"preconds\",\"precond\", #list_map(!xml_string_of_verdict,ps));
       !xml_print_several(\"conclus\",\"conclu\", #list_map(!xml_string_of_verdict,cs));
       #print_file(\"<resultat>\");
       !xml_print_verdict(res);
       #print_file(\"</resultat>\");
       !xml_print_newline(@VUNIT)"
  ];;
       (*#list_iter(!xml_print_conclu_one, l);*)

let top_xml_def () =
  [create_toplevel_spec(
    spec_create top_xml_spec_name [] [] (Some (TAtom(None, foctunit))) (top_xml_meths ()) 
             );
    let m = Whattodo.get_output_module () in
    create_toplevel_coll(coll_create top_xml_coll_name
                                     (create_species_name m top_xml_spec_name,[]))
  ]


