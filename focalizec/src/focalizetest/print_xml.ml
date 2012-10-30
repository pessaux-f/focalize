open Own_types ;;
open Own_basics ;;
open Own_expr ;;
open Useful_parsing ;;

let top_xml_spec_name = "Spec_xml" ;;
let top_xml_coll_name = "Coll_xml" ;;

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
   '>', "&gt;";
   '*', "&times;"
  ];;
let safe_replace s = string_assoc s special_xml;;

let xml_string_of_ident =
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
   "let xml_print_newline in @UNIT =\n\
       fun x in (@UNIT) -> #print_file(\"\\n\")";
   "let xml_print_header in @UNIT = fun t in (@UNIT) ->\n\
      #print_file(\"<?xml version=\\\"1.0\\\" encoding=\\\"utf-8\\\" ?>\\n\");\n\
      #print_file(\"<!DOCTYPE xsl:stylesheet [ <!ENTITY times \\\"&#215;\\\" > ]>\");\n\
      #print_file( \"<?xml-stylesheet type=\\\"text/xsl\\\" href=\\\"test_report.xslt\\\" ?>\");\n\
      !xml_print_newline(@VUNIT)";
      (* * prints the property * *)
   "let xml_variable_to_string in @STRING =\n\
     fun v in (@STRING * @STRING) ->\n\
         let n = @FST(v) in\n\
         let t = @SND(v) in\n\
         @SC(\n\
           @SC(\"\\n<varname>\",@SC(n,\"</varname>\\n\")),\n\
           @SC(\"<vartype>\",@SC(t,\"</vartype>\\n\"))\n\
            )\n\
     ";
   "let rec xml_aux_print_several in @UNIT =\n\
       fun tag in (@STRING) ->\n\
       fun l in ( list(@STRING)) ->\n\
     match l with\n\
     | @NIL -> @VUNIT\n\
     | @CONS(e,r) ->\n\
         (#print_file(@SC(\"<\", @SC(tag,\">\")));\n\
          #print_file(e);\n\
          #print_file(@SC(\"</\", @SC(tag,\">\\n\")));\n\
          !xml_aux_print_several(tag,r)\n\
         )";
   "let xml_print_several in @UNIT =\n\
     fun tags in (@STRING) ->\n\
     fun tag in (@STRING) ->\n\
     fun l in (list(@STRING)) ->\n\
       if @STRUCT_EQUAL(l,@NIL) then\n\
         @VUNIT\n\
       else\n\
        (#print_file(@SC(\"<\", @SC(tags,\">\\n\")));\n\
         !xml_aux_print_several(tag,l);\n\
         #print_file(@SC(\"</\", @SC(tags,\">\")));\n\
         !xml_print_newline(@VUNIT)\n\
        )";
         (* *********************** *)
         (* Les formes elementaires *)
   "let xml_print_elementaire in @UNIT =\n\
        fun propr in (list(@STRING * @STRING) * (list(@STRING) * list(@STRING))) ->\n\
          #print_file(\"<elementaire>\\n\");\n\
          #print_file(\"<forme>\\n\");\n\
          !xml_print_several(\"variables\",\"variable\",#list_map(!xml_variable_to_string,@FST(propr)));\n\
          !xml_print_several(\"preconditions\",\"focexpr\",@FST(@SND(propr)));\n\
          !xml_print_several(\"conclusions\",\"focexpr\",@SND(@SND(propr)));\n\
          #print_file(\"</forme>\");\n\
          !xml_print_newline(@VUNIT)";
   "let xml_close_elementaire in @UNIT =\n\
     fun l in (@LIST(@LIST(@RESULT))) ->\n\
     fun nb in (@INT) ->\n\
     fun nb_try in (@INT) ->\n\
     fun times in (@INT) ->\n\
     fun meth in (@STRING) ->\n\
       #print_file(\"<stats>\\n\");\n\
       #print_file(\"<conclucoverage>\\n\");\n\
       #print_file(\"<nb_reach>\");\n\
       #print_file(#string_of_int(Coll_coverage!coverage_conclu(l)));\n\
       #print_file(\"</nb_reach>\\n\");\n\
       #print_file(\"<nb_total>\");\n\
       #print_file(#string_of_int(nb));\n\
       #print_file(\"</nb_total>\\n\");\n\
       #print_file(\"</conclucoverage>\\n\");\n\
       #print_file(\"<nb_generated>\");\n\
       #print_file(#string_of_int(nb_try));\n\
       #print_file(\"</nb_generated>\\n\");\n\
       #print_file(\"<times>\");\n\
       #print_file(#string_of_int(times));\n\
       #print_file(\"</times>\\n\");\n\
       #print_file(\"<method>\");\n\
       #print_file(meth);\n\
       #print_file(\"</method>\\n\");\n\
       #print_file(\"</stats>\\n\");\n\
       #print_file(\"</elementaire>\\n\")";
         (* * * * * * * * * * * * * * * * *)
          (*   Les proprietes de départ  *)
           (* * * * * * * * * * * * * * *)
   "let xml_print_property in @UNIT =\n\
     fun n in (@STRING) -> fun f in (@STRING) ->\n\
      #print_file(\"<propriete>\\n\");\n\
      #print_file(\"<name>\");\n\
      #print_file(n);\n\
      #print_file(\"</name>\\n\");\n\
      #print_file(\"<forme>\");\n\
      #print_file(f);\n\
      #print_file(\"</forme>\");\n\
      !xml_print_newline(@VUNIT)";
   "let xml_close_property in @UNIT = fun x in (@UNIT) ->\n\
      #print_file(\"</propriete>\");\n\
        !xml_print_newline(@VUNIT)";
         (* *************************************** *)
   "let xml_print_parameter in @UNIT =\n\
     fun p in (@STRING) ->\n\
       #print_file(\"<specparameter>\");\n\
       #print_file(p);\n\
       #print_file(\"</specparameter>\\n\")";
   "let xml_print_species in @UNIT = \n\
     fun spec in ((@STRING * @STRING) * @LIST(@STRING)) ->\n\
       #print_file(\"<species>\");\n\
       #print_file(@SC(\"<module>\",@SC(@FST(@FST(spec)),\"</module>\\n\")));\n\
       #print_file(@SC(\"<name>\",@SC(@SND(@FST(spec)),\"</name>\\n\")));\n\
       if @INT_EQUAL(#list_length(@SND(spec)), 0) then\n\
         @VUNIT\n\
       else\n\
         (#print_file(\"<specparameters>\\n\");\n\
          #list_map(!xml_print_parameter,@SND(spec));\n\
          #print_file(\"</specparameters>\\n\")\n\
         );\n\
       #print_file(\"</species>\")";
   "let xml_print_collection in @UNIT = \n\
     fun spec in (@STRING * ((@STRING * @STRING) * @LIST(@STRING))) ->\n\
       #print_file(\"<collection>\");\n\
       #print_file(\"<paramname>\");\n\
       #print_file(@FST(spec));\n\
       #print_file(\"</paramname>\");\n\
       !xml_print_species(@SND(spec));\n\
       #print_file(\"</collection>\")";
   "let xml_open_report in @UNIT =\n\
     fun context in (@LIST(@STRING * ((@STRING * @STRING) * @LIST(@STRING))) * \n\
                    (@STRING * @STRING) * @LIST(@STRING)) ->\n\
     #print_file(\"<rapport>\\n\");\n\
     #print_file(\"<context_test>\\n\");\n\
     if @INT_EQUAL(#list_length(@FST(context)), 0) then\n\
         @VUNIT\n\
      else\n\
        (#print_file(\"<collections>\\n\");\n\
         #list_map(!xml_print_collection,@FST(context));\n\
         #print_file(\"</collections>\\n\")\n\
        );\n\
     !xml_print_species(@SND(context));\n\
     #print_file(\"</context_test>\");\n\
     !xml_print_newline(@VUNIT)";
   "let xml_close_report in @UNIT = fun x in (@UNIT) ->\n\
       #print_file(\"</rapport>\\n\")";
       (* ******************************** *)
   "let xml_timeout in @UNIT =\n\
     fun x in (@UNIT) -> #print_file(\"<timeout></timeout>\\n\")";
                     (* ** *)
   "let xml_open_test in @UNIT =\n\
     fun x in (@UNIT) -> #print_file(\"<test>\\n\")";
   "let xml_close_test in @UNIT =\n\
      fun x in (@UNIT) ->  #print_file(\"</test>\\n\")";
       (* ******************************** *)
   "let xml_print_value in @UNIT =\n\
      fun v in (@STRING * @STRING) ->\n\
        #print_file(\"<value>\");\n\
        #print_file(\"<valstring>\");\n\
        #print_file(@FST(v));\n\
        #print_file(\"</valstring>\");\n\
        #print_file(\"<valexpr>\");\n\
        #print_file(@SND(v));\n\
        #print_file(\"</valexpr>\");\n\
        #print_file(\"</value>\");\n\
        !xml_print_newline(@VUNIT)";
   "let xml_string_of_verdict in @STRING = fun v in (@RESULT) ->\n\
     match v with\n\
     | " ^ fst result_ok ^ " -> \"<ok></ok>\"\n\
     | " ^ fst result_ko ^ " -> \"<ko></ko>\"\n\
     | " ^ fst result_raise ^ "(s) -> @SC(\"<raise>\",@SC(s,\"</raise>\"))\n\
     ";
   "let xml_print_verdict in @UNIT = fun v in (@RESULT) ->\n\
     #print_file(!xml_string_of_verdict(v))\n\
     ";
   "let xml_print_conclu_one in @UNIT = fun x in (@RESULT) ->\n\
       #print_file(\"<conclu>\");\n\
       !xml_print_verdict(x);\n\
       #print_file(\"</conclu>\");\n\
           !xml_print_newline(@VUNIT)";
   "let xml_result_test in @UNIT =\n\
       fun vars in (@LIST(@STRING * @STRING)) ->\n\
       fun precond in (@RESULT * @LIST(@RESULT)) ->\n\
       fun conclu in (@RESULT * @LIST(@RESULT)) ->\n\
       #list_map(!xml_print_value,vars);\n\
       let ps = @SND(precond) in\n\
       let res = @FST(conclu) in\n\
       let cs = @SND(conclu) in\n\
       !xml_print_several(\"preconds\",\"precond\", #list_map(!xml_string_of_verdict,ps));\n\
       !xml_print_several(\"conclus\",\"conclu\", #list_map(!xml_string_of_verdict,cs));\n\
       #print_file(\"<resultat>\");\n\
       !xml_print_verdict(res);\n\
       #print_file(\"</resultat>\");\n\
       !xml_print_newline(@VUNIT)"
  ];;
       (*#list_iter(!xml_print_conclu_one, l);*)

let top_xml_def () =
  [create_toplevel_spec(
    spec_create top_xml_spec_name [] [] (Some (TAtom(None, foctunit))) (List.map (fun x -> Unique x) (top_xml_meths ()))
             );
    let m = Whattodo.get_output_module () in
    create_toplevel_coll(coll_create top_xml_coll_name
                                     (create_species_name m top_xml_spec_name,[]))
  ]


