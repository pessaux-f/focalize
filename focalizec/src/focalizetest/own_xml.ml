open Own_expr;;
open Own_types;;
open Own_prop;;
open Own_basics;;

type xml_tree =
  | Node of string * xml_tree list
         (** A node has a name and contains zero or more xml tree as son. *)
  | Leave of string * string option
         (** A leave has a name and can contains a value of type string. *)
(** The [xml_tree] defines the abstract syntax type of the xml tree. *)




(** The three report types. All xml_tree should be converted to these types *)
type test_case = myexpr list;; (** a test case is a list of values *)
type report_property = string * string *
                       ((variables * elementaire) * test_case list) list
             (** The report on a property is defined by its name, statement and
                 list of elementary forms and test sets *)
type report = Context_test.test_context * report_property list
(** A report is a species under test and a list of property reports *)


exception Bad_xml of string;;
(** Exception raise when a xml tree is malformed *)


let string_of_test_case : test_case -> string =
  fun e ->
  "\nTestcase : (" ^ List.fold_left (fun s e -> string_of_myexpr e ^ "|" ^ s) ")"
                                    e;;
(** For verbose puporses *)

let string_of_report_property ((n,f,elem_l) : report_property) =
  let null s t = if s = "" then "" else t  in
  "Propriete : " ^ n ^ "\n  " ^ f ^ "\n" ^
  "Elementaire : \n  " ^
  List.fold_left
    (fun s ((vars, elem_form),tc_l) ->
           let pred = list_of_precond (get_precond elem_form) in
           let con = list_of_conclusion (get_conclusion elem_form) in
           let forall = string_of_variables vars in
           let precond = (List.fold_left (fun s e ->
                                           s ^
                                           null s " and " ^ string_of_myexpr e) "" pred) in
           (null forall ("all " ^ forall)) ^ " " ^
           null precond (precond ^ " -> ") ^
           (List.fold_left (fun s e -> string_of_myexpr e ^ null s " or " ^ s) "" con) ^ "\n" ^
          (List.fold_left (fun s e -> string_of_test_case e ^ " " ^ s) "" tc_l) ^ "\n" ^  s
    )
    "" elem_l
;;
(** For verbose puporses *)

let string_of_report ((_s,e) : report) =
(*   print_string "Contexte de test :\n"; *)
(*   print_string (Context_test.string_of_tc s); *)
(*   print_string "\n\n"; *)
  List.fold_left (fun s e -> string_of_report_property e ^ s) "" e;;
(** For verbose puporses *)

let get_xml_tag_name t =
  match t with
  | Leave(tn, _)
  | Node(tn, _) ->
      tn;;
(** [get_xml_tag_name t] returns the name of the tag, root of the xml tree [t].

@raise Bad_xml if [t] does not contains a list of tags or is not of name [n]. *)




let get_xml_ldef t n =
  match t with
  | Node(tn, t_l) ->
      if tn = n then t_l else raise (Bad_xml ("expect <" ^ n ^ "> encounter <" ^ tn ^ ">"))
  | Leave(a, _) ->
      raise (Bad_xml ("Leave :" ^ n ^ "<>" ^ a));;
(** [get_xml_ldef t n] gets the list of tags within the tag [t] of name [n].

@raise Bad_xml if [t] does not contains a list of tags or is not of name [n]. *)




let get_xml_1ldef t n =
  match t with
  | Node(tn, t_l) ->
      (match t_l with
      | e::[] ->
         if tn = n then e else raise (Bad_xml n)
      | _ -> raise (Bad_xml n)
      )
  | Leave(_, _) ->
      raise (Bad_xml n);;
(** get_xml_1def t n same as [get_xml_ldef] but the tag should contain one tag,
it returns this tag.

@raise Bad_xml if [t] contains several tags or is not of name [n]. *)




let get_xml_def x s =
  match x with
  | Leave(_, None) -> ""
  | Node(_,_) -> raise (Bad_xml ("get_xml_def searching for " ^ s))
  | Leave(xs,Some o) ->
      if xs = s then o else raise (Bad_xml ("get_xml_def : " ^ xs ^ "<>" ^ s));;
(** get_xml_def t n get the string within the tag [t] of name [n].

@raise Bad_xml if [t] does not contains a list of tags or is not of name [n]. *)




let rec xml_get_pentvalue l =
  match l with
  | _n::v::[] -> get_xml_1ldef v "pentvalue"
  | _ -> raise (Bad_xml "xml_get_pent_value");;
(** [xml_get_pentvalue l] takes a list of xml tree which represents :

<pentname>string</pentname>
<pentvalue>...</pententvalue> (... is a focal expression represented in xml).

returns the expression inside the pentvalue tags.

@raise Bad_xml if [l] if the xml tree is malformed. *)




let rec xml_get_pcollvalue l =
  match l with
  | _n::v::[] -> get_xml_def v "pcollvalue"
  | _ -> raise (Bad_xml "xml_get_pent_value");;
(** [xml_get_pcollvalue l] takes a list of xml tree which represents :

<pcollname>string</pcollname>
<pcollvalue>string</pcollvalue>

returns the expression inside the pcollvalue tags.

@raise Bad_xml if [l] if the xml tree is malformed. *)




let xml_vartype_to_typ xml =
  let rec aux xml =
    match get_xml_tag_name xml with
    | "typeatom" -> TAtom(None, get_xml_def xml "typeatom") 
    | "typeprmatom" -> TSpecPrm(get_xml_def xml "typeprmatom")
    | "typefct" ->
        (match get_xml_ldef xml "typefct" with
        | e1::e2::[] ->
        TFct(aux (get_xml_1ldef e1 "left"),
             aux (get_xml_1ldef e2 "right"))
        | _ -> raise (Bad_xml "xml_vartype_to_typ")
        )
    | "typeprod" ->
        (match get_xml_ldef xml "typefct" with
        | e1::e2::[] ->
        TProd(aux (get_xml_1ldef e1 "left"),
              aux (get_xml_1ldef e2 "right"))
        | _ ->  raise (Bad_xml "xml_vartype_to_typ")
        )
    | "typeprm" ->
        begin
          match get_xml_ldef xml "typeprm" with
          | [] -> raise (Bad_xml "xml_vartype_to_typ")
          | e::r -> TPrm(None, get_xml_def e "prmname",
                 List.map (fun e -> aux (get_xml_1ldef e "prmlist")) r
                        )
        end
    | _ -> raise (Bad_xml "xml_vartype_to_typ") in 
  let body = get_xml_ldef xml "vartype" in
  match body with
  | _e::[] ->
      aux (get_xml_1ldef xml "vartype")
  | _ -> raise (Bad_xml "xml_vartype_to_typ");;

let xml_variable_to_variable xml =
  match get_xml_ldef xml "variable" with
  | n::t::[] ->
      create_variable (get_xml_def n "varname")
                      (xml_vartype_to_typ t)
  | _ -> raise (Bad_xml "xml_variable_to_variable");;

let xml_specname_to_specname m s =
  create_species_name
    (get_xml_def m "module")
    (get_xml_def s "name")

let special_xml =
  ["&lt;", "<";
   "&gt;", ">";
   "&amp;", "&"];;
let safe_replace s = string_assoc s special_xml;;

let rec xml_ident_to_ident x =
  match get_xml_tag_name x with
  | "prefix" ->
      let l = get_xml_ldef x "prefix" in
      begin
        match l with
        | i::[] -> Prefix(None, safe_replace (get_xml_def i "name"))
        | [m; i] -> Prefix(Some (get_xml_def m "module"),
                           safe_replace(get_xml_def i "name"))
        | _ -> raise (Bad_xml "prefix_ident")
        end
  | "infix" -> Infix (safe_replace (get_xml_def x "infix"))
  | _ -> raise (Bad_xml "ident");;

let rec xml_pat_to_pat p =
  let rec aux l =
   match l with
   | [] -> raise (Bad_xml "exprpattern")
   | e::[] -> [], xml_focexpr_to_expr (get_xml_1ldef e "patexpr")  
   | e::r ->
       let a,e' = aux r in
         Some (get_xml_def e "patappl")::a,e'
     in
  let x = get_xml_ldef p "exprpattern" in
  match x with
  | r::a_e ->
      let a,e = aux a_e in
        (xml_ident_to_ident r,a,e)
  | _ -> raise (Bad_xml "exprpattern")

and xml_focexpr_to_expr x = 
  match get_xml_tag_name x with
  | "exprif" ->
     (let x = get_xml_ldef x "exprif" in
      match x with
      | econd::ethen::eelse::[] ->
          MIfte(xml_focexpr_to_expr (get_xml_1ldef econd "cond"),
                xml_focexpr_to_expr (get_xml_1ldef ethen "then"),
                xml_focexpr_to_expr (get_xml_1ldef eelse "else"))
      | _ -> raise (Bad_xml "exprif")
     )
  | "exprapp" ->
     (let x = get_xml_ldef x "exprapp" in
      match x with
      | l::rs ->
          expr_app
              (xml_focexpr_to_expr (get_xml_1ldef l "appleft"))
              (List.map
                 (fun e -> xml_focexpr_to_expr (get_xml_1ldef e "appright"))
                 rs
              )
      | _ -> raise (Bad_xml "exprapp")
     )
  | "exprfun" ->
     (let x = get_xml_ldef x "exprfun" in
      match x with
      | v::t::e::[] -> MFun(get_xml_def v "funvar",
                            (Some (xml_vartype_to_typ (get_xml_1ldef t "funtype"))),
                            xml_focexpr_to_expr (get_xml_1ldef e "funexpr"))
      | _ -> raise (Bad_xml "exprfun")
     )
  | "exprvar" ->
     (match get_xml_ldef x "exprvar" with
      | n::t::[] ->
          expr_var_typ (get_xml_def n "varname") (xml_vartype_to_typ t)
      | _ -> raise (Bad_xml "xml_variable_to_variable")
     )
  | "exprint" -> MInt(int_of_string (get_xml_def x "exprint"))
  | "exprstring" -> MString(get_xml_def x "exprstring")
  | "exprcaml_def" -> MCaml_def(get_xml_def x "exprcaml_def")
  | "exprmeth" ->
     (let x = get_xml_ldef x "exprmeth" in
      match x with
      | m::[] -> MMeth(None, get_xml_def m "methname")
      | s::m::[] -> 
         MMeth(Some (get_xml_def s "collname"),
               get_xml_def m "methname"
              )
      | _ -> raise (Bad_xml "exprfun")
     )

  | "exprmatch" ->
     (let x = get_xml_ldef x "exprmatch" in
      match x with
      | e::p -> MMatch((xml_focexpr_to_expr (get_xml_1ldef e "matchval"), None),
                           List.map
                             (fun e -> xml_pat_to_pat e) p
                          )
      | _ -> raise (Bad_xml "exprmatch")
     )
  | "exprlet" ->
     (let x = get_xml_ldef x "exprlet" in
      match x with
      | v::e1::e2::[] ->
          MVarloc(false, (get_xml_def v "letvar", None),
                  xml_focexpr_to_expr (get_xml_1ldef e1 "letvarexpr"),
                  xml_focexpr_to_expr (get_xml_1ldef e2 "letexpr"))
      | _ -> raise (Bad_xml "exprif")
     )
  | "exprglobid" ->
     (let x = get_xml_1ldef x "exprglobid" in
     MGlob_id (xml_ident_to_ident x)
     )
  | s -> raise (Bad_xml ("focexpr -> " ^ s));;


let xml_param_to_param e =
  let p = get_xml_1ldef e "specparameter" in
  try
    create_instprmcoll (xml_get_pcollvalue (get_xml_ldef p "paramcoll"))
  with
  | Bad_xml _ ->
      create_instprment
        (xml_focexpr_to_expr
          (xml_get_pentvalue
            (get_xml_ldef p "parament")));;
(** xml_species_to_param e takes a parameter tag, returns the
[species_test] encoded inside [xml].

@raise Bad_xml for obvious reasons. *)

let xml_parameters_to_string_list xml : string list =
  let defs = get_xml_ldef xml "specparameters" in
  List.map (fun e -> get_xml_def e "specparameter") defs
;;

let xml_species_to_species_context xml : Context_test.species_context =
  let defs = get_xml_ldef xml "species" in
  match defs with
  | m::s::[]    -> Context_test.create_sc (xml_specname_to_specname m s) [] (* no parameters *)
  | m::s::r::[] -> Context_test.create_sc (xml_specname_to_specname m s) 
                                       (xml_parameters_to_string_list r)
  | _       -> raise (Bad_xml "xml_species_to_species_test")
;;

let xml_collection_to_binding_context xml : Context_test.binding_context =
  let defs = get_xml_ldef xml "collection" in
  match defs with
  | name::species::[] ->
      Context_test.create_bc (get_xml_def name "paramname")
                (Context_test.bca_c (xml_species_to_species_context species))
  | _ -> raise (Bad_xml "xml_collection_to_binding_context")
;;

let xml_species_to_test_context xml : Context_test.test_context =
  let defs = get_xml_ldef xml "context_test" in
  match defs with
  | []       -> raise (Bad_xml "xml_species_to_species_test")
  | e::[]    -> Context_test.create_tc [] (xml_species_to_species_context e) (* no parameters *)
  | e::r::[] ->
      begin
        let defs = get_xml_ldef e "collections" in
        Context_test.create_tc (List.map (xml_collection_to_binding_context) defs)
                  (xml_species_to_species_context r)
      end
  | _ -> raise (Bad_xml "xml_species_to_sepcies_test");;
(** xml_species_to_species_test xml takes a species tag, returns the
[species_test] encoded inside [xml].

@raise Bad_xml for obvious reasons. *)

let xml_forme_to_elementary : xml_tree -> variables * elementaire =
   let xml_focexpr_to_expr xml =
     let x = get_xml_1ldef xml "focexpr" in xml_focexpr_to_expr x in
   fun xml -> 
  let body = get_xml_ldef xml "forme" in
  match body with
  | [con] ->
      let c = get_xml_ldef con "conclusions" in
      variables_null ,
        List.fold_right (fun c s -> add_elem_conclusion (xml_focexpr_to_expr c) s)
                        c
                        elementaire_null
  | vp::con::[] ->
      if get_xml_tag_name vp = "variables" then
        let v = get_xml_ldef vp "variables" in
        let c = get_xml_ldef con "conclusions" in
        List.fold_right (fun e s -> add_variable (xml_variable_to_variable e) s) v variables_null, 
        List.fold_right (fun c s -> add_elem_conclusion (xml_focexpr_to_expr c) s)
                        c
                        elementaire_null
      else
(*       | Not_found -> failwith "Not_found" *)
(*       | Bad_xml _ -> *)
        let p = get_xml_ldef vp "preconditions" in
        let c = get_xml_ldef con "conclusions" in
        let elem_c = 
          List.fold_right (fun c s -> add_elem_conclusion (xml_focexpr_to_expr c) s)
                          c
                          elementaire_null in
        variables_null,
          List.fold_right (fun c s -> add_elem_precond (xml_focexpr_to_expr c) s)
                          p
                          elem_c
  | vars::pre::con::[] ->
      let v = get_xml_ldef vars "variables" in
      let p = get_xml_ldef pre "preconditions" in
      let c = get_xml_ldef con "conclusions" in
      let elem_c = 
        List.fold_right (fun c s -> add_elem_conclusion (xml_focexpr_to_expr c) s)
                        c
                        elementaire_null in
        List.fold_right (fun e s -> add_variable (xml_variable_to_variable e) s) v variables_null, 
        List.fold_right (fun c s -> add_elem_precond (xml_focexpr_to_expr c) s)
                        p
                        elem_c
  | _ -> raise (Bad_xml "xml_forme_to_elementary");;

let rec xml_test_case_value_to_myexpr defs : myexpr =
  let l = get_xml_ldef defs "value" in
  match l with
  | _::expr::[] ->
      let e = get_xml_1ldef expr "valexpr" in
         xml_focexpr_to_expr e
  | _ -> raise (Bad_xml "xml_test_case_value_to_myexpr");;

let rec xml_test_case_to_test_case l : test_case =
  match l with
  | [] -> []
  | e::r ->
      if get_xml_tag_name e = "value" then
         xml_test_case_value_to_myexpr e::xml_test_case_to_test_case r
      else
        [];;

let rec xml_test_case_set_to_test_case_set l : test_case list =
  match l with
  | [] -> []
  | e::r ->
      if get_xml_tag_name e = "test" then
        let l = get_xml_ldef e "test" in
         xml_test_case_to_test_case l::xml_test_case_set_to_test_case_set r
      else
        [];;

let xml_elementary_to_test_elementary xml : (variables * elementaire) * test_case list =
  let elem = get_xml_ldef xml "elementaire" in
  match elem with
  | f::((_::_) as l) ->
      let vars = xml_forme_to_elementary f in
      let ts = xml_test_case_set_to_test_case_set l in
      vars,ts 
  | _ -> raise (Bad_xml "xml_elementary_to_test_elementary") 
;;

let xml_property_to_property xml : report_property =
  let l = get_xml_ldef xml "propriete" in
  match l with
  | n::f::l_e -> 
     get_xml_def n "name",
     get_xml_def f "forme",
     (List.map xml_elementary_to_test_elementary l_e)
  | _ -> raise (Bad_xml "xml_property_to_property");;

let xml_report_to_report xml : report =
  let l = get_xml_ldef xml "rapport" in
  match l with
  | tc::props -> (* There should be one elementary form at least *)
      let bad = xml_species_to_test_context tc in
      let good = List.map xml_property_to_property props in
         bad,good
  | _ -> raise (Bad_xml "xml_report_to_report");;

