open Whattodo ;;
open Own_types ;;
open Own_expr ;;
open Own_basics ;;
open Useful_parsing ;;
open Fresh_variable ;;

(* Appel et declaration des fonctions importées *)

exception Coinductive_type of string;;

let meth_rnd_self = random_meth (string_of_ttyp (TAtom(None, focself)));;
let meth_parse_self = parse_meth (string_of_ttyp (TAtom(None, focself)));;

(* Get all intermediaries types within a type *)
let depends =
  let rec aux typ cumul =
    if List.mem typ cumul then cumul
    else
      match typ with
      | TAtom (m, t) -> (
          try 
            let types_inside =
              List.fold_left
                (fun s (_,t) -> t ++ s)
                [] (Focalize_inter.get_concrete_def (TPrm(m,t, []))) in
            List.fold_left (fun s e -> aux e s) ([typ]++cumul) types_inside
          with Focalize_inter.Not_a_concrete_type _t_o -> [typ] ++ cumul
         )
      | TSpecPrm _ -> [typ] ++ cumul
      | TProd (t1,t2)
      | TFct (t1,t2) -> aux t1 (aux t2 ([typ] ++ cumul))
      | TPrm (_m, _, _l) ->
          let types_inside =
            List.fold_left
              (fun s (_,t) -> t ++ s)
              [] (Focalize_inter.get_concrete_def typ) in
          List.fold_left (fun s e -> aux e s) ([typ]++cumul) types_inside in
  function
    | TAtom _ as typ -> [typ]
    | TPrm (_, _s, _l) as typ ->
        let types_inside =
          List.fold_left
            (fun s (_, t) -> t ++ s)
            [] (Focalize_inter.get_concrete_def typ) in
        List.fold_left (fun s e -> aux e s) [typ] types_inside
    | typ -> aux typ []
;;

(* The predefinition *)

(*
let predefined_random_fml =
 [foctint,("rand_int","fun n -> Random.int n");
  foctfloat,("rand_float","fun n -> Random.float 10.0");
  foctunit,("rand_unit","fun n -> ()");
  foctstring,("rand_string","fun n -> \"string\"");
  foctchar,("rand_char","fun n -> char_of_int (Random.int 256)");
  foctbool,("rand_bool","fun n -> (Random.int 2) = 0");
  "big_int",("rand_big_int",
                "fun n ->
                   let zero = Char.code '0' in
                   let rec entier_string s n =
                     if n <= 0 then
                       s
                     else
                       entier_string (s ^ Char.escaped (Char.chr (zero + Random.int 10))) (n-1) in
                   let generate_chaine = entier_string \"0\" in
                   big_int_of_string (generate_chaine n)")
 ];;
*)

(*
let predefined_print_fml =
 [foctfloat,("print_float","fun f -> string_of_float f");
  foctchar,("print_char","fun c -> String.make 1 c");
  "big_int",("print_big_int", "fun bi -> string_of_bigint bi")
 ];;
*)

(*
let default_random_name = [
   TAtom(Some "basics", foctint),   "random_int";
   TAtom(Some "basics", foctunit),  "random_unit";
   TAtom(Some "basics", "big_int"), "random_big_int";
   TAtom(Some "basics", foctbool),  "random_bool";
   TAtom(Some "basics", foctfloat), "random_float";
];;
*)

(*
let default_print_name = [
   TAtom(Some "basics", foctint),"print_int";
   TAtom(Some "basics", foctunit),"print_unit";
   TAtom(Some "basics", "big_int"),"print_big_int";
   TAtom(Some "basics", foctfloat),"print_float";
];;
*)

let predefined_random_meth = (* définition des fonctions importées *)
     [  foctint, meth_create
                    (random_meth (string_of_ttyp (TAtom(Some "basics", foctint))))
                    (TFct(TAtom(Some "basics", foctint),TAtom(Some "basics", foctint)))
                    (expr_basic (Prefix(None, "rand_int")) [])
                    false;
        foctunit, meth_create
                    (random_meth (string_of_ttyp (TAtom(Some "basics", foctunit))))
                    (TAtom(Some "basics", foctunit))
                    (expr_fun "n" (TAtom(Some "basics", foctint)) (expr_glob focunit))
                    false;
        "big_int", meth_create
                    (random_meth (string_of_ttyp (TAtom(Some "basics", "big_int"))))
                    (TAtom(Some "basics", "big_int"))
                    (expr_fun "n" (TAtom(Some "basics", foctint)) (expr_basic (Prefix(None, "rand_big_int")) []))
                    false;
        foctbool, meth_create
                    (random_meth (string_of_ttyp (TAtom(Some "basics", foctbool))))
                    (TFct(TAtom(Some "basics", foctint), TAtom(Some "basics", foctbool)))
                    (expr_basic (Prefix(None, "rand_bool")) [])
                    false;
        foctfloat, meth_create
                    (random_meth (string_of_ttyp (TAtom(Some "basics", foctfloat))))
                    (TAtom(Some "basics", foctfloat))
                    (expr_basic (Prefix(None, "rand_float")) [])
                    false;
        foctstring, meth_create 
                    (random_meth (string_of_ttyp (TAtom(Some "basics", foctstring))))
                    (TAtom(Some "basics", foctstring))
                    (expr_fun "n" (TAtom(Some "basics", foctint)) (expr_string "unavailable"))
                    false;
		 ];;

let predefined_print_meth = (* définition des fonctions importées *)
     [ foctint, meth_create
                    (print_meth (string_of_ttyp (TAtom(Some "basics", foctint))))
                    (TFct(TAtom(Some "basics", foctint),TAtom(Some "basics", foctstring)))
                    (expr_glob focprintint)
                    false;
       foctbool,
          parse_foc_meth ("let " ^ print_meth (string_of_ttyp 
                                                (TAtom(Some "basics",
                                                foctbool))) ^ " in @STRING =\n\
                             fun b in (@BOOL) -> if b then \"true\" else \"false\"");
       foctunit, meth_create
                    (print_meth (string_of_ttyp (TAtom(Some "basics", foctunit))))
                    (TAtom(Some "basics", foctstring))
                    (expr_fun "n" (TAtom(Some "basics", foctunit)) (MString (ident_name focunit)))
                    false;
       "big_int", meth_create
                    (print_meth (string_of_ttyp (TAtom(Some "basics", "big_int"))))
                    (TAtom(Some "basics", foctstring))
                    (expr_fun "n" (TAtom(Some "basics", "big_int")) (expr_caml "print_big_int"))
                    false;
       foctstring, meth_create
                    (print_meth (string_of_ttyp (TAtom(Some "basics", foctstring))))
                    (TAtom(Some "basics", foctstring))
                    (expr_fun "n" (TAtom(Some "basics", foctstring)) (MString "unavailable"))
                    false;
		 ];;

let predefined_reinject_value_meth = (* définition des fonctions importées *)
     [ foctint, meth_create
                    (reinject_value_meth (string_of_ttyp (TAtom(Some "basics", foctint))))
                    (TAtom(Some "basics", foctint))
                    (expr_fun "n" (TAtom(Some "basics", foctint)) (expr_var "n"))
                    false;
       foctunit, meth_create
                    (reinject_value_meth (string_of_ttyp (TAtom(Some "basics", foctunit))))
                    (TAtom(Some "basics", foctunit))
                    (expr_fun "n" (TAtom(Some "basics", foctunit)) (expr_var "n"))
                    false;
       foctbool,
          parse_foc_meth ("let " ^ reinject_value_meth (string_of_ttyp 
                                                (TAtom(Some "basics",
                                                foctbool))) ^ " in @BOOL =\n\
                             fun b in (@BOOL) -> b");
       "big_int", meth_create
                    (reinject_value_meth (string_of_ttyp (TAtom(Some "basics", "big_int"))))
                    (TAtom(Some "basics", "big_int"))
                    (expr_fun "n" (TAtom(Some "basics", "big_int")) (expr_var "n"))
                    false;
       foctstring, meth_create
                    (reinject_value_meth (string_of_ttyp (TAtom(Some "basics", foctstring))))
                    (TAtom(Some "basics", foctstring))
                    (expr_fun "n" (TAtom(Some "basics", foctstring)) (expr_var "n"))
                    false;
		 ];;

let predefined_print_xml_meth = (* définition des fonctions importées *)
     [ foctint, meth_create
                    (print_xml_meth (string_of_ttyp (TAtom(Some "basics", foctint))))
                    (TAtom(Some "basics", foctstring))
                    (expr_fun "n" (TAtom(Some "basics", foctint))
                    (expr_basic focstringconcat [expr_string "<exprint>";
                     expr_basic focstringconcat [expr_basic focprintint [expr_var "n"];
                                      expr_string "</exprint>"]])
                    )
                    false;
       foctbool,
          parse_foc_meth ("let " ^ print_xml_meth (string_of_ttyp 
                                                (TAtom(Some "basics",
                                                foctbool))) ^ " in @STRING =\n\
                             fun b in (@BOOL) -> if b then\n\
                               \"<exprbool>true</exprbool>\" else \"<exprbool>false</exprbool>\"");
       foctunit, meth_create
                    (print_xml_meth (string_of_ttyp (TAtom(Some "basics", foctunit))))
                    (TAtom(Some "basics", foctstring))
                    (expr_fun "n" (TAtom(Some "basics", foctunit)) (MString "<exprglobid><prefix><name>()</name></prefix></exprglob_id>"))
                    false;
       foctstring, meth_create
                    (print_xml_meth (string_of_ttyp (TAtom(Some "basics", foctstring))))
                    (TAtom(Some "basics", foctstring))
                    (expr_fun "n" (TAtom(Some "basics", foctstring)) (MString "<exprglobid><prefix><name>unavailable</name></prefix></exprglob_id>"))
                    false;
                    
(*       "big_int", meth_create
                    (print_meth (string_of_ttyp (TAtom(Some "basics", "big_int"))))
                    (TAtom "string")
                    (expr_fun "n" (TAtom(Some "basics", "big_int")) (expr_caml "print_big_int"))
                    false; *)
		 ];;

let predefined_parse_meth = (* définition des fonctions importées *)
     [  foctint, meth_create
                    (parse_meth (string_of_ttyp (TAtom(Some "basics", foctint))))
                    (TFct(TAtom(Some "basics", foctstring),TAtom(Some "basics", foctint)))
                    (expr_glob focparseint)
                    false;
       foctbool,
          parse_foc_meth ("let " ^ parse_meth (string_of_ttyp 
                                                (TAtom(Some "basics",
                                                foctbool))) ^ " in @BOOL =\n\
                             fun b in (@STRING) -> if @STRUCT_EQUAL(b, \"true\") then true else false");
       foctstring, meth_create
                    (parse_meth (string_of_ttyp (TAtom(Some "basics", foctstring))))
                    (TAtom(Some "basics", foctstring))
                    (expr_fun "x" (TAtom(Some "basics", foctstring)) (expr_var "x"))
                    false;
		 ];;

(* separate the elements of a constructor.
   for example :
   couple in int -> int -> couple_int
   it returns :
   [int;int]
 
 *)
(*let rec recolte_fun e =
  match e with
    | MFun(_,t,e) -> t :: recolte_fun e
    | _ -> [];;
*)

(* ************************************************************************* *)
(* ************************************************************************* *)
(* ************************************************************************* *)
(* ************************************************************************* *)
(*                       Functions for the constructors                      *)

(* ast_random_cons : typ list -> myexpr
   
   ast_random_cons l return the expr using a constructor on the list choice randomly
*)
let ast_random_cons (l : Own_types.constructor list) name =
    let nval_def e =
      expr_let_notyp "nval"
               (expr_app (expr_glob (Prefix(None, "rand_int_good"))) [expr_int (List.length l)])
               e in
    let call_construct n l =
      match l with
      | [] -> expr_glob n
      | _ ->
            (expr_app (expr_glob n)
               (List.map (fun x ->
                            expr_meth focself
                                      (random_meth (string_of_ttyp x))
                                      [expr_basic focpred [expr_var "n"]]
                         )
                         l
               )
            ) in
    let rec aux i lcons name =
        match lcons with
          | [] -> failwith ("Random_rep.ast_random_cons: No constructors for type " ^ name)
          | [n,t_opt] ->
              call_construct n t_opt
          | (n,t_opt)::(_::_ as r) ->
              expr_if (expr_app (expr_glob focintequal) [expr_var "nval";expr_int i])
                      (call_construct n t_opt)
                      (aux (i+1) r name) in
      nval_def (aux 0 l name);;

(* Take a constructor name and returns the ast which convert its to a string *)
let ast_print_cons ((n, param) : Own_types.constructor) =
  let new_id =
    let id = ref 0 in
    fun () -> id := !id + 1 ; "v" ^ string_of_int !id in
  let aux (v, f) = "!" ^ f ^ "(" ^ v ^ ")" in
  let aux2 l =
    match l with
    | [] -> "\"\""
    | e :: r ->
        "@SC(@SC(\"(\"," ^
        List.fold_left
          (fun s e -> "@SC(" ^ s ^ ", @SC(\", \", " ^ aux e ^ "))") (aux e) r ^
        "),\")\")" in
  let param_n =
    List.map (fun e -> new_id (), print_meth (string_of_ttyp e)) param in
  n,
  List.map (fun e -> Some e) (fst (List.split param_n)),
  parse_foc_expr ("@SC(\"" ^ ident_name n ^ "\",\n" ^ aux2 param_n ^ ")") ;;


let string_for_parser_of_ident s =
  match s with
  | Infix s -> "#" ^ "'" ^ s ^ "'"
  | Prefix (None, t) -> "#" ^ t
  | Prefix (Some m, t) -> m ^ "#" ^ t
;;


(* Take a constructor name and returns the ast which convert its to a string *)
let ast_parse_one_cons ((n,l) : Own_types.constructor) e err =
  let a i = "a" ^ string_of_int i in
  let aux n l  = 
    let rec aux n l i cumul =
      match l with
      | [] -> string_for_parser_of_ident n ^ to_args 
                      (fun (t,i) -> "!" ^ parse_meth (string_of_ttyp t) ^ "(" ^ a i ^ ")")
                      (List.rev cumul)
      | e::r -> "match args with\n\
                 | @NIL -> " ^ err ^ "\n\
                 | @CONS(" ^ a i ^ ",args) -> " ^ aux n r (i+1) ([e,i] @ cumul) in
    aux n l 1 [] in
  match l with
  | [] -> "if @STRUCT_EQUAL(cons, \"" ^ atom_of_cons_name (ident_name n) ^ "\") then\n\
                   " ^ string_for_parser_of_ident n ^ "\n\
             else\n\
               " ^ e
  | _ ->
            "if @STRUCT_EQUAL(cons, \"" ^ atom_of_cons_name (ident_name n) ^ "\") then\n\
               "  ^ aux n l ^ "\n\
             else\n\
               " ^ e;;

let ast_parse_cons (l : Own_types.constructor list) name _t =
  let err = "@FOC_ERROR(\"parse_error on type :" ^ name ^ "\")" in
  let rec aux l =
    match l with
    | [] -> err
    | e::r -> ast_parse_one_cons e (aux r) err in
     aux l


(* ************************************************************************* *)
(* ************************************************************************* *)
(* ************************************************************************* *)
(* ************************************************************************* *)



let rec ast_print_type l_param typ : Own_expr.a_method list   =
  match typ with
    | TProd(t1,t2) ->
        let n1 = print_meth (string_of_ttyp t1) in
        let n2 = print_meth (string_of_ttyp t2) in
        let n = print_meth (string_of_ttyp typ) in
          [parse_foc_meth (
             "let " ^ n ^ "  in @STRING =\n\
                 fun x in (" ^ string_of_typ typ ^ ") ->\n\
                   @SC(!" ^ n1 ^ "(@FST(x)), !" ^ n2 ^ "(@SND(x)))")]
    | TAtom(m, s) ->
        begin try
          let l = Focalize_inter.get_concrete_def (TPrm(m, s, [])) in
          if l = [] then
            raise (Focalize_inter.Not_a_concrete_type None)
          else
            ast_print_type l_param (TPrm(m, s,[])) 
        with
        | Focalize_inter.Not_a_concrete_type _
        | Focalize_inter.Type_dont_exists _  ->
          begin try
            let meth = List.assoc s predefined_print_meth in
            [meth]
          with
          | Not_found -> failwith ("The type " ^ s ^ " is unknown.")
          end
        end
    | TPrm(_m, _name, _)  ->
        let cons = Focalize_inter.get_concrete_def typ in
        let n = print_meth (string_of_ttyp typ) in
        let cons_print = List.map ast_print_cons cons in
        [meth_create n (TAtom(Some "basics", foctstring)) 
          (expr_fun "x" typ
             (expr_match_notyp (expr_var "x") cons_print))
          true]
    | TSpecPrm s ->
        let n = print_meth (string_of_ttyp typ) in
        [parse_foc_meth ("let " ^ n ^ "  in @STRING = fun x in (" ^ s ^ ") -> " ^ s ^ "!print(x)")]
    | TFct (_,_) -> failwith "Random_rep.ast_print_type : functional type, not yet implemented";;

(* ************************************************************************* *)
(* ************************************************************************* *)
let ast_print_ident i =
  match i with
  | Prefix(None, s) ->
      "@SC(\"<prefix><name>\",@SC(\"" ^ s ^ "\", \"</name></prefix>\"))"
  | Prefix(Some m, s) ->
      "\"<prefix><module>"^ m ^ "</module><name>" ^ s ^ "</name></prefix>\""
  | Infix s -> "@SC(\"<infix>\",@SC(\"" ^ s ^ "\", \"</infix>\"))";;

(* Take a constructor name and returns the ast which convert its to an xml string *)
let ast_print_xml_cons ((n, param) : Own_types.constructor) =
  let new_id = let id = ref 0 in fun () -> id := !id +1; "v" ^ string_of_int !id in
  let aux (v,f) = "@SC(\"<appright>\",\n\
                   @SC(" ^ "!" ^ f ^ "(" ^ v ^ "),\n\
                       \"</appright>\"))" in
  let aux2 l =
    match l with
    | [] -> "\"\""
    | e::r ->
        List.fold_left
        (fun s e -> "@SC(" ^ s ^ ", " ^ aux e ^ ")") (aux e) r
  in
  let param_n = List.map (fun e -> new_id (), print_xml_meth (string_of_ttyp e)) param in
  n,
  List.map (fun e -> Some e) (fst (List.split param_n)),
  parse_foc_expr
               (if  param = [] then
                 "@SC(\"<exprglobid>\"," ^
                 "@SC(" ^ ast_print_ident n ^ "," ^
                 "    \"</exprglobid>\"))"
                else
                 "@SC(\"<exprapp><appleft><exprglobid>\",\n\
                  @SC(" ^ ast_print_ident n ^ ",\n\
                  @SC(\"</exprglobid></appleft>\",\n\
                  @SC(" ^ aux2 param_n ^ ",\n\
                      \"</exprapp>\"))))"
               );;



(* ************************************************************************* *)
(* ************************************************************************* *)


let rec ast_print_xml_type l_param typ : Own_expr.a_method list   =
  match typ with
    | TProd(t1,t2) ->
        let n1 = print_xml_meth (string_of_ttyp t1) in
        let n2 = print_xml_meth (string_of_ttyp t2) in
        let n = print_xml_meth (string_of_ttyp typ) in
          [parse_foc_meth (
             "let " ^ n ^ "  in @STRING =\n\
                 fun x in (" ^ string_of_typ typ ^ ") ->\n\
                  @SC(\"<exprapp><appleft><exprglobid><prefix><module>basics</module><name>pair</name></prefix></exprglobid></appleft><appright>\",\n\
                  @SC(" ^ "!" ^ n1 ^ "(@FST(x)),\n\
                  @SC(\"</appright><appright>\",\n\
                  @SC(" ^ "!" ^ n2 ^ "(@SND(x)),\n\
                      \"</appright></exprapp>\"))))"
          )]
    | TAtom(m, s) ->
        begin try
          let l = Focalize_inter.get_concrete_def (TPrm(m, s, [])) in
          if l = [] then
            raise (Focalize_inter.Not_a_concrete_type None)
          else
            ast_print_xml_type l_param (TPrm(m, s,[])) 
        with
        | Focalize_inter.Not_a_concrete_type _
        | Focalize_inter.Type_dont_exists _  ->
          begin try
            let meth = List.assoc s predefined_print_xml_meth in
            [meth]
          with
          | Not_found -> failwith ("The type " ^ s ^ " is unknown.")
          end
        end
    | TPrm(_m, _name,_)  ->
        let cons = Focalize_inter.get_concrete_def typ in
        let n = print_xml_meth (string_of_ttyp typ) in
        let cons_print = List.map ast_print_xml_cons cons in
        [meth_create n (TAtom(Some "basics", foctstring)) 
          (expr_fun "x" typ
             (expr_match_notyp (expr_var "x") cons_print))
          true]
    | TSpecPrm s ->
        let n = print_xml_meth (string_of_ttyp typ) in
        let fself = print_xml_meth (string_of_ttyp (TAtom(None, focself))) in
        [parse_foc_meth ("let " ^ n ^ "  in @STRING = fun x in (" ^ s ^ ") -> " ^
        s ^ "!" ^ fself ^ "(x)")]
    | TFct (_,_) -> failwith "Random_rep.ast_print_xml_type : can't print a functional type";;


(* ************************************************************************* *)
(* ************************************************************************* *)
(* ************************************************************************* *)

(* ast_random_type : typ -> import * methods * string *)
(* Take a type and return a method instanciating it *)
let rec ast_random_type l_param typ =
  match typ with
    | TProd(t1,t2) ->
        let meth_name = random_meth (string_of_ttyp typ) in
        let m1 = random_meth (string_of_ttyp t1) in
        let m2 = random_meth (string_of_ttyp t2) in
          [parse_foc_meth ("let " ^ meth_name ^ " in " ^ string_of_typ typ ^ " =\n\
                             fun n in (@INT) ->\n\
                               @CRP(!" ^ m1 ^ "(n),!" ^ m2 ^ "(n))")]
    | TPrm(m, name, _)  ->
        let cons = Focalize_inter.get_concrete_def typ in
        let (cons_fin,cons_nfin) =
          let list_or f l = List.fold_left (fun s e -> s || f e) false l in
          List.partition (fun (_s, l) ->
                            list_or (Own_types.is_in (TAtom(m, name))) l
                         ) 
                         cons
        in
        let meth_name = (random_meth (string_of_ttyp typ)) in
        let random_choice = expr_app (expr_glob (Prefix(None, "rand_int_good"))) [MInt 5] in
        let condthen =  expr_app (expr_glob focintequal) [expr_var "value"; MInt 0] in
        let choisi_cons_nfin = ast_random_cons cons_nfin name in
 (*       let choisi_cons_fin  = ast_random_cons cons_fin name in *)
        let thenbranch = if cons_nfin = [] then failwith ("Le type " ^ name ^ " est co_inductif") else choisi_cons_nfin in
        let elsebranch = if cons_fin = [] then None else Some (ast_random_cons cons_fin name) in
        let corpus =
          match elsebranch with
            | None -> thenbranch
            | Some e -> expr_let_notyp "value" (expr_if (expr_basic focintleq [expr_var "n"; expr_int 0])
                                                (expr_int 0)
                                                random_choice) (expr_if condthen thenbranch e) in
           (* Calculate the list of caml imported function and the list of method *)
        [meth_create meth_name 
                    typ
                    (expr_fun "n" (TAtom(Some "basics", foctint)) corpus)
                    true]
    | TAtom(m, s) ->
        begin try
          let l = Focalize_inter.get_concrete_def (TPrm(m, s, [])) in
          if l = [] then
            raise (Focalize_inter.Not_a_concrete_type None)
          else
            ast_random_type l_param (TPrm(m, s,[])) 
        with
        | Focalize_inter.Not_a_concrete_type _
        | Focalize_inter.Type_dont_exists _  ->
            begin try
              let func = List.assoc s predefined_random_meth in
              [func]
            with
            | Not_found -> (* It's probably a parameters of the species *)
                List.iter (fun s -> print_string ("   " ^ s ^ "   ")) l_param;
                failwith ("Error: Imported caml's type or collection unknowns, (non-subrep types are not yet supported): " ^ s) 
            end (* try *)             
        end
    | TSpecPrm s ->
      let meth_name = random_meth (string_of_ttyp typ) in
      let mmethod = meth_create meth_name typ
                                (expr_fun "n"
                                  (TAtom(Some "basics", foctint))
                                  (expr_meth s meth_rnd_self [expr_var "n"])) false in
        [mmethod]
    | TFct (_,_) -> failwith "Random_rep.ast_random_type : Not yet implemented";;

(* ast_parse_type : typ -> import * methods * string *)
(* Take a type and return a method instanciating it *)
let rec ast_parse_type l_param typ =
  match typ with
    | TProd(t1,t2) ->
        let meth_name = parse_meth (string_of_ttyp typ) in
        let m1 = parse_meth (string_of_ttyp t1) in
        let m2 = parse_meth (string_of_ttyp t2) in
        let error = "@FOC_ERROR(\"parse error on type : " ^ string_of_typ typ ^ "\")"
        in
          [parse_foc_meth ("let " ^ meth_name ^ " in " ^ string_of_typ typ ^ " =\n\
                             fun n in (@STRING) ->\n\
                               let truc = #split_cons(n) in\n\
                               if @STRUCT_EQUAL(@FST(truc),\"pair\") then\n\
                                 match @SND(truc) with\n\
                                 | @NIL -> " ^ error ^ "\n\
                                 | @CONS(e1,r) ->\n\
                                     ( match r with\n\
                                       | @NIL -> " ^ error ^ " \n\
                                       | @CONS(e2,r) ->\n\
                                         @CRP(!" ^ m1 ^ "(e1), !" ^ m2 ^ "(e2))\n\
                                     )\n\
                               else\n\
                                " ^ error)
          ] 
    | TPrm(_m, name,_)  ->
        let cons = Focalize_inter.get_concrete_def typ in
        let meth_name = (parse_meth (string_of_ttyp typ)) in
        [parse_foc_meth
           ("let rec " ^ meth_name ^ " in " ^ string_of_typ typ ^ " =\n\
               fun e ->\n\
                let ee = #split_cons(e) in\n\
                let cons = @FST(ee) in\n\
                let args = @SND(ee) in \n\
                " ^ ast_parse_cons cons name typ)]
    | TAtom(m, s) ->
        begin try
          let l = Focalize_inter.get_concrete_def (TPrm(m, s, [])) in
          if l = [] then
            raise (Focalize_inter.Not_a_concrete_type None)
          else
            ast_parse_type l_param (TPrm(m, s,[])) 
        with
        | Focalize_inter.Not_a_concrete_type _
        | Focalize_inter.Type_dont_exists _  ->
            begin try
              let func = List.assoc s predefined_parse_meth in
              [func]
            with
            | Not_found -> (* It's probably a parameters of the species *)
                List.iter (fun s -> print_string ("   " ^ s ^ "   ")) l_param;
                failwith ("Error: Imported caml's type or collection unknowns, (non-subrep types are not yet supported) for parse: " ^ s) 
            end (* try *)             
        end
    | TSpecPrm s ->
      let meth_name = parse_meth (string_of_ttyp typ) in
      let mmethod = meth_create meth_name typ
                                (expr_fun "n"
                                  (TAtom(Some "basics", foctstring))
                                  (expr_meth s meth_parse_self [expr_var "n"])) false in
        [mmethod]
    | TFct (_,_) -> failwith "Random_rep.ast_random_type : Not yet implemented";;



(* Create the ast of the method generating an object of type rep
    (it consists in calling the good function)   *)
let meth_random_rep rep =
  let func = Whattodo.externfun in 
  match func None with
  | Nothing -> meth_create meth_rnd_self
                  (TAtom(None, focself))
                  (expr_fun "n" (TAtom(Some "basics", foctint))
                                (expr_meth focself
                                  (random_meth (string_of_ttyp rep)) [expr_var "n"]))
                  false
  | Species s -> meth_create meth_rnd_self
                     (TAtom(None, focself))
                     (expr_fun "n" (TAtom(Some "basics", foctint))
                                   (expr_meth focself s [expr_var "n"]))
                     false
  | Toplevel t -> meth_create meth_rnd_self
                     (TAtom(None, focself))
                     (expr_fun "n" (TAtom(Some "basics", foctint))
                                   (expr_app (expr_glob (Prefix(None, t))) [expr_var "n"]))
                     false;;


(* ast_random_types typ list -> import, methods list, string list *)
(* ast_random_types l_t : returns all methods needed to create a random instance for all types l_t *)
let ast_random_types l_param typ_l =
   List.fold_left (fun m t ->
                       let n_m = (ast_random_type l_param t) in
                     meths_concat m n_m
                  )
                  []
                  typ_l;;

(** the methods the import a value for type Self *)
(*
let ast_of_external = 
  let rec aux t r = 
    match t with
    | TAtom(s) ->
        r
    | TSpecPrm(s) -> 
        s ^ "!" ^ of_external_meth () ^ "(" ^ r ^ ")"
    | TFct(t1, t2) ->
        failwith "Random_rep.ast_of_external"
    | TProd(t1, t2) -> (** product type *)
        let t1n = Fresh_variable.new_uncapitalize () in
        let t2n = Fresh_variable.new_uncapitalize () in
       "let " ^ t1n ^ " = @FST(" ^ r ^ ") in
        let " ^ t2n ^ " = @SND(" ^ r ^ ") in
        @CRP(" ^ aux t1 t1n ^ ", " 
               ^ aux t2 t2n ^ ")" 
        (** for a type : typ -> typ *)
    | TPrm(t,t_l) -> r (* FIXME : Error *) in
  fun rep ->
parse_foc_meth ("let " ^ of_external_meth ()  ^ " in Self = "
           ^ "fun n -> " ^
               aux rep "n");;
*)

(* ************************************************************************* *)
(* ************************************************************************* *)
(* ************************************************************************* *)

(* Take a constructor name and returns the ast which convert its to a string *)
let ast_reinject_value_cons ((n, param) : Own_types.constructor) =
  let new_id = let id = ref 0 in fun () -> id := !id +1; "v" ^ string_of_int !id in
  let aux (v,f) = "!" ^ f ^ "(" ^ v ^ ")" in
  let aux2 l =
    match l with
    | [] -> "\"\""
    | e::r ->
        List.fold_left (fun s e -> s ^ ", " ^ aux e) (aux e) r
  in
  let param_n = List.map (fun e -> new_id (), reinject_value_meth (string_of_ttyp e)) param in
  if param_n = [] then
    n,[], parse_foc_expr ("#" ^ ident_name n)
  else
    n,
    List.map (fun e -> Some e) (fst (List.split param_n)),
    parse_foc_expr
       (string_for_parser_of_ident n ^ "(" ^ aux2 param_n ^ ")");;



let rec ast_reinject_value_type l_param typ : Own_expr.a_method list   =
  match typ with
    | TProd(t1,t2) ->
        let n1 = reinject_value_meth (string_of_ttyp t1) in
        let n2 = reinject_value_meth (string_of_ttyp t2) in
        let n = reinject_value_meth (string_of_ttyp typ) in
          [parse_foc_meth (
             "let " ^ n ^ "  in " ^ (string_of_typ typ) ^ " =\n\
                 fun x ->\n\
                   @CRP(!" ^ n1 ^ "(@FST(x)), !" ^ n2 ^ "(@SND(x)))")]
    | TAtom(m, s) ->
        begin try
          let l = Focalize_inter.get_concrete_def (TPrm(m, s, [])) in
          if l = [] then
            raise (Focalize_inter.Not_a_concrete_type None)
          else
            ast_reinject_value_type l_param (TPrm(m, s, [])) 
        with
        | Focalize_inter.Not_a_concrete_type _
        | Focalize_inter.Type_dont_exists _  ->
          begin try
            let meth = List.assoc s predefined_reinject_value_meth in
            [meth]
          with
          | Not_found -> failwith ("The type " ^ s ^ " is unknown.")
          end
        end
    | TPrm(_m, _name,_)  ->
        let cons = Focalize_inter.get_concrete_def typ in
        let n = reinject_value_meth (string_of_ttyp typ) in
        let cons_print = List.map ast_reinject_value_cons cons in
        [meth_create n typ
          (expr_fun_notyp "x"
             (expr_match_notyp (expr_var "x") cons_print))
          true]
    | TSpecPrm s ->
        let n = reinject_value_meth (string_of_ttyp typ) in
        [parse_foc_meth ("let " ^ n ^ "  in " ^ string_of_typ typ ^ " =\n\
                fun x -> " ^ s ^ "!" ^ reinject_value_meth (string_of_ttyp
                (TAtom(None, focself))) ^ "(x)" )]
    | TFct (_,_) -> failwith "Random_rep.ast_print_type : Not yet implemented";;

(** Takes the list of parameters, the list of type we want to handle.
 Returns the methods, the caml import section corresponding to the methods and
 the association list typ <-> (print, random) methods *)
let ast_gen_print_types l_param typ_l : a_method list =
   List.fold_left
     (fun meths t ->
        let n_meths1 = ast_random_type l_param t in
        let n_meths2 = ast_print_type l_param t in
        let n_meths3 = ast_print_xml_type l_param t in
        let n_meths4 = ast_parse_type l_param t in
        let n_meths5 = ast_reinject_value_type l_param t in
         meths   @@  n_meths1 @@ n_meths2 @@
           n_meths3 @@ n_meths4 @@ n_meths5 @@ [] 
     )
     []
     typ_l;;

let ast_gen_print_types_rep rep =
  let r1 = random_meth (string_of_ttyp (TAtom(None, focself))) in
  let r2 = random_meth (string_of_ttyp rep) in
  let pr1 = print_meth (string_of_ttyp (TAtom(None, focself))) in
  let pr2 = "print" in (* print_meth (string_of_ttyp rep) in *)
  let pxml1 = print_xml_meth (string_of_ttyp (TAtom(None, focself))) in
  let pxml2 = print_xml_meth (string_of_ttyp rep) in
  let pa1 = parse_meth (string_of_ttyp (TAtom(None, focself))) in
  let pa2 = parse_meth (string_of_ttyp rep) in
  let rv1 = reinject_value_meth (string_of_ttyp (TAtom(None, focself))) in
  let rv2 = reinject_value_meth (string_of_ttyp rep) in
  meth_create r1 (TAtom(None, focself))
                 (expr_fun "n" (TAtom(Some "basics", foctint)) (expr_meth focself r2 [expr_var "n"]))
                 false::[
  meth_create pr1 (TAtom(Some "basics", foctstring))
                 (expr_fun "n" (TAtom(None, focself)) (expr_meth focself pr2 [expr_var "n"]))
                 false;
  meth_create pa1 (TAtom(None, focself))
                 (expr_fun "n" (TAtom(Some "basics", foctstring)) (expr_meth focself pa2 [expr_var "n"]))
                 false;
  meth_create pxml1 (TAtom(Some "basics", foctstring))
                 (expr_fun "n" (TAtom(None, focself)) (expr_meth focself pxml2 [expr_var "n"]))
                 false;
  meth_create rv1 (TAtom(None, focself)) (expr_fun_notyp "n" (expr_meth focself rv2 [expr_var "n"]))
                 false
                 ] ;;

(*
 string list -> Own_types.typ -> (string * string list) * Own_expr.methods list * string 
*)

(* Reverse the depends relation *)
let rec successors n all =
  match all with
  | [] -> []
  | e::r ->
      if List.mem n (List.filter (fun ee -> ee <> e) (depends e)) then
        e::successors n r
      else
        successors n r

let tsort edges seed =
   let rec sort path visited =
   function
     []  -> visited
    | n::nodes ->
      if List.mem n path then
       (let p_t t = print_string (string_of_typ t) in
        let p_s = print_string in
        List.iter (fun e -> p_s "Warning: "; p_t e; p_s " <- "; List.iter (fun t -> p_t t; p_s " ") (depends e); p_s "\n") edges;
        print_string "Warning: cyclic dependencies between types\n";


        let v' = visited in
        sort path v' nodes 

       )
      else
        let v' =
          if List.mem n visited then
            visited
          else
            n :: sort (n::path) visited (successors n edges) in
        sort path v' nodes in
  sort [] [] seed;;

let tri_topologique l =
  tsort l l;;

let ast_random l_param typ_list rep =
  let l = (List.map (fun e -> TSpecPrm e) l_param @ 
          (typ_list @ [rep])) ++ [] in
  let typ_list = List.fold_right (fun e s-> depends e ++ s) l [] in
  let typ_list = List.filter (function | TAtom(_,"int") -> false | _ -> true) typ_list in
  let typ_list = tri_topologique typ_list in
  let typ_list = TAtom(Some "basics", foctint)::typ_list in
  let p_t t = print_string (string_of_typ t) in
  List.iter (fun e -> p_t e; print_string " ") typ_list;
  print_newline ();
  let meths1 = ast_gen_print_types_rep rep in
  let meths2 = ast_gen_print_types l_param typ_list in
(*   ast_of_external rep :: *)
  [Multiple (meths2 @@ meths1)];;





