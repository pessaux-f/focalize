open Parsetree;;
open Parsetree_utils;;
open Env;;
open TypeInformation;;
open Own_basics;;

let extract_vname v =
  match v with
  | Vlident v -> v
  | Vuident v -> v
  | Vpident v -> v
  | Viident v -> v
  | Vqident v -> v
;;

let extract_qname v =
  match v with
  | Vname vn -> extract_vname vn
  | Qualified(m,vn) -> m ^ "#" ^ extract_vname vn;;

let extract_ident i =
  match i with
  | I_local v -> extract_vname v
  | I_global v -> extract_qname v;;

let rec string_of_foctyp =
        let open_paren prec op_prec =
                if prec > op_prec then "(" else "" in
        let close_paren prec op_prec =
                if prec > op_prec then ")" else "" in
        let rec print prec exp = 
                match exp with
                | TE_ident s -> extract_ident s.ast_desc
                | TE_fun (t1,t2) ->
                                open_paren prec 0 ^
                                print 0 t1.ast_desc ^ " -> " ^ print 0 t2.ast_desc ^
                                close_paren prec 0 (* Ok *)
                | TE_prod ([]) -> "empty_tuple"
                | TE_prod (e::t_l) ->
                                open_paren prec 1 ^
                                List.fold_left (fun s e -> s ^ " * " ^ print 0
                                e.ast_desc) (print 0 e.ast_desc) t_l ^
                                close_paren prec 1
                | TE_prop -> "prop" (* type of proposition. *)
                | TE_self -> "Self"  (* carrier type of the currently analysed species.  *)
                | TE_app(s, l) -> extract_ident s.ast_desc ^ to_args string_of_foctyp l
                | TE_paren e -> string_of_foctyp e in
         fun t -> 
            print 0 t.ast_desc;;

let print_foctyp t = print_string (string_of_foctyp t);;

(*
let rec meth_to_string m =
        let args_to_string arg = match arg with
        | [] -> ""
        | e::r -> "(" ^ (List.fold_left (fun seed e -> seed ^
                                                      "," ^
                                                      (meth_to_string e) )
                                       (meth_to_string e)
                                       r) ^ ")" in
        match m with
        | SpecCall (s,m,arg) ->
                        if s = "Self" then
                                "!" ^ m ^  (args_to_string arg)
                        else  

  let a,b = pattern_of_tpattern p in
                                s ^ "!" ^ m ^  (args_to_string arg)
        | Basic (m,arg) -> "#" ^ (args_to_string arg)
        | (LVar x) -> x;;

*)
let extract_constant c =
  match c with
  | C_int r -> r
  | C_float r -> r
  | C_bool r -> r
  | C_string r -> r
  | C_char r -> "\"" ^ String.make 1 r  ^ "\"";;



let rec string_of_texp_desc e =
      let rec aux e  =
        match e with
        | E_const c -> extract_constant c.ast_desc
        | E_fun ([],e) -> aux e.ast_desc
        | E_fun (Vlident s::t,e) -> "fun " ^ s ^ " -> " ^ aux (E_fun(t,e))
      (*  | E_let (s,e1,e2) -> "let " ^ s ^ "=" ^ string_of_expr e1 ^ "in" ^ aux
       *  e2 *)
        | E_if (e1,e2,e3) -> "if " ^ aux e1.ast_desc ^ " then " ^ aux
        e2.ast_desc ^ " else " ^ aux e3.ast_desc 
        | E_match (e,_l) -> "match " ^ string_of_expr e ^ "with " ^ " bla bla bla"
        | E_app(e1,l1) -> string_of_expr e1 ^ to_args string_of_expr l1
        | E_tuple _ -> "E_tuple"
        | E_external _ -> "E_external"
        | E_paren e -> "(" ^ aux e.ast_desc ^ ")" 
        | _ -> failwith "string_of_texp_desc (debug)" in
        aux e.ast_desc
and string_of_expr e = string_of_texp_desc e;;

let print_expr e = print_string (string_of_expr e);;

(* Transforms a proposition to string  *)
let string_of_prop =
  let open_paren prec op_prec =
    if prec > op_prec then "(" else "" in
  let close_paren prec op_prec =
    if prec > op_prec then ")" else "" in
  let rec pts prec =
    fun p -> match p with
     | Pr_forall([],_,p) -> pts (-1) p.ast_desc

     | Pr_forall(e::r,t,p) -> "all " ^ extract_vname e ^ " " ^  pts (-1) (Pr_forall(r,t,p)) ^ ", "
 (*      let rec f l =
         match l with
         | [] -> " : "
*)
     | Pr_exists([],_,p) -> pts (-1) p.ast_desc
     | Pr_exists(e::r,t,p) -> "exists " ^ extract_vname e ^ " " ^  pts (-1) (Pr_exists(r,t,p)) ^ ", "
     | Pr_and (p1,p2) ->
        (open_paren prec 4) ^
        (pts 4 p1.ast_desc) ^ " and " ^ (pts 4 p2.ast_desc) ^ 
        (close_paren prec 4)
     | Pr_or (p1,p2) ->
        (open_paren prec 3) ^
        (pts 3 p1.ast_desc) ^ " or " ^ (pts 3 p2.ast_desc) ^
        (close_paren prec 3)
     | Pr_imply ({ ast_desc = (Pr_imply (_,_) as p1) }, p2) ->
        (open_paren prec 2) ^
        (pts 18 p1) ^ " -> " ^ (pts 2 p2.ast_desc) ^
        (close_paren prec 2)
     | Pr_imply(p1,p2) ->
        (open_paren prec 2) ^
        (pts 2 p1.ast_desc) ^ " -> " ^ (pts 2 p2.ast_desc) ^
        (close_paren prec 2)
     | Pr_equiv(p1,p2) ->
        (open_paren prec 1) ^
        (pts 1 p1.ast_desc) ^ " <-> " ^ (pts 1 p2.ast_desc) ^
        (close_paren prec 1)
     | Pr_not p -> "not(" ^ (pts 0 p.ast_desc) ^ ")"
     | Pr_paren p -> "(" ^ pts (-1) p.ast_desc ^ ")"
     | Pr_expr e -> string_of_expr e in
     pts 0;;

let print_prop p = print_string (string_of_prop p);;

(** type lident used in Let to accept pairs in let (a,b)=c *)
(*let string_of_lident l =
  match l with
  | V s -> s
  | Crp(s1,s2) ->
    "Crp(" ^ s1 ^ ", " ^ s2 ^ ")";;
*)
(*
let string_of_tdef =
  function
  | TLet(lident, texpr) -> "TLet(" ^ string_of_lident lident ^ ", " ^
                                     string_of_expr texpr ^ ")" ^
                                     string_of_foctyp (Global.fd_env None "Cons")
  | TSig(s,t) -> s ^ " " ^ string_of_foctyp t
  | TBtype(s, prm_list, texp_desc, tdef_list) -> "azerty"
  | _ -> failwith "mlsdfklm"
;;

*)


let ff(r : species_field) : unit =
  match r with
  | SF_sig _ -> print_string "sig "
  | SF_let _ -> print_string "let "
  | SF_let_rec _ -> print_string "let_rec "
  | SF_theorem _ -> print_string "theorem "
  | SF_property _ -> print_string "property "
;;


let pppexpr r =
  match r with
  | SPE_Self -> print_string "Self "
  | SPE_Species((qvn, _)) -> print_string (extract_qname qvn); print_string " "
  | SPE_Expr_entity _e -> print_string "entity"
;;

let f (r : species_param) : unit =
  match r with
  | SPAR_in(_vn1,(vn2,c_n), _kind) ->
      print_string "in : ";
      print_string (vn2 ^ " " ^ c_n ^ "\n")
  | SPAR_is((vn2,c_n),_kind,field_l, expr, _) ->
      print_string "is : ";
      print_string (vn2 ^ "#" ^ c_n);
      print_string "[";
      List.iter ff field_l;
      print_string "]\n";
      print_string (extract_ident expr.sse_name.ast_desc);
      print_string "(";
      List.iter pppexpr expr.sse_effective_args;
      print_string ")"
;;


