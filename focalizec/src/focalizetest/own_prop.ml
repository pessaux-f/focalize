(* open Typed_elt;; *)

open Own_types;;
open Own_expr;;
open Own_basics;;

let rename_expr_assoc assoc = 
  let add_s_o s_o l =
    List.fold_left (fun e -> (function None -> e | Some t -> t::e)) l s_o in
  let rec aux l_bound expr =
      match expr with
      | MIfte(b,e1,e2) -> MIfte(aux l_bound b,aux l_bound e1,aux l_bound e2)
      | MApp(e1, _t , e_l) ->
          expr_app (aux l_bound e1) (List.map (fun (e,_t) -> aux l_bound e) e_l)
      | MMeth(_e, _m) -> expr
      | MMatch((e, t), c_l) -> MMatch((aux l_bound e, t),
                            List.map (fun (s,l,e) -> (s,l,aux (add_s_o l l_bound) e)) c_l)
      | MFun(s,t,e) -> MFun(s,t,aux (s::l_bound) e)
      | MVarloc(b,(v,t),e1,e2) -> MVarloc(b,(v,t),aux l_bound e1,aux (v::l_bound) e2)
      | MVar(s, o) -> if not (List.mem s l_bound) then
                   try MVar (List.assoc s assoc, o) with
                   | Not_found -> expr
                  else
                   expr
       | MGlob_id(_)
       | MInt _
       | MString _
       | MCaml_def _ -> expr in
    aux [];;

(* type of proposition *)
type proposition =
  | PUniv of string * typ * proposition
  | PEx   of string * typ * proposition
  | PAnd  of proposition * proposition 
  | POr   of proposition * proposition
  | PImp  of proposition * proposition
  | PEq   of proposition * proposition
  | PNot  of proposition 
  | PCall of myexpr;;

type name_and_prop = string * proposition;;

(* constructors for proposition type *)
let puniv v p = PUniv(v,TAtom(None, "Self"),p);;
let pex   v p = PEx(v,TAtom(None, "Self"),p);;
let pand p1 p2 = PAnd(p1,p2);;
let por p1 p2 = POr(p1,p2);;
let pimp p1 p2 = PImp(p1,p2);;
let peq p1 p2 = PEq(p1,p2);;
let pnot c = PNot c;;
let pcall c = PCall c;;

let get_forall_types =
  let rec aux cumul p =
    match p with
    | PUniv(_,t,p) -> aux (if List.mem t cumul then cumul else t::cumul) p
    | _ -> cumul in
    aux [];;

(* Operations on pre-conditions *)
(* My own types for elementary type (after rewriting) *)
type pre_cond = myexpr list;; (* la conjonction des
                                     différents prédicats pre-conditions *)
let precond_null : pre_cond = [];;
let create_precond (e: myexpr) : pre_cond = [e];;
let add_precond (e: myexpr) (r:pre_cond) : pre_cond = e::r ;; 
let list_of_precond : pre_cond -> myexpr list = fun i -> i;;

(* Operations on conclusion *)
type conclusion = myexpr list;; (* la dijonction des différents prédicats
                                      conclusion *)
let conclusion_null  : conclusion = [];;
let create_conclusion  (e: myexpr) : conclusion = [e];;
let add_conclusion  (e:myexpr) (r:conclusion)  = e::r;; 
let list_of_conclusion : conclusion -> myexpr list = fun i -> i;;

(* Operations on variable *)
type variable = string * typ;;
let create_variable (v : string) (t: typ) : variable = v,t;;
let get_variable_name ((n,_) : variable) = n;; 
let get_variable_type ((_,t) : variable) = t;;
let variable_ren (f : string -> string) ((n,t): variable) : variable = f n,t;;
let foc_string_of_variable ((n,t) : variable) : string =
   n ^ " in " ^ string_of_typ t;;

(* Operations on variables *)
type variables = variable list;;
let variables_nb : variables -> int = List.length;;
let variables_null : variables = [];;
let string_of_variables : variables -> string =
  let rec aux t =
  function
    | [] -> ":" ^ string_of_typ t ^ ")"
    | (n,t')::r -> if t' = t then
                 " " ^ n ^ aux t r
              else 
                ":" ^ string_of_typ t ^ ")" ^
                " (" ^ n ^ aux t' r
        in
  function
    | [] -> ""
    | (s,t)::r -> "(" ^ s ^ aux t r;;

let variables_get_type (vs : variables) (v : string) = try List.assoc v vs with | Not_found -> failwith "Internal Error :|";;

let variables_is_null (v : variables) = v = variables_null;;
let create_variables (v : variable) : variables = [v];;
let add_variable (v: variable) (lv : variables) : variables = v::lv;;
let variables_mem (v:string) (vars : variables) = List.mem_assoc v vars;;
let variables_map_esc (f : variable -> 'a) (vs: variables)  = List.map f vs;;
let variables_map (f : variable -> variable) (vs: variables) = List.map f vs;;
let variables_to_list (vs : variables) : (string * typ) list = vs;;
let rec variables_to_tuple_type (vs : variables) : Own_types.typ =
  match vs with
  | [] -> TAtom(None, foctunit)
  | (_,t)::[] -> t
  | (_,t)::r -> TProd(t,variables_to_tuple_type r);;
let rec variables_to_tuple : variables -> Own_expr.myexpr =
    fun vs -> 
  match vs with
  | [] -> expr_basic focunit []
  | (n, t)::[] -> expr_var_typ n t
  | (n, t)::r -> expr_basic foccrp [expr_var_typ n t; variables_to_tuple r];;

let foc_argsdef_of_variables (vars : variables) : string =
    if vars = [] then
      "(_unused_var in @UNIT)"
    else
      to_args foc_string_of_variable vars;;
let foc_argscall_of_variables (vars : variables) : string =
    if vars = [] then
      "(@VUNIT)"
    else
      to_args get_variable_name vars;;

(* Operation on elementary form *)
type elementaire = pre_cond * conclusion;; (* une forme elementaire est le
                                            couple pre-condition conclusion *)
type elementaires = elementaire list;;
let create_elementaire p c = (p,c);;
let elementaire_null = [],[];;
let map_elementaire_esc f (p,c) = (List.map f p),(List.map f c);;
let map_elementaire f (p,c) = (List.map f p),(List.map f c);;
let get_precond e = fst e;;
let get_conclusion e = snd e;;
let add_elem_precond    np (p,c) = (add_precond np p),c;;
let add_elem_conclusion nc (p,c) = p,(add_conclusion nc c);;
let map_elementaire_ren f vs (p,c) =
  let assoc = List.map (function (v,_t) -> let nv = f v in (v,nv)) vs in
  List.map (rename_expr_assoc assoc) p, List.map (rename_expr_assoc assoc) c;;
let string_of_elem vs elem =
  let pre = get_precond elem in
  let conclu = get_conclusion elem in
  let v_l = vs in
  let forall =
    match v_l with
      | [] -> ""
      | _e::_r -> "all " ^ (add_string_args v_l " "
                                                 (fun (e,t) -> "(" ^ e ^ ":" ^ string_of_typ t ^ ")")) ^
                ", " in
  let spre =
    match pre with
    | [] -> ""
    | _ -> (add_string_args pre " and " string_of_myexpr) ^ " -> " in
  let sconclu =
    match conclu with
    | [] -> "#True"
    | _  -> (add_string_args conclu " or " string_of_myexpr) in
   forall ^ spre ^ sconclu;;

(* Operations on normals forms *)
(* a normal form is a list of bounded variables and a list of elementary *)
type normal_forms =  string * variables * elementaire list
let create_normal_forms (s:string) (v : variables) (e:elementaire list) = (s,v,e);;
let get_norm_variables ((_,v,_) : normal_forms) : variables = v;;
let get_norm_name ((n,_,_) : normal_forms) : string = n;;
let get_norm_elems ((_,_,e) : normal_forms) : elementaire list = e;;
let get_norm_nbvar ((_,v,_) : normal_forms) = List.length v;;
let get_norm_num_elems ((_,_,e) : normal_forms) = List.length e;;

(* map_norm_elementaire f norm
Rename all free variable x of norm to (f x).
f can return differents value when evaluate several time with the same variable
name (side-effect function) *)
let map_norm_ren f (n,vs,es) =
  let tmp = List.map (function (v,t) -> let nv = f v in (v,nv),(nv,t)) vs in
  let assoc,n_vs = List.split tmp in
  n,n_vs, List.map (map_elementaire (rename_expr_assoc assoc)) es;;

let map_norm_ren_assoc assoc ((n,vs,es) : normal_forms) =
  let n_vs = List.map (function ((v,t) as var) ->
                        try
                          let nv = List.assoc v assoc in
                            (nv,t)
                        with
                          | Not_found -> var) vs in
  n,n_vs, List.map (map_elementaire (rename_expr_assoc assoc)) es;;

let map_norm f (_n,vs,es) =
  List.map (f vs) es;;
