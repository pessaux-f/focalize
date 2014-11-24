open Parsetree;;
module PtU = Parsetree_utils;;

open Own_prop;;
open Own_types;;
open Own_expr;;
open Own_basics;;


exception No_module_opened;;

let print_debug _s =(*  print_string (_s);; *) ();;
(*   print_string ("focalize_inter : " ^ _s);; *)

type focalize_type = Parsetree.type_expr_desc;;

type focalize_expr = Parsetree.expr;;

type focalize_prop = Parsetree.logical_expr_desc;;
type focalize_species = Env.TypeInformation.species_description;;
type focalize_species_field = Env.TypeInformation.species_field;;
type focalize_species_param = Env.TypeInformation.species_param;;

type myfield = 
  | Signature of string * Own_types.typ
  | Definition of string * Parsetree.expr
  | RecDef     of myfield list
  | PropDef    of string * Own_prop.proposition
  | Property   of string * Own_prop.proposition
  | Theorem    of string * Own_prop.proposition;;

let add_path = Files.add_lib_path;;

let (set_env, get_env) =
  let env : Env.TypingEnv.t ref = ref (Env.TypingEnv.empty ()) in
  (fun m -> env := m),
  fun () -> !env;;

let (set_module, get_module) =
  let modu : string option ref = ref None in
  (fun m -> modu := Some m),
  (fun () -> match !modu with
             | None -> raise No_module_opened
             | Some t -> t
  );;


(* The definition of some exception raised inside this module *)

exception Bad_parameters of string;;
exception Bad_parameters_numbers of int * int

exception Cant_convert_typ of string;;
exception Cant_convert_texpr;;

exception Property_doesnt_exists of string;;
exception Function_doesnt_exists of string;;
exception Species_dont_exists of string;;
exception Rep_not_defined of string;;

exception Not_a_concrete_type of typ option;;
exception Not_a_property of string;;
exception Not_defined of string * (string list);;

exception Type_dont_exists of string;;


(** Some useful definitions :  *)
let locnone = Location.none;;
let get_dummy_ast a = { ast_loc = locnone;
                        ast_desc = a;
                        ast_annot = [];
                        ast_type = ANTI_none};;
let get_uident u = get_dummy_ast (I_local (Vuident u));;
let get_lident l = get_dummy_ast (I_local (Vlident l));;
let get_global_lident m l = get_dummy_ast (I_global (Qualified(m, Vlident l)));;

let extract_vname v =
  match v with
  | Vlident v -> v
  | Vuident v -> v
  | Vpident v -> v
  | Viident v -> v
  | Vqident v -> v;;

let convert_vname_m m v =
  match v with
  | Vlident v -> Prefix(m, v)
  | Vuident v ->
      if v = "::" then
        Infix "::"
      else
        Prefix(m, v)
  | Vpident v -> Prefix(m, v)
  | Viident v -> Infix v
  | Vqident v -> Prefix(m, v);;

let convert_vname v = convert_vname_m None v;;

let extract_qname v =
  match v with
  | Vname vn -> extract_vname vn
  | Qualified(_m,vn) -> extract_vname vn;;

let convert_qname v =
  match v with
  | Vname vn -> convert_vname vn
  | Qualified(m,vn) -> convert_vname_m (Some m) vn;;

let extract_snd_qname v =
  match v with
  | Qualified(_,vn) | Vname vn -> extract_vname vn

let extract_ident i =
  match i with
  | I_local v -> extract_vname v
  | I_global v -> extract_qname v;;

let convert_ident i =
  match i with
  | I_local v -> convert_vname v
  | I_global v -> convert_qname v;;

(* Convert an identifier whithout the indication of infix/prefix.
   Thsi function is used for the type translation *)
let convert_ident_no =
  let convert_vname_no v =
    match v with
    | Vuident v | Vpident v | Viident v
    | Vqident v | Vlident v -> v in
  let convert_qname_no v =
    match v with
    | Vname vn -> None, convert_vname_no vn
    | Qualified(m,vn) -> Some m, convert_vname_no vn in
  fun i ->
    match i with
    | I_local v -> None, convert_vname_no v
    | I_global v -> convert_qname_no v;;




let type_simple_of_typ t =
  let i = Types.make_type_constructor in
  let rec aux t =
  match t with 
  | TAtom (Some m, s) -> Types.type_basic (i m s) []
  | TAtom (None, s) -> Types.type_basic (i "basics" s) []
  | TProp -> Types.type_prop ()
  | TSpecPrm s ->
      Types.type_rep_species ~species_module: "basics" ~species_name: s
  | TFct (t1, t2) -> Types.type_arrow (aux t1) (aux t2)
  | TProd (t1, t2) -> Types.type_tuple [aux t1; aux t2]
  | TPrm (Some m, n, t_l) -> Types.type_basic (i m n) (List.map aux t_l)
  | TPrm (None, n, t_l) -> Types.type_basic (i "basics" n) (List.map aux t_l) in
  aux t
;;



let extract_snd_ident i =
  match i with
  | I_local v -> extract_vname v
  | I_global v -> extract_snd_qname v;;


(* Convertion of types *)

let rec string_of_type_simple =
  let aux = string_of_type_simple in
  let fvar () = "Var" in
  let farrow t1 t2 = aux t1 ^ " -> " ^ aux t2 in
  let ftuple l =
    "(" ^ add_string_args l ", " aux ^ ")" in
  let fsum l = "{" ^ add_string_args l ", " aux ^ "}" in 
  let fconstruct _ tn args =
    tn ^ "(" ^ add_string_args args ", " aux ^ ")" in
  let frep () = "Self" in
  let fprop () = "prop" in
  let fspecrep _s1 s2 = s2 in
  fun t ->
    Types.extract_type_simple fvar farrow ftuple fsum fconstruct fprop frep fspecrep t;;

let rec typ_of_type_simple t =
  let fvar = fun () -> failwith "typ_of_type_simple: variable type met" in
  let farrow t1 t2 = TFct(typ_of_type_simple t1, typ_of_type_simple t2) in
  let ftuple l =
    match l with
    | [t1; t2] -> TProd(typ_of_type_simple t1, typ_of_type_simple t2)
    | _ -> failwith "typ_of_type_simple: tuple > 2" in
  let fsum _l = failwith "typ_of_type_simple: sum" in
  let fconstruct m tn args = if args = [] then TAtom(Some m, tn) else
                           TPrm(Some m, tn, List.map typ_of_type_simple args) in
  let frep () = TAtom(None, focself) in
  let fprop () = TProp in
  let fspecrep _s1 s2 = TSpecPrm(s2) in
  Types.extract_type_simple fvar farrow ftuple fsum fconstruct fprop frep fspecrep t;;

exception Not_a_constructor;;

let split_constructor t =
  let aux2 t =
    let ferr _ = raise Not_a_constructor in
    let fsum l = l in
    Types.extract_type_simple ferr ferr ferr fsum ferr ferr ferr ferr t in
  let aux t =
    let fnone _ = ([],t) in
    let fnone2 _ = fnone in
    let fnone3 _ = fnone2 in
    let farrow t1 t2 = (aux2 t1, t2) in
    Types.extract_type_simple fnone farrow fnone fnone fnone3 fnone fnone fnone2 t in
  aux t;;

let split_applied_type t =
  let fnone _ = raise (Not_a_concrete_type None) in
  let fconstruct m n t_l = (Types.make_type_constructor m n, t_l) in
  Types.extract_type_simple fnone fnone fnone fnone fconstruct fnone fnone fnone t;;


let typ_of_type_scheme t =
  typ_of_type_simple (snd (Types.scheme_split t)) ;;

(** focalize_open_module m

Open the module [m].
*)

let open_module (_m : Parsetree.module_name) =
(*   get_env ();; *)
  Env.type_open_module ~loc:Location.none _m (Env.TypingEnv.empty ());; 

let open_module_cumul (m : Parsetree.module_name) =
  let env = get_env () in
  let nenv = Env.type_open_module ~loc:Location.none m env in
  set_env nenv;;

(* ************************************************************************** *)
(*                       Species manipulation functions                       *)
(* ************************************************************************** *)

(** focalize_get_species_list module

Gets the list of species defined in module [module].
*)
let focalize_get_species_list m =
  List.map extract_vname (Env.get_species_list (open_module m));;

(** focalize_get_species_def m s 

Gets the definition of the species [s] in module [m].
*)
let focalize_get_species ((ms, s) : species_name) =
  try
    let m = open_module ms in
      Env.TypingEnv.find_species ~loc:locnone ~current_unit:"" (get_uident s) (m)
  with
  | Env.Unbound_species(_,_) -> raise (Species_dont_exists (ms ^ "#" ^ s));;

module ET = Env.TypeInformation;;

let focalize_get_fields s =
  s.ET.spe_sig_methods;;


(** [focalize_get_functions_list sn]

Get the list of functions in species [sn].
*)
let focalize_get_functions_list =
  let rec flatten_def l tail =
    match l with
    | [] -> tail
    | ET.SF_let_rec((first :: others))::r ->
        flatten_def (ET.SF_let_rec((others))::r) (first::tail)
    | ET.SF_let_rec([])::r ->
        flatten_def r tail
    | ET.SF_let(info)::r ->
        flatten_def r (info::tail)
    | _::r -> flatten_def r tail in
  fun sn ->
    let ss = focalize_get_species sn in
    flatten_def ss.ET.spe_sig_methods [];;

(** [focalize_get_properties_list sn]

Get the list of properties in species [sn].
*)
let focalize_get_properties_list =
  let rec flatten_def l tail =
    match l with
    | [] -> tail
    | (ET.SF_theorem((_,n,l,p,_,_)))::r 
    | (ET.SF_property((_,n,l,p,_)))::r ->
        if l <> [] then
          failwith "focalize_get_properties_list: non empty list, what does it mean ???"
        else
          flatten_def r ((n,l,p)::tail)
    | _e::r -> flatten_def r tail in
  fun s ->
    let ss = focalize_get_species s in
    flatten_def ss.ET.spe_sig_methods [];;

(** focalize_get_representation s

Gets the carrier type of species [s].

*)
let focalize_get_representation sn =
  let rec get_rep l =
    match l with
    | [] -> raise (Rep_not_defined (string_of_species_name sn))
    | (ET.SF_sig(param))::_r ->
        let (_,_s,t) = param in 
          typ_of_type_scheme t
    | _e::r -> get_rep r in
  let s = focalize_get_species sn in
  get_rep s.ET.spe_sig_methods;;

(** Take a species name and a parameter name.
Returns the number of the parameter *)
let get_parameters_number species a =
  let spec = focalize_get_species species in
  let rec aux l n =
    match l with
    | [] -> raise Not_found
    | e::r ->
        (match e with
        |  ET.SPAR_in(v,(_,_),_) ->
            if extract_vname v = a then
              failwith "Error get_parameters_number"
            else
             aux r (n + 1)
        | ET.SPAR_is((_,e),_,_,_,_) ->
(*
            print_string (e ^ " ");
            print_newline ();
*)
            if e = a then
              n
            else
             aux r (n + 1)
        ) in
 try aux spec.ET.spe_sig_params 0 with
 | Not_found -> failwith "Internal error :)";;

(** raise Err_types.Def_err(_) *)
let get_parameters species =
  let spec = focalize_get_species species in
  let convert_type r =
    match r with
    | PtU.SPE_Self -> "Self"
    | PtU.SPE_Species((qvn, _)) -> extract_snd_qname qvn
    | PtU.SPE_Expr_entity _e -> failwith "get_parameters"
    in
    List.map (function
                  ET.SPAR_in(e,t,_) ->
(*                     let s,l = convert_type t in  *)
                      create_prmexpent (extract_vname e) (TAtom(Some (fst t), snd t))
                | ET.SPAR_is((_,e),_,_,t,_) ->
                    let l = List.map convert_type t.PtU.sse_effective_args in
                      create_prmexpcoll e (extract_snd_ident t.PtU.sse_name.ast_desc,l)
             )
             spec.ET.spe_sig_params;;


(******************************************************************************)
(**** Obtain the lists of module used by the functions of the species in m ****)
(******************************************************************************)

let focalize_get_ident_modules_dep i l=
  match i.ast_desc with
  | I_local _ -> l 
  | I_global (Vname _) -> l
  | I_global (Qualified(m, _)) -> m::l;;
  
let focalize_get_expr_ident_modules_dep i l =
  match i.ast_desc with
  | EI_local _ -> l
  | EI_global (Vname _) -> l
  | EI_global (Qualified(m, _)) -> m::l
  | EI_method (None, _) -> l
  | EI_method (Some (Vname _), _) -> l
  | EI_method (Some (Qualified(m, _)), _) -> m::l;;
  
let focalize_get_constructor_ident_modules_dep i l =
  match i.ast_desc with
  | CI c -> focalize_get_ident_modules_dep c l;;
  
let rec focalize_get_pat_modules_dep p l =
  let rec get p l =
    match p with
    | P_const _ -> l
    | P_var _ -> l
    | P_as(p, _) ->
        focalize_get_pat_modules_dep p l
    | P_wild -> l
    | P_constr(ci, p_l) ->
        get (P_tuple p_l)
            (focalize_get_constructor_ident_modules_dep ci l)
    | P_record [] -> l
    | P_record ((l',p):: _r) ->
        (match l'.ast_desc with
        | LI i ->
            focalize_get_ident_modules_dep
            i 
            (focalize_get_pat_modules_dep p l) 
        )
    | P_tuple [] -> l
    | P_tuple(e::r) ->
        get (P_tuple r)
            (focalize_get_pat_modules_dep e l)
    | P_paren p -> focalize_get_pat_modules_dep p l in
  get p.ast_desc l;;

let focalize_get_label_ident_modules_dep li l =
  match li.ast_desc with
  | LI i ->
      focalize_get_ident_modules_dep i l;;

let focalize_get_binding_modules_dep b l =
  let rec get_bs bs l =
    match bs with
    | [] -> l
    | e::r -> get_bs r (get_b_ e l)
  and get_ e l =
    get e.ast_desc l 
  and     get e (l : Parsetree.module_name list) =
    match e with
    | E_self | E_const _ -> l
    | E_var v -> focalize_get_expr_ident_modules_dep v l
    | E_constr(c,l') ->
        get (E_tuple l')
            (focalize_get_constructor_ident_modules_dep c l)
    | E_match(e1,[]) -> get_ e1 l
    | E_match(e1,(p,e)::l') -> 
        get (E_match(e1,l'))
            (get_ e 
                 (focalize_get_pat_modules_dep p l)
            )
    | E_let(ld,_e) -> get_bs ld.ast_desc.ld_bindings l
    | E_tuple(e::r) -> get (E_tuple r) (get_ e l)
    | E_tuple([]) -> l
    | E_if(e1, e2, e3) -> get_ e3 (get_ e2 (get_ e1 l))
    | E_app(e,l') -> get (E_tuple l') (get_ e l)
    | E_paren(e) -> get_ e l
    | E_fun(_,e) -> get_ e l
    | E_record([]) -> l
    | E_record((li,e)::l') -> 
        get_ e 
            (focalize_get_label_ident_modules_dep li
                                                  (get (E_record(l')) l)
            )
  | E_record_access(e,li) ->
      get_ e (focalize_get_label_ident_modules_dep li l)
  | E_record_with(e,li_e_l) ->
     get_ e (get (E_record li_e_l) l)
  | E_external _ -> l
  | E_sequence e ->
     get (E_tuple e) l
  and get_b_ b l =
     get_b b.ast_desc.b_body l
  and get_b b l =
  match b with
  | BB_logical _ -> l
  | BB_computational e -> get_ e l in
  get_b b l;;

let focalize_get_f_modules_dep (_, _, _, _t, b, _, _, _) l =
  focalize_get_binding_modules_dep b l;;

let focalize_get_s_modules_dep (sn : species_name) l =
  let rec get f l =
    match f with
    | [] -> l
    | e::r ->
      get r (focalize_get_f_modules_dep e l) in
  let f = focalize_get_functions_list sn in
  get f l;;

let focalize_get_m_modules_dep m =
  let rec get_fs f l =
    match f with
    | [] -> l
    | e::r ->
        get_fs r (focalize_get_s_modules_dep (m, e) l) in
  let l = focalize_get_species_list m in
  get_fs l [];;




(******************************************************************************)
(******************************************************************************)
(******************************************************************************)

let focalize_get_all_open () =
  let rec local_merge current news olds l =
    match l with
    | [] -> (news, current::olds)
    | e::r ->
        if List.mem e (current::olds) then
          local_merge current news olds r
        else
          local_merge current (e::news) olds r in
  let rec aux (news, olds) =
    match news with
    | [] -> olds
    | e::r ->
        if List.mem e olds then
          aux (r, olds)
        else
          let l = focalize_get_m_modules_dep e in
          aux (local_merge e r olds l) in
  aux ([get_module ()], []);;

(******************************************************************************)
(*                        Functions for concrete types                        *)
(******************************************************************************)

let constructor_ident_of_constructor_name (n : constructor_name) =
  get_dummy_ast (CI (get_dummy_ast (I_local n)));;

let get_constructor_name c =
  match c.ast_desc with
  | CI o -> extract_ident o.ast_desc;;

(** Get all constructors defined in the current environment *)
let focalize_get_all_constructors =
  let get_constr_def m cn =
    let ci = constructor_ident_of_constructor_name cn in
    (cn, Env.TypingEnv.find_constructor ~loc:locnone ~current_unit:"" ci m) in
  fun m ->
  let all_modules = focalize_get_all_open m in
  List.fold_left
      (fun s e ->
        let opened = open_module e in
        let constr = Env.get_constructor_list opened in
         List.map (get_constr_def opened) constr @ s
      ) [] all_modules;;

let is_constructor (i : ident) =
  let c = ident_name i in
  List.mem c (List.map (fun (e, _) -> extract_vname e)
                     (focalize_get_all_constructors ())
             );;

(** Get the constructors of a special type *)
let get_constructors_of_a_type t =
  let rec is_ok typ  =
    let nok _ = false in
    let nok2 _ _ = false in
    let farrow _t1 t2 = is_ok t2 in
    let fconstruct _ tn _args = (tn = t) in
      Types.extract_type_simple nok farrow nok nok fconstruct nok nok nok2 typ in
    let cons = focalize_get_all_constructors () in
    let pe = List.filter (fun r -> is_ok (Types.specialize (snd r).ET.cstr_scheme)) cons in
    List.fold_left (fun s e -> if List.mem e s then s else e::s) [] pe;;

(** Construct a generic type for a constructor of arity lenght of [i]
Example :
  [get_constructor_scheme [e1; e2; e3] t ] gives
    'a -> 'b -> 'c -> t
useful when the constructor as type 
    'a -> int -> float -> t('b,'a).
If we want to instanciate the constructor for t(float,string) we have to unify
it with 
    'a -> 'b -> 'c -> t(float, string)
*)
let get_constructor_scheme i right =
  if i = [] then
    right
  else
    begin
      Types.begin_definition ();
      let l = List.map (fun _e -> Types.type_variable ()) i in
      Types.end_definition ();
      Types.type_arrow (Types.type_sum_arguments l) right
    end;;

(** Get the definition (list of constructor) of a concrete type when the
 parameters are instanciated *)
let get_concrete_def : typ -> constructor list = 
  let get_simple e = Types.specialize e.ET.cstr_scheme in
  fun concret ->
    match concret with
    | TPrm(_m, s,t_l) ->
        let constructors = get_constructors_of_a_type s in
        begin
          match constructors with
          | []   -> (* print_string ("No constructor for " ^ string_of_typ
          concret ^ "\n"); *)
                    raise(Not_a_concrete_type(Some concret))
          | (_,e)::_r ->
              let right =
                let args = List.map type_simple_of_typ t_l in
                let _, scheme = split_constructor (get_simple e) in
                let left, _ = split_applied_type scheme in
                Types.type_basic left args in
              let convert (name, desc) =
                let args, _ = split_constructor (get_simple desc) in
                let cons_scheme = get_constructor_scheme args right in
                let result = 
                  try Types.unify ~loc:locnone ~self_manifest:None 
                                  (Types.specialize desc.ET.cstr_scheme)
                                  (cons_scheme)
                  with
                  | Types.Conflict (t1,t2,_) ->
                      Format.pp_print_string Format.std_formatter "Erreur d'unification : ";
                      Types.pp_type_simple Format.std_formatter t1;
                      Format.pp_print_string Format.std_formatter "and ";
                      Types.pp_type_simple Format.std_formatter t2;
                      Format.pp_print_newline Format.std_formatter ();
                      exit 54
                in
                let typs,_ = split_constructor result in
                convert_vname name,
                List.map typ_of_type_simple typs in
              List.map convert constructors
        end 
    | _ -> raise (Not_a_concrete_type None);;

let focalize_get_all_types () = 
  let qualify m e = Qualified(m,e) in
  let all_modules = focalize_get_all_open () in
  List.fold_left
     (fun s e ->
       let opened = open_module e in
       List.map (qualify e) (Env.get_type_list opened) @ s
     ) [] all_modules;;

(** Get the constructors of a special type *)
let get_type_arity t : int option =
  let get_simple e = Types.specialize e.ET.cstr_scheme in
  let rec is_ok typ  =
    let nok _ = false in
    let nok2 _ _ = false in
    let farrow _t1 t2 = is_ok t2 in
    let fconstruct _ tn _args = (tn = t) in
      Types.extract_type_simple nok farrow nok nok fconstruct nok nok nok2 typ in
    let cons = focalize_get_all_constructors () in
    match List.filter 
            (fun r ->
               is_ok (Types.specialize (snd r).ET.cstr_scheme)
            )
            cons
    with
    | [] -> None
    | (_, d)::_ ->
        let _, scheme = split_constructor (get_simple d) in
        let _, r = split_applied_type scheme in
        Some (List.length r);;

let get_new_variable =
  let a = Char.code 'a' in
  let l = ref [0] in
  let rec incr l =
    match l with
    | [] -> [0]
    | e::r -> if e >= 25 then 0::(incr r) else (e+1)::r in
  let update_l () = l := incr !l in
  let rec get_string l =
    match l with
    | [] -> "" | e::r -> String.make 1 (Char.chr (a + e)) ^ get_string r in
  fun () ->
    update_l ();
    get_string !l;;

let rec get_variables i =
  if i <= 0 then [] else (get_new_variable ())::get_variables (i-1);;

let get_a_type_body (t : Parsetree.qualified_vname) : Own_types.typ_body option =
  let rec aux l t =
    match l with
    | [] -> Type t
    | e::r -> TParam(e, aux r t) in
  let n = extract_qname t in
  let i = get_type_arity n in
  match i with
  | None -> None
  | Some i ->
      let l = get_variables i in
      let t = TPrm(None, n, List.map (fun e -> TAtom(None, e)) l) in
      let c = get_concrete_def t in
      Some (aux l c);;

let get_all_types () =
  let types = focalize_get_all_types () in
  List.fold_left (fun s e ->
                    match get_a_type_body e with
                    | None -> s
                    | Some t -> (extract_qname e , t)::s
                 ) [] types;;


(* ************************************************************************* *)
(*                        End of concrete type functions                     *)
(* ************************************************************************* *)

let rec typ_of_foctyp =
  function
    | TE_ident (s) ->
        let m,s = convert_ident_no s.ast_desc in
        if Char.lowercase (String.get s 0) = String.get s 0 then
          TAtom(m, s) 
        else
          TSpecPrm(s)
    | TE_fun(e1,e2) -> TFct(typ_of_foctyp e1.ast_desc,
                            typ_of_foctyp e2.ast_desc)
    | TE_prod([e1;e2]) -> TProd(typ_of_foctyp e1.ast_desc,
                                typ_of_foctyp e2.ast_desc)
    | TE_prod(_) -> failwith "typ_of_foctyp : prod not yet implemented"
    | TE_app(n,t_l) ->
        let m, s = convert_ident_no n.ast_desc in
        TPrm(m, s, List.map (fun e -> typ_of_foctyp e.ast_desc) t_l)
    | TE_self -> TAtom(None, "Self")
        (**)
    | TE_prop  -> raise (Cant_convert_typ "Prop")
    | TE_paren t -> typ_of_foctyp t.ast_desc

let rec pattern_of_tpattern p =
  let aux p =
    match p.ast_desc with
    | P_var(Vlident s) -> Some s
    | P_var(_) -> failwith("Hahah")
    | P_wild -> None 
    | P_as(_, _) -> failwith "Bad pattern : P_as"
    | P_constr(_, _) -> failwith "Bad pattern : P_constr" 
    | P_const _ -> failwith "Bad pattern : P_const" 
    | P_record _ -> failwith "Bad pattern: P_record" 
    | P_tuple _ -> failwith "Bad pattern: P_tuple" 
    | P_paren _ -> failwith "Bad pattern: P_paren" 
    in
  match p.ast_desc with
  | P_tuple _  -> failwith "Bad pattern: P_tuple"
  | P_record _
  | P_wild | P_as(_,_)
  | P_const _ | P_var _ ->
      failwith "Bad pattern"
  | P_paren _t -> pattern_of_tpattern p
  | P_constr(c,ps) ->
      convert_ident (match c.ast_desc with CI t -> t).ast_desc,
      List.map aux ps;;

(*
let search_binding n let_def =
  let rec aux l =
  match l with
  | [] -> None
  | e::r ->
    if extract_vname e.ast_desc.b_name = n then
        match e.ast_desc.b_body with
       | BB_logical _ -> failwith "search_binding: logical"
       | BB_computational ee ->
           Some (List.fold_right
             (function (n,Some t) -> (fun s -> MFun(extract_vname n,
                                               Some (typ_of_foctyp t.ast_desc), s))
              |        (n,None) -> (fun s -> failwith "search_binding: no type")
             )
             e.ast_desc.b_params (myexpr_of_texpr ee))
      else
        aux r
  in
  aux let_def.ast_desc.ld_bindings;;
*)
let get_ast_type e =
  match e.ast_type with
  | ANTI_irrelevant
  | ANTI_none -> None (* failwith "Lack of type information" *)
  | ANTI_type t -> Some(typ_of_type_simple t)
  | ANTI_scheme _s -> failwith "Type scheme found";;

(** A function converting a focal expression to my expression *)
let rec myexpr_of_texpr m_name bv texpr : Own_expr.myexpr =
  let add_s_o s_o l =
    List.fold_left (fun s -> (function None -> s | Some t -> t::s)) l s_o in
  let get_typ t =
    match t.ast_type with
    | ANTI_type t -> let t = typ_of_type_simple t in t
    | _ -> failwith "myexpr_of_texpr: get_typ" in
  let split_function_typ t =
    match t with
    | TFct(t1, t2) -> (t1, t2)
    | _ -> failwith "myexpr_of_texpr: Not a functional type (2)" in
  let expr_typ bv e =
    myexpr_of_texpr m_name bv e,
    match e.ast_type with
    | ANTI_irrelevant 
    | ANTI_none -> failwith "myexpr_of_texpr: we require a type information"
    | ANTI_type t -> let t = typ_of_type_simple t in t
    | ANTI_scheme t -> typ_of_type_scheme t in
  let rec aux bv expr : Own_expr.myexpr =
  match expr.ast_desc with
  | E_constr(s, e_l) ->
      let n = match s.ast_desc with CI i -> i in
      let id =
        match n.ast_desc with
        | I_global (Vname v) 
        | I_local v -> MGlob_id (convert_vname v)
        | I_global (Qualified(m, v)) ->
            MGlob_id(convert_vname_m (Some m) v) in
      if e_l = [] then
        id
      else
        let args = List.map (expr_typ bv) e_l in 
        let res = MApp(id, None (* Some (get_typ s) *), args) in
          res
  | E_const { ast_desc = C_bool s } ->
    ( match s with
     | "true" -> expr_glob foctrue
     | "false" -> expr_glob focfalse
     | _ -> failwith "myexpr_of_texpr: bad bool"
    )
  | E_const { ast_desc = C_int r } -> MInt (int_of_string r)
  | E_const { ast_desc = C_string s } -> MString s
  | E_var x ->
     (match x.ast_desc with
      | EI_local vn -> 
          if List.mem (extract_vname vn) bv then
            expr_var_typ (extract_vname vn) (get_typ x)
          else
            let n = extract_vname vn in
(*             print_debug ("nom de la variable :  " ^ n ^ "\n"); *)
            MVar(n, Some (get_typ x))
      | EI_global qn ->
         (match qn with
          | Vname vn -> MGlob_id(convert_vname vn)
          | Qualified(m,vn) -> MGlob_id(convert_vname_m (Some m) vn)
         )
      | EI_method (qno,vn) ->
(*           let n = extract_vname vn in *)
(*           print_debug ("nom de la methode :  " ^ n ^ "\n"); *)
         (match qno with
          | None -> MMeth(None, extract_vname vn)
          | Some(Vname vn2) ->
              MMeth(Some (extract_vname vn2), extract_vname vn)
          | Some(Qualified(_m,vn2)) ->
              MMeth(Some (extract_vname vn2), extract_vname vn)
(*
              failwith ("Can't call a method of others module collection" ^ m  ^
              "\n")
*)
(*               MMeth((m, convert_vname vn2), convert_vname vn) *)
         )
     )
  | E_fun([], e) ->
      aux bv e
  | E_fun(l, e') ->
    let rec decompose_fun l t =
      match l with
      | [] -> aux bv e'
      | e::r ->
        let (t1,t2) = split_function_typ t in
        MFun(extract_vname e, Some t1, decompose_fun r t2) in
    decompose_fun l (get_typ expr)
 (* | TCase(e,l) -> MMatch(myexpr_of_texpr e *)
  (*
  | TVarloc(s,expr1,expr2) ->
      MVarloc(false,s,myexpr_of_texpr expr1,aux expr2 typ)
  *)
  | E_if(c,e1,e2) ->
      MIfte(aux bv c,
            aux bv e1,
            aux bv e2)
  | E_app(m,args) ->
     let args = List.map (expr_typ bv) args in 
     let res = MApp(aux bv m, Some (get_typ m), args) in
     res
  (*| TMeth(s,meth),_ -> MMeth(myexpr_of_texpr s, meth) *)
  | E_sequence _ -> failwith "myexpr_of_texpr: sequence"
  | E_record _ -> failwith "myexpr_of_texpr: record"
  | E_record_access _ -> failwith "myexpr_of_texpr: record_access"
  | E_record_with _ -> failwith "myexpr_of_texpr: record_with"
  | E_tuple([e1;e2]) -> MApp(expr_glob foccrp, None, [expr_typ bv e1; expr_typ bv e2])
  | E_tuple(_) -> failwith "myexpr_of_texpr: e_tuple"
  | E_external(_) -> failwith "myexpr_of_texpr: e_external"
  | E_paren(e) -> aux bv e
  | E_let(e1,e2) ->
      let e1' = try(List.hd e1.ast_desc.ld_bindings).ast_desc with | Not_found -> failwith "Internal Error :/" in
      (* Take into account of parameters *)
      let add_params =
          List.fold_right (fun e s ->
                             match e with
                             | vn, Some t -> (fun e -> MFun(extract_vname vn,
                                                            Some(typ_of_foctyp t.ast_desc), s e))
                             | vn, None -> failwith ("myexpr_of_texpr : E_let lack of type for " ^ extract_vname vn ^ " in local function " ^ extract_vname e1'.b_name)
                          ) e1'.b_params (fun e -> e)
      in
      let x = extract_vname e1'.b_name in
      let _x_t = e1'.b_type in
      let e1',tttt = match e1'.b_body with
               | BB_logical _ -> failwith "myexpr_of_texpr: logical E_let"
               | BB_computational e -> aux bv e, get_ast_type e in
(*       let tttt = get_ast_type e1 in *)
      (if tttt = None then failwith (string_of_myexpr e1'));
      MVarloc(false,(x, tttt), add_params e1', aux (x::bv) e2)
  | E_match(e,l) ->
      let tttt = get_ast_type e in
      let res = MMatch((aux bv e, tttt),
                       List.map
                         (fun (p,e) ->
                            let a,b = pattern_of_tpattern p in
                              a,b,aux (add_s_o b bv) e
                         ) l
                      ) in
(*       print_debug (dbg_string_myexpr res); *)
        res
                       (***)
  | _ -> raise Cant_convert_texpr in
    aux bv texpr;;


(* convert a Typed_elt.tprop to a proposition *)
let proposition_of_tprop m =
  let rec aux l =
  function
    | Pr_forall([], _, p) -> aux l p.ast_desc
    | Pr_forall(v::r, t, p) -> 
        let v = extract_vname v in
         PUniv(v, typ_of_foctyp t.ast_desc,
                  aux (v::l) (Pr_forall(r,t,p)))
    | Pr_exists([], _, p) -> aux l p.ast_desc
    | Pr_exists(v::r, t, p) -> 
        let v = extract_vname v in
         PEx(v, typ_of_foctyp t.ast_desc,
                aux (v::l) (Pr_exists(r,t,p)))
    | Pr_and (p1,p2) -> PAnd(aux l p1.ast_desc,aux l p2.ast_desc)
    | Pr_or (p1,p2) -> POr(aux l p1.ast_desc,aux l p2.ast_desc)
    | Pr_imply(p1,p2) -> PImp(aux l p1.ast_desc,aux l p2.ast_desc)
    | Pr_equiv(p1,p2) -> PEq(aux l p1.ast_desc,aux l p2.ast_desc)
    | Pr_not p -> PNot(aux l p.ast_desc)
    | Pr_expr e -> PCall(myexpr_of_texpr m l e)
    | Pr_paren e -> aux l e.ast_desc in
  aux [];;

type expr_or_logic =
| FExpr of  myexpr
| FLogic of proposition;;

let expr_or_logic_of_body =
  function
  | Parsetree.BB_logical p -> FLogic (proposition_of_tprop "" p.ast_desc)
  | Parsetree.BB_computational e -> FExpr (myexpr_of_texpr "" [] e);;


let get_prop_def species prop =
  Whattodo.print_verbose ("Getting prop : " ^ prop ^ "...\n");
  let rec search_prop nn l =
    match l with
    | [] ->
        raise (Property_doesnt_exists prop)
    | (ET.SF_theorem((_,n,l,p,_,_)))::r 
    | (ET.SF_property((_,n,l,p,_)))::r ->
        if extract_vname n = nn then
          if l <> [] then
            failwith "get_prop_def: non empty list, what does it mean ???"
          else
            p
        else
          search_prop nn r
    | _e::r -> search_prop nn r in
  let ss = focalize_get_species species in
    proposition_of_tprop (fst species) (search_prop prop ss.ET.spe_sig_methods).ast_desc
;;

let rec add_meth_params p t e =
  match p, t with
  | p::r, TFct(t1,t2) ->
      let expr, typ_final = add_meth_params r t2 e in 
      MFun(p, Some t1, expr), typ_final
  | [], _ ->
      e, t
  | _, _ -> failwith "add_meth_params: no enough types";;

let get_meth_def sn meth =
  Whattodo.print_verbose ("Getting function : " ^ meth ^ "...\n");
  let rec search_meth nn l =
    match l with
    | [] ->
        raise (Function_doesnt_exists meth)
    | ET.SF_let_rec((tt::p))::r ->
        (try search_meth nn [ET.SF_let tt] with
         | Function_doesnt_exists _ ->
             search_meth nn (ET.SF_let_rec(p)::r)
        )
    | ET.SF_let((_,n,n_l, m, b, _, _, _))::r ->
        let type_parms = typ_of_type_scheme m in
        if extract_vname n = nn then
          match b with
          | BB_logical _ -> failwith "get_meth_def: logical"
          | BB_computational e ->
              let n_l = List.map extract_vname n_l in
              let def = myexpr_of_texpr (fst sn) n_l e in
              add_meth_params n_l type_parms def
        else
          search_meth nn r
    | ET.SF_let_rec([])::r
    | _::r -> search_meth nn r in
  let ss = focalize_get_species sn in
  search_meth meth (ss.ET.spe_sig_methods)
;;

let is_concrete_type typ =
  let aux t m = 
     Env.TypingEnv.find_type ~loc:Location.none ~current_unit:"???" (get_lident t) m in
  match typ with
  | TAtom(None, s) | TPrm(None, s, _) ->
      begin
        try ignore (aux s (get_env ())); true with
        | Env.Unbound_type(_vn, _) -> false
      end
  | TAtom(Some m, s) | TPrm(Some m, s, _) ->
      begin
        try ignore (aux s (open_module m)); true with
        | Env.Unbound_type(_vn, _) -> false
      end
  | _ -> print_debug (string_of_typ typ ^ "Not a concrete typ\n") ; false;;


let get_type_kind tk =
  match tk with
  | ET.TK_abstract -> print_debug "Type abstrait"
  | ET.TK_external(yuio,_) -> (List.iter (fun t -> match fst(t) with EL_external t -> print_debug t |
  _ -> ()) yuio.ast_desc); print_debug "Type importé"
  | ET.TK_variant _ -> print_debug "type avec arguments"
  | ET.TK_record _ -> print_debug "Type enregistrement"
;;

(* Initialize the environnement *)
let focalize_init install_lib_dir m =
  set_module m;
  Files.add_lib_path install_lib_dir;
  ignore (open_module_cumul m)
;;

let get_rep = focalize_get_representation;;

let expr_for_prolog_of_texpr _ = failwith "expr_for_prolog_of_texpr: Not yet implemented" 
let typ_of_const _ = failwith "typ_of_const: Not yet implemented" 
let foctyp_of_typ _ = failwith "foctyp_of_typ: Not yet implemented" 
let is_complete_def s = s.ET.spe_is_closed;;

let is_complete m s = is_complete_def (focalize_get_species (m, s));;

let get_meth_def_split sn m =
  let def, typ = get_meth_def sn m in (* Get the definition and the type of m *)
(*   print_debug (dbg_string_myexpr def); *)
  let rec aux s =
    match s with
    | MFun(x, t, e) -> 
        let (args, e_t) = aux e in
        (x, t)::args, e_t
    | _ -> [], (s, typ) in
  aux def;;



let convert_field f =
  match f with
  | ET.SF_sig((_, name, typ)) ->
      Signature(extract_vname name, typ_of_type_scheme typ)
  | ET.SF_let((_, name, _, _, body, _, _, _)) ->
      begin
        match body with
        | BB_logical l ->
            PropDef(extract_vname name, proposition_of_tprop "" l.ast_desc)
        | BB_computational e -> Definition(extract_vname name, e)
      end
  | ET.SF_let_rec l ->
      RecDef
        (List.map
           (fun ((_, name, _, _, body, _, _, _)) ->
             match body with
             | BB_logical l ->
                 PropDef
                   (extract_vname name, proposition_of_tprop "" l.ast_desc)
             | BB_computational e -> Definition (extract_vname name, e))
           l)
  | ET.SF_theorem (_, name, _, body, _, _) ->
      Theorem (extract_vname name, proposition_of_tprop "" body.ast_desc)
  | ET.SF_property (_, name,_ ,body, _) ->
      Property (extract_vname name, proposition_of_tprop "" body.ast_desc)
;;



let is_collection spc =
 match spc.ET.spe_kind with
  | Types.SCK_toplevel_collection -> true
  | Types.SCK_toplevel_species    -> false
  | Types.SCK_species_parameter   -> false (* ? *)
;;


let focalize_get_params spec = spec.ET.spe_sig_params;; 

let focalize_param_is_ent param =
  match param with
  |  ET.SPAR_in(_,(_,_),_) -> true
  | ET.SPAR_is((_,_),_,_,_,_) -> false;;


let focalize_get_param_name param =
  match param with
  |  ET.SPAR_in(v,(_,_),_) -> extract_vname v
  | ET.SPAR_is((_,e),_,_,_,_) -> e;;

let focalize_get_param_type param =
  match param with
  |  ET.SPAR_in(_,(_modul, v),_) ->(* (if modul = current then "" else modul ^
  "." ) ^ *) v
  | ET.SPAR_is((_,_),_,_, p ,_) -> extract_ident p.Parsetree_utils.sse_name.ast_desc;;

  (*
let get_param_col_type param =
  match param with
  |  ET.SPAR_in(v,(_,_),_) -> extract_vname v
  | ET.SPAR_is((_,e),_,_,_,_) -> e
  *)
