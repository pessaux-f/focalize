open Own_types;;
open Own_basics;;

type species_name = string * string;;
                                 (** a couple of module name * species name *)

let string_of_species_name ((m,s) : species_name) =
(*
  match m with
  | None -> s
  | Some m ->
*)
      m ^ "#" ^ s;;

let create_species_name m s = (m,s : species_name);;

(* My expressions *)
type myexpr =
  | MIfte of myexpr * myexpr * myexpr
  | MApp of myexpr * ((myexpr * typ) list)
  | MMeth of string option * string
  | MFun of string * typ option * myexpr
  | MVar of string * typ option
  | MMatch of (myexpr * typ option) * (ident * string option list * myexpr) list
  | MInt of int
  | MString of string
  | MVarloc of bool * (string * typ option) * myexpr * myexpr
  | MGlob_id of ident
  | MCaml_def of string
;;


(* some convenient constructors *)

let expr_meth sn meth l = MApp( MMeth(Some sn,meth),
                             List.map (fun e -> e, Own_types.TAtom(None, "unit")) l);;
let expr_meth2 sn meth l = MApp( MMeth(Some sn, meth),l);;
let expr_basic meth l = MApp( MGlob_id(meth),
                             List.map (fun e -> e, Own_types.TAtom(None, "unit")) l);;
let expr_basic2 meth l = MApp( MGlob_id(meth), l );;
let expr_string s = MString s;;
let expr_match e t c = MMatch((e, Some t),c);;
let expr_match_notyp e c = MMatch((e, None),c);;
let expr_app e l = MApp(e, List.map (fun e -> e , Own_types.TAtom(None, "unit")) l);;
let expr_app2 e l = MApp(e, l);;
let expr_int i = MInt i;;
let expr_fun s typ e = MFun(s, Some typ, e);;
let expr_fun_notyp s e = MFun(s,None,e);;
let expr_if c e1 e2 = MIfte(c,e1,e2);;
let expr_var id = MVar(id, None);;
let expr_var_typ id t = MVar(id, Some t);;
let expr_glob s = MGlob_id(s);;
let expr_caml s = MCaml_def s;;
let expr_let s t e1 e2 = MVarloc(false, (s, Some t),e1,e2);;
let expr_let_notyp s e1 e2 = MVarloc(false, (s, None),e1,e2);;
let expr_seq t1 t2 b =
  if b then
(*    Focal has the same evaluate strategy than Ocaml (ie: right to left) *)
    expr_app (expr_glob (Prefix(None, "seq"))) [t2;t1]
  else
    expr_let_notyp "unused_var" t1 t2;;

let positif e = e;;
let negatif e = expr_app (expr_glob focnot) [e];;

let rec expr_of_caml_list l =
  match l with
  | [] -> expr_glob focnil
  | e::r -> expr_app (expr_glob foccons) [e;expr_of_caml_list r];;
let expr_of_caml_couple f1 f2 (e1,e2) =
  expr_app (expr_glob foccrp) [f1 e1; f2 e2];;
let expr_of_caml_bool b =
  expr_glob (if b then foctrue else focfalse);;

let rec expr_tuple_of_caml_list f l =
  match l with
  | [] -> expr_basic focunit []
  | n::[] -> f n
  | n::r -> expr_basic foccrp [f n; expr_tuple_of_caml_list f r];;

let string_of_string_option t =
  match t with
  | None -> ""
  | Some e -> e;;

let string_of_ident i =
  match i with
  | Prefix(None, s) -> "#" ^ s
  | Prefix(Some o, s) -> o ^ "#" ^ s
  | Infix s ->s ;;

(** Convert a term of type myexpr to a string *)
let rec string_of_myexpr =
      let rec aux e  =
        match e with
        | MGlob_id (s) -> string_of_ident s
        | MVar(s,_o) -> if s = focself then "" else s
        | MInt i -> string_of_int i
        | MString s -> "\"" ^ s ^ "\""
        | MFun (s, Some t,e) -> "fun (" ^ s ^ ":" ^ string_of_typ t ^ ") -> " ^ aux e
        | MFun (s, None,e) -> "fun " ^ s ^ " -> " ^ aux e
(*         | MFun (s,None,e) -> "fun (" ^ s ^ ") -> " ^ aux e *)
        | MVarloc (b, (s, _),e1,e2) -> "let " ^ (if b then "rec " else "") ^ s ^ " = " ^ aux e1 ^ " in " ^ aux e2
        | MIfte   (e1,e2,e3) -> "if " ^ aux e1 ^ " then " ^
                                    aux e2 ^
                                " else " ^
                                    aux e3 
        | MApp (e1,l1) -> aux e1 ^ (if l1 =[] then "" else to_args 
                                        (fun (e, _t) -> aux e) l1)
        | MMeth (sn, f) -> (string_of_string_option sn) ^ "!" ^ f
        | MCaml_def s -> "caml " ^ s
        | MMatch((e, _) ,case_l) -> "match " ^ aux e ^ "with " ^
                              List.fold_right
                                (fun (s,l,e) se -> " | #" ^ ident_name s ^
                                                   to_args (function None -> "_"
                                                                   | Some s -> s) l ^
                                                   " -> " ^ aux e ^ se)
                                       case_l "" in
        aux;;


let string_of_an_option f t =
  match t with
  | None -> "None"
  | Some t -> "Some(" ^ f t ^ ")";;

let string_of_typ_option t =
  string_of_an_option string_of_typ t;;

let string_of_string_option t =
  string_of_an_option (fun e -> e) t;;

let dbg_string_ident s =
  match s with
  | Prefix(None, s) -> "Prefix(None, " ^ s  ^ ")"
  | Prefix(Some e, s) -> "Prefix(Some(" ^ e ^ "), " ^ s  ^ ")"
  | Infix s -> "Infix(" ^ s ^ ")"
;;
(** Convert a term of type myexpr to a string *)
let rec dbg_string_myexpr =
      let rec aux e  =
        match e with
        | MGlob_id s ->
            "MGlob_id(" ^ dbg_string_ident s ^ ")"
        | MVar(s, o) ->
            "MVar(" ^ s ^ ", " ^ string_of_typ_option o ^ ")"
        | MInt i ->
            "MInt(" ^ string_of_int i ^ ")"
        | MString s ->
            "Mstring(\"" ^ s ^ "\")"
        | MFun (s, t, e) ->
            "MFun(" ^ s ^ ", " ^ string_of_typ_option t ^ ", " ^ aux e ^ ")"
(*         | MFun (s,None,e) -> "fun (" ^ s ^ ") -> " ^ aux e *)
        | MVarloc (b, (s,t),e1,e2) ->
            "MVatloc(" ^ string_of_bool b ^ ", (" ^
                         s ^ ", " ^ string_of_typ_option t ^ ")," ^
                          aux e1 ^ ", " ^ aux e2 ^ ")"
        | MIfte   (e1,e2,e3) -> 
            "MIfte(" ^ aux e1 ^ ", " ^
                                    aux e2 ^
                                ", " ^
                                    aux e3  ^ ")"
        | MApp (e1,l1) -> 
            "Mapp(" ^
            aux e1 ^ ", " ^ (if l1 =[] then "" else to_args 
                                        (fun (e, _t) -> aux e) l1) ^ ")"
        | MMeth (sn, f) ->
            "MMeth(" ^
            (string_of_string_option sn) ^ ", " ^ f ^ ")"
        | MCaml_def s -> "MCaml_def(" ^ s ^ ")"
        | MMatch((e, t), case_l) ->
            "MMatch((" ^ aux e ^ ", " ^ string_of_typ_option t ^ "), " ^
                    List.fold_right
                                (fun (s,l,e) se -> " | #" ^ ident_name s ^
                                                   to_args string_of_string_option l ^
                                                   " -> " ^ aux e ^ se)
                                       case_l "" ^ ")" in
        aux;;






(* The type defining a method *)
type methods =
    {methname : string;
     methtyp  : typ;
     methdef  : myexpr;
     methrec  : bool
    };;

(* For creating a method *)
let meth_create s typ def is_rec =
  {methname=s; methtyp=typ; methdef=def; methrec=is_rec };; 


let meths_concat m1 m2 =
  let rec aux cumul m_l =
    match m_l with
    | [] -> cumul
    | e::r ->
        if List.exists (fun x -> x.methname = e.methname) cumul then
          aux cumul r
        else
          aux (e::cumul) r in
   List.rev(aux (aux [] m1) m2);;

let ( @@ ) = meths_concat;;

(** We have three types for parameters:
    - parameters_expect is used when we extract the parameters from 
      a species in the environment
    - parameters_instance is the type used when the tester specify the parameter
      he wants to instanciate.
    - parameters is used for creating a species (here the entities parameters
      should be known)
*)

(** Type of parameters *)
type parameters_expect =
 | PrmExpColl of string * (string * string list) (** The values are parameters
                                                     name, species name and the
                                                     list of parameters applied
                                                     to the species *)
 | PrmExpEnt of string * Own_types.typ;; (** The values corresponds to
                                                      a possibly parameterized
                                                      type:
                                                      PrmExpEnt(p1,(list,[int])) for
                                                      p1 in list(int)
                                                      
                                                      p1 in list(list(int)) *)

let create_prmexpcoll s p= PrmExpColl(s, p);;
let create_prmexpent s p = PrmExpEnt(s, p);;
let get_name_prmexp = (function PrmExpColl(n,_) -> n | PrmExpEnt(n,_) -> n);;


let string_of_parameters_expect =
  function
    | PrmExpColl(p,(s,lp)) ->
        p ^ " is " ^ s ^
         (if lp = [] then "" else to_args (fun x -> x) lp)
           (*"(" ^ List.fold_right (fun e s -> e ^ "," ^ s)  lp ")" *)
    | PrmExpEnt (p,t) ->  p ^ " in " ^ string_of_typ t;;


(** Type of parameters the user give us
    When the user user give us a effective parameters he wants to test there
    two case :
    - when it is a collection parameter, he give us the name of the species he
      wants to test and the name of the parameter. The testing tools instanciate
      this parameter by its own.
    - when it is an entity parameter, he give us the value of this parameter.
  *)
type parameters_instance =
 | InstPrmColl of string option * string (** Name of parameter and the collection instanciating *)
 | InstPrmEnt  of myexpr;; (** Value we want to put *)

let create_instprmcoll s = InstPrmColl(None,s);;
let create_instprment e = InstPrmEnt(e);;

let is_coll_param p =
  match p with
  | InstPrmColl _ -> true
  | _ -> false

let string_of_parameters_instance =
  function
    | InstPrmColl (_,s) -> s
    | InstPrmEnt expr -> string_of_myexpr expr;;

let string_of_parameters_instance_verbose =
  function
    | InstPrmColl (None,s) -> "collection(" ^ s ^ ")"
    | InstPrmColl (Some n,s) -> "collection(" ^ n ^ "=" ^ s ^ ")"
    | InstPrmEnt expr -> "entity(" ^ string_of_myexpr expr ^ ")";;


type species_test = string * parameters_instance list;;


type parameters =
 | PrmColl of string * (species_name * string list) (** The values are parameters'
                                                  name, species' name and the
                                                  list of parameters applied
                                                  to the species *)
 | PrmEnt of string * Own_types.typ * myexpr;; (** Here we have the
                                                            value in plus *)
let create_prmcoll s p= PrmColl(s, p);;
let create_prment s p e = PrmEnt(s, p, e);;
let get_name_prm = (function PrmColl(n,_) -> n | PrmEnt(n,_,_) -> n);;

exception Cant_instanciate_param of parameters_expect * parameters_instance;;

let merge_prm expected instance = (* TODO : add to arguments the list of parameters at the
                                            left of those given in this function
                                              (the context) *)
  match expected,instance with
  | PrmExpColl(n,(_,l)), InstPrmColl(_,e') ->
      create_prmcoll n
                     (create_species_name (Whattodo.get_output_module ()) e',l)
  | PrmExpEnt(n,t)     , InstPrmEnt(v)   -> create_prment n t v
  | _,_ -> raise (Cant_instanciate_param (expected,instance));;


(* The type defining a species *) 
type species =
    {specname : string;
     specparam : parameters list; (* parameter name *
                                 species name  * (parameter name) list *)
     specinh  : (species_name * string list) list;
     specdef  : methods list;
     specrep  : typ option
    };;

(* this exception is raised if 
  * the nth parameter use others parameter than
  * the (n-1)nt first parameter *)
(* this exception is raised if the invariant is not respected *)
exception Param_non_canon of string*string;;

let spec_create s params inh rep meth_l =
  {specname = s;
   specparam = params;
   specinh = inh;
   specrep = rep;
   specdef = meth_l
  };;

(* The type of collection *)
type collection =
    {collname : string;
     collimpl : species_name * species_name list;
    };;

let coll_create c s =
  {collname = c;
   collimpl = s;
  };;

(* The type including collection and species within it *)

type toplevel_def =
    ObjCollection of collection
  | ObjSpecies    of species
  | ObjToplet     of string * typ option * myexpr
  | ObjTopcall    of myexpr
  | ObjType       of string * (string * typ list) list;;

let create_toplevel_coll c =
  ObjCollection c;;

let create_toplevel_spec s =
  ObjSpecies s;;

let create_toplevel_let n t e =
  ObjToplet(n,t,e);;

let create_toplevel_call e =
  ObjTopcall e;;

let create_toplevel_type n c =
  ObjType(n,c);;


(* The type of generate types *)
type gen_print_typ =
    Own_types.typ *  (**  The type. *)
    (string *        (**  Name of the function generating it. *)
     string);;       (**  Reference of the function printing it. *)

let create_gen_print_typ t g p : gen_print_typ =
(*   print_string " "; print_string (string_of_typ t); *)
  t,(g,p);;

let typ_get_generate t (l : gen_print_typ list) =
     fst(List.assoc t l);;

let typ_get_print t (l : gen_print_typ list) =
     snd(List.assoc t l);;

(* The definition of a fcl file : *)
type fichier_foc =
    {
     ficopenuse : string list;
     ficobjet : toplevel_def list;
    };;

let fic_create uses toplevel_def =
  {
   ficopenuse = uses;
   ficobjet = toplevel_def;
  };;


(* The type defining a .fml file *)

type import =
   string * (string * string) list

type fichier_fml =
   import list

let create_import n l : import = n,l;;

let add_caml_import (nimpo,def : import) n ndef : import =nimpo,(n,ndef)::def;;

let add_import (import : import)
               (fic : fichier_fml) : fichier_fml =
  import::fic;;

