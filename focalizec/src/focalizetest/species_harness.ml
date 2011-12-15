open Context_test;;
open Focalize_inter;;
open Own_expr;;
open Own_types;;
open Own_basics;;
open Random_rep;;

open Expr_prolog;;

let suffixe_id =
  let i = ref 0 in
    fun s ->
      i := !i + 1;
      s ^ "__" ^ string_of_int (!i);;

let spec_harness ((m, s) : species_name) =
  let name = m ^ "#" ^ s in
  create_species_name
      (Whattodo.get_output_module ())
      (Fresh_variable.get_from_existing_prefix name "Spec_");;

let spec_harness_harness s =
   create_species_name (Whattodo.get_output_module ())
                       (Fresh_variable.get_from_existing_prefix s "Harness__spec_");;

let coll_harness ((m,s) : species_name) =
  let name = m ^ "#" ^ s in
  create_species_name (Whattodo.get_output_module ())
                      (Fresh_variable.get_from_existing_prefix name "Coll_");;

let coll_harness_harness s = Fresh_variable.get_from_existing_prefix s "Harness__coll_";;

let new_param_name () = Fresh_variable.new_var_prefix "P";;

let parameters_list_of_parameters_list p_l expect_list =
  let rec aux p_l expect_list assoc i =
    match p_l, expect_list with
    | [], _ -> [] 
    | For_harness(spec, prm_l)::r, _ ->
        let prm_name = new_param_name () in
        let prm_prm = List.map (fun e -> List.assoc e assoc) prm_l in
        let prm = 
           (false,PrmColl(prm_name,
                     (spec_harness (Whattodo.get_output_module (), spec),
                      prm_prm))) in
        prm :: aux r expect_list ((i,prm_name)::assoc) (i+1)
    | From_user(spec, prm_l)::r, p::rr ->
        let prm_name = p in
        let prm_prm = List.map (fun e -> List.assoc e assoc) prm_l in
        let prm = 
           (true, PrmColl(prm_name,
                     (spec_harness (Whattodo.get_output_module (), spec),
                      prm_prm))) in
        prm :: aux r rr ((i,prm_name)::assoc) (i+1)
    | _ -> raise (Focalize_inter.Bad_parameters_numbers(0,0)) in
  aux p_l expect_list [] 1;;


let harness_one_sch target_name (sch : species_context_harness) l_typ =
  let s_to_harn = sch_get_species sch in
  print_string ("Harnessing : " ^ (snd s_to_harn) ^ "...\n");
  let user_prm = sch_get_parameters sch in
  let s_prm = get_parameters s_to_harn in
  let s_prm_name = List.map get_name_prmexp s_prm in
  let s_coll_prm_name = List.fold_left
                               (fun s e ->
                                  match e with
                                    | PrmExpColl(n,_) -> n :: s
                                    | _ -> s
                               ) [] s_prm in
  (* We remove all types which are Self or other *)
  let l_typ = List.filter (function  TAtom(_m, e) when e = focself -> false
                                   | TSpecPrm e when List.mem e s_coll_prm_name -> true
                                   | TPrm(_, _,_) as e when is_concrete_type e -> true
                                   | TAtom(_) as e when is_concrete_type e -> true
                                   | TAtom(Some "basics", "int")
                                   | TAtom(None, "int")-> true
                                   | _e -> (* print_string (string_of_typ e); *) false) l_typ in
  (* Body of the new species *)
(*             print_string "********\n"; *)
  let lvm = ast_random s_coll_prm_name l_typ (get_rep s_to_harn) in
  (***************************)
  let n_spec = spec_harness target_name in
  let n_coll = coll_harness target_name in
  let params = parameters_list_of_parameters_list user_prm s_prm_name in
  let n_spec_prm = List.map snd params in
  let prm_inh = List.map snd (List.filter fst params) in
  let prm_impl = List.map coll_harness
                   (List.map (fun e -> Own_expr.create_species_name
                                         (Whattodo.get_output_module ())
                                         (parameters_get_name e)) user_prm) in
  (* Import for species : *)
  (* Species : *)
    spec_create (snd n_spec)                      (* name *)
                n_spec_prm                        (* list_params (* params *) *)
                [(s_to_harn, List.map get_name_prm prm_inh)]  (* inherits *)
                None                               (* rep is not redefined *)
                lvm,                               (* new methods *)
  (* Collection : *)
               coll_create (snd (n_coll))           (* name *)
                           (n_spec,prm_impl) (* implements *)

(* Same but for a binding_context_harness argument *)
let harness_one_bch target_name (sch : species_context_harness) l_typ =
 (* let target_name = bch_get_name bch in 
  let sch = bch_get_sch bch in *)
  harness_one_sch target_name sch l_typ;;

let species_harness ct typ_to_gen =
  if wf_tc ct then
    let cth = tch_of_tc ct in
    let sc = tch_get_sc cth in
    let spec_name = sch_get_species sc in
    let spec, coll =
      harness_one_sch spec_name sc typ_to_gen in
    let list_binding = tch_get_binding cth in
    let top_def = List.fold_right (fun e ts ->
                                 match bch_get_sch e with
                                 | BCH_Coll(sch) ->
                                   let s, c =
                                     harness_one_bch
                                       (create_species_name
                                          (Whattodo.get_output_module ())
                                          (bch_get_name e)) sch [] in
                                     ObjSpecies s::ObjCollection c::ts
                                 | BCH_Ent(_) -> ts)
                                 list_binding
                                 (ObjSpecies spec::[ObjCollection coll]) in
            (****)
     (spec_name, spec.specparam, snd coll.collimpl),
     top_def
  else
    failwith "Le contexte de test n'est pas clos"
  ;;

