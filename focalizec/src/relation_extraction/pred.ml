
(******************)
(* Specifications *)
(******************)

(* Identifiers for everything *)
type ident = string

(* Pre-extraction environment *)
type 'a info_dic = (ident * 'a) list


(* Constructors' list of an inductive type *)
type s_ind_cstr_list =
  | CLList of ident list
  | CLNone

(* Inductive types *)
type s_ind_env = (ident * s_ind_cstr_list) list

(* Mode of an inductive predicate *)
type s_mode_option =
  | MInput
  | MOutput
  | MSkip
type s_mode = s_mode_option list

(* Predicate signature *)
type s_pred = {
  pred_name : ident; 
  pred_mode : s_mode;
  pred_fun_name : ident;
}

(* Term in specification *)
type s_term =
  | TVar of ident
  | TTuple of s_term list
  | TRecord of ident list * s_term list
  | TConstr of ident * s_term list * s_ind_cstr_list
  | TConst of ident
  | TFun of ident * s_term list
  | TFunNot of ident * s_term list

(* Predicate term: p(...) *)
type s_pred_term = {
  pdt_pred : s_pred;
  pdt_args : s_term list;
  pdt_not_flag : bool;
}

(* Premisse term: either function or predicate *)
type s_prem_term =
  | PMTFun of s_term
  | PMTPred of s_pred_term

(* Premisse *)
type s_prem =
  | PMTerm of s_prem_term
  | PMOr of s_prem list
  | PMChoice of s_prem list
  | PMAnd of s_prem list

(* Property (or constructor of an inductive predicate) *)
type s_prop = {
  p_forall : ident list;
  p_prems : s_prem list;
  p_concl : s_pred_term;
}


(* Specification of an inductive predicate *)
type ('p, 'c, 'r, 'co, 'f) s_pred_spec = {
  ps_props : s_prop list;
  ps_pred_info : 'p info_dic;
  ps_constr_info : 'c info_dic;
  ps_record_info : 'r info_dic;
  ps_const_info : 'co info_dic;
  ps_fun_info : 'f info_dic;
  ps_ind_env : s_ind_env;
  ps_pred : s_pred;
}


(* Trees *)

(* Tree node content *)
type node_type =
  | NTPrem of s_prem_term
  | NTConcl of s_pred_term

(* Tree structure *)
type tree_node =
  | TreeNode of (node_type * tree_node list)
  | TreeOutput of (node_type * s_pred_term)

type pred_tree = tree_node list

(******************)
(* Target langage *)
(******************)

type l_term =
  | LVar of ident
  | LTuple of l_term list
  | LRecord of ident list * l_term list
  | LConstr of ident * l_term list
  | LConst of ident
  | LFun of ident * l_term list
  | LFunNot of ident * l_term list
  | LMatch of l_term * (l_pat * l_term) list
  | LLin of (ident * ident) list
  | LTrue | LFalse | LDefault

and l_pat =
  | LPVar of ident
  | LPTuple of l_pat list
  | LPRecord of ident list * l_pat list
  | LPConstr of ident * l_pat list
  | LPConst of ident
  | LPWild
  | LPTrue | LPFalse

type l_fun = ident * ident list * l_term

(* Printers *)

let flatmap f l = List.flatten (List.map f l)

let rec pp_s_mode mode = match mode with
  | [] -> ""
  | MInput::tl_mode -> "I" ^ (pp_s_mode tl_mode)
  | MOutput::tl_mode -> "O" ^ (pp_s_mode tl_mode)
  | MSkip::tl_mode -> "S" ^ (pp_s_mode tl_mode)

let pp_s_pred pred = pred.pred_name ^ (pp_s_mode pred.pred_mode)

let rec concat_list l sep = match l with
  | [] -> ""
  | [a] -> a
  | a::tl -> a ^ sep ^ (concat_list tl sep)

let rec pp_s_term t = match t with
  | TConst i -> i
  | TVar i -> i
  | TTuple tl -> "(" ^ (concat_list (List.map pp_s_term tl) ", ") ^ ")"
  | TRecord (il, tl) -> "{" ^ (concat_list (List.map2 (fun i t -> i ^ " = " ^
      (pp_s_term t)) il tl) "; ") ^ "}"
  | TConstr (i, tl, _) -> i ^ "(" ^ (concat_list (List.map pp_s_term tl) ", ")
      ^ ")"
  | TFun (i, tl) -> i ^ "(" ^ (concat_list (List.map pp_s_term tl) ", ") ^ ")"
  | TFunNot (i, tl) -> "not " ^ i ^ "(" ^
      (concat_list (List.map pp_s_term tl) ", ") ^ ")"

let pp_s_pred_term pdt = (pp_s_pred pdt.pdt_pred) ^ "(" ^
  (concat_list (List.map pp_s_term pdt.pdt_args) ", ") ^ ")"

let pp_s_prem_term pmt = match pmt with
  | PMTPred pdt -> pp_s_pred_term pdt
  | PMTFun t -> pp_s_term t

let rec pp_s_prem prem = match prem with
  | PMTerm pmt -> pp_s_prem_term pmt
  | PMChoice pml -> "(" ^ (concat_list (List.map pp_s_prem pml) " | ") ^ ")"
  | PMAnd pml -> "(" ^ (concat_list (List.map pp_s_prem pml) " /\\ ") ^ ")"
  | PMOr pml -> "(" ^ (concat_list (List.map pp_s_prem pml) " \\/ ") ^ ")"

let pp_s_prop p = concat_list
  ((List.map pp_s_prem p.p_prems)@[pp_s_pred_term p.p_concl]) " -> "

let pp_s_pred_spec spec = concat_list (List.map pp_s_prop spec.ps_props) "\n"

let pp_nt nt = match nt with
  | NTPrem pmt -> pp_s_prem_term pmt
  | NTConcl pdt -> "[" ^ (pp_s_pred_term pdt) ^ "]"

let rec pp_tn inc tn = match tn with
  | TreeNode (nt, ch) -> inc ^ (pp_nt nt) ^ "\n" ^
    (concat_list (List.map (pp_tn (inc^"  ")) ch) "\n")
  | TreeOutput (nt, pdt) -> inc ^ (pp_nt nt) ^ " -> " ^ (pp_s_pred_term pdt)

let pp_tree tree = concat_list (List.map (pp_tn "") tree) "\n"

let rec pp_l_pat pat = match pat with
  | LPVar i -> i
  | LPTuple pl -> "(" ^ (concat_list (List.map pp_l_pat pl) ", ") ^ ")"
  | LPRecord (il, pl) -> "{" ^ (concat_list (List.map2 (fun i t -> i ^ " = " ^
      (pp_l_pat t)) il pl) "; ") ^ "}"
  | LPConstr (i, pl) -> i ^ "(" ^ (concat_list (List.map pp_l_pat pl) ", ") ^ ")"
  | LPConst i -> i
  | LPWild -> "_"
  | LPTrue -> "true"
  | LPFalse -> "false"

let rec pp_l_term inc term = match term with
  | LVar i -> i
  | LTuple tl -> "(" ^ (concat_list (List.map (pp_l_term inc) tl) ", ") ^ ")"
  | LRecord (il, tl) -> "{" ^ (concat_list (List.map2 (fun i t -> i ^ " = " ^
      (pp_l_term inc t)) il tl) "; ") ^ "}"
  | LConstr (i, tl) -> i ^ "(" ^ (concat_list (List.map (pp_l_term inc) tl) ", ") ^ ")"
  | LConst i -> i
  | LFun (i, tl) -> i ^ "(" ^ (concat_list (List.map (pp_l_term inc) tl) ", ") ^ ")"
  | LFunNot (i, tl) -> "not " ^ i ^ "(" ^
      (concat_list (List.map (pp_l_term inc) tl) ", ") ^ ")"
  | LMatch (t, ptl) -> "match " ^ (pp_l_term inc t) ^ " with\n" ^
      concat_list (List.map (pp_l_patterm (inc ^ "  ")) ptl) ""
  | LLin iil -> concat_list (List.map (fun (i1, i2) -> i1 ^ " = " ^ i2) iil) " && "
  | LTrue -> "true" | LFalse -> "false" | LDefault -> "default"

and pp_l_patterm inc (pat, term) = 
  inc ^ "| " ^ (pp_l_pat pat) ^ " -> " ^ (pp_l_term inc term) ^ "\n"
  
let pp_l_fun (i, args, term) = "fun " ^ i ^ " " ^ (concat_list args " ") ^ " =\n" ^
  pp_l_term "" term


(* Extraction algorithm *)

(* Select inputs or outputs of a inductive predicate term *)
let get_in_terms_pred pdt =
  let rec get_rec args mode = match (args, mode) with
    | (a::tl_args, MInput::tl_mode) -> a::(get_rec tl_args tl_mode)
    | (_::tl_args, MOutput::tl_mode) -> get_rec tl_args tl_mode
    | (_, MSkip::tl_mode) -> get_rec args tl_mode
    | _ -> [] in
  get_rec pdt.pdt_args pdt.pdt_pred.pred_mode
let get_out_terms_pred pdt =
  let rec get_rec args mode = match (args, mode) with
    | (a::tl_args, MOutput::tl_mode) -> a::(get_rec tl_args tl_mode)
    | (_::tl_args, MInput::tl_mode) -> get_rec tl_args tl_mode
    | (_, MSkip::tl_mode) -> get_rec args tl_mode
    | _ -> [] in
  get_rec pdt.pdt_args pdt.pdt_pred.pred_mode

(* Get a list of variables in a term *)
let rec get_variables t = match t with
  | TConst _ -> []
  | TVar i -> [i]
  | TTuple tl | TRecord (_, tl) | TConstr (_, tl, _)
  | TFun (_, tl) | TFunNot (_, tl) ->
    List.flatten (List.map get_variables tl)

(* Select inputs or outputs of a node_type element *)
(* for these 4 functions, in and out are reversed for conclusion nodes *)
let get_in_terms nt = match nt with
  | NTPrem pmt -> ( match pmt with
    | PMTPred pdt -> get_in_terms_pred pdt
    | PMTFun t -> [t]
    )
  | NTConcl pdt -> get_out_terms_pred pdt
let get_out_terms nt = match nt with
  | NTPrem pmt -> ( match pmt with
    | PMTPred pdt -> get_out_terms_pred pdt
    | PMTFun t -> []
    )
  | NTConcl pdt -> get_in_terms_pred pdt
(* Get variables in inputs or outputs of a node_type element *)
let get_in_vars nt = List.flatten (List.map get_variables (get_in_terms nt))
let get_out_vars nt = List.flatten (List.map get_variables (get_out_terms nt))

(* Variable substitution in a term *)
let rename_var i mapping = if List.mem_assoc i mapping then
  List.assoc i mapping else i

(* Find a variables substitution in order to make term and ref_term equals *)
let rec find_renaming_terms mapping term ref_term = match term, ref_term with
  | (TConst i1, TConst i2) when i1 = i2 -> mapping
  | (TVar i1, TVar i2) -> if List.mem_assoc i1 mapping then
      if (rename_var i1 mapping) = i2 then mapping else failwith "impossible"
    else
      (i1, i2)::mapping
  | (TTuple tl1, TTuple tl2) | (TRecord (_, tl1), TRecord (_, tl2)) ->
    List.fold_left2 find_renaming_terms mapping tl1 tl2
  | (TConstr (i1, tl1, _), TConstr (i2, tl2, _)) when i1 = i2 ->
    List.fold_left2 find_renaming_terms mapping tl1 tl2
  | (TFun (_, tl1), TFun (_, tl2)) | (TFunNot (_, tl1), TFunNot (_, tl2)) ->
    List.fold_left2 find_renaming_terms mapping tl1 tl2
  | _ -> failwith "impossible"

(* Find a variables substitution in order to make term_list and ref_term_list
   equals *)
let find_renaming term_list ref_term_list =
  List.fold_left2 find_renaming_terms [] term_list ref_term_list

(* Apply variables subsitution defined by mapping to the term *)
let rec rename_term mapping term = match term with
  | TConst _ -> term
  | TVar i -> TVar (rename_var i mapping)
  | TTuple tl -> TTuple (List.map (rename_term mapping) tl)
  | TRecord (il, tl) -> TRecord (il, (List.map (rename_term mapping) tl))
  | TConstr (i, tl, it) -> TConstr (i, (List.map (rename_term mapping) tl), it)
  | TFun (i, tl) -> TFun (i, (List.map (rename_term mapping) tl))
  | TFunNot (i, tl) -> TFunNot (i, (List.map (rename_term mapping) tl))

(* Apply variables subsitution defined by mapping to the predicate term *)
let rename_pdt mapping pdt =
  { pdt_pred = pdt.pdt_pred;
    pdt_args = List.map (rename_term mapping) pdt.pdt_args;
    pdt_not_flag = pdt.pdt_not_flag }

(* Apply variables subsitution defined by mapping to the node_type *)
let rename_nt mapping nt = match nt with
  | NTConcl pdt -> NTConcl (rename_pdt mapping pdt)
  | NTPrem (PMTFun t) -> NTPrem (PMTFun (rename_term mapping t))
  | NTPrem (PMTPred pdt) -> NTPrem (PMTPred (rename_pdt mapping pdt))

(* Apply variables subsitution defined by mapping to the property *)
let rename_prop mapping prop = 
  let rec rename_prem prem = match prem with
    | PMTerm (PMTFun t) -> PMTerm (PMTFun (rename_term mapping t))
    | PMTerm (PMTPred pdt) -> PMTerm (PMTPred (rename_pdt mapping pdt))
    | PMAnd pml -> PMAnd (List.map rename_prem pml)
    | PMOr pml -> PMOr (List.map rename_prem pml)
    | PMChoice pml -> PMChoice (List.map rename_prem pml)
  in
  { p_forall = prop.p_forall;
    p_prems = List.map rename_prem prop.p_prems;
    p_concl = rename_pdt mapping prop.p_concl }

(* try to rename a node_type and the associated property to fit a tree node *)
(* raises Failure "impossible" if it fails *)
(* after renaming nt can be inserted in tn *)
let rename_outputs_if_possible nt tn prop = match nt, tn with
  | (NTConcl pdt, TreeNode (NTConcl refpdt, _)) -> 
    let ts, refts = get_in_terms_pred pdt, get_in_terms_pred refpdt in
    let mapping = find_renaming ts refts in
    (rename_nt mapping nt, rename_prop mapping prop)
  |          ( NTPrem (PMTFun (TFun(i, _))),
    TreeNode ( (NTPrem (PMTFun (TFun(ri, _))),_) )) when i = ri ->
    (nt, prop) (* nothing to rename, as there are no outputs *)
  |          ( NTPrem (PMTFun (TFunNot(i, _))),
    TreeNode ( (NTPrem (PMTFun (TFunNot(ri, _))),_) )) when i = ri ->
    (nt, prop) (* nothing to rename, as there are no outputs *)
  |          ( NTPrem (PMTPred pdt),
    TreeNode ( (NTPrem (PMTPred refpdt),_) )) ->
    if pdt.pdt_pred = refpdt.pdt_pred then
      let ts, refts = get_out_terms_pred pdt, get_out_terms_pred refpdt in
      let mapping = find_renaming ts refts in
      (rename_nt mapping nt, rename_prop mapping prop)
    else failwith "impossible"
  | _ -> failwith "impossible"

(* try to rename a node_type and the associated property to fit a tree node *)
(* raises Failure "impossible" if it fails *)
(* after renaming nt can be inserted in the tn list *)
let rename_inputs_if_possible nt tn prop =  match nt, tn with
  | NTConcl _, _ -> (nt, prop) (* nothing to do *)
  |          ( NTPrem (PMTFun (TFun(i,a)|TFunNot(i,a))),
    TreeOutput ( (NTPrem (PMTFun (TFun(ri,ra)|TFunNot(ri,ra))),_) ))
  |          ( NTPrem (PMTFun (TFun(i,a)|TFunNot(i,a))),
    TreeNode ( (NTPrem (PMTFun (TFun(ri,ra)|TFunNot(ri,ra))),_) )) when i = ri ->
    let mapping = find_renaming a ra in
    (rename_nt mapping nt, rename_prop mapping prop)
  |          ( NTPrem (PMTPred pdt),
    TreeNode ( (NTPrem (PMTPred refpdt),_) )) 
  |            ( NTPrem (PMTPred pdt),
    TreeOutput ( (NTPrem (PMTPred refpdt),_) )) -> 
    if pdt.pdt_pred = refpdt.pdt_pred then 
      let ts, refts = get_in_terms_pred pdt, get_in_terms_pred refpdt in
      let mapping = find_renaming ts refts in 
      (rename_nt mapping nt, rename_prop mapping prop)
    else failwith "impossible"
  | _ ->  failwith "impossible"

  
(* Determinism check between terms *)
let rec test_det_terms t1 t2 = match (t1, t2) with
  | (TFun _, TFunNot _) | (TFunNot _, TFun _) -> true
  | (TTuple tl1, TTuple tl2) | (TRecord (_, tl1), TRecord (_, tl2)) ->
    List.exists2 test_det_terms tl1 tl2
  | (TConstr (i1, tl1, _) , TConstr (i2, tl2, _)) ->
    i1 <> i2 || (List.exists2 test_det_terms tl1 tl2)
  | (TConst i1, TConst i2) -> i1 <> i2
  | _ -> false

(* Determinism check between node_types *)
let test_det_nt nt1 nt2 =
  List.exists2 test_det_terms (get_out_terms nt1) (get_out_terms nt2)

(* determinism test between nt and first nt of each tn *)
let test_det nt tnl = List.for_all
  (fun tn -> let nt2 = (match tn with
    | TreeNode (nt, _) -> nt
    | TreeOutput (nt, _) -> nt 
                        ) in test_det_nt nt nt2) tnl

(* Return true if t1 is an instance of t2, false else. *)
let terms_ordering nt1 nt2 =
  let rec test_unif (t1, t2) = match t1, t2 with
    | _, TVar _ -> true
    | TConst i1, TConst i2 -> i1 = i2
    | TConstr(i1, tl1, _), TConstr(i2, tl2, _) -> i1 = i2 &&
        List.for_all test_unif (List.combine tl1 tl2)
    | TRecord(_, tl1), TRecord(_, tl2) | TTuple tl1, TTuple tl2 ->
        List.for_all test_unif (List.combine tl1 tl2)
    | _ -> false in
  let rec test_inclusion (t1, t2) = match t1, t2 with
    | TConstr _, TVar _ -> true
    | TConstr(_, tl1, _), TConstr(_, tl2, _)
    | TRecord(_, tl1), TRecord(_, tl2) | TTuple tl1, TTuple tl2 ->
        List.exists test_inclusion (List.combine tl1 tl2)
    | _ -> false in
  let terms1, terms2 = (get_out_terms nt1), (get_out_terms nt2) in
  let comb = List.combine terms1 terms2 in
  List.for_all test_unif comb && List.exists test_inclusion comb

(* Return
     true if nt1 is an instance of nt2 or if nt1 is not unifiable with nt2,
     false else. *)
let nt_partial_ordering ord nt1 nt2 =
  test_det_nt nt1 nt2 || (ord && (terms_ordering nt1 nt2))

(* List inclusion *)
let rec included l1 l2 = match l1 with
  | [] -> true
  | a::t -> (List.mem a l2) && (included t l2)

(* Mode coherency analysis for a node_type *)
(* Check that all variables needed by nt are known *)
(* Don't chack anything for conclusion nodes, mca_check_output must be used
   for the output node *)
let mca_check kv nt = match nt with
  | NTConcl _ -> true
  | NTPrem _ -> included (get_in_vars nt) kv


(* Mode coherency analysis for a predicate term *)
(* Check that variables needed by the output are known *)
let mca_check_output kv pdt = 
  let vars = get_in_vars (NTConcl pdt) in
included vars kv

(* Get new knwon variables for the mode coherency analysis *)
(* Add variables calculated with nt to the known variables *)
let mca_add_vars kv nt = (get_out_vars nt) @ kv

(* Find all possible couple of the form (premisse, prop without the premisse) *)
let select_one_prem prop = 
  let rec select_rec prems = match prems with
    | [] -> []
    | p::ptail -> (p, ptail) ::
      (List.map (fun (p2, prems) -> (p2, p::prems)) (select_rec ptail)) in
  List.map (fun (p, prems) -> (p,
    {p_forall = prop.p_forall;
     p_prems = prems;
     p_concl = prop.p_concl}))
  (select_rec prop.p_prems)
(* Quick version, but not exhaustive ! *)
let select_one_prem_quick prop = 
  match prop.p_prems with
    | [] -> []
    | p::ptail -> [(p,
    {p_forall = prop.p_forall;
     p_prems = ptail;
     p_concl = prop.p_concl})]

(* try to rename inputs of a node type when this is necessary *)
let rename_inputs_if_needed nt tnl prop = match tnl with
  | [] -> nt, prop
  | tn::_ -> rename_inputs_if_possible nt tn prop


(* Put one premisse in conjunctive normal form *)
let rec develop_and and_prems = match and_prems with
  | [PMAnd prems] -> prems
  | (PMAnd (prems))::tl_and_prems ->
    let dev_prems = develop_and tl_and_prems in
    flatmap (fun p -> match p with PMOr top_prems ->
      List.map
       (fun dp -> match dp with PMOr dtop_prems -> PMOr (top_prems@dtop_prems)
         | _ -> assert false)
       dev_prems
      | _ -> assert false) prems
  | _ -> assert false
let rec normalize_and_or prem = let rec norm_rec prem = match prem with
  | PMTerm _ -> PMAnd [PMOr [prem]]
  | PMChoice _ -> PMAnd [PMOr [prem]]
  | PMAnd prems -> PMAnd (flatmap
    (fun p -> match norm_rec p with PMAnd ors -> ors | _ -> assert false)
    prems)
  | PMOr prems -> let nprems = List.map norm_rec prems in
    PMAnd (develop_and nprems) in
  let norm_prem = norm_rec prem in
  let simplify_or prem = match prem with
    | PMOr [p] -> p
    | _ -> prem in
  match norm_prem with
    | PMAnd [or_prem] -> simplify_or or_prem
    | PMAnd or_prems -> PMAnd (List.map simplify_or or_prems)
    | _ -> assert false


(* check if a node_type can be safely inserted in a treenode list *)
let check_insertable nt tnl = match tnl with
  | [] -> true
  | (TreeOutput(nt', _) | TreeNode(nt', _))::_ -> match (nt, nt') with
    | (NTConcl pdt, NTConcl pdt')
    | (NTPrem (PMTPred pdt), NTPrem(PMTPred pdt')) ->
      pdt.pdt_pred = pdt'.pdt_pred
    | (NTPrem (PMTFun (TFun (f, _) | TFunNot (f, _))),
       NTPrem (PMTFun (TFun (f', _) | TFunNot (f', _)))) ->
      f = f'
    | _ -> false


(* TODO: modify insert_output and the recusrsor... There is something wrong. *)
(* insert the tree output (the last node) of a property *)
let rec insert_output ord kv nt prop tnl = 
  if not (check_insertable nt tnl) then [] (* check logical terms compatibility *)
  else try let (nt, prop) = match tnl with (* rename inputs (matching term) *)
    | tn::_ -> rename_inputs_if_possible nt tn prop
    | [] -> (nt, prop) in
  let tn = TreeOutput (nt, prop.p_concl) in
  let rec io_rec tnl = match tnl with (* try to insert tn in the right place *)
    | [] -> [[TreeOutput (nt, prop.p_concl)]]
      (* we can always insert at the end because all the tests have been done before *)
    | ((TreeOutput (nti, _) | (TreeNode (nti, _))) as tni)::tntl ->
      if nt_partial_ordering ord nti nt then (* nti still < nt, continue *)
        List.map (fun ntnl -> tni::ntnl) (io_rec tntl)
      else
        (* nt must be inserted *)
       let test_tail tni = match tni with
         | TreeNode (nti, _) | TreeOutput (nti, _) ->
            nt_partial_ordering ord nt nti in
        if List.for_all test_tail tnl then [tn::tnl]
        else []
  in io_rec tnl
  with Failure "impossible" -> []


(* insert the last premisse of a property in a tree *)
let rec insert_last_prem_term ord kv nt prop tnl = 
  insert_output ord kv nt prop tnl

let rec insertion_recursor ord prem_selector prop kv nt tnl =
  let rec ir_rec tnl_acc = (* try to insert prop in every node or alone *)
    match tnl_acc with
      | [] -> (* no matching nt, insert alone *)
        let kv' = mca_add_vars kv nt in
        List.map (fun nchild -> [TreeNode (nt, nchild)])
          (choose_prop_prem ord prem_selector kv' prop [])
      | (TreeNode (nti, child) as tni)::acc_tail -> 
        if nt_partial_ordering ord nti nt then
          (* nti still < nt, continue *)
          List.map (fun tnl -> tni::tnl) (ir_rec acc_tail)
        else let test_tail tn = match tn with
          | TreeNode (nti, _) | TreeOutput (nti, _) -> 
            nt_partial_ordering ord nt nti in
        if List.for_all test_tail tnl_acc then
          (* nt can be inserted alone, before tni *)
          let kv' = mca_add_vars kv nt in
          List.map (fun nchild -> (TreeNode (nt, nchild))::tnl_acc)
            (choose_prop_prem ord prem_selector kv' prop [])
        else (* try to insert nt into tni *)
        ( try let rnt, rprop = rename_outputs_if_possible nt tni prop in
          let kv' = mca_add_vars kv rnt in
          List.map (fun nchild -> (TreeNode (nti, nchild))::acc_tail)
            (choose_prop_prem ord prem_selector kv' rprop child)
         with Failure "impossible" -> [])
      | tn::acc_tail -> if test_det nt [tn] then
          List.map (fun tnl -> tn::tnl) (ir_rec acc_tail)
        else []
  in ir_rec tnl

(* insert a premisse term in the treenode list *)
and insert_prem_term ord prem_selector kv nt prop tnl =
  if not (check_insertable nt tnl) then []
  else if mca_check kv nt then
    try let nt, prop = rename_inputs_if_needed nt tnl prop in
      insertion_recursor ord prem_selector prop kv nt tnl
    with Failure "impossible" -> []
  else []

(* Insert prem into tnl. Select insert_last_prem_term or insert_prem_term *)
and insert_prem ord prem_selector kv prem prop tnl = match prem with
  | PMTerm pmt -> let nt = NTPrem pmt in
    if prop.p_prems = [] then insert_last_prem_term ord kv nt prop tnl
    else insert_prem_term ord prem_selector kv nt prop tnl
  | PMAnd pl -> flatmap (fun prem ->
      let other_prems = List.filter (fun a -> a <> prem) pl in
      let nprop = { prop with p_prems = other_prems@prop.p_prems } in
      insert_prem ord prem_selector kv prem nprop tnl
    ) pl
  | PMChoice pl -> flatmap
    (fun prem -> insert_prem ord prem_selector kv prem prop tnl) pl
  | PMOr pl -> List.fold_left
    (fun trees prem -> flatmap
      (fun tnl -> insert_prem ord prem_selector kv prem prop tnl) trees)
    [tnl] pl

(* Choose one premisse of prop to insert into tnl *)
and choose_prop_prem ord prem_selector kv prop tnl =
  flatmap
    (fun (prem, prop) ->
      insert_prem ord prem_selector kv (normalize_and_or prem) prop tnl)
    (prem_selector prop)

(* all trees that can result of the insertion of prop in tnl *)
let insert_prop_concl ord prem_selector prop tnl =
  let nt = NTConcl prop.p_concl in
  match prop.p_prems with
    | [] -> (* insert the prop alone, as a tree output *)
      insert_output ord [] nt prop tnl
    | _ -> (* try to insert prop in one nt or alone *)
      insertion_recursor ord prem_selector prop [] nt tnl

(* TODO: optimization when there is no overlapping constructor to add after the
 * current one. It may be possible to test with nt_partial_ordering...
 *)
(* Build a tree from a specification *)
let tree_from_spec ord prem_selector spec =
  let trees = List.fold_left (fun tree_list prop ->
  match tree_list with
  | [] -> insert_prop_concl ord prem_selector prop []
  | _ -> begin
    match flatmap (insert_prop_concl ord prem_selector prop) tree_list with
      | [] ->
(*List.iter (fun t -> Printf.printf "**************\n%s\n\n\n" (pp_tree t))
  tree_list;*)
failwith "error"        (*errorlabstrm "Exraction"
          (str ("Extraction not possible: " ^ (pp_s_pred_term prop.p_concl))) *)
      | l -> l
    end
) [] spec.ps_props in
match trees with
  | t::_ -> t
  | _ -> assert false


let tree_from_spec_with_quick_try ord spec =
  try tree_from_spec ord select_one_prem_quick spec
  with _ ->
    Printf.printf "Quick algorithm failed. Trying the exhaustive one...\n";
    tree_from_spec ord select_one_prem spec

let string_of_mode mode = 
  if List.for_all (fun mo -> mo != MOutput) mode then ""
  else let rec rec_som mode i =  match mode with
    | [] -> ""
    | MInput::tl_mode -> (string_of_int i) ^ (rec_som tl_mode (i+1))
    | MOutput::tl_mode -> rec_som tl_mode (i+1)
    | MSkip::tl_mode -> rec_som tl_mode i in
  rec_som mode 1

let get_pred_name pred =
  if pred.pred_fun_name = "" then
    pred.pred_name ^ (string_of_mode pred.pred_mode)
  else pred.pred_fun_name

let gen_args mode =
  let rec rec_args mode i = match mode with
    | [] -> []
    | MInput::m_tail -> ("p" ^ (string_of_int i)) :: (rec_args m_tail (i+1))
    | _::m_tail -> rec_args m_tail i in
  rec_args mode 1

let rec gen_term t = match t with
  | TVar i -> LVar i
  | TTuple tl -> LTuple (List.map gen_term tl)
  | TRecord (il, tl) -> LRecord (il, (List.map gen_term tl))
  | TConstr (i, tl, _) -> LConstr (i, (List.map gen_term tl))
  | TConst i -> LConst i
  | TFun (i, tl) -> LFun (i, (List.map gen_term tl))
  | TFunNot (i, tl) -> LFun (i, (List.map gen_term tl))

let rec gen_term_pat t = match t with
  | TVar i -> LPVar i
  | TTuple tl -> LPTuple (List.map gen_term_pat tl)
  | TRecord (il, tl) -> LPRecord (il, (List.map gen_term_pat tl))
  | TConstr (i, tl, _) -> LPConstr (i, (List.map gen_term_pat tl))
  | TConst i -> LPConst i
  | _ -> failwith "blabla" (*errorlabstrm "Exraction" (str ("Function call in pattern")) *)

let gen_tuple terms_list = match terms_list with
  | [] -> LTrue
  | [t] -> gen_term t
  | _ -> LTuple (List.map gen_term terms_list)
let gen_tuple_pat terms_list orig_nt = match terms_list with
  | [] -> begin match orig_nt with
      | NTPrem (PMTFun (TFunNot _)) -> LPFalse
      | _ -> LPTrue
    end
  | [t] -> gen_term_pat t
  | _ -> LPTuple (List.map gen_term_pat terms_list)


let rec lin_pat pat vars i lins = match pat with
  | LPVar v -> if included [v] vars then
    let nv = v ^ "__" ^ (string_of_int i) in
      (LPVar nv, vars, i+1, (v, nv)::lins)
    else (LPVar v, v::vars, i, lins)
  | LPTuple pl -> let (npatl, nvars, ni, nlins) =
    List.fold_right (fun p (patl, vars, i, lins) ->
      let (np, nvars, ni, nlins) = lin_pat p vars i lins in
      (np::patl, nvars, ni, nlins)
    ) pl ([], vars, i, lins) in
    (LPTuple npatl, nvars, ni, nlins)
  | LPRecord (il, pl) -> let (npatl, nvars, ni, nlins) =
    List.fold_right (fun p (patl, vars, i, lins) ->
      let (np, nvars, ni, nlins) = lin_pat p vars i lins in
      (np::patl, nvars, ni, nlins)
    ) pl ([], vars, i, lins) in
    (LPRecord (il, npatl), nvars, ni, nlins)
  | LPConstr (id, pl) -> let (npatl, nvars, ni, nlins) =
    List.fold_right (fun p (patl, vars, i, lins) ->
      let (np, nvars, ni, nlins) = lin_pat p vars i lins in
      (np::patl, nvars, ni, nlins)
    ) pl ([], vars, i, lins) in
    (LPConstr (id, npatl), nvars, ni, nlins)
  | _ -> (pat, vars, i, lins)

let gen_match_term nt = match nt with
  | NTPrem (PMTFun _) -> gen_tuple (get_in_terms nt)
  | NTConcl pdt | NTPrem (PMTPred pdt) -> let pn = get_pred_name pdt.pdt_pred in
    LFun (pn, (List.map gen_term (get_in_terms nt)))
let gen_pat_term nt next_term = 
  let (pat, _, _, lins) = lin_pat (gen_tuple_pat (get_out_terms nt) nt) [] 1 [] in
  if List.length lins > 0 then
    (pat, LMatch (LLin lins, [(LPTrue, next_term);(LPFalse, LDefault)]))
  else
    (pat, next_term)

let rec gen_pat tn = match tn with
  | TreeNode (nt, tnl) -> gen_pat_term nt (gen_match tnl)
  | TreeOutput (nt, pdt) -> gen_pat_term nt (gen_tuple (get_out_terms_pred pdt))

and gen_match tree = match List.hd tree with
  | TreeNode (nt, _) | TreeOutput (nt, _) ->
    let mt = gen_match_term nt in
    LMatch (mt, List.map gen_pat tree)
    
    

let code_from_tree spec tree = 
  let args = gen_args spec.ps_pred.pred_mode in
  let fun_ident = get_pred_name spec.ps_pred in
  let pats = List.map gen_pat tree in
  (fun_ident, args, LMatch (gen_tuple (List.map (fun a -> TVar a) args), pats))
