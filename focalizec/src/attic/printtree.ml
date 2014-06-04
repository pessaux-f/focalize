(* $Id: printtree.ml,v 1.1 2007-06-29 05:43:53 pessaux Exp $ *)

(* Print the parse tree, or shallow abstract syntax in the Caml way
   (i.e. you get the tree of constructor applications that build the given
    a.s.t.). *)

(* Camlpp generated file *)

open Lib_pp;;
open Format;;
open Parsetree;;

let rec print_position =
  function
   {pos_fname = s; pos_lnum = i; pos_bol = i0; pos_cnum = i1; } ->
     printf "@[<1>{"; printf "@[<1>pos_fname =@ "; print_quoted_string s;
     printf ";@]@ "; printf "@[<1>pos_lnum =@ "; print_quoted_int i;
     printf ";@]@ "; printf "@[<1>pos_bol =@ "; print_quoted_int i0;
     printf ";@]@ "; printf "@[<1>pos_cnum =@ "; print_quoted_int i1;
     printf ";@]@ "; printf "@,}@]";;

let rec print_location = function
  {l_beg = p; l_end = p0; } ->
    printf "@[<1>{"; printf "@[<1>l_beg =@ "; print_position p;
    printf ";@]@ "; printf "@[<1>l_end =@ "; print_position p0;
    printf ";@]@ "; printf "@,}@]";;

let rec print_fname = (function s -> print_quoted_string s);;

let rec print_cname = (function s -> print_quoted_string s);;

let rec print_sname = (function s -> print_quoted_string s);;

let rec print_tname = (function s -> print_quoted_string s);;

let rec print_vname = function
  | Vlident s ->
     printf "@[<1>(%s@ " "Vlident"; print_quoted_string s; printf ")@]"
  | Vuident s ->
     printf "@[<1>(%s@ " "Vuident"; print_quoted_string s; printf ")@]"
  | Vpident s ->
     printf "@[<1>(%s@ " "Vpident"; print_quoted_string s; printf ")@]"
  | Viident s ->
     printf "@[<1>(%s@ " "Viident"; print_quoted_string s; printf ")@]"
  | Vqident s ->
     printf "@[<1>(%s@ " "Vqident"; print_quoted_string s; printf ")@]";;

let rec print_label_name = (function s -> print_quoted_string s);;

let rec print_constr_name = (function v -> print_vname v);;

let rec print_node_label =
  (function (i, s) ->
    printf "@[<1>("; print_quoted_int i; printf ",@ "; print_quoted_string s;
    printf ")@]");;

let rec print_external_name =
  (function (s, s0) ->
    printf "@[<1>("; print_quoted_string s; printf ",@ ";
    print_quoted_string s0; printf ")@]");;

let rec print_generic_ast pr_a pr_b = function
  {ast_loc = l; ast_desc = a; ast_doc = o_b; } ->
    printf "@[<1>{"; printf "@[<1>ast_loc =@ "; print_location l;
    printf ";@]@ "; printf "@[<1>ast_desc =@ "; pr_a a; printf ";@]@ ";
    printf "@[<1>ast_doc =@ "; print_option (pr_b) o_b; printf ";@]@ ";
    printf "@,}@]";;

let rec print_ast pr_a =
  (function g_a_s -> print_generic_ast (pr_a) (print_quoted_string) g_a_s);;

let rec print_ast_doc pr_a =
  (function g_a_s -> print_generic_ast (pr_a) (print_quoted_string) g_a_s);;

let rec print_ident = (function a_i -> print_ast (print_ident_desc) a_i)

and print_ident_desc = function
  | I_local v -> printf "@[<1>(%s@ " "I_local"; print_vname v; printf ")@]"
  | I_global (o_f, v) ->
     printf "@[<1>(%s@ " "I_global"; printf "@[<1>(";
     print_option (print_fname) o_f; printf ",@ "; print_vname v;
     printf ")@]"; printf ")@]"
  | I_method (o_c, v) ->
     printf "@[<1>(%s@ " "I_method"; printf "@[<1>(";
     print_option (print_cname) o_c; printf ",@ "; print_vname v;
     printf ")@]"; printf ")@]";;

let rec print_rep_type_def =
  (function a_r -> print_ast_doc (print_rep_type_def_desc) a_r)

and print_rep_type_def_desc = function
  | RTE_ident i ->
     printf "@[<1>(%s@ " "RTE_ident"; print_ident i; printf ")@]"
  | RTE_fun (r, r0) ->
     printf "@[<1>(%s@ " "RTE_fun"; printf "@[<1>("; print_rep_type_def r;
     printf ",@ "; print_rep_type_def r0; printf ")@]"; printf ")@]"
  | RTE_app (i, l_r) ->
     printf "@[<1>(%s@ " "RTE_app"; printf "@[<1>("; print_ident i;
     printf ",@ "; print_list (print_rep_type_def) l_r; printf ")@]";
     printf ")@]"
  | RTE_prod (r, r0) ->
     printf "@[<1>(%s@ " "RTE_prod"; printf "@[<1>("; print_rep_type_def r;
     printf ",@ "; print_rep_type_def r0; printf ")@]"; printf ")@]"
  | RTE_paren r ->
     printf "@[<1>(%s@ " "RTE_paren"; print_rep_type_def r; printf ")@]";;

let rec print_type_expr =
  (function a_t -> print_ast (print_type_expr_desc) a_t)

and print_type_expr_desc = function
  | TE_ident i -> printf "@[<1>(%s@ " "TE_ident"; print_ident i; printf ")@]"
  | TE_fun (t, t0) ->
     printf "@[<1>(%s@ " "TE_fun"; printf "@[<1>("; print_type_expr t;
     printf ",@ "; print_type_expr t0; printf ")@]"; printf ")@]"
  | TE_app (i, l_t) ->
     printf "@[<1>(%s@ " "TE_app"; printf "@[<1>("; print_ident i;
     printf ",@ "; print_list (print_type_expr) l_t; printf ")@]";
     printf ")@]"
  | TE_prod (t, t0) ->
     printf "@[<1>(%s@ " "TE_prod"; printf "@[<1>("; print_type_expr t;
     printf ",@ "; print_type_expr t0; printf ")@]"; printf ")@]"
  | TE_self -> printf "TE_self" | TE_prop -> printf "TE_prop"
  | TE_paren t ->
     printf "@[<1>(%s@ " "TE_paren"; print_type_expr t; printf ")@]";;

let rec print_constant =
  (function a_c -> print_ast (print_constant_desc) a_c)

and print_constant_desc = function
  | C_int s ->
     printf "@[<1>(%s@ " "C_int"; print_quoted_string s; printf ")@]"
  | C_float s ->
     printf "@[<1>(%s@ " "C_float"; print_quoted_string s; printf ")@]"
  | C_bool s ->
     printf "@[<1>(%s@ " "C_bool"; print_quoted_string s; printf ")@]"
  | C_string s ->
     printf "@[<1>(%s@ " "C_string"; print_quoted_string s; printf ")@]"
  | C_char c ->
     printf "@[<1>(%s@ " "C_char"; print_quoted_char c; printf ")@]";;

let rec print_rec_flag = function
  | RF_no_rec -> printf "RF_no_rec" | RF_rec -> printf "RF_rec";;

let rec print_log_flag = function
  | LF_no_log -> printf "LF_no_log" | LF_log -> printf "LF_log";;

let rec print_loc_flag = function
  | LF_no_loc -> printf "LF_no_loc" | LF_loc -> printf "LF_loc";;

let rec print_pattern = (function a_p -> print_ast (print_pat_desc) a_p)

and print_pat_desc = function
  | P_const c ->
     printf "@[<1>(%s@ " "P_const"; print_constant c; printf ")@]"
  | P_var v -> printf "@[<1>(%s@ " "P_var"; print_vname v; printf ")@]"
  | P_as (p, v) ->
     printf "@[<1>(%s@ " "P_as"; printf "@[<1>("; print_pattern p;
     printf ",@ "; print_vname v; printf ")@]"; printf ")@]"
  | P_wild -> printf "P_wild"
  | P_app (i, l_p) ->
     printf "@[<1>(%s@ " "P_app"; printf "@[<1>("; print_ident i;
     printf ",@ "; print_list (print_pattern) l_p; printf ")@]"; printf ")@]"
  | P_record l_t_l_p ->
     printf "@[<1>(%s@ " "P_record";
     print_list
      ((function (l, p) ->
         printf "@[<1>("; print_label_name l; printf ",@ "; print_pattern p;
         printf ")@]"))
      l_t_l_p;
     printf ")@]"
  | P_tuple l_p ->
     printf "@[<1>(%s@ " "P_tuple"; print_list (print_pattern) l_p;
     printf ")@]"
  | P_paren p -> printf "@[<1>(%s@ " "P_paren"; print_pattern p; printf ")@]";;

let rec print_external_language = function
  | EL_Caml -> printf "EL_Caml" | EL_Coq -> printf "EL_Coq"
  | EL_Dk -> printf "EL_Dk"
  | EL_external s ->
     printf "@[<1>(%s@ " "EL_external"; print_quoted_string s; printf ")@]";;

let rec print_external_def =
  (function a_e -> print_ast (print_external_def_desc) a_e)

and print_external_def_desc = function
  | ED_type e ->
     printf "@[<1>(%s@ " "ED_type"; print_external_def_body e; printf ")@]"
  | ED_value e ->
     printf "@[<1>(%s@ " "ED_value"; print_external_def_body e; printf ")@]"

and print_external_def_body =
  (function a_e -> print_ast (print_external_def_body_desc) a_e)

and print_external_def_body_desc = function
  {ed_name = v; ed_body = e; } ->
    printf "@[<1>{"; printf "@[<1>ed_name =@ "; print_vname v;
    printf ";@]@ "; printf "@[<1>ed_body =@ "; print_external_expr e;
    printf ";@]@ "; printf "@,}@]"

and print_external_expr =
  (function a_e -> print_ast (print_external_expr_desc) a_e)

and print_external_expr_desc =
  (function l_t_e_e ->
    print_list
     ((function (e, e0) ->
        printf "@[<1>("; print_external_language e; printf ",@ ";
        print_external_expression e0; printf ")@]"))
     l_t_e_e)

and print_external_expression = (function s -> print_quoted_string s);;

let rec print_species_def =
  (function a_s -> print_ast_doc (print_species_def_desc) a_s)

and print_species_def_desc = function
  {sd_name = s; sd_params = l_t_v_s; sd_inherits = a_l_s; sd_fields = l_s; } ->
    printf "@[<1>{"; printf "@[<1>sd_name =@ "; print_sname s;
    printf ";@]@ "; printf "@[<1>sd_params =@ ";
    print_list
     ((function (v, s0) ->
        printf "@[<1>("; print_vname v; printf ",@ ";
        print_species_param_type s0; printf ")@]"))
     l_t_v_s;
    printf ";@]@ "; printf "@[<1>sd_inherits =@ ";
    print_ast_doc (print_list (print_species_expr)) a_l_s; printf ";@]@ ";
    printf "@[<1>sd_fields =@ "; print_list (print_species_field) l_s;
    printf ";@]@ "; printf "@,}@]"

and print_species_param_type =
  (function a_s -> print_ast (print_species_param_type_desc) a_s)

and print_species_param_type_desc = function
  | SPT_in i -> printf "@[<1>(%s@ " "SPT_in"; print_ident i; printf ")@]"
  | SPT_is s ->
     printf "@[<1>(%s@ " "SPT_is"; print_species_expr s; printf ")@]"

and print_species_expr =
  (function a_s -> print_ast (print_species_expr_desc) a_s)

and print_species_expr_desc = function
  {se_name = i; se_params = l_s; } ->
    printf "@[<1>{"; printf "@[<1>se_name =@ "; print_ident i;
    printf ";@]@ "; printf "@[<1>se_params =@ ";
    print_list (print_species_param) l_s; printf ";@]@ "; printf "@,}@]"

and print_species_param =
  (function a_s -> print_ast (print_species_param_desc) a_s)

and print_species_param_desc = function
  | SP e -> printf "@[<1>(%s@ " "SP"; print_expr e; printf ")@]"

and print_sig_def = (function a_s -> print_ast_doc (print_sig_def_desc) a_s)

and print_sig_def_desc = function
  {sig_name = i; sig_type = t; } ->
    printf "@[<1>{"; printf "@[<1>sig_name =@ "; print_ident i;
    printf ";@]@ "; printf "@[<1>sig_type =@ "; print_type_expr t;
    printf ";@]@ "; printf "@,}@]"

and print_proof_def =
  (function a_p -> print_ast_doc (print_proof_def_desc) a_p)

and print_proof_def_desc = function
  {pd_name = i; pd_proof = p; } ->
    printf "@[<1>{"; printf "@[<1>pd_name =@ "; print_ident i;
    printf ";@]@ "; printf "@[<1>pd_proof =@ "; print_proof p;
    printf ";@]@ "; printf "@,}@]"

and print_property_def =
  (function a_p -> print_ast_doc (print_property_def_desc) a_p)

and print_property_def_desc = function
  {prd_name = i; prd_prop = p; } ->
    printf "@[<1>{"; printf "@[<1>prd_name =@ "; print_ident i;
    printf ";@]@ "; printf "@[<1>prd_prop =@ "; print_prop p; printf ";@]@ ";
    printf "@,}@]"

and print_species_field =
  (function a_s -> print_ast (print_species_field_desc) a_s)

and print_species_field_desc = function
  | SF_rep r ->
     printf "@[<1>(%s@ " "SF_rep"; print_rep_type_def r; printf ")@]"
  | SF_sig s -> printf "@[<1>(%s@ " "SF_sig"; print_sig_def s; printf ")@]"
  | SF_let l -> printf "@[<1>(%s@ " "SF_let"; print_let_def l; printf ")@]"
  | SF_property p ->
     printf "@[<1>(%s@ " "SF_property"; print_property_def p; printf ")@]"
  | SF_theorem t ->
     printf "@[<1>(%s@ " "SF_theorem"; print_theorem_def t; printf ")@]"
  | SF_proof p ->
     printf "@[<1>(%s@ " "SF_proof"; print_proof_def p; printf ")@]"

and print_let_def = (function a_l -> print_ast_doc (print_let_def_desc) a_l)

and print_let_def_desc = function
  {ld_rec = r; ld_log = l; ld_loc = l0; ld_bindings = l_b; } ->
    printf "@[<1>{"; printf "@[<1>ld_rec =@ "; print_rec_flag r;
    printf ";@]@ "; printf "@[<1>ld_log =@ "; print_log_flag l;
    printf ";@]@ "; printf "@[<1>ld_loc =@ "; print_loc_flag l0;
    printf ";@]@ "; printf "@[<1>ld_bindings =@ ";
    print_list (print_binding) l_b; printf ";@]@ "; printf "@,}@]"

and print_binding = (function a_b -> print_ast (print_binding_desc) a_b)

and print_binding_desc = function
  {b_name = i; b_params = l_t_i_o_t; b_type = o_t0; b_body = e; } ->
    printf "@[<1>{"; printf "@[<1>b_name =@ "; print_ident i; printf ";@]@ ";
    printf "@[<1>b_params =@ ";
    print_list
     ((function (i0, o_t) ->
        printf "@[<1>("; print_ident i0; printf ",@ ";
        print_option (print_type_expr) o_t; printf ")@]"))
     l_t_i_o_t;
    printf ";@]@ "; printf "@[<1>b_type =@ ";
    print_option (print_type_expr) o_t0; printf ";@]@ ";
    printf "@[<1>b_body =@ "; print_expr e; printf ";@]@ "; printf "@,}@]"

and print_theorem_def =
  (function a_t -> print_ast_doc (print_theorem_def_desc) a_t)

and print_theorem_def_desc = function
  {th_name = i; th_loc = l; th_stmt = p; th_proof = p0; } ->
    printf "@[<1>{"; printf "@[<1>th_name =@ "; print_ident i;
    printf ";@]@ "; printf "@[<1>th_loc =@ "; print_loc_flag l;
    printf ";@]@ "; printf "@[<1>th_stmt =@ "; print_prop p; printf ";@]@ ";
    printf "@[<1>th_proof =@ "; print_proof p0; printf ";@]@ ";
    printf "@,}@]"

and print_fact = (function a_f -> print_ast (print_fact_desc) a_f)

and print_fact_desc = function
  | F_def l_i ->
     printf "@[<1>(%s@ " "F_def"; print_list (print_ident) l_i; printf ")@]"
  | F_property l_i ->
     printf "@[<1>(%s@ " "F_property"; print_list (print_ident) l_i;
     printf ")@]"
  | F_hypothesis l_v ->
     printf "@[<1>(%s@ " "F_hypothesis"; print_list (print_vname) l_v;
     printf ")@]"
  | F_node l_n ->
     printf "@[<1>(%s@ " "F_node"; print_list (print_node_label) l_n;
     printf ")@]"

and print_proof = (function a_p -> print_ast (print_proof_desc) a_p)

and print_proof_desc = function
  | Pf_assumed -> printf "Pf_assumed"
  | Pf_auto l_f ->
     printf "@[<1>(%s@ " "Pf_auto"; print_list (print_fact) l_f; printf ")@]"
  | Pf_coq s ->
     printf "@[<1>(%s@ " "Pf_coq"; print_quoted_string s; printf ")@]"
  | Pf_dk s ->
     printf "@[<1>(%s@ " "Pf_dk"; print_quoted_string s; printf ")@]"
  | Pf_node l_p ->
     printf "@[<1>(%s@ " "Pf_node"; print_list (print_proof_node) l_p;
     printf ")@]"

and print_proof_node =
  (function a_p -> print_ast (print_proof_node_desc) a_p)

and print_proof_node_desc = function
  | PN_sub (n, s, p) ->
     printf "@[<1>(%s@ " "PN_sub"; printf "@[<1>("; print_node_label n;
     printf ",@ "; print_statement s; printf ",@ "; print_proof p;
     printf ")@]"; printf ")@]"
  | PN_qed (n, p) ->
     printf "@[<1>(%s@ " "PN_qed"; printf "@[<1>("; print_node_label n;
     printf ",@ "; print_proof p; printf ")@]"; printf ")@]"

and print_statement = (function a_s -> print_ast (print_statement_desc) a_s)

and print_statement_desc = function
  {s_hyps = l_h; s_concl = o_p; } ->
    printf "@[<1>{"; printf "@[<1>s_hyps =@ "; print_list (print_hyp) l_h;
    printf ";@]@ "; printf "@[<1>s_concl =@ "; print_option (print_prop) o_p;
    printf ";@]@ "; printf "@,}@]"

and print_hyp = (function a_h -> print_ast (print_hyp_desc) a_h)

and print_hyp_desc = function
  | H_var (v, t) ->
     printf "@[<1>(%s@ " "H_var"; printf "@[<1>("; print_vname v;
     printf ",@ "; print_type_expr t; printf ")@]"; printf ")@]"
  | H_hyp (v, p) ->
     printf "@[<1>(%s@ " "H_hyp"; printf "@[<1>("; print_vname v;
     printf ",@ "; print_prop p; printf ")@]"; printf ")@]"
  | H_not (v, e) ->
     printf "@[<1>(%s@ " "H_not"; printf "@[<1>("; print_vname v;
     printf ",@ "; print_expr e; printf ")@]"; printf ")@]"

and print_prop = (function a_p -> print_ast (print_prop_desc) a_p)

and print_prop_desc = function
  | Pr_forall (l_v, o_t, p) ->
     printf "@[<1>(%s@ " "Pr_forall"; printf "@[<1>(";
     print_list (print_vname) l_v; printf ",@ ";
     print_option (print_type_expr) o_t; printf ",@ "; print_prop p;
     printf ")@]"; printf ")@]"
  | Pr_exists (l_v, o_t, p) ->
     printf "@[<1>(%s@ " "Pr_exists"; printf "@[<1>(";
     print_list (print_vname) l_v; printf ",@ ";
     print_option (print_type_expr) o_t; printf ",@ "; print_prop p;
     printf ")@]"; printf ")@]"
  | Pr_imply (p, p0) ->
     printf "@[<1>(%s@ " "Pr_imply"; printf "@[<1>("; print_prop p;
     printf ",@ "; print_prop p0; printf ")@]"; printf ")@]"
  | Pr_or (p, p0) ->
     printf "@[<1>(%s@ " "Pr_or"; printf "@[<1>("; print_prop p;
     printf ",@ "; print_prop p0; printf ")@]"; printf ")@]"
  | Pr_and (p, p0) ->
     printf "@[<1>(%s@ " "Pr_and"; printf "@[<1>("; print_prop p;
     printf ",@ "; print_prop p0; printf ")@]"; printf ")@]"
  | Pr_equiv (p, p0) ->
     printf "@[<1>(%s@ " "Pr_equiv"; printf "@[<1>("; print_prop p;
     printf ",@ "; print_prop p0; printf ")@]"; printf ")@]"
  | Pr_not p -> printf "@[<1>(%s@ " "Pr_not"; print_prop p; printf ")@]"
  | Pr_expr e -> printf "@[<1>(%s@ " "Pr_expr"; print_expr e; printf ")@]"
  | Pr_paren p -> printf "@[<1>(%s@ " "Pr_paren"; print_prop p; printf ")@]"

and print_expr = (function a_e -> print_ast (print_expr_desc) a_e)

and print_expr_desc = function
  | E_const c ->
     printf "@[<1>(%s@ " "E_const"; print_constant c; printf ")@]"
  | E_fun (l_v, e) ->
     printf "@[<1>(%s@ " "E_fun"; printf "@[<1>(";
     print_list (print_vname) l_v; printf ",@ "; print_expr e; printf ")@]";
     printf ")@]"
  | E_var i -> printf "@[<1>(%s@ " "E_var"; print_ident i; printf ")@]"
  | E_app (e, l_e) ->
     printf "@[<1>(%s@ " "E_app"; printf "@[<1>("; print_expr e;
     printf ",@ "; print_list (print_expr) l_e; printf ")@]"; printf ")@]"
  | E_constr (e, l_e) ->
     printf "@[<1>(%s@ " "E_constr"; printf "@[<1>("; print_expr e;
     printf ",@ "; print_list (print_expr) l_e; printf ")@]"; printf ")@]"
  | E_match (e, l_t_p_e) ->
     printf "@[<1>(%s@ " "E_match"; printf "@[<1>("; print_expr e;
     printf ",@ ";
     print_list
      ((function (p, e0) ->
         printf "@[<1>("; print_pattern p; printf ",@ "; print_expr e0;
         printf ")@]"))
      l_t_p_e;
     printf ")@]"; printf ")@]"
  | E_if (e, e0, e1) ->
     printf "@[<1>(%s@ " "E_if"; printf "@[<1>("; print_expr e; printf ",@ ";
     print_expr e0; printf ",@ "; print_expr e1; printf ")@]"; printf ")@]"
  | E_let (l, e) ->
     printf "@[<1>(%s@ " "E_let"; printf "@[<1>("; print_let_def l;
     printf ",@ "; print_expr e; printf ")@]"; printf ")@]"
  | E_record l_t_l_e ->
     printf "@[<1>(%s@ " "E_record";
     print_list
      ((function (l, e) ->
         printf "@[<1>("; print_label_name l; printf ",@ "; print_expr e;
         printf ")@]"))
      l_t_l_e;
     printf ")@]"
  | E_record_access (e, l) ->
     printf "@[<1>(%s@ " "E_record_access"; printf "@[<1>("; print_expr e;
     printf ",@ "; print_label_name l; printf ")@]"; printf ")@]"
  | E_record_with (e, l_t_l_e) ->
     printf "@[<1>(%s@ " "E_record_with"; printf "@[<1>("; print_expr e;
     printf ",@ ";
     print_list
      ((function (l, e0) ->
         printf "@[<1>("; print_label_name l; printf ",@ "; print_expr e0;
         printf ")@]"))
      l_t_l_e;
     printf ")@]"; printf ")@]"
  | E_tuple l_e ->
     printf "@[<1>(%s@ " "E_tuple"; print_list (print_expr) l_e; printf ")@]"
  | E_external e ->
     printf "@[<1>(%s@ " "E_external"; print_external_expr e; printf ")@]"
  | E_paren e -> printf "@[<1>(%s@ " "E_paren"; print_expr e; printf ")@]";;

let rec print_coll_def =
  (function a_c -> print_ast_doc (print_coll_def_desc) a_c)

and print_coll_def_desc = function
  {cd_name = c; cd_body = s; } ->
    printf "@[<1>{"; printf "@[<1>cd_name =@ "; print_cname c;
    printf ";@]@ "; printf "@[<1>cd_body =@ "; print_species_expr s;
    printf ";@]@ "; printf "@,}@]";;

let rec print_type_def =
  (function a_t -> print_ast (print_type_def_desc) a_t)

and print_type_def_desc = function
  {td_name = t; td_params = l_s; td_body = t0; } ->
    printf "@[<1>{"; printf "@[<1>td_name =@ "; print_tname t;
    printf ";@]@ "; printf "@[<1>td_params =@ ";
    print_list (print_quoted_string) l_s; printf ";@]@ ";
    printf "@[<1>td_body =@ "; print_type_body t0; printf ";@]@ ";
    printf "@,}@]"

and print_type_body = (function a_t -> print_ast (print_type_body_desc) a_t)

and print_type_body_desc = function
  | TD_alias t ->
     printf "@[<1>(%s@ " "TD_alias"; print_type_expr t; printf ")@]"
  | TD_union l_t_c_l_t ->
     printf "@[<1>(%s@ " "TD_union";
     print_list
      ((function (c, l_t) ->
         printf "@[<1>("; print_constr_name c; printf ",@ ";
         print_list (print_type_expr) l_t; printf ")@]"))
      l_t_c_l_t;
     printf ")@]"
  | TD_record l_t_l_t ->
     printf "@[<1>(%s@ " "TD_record";
     print_list
      ((function (l, t) ->
         printf "@[<1>("; print_label_name l; printf ",@ ";
         print_type_expr t; printf ")@]"))
      l_t_l_t;
     printf ")@]";;

let rec print_expr_def = (function e -> print_expr e);;

let rec print_phrase = (function a_p -> print_ast (print_phrase_desc) a_p)

and print_phrase_desc = function
  | Ph_external e ->
     printf "@[<1>(%s@ " "Ph_external"; print_external_def e; printf ")@]"
  | Ph_use f -> printf "@[<1>(%s@ " "Ph_use"; print_fname f; printf ")@]"
  | Ph_open f -> printf "@[<1>(%s@ " "Ph_open"; print_fname f; printf ")@]"
  | Ph_species s ->
     printf "@[<1>(%s@ " "Ph_species"; print_species_def s; printf ")@]"
  | Ph_coll c ->
     printf "@[<1>(%s@ " "Ph_coll"; print_coll_def c; printf ")@]"
  | Ph_type t ->
     printf "@[<1>(%s@ " "Ph_type"; print_type_def t; printf ")@]"
  | Ph_let l -> printf "@[<1>(%s@ " "Ph_let"; print_let_def l; printf ")@]"
  | Ph_theorem t ->
     printf "@[<1>(%s@ " "Ph_theorem"; print_theorem_def t; printf ")@]"
  | Ph_expr e ->
     printf "@[<1>(%s@ " "Ph_expr"; print_expr_def e; printf ")@]";;

let rec print_file = (function a_f -> print_ast_doc (print_file_desc) a_f)

and print_file_desc = function
  | File l_p ->
     printf "@[<1>(%s@ " "File"; print_list (print_phrase) l_p; printf ")@]";;

(* End of Camlpp generated code *)
