open Ast;;
open Pcm;;

type context =
    { c_lang : string }
;;

exception MissingExternalBinding of string * Location.t option;;

let binding ctx f b =
  let (_, _, body) = b.ast_desc in
  f ctx body
;;

let rec expr ctx e =
  match e.ast_desc with
    E_self -> ()
  | E_constant _ -> ()
  | E_fun (_, e') -> expr ctx e'
  | E_var _ -> ()
  | E_app (e', args) -> expr ctx e'; List.iter (expr ctx) args
  | E_constr (_, args) -> List.iter (expr ctx) args
  | E_match (e', l) ->
      expr ctx e'; List.iter (function (_, b) -> expr ctx b) l
  | E_if (a, b, c) -> List.iter (expr ctx) [a; b; c]
  | E_let (_, l, e') -> List.iter (binding ctx expr) l; expr ctx e'
  | E_logical_let (_, l, e') -> 
      List.iter (binding ctx logical_expr) l; expr ctx e'
  | E_record l ->
      List.iter (function (_, x) -> expr ctx x) l
  | E_record_access (e', _) -> expr ctx e'
  | E_record_with (e', l) ->
      expr ctx e'; List.iter (function (_, x) -> expr ctx x) l
  | E_tuple l -> List.iter (expr ctx) l
  | E_external ee ->
      begin try 
	e.ast_desc <-
	  E_var { ast_desc = { i_file = "";
			       i_spc = "";
			       i_name = List.assoc ctx.c_lang ee.ast_desc };
		  ast_type = e.ast_type;
		  ast_loc = ee.ast_loc }
      with Not_found -> raise (MissingExternalBinding (ctx.c_lang, e.ast_loc))
      end
  | E_paren e' -> expr ctx e'

and logical_expr ctx le =
  match le.ast_desc with
    Le_forall (_, e) -> logical_expr ctx e
  | Le_exists (_, e) -> logical_expr ctx e
  | Le_imply (a, b)
  | Le_or (a, b)
  | Le_and (a, b)
  | Le_equiv (a, b) -> logical_expr ctx a; logical_expr ctx b
  | Le_not e -> logical_expr ctx e
  | Le_expr e -> expr ctx e
  | Le_paren e -> logical_expr ctx e
;; 
      

let species_field ctx f =
  match f.ast_desc with
    Sf_sig _ -> ()
  | Sf_let (_, l) ->
      List.iter (binding ctx (fun x (e, _) -> expr x e)) l
  | Sf_logical_let (_, l) ->
      List.iter (binding ctx (fun x (le, _) -> logical_expr x le)) l
  | Sf_theorem (_, le, _, _) -> logical_expr ctx le
  | Sf_property (_, le, _) -> logical_expr ctx le
;;

let type_def _ _ =
  Format.printf "TODO : extelim.type_def@.";
  ()
;;

let phrase ctx ph =
  match ph.ast_desc with
    Ph_open _ -> ()
  | Ph_use _ -> ()
  | Ph_species (_, fields) -> List.iter (species_field ctx) fields
  | Ph_collection (_, fields) -> List.iter (species_field ctx) fields
  | Ph_type td -> type_def ctx td
  | Ph_let (_, l) -> List.iter (binding ctx expr) l
  | Ph_logical_let (_, l) -> List.iter (binding ctx logical_expr) l
  | Ph_theorem (_, log, _) -> logical_expr ctx log
  | Ph_expr e -> expr ctx e
;;

let file ctx f =
  List.iter (phrase ctx) f.file_body
;; 
