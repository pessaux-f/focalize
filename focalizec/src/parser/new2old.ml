(* *************************************************************** *)
(*  [Fun] process_generic_ast_desc :                               *)
(*         ('a -> 'b) -> ('a, 'c) Parsetree.generic_ast -> 'b      *)
(** [Descr] : Wrapper to apply processing only on the 'desc' field
              of a generic_ast. Ignores all other fields.          *)
(* *************************************************************** *)
let process_generic_ast_desc f ga = f ga.Parsetree.ast_desc ;;


let downgrade_ident ident_desc = failwith "TODO" ;;



let downgrade_species_param_type_desc name = function
  | Parsetree.SPT_in ident ->
      (* Entity parameter. *)
      Ast_types.Ent (name, downgrade_ident ident)
  | Parsetree.SPT_is species_expr ->
      (* Collection parameter. *)
      Ast_types.Collec (name, 
;;
let downgrade_species_param_type name ptype = 
  process_generic_ast_desc downgrade_species_param_type_desc
;;



let downgrade_species_desc_desc species_def =
  let prms =
    List.map
      (fun (name, ptype) -> downgrade_species_param_type name ptype)
      species_def.Parsetree.sd_params in
  let inheritance = failwith "TODO" in
  let fields = failwith "TODO" in
  Typed_elt.Spec (species_def.Parsetree.sd_name, prms, inheritance, fields)
;;
let downgrade_species_def =
  process_generic_ast_desc downgrade_species_desc_desc
;;



let downgrade_external_desc ext_desc = failwith "TODO" ;;



let downgrade_external = process_generic_ast_desc downgrade_external_desc ;;



(* ************************************************************************* *)
(*  [Fun] downgrade_phrase_desc : Parsetree.phrase_desc -> Typed_elt.command *)
(* ************************************************************************* *)
let downgrade_phrase_desc = function
  | Parsetree.Ph_external external_def -> downgrade_external external_def
  | Parsetree.Ph_use fname -> Typed_elt.Uses fname
  | Parsetree.Ph_open fname -> Typed_elt.Open fname
  | Parsetree.Ph_species species_def -> downgrade_species_def species_def
  | Parsetree.Ph_coll coll_def -> failwith "TODO"
  | Parsetree.Ph_type type_def -> failwith "TODO"
  | Parsetree.Ph_let let_def -> failwith "TODO"
  | Parsetree.Ph_theorem theorem -> failwith "TODO"
  | Parsetree.Ph_expr expr -> failwith "TODO"
;;
let downgrade_phrase = process_generic_ast_desc downgrade_phrase_desc ;;
  
