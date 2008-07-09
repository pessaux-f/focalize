open Imports;;
open Ccodegen;;

let vname2string = Parsetree_utils.vname_as_string_with_operators_expanded;;

let ident
    ?(loc = None)
    ?(file = None)
    ?(col = None)
    (ty : PCM.coq_type)
    (name : Parsetree.vname) =
  { PCM.i_info = { loc = loc };
    i_type = ty;
    i_file = file;
    i_coll = col;
    i_name = vname2string name }
;;
    
type context = {
    c_file : string
  };;

let rec local_type (t : Types.local_type) =
  match t with
    Types.Lt_var v -> PCM.T_var v
  | Types.Lt_fun (a, b) -> PCM.T_arrow (local_type a, local_type b)
  | Types.Lt_tuple l -> PCM.T_tuple (list_local_type l)
  | Types.Lt_constr ((f, n), l) ->
      PCM.T_constr ({ PCM.ti_file = f;
		      ti_name = n },
		    list_local_type l)
  | Types.Lt_self -> PCM.T_self
  | Types.Lt_species ((f, n)) -> PCM.T_species ({ PCM.ti_file = f;
						  ti_name = n })

and list_local_type (l : Types.local_type list) =
  match l with
    [] -> PCM.Coq_lt_nil
  | h::t -> PCM.Coq_lt_cons (local_type h, list_local_type t)  
;; 

let type_scheme (t : Types.type_scheme) : PCM.coq_type =
  local_type (Types.type_scheme_to_local_type t)
;;

let rec species_fields ctx (l : Env.TypeInformation.species_field list) =
  match l with
    [] -> []
  
  | (Env.TypeInformation.SF_sig ((hist, vn, ty))) :: l' ->
      let ty = type_scheme ty in
      (PCM.Sf_sig { PCM.sfi_info = {loc = None};
		    sfi_from = from ctx hist;
		    sfi_name = ident ty (vname2string vn);
		    sti_type = ty })
      ::(species_fields ctx l')
;;

let rec phrases ctx (l : Infer.please_compile_me list) =
  match l with
    [] -> []
  
  | Infer.PCM_no_matter :: l' -> phrases ctx l'
  
  | (Infer.PCM_open (loc, name)) :: l' ->
      (PCM.Phrase (PCM.Ph_open name, {loc = Some loc}))::(phrases ctx l')
  
  | (Infer.PCM_coq_require name) :: l' ->
      (PCM.Phrase (PCM.Ph_use name, {loc = None}))::(phrases ctx l')
  
  | (Infer.PCM_species (_, _, _)) :: l' ->
      (PCM.Phrase (PCM.Ph_species PCM.dummy_species, {loc = None}))
      ::(phrases ctx l')
  
  | (Infer.PCM_collection (coll, spcdef, _)) :: l' ->
      let loc = coll.Parsetree.ast_loc in
      let name = coll.Parsetree.ast_desc.Parsetree.cd_name in
      let fields = spcdef.Env.TypeInformation.spe_sig_methods in
      let name_type = (* not specified and should not be used *)
	PCM.T_species {PCM.ti_file = Some ctx.c_file;
			ti_name = vname2string name}
      in
      (PCM.Phrase
	 (PCM.Ph_collection
	    {PCM.coll_name = ident name_type name;
	      coll_body = species_fields ctx fields},
	  {loc = Some loc}))
      ::(phrases ctx l')
      
;;
      

let file (str : string) (l : Infer.please_compile_me list) =
  { file_name = str;
    file_body = (phrases str) l }
;;
