open Saving;;
open Ast;;
open Pcm;;

type ident_sort =
    Constructor
  | Identifier
  | Type
  | Species
  | File
  | Projection

type context = 
    { mutable l_unit : string;
      mutable l_loaded : (string * context ref) list;
      mutable l_let : string list;
      mutable l_species : (string * string list) list;
      mutable l_collections : (string * string list) list;
      mutable l_types : string list;
      mutable l_constr : string list;
      mutable l_projs : string list;
      mutable l_locals : string list;
      mutable l_local_fields : string list;
      mutable l_local_species : (string * string list) list;
      mutable l_local_pat : string list }
      
exception Unbound of context * ident_sort * ident
exception AlreadyBounded of ident
exception UseMissing of string
    
let empty =
  { l_unit = "";
    l_loaded = [];
    l_let = [];
    l_species = [];
    l_collections = [];
    l_types = [];
    l_constr = [];
    l_projs = [];
    l_locals = [];
    l_local_fields = [];
    l_local_species = [];
    l_local_pat = [] }
    
let merge a b =
  b.l_let <- a.l_let @ b.l_let;
  b.l_species <- a.l_species @ b.l_species;
  b.l_collections <- a.l_collections @ b.l_collections;
  b.l_types <- a.l_types @ b.l_types;
  b.l_constr <- a.l_constr @ b.l_constr;
  b.l_projs <- a.l_projs @ b.l_projs

let get_interface ctx id =
  let find x =
    try List.assoc id.ast_desc.i_name x.l_species 
    with Not_found -> raise (Unbound (x, Species, id))
  in 
  match id.ast_desc.i_file with
    "" -> find ctx.cc_env.scoping      
  | file ->
      begin
	try find !(List.assoc file ctx.cc_env.scoping.l_loaded)
	with Not_found -> raise (Unbound (ctx.cc_env.scoping, Species, id))
      end

let known_species ctx id =
  let find = List.exists (function (i, _) -> i = id.ast_desc.i_name) in
  match id.ast_desc.i_file with
    "" -> find ctx.cc_env.scoping.l_species
  | file ->
      try find !(List.assoc file ctx.cc_env.scoping.l_loaded).l_species
      with Not_found -> false
	
let known_collection ctx id =
  let find = List.exists (function (i, _) -> i = id.ast_desc.i_name) in
  match id.ast_desc.i_file with
    "" -> find ctx.cc_env.scoping.l_collections
  | file ->
      try find !(List.assoc file ctx.cc_env.scoping.l_loaded).l_collections
      with Not_found -> false

let known_type ctx id =
  let find = List.exists (function (i, _) -> i = id.ast_desc.i_name) in
  let find x =
    (* It may be in types ... *)
    (List.mem id.ast_desc.i_name x.l_types) ||
    (* ... or in collections... *)
    (find x.l_collections) ||
    (* ... or in local_species for types within species. *)
    (find x.l_local_species)
  in
  match id.ast_desc.i_file with
    "" -> find ctx.cc_env.scoping
  | file -> 
      try find !(List.assoc file ctx.cc_env.scoping.l_loaded)
      with Not_found -> false

let known_constructor ctx id =
  let find x =
    List.exists (List.mem id.ast_desc.i_name) 
      [ x.l_constr;
	List.map fst x.l_species;
	List.map fst x.l_collections;
	List.map fst x.l_local_species ] in
  match id.ast_desc.i_file with
    "" -> find ctx.cc_env.scoping
  | file ->
      begin
	try find !(List.assoc file ctx.cc_env.scoping.l_loaded)
	with Not_found -> false
      end

let known_let ctx id =
  match id.ast_desc.i_file with
    "" -> List.mem id.ast_desc.i_name ctx.cc_env.scoping.l_let
  | file ->
      try 
	List.mem 
	  id.ast_desc.i_name
	  !(List.assoc file ctx.cc_env.scoping.l_loaded).l_let
      with
	Not_found -> false

let known_projection ctx id = 
  match id.ast_desc.i_file with
    "" -> List.mem id.ast_desc.i_name ctx.cc_env.scoping.l_projs
  | file ->
      try 
	List.mem 
	  id.ast_desc.i_name
	  !(List.assoc file ctx.cc_env.scoping.l_loaded).l_projs
      with
	Not_found -> false

let known ctx id =
  let find x = 
    List.exists 
      (List.mem id.ast_desc.i_name)
      [ x.l_let;
	x.l_constr;
	x.l_projs;
	x.l_locals;
	x.l_local_pat;
	x.l_local_fields;
	(List.map fst x.l_species);
	(List.map fst x.l_collections);
	(List.map fst x.l_local_species) ]
  in
  let find_spc x spc =
    try
      List.mem 
	id.ast_desc.i_name 
	(List.assoc spc (x.l_collections @
			 x.l_local_species))
    with
      Not_found -> false
  in
  match (id.ast_desc.i_file, id.ast_desc.i_spc) with
    ("", "") | ("", "Self") -> find ctx.cc_env.scoping
  | ("", spc) -> find_spc ctx.cc_env.scoping spc
  | (file, "") | (file, "Self") ->
      begin
	try find !(List.assoc file ctx.cc_env.scoping.l_loaded)
	with Not_found -> false
      end
  | (file, spc) ->
      begin
	try find_spc !(List.assoc file ctx.cc_env.scoping.l_loaded) spc
	with Not_found -> false
      end
      

(* Some tools. *)
    
let rec discard l e =
  match (l, e) with
    [], _ -> e
  | _::q, _::q' -> discard q q'
  | _, [] -> assert false
	
(* (\* Simple equality between two idents : just check the species and name. *\) *)
(* let id_eq i1 i2 = *)
(*   ((!i1).ast_desc.i_spc = (!i2).ast_desc.i_spc) && *)
(*   ((!i1).ast_desc.i_name = (!i2).ast_desc.i_name) *)
    
(* let eq_name i1 i2 = *)
(*   ((!i1).ast_desc.i_name = (!i2).ast_desc.i_name) *)
    
(* (\* Search for identifiers. *\) *)
(* let known ctx *)
(*     ?(sort = ((global ctx.cc_env.scoping) @ *)
(* 	      ctx.cc_env.scoping.l_local @ *)
(* 	      ctx.cc_env.scoping.l_local_pat @ *)
(* 	      ctx.cc_env.scoping.l_fields))  *)
(*     id = *)
(*   (\* Checking if the identifier is qualified. *\) *)
(*   match (!id).ast_desc.i_file with *)
(*     "" ->  *)
(*       (\* If the identifier isn't qualified, we search for it in the global *\) *)
(*       (\* name context. *\) *)
(*       ((List.exists *)
(* 	  (fun x -> let res = eq_name id x in *)
(* 	  if res && (!id).ast_desc.i_file = "" then *)
(* 	    (!id).ast_desc.i_file <- (!x).ast_desc.i_file; *)
(* 	  res) sort) || *)
(*        (\* If not found, if may be a species/collection method so we loop up *\) *)
(*        (\* the tables. *\) *)
(* 	  (let spc = (!id).ast_desc.i_spc in *)
(* 	  try *)
(* 	    let interface =  *)
(* 	      snd (List.find  *)
(* 		     (function (spc_id, _) -> (!spc_id).ast_desc.i_spc = spc) *)
(* 		     (ctx.cc_env.scoping.l_local_table @ *)
(* 		      ctx.cc_env.scoping.l_table)) in *)
(* 	    List.exists (eq_name id) interface *)
(* 	  with *)
(* 	    Not_found -> false)) *)
(*   | file -> *)
(*       (\* If the name is qualified, we search for it in the corresponding *\) *)
(*       (\* scoping context. If search failed, compilation is aborded. *\) *)
(*       begin  *)
(* 	try  *)
(* 	  let file_ctx = List.assoc file ctx.cc_env.scoping.l_loaded in *)
(* 	  List.exists (id_eq id) (global !file_ctx) *)
(* 	with *)
(* 	  Not_found -> false *)
(*       end *)
	
	
(* Core stuff. *)
let rec pattern ctx p =
  match p.ast_desc with
    P_const _ -> []
  | P_var id ->
      (* Display a warning (if enabled) if the name is already introduced. *)
      if List.mem Wall ctx.cc_options && known ctx id then
	Format.fprintf Format.err_formatter
	  "@[<hov 2>%a :@;@[Warning (scoping) : %a hides a previous value.@]@]@."
	  Location.pp_location id.ast_loc
	  Pcm.PP.ident id;
      (* Saving the name by returning it. *)
      [id.ast_desc.i_name] 
  | P_wild -> []
  | P_as (p', id) ->
      (* Same as P_var. *)
      if List.mem Wall ctx.cc_options && known ctx id then
	Format.fprintf Format.err_formatter
	  "@[<hov 2>%a :@;@[Warning (scoping) : %a hides a previous value.@]@]@."
	  Location.pp_location id.ast_loc
	  Pcm.PP.ident id;
      (* Checking the sub-pattern and returning all names introduced. *)
      (pattern ctx p') @ [id.ast_desc.i_name]
  | P_constr (id, l) ->
      (* Checking if the constructor exists. *)
      if not (known_constructor ctx id) then
	raise (Unbound (ctx.cc_env.scoping,  Constructor, id));
      (* Checking for arguments. *)
      List.flatten (List.map (pattern ctx) l)
  | P_record l ->
      (* Checking the field patterns. *)
      List.flatten (List.map (pattern ctx) (List.map snd l))
  | P_tuple l ->
      (* Checking all the patterns of the tuple. *)
      List.flatten (List.map (pattern ctx) l)
  | P_paren p' ->
      pattern ctx p'
	
let rec type_expr ctx t =
  match t.ast_desc with
    TE_var _ -> ()
  | TE_ident id ->
      if not (known_type ctx id) then
	raise (Unbound (ctx.cc_env.scoping,  Type, id))
  | TE_self -> ()
  | TE_prop -> ()
  | TE_fun (l, r) ->
      type_expr ctx l;
      type_expr ctx r
  | TE_app (id, l) ->
      (* Checking the type name. *)
      if not (known_type ctx id) then
	raise (Unbound (ctx.cc_env.scoping,  Type, id));
      (* Checking the params. *)
      List.iter (type_expr ctx) l
  | TE_tuple l ->
      List.iter (type_expr ctx) l
  | TE_paren t' ->
      type_expr ctx t'
	
let binding ctx f b =
  (* Introducing params. *)
  ctx.cc_env.scoping.l_locals <-
    (List.map (fun x -> x.ast_desc.i_name) b.ast_desc.b_params) @
    ctx.cc_env.scoping.l_locals;
  (* Checking the parameter's spec. *)
  List.iter
    (function
	None -> ()
      | Some t -> type_expr ctx t)
    ((fst b.ast_desc.b_type) @ [snd b.ast_desc.b_type]);
  (* Checking the body *)
  f ctx b.ast_desc.b_body;
  (* Discarded parameters. *)
  ctx.cc_env.scoping.l_locals <-
    discard b.ast_desc.b_params ctx.cc_env.scoping.l_locals
      
      
let rec expr ctx e =
  match e.ast_desc with
    E_self -> ()
  | E_constant _ -> ()
  | E_fun (l, e') ->
      (* Adding the arguments to local env. *)
      ctx.cc_env.scoping.l_locals <- 
	(List.map (fun x -> x.ast_desc.i_name) l) @ ctx.cc_env.scoping.l_locals;
      (* Checking the sub-expression. *)
      expr ctx e';
      (* When done, discarding arguments. *)
      ctx.cc_env.scoping.l_locals <- discard l ctx.cc_env.scoping.l_locals
  | E_var id ->
      if not (known ctx id) then
	raise (Unbound (ctx.cc_env.scoping,  Identifier, id))
  | E_app (e', args) ->
      expr ctx e';
      List.iter (expr ctx) args
  | E_constr (id, args) ->
      (* Checking the constructor id. *)
      if not (known_constructor ctx id) then
	raise (Unbound (ctx.cc_env.scoping,  Constructor, id));
      (* If it's ok, we continue... *)
      List.iter (expr ctx) args
  | E_match (e', l) ->
      (* Checking the expr. *)
      expr ctx e';
      (* And the clauses. *)
      List.iter
	(function (p, x) ->
	  (* Pattern may introduce variables available in the right part of *)
	  (* the clause so thay don't discard them. In order to know what *)
	  (* names were introduced, they are stored in l_local_pat. *)
	  let vars = pattern ctx p in
	  ctx.cc_env.scoping.l_local_pat <-
	    vars @ ctx.cc_env.scoping.l_local_pat;
	  expr ctx x;
	  (* Now we can discard the names introduced by the pattern. *)
	  ctx.cc_env.scoping.l_local_pat <- 
	    discard vars ctx.cc_env.scoping.l_local_pat)
	l
  | E_if (a, b, c) ->
      (* Checking all expressions. *)
      expr ctx a;
      expr ctx b;
      expr ctx c
  | E_let (ld, e') ->
      (* If the let is recursive then we introduce the names before *)
      (* analysing the bodies. If not, names are introduced after in the *)
      (* OCaml way. *)
      let names =
	List.map (fun x -> x.ast_desc.b_name) ld.ast_desc.ld_bindings in
      if ld.ast_desc.ld_rec then
	begin
	  ctx.cc_env.scoping.l_locals <-
	    (List.map (fun x -> x.ast_desc.i_name) names) @
	    ctx.cc_env.scoping.l_locals;
	  List.iter (binding ctx expr) ld.ast_desc.ld_bindings
	end
      else
	begin
	  List.iter (binding ctx expr) ld.ast_desc.ld_bindings;
	  ctx.cc_env.scoping.l_locals <-
	    (List.map (fun x -> x.ast_desc.i_name) names) @
	    ctx.cc_env.scoping.l_locals
	end;
      (* Checking the sub-expression. *)
      expr ctx e';
      (* Now we can discard the name introduced. *)
      ctx.cc_env.scoping.l_locals <- discard names ctx.cc_env.scoping.l_locals
  | E_logical_let (ld, e') ->
      (* Same as E_let. *)
      let names =
	List.map (fun x -> x.ast_desc.b_name) ld.ast_desc.ld_bindings in
      if ld.ast_desc.ld_rec then
	begin
	  ctx.cc_env.scoping.l_locals <-
	    (List.map (fun x -> x.ast_desc.i_name) names) @
	    ctx.cc_env.scoping.l_locals;
	  List.iter (binding ctx logical_expr) ld.ast_desc.ld_bindings
	end
      else
	begin
	  List.iter (binding ctx logical_expr) ld.ast_desc.ld_bindings;
	  ctx.cc_env.scoping.l_locals <-
	    (List.map (fun x -> x.ast_desc.i_name) names) @
	    ctx.cc_env.scoping.l_locals
	end;
      (* Checking the sub-expression. *)
      expr ctx e';
      (* Now we can discard the name introduced. *)
      ctx.cc_env.scoping.l_locals <- discard names ctx.cc_env.scoping.l_locals
  | E_record l ->
      (* Checking sub-expressions in fields. *)
      List.iter (expr ctx) (List.map snd l)
  | E_record_access (e', id) ->
      (* Checking the sub-expression. *)
      expr ctx e';
      (* And if the projection is known. *)
      if not (known_projection ctx id) then
	raise (Unbound (ctx.cc_env.scoping, Projection, id))
  | E_record_with (e', l) ->
      (* Same as E_record_acces and E_record. *)
      expr ctx e';
      List.iter (expr ctx) (List.map snd l)
  | E_tuple l ->
      (* Checking all tuple's sub-expressions. *)
      List.iter (expr ctx) l
  | E_external _ -> ()
  | E_paren e' ->
      (* Checking the sub-expression. *)
      expr ctx e'
	
	
and logical_expr _ _ = ()
    
    
(* let rec field_merge x y = *)
(*   match x with *)
(*     [] -> y *)
(*   | h::t ->  *)
(*       if List.exists (eq_name h) t then field_merge t y *)
(*       else h::(field_merge t y) *)
		
let sig_def ctx sd =
  let name = sd.ast_desc.sig_name in
  (* We check the type. *)
  type_expr ctx sd.ast_desc.sig_type;
  (* If all is ok, we introduced the name in the current field names. *)
  ctx.cc_env.scoping.l_local_fields <-
    name.ast_desc.i_name :: ctx.cc_env.scoping.l_local_fields
      
      
let species_field ctx sf =
  match sf.ast_desc with
    Sf_rep t -> type_expr ctx t
  | Sf_sig sd -> sig_def ctx sd
  | Sf_let ld ->
      (* Same as other let_def but instead of using the local env, we use *)
      (* the local fields env. *)
      let names =
	List.map (fun x -> x.ast_desc.b_name) ld.ast_desc.ld_bindings in
      if ld.ast_desc.ld_rec then
	begin
	  ctx.cc_env.scoping.l_local_fields <-
	    (List.map (fun x -> x.ast_desc.i_name) names) @
	    ctx.cc_env.scoping.l_local_fields;
	  List.iter (binding ctx expr) ld.ast_desc.ld_bindings
	end
      else
	begin
	  List.iter (binding ctx expr) ld.ast_desc.ld_bindings;
	  ctx.cc_env.scoping.l_local_fields <-
	    (List.map (fun x -> x.ast_desc.i_name) names) @
	    ctx.cc_env.scoping.l_local_fields
	end
  | Sf_logical_let ld ->
      (* Same as other let_def but instead of using the local env, we use *)
      (* the local fields env. *)
      let names =
	List.map (fun x -> x.ast_desc.b_name) ld.ast_desc.ld_bindings in
      if ld.ast_desc.ld_rec then
	begin
	  ctx.cc_env.scoping.l_local_fields <-
	    (List.map (fun x -> x.ast_desc.i_name) names) @
	    ctx.cc_env.scoping.l_local_fields;
	  List.iter (binding ctx logical_expr) ld.ast_desc.ld_bindings
	end
      else
	begin
	  List.iter (binding ctx logical_expr) ld.ast_desc.ld_bindings;
	  ctx.cc_env.scoping.l_local_fields <-
	    (List.map (fun x -> x.ast_desc.i_name) names) @
	    ctx.cc_env.scoping.l_local_fields
	end
  | Sf_theorem td ->
      if List.mem Verbose ctx.cc_options then
	Format.fprintf Format.err_formatter
	  "@[<hov 2>%a :@;@[Warning (scoping - TODO) : Pcm.Sf_theorem.@]@]@."
	  Location.pp_location td.ast_loc
  | Sf_property pd ->
      if List.mem Verbose ctx.cc_options then
	Format.fprintf Format.err_formatter
	  "@[<hov 2>%a :@;@[Warning (scoping - TODO) : Pcm.Sf_property.@]@]@."
	  Location.pp_location pd.ast_loc
  | Sf_proof pd ->
      if List.mem Verbose ctx.cc_options then
	Format.fprintf Format.err_formatter
	  "@[<hov 2>%a :@;@[Warning (scoping - TODO) : Pcm.Sf_proof.@]@]@."
	  Location.pp_location pd.ast_loc
	  
	  
let rec get_species_id = function
    [] -> []
  | h::t ->
      begin match h.ast_desc with
      	E_var id -> id::(get_species_id t)
      | E_app (e, _) -> (get_species_id [e])@(get_species_id t)
      |	E_self | E_constant _ |	E_fun _ | E_constr _
      |	E_match _ | E_if _ | E_let _ | E_logical_let _
      |	E_record _ | E_record_access _ | E_record_with _
      |	E_tuple _ | E_external _ | E_paren _ -> []
      end
	
	
(* let inherits_merge l = *)
(*   List.fold_right field_merge [] l *)
    
let species_def ctx sd =
  if (not (List.mem Redef ctx.cc_options) && 
      known_species ctx sd.ast_desc.s_name) then
    raise (AlreadyBounded sd.ast_desc.s_name);
  (* Since species are not recursive, we check its body first. *)
  (* Scoping the species's arguments. *)
  List.iter
    (function (id, spec) ->
      (* We check the species expression associated. *)
      begin match spec.ast_desc with
	Sp_in e -> 
	  expr ctx e;
	  (* And we introduce the name into local context. *)
	  ctx.cc_env.scoping.l_locals <-
	    id.ast_desc.i_name :: ctx.cc_env.scoping.l_locals
      | Sp_is e ->
	  (* If it is a species, we check the expression but we add its *)
	  (* pseudo-collection to the context. *)
	  expr ctx e;
	  begin match get_species_id [e] with
	    [spc] ->
	      (* Getting the species interface. *)
	      begin
		try
		  let interface = get_interface ctx spc in
		  ctx.cc_env.scoping.l_local_species <-
		    (id.ast_desc.i_name, interface) ::
		    ctx.cc_env.scoping.l_local_species
		with
		  Not_found -> assert false
	      end
	  | _ -> assert false
	  end
      end)
    (List.combine sd.ast_desc.s_params sd.ast_desc.s_spec);
  (* Scoping the inheritance clause. *)
  List.iter (expr ctx) sd.ast_desc.s_inh;
  (* Now, we resolve the inheritance for scoping purpose (this is not a true *)
  (* resolution as don't override fields). *)
  let inhs = get_species_id sd.ast_desc.s_inh in
  ctx.cc_env.scoping.l_local_fields <-
    List.flatten (List.map (get_interface ctx) (List.rev inhs));
  (* Now the body. *)
  List.iter (species_field ctx) sd.ast_desc.s_fields;
  (* If all is ok, we clean the local environment and add the species into *)
  (* known species. *)
  ctx.cc_env.scoping.l_species <-
    (sd.ast_desc.s_name.ast_desc.i_name, ctx.cc_env.scoping.l_local_fields)
    :: ctx.cc_env.scoping.l_species;
  (* Drop the field and local table contexts. *)
  ctx.cc_env.scoping.l_locals <- [];
  ctx.cc_env.scoping.l_local_fields <- []
      
      
let external_binding ctx eb =
  let (id, _) = eb.ast_desc in
  if not (known_constructor ctx id) then
    raise (Unbound (ctx.cc_env.scoping,  Constructor, id))
      
      
let rec type_kind ctx tk =
  match tk.ast_desc with
    Tk_external (t, _, l) ->
      (* Checking the internal type representation. *)
      begin match t with
	None -> ()
      |	Some ti -> type_kind ctx ti
      end;
      (* Checking if the external bindings names are known. *)
      List.iter (external_binding ctx) l
  | Tk_inductive l ->
      List.iter
	(function (id, lt) ->
	  if (not (List.mem Redef ctx.cc_options) &&
	      known_constructor ctx id) then
	    raise (AlreadyBounded id);
	  (* Checking the arguments. *)
	  List.iter (type_expr ctx) lt;
	  (* Introducing the identifier. *)
	  ctx.cc_env.scoping.l_constr <-
	    id.ast_desc.i_name :: ctx.cc_env.scoping.l_constr)
	l
  | Tk_record l ->
      List.iter
	(function (id, t) ->
	  if (not (List.mem Redef ctx.cc_options) &&
	      known_projection ctx id) then
	    raise (AlreadyBounded id);
	  (* Checking the field's type. *)
	  type_expr ctx t;
	  (* Introducing the projector. *)
	  ctx.cc_env.scoping.l_projs <-
	    id.ast_desc.i_name :: ctx.cc_env.scoping.l_projs)
	l
  | Tk_alias t ->
      type_expr ctx t
	
let type_def ctx td =
  (* Checking the name. *)
  let name = td.ast_desc.type_name in
  if (not (List.mem Redef ctx.cc_options) &&
      known_type ctx name) then
    raise (AlreadyBounded name);
  (* Adding the name to the types names as types are potentially recursive *)
  (* by default. *)
  ctx.cc_env.scoping.l_types <-
    name.ast_desc.i_name :: ctx.cc_env.scoping.l_types;
  (* We do not check parameters as they are introducing polymorphic names but *)
  (* we add them to local names. *)
  ctx.cc_env.scoping.l_types <-
    (List.map (fun x -> x.ast_desc.i_name) td.ast_desc.type_params) @ 
    ctx.cc_env.scoping.l_types;
  (* Checking the body. *)
  type_kind ctx td.ast_desc.type_body;
  (* And we can discard parameters. *)
  ctx.cc_env.scoping.l_types <-
    discard td.ast_desc.type_params ctx.cc_env.scoping.l_types
      
      
let phrases ctx ph =
  match ph.ast_desc with
    Ph_open unit ->
      if unit <> ctx.cc_env.scoping.l_unit then
	begin try
	(* Merging the unit's global context into the current global context. *)
	  merge
	    (!(List.assoc unit ctx.cc_env.scoping.l_loaded)) ctx.cc_env.scoping
	with
	  Not_found ->
	  (* Experimental : Auto use the unit. *)
	    if List.mem Auto_use ctx.cc_options then
	      let data = load ctx unit in
	      ctx.cc_env.scoping.l_loaded <-
		(unit, ref data.d_env.scoping) :: ctx.cc_env.scoping.l_loaded;
	      merge data.d_env.scoping ctx.cc_env.scoping
	    else
	    (* Traditionnal behavior : compilation is aborted. *)
	      raise (UseMissing unit)
	end
  | Ph_use unit ->
      if unit <> ctx.cc_env.scoping.l_unit then
        (* Checking if the file isn't already loaded. *)
	if not (List.exists
		  (function (f, _) -> f = unit)
		  ctx.cc_env.scoping.l_loaded) then
	  let data = load ctx unit in
	  ctx.cc_env.scoping.l_loaded <-
	    (unit, ref data.d_env.scoping) :: ctx.cc_env.scoping.l_loaded
  | Ph_require (_, _) -> ()
  | Ph_species sd -> species_def ctx sd
  | Ph_collection (id, e) ->
      (* If Redef option is no present and the name already exists then *)
      (* the compilation fails. *)
      if (not (List.mem Redef ctx.cc_options) && known_collection ctx id) then
	raise (AlreadyBounded id);
      (* Checking the body. *)
      expr ctx e;
      (* Getting the interface. *)
      begin match get_species_id [e] with
	[spc] ->
	  begin
	    try
	      (* Introducing the new collection. *)
	      let interface = get_interface ctx spc in
	      ctx.cc_env.scoping.l_collections <-
		(id.ast_desc.i_name, interface) ::
		ctx.cc_env.scoping.l_collections
	    with
	      Not_found -> assert false
	  end
      | _ -> assert false
      end
  | Ph_type td ->
      type_def ctx td
  | Ph_let ld ->
      (* Same as E_let without discarding the names introduced since they are *)
      (*  declared in the global level. *)
      let names =
	List.map (fun x -> x.ast_desc.b_name) ld.ast_desc.ld_bindings in
      (* We check that names weren't previously defined. *)
      if not (List.mem Redef ctx.cc_options) then
	List.iter
	  (fun x -> if known_let ctx x then
	    raise (AlreadyBounded x))
	  names;
      if ld.ast_desc.ld_rec then
	begin
	  ctx.cc_env.scoping.l_let <-
	    (List.map (fun x -> x.ast_desc.i_name) names) @
	    ctx.cc_env.scoping.l_let;
	  List.iter (binding ctx expr) ld.ast_desc.ld_bindings
	end
      else
	begin
	  List.iter (binding ctx expr) ld.ast_desc.ld_bindings;
	  ctx.cc_env.scoping.l_let <-
	    (List.map (fun x -> x.ast_desc.i_name) names) @
	    ctx.cc_env.scoping.l_let
	end
  | Ph_logical_let ld ->
      (* Same as Ph_let for logical_expr. *)
      let names =
	List.map (fun x -> x.ast_desc.b_name) ld.ast_desc.ld_bindings in
      (* We check that names weren't previously defined. *)
      if not (List.mem Redef ctx.cc_options) then
	List.iter
	  (fun x -> if known_let ctx x then
	    raise (AlreadyBounded x))
	  names;
      if ld.ast_desc.ld_rec then
	begin
	  ctx.cc_env.scoping.l_let <-
	    (List.map (fun x -> x.ast_desc.i_name) names) @
	    ctx.cc_env.scoping.l_let;
	  List.iter (binding ctx logical_expr) ld.ast_desc.ld_bindings
	end
      else
	begin
	  List.iter (binding ctx logical_expr) ld.ast_desc.ld_bindings;
	  ctx.cc_env.scoping.l_let <-
	    (List.map (fun x -> x.ast_desc.i_name) names) @
	    ctx.cc_env.scoping.l_let
	end
  | Ph_theorem td ->
      if List.mem Verbose ctx.cc_options then
	Format.fprintf Format.err_formatter
	  "@[<hov 2>%a :@;@[Warning (scoping - TODO) : Pcm.Ph_theorem.@]@]@."
	  Location.pp_location td.ast_loc
  | Ph_expr e ->
      expr ctx e
	
let file ctx f =
  (* Setting the current unit. *)
  ctx.cc_env.scoping.l_unit <- f.ast_desc.file_name;
  (* Adding its own reference to the loaded unit to simplify the searching *)
  (* for known names. *)
  ctx.cc_env.scoping.l_loaded <-
    (f.ast_desc.file_name, ref ctx.cc_env.scoping) ::
    ctx.cc_env.scoping.l_loaded;
  (* Checking phrases (with basics automatically loaded). *)
  List.iter
    (phrases ctx)
    ((mk_ast f.ast_loc () (Ph_use "basics")) :: f.ast_desc.file_body)
    
    
    
