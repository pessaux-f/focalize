open Parsetree
open Parsetree_utils
open Infer
open Env
open Env.TypeInformation
open Format
open Types
open Cast
  
let todo str =
  fprintf err_formatter "TODO : %s@." str
    
let fold f (ctx, res) l =
  List.fold_left 
    (fun (c, l') a -> let (c', a') = f c a in (c', l'@a')) (ctx, res) l
    
let generate_names n =
  let gen = sprintf "a%i" in
  let rec aux i acc =
    if i = 0 then (gen i)::acc
    else aux (i-1) ((gen i)::acc)
  in aux n []
    
(* let rec pp_list f ppf = function *)
(*     [] -> () *)
(*   | [e] -> f ppf e *)
(*   | h::t -> fprintf ppf "%a,@;%a" f h (pp_list f) t *)
    
(* let rec compile_species_field ctx = function *)
(*     [] -> () *)
(*   | h::t -> *)
(*       begin match h with *)
(* 	SF_sig ((_, vn, ts)) -> *)
(* 	  let name = vname_as_string_with_operators_expanded vn in *)
(* 	  let (args, ret) = Types.decompose (Types.specialize ts) in *)
(* 	  fprintf ctx.c_hdr "@\n@[%a (\*%s) (@[%a@]);@]" *)
(* 	    Types.pp_type_simple_to_c ret *)
(* 	    name *)
(* 	    (pp_list Types.pp_type_simple_to_c) args *)
(*       | SF_let _ -> *)
(* 	  fprintf err_formatter "@[TODO : SF_let@]@." *)
(*       |	SF_let_rec _ -> *)
(* 	  fprintf err_formatter "@[TODO : SF_let_rec@]@." *)
(*       |	SF_theorem _ -> () *)
(*       |	SF_property _ -> () *)
(*       end; *)
(*       compile_species_field ctx t *)
    
let compile_expr_ident ctx ei =
  match ei.ast_desc with
    EI_local vn ->
      sprintf "%s"
	(vname_as_string_with_operators_expanded vn)
  | EI_global (Vname vn) ->
      sprintf "%s_%s"
	(CGenEnv.unit_name ctx)
	(vname_as_string_with_operators_expanded vn)
  | EI_global (Qualified (f, vn)) ->
      sprintf "%s_%s"
	f
	(vname_as_string_with_operators_expanded vn)
  | EI_method (None, vn) ->
      sprintf "%s"
	(vname_as_string_with_operators_expanded vn)
  | EI_method (Some (Vname s), vn) ->
      sprintf "%s_%s_%s"
	(CGenEnv.unit_name ctx)
	(vname_as_string_with_operators_expanded s)
	(vname_as_string_with_operators_expanded vn)
  | EI_method (Some (Qualified (f, s)), vn) ->
      sprintf "%s_%s_%s"
	f
	(vname_as_string_with_operators_expanded s)
	(vname_as_string_with_operators_expanded vn)
	
let compile_constructor_ident _ ci =
  match ci.ast_desc with
    CI (Vname vn) ->
      let name = vname_as_string_with_operators_expanded vn in
      (name, String.uppercase name)
  | CI (Qualified (f, vn)) ->
      let name = vname_as_string_with_operators_expanded vn in
      (sprintf "%s_%s" f name, String.uppercase name)

let external_value ee =
  let rec search = function
      [] -> raise Not_found
    | (EL_external "C", c)::_ | (EL_external "c", c)::_ -> c
    | _::t -> search t
  in
  let code = search ee.ast_desc in
  let len = String.length code in
  let off_start = ref 0 in
  let off_end = ref (len-1) in
  while code.[!off_start] = ' ' do
    incr off_start
  done;
  while code.[!off_end] = ' ' do
    decr off_end
  done;
  String.sub code !off_start ((!off_end + 1) - !off_start)
    
exception C_def_missing of vname option * Location.t
    
let rec compile_external_binding ctx eb =
  let (vn, ee) = eb.ast_desc in
  try
    let code = external_value ee in
    (CGenEnv.add_constr ctx vn code, [])
  with
    Not_found -> raise (C_def_missing (Some vn, eb.ast_loc))
	
	
let rec inner_type t =
  match t with
    TypeId _ -> t
  | Ptr t' -> inner_type t'
  | Annot (t', _) -> inner_type t'
  | Fun _ -> t
  | Param (t', _) -> inner_type t'
  | Struct _ -> t
  | Enum _ -> t
  | Union _ -> t
	
exception Non_valid_type_identifier of string * char * Location.t
    
let compile_type ctx (_, vn, desc) =
  let name =
    (CGenEnv.unit_name ctx)^"_"^
    (vname_as_string_with_operators_expanded vn) in
  match desc.type_kind with
    TK_abstract ->
      todo "TK_abstract";
      (ctx, [])
	
  | TK_external (ee, eb) ->
      begin
	try
	  let code = external_value ee in
	  if String.contains code '*' then
	    raise (Non_valid_type_identifier (code, '*', ee.ast_loc));
	  (* Generating a warning if it is not a known type. *)
	  if
	    not
	      (CGenEnv.mem_eq ctx code ||
	      CGenEnv.is_known_type ctx code)
	  then
	    fprintf err_formatter "@[<hov 2>(Warning) %a :@;@[The type \"%s\" must be a record which first field defines the type's structural equality with this signature :@]@;@[int@ equal_%s(@[%s*,@;%s*@]);@]@]@."
	      Location.pp_location desc.type_loc
	      code code code code;
	  let res = 
	    if code = name then
	      [Export (Comment (sprintf "useless@ alias@ for@ %s" name))]
	    else
	      [Export (Typedef (TypeId code, name))] in
	  (* Now, the bindings. *)
	  if eb.ast_desc <> [] then
	    fold compile_external_binding (ctx, res) eb.ast_desc
	  else
	    (ctx, res)
	with
	  Not_found -> raise (C_def_missing (Some vn, desc.type_loc))
      end
	
  | TK_variant l ->
      let constr_names = ref [] in
      let constr_bodies = ref [] in
      let mk_sigs = ref [] in
      let mk_body = ref [] in
      let generate =
	let cpt = ref 0 in
	fun () ->
	  let res = sprintf "_%i" !cpt in
	  incr cpt;
	  res
      in
      List.iter
	(function (vn, _, t) ->
	  let cname = vname_as_string_with_operators_expanded vn in
	  constr_names := !constr_names @ [cname];
	  match Types.type_scheme_to_c t with
	    Fun (_, []) -> assert false
	  | Fun (ret, [ty]) ->
	      let label = sprintf "_%s_a0" cname in
	      let body = (label, ty) in
	      constr_bodies := !constr_bodies @ [body];
	      let cons_name = sprintf "mk_%s" cname in
	      let cons_sig = 
		Export (Signature (false, cons_name, Fun (ret, [ty]))) in
	      mk_sigs := !mk_sigs @ [cons_sig];
	      let arg_name = generate () in
	      let body = 
		[ Function (ret, cons_name, [(arg_name, ty)],
			    [ Decl ("result", ret);
			      Affect (Ident "result",
				      ECast (ret, 
					     Call (Ident "malloc",
						   [ Call (Ident "sizeof",
							   [ Prim (Ident name) ]) ])));
			      Affect (Access (Ident "result", "equal"),
				      ECast (Fun (TypeId "int",
						  [ Ptr (TypeId name);
						    Ptr (TypeId name) ]),
					     Prim (Ident "structural_equality")));
			      Affect (Access (Ident "result", "_type"),
				      Prim (Ident (String.uppercase cname)));
			      Affect (Access (Ident "result", "_arity"),
				      Prim (Constant "1"));
			      Affect (Access (Ident "result", label),
				      Prim (Ident arg_name));
			      Return (Prim (Ident "result")) ]) ] in
	      mk_body := !mk_body @ body
	  | Fun (_, args) ->
	      let body =
		("",
		 Struct (None, List.map (fun x -> (generate (), Ptr x)) args))
	      in
	      constr_bodies := !constr_bodies @ [body];
	      todo "constructor def with multiple arguments"
	  | ty ->
	      let cons_name = sprintf "mk_%s" cname in
	      let cons_sig =
		Export (Signature (true, cons_name, Fun (ty, []))) in
	      mk_sigs := !mk_sigs @ [cons_sig];
	      let local_name = sprintf "_%s" cons_name in
	      let body =
		[ Data (local_name, 
			inner_type ty,
			Concat [ Cast (Fun (TypeId "int",
					    [ Ptr (Struct (Some name, []));
					      Ptr (Struct (Some name, [])) ]),
				       Ident "structural_equality");
				 Ident (String.uppercase cname);
				 Constant "0" ]);
		  Function (ty, 
			    cons_name,
			    [],
			    [Return (Prim (Ref (Ident local_name)))]) ] in
	      mk_body := !mk_body @ body)
	l;
      let discs = List.map String.uppercase !constr_names in
      let type_def = 
	Export 
	  (Typedef 
	     (Struct
		(Some name,
		 ("equal", Fun (TypeId "int", 
				[ Ptr (Struct (Some name, []));
				  Ptr (Struct (Some name, [])) ]))::
		 ("_type", Enum (None, discs))::
		 ("_arity", TypeId "int")::
		 [("", Union (None, !constr_bodies))])
		, name)) in
      (ctx, type_def :: !mk_sigs @ !mk_body)
	
  | TK_record _ ->
      todo "TK_record";
      (ctx, [])
	
(* let rec union = function *)
(*     [] -> [] *)
(*   | []::q -> union q *)
(*   | (h::t)::q -> *)
(*       if List.exists (List.mem h) (t::q) then *)
(* 	union (t::q) *)
(*       else *)
(* 	h::(union (t::q)) *)
	
(* (\* let rec free_vars vars ctx e = *\) *)
(* (\*   match e.ast_desc with *\) *)
(* (\*     E_self -> [] *\) *)
(* (\*   | E_const _ -> [] *\) *)
(* (\*   | E_fun (args, e') -> *\) *)
(* (\*       free_vars (vars @ args) e' *\) *)
(* (\*   | E_var ({ast_desc = EI_local vn} as ei) when not (List.mem vn vars) -> *\) *)
(* (\*       [(vn, ei.ast_type)] *\) *)
(* (\*   | E_var _ -> [] *\) *)
(* (\*   | E_app (e', l) -> *\) *)
(* (\*       (free_vars vars ctx e')@ *\) *)
(* (\*       (union (List.map (free_vars vars ctx) l)) *\) *)
(* (\*   | E_constr (_, l) -> *\) *)
(* (\*       union (List.map (free_vars vars ctx) l) *\) *)
(* (\*   | E_match (e', l) -> *\) *)
	
exception Missing_type_info of Location.t
    
let anti_elim a =
  match a.ast_type with
    ANTI_non_relevant -> raise (Missing_type_info a.ast_loc)
  | ANTI_none -> raise (Missing_type_info a.ast_loc)
  | ANTI_type ts -> Types.type_simple_to_c ts
  | ANTI_scheme ts -> Types.type_scheme_to_c ts
	
(* let lambda_lift _ = *)
(* (\*   let cpt = ref 0 in *\) *)
(* (\*   let generate () = *\) *)
(* (\*     let res = sprintf "_%s_%i" name !cpt in *\) *)
(* (\*     incr cpt; *\) *)
(* (\*     res *\) *)
(* (\*   in *\) *)
(*   let rec lift _ ctx e = *)
(*     match e.ast_desc with *)
(*       E_self -> ctx *)
(*     | E_const _ -> ctx *)
(*     | E_fun _ -> *)
(* 	fprintf err_formatter "TODO : lambda lift fun@."; ctx *)
(*     | E_var _ -> ctx *)
(*     | E_app _ -> *)
(* 	fprintf err_formatter "TODO : lambda lift app@."; ctx *)
(*     | _ -> *)
(* 	fprintf err_formatter "TODO : lambda lift@."; ctx *)
(*   in *)
(*   lift *)
	
let rec result_calculus = function
    [] -> []
  | [Expr e] -> [Return e]
  | h::t -> h::(result_calculus t)
		 

	
let rec compile_let_def =
  
  (* The local variable counter. *)
  let cpt = ref 0 in
  
  (* The local variable name generator. *)
  let  generate_local_name () =
    let res = sprintf "_local_%i" !cpt in
    incr cpt;
    res
  in
  
  let rec compile_let_def' ctx (ld, l) =
    fold 
      (compile_binding ld)
      (ctx, []) 
      (List.combine 
	 ld.ast_desc.ld_bindings 
	 (List.map Types.type_scheme_to_c l))
      
  and compile_binding _ ctx (b, t) =
    (* We do not compile propositional let. *)
    match t with
      Fun (TypeId "basics_prop", _) | TypeId "basics_prop" -> (ctx, [])
    | Fun (ret, args) ->
        (* This is a function definition. *)
	let name =
	  (CGenEnv.unit_name ctx)^"_"^
	  vname_as_string_with_operators_expanded b.ast_desc.b_name in
	let nargs =
	  match b.ast_desc.b_params with
	    [] -> generate_names (List.length args -1)
	        (* We need to name the arguments.*)
	  | _ -> List.map (function (vn, _) ->
	      vname_as_string_with_operators_expanded vn) b.ast_desc.b_params in
	let (ctx, res, body, last) =
	  compile_binding_body nargs ctx b.ast_desc.b_body in
        (* The body is not directly usable, we need to do some stuff. *)
        (* Adding the result statement. *)
	let body = result_calculus (body@[last]) in
	let params = List.combine nargs args in 
	let res = res @
	  [ Export (Signature (false, name, t));
	    Function (ret, name, params, body) ] in
	(ctx, res)
	  
    | _ ->
	todo "data definition"; (ctx, [])
  
  and compile_binding_body args ctx bb =
    match bb with
      BB_logical _ ->
	todo "BB_logical"; (ctx, [], [], Skip)
    | BB_computational e ->
	compile_expr args ctx e
	
  and compile_expr args ctx e =
    match e.ast_desc with
      E_self -> todo "E_self"; (ctx, [], [], Skip)
    | E_const c ->
	compile_constant ctx c
	  
    | E_fun _ -> todo "E_fun"; (ctx, [], [], Skip)
    | E_var ei ->
	let name = compile_expr_ident ctx ei in
	(ctx, [], [], Expr (Prim (Ident name)))
	  
    | E_app (e', params) ->
      (* Checking sub-expressions. *)
	let (ctx, lifted, body, last) = compile_expr args ctx e' in
	let (ctx, lifted, body, params') =
	  List.fold_left
	    (fun (c, l, b, lst) x ->
	      match compile_expr args c x with
		(c', l', b', Expr a) -> 
		  (c', l@l', b@b', lst@[a])
	      | (c', l', b', Skip) -> 
		(* This is aan error case, It just happens as some expr are *)
		(* not compiled yet. *)
		  (c', l', b', lst)
	      | _ -> assert false)
	    (ctx, lifted, body, [])
	    params in
      (* We must check if the application is total. *)
	begin match anti_elim e with
	  Fun (_, _) ->
	    todo "E_app (lambda-lifting)"; (ctx, [], [], Skip)
	|	_ ->
	    begin match last with
	      Expr (Prim id) ->
		(ctx, lifted, body, Expr (Call (id, params')))
	    | _ ->
		todo "E_app (SSA lifting)"; (ctx, [], [], Skip)
	    end
	end
	  
    | E_constr _ -> todo "E_constr"; (ctx, [], [], Skip)
    | E_match (e', clauses) ->
	begin match compile_expr args ctx e' with
	  (ctx, lifted, body, Expr exp) ->
	    let (ctx, lifted', body', exp') = 
	      compile_clauses args exp ctx clauses in
	    (ctx, lifted @ lifted', body @ body', exp')
	|	something -> something
	end
      (* let (ctx, (lifted, stmts, cexp)) = compile_expr ctx exp in *)
(*       let (ctx, (lifted', stmts')) = compile_clauses cexp ctx l in *)
(*       (ctx, (lifted@lifted', stmts@stmts')) *)
    | E_if _ -> todo "E_if"; (ctx, [], [], Skip)
    | E_let _ -> todo "E_let"; (ctx, [], [], Skip)
    | E_record _ -> todo "E_record"; (ctx, [], [], Skip)
    | E_record_access _ -> todo "E_record_access"; (ctx, [], [], Skip)
    | E_record_with _ -> todo "E_record_with"; (ctx, [], [], Skip)
    | E_tuple l ->
      (* Getting the fields expressions. *)
	let (ctx, lifted, body, le) = List.fold_left 
	    (fun (ctx, a, b, c) d -> 
	      match compile_expr args ctx d with
		(ctx, a', b', Expr c') ->
		  (ctx, a@a', b@b', c@[c'])
	      | _ ->
		  (ctx, a, b, []))
	    (ctx, [], [], [])
	    l in
      (* Builing the corresponding tuple value. *)
	let len = List.length l in
	let local = generate_local_name () in
	let tuple_value = [
	  Decl (local, Ptr (TypeId "foc_value"));
	  Affect (Ident local,
		  Call (Ident "mk_tuple",
			[ Prim (Constant (sprintf "%i" len)) ])) ] in
	let laff =
	  let cpt = ref 0 in
	  List.map
	    (fun x -> 
	      let res =
		Expr (Call (Ident "set_tuple",
			    [ Prim (Ident local);
			      Prim (Constant (sprintf "%i" !cpt));
			      x ])) in
	      incr cpt;
	      res)
	    le in
	(ctx, lifted, body @ tuple_value @ laff, Expr (Prim (Ident local)))
	  
    | E_external ee ->
	let code = external_value ee in
	let args = List.map (fun x -> Prim (Ident x)) args in
	let code = Expr (Call (Ident code, args)) in
	(ctx, [], [], code)
    | E_paren _ -> todo "E_paren"; (ctx, [], [], Skip)
	  
  and compile_clauses args e ctx = function
      [] -> (ctx, [], [], Skip) (* Should give an error message ? *)
    | (p, exp)::t ->
	let (ctx, lifted, equations) = compile_pattern e ctx p in
	let (ctx, lifted', body', stmts) = compile_expr args ctx exp in 
	let (ctx, lifted'', body'', stmt) = compile_clauses args e ctx t in
	let rec eqs2if = function
	    [] -> Skip
	  | (body, cond)::t ->
	      let next = eq2if t in
	      body @ [If (cond, next, [])]
	in 
	(ctx, lifted@lifted'@lifted'', body @, 
	 If (condition, body'@ [stmts], body'' @ [stmt]))
	  
  and compile_pattern e ctx p =
    begin match p.ast_desc with
      P_const c ->
	begin match compile_constant ctx c with
	  (ctx, lifted, body, Expr (Prim id)) ->
	    let result =
	      Call (Access (id, "equal"), [ Expr (Prim id); e ])
	    in
	    (ctx, lifted, [([], result)])
	|	_ -> assert false
	end

    | P_var vn ->
	let name = vname_as_string_with_operators_expanded vn in
	let ty = anti_elim p in
	let result = Call (Access (id, "equal"), [ Expr (Prim id); e ]) in
	(ctx, [],
	 [ ([ Decl (name, ty); Affect (Ident name, e) ],
	    result) ])
	  
    | P_as _ ->
	todo "P_as"; (ctx, [], [])
	  
    | P_wild ->
	(ctx, [], [])
	  
    | P_constr (ci, l) ->
	let (_, disc) = compile_constructor_ident ctx ci in
	let index = ref 0 in
	let (ctx, lifted, beqs) =
	  List.fold_left
	    (fun (c, l, beqs) p ->
	      let eacc =
		Call (Ident "get_variant_arg", 
		      [e; Prim (Constant (sprintf "%i" !index)) ]) in
	      let (c, l', beq') = compile_pattern eacc c p in
	      incr index;
	      (c, l@l', beqs@beq'))
	    (ctx, [], [])
	    l 
	in
	let result = Equal (Call (Ident "get_disc", [e]), Prim (Ident disc)) in
	(ctx, lifted, ([], [result])::beqs)
	       
    | P_record _ ->
	todo "P_record"; (ctx, [], [])
	  
    | P_tuple l ->
	let index = ref 0 in
	List.fold_left
	  (fun (c, l, beq) p ->
	    let eacc =
	      Call (Ident "get_tuple", 
		    [e; Prim (Constant (sprintf "%i" !index)) ]) in
	    let (c, l', beq') = compile_pattern eacc c p in
	    incr index;
	    (c, l@l', beq@beq'))
	  (ctx, [], [])
	  l
	  
    | P_paren _ ->
	todo "P_paren"; (ctx, [], [])
    end
    


  and compile_constant ctx c =
    match c.ast_desc with
      C_int s ->
	let local = generate_local_name () in
	(ctx, [], [ Decl (local, Ptr (TypeId "foc_int"));
		    Affect (Ident local, Call (Ident "mk_foc_int",
					       [ Prim (Constant s) ])) ],
	 Expr (Prim (Ident local)))
	  
    | C_float s ->
	let local = generate_local_name () in
	(ctx, [], [ Decl (local, Ptr (TypeId "foc_float"));
		    Affect (Ident local, Call (Ident "mk_foc_float",
					       [ Prim (Constant s) ])) ],
       Expr (Prim (Ident local)))
	  
    | C_bool s ->
	let local = generate_local_name () in
	(ctx, [],
	 [ Decl (local, Ptr (TypeId "foc_bool"));
	   Affect (Ident local,
		   Call ((match s with 
		     "true" -> Ident "mk_foc_true"
		   | "false" -> Ident "mk_foc_false"
		   | _ -> assert false),
			 [])) ],
	 Expr (Prim (Ident local)))
	  
    | C_string s ->
	let local = generate_local_name () in
	let s' = sprintf "\"%s\"" (String.escaped s) in
	(ctx, [],
	 [ Decl (local, Ptr (TypeId "foc_bool"));
	   Affect (Ident local,
		   Call (Ident "mk_foc_string",
			 [ Prim (Constant s') ])) ],
	 Expr (Prim (Ident local)))

    | C_char _ ->
	todo "C_char"; (ctx, [], [], Skip)
  in
  
  compile_let_def'

(* 	P_const c -> *)
(* 	  let cte = compile_constant c in *)
(* 	  (ctx, (lifted @ lifted', [If (Equal (e, cte), body, body')])) *)
(*       |	P_var vn -> *)
(* 	  let name = vname_as_string_with_operators_expanded vn in *)
(* 	  (ctx, (lifted @ lifted', [Decl (name,])) *)
    
    
(*     [] -> (ctx, ([], [])) *)
(*   | (p, exp) :: t -> *)
(*       let (ctx, (lifted, stmts, cexp)) = compile_expr ctx exp in *)
(*       let (ctx, equation) = compile_pattern e ctx p in *)
(*       let (ctx, (lifted', stmts')) = compile_clause e ctx t in *)
(*       let code = [ If (equation, stmts  *)
(*       begin match p.ast_desc with *)
(* 	P_const c -> *)
(* 	  let (ctx, c') = compile_constant ctx c in *)
(* 	  let equation = [(e, c')] in *)
    
    
let compile_phrase ctx (ph, pcm) =
  match (ph.ast_desc, pcm) with
    (Ph_use file, PCM_no_matter) ->
      let env = cgen_open_module ~loc:ph.ast_loc file in
      (CGenEnv.merge env ctx, [Export (Include file)])
	
  | (Ph_open _, PCM_open _) ->
      (ctx, [])
	
  | (Ph_coq_require _, PCM_coq_require _) ->
      (ctx, [])
	
  | (Ph_species _, PCM_species (_, _, _)) ->
      todo "Ph_species";
      (ctx, [])
(*       fprintf ctx.c_hdr "@\n@[@[<hov 2>typedef struct {"; *)
(*       compile_species_field ctx desc.spe_sig_methods; *)
(*       fprintf ctx.c_hdr "@]@\n} %a;@]@." *)
(* 	pp_vname_with_operators_expanded sd.ast_desc.sd_name; *)
(*       ctx *)
	
  | (Ph_theorem _, PCM_theorem _) ->
      (ctx, [])
	
  | (Ph_collection _, _) ->
      todo "Ph_collection";
      (ctx, [])
(*       fprintf err_formatter "@[TODO : Ph_collection@]@."; *)
(*       ctx *)
	
  | (Ph_type td, PCM_type (vn, desc)) ->
      compile_type ctx (td, vn, desc)
	
  | (Ph_let _, PCM_let_def (ld, l)) ->
      compile_let_def ctx (ld, l)
	
  | (Ph_expr _, _) ->
      todo "Ph_expr";
      (ctx, [])
(*       fprintf err_formatter "@[TODO : Ph_expr@]@."; *)
(*       ctx *)
  | _ -> assert false
	
	
let compile file l =
  let dir = Filename.dirname file in
  let base = Filename.chop_extension (Filename.basename file) in
  (* Initializing the context. *)
  let ctx =
    CGenEnv.create
      file
      ["foc_int"; "foc_unit"; "foc_float"; "foc_char";
	"foc_string"; "foc_bool"; "foc_list"]
  in
  (* Compiling. *)
  let (ctx, phrases) = fold compile_phrase (ctx, []) l in
  let ast = (base, phrases) in
  (* Generating the code. *)
  let header = Filename.concat dir (base^".h") in
  let source = Filename.concat dir (base^".c") in
  let header_out = open_out_bin header in
  let source_out = open_out_bin source in
  try
    let header_ppf = formatter_of_out_channel header_out in
    let source_ppf = formatter_of_out_channel source_out in
    PP.file (header_ppf, source_ppf) ast;
    close_out header_out;
    close_out source_out;
    (* Finally, returning the generated context. *)
    ctx
  with
    anything ->
      begin
	close_out header_out;
	close_out source_out;
	raise anything
      end
	
	
	
	
	
