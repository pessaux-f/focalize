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
    
let rec subst_ctype t t' ty =
  if ty = t then t'
  else match ty with
    Fun (a, l) -> Fun (subst_ctype t t' a, List.map (subst_ctype t t') l)
  | Param (a, l) -> Param (subst_ctype t t' a, List.map (subst_ctype t t') l)
  | Struct (s, l) -> 
      Struct (s, List.map (fun (id, a) -> (id, subst_ctype t t' a)) l)
  | Union (s, l) -> 
      Union (s, List.map (fun (id, a) -> (id, subst_ctype t t' a)) l)
  | Ptr a -> Ptr (subst_ctype t t' a)
  | _ -> ty
	
let rec type_realize ctx t =
  match t with
    TypeId _ -> CGenEnv.normalize_type ctx t
  | Ptr _ -> t
  | Annot (t', _) -> type_realize ctx t'
  | Fun _ -> t
  | Param _ -> t
  | Struct _ -> t
  | Enum _ -> t
  | Union _ -> t

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
	
let compile_constructor_ident ctx ci =
  match ci.ast_desc with
    CI (Vname vn) ->
      (* Getting the C definition of the constructor if any. *)
      let name =
	try 
	  CGenEnv.get_constr ctx vn
	with Not_found ->
	  vname_as_string_with_operators_expanded vn
      in
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
	  (* Memorising the implicit type aliasing. *)
	  let original = TypeId code in
	  let alias = TypeId name in
	  let ctx =
	    if code <> name then
	      CGenEnv.add_alias ctx alias original
	    else ctx in
	  let res = 
	    if code = name then
	      [Export (Comment (sprintf "useless@ alias@ for@ %s" name))]
	    else
	      [Export (Typedef (original, name))]
	  in
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
	      let cons_name = sprintf "%s_%s" (CGenEnv.unit_name ctx) cname in
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
		 (match !constr_bodies with
		   [] -> []
		 | bodies ->
		     [("", Union (None, bodies))]))
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
	
let result_calculus ty l =
  let rec result_calculus' = function
      [] -> []
    | [Expr e] -> [Affect (Ident "_result", ECast (ty, e))]
    | (If (c, a, b))::t ->
	(If (c, result_calculus' a, result_calculus' b)) ::
	(result_calculus' t)
    | h::t -> h::(result_calculus' t)
  in
  (Decl ("_result", ty)) :: 
  (result_calculus' l)@
  [Return (Prim (Ident "_result"))]

		 
let rec skip_elimination = function
    [] -> []
  | Skip :: t -> skip_elimination t
  | (If (c, a, b)) :: t ->
      (If (c, skip_elimination a, skip_elimination b)) ::
      (skip_elimination t)
  | h::t ->
      h::(skip_elimination t)
	   
	   
	   
  (* The local variable name generator. *)
let  generate_local_name =
    (* The local variable counter. *)
  let cpt = ref 0 in
  (fun () ->
    let res = sprintf "_local_%i" !cpt in
    incr cpt;
    res)
    
let rec compile_let_def ctx (ld, l) =
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
      let body = (body@[last]) in
      let body = skip_elimination body in
      let body = result_calculus ret body in
      let params = List.combine nargs args in 
      let res = res @
	[ Export (Signature (false, name, t));
	  Function (ret, name, params, body) ] in
      (* Add the function's type into context. *)
      let ctx = CGenEnv.add_function ctx name t in
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
	
  | E_constr (ci, params) -> 
      let (name, _) = compile_constructor_ident ctx ci in
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
	  (ctx, [], [], [])
	  params in
      (ctx, lifted, body, Expr (Call (Ident name, params'))) 

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
      let (ctx, lifted', body', stmt) = compile_expr args ctx exp in 
      let (ctx, lifted'', body'', stmt') = compile_clauses args e ctx t in
      let rec eqs2if next = function
	  [] -> body' @ [stmt]
	| (body, cond)::t ->
	    body @ [If (cond, eqs2if next t, next)]
      in 
      (ctx, lifted@lifted'@lifted'',
       eqs2if (body'' @ [stmt']) equations, Skip)
	
and compile_pattern e ctx p =
  begin match p.ast_desc with
    P_const c ->
      begin match compile_constant ctx c with
	(ctx, lifted, body, Expr (Prim id)) ->
	  let result =
	    Call (Access (id, "equal"), [ Prim id; e ])
	  in
	  (ctx, lifted, [(body, result)])
      |	_ -> assert false
      end
	
  | P_var vn ->
      let name = vname_as_string_with_operators_expanded vn in
      let ty = anti_elim p in
      let result = Call (Access (Ident name, "equal"),
			 [ Prim (Ident name); e ]) in
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
		    [ ECast (Ptr (TypeId "foc_value"), e);
		      Prim (Constant (sprintf "%i" !index)) ]) in
	    let (c, l', beq') = compile_pattern eacc c p in
	    incr index;
	    (c, l@l', beqs@beq'))
	  (ctx, [], [])
	  l 
      in
      let result = Equal (Call (Ident "get_disc", [e]), Prim (Ident disc)) in
      (ctx, lifted, ([], result)::beqs)
	
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
       [ Decl (local, Ptr (TypeId "foc_string"));
	 Affect (Ident local,
		 Call (Ident "mk_foc_string",
		       [ Prim (Constant s') ])) ],
       Expr (Prim (Ident local)))
	
  | C_char _ ->
      todo "C_char"; (ctx, [], [], Skip)

  
  
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
  
let compile_species_field spc ctx f =
  match f with
    SF_sig (_, vn, ty) ->
      begin match vname_as_string_with_operators_expanded vn with
	"rep" ->
	  todo "SF_sig (rep)"; (ctx, [], [])
      |	name ->
	  let name = sprintf "%s_%s" spc name in 
	  let ty = Types.type_scheme_to_c ty in
	  let ty = 
	    subst_ctype (TypeId "Self") (TypeId (sprintf "%s_Self" spc)) ty in
	  let sign = [(name, ty)] in
	  (ctx, sign, [])
      end
  | SF_let (_, vn, args, ty, bb, _, lf) ->
      begin match lf with
	LF_logical -> (ctx, [], [])
      |	LF_no_logical ->
	  let name =
	    sprintf "%s_%s"
	      spc
	      (vname_as_string_with_operators_expanded vn) in
	  let ty = Types.type_scheme_to_c ty in
	  let ty = 
	    subst_ctype (TypeId "Self") (TypeId (sprintf "%s_Self" spc)) ty in
	  let sign = [(name, ty)] in
	  begin match ty with
	    Fun (ret, targs) ->
	      let nargs =
		match args with
		  [] -> generate_names (List.length targs -1)
	        (* We need to name the arguments.*)
		| _ -> List.map vname_as_string_with_operators_expanded args in
	      let (ctx, res, body, last) = match bb with
		BB_logical _ ->
		  todo "SF_let : BB_logical"; (ctx, [], [], Skip)
	      | BB_computational e ->
		  compile_expr nargs ctx e in
	      let body = body@[last] in
	      let body = skip_elimination body in
	      let body = result_calculus ret body in
	      let params = List.combine nargs targs in 
	      let res = res @
		[ Signature (false, name, ty);
		  Function (ret, name, params, body) ] in
	      (ctx, sign, res)
	  | _ ->
	      todo "SF_sig : Data";
	      (ctx, [], [])
	  end
      end
	
  | SF_let_rec _ ->
      todo "SF_let_rec"; (ctx, [], [])
  | SF_theorem _ ->
      (ctx, [], [])
  | SF_property _ ->
      (ctx, [], [])
	
let compile_species_param ctx = function
    SPAR_in _ ->
      todo "SPAR_in";
      (ctx, [])
  | SPAR_is _ ->
      todo "SPAR_is";
      (ctx, [])

let rec resolve_scoping spc ctx args local = function
    [] -> []
  | h::t -> 
      let (local', h') = resolve_scoping_statement spc ctx args local h in
      h' :: (resolve_scoping spc ctx args (local'@local) t)
	     
and resolve_scoping_statement spc ctx args local s =
  match s with
    Expr e ->
      ([], Expr (resolve_scoping_expr spc ctx args local e))

  | Return e ->
      ([], Return (resolve_scoping_expr spc ctx args local e))
	
  | Decl (id, ty) ->
      ([(id, ty)], s)
	
  | Affect (p, e) ->
      let p' = resolve_scoping_primary_expr spc ctx args local p in
      let e' = resolve_scoping_expr spc ctx args local e in
      begin match (p', e') with
	(Ident id, ECast (t, e'')) ->
	  let ty = try List.assoc id local with
	    Not_found ->
	      begin try CGenEnv.get_function_type ctx id with
		Not_found -> assert false
	      end
	  in
	  if type_realize ctx t = type_realize ctx ty then 
	    resolve_scoping_statement spc ctx args local (Affect (p', e''))
	  else
	    ([], Affect (p', e'))
      
      |	(Ident id, Call (Ident f, _)) ->
	  let ty = try List.assoc id local with
	    Not_found ->
	      begin try CGenEnv.get_function_type ctx id with
		Not_found -> assert false
	      end
	  in
	  begin try
	    let ty' = CGenEnv.get_function_type ctx f in
	    match ty' with
	      Fun (ret, _) ->
		if type_realize ctx ty = type_realize ctx ret then
		  ([], Affect (p', e'))
		else 
		  ([], Affect (p', ECast (ty, e')))
	    | _ -> assert false
	  with
	    Not_found ->
	      ([], Affect (p', e'))
	  end
 
      |	_ -> 
	  ([], Affect (p', e'))
      end	
  | Skip ->
      ([], s)
	
  | If (e, a, b) ->
      let a' = resolve_scoping spc ctx args local a in
      let b' = resolve_scoping spc ctx args local b in
      ([], If (resolve_scoping_expr spc ctx args local e, a', b'))
	
and resolve_scoping_expr spc ctx args local e =
  match e with
    Prim ((Ident id) as p) ->
      begin
	try
	  begin match CGenEnv.get_function_type ctx id with
	    Fun (_, []) ->
	      Call (Ident id, [])
	  | Fun (_, arg_types) ->
	      Call (Ident id,
		    List.map 
		      (fun (t, (i, t')) -> 
			let name = Prim (Ident i) in
			if t = t' then name else ECast (t, name))
		      (List.combine arg_types args))
	  | _ ->
	      Prim (resolve_scoping_primary_expr spc ctx args local p)
	  end
	with
	  Not_found -> Prim (resolve_scoping_primary_expr spc ctx args local p)
      end
  | Prim p ->
      Prim (resolve_scoping_primary_expr spc ctx args local p)
      
  | Call (p, args') ->
      let p' = resolve_scoping_primary_expr spc ctx args local p in
      let args'' = List.map (resolve_scoping_expr spc ctx args local) args' in
      Call (p', args'')
      
  | ECast (ty, e) ->
      ECast (ty, resolve_scoping_expr spc ctx args local e)
  | Equal (a, b) ->
      Equal (resolve_scoping_expr spc ctx args local a,
	     resolve_scoping_expr spc ctx args local b)

and resolve_scoping_primary_expr spc ctx args local p =
  match p with
    Ident id ->
      begin try let _ = List.assoc id local in p
      with
	Not_found ->
	  begin try
	    let _ = CGenEnv.get_function_type ctx id in
	    p
	  with
	    Not_found ->
	      let name = sprintf "%s_%s" spc id in
	      begin try
		let _ = CGenEnv.get_function_type ctx name in
		Ident name
	      with
		Not_found -> p
	      end
	  end
      end
  | Concat l ->
      Concat (List.map (resolve_scoping_primary_expr spc ctx args local) l)
  | Constant _ -> p
  | Ref p' ->
      Ref (resolve_scoping_primary_expr spc ctx args local p')
  | Cast (ty, p') ->
      Cast (ty, resolve_scoping_primary_expr spc ctx args local p')
  | Access (p', str) ->
      Access (resolve_scoping_primary_expr spc ctx args local p', str)

let compile_collection_field spc ctx = function
    SF_sig (_, vn, ty) ->
      begin match vname_as_string_with_operators_expanded vn with
	"rep" ->
	  let ty = Types.type_scheme_to_c ty in
	  let ty = subst_ctype (TypeId "Self") (TypeId spc) ty in
	  begin match ty with
	    Ptr ty' -> 
	      let body = Export (Typedef (ty', spc)) in
	      (ctx, [body])
	  | Annot (Ptr ty', str) ->
	      let body = Export (Typedef (Annot (ty', str), spc)) in
	      (ctx, [body])
	  | Param (Ptr ty', l) ->
	      let body = Export (Typedef (Param (ty', l), spc)) in
	      (ctx, [body])
	  | _ -> assert false
	  end
      |	_ -> assert false
      end
      
  | SF_let (_, vn, args, ty, bb, _, lf) ->
      begin match lf with
	LF_no_logical ->
	  let name =
	    sprintf "%s_%s"
	      spc
	      (vname_as_string_with_operators_expanded vn) in
	  let ty = Types.type_scheme_to_c ty in
	  (* let ty =  *)
(* 	    subst_ctype (TypeId "Self") (TypeId (sprintf "%s_Self" spc)) ty in *)
	  begin match ty with
	    Fun (ret, targs) ->
	      let signature = Export (Signature (false, name, ty)) in
	      let nargs =
		match args with
		  [] -> generate_names (List.length targs -1)
	        (* We need to name the arguments.*)
		| _ -> List.map vname_as_string_with_operators_expanded args in
	      let (ctx, res, body, last) = match bb with
		BB_logical _ ->
		  todo "SF_let : BB_logical"; (ctx, [], [], Skip)
	      | BB_computational e ->
		  compile_expr nargs ctx e in
	      let params = List.combine nargs targs in 
	      let body = body@[last] in
	      let body = skip_elimination body in
	      let body = result_calculus ret body in
	      let body = resolve_scoping spc ctx params [] body in 
	      let body = Function (ret, name, params, body) in
	      let ctx = CGenEnv.add_function ctx name ty in
	      (ctx, res@(signature::body::[]))
	  | _ ->
	      let ty' = Fun (ty, []) in
	      let signature = Export (Signature (true, name, ty')) in
	      let nargs = [] in
	      let (ctx, lifted, body, last) =  match bb with
		BB_logical _ ->
		  todo "SF_let : BB_logical"; (ctx, [], [], Skip)
	      | BB_computational e ->
		  compile_expr nargs ctx e
	      in
	      let body = body@[last] in
	      let body = skip_elimination body in
	      let body = result_calculus ty body in
	      let body = resolve_scoping spc ctx [] [] body in 
	      let body = Function (ty, name, [], body) in
	      let ctx = CGenEnv.add_function ctx name ty' in
	      (ctx, lifted@(signature::body::[]))
	  end   
      |	LF_logical ->
	  (ctx, [])
      end
      
  | SF_let_rec _ ->
      todo "SF_let_rec collection"; (ctx, [])
  | SF_theorem _ ->
      (ctx, [])
  | SF_property _ ->
      (ctx, [])

let compile_phrase ctx (ph, pcm) =
  match (ph.ast_desc, pcm) with
    (Ph_use file, PCM_no_matter) ->
      let env = cgen_open_module ~loc:ph.ast_loc file in
      (CGenEnv.merge env ctx, [Export (Include file)])
	
  | (Ph_open _, PCM_open _) ->
      (ctx, [])
	
  | (Ph_coq_require _, PCM_coq_require _) ->
      (ctx, [])
	
  | (Ph_species _, PCM_species _) (*spcdef, desc*) ->
      (* let name = *)
(* 	sprintf "%s_%s" *)
(* 	  (CGenEnv.unit_name ctx) *)
(* 	  (vname_as_string_with_operators_expanded spcdef.ast_desc.sd_name) in *)
(*       let (ctx, params) =  *)
(* 	List.fold_left *)
(* 	  (fun (c, l) a -> *)
(* 	    let (c, l') = compile_species_param c a in *)
(* 	    (c, l@l')) *)
(* 	  (ctx, []) *)
(* 	  desc.spe_sig_params in *)
(*       let fields = desc.spe_sig_methods in *)
(*       let (ctx, field_types, field_body) =  *)
(* 	List.fold_left *)
(* 	  (fun (c, ft, fb) a -> *)
(* 	    let (c', ft', fb') = compile_species_field name c a in *)
(* 	    (c', ft@ft', fb@fb')) *)
(* 	  (ctx, [], []) *)
(* 	  fields in *)
(*       let self_name = sprintf "%s_Self" name in *)
(*       let self = Export (Typedef (TypeId "foc_value", self_name)) in *)
(*       let body = Export (Typedef (Struct (None, field_types), name)) in *)
(*       (ctx, params@(self::body::field_body)) *)
      (ctx, [])
	
  | (Ph_theorem _, PCM_theorem _) ->
      (ctx, [])
	
  | (Ph_collection _, PCM_collection (def, desc, _)) ->
      let name =
	sprintf "%s_%s"
	  (CGenEnv.unit_name ctx)
 	  (vname_as_string_with_operators_expanded def.ast_desc.cd_name) in 
      let (ctx, body) =
	List.fold_left
	  (fun (c, b) a ->
	    let (c', b') = compile_collection_field name c a in
	    (c', b@b'))
	  (ctx, [])
	  desc.spe_sig_methods
      in
      (ctx, body)
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
	
	
	
	
	
	
