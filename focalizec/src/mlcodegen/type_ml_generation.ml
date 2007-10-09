(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: type_ml_generation.ml,v 1.1 2007-10-09 08:37:35 pessaux Exp $ *)


(* ************************************************************************ *)
(* Misc_ml_generation.reduced_compil_context -> Types.type_simple list ->   *)
(*   unit                                                                   *)
(** {b Descr} : Just an helper to print a list of types separated by commas
       and sharing a same variables mapping and an empty collection carrier
       mapping. If the list has only 1 element then it is NOT enclosed
       between parens.
       If it a several elements, then it IS enclosed between parens.
       If is has no element (degenerated case) then nothing gets printed.
       This is espercially used to print the parameters of a type
       definition in [type_def_compile].

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let print_types_comma_with_same_vmapping_and_empty_carrier_mapping ctx tys =
  let current_unit = ctx.Misc_ml_generation.rcc_current_unit in
  let out_fmter = ctx.Misc_ml_generation.rcc_out_fmter in
  match tys with
   | [] -> ()
   | [one] ->
       Format.fprintf out_fmter " %a"
	 (Types.pp_type_simple_to_ml ~current_unit ~reuse_mapping: true []) one
   | several ->
       (begin
       (* Enclose by parentheses and separate by commas. *)
       let rec rec_print_params = function
	 | [] -> ()
	 | [last] ->
	     Format.fprintf out_fmter "%a"
	       (Types.pp_type_simple_to_ml
		  ~current_unit ~reuse_mapping: true [])
	       last
	 | first :: rem ->
	     Format.fprintf out_fmter "%a,@ "
	       (Types.pp_type_simple_to_ml
		  ~current_unit ~reuse_mapping: true [])
	       first ;
	     rec_print_params rem in
       Format.fprintf out_fmter " (@[<1>" ;
       rec_print_params several ;
       Format.fprintf out_fmter "@])"
       end)
;;



(* **************************************************************** *)
(* Misc_ml_generation.reduced_compil_context -> Parsetree.vname ->  *)
(*   Env.TypeInformation.type_description -> unit                   *)
(** {b Descr} : Generates the OCaml code for a "regular" (i.e. non 
          "external" FoCaL type definition.

    {b Rem} : Exported outside this module.                         *)
(* **************************************************************** *)
let type_def_compile ctx type_def_name type_descr =
  let out_fmter = ctx.Misc_ml_generation.rcc_out_fmter in
  (* Type definition header. *)
  Format.fprintf out_fmter "@[<2>type" ;
  (* Get a fresh instance of the type's identity scheme. *)
  let (instanciated_body, params) =
    Types.specialize2
      type_descr.Env.TypeInformation.type_identity
      type_descr.Env.TypeInformation.type_params in
  (* Useless, but just in case.... This does not hurt ! *)
  Types.purge_type_simple_to_ml_variable_mapping () ;
  (* Now, generates the type definition's body. *)
  match type_descr.Env.TypeInformation.type_kind with
   | Env.TypeInformation.TK_abstract ->
       (* Print the parameter(s) stuff if any. *)
       print_types_comma_with_same_vmapping_and_empty_carrier_mapping
	 ctx params ;
       (* Now print the type constructor's name. *)
       Format.fprintf out_fmter " _focty_%a =@ "
	 Misc_ml_generation.pp_to_ocaml_vname type_def_name ;
       (* Type abbreviation: the body is the abbreviated type. *)
       Format.fprintf out_fmter "%a@] ;;@\n "
	 (Types.pp_type_simple_to_ml
	    ~current_unit: ctx.Misc_ml_generation.rcc_current_unit
	    ~reuse_mapping: true [])
	 instanciated_body
   | Env.TypeInformation.TK_variant cstrs ->
       (begin
       (* To ensure variables names sharing, we will unify an instance of   *)
       (* each constructor result type (remind they have a functional type  *)
       (* whose arguments are the sum constructor's arguments and result is *)
       (* the same type that the hosting type itself) with the instance of  *)
       (* the defined type identity.                                        *)
       let sum_constructors_to_print =
	 List.map
	   (fun (sum_cstr_name, sum_cstr_arity, sum_cstr_scheme) ->
	     if sum_cstr_arity = Env.TypeInformation.CA_one then
	       (begin
	       try
		 let sum_cstr_ty = Types.specialize sum_cstr_scheme in
		 let sum_cstr_args = Types.type_variable () in
		 Types.unify
		   ~loc: Location.none ~self_manifest: None
		   (Types.type_arrow sum_cstr_args instanciated_body)
		   sum_cstr_ty ;
		 (sum_cstr_name, (Some sum_cstr_args))
	       with _ ->
		 (* Because program is already well-typed, this *)
		 (* should always succeed.                      *)
		 assert false
	       end)
	     else (sum_cstr_name, None))
	   cstrs in
       (* Print the parameter(s) stuff if any. Do it only now the  *)
       (* unifications have been done with the sum constructors to *)
       (* be sure that thanks to unifications, "sames" variables   *)
       (* will have the "same" name everywhere (i.e. in the        *)
       (* the parameters enumeration of the type and in the sum    *)
       (* constructors definitions).                               *)
       print_types_comma_with_same_vmapping_and_empty_carrier_mapping
	 ctx params ;
       (* Now print the type constructor's name. *)
       Format.fprintf out_fmter " _focty_%a =@ "
	 Misc_ml_generation.pp_to_ocaml_vname type_def_name ;
       (* And finally really print the constructors definitions. *)
       List.iter
	 (fun (sum_cstr_name, opt_args) ->
	   (* The sum constructor name. *)
	   Format.fprintf out_fmter "@\n| %a"
	     Misc_ml_generation.pp_to_ocaml_vname sum_cstr_name ;
	   match opt_args with
	    | None -> ()
	    | Some sum_cstr_args ->
		(* The argument(s) of the constructor. *)
		Format.fprintf out_fmter " of@ (@[<1>%a@])"
		  (Types.pp_type_simple_to_ml
		     ~current_unit: ctx.Misc_ml_generation.rcc_current_unit
		     ~reuse_mapping: true [])
		  sum_cstr_args)
	 sum_constructors_to_print ;
       Format.fprintf out_fmter "@]@\n ;;@\n "
       end)
   | Env.TypeInformation.TK_record fields ->
       (begin
       (* Like for the sum types, we make use of unification to ensure the *)
       (* sharing of variables names. We proceed exactly the same way,     *)
       (* delaying the whole print until we unified into each record-field *)
       (* type.                                                            *)
       let record_fields_to_print =
	 List.map
	   (fun (field_name, field_mut, field_scheme) ->
	     try
	       let field_ty = Types.specialize field_scheme in
	       let field_args = Types.type_variable () in
	       Types.unify
		 ~loc: Location.none ~self_manifest: None
		 (Types.type_arrow field_args instanciated_body)
		 field_ty ;
	       (field_name, field_mut, field_args)
	     with _ ->
	       (* Because program is already well-typed, this *)
	       (* should always succeed.                      *)
	       assert false)
	   fields in
       (* Print the parameter(s) stuff if any. *)
       print_types_comma_with_same_vmapping_and_empty_carrier_mapping
	 ctx params ;
       (* Now print the type constructor's name. *)
       Format.fprintf out_fmter " _focty_%a = {@ "
	 Misc_ml_generation.pp_to_ocaml_vname type_def_name ;
       (* And finally really print the fields definitions. *)
       List.iter
	 (fun (field_name, field_mut, field_ty) ->
	   Format.fprintf out_fmter "@\n " ;
	   (* Generate the mutability flag. *)
	   if field_mut = Env.TypeInformation.FM_mutable then
	     Format.fprintf out_fmter "mutable " ;
	   Format.fprintf out_fmter "%s :@ %a ;"
	     field_name
	     (Types.pp_type_simple_to_ml
		~current_unit: ctx.Misc_ml_generation.rcc_current_unit
		~reuse_mapping: true [])
	     field_ty)
	 record_fields_to_print ;
       Format.fprintf out_fmter " }@] ;;@\n "
       end)
;;
