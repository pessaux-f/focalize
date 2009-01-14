(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*            François Pessaux                                         *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: env_docgen.ml,v 1.1 2009-01-14 10:44:19 pessaux Exp $ *)



(* *********************************************************************** *)
(** {b Descr} : This module contains the documentation environment
    mechanisms.
    This environment maps **methods** [vname]s onto some optional MathML
    and LaTeX code.
    This environment is not handled with the generic environment structure
    of "src/typing/env.ml" because it does not have a nested "modules"
    structure. Moreover, it only addresses method identifiers, not type
    names, species names, etc.
    Hence its structure is simpler than the generic environments' one.
    Reminding the mapping of methods names onto MathML/LaTeX symbols
    allows to annotate on the fly methods names by the related code during
    XML generation thanks to a "<foc:symbol></foc:symbol>" markup.         *)
(* *********************************************************************** *)



(* ******************************************************************* *)
(** {b Descr}: Information "contained" in the documentation generation
    environment ***as shown to the user*** to find out on what symbol
    a method identifier must be mapped in MathML and in LaTeX.

    {b Rem}: Exported outside this module.                             *)
(* ******************************************************************* *)
type symbol_description = {
  sd_mathml : string option ;  (** Optional MathML translation. *)
  sd_latex : string option     (** Optional LaTeX translation. *)
} ;;



(* ************************************************************************ *)
(** {b Descr}: Information really contained in the documentation generation
    environment. Our environments are a bit biased. In fact adding a
    binding of an ident to some translation [None] must not discard the
    possible Some already existing translation [Some] in the environment.
    In effect, if in a species a documentation tells to generate "equal" as
    a MathML "<eq/>", we can have later another species redefining or
    implementing the method "equal". And in this later species, the user
    won't repeat the MathML "<eq/>" mapping. So when seeing the method, the
    analysis of the documentation will return [None] as a MathML stuff but
    we do not want to overwrite and lose the previous [Some "<eq/>"].
    For this reason, before inserting a binding in the environment we
    search for an older one and if the inserted symbol translation is
    "None" we do not change the existing binding. If it is "Some" and the
    previous binding was [None], then we just change in place the value
    of the binding.

    {b Rem}: Not exported outside this module.                              *)
(* ************************************************************************ *)
type internal_symbol_description = {
  mutable isd_mathml : string option ;  (** Optional MathML translation. *)
  mutable isd_latex : string option     (** Optional LaTeX translation. *)
} ;;



(* ***************************************************************** *)
(** {b Descr} : Documentation generation environment mapping mathods
    [vname]s onto their related symbols in MathML and in LaTeX.

    {b Rem}: Exported abstract outside this module.                  *)
(* ***************************************************************** *)
type t = (Parsetree.vname * internal_symbol_description) list ;;



let empty = [] ;;


let find_method method_vname env =
  let { isd_mathml = mathml ; isd_latex = latex } =
    List.assoc method_vname env in
  { sd_mathml = mathml ; sd_latex = latex }
;;



let add_method method_vname mathml latex env =
  match (mathml, latex) with
   | (None, None) -> env   (* If nothing to add, make it quicker... *)
   | (_, _) ->
       (begin
       try
         let { isd_mathml = old_mathml ; isd_latex = old_latex } as old_bind =
           List.assoc method_vname env in
         (* We found a binding. We are then sure that at least one of its
            fields is not [None]. We will then modify the binding in place and
            return the environment modified by side effect. *)
         (match (mathml, old_mathml) with
          | (None, _) -> ()
          | ((Some _), _) -> old_bind.isd_mathml <- mathml) ;
         (match (latex, old_latex) with
          | (None, _) -> ()
          | ((Some _), _) -> old_bind.isd_latex <- latex) ;
         env
       with Not_found ->
         (* No previous binding found. We then extend the environment in a
            regular way. *)
         (method_vname, { isd_mathml = mathml ; isd_latex = latex }) :: env
       end)
;;
