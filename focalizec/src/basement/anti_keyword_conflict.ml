(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            Fran�ois Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007, 2008 LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: anti_keyword_conflict.ml,v 1.1 2012-02-10 16:24:40 pessaux Exp $ *)


module StrMod = struct
  type t = string
  let compare = compare
end ;;
module StrMap = Map.Make(StrMod) ;;

let lowerc_keywords_map = ref (StrMap.empty : string StrMap.t) ;;



(* Initialize the map providing a translation for identifiers that would be
   named like a keyword of the backends targets languages. *)
List.iter
(fun (k, v) -> lowerc_keywords_map := StrMap.add k v !lowerc_keywords_map)
  [ (* Ocaml keywords not being also FoCaLize keywords. *)
    ("class", "_class_") ;
    ("constraint", "_constraint_") ;
    ("do", "_do_") ;
    ("done", "_done_") ;
    ("downto", "_downto_") ;
    ("exception", "_exception_") ;
    ("for", "_for_") ;
    ("fun", "_fun_") ;
    ("functor", "_functor_") ;
    ("include", "_include_") ;
    ("initializer", "_initializer_") ;
    ("lazy", "_lazy_") ;
    ("method", "_method_") ;
    ("module", "_module_") ;
    ("mutable", "_mutable_") ;
    ("new", "_new_") ;
    ("object", "_object_") ;
    ("sig", "_sig_") ;
    ("struct", "_struct_") ;
    ("to", "_to_") ;
    ("try", "_try_") ;
    ("val", "_val_") ;
    ("virtual", "_virtual_") ;
    ("when", "_when_") ;
    ("while", "_while_") ;
    (* Coq keywords not being also FoCaLize keywords. *)
    ("at", "_at_") ;
    ("cofix", "_cofix_") ;
    ("exists2", "_exists2_") ;
    ("fix", "_fix_") ;
    ("forall", "_forall_") ;
    (* ("IF", "_IF_") ; Non-lowercase. *)
    ("mod", "_mod_") ;
    (* ("Prop", "_PROP_") ; Non-lowercase. *)
    ("return", "_return_") ;
    (* ("Set", "_Set_") ; Non-lowercase. *)
    (* ("Type", "_Type_") ; Non-lowercase. *)
    ("using", "_using_") ;
    ("", "__") ;
    ("", "__") ;
  ]
;;
