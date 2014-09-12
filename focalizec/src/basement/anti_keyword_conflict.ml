(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*                                                                     *)
(*                 LIP6  --  INRIA Rocquencourt  -- ENSTA              *)
(*                                                                     *)
(*  Copyright 2007 - ... LIP6 and INRIA                                *)
(*            2012 - ... ENSTA ParisTech                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

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
    ("exists", "_exists_") ;
    ("exists2", "_exists2_") ;
    ("fix", "_fix_") ;
    ("forall", "_forall_") ;
    (* ("IF", "_IF_") ; Non-lowercase. *)
    ("mod", "_mod_") ;
    (* ("Prop", "_PROP_") ; Non-lowercase. *)
    ("return", "_return_") ;
    (* ("Set", "_Set_") ; Non-lowercase. *)
    (* ("Type", "_Type_") ; Non-lowercase. *)
    ("using", "_using_")
  ]
;;


let string_to_no_keyword_string s =
  try StrMap.find s !lowerc_keywords_map with Not_found -> s ;;



let string_to_no_keyword_string_if_diff s =
  try Some (StrMap.find s !lowerc_keywords_map) with Not_found -> None ;;
