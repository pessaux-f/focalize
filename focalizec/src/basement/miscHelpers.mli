(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                 LIP6  --  INRIA Rocquencourt  -- ENSTA              *)
(*                                                                     *)
(*  Copyright 2007 - 2012 LIP6 and INRIA                               *)
(*            2012 ENSTA ParisTech                                     *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)


val bind_parameters_to_types_from_type_scheme :
  self_manifest: Types.type_simple option ->
  Types.type_scheme option -> Parsetree.vname list ->
    (((Parsetree.vname * Types.type_simple option) list) *
       (Types.type_simple option) *
       (Types.type_variable list))

val map2_opt2 : ('a -> 'b option -> 'c) -> 'a list -> 'b list -> 'c list

val fold_left3 :
  ('a -> 'b -> 'c -> 'd -> 'a) -> 'a -> 'b list -> 'c list -> 'd list -> 'a

val iter3 : ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> unit

val map3 :
  ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list

val list_fill : 'a -> int -> 'a list

val iter4 :
  ('a -> 'b -> 'c -> 'd -> 'e) -> 'a list -> 'b list -> 'c list -> 'd list ->
    unit
