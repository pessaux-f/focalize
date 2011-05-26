#**********************************************************************#
#                                                                      #
#                        FoCaL compiler                                #
#                                                                      #
#            François Pessaux                                          #
#            Pierre Weis                                               #
#            Damien Doligez                                            #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2008 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#
# $Id: test.spec,v 1.1 2011-05-26 16:08:09 maarek Exp $


High level specification of the Focalize testbed technology
===========================================================

2011/04/19:
===========

Integrate tests within focalize compiler

Test specification language should be written within program sources.

Use dedicated annotations and syntax ? Orelse integrate within the compiler
with a specific syntax for test as for the proof language.

Let us assume Lenght_spec is a species:

species Length_spec =
(*   representation; *)

  representation = int;

  signature parse : string -> Self;
  signature print : Self -> string;
  signature equal : Self -> Self -> bool;

  signature add : Self -> Self -> Self;
  signature zero : Self;
  (* add is an addition *)

  theorem zero_neutral :
      all l : Self, !equal(!add(l, !zero), l) 
    proof = assumed;

end;;

A sample test annotation for species Length_spec could ressemble

(** {@Focalizetest}
    collections :
      collection Length_test = implement Length_spec; end ;;
    theorems :
    	    zero_neutral
    report : "length.rprt"
    test parameters :
     random_seed = 0101111110;
     int_inter = [-10; 50];
     max_depth = 5;
     nb_bus_error = 41;
     ...

*)


If we choose an integrated syntax we could have

species Length_spec =
(*   representation; *)

  representation = int;

  signature parse : string -> Self;
  signature print : Self -> string;
  signature equal : Self -> Self -> bool;

  signature add : Self -> Self -> Self;
  signature zero : Self;
  (* add is an addition *)

  theorem zero_neutral :
      all l : Self, !equal(!add(l, !zero), l) 
    proof = assumed;

  test: 
    collection Length_test = 
      implement Length_spec;
      theorem zero_neutral, equal_symmetric;
      test parameters = {
         random_seed = 0101111110;
         int_inter = [-10; 50];
     	 max_depth = 5;
     	 nb_bus_error = 41;
     	 report = "length.rprt";
         }

end;;

or using meta annotations 

testing Length_test = 

  collection Length_test = implement Length_spec;

  property zero_neutral, equal_symmetric;

  property one_is_not_neutral: all x in Self,
      !different(x, x+1);

  theorem zero_is_not_one: !different(!zero, !one )
   proof = assumed;

  let random_seed = 0101111110
  and int_inter = [-10; 50]
  and max_depth = 5
  and nb_bus_error = 41
  ;

end;

In this latter case the tester part of the compiler should always check for
consistency and possibly generate nothing (--no-test option).

A more complicated example could be

testing Triangle_test =
  collection Length =
    implement Length_spec;
  collection Triangle_test = 
    implement Triangle(Length);

  property min_is_min, med_is_med, max_is_max, min_med_max_give_an_edge,
      create_triangle_correct, length_tsf_correct, parse_print_correct,
      organize_sort, organize_permute, triangle_type_complete;
  property triangle_type_correct_equi, triangle_type_correct_iso, 
      triangle_type_correct_scal, triangle_type_correct_err;

  parameters :
    let random_seed = 0101111110
    and int_inter = [-10; 50]
    and max_depth = 5
    and nb_bus_error = 41
  ;

end
;;



2011/04/26:
===========

Three alternative extensions of the Parsetree type with a testing_def
type to integrate the testing instructions at the toplevel of the
language. Here, we name "context" the let bindings and collection
definitions required by the testing and unused elsewhere in the code.

type testing_expr = testing_expr_desc ast
and testing_expr_desc = {
    ...
  }
;;

type testing_def = testing_def_desc ast
and testing_def_desc = {
  tstd_name : vname;
  tstd_body : testing_expr;
}
;;


1. Having a context AST field containing the context phrases. This
   solution breaks the separation between language levels and forces
   most operations on phrase level of the AST to call operations on
   top level and vice versa.

type testing_expr =
  {
    tst_context : phrase list;
    tst_properties : ...;
    tst_parameters : ...;
  }

...
  | Ph_testing of testing_def

2. Having two lists (a list of let bindings and a list of collection
   definitions) representing the context. This solution does not take
   into account the fact that a testing context is only composed of
   toplevel instructions discarded in the main compilation path. The
   solution therefore requires to duplicate iterations on phrases for
   dealing specifically with let bindings and collection definitions.

type testing_expr =
  {
    tst_bindings : let_def list;
    tst_collections: collection_def list;
    tst_properties : ...;
    tst_parameters : ...;
  }

...
  | Ph_testing of testing_def

3. (this is the solution chosen for now) Having two separate arguments
   for testing phrases. The first one being the testing instructions,
   the second one being a list of phrases representing the
   context. The first argument resides at the phrase level the second
   at the toplevel. The type definitions of phrase and phrase_desc
   need to be recursive as well as the operations on them but there is
   no need for inter-level recursion.

type testing_expr =
  {
    tst_properties : ...;
    tst_parameters : ...;
  }

...
  | Ph_testing of testing_def * phrase list


2011/05/25:
===========

The previous solution is not suitable because it creates a gap between
the internal representation AST and the reality of a testing
instruction. The new types representing a testing instruction is as
follows:

type testing_context_phrase = testing_context_phrase_desc ast
and testing_context_phrase_desc =
  | TstCtxPh_collection of collection_def
  | TstCtxPh_let of let_def
  | TstCtxPh_property of property_def
and testing_context = testing_context_phrase list
;;

type testing_expr = testing_expr_desc ast
and testing_expr_desc = {
    tst_context : testing_context;
    tst_properties : expr_ident list;
    tst_parameters : let_def list;
  }
;;

2011/05/26:
===========

Where to insert the generation of testing collections?

The current compilation process is as follows:
 1. compile_fcl
    a. parse
    b. dump_ast if requested 
    c. sourcify if requested
    d. scoping
    e. type checking (outputs interface on the fly if requested)
    f. pattern matching
    g. generating doc
    h. generating ml
    i. generating coq
    j. generating .fo
 2. compile_ml
 3. compile_coq

The testing collections (Ph_collection items) will be produced if
requested from the testing instructions (Ph_testing). These generated
items will lead to extra code in the ml produce (only). We could have
a scoping and typing of the testing instructions (mostly done). We
could then generate the testing collections and scope/type them extra
elements in the file (we would need to recover the scoping/typing
context not to perform it on the whole file again). We could perform
the coq generation on the original ast and the ml generation on the
new one.

