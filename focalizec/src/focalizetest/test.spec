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
# $Id: test.spec,v 1.1 2011-04-19 14:20:42 rr Exp $


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

testing collection Length_test = 

  implement Length_spec;

  theorem zero_neutral, equal_symmetric;

  let random_seed = 0101111110
  and int_inter = [-10; 50]
  and max_depth = 5
  and nb_bus_error = 41
  and report = "length.rprt"


end;
