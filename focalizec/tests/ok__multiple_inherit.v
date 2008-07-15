Require Export Bool.
Require Export ZArith.
Open Scope Z_scope.
Require Export Reals.
Require Export Ascii.
Require Export String.
Require Export List.
Require Export Recdef.
Require Export coq_builtins.

Module Foo.
  Record Foo : Type :=
    mk_record {
    rf_T :> Set ;
    (* From species ok__multiple_inherit#Foo. *)
    rf_m : basics.int__t
    }.
  
  Definition m : basics.int__t := 1.
  
End Foo.

Module Bar.
  Record Bar : Type :=
    mk_record {
    rf_T :> Set ;
    (* From species ok__multiple_inherit#Bar. *)
    rf_m : basics.int__t
    }.
  
  Definition m : basics.int__t := 2.
  
End Bar.

Module Gee.
  Record Gee : Type :=
    mk_record {
    rf_T :> Set ;
    (* From species ok__multiple_inherit#Bar. *)
    rf_m : basics.int__t
    }.
  
  
End Gee.

Module Buz.
  Record Buz : Type :=
    mk_record {
    rf_T :> Set ;
    (* From species ok__multiple_inherit#Foo. *)
    rf_m : basics.int__t
    }.
  
  
End Buz.

Module Oups1.
  Record Oups1 : Type :=
    mk_record {
    rf_T :> Set ;
    (* From species ok__multiple_inherit#Foo. *)
    rf_m : basics.int__t
    }.
  
  
End Oups1.

Module Oups2.
  Record Oups2 : Type :=
    mk_record {
    rf_T :> Set ;
    (* From species ok__multiple_inherit#Foo. *)
    rf_m : basics.int__t
    }.
  
  
End Oups2.

Module Oups3.
  Record Oups3 : Type :=
    mk_record {
    rf_T :> Set ;
    (* From species ok__multiple_inherit#Bar. *)
    rf_m : basics.int__t
    }.
  
  
End Oups3.

Module Oups4.
  Record Oups4 : Type :=
    mk_record {
    rf_T :> Set ;
    (* From species ok__multiple_inherit#Foo. *)
    rf_m : basics.int__t
    }.
  
  
End Oups4.

