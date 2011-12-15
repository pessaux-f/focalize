
(** This module defines the abstract syntax tree types for expression and files.
It also defines the methods manipulates these types. *)

(**  {6 Type of expressions} *)

type species_name = string * string;;
                                 (** a couple of module name * species name *)

val create_species_name : string -> string -> species_name
(** [create_species_name m s] create a [species_name] from a module [m] and the
name of a species [s]. *)

val string_of_species_name : species_name -> string;;
(** Get the string identification from a species name. It has the form
[module#species]. *)

type myexpr =
    MIfte of myexpr * myexpr * myexpr (** If then else. *)
  | MApp of myexpr * Own_types.typ option * ((myexpr * Own_types.typ) list) (** Application. *)
  | MMeth of string option * string          (** Method reference. *)  
  | MFun of string * Own_types.typ option * myexpr (** Function definition. *)
  | MVar of string * Own_types.typ option   (** Identifier reference. *)
  | MMatch of (myexpr * Own_types.typ option) *
              (Own_basics.ident * string option list * myexpr) list
                                            (** the Match expression *)
  | MInt of int                             (** Integer. *)
  | MString of string                       (** String. *)
  | MVarloc of bool * (string * Own_types.typ option) *
               myexpr * myexpr     (** Let statement *)
  | MGlob_id of Own_basics.ident      (** Toplevel reference (module name, identifier *)
  | MCaml_def of string                     (** Caml import. *)

val expr_meth : string -> string -> myexpr list -> myexpr
(** [expr_meth spec meth_name expr_l] returns the expression calling the method
[meth_name] of species [spec] applied to the arguments [expr_l]. *)

val expr_meth2 : string -> string -> (myexpr * Own_types.typ) list -> myexpr

val expr_basic : Own_basics.ident -> myexpr list -> myexpr
(** [expr_basic ident expr_l] returns the expression calling the indentifier
[ident] in defined in toplevel applied to arguments [expr_l]. *)

val expr_basic2 : Own_basics.ident -> (myexpr * Own_types.typ) list -> myexpr

val expr_string : string -> myexpr
(** [expr_string s] returns the expression corresponding to the string [s]. *)

val expr_app : myexpr -> myexpr list -> myexpr
(** [expr_app expr expr_l] returns the expression corresponding to [expr]
applied to [expr_l]. *)

val expr_app2 : myexpr -> (myexpr * Own_types.typ) list -> myexpr;;

val expr_int : int -> myexpr
(** [expr_int i] returns the expression corresponding to the int [i]. *)

val expr_fun : string -> Own_types.typ -> myexpr -> myexpr
(** [expr_fun var t expr] returns the expression defining the function taking
variable [var] of type [t] and returning [expr]. *)

val expr_fun_notyp : string -> myexpr -> myexpr
(** [expr_fun var expr] returns the expression defining the function taking
variable [var] and returning [expr]. *)

val expr_if : myexpr -> myexpr -> myexpr -> myexpr
(** [expr_if cond then else] returns the expression defining the if statement
with condition [cond], then part [then] and else part [else]. *)

val expr_var : string -> myexpr
(** [expr_var x] returns the identifier reference to [x]. *)

val expr_var_typ : string -> Own_types.typ -> myexpr
(** [expr_var_typ x t] returns the identifier reference to [x] of type [t]. *)

val expr_match :
  myexpr -> 
    Own_types.typ ->
      (Own_basics.ident * string option list * myexpr) list -> myexpr
(** [expr_match expr case_l] returns the expression matching [expr] with case
[case_l]. *)

val expr_match_notyp :
  myexpr -> 
      (Own_basics.ident * string option list * myexpr) list -> myexpr
(** [expr_match expr case_l] returns the expression matching [expr] with case
[case_l]. *)

val expr_glob : Own_basics.ident -> myexpr
(** [expr_glob ident] returns the expression referencing the identifier [ident]. *)

val expr_caml : string -> myexpr
(** [expr_caml caml] returns the caml import statement of [caml]. *)

val expr_let : string -> Own_types.typ -> myexpr -> myexpr -> myexpr
(** [expr_let var expr1 expr2] returns the expression which bound variable [var]
to expression [expr1] in expression [expr2]. *)

val expr_let_notyp : string -> myexpr -> myexpr -> myexpr
(** [expr_let var expr1 expr2] returns the expression which bound variable [var]
to expression [expr1] in expression [expr2]. *)

val expr_seq : myexpr -> myexpr -> bool -> myexpr 
(** [expr_seq e1 e2 b] returns the expression which is the instruction sequence
[e1 ; e2]. the boolean [b] indicate the method used to simulate the sequence
(by a [let] of or a call to a function which returns its first argument). *)

val positif : myexpr -> myexpr
(** [positif expr] returns [expr]. *)

val negatif : myexpr -> myexpr
(** [negatif expr] returns the negation of [expr] ([#not_b(expr)]). *)

val string_of_myexpr : myexpr -> string
(** [string_of_myexpr expr] converts [expr] to a string *)

val dbg_string_myexpr : myexpr -> string
(** [dbg_string_myexpr expr] prints the ast of [expr]. *)

(**  {6 Conversion from caml expression} *)

val expr_of_caml_list : myexpr list -> myexpr
(** [expr_of_caml l] returns the focal expression which corresponds to the list
[l]. *)

val expr_tuple_of_caml_list : ('a -> myexpr) -> 'a list -> myexpr
(** [expr_of_caml l] returns the focal tuple expression which corresponds to the list
[l]. If [l] is empty it's [unit]. *)

val expr_of_caml_couple : ('a -> myexpr) -> ('b -> myexpr) -> ('a * 'b) -> myexpr
(** [expr_of_caml_couple f1 f2 (e1,e2)] returns the focal expression which corresponds
to the couple [f1 e1] [f2 e2]. *)

val expr_of_caml_bool : bool -> myexpr
(** [expr_of_caml_bool b] returns the focal expression which corresponds
to the boolean [b]. *)

(**  {6 Type of Own_type.typ association with generate/print methods} *)

type gen_print_typ;;

val create_gen_print_typ : Own_types.typ -> string -> string ->
                            gen_print_typ

val typ_get_generate : Own_types.typ -> gen_print_typ list -> string

val typ_get_print : Own_types.typ -> gen_print_typ list -> string
 

(**  {6 Type of methods} *)                                   

type a_method = {
  methname : string;       (** Name of the method. *)
  methtyp : Own_types.typ; (** Type of the value returned by the method. *)
  methdef : myexpr;        (** Body of the method. *)
  methrec : bool;          (** Is the method recursive ? *)
}

type methods =
  | Unique of a_method
  | Multiple of a_method list
;;


val meth_create : string -> Own_types.typ -> myexpr -> bool -> a_method
(** [meth_create n t expr rec] returns the method of name [n] which return a
value of type [t], with body [expr] and with the recursive flag [rec] *)

val meths_concat : a_method list -> a_method list -> a_method list
(** Concats the two list of methods and delete all duplicates entries.
It preserves the order. *)

val ( @@ ) : a_method list -> a_method list -> a_method list
(** Infix definition of [meths_concat]. *)


(**  {6 Types of parameters} *)                                   

type parameters_expect =
    PrmExpColl of string * (string * string list) (** Collection parameters, a name with a 
                                                      species name applied to a list of parameters name. *)
  | PrmExpEnt of string * Own_types.typ (** Entities parameters, a name with a
                                            type, eventually parameterized. *)
(** Type of parameters expected by a species, this type is used when taking a
species from the focal environment. [PrmExpColl] for a collection
parameters, [PrmExpEnt] for an entities parameters. *)

val create_prmexpcoll : string -> string * string list -> parameters_expect
(** [create_prmexpcoll n s_prm] create a collection parameters of type
[parameters_expect] with name [n] and type [s_prm] (species applied to a
list of parameters name. *)

val create_prmexpent : string -> Own_types.typ -> parameters_expect
(** [create_prmexpent n s_prm] create a entity parameters of type
[parameters_expect] with name [n] and type [s_prm] (type name applies to a
list of type, list(int) for example). *)

val get_name_prmexp : parameters_expect -> string
(** [get_name_prmexp p] returns the name of parameters [p]. *)

val string_of_parameters_expect : parameters_expect -> string
(** [string_of_parameteres_expect p] converts to string the value [p]. *)

type parameters_instance =
  | InstPrmColl of string option * string  (** Parameter name and name of a species. *)
  | InstPrmEnt of myexpr   (** Mini-expression. *)
(** Type of parameters given by the users. This type is used to define which
value he want to instanciate the parameters defined in the
[parameters_expect] type. [InstPrmColl] is the name of the species the user
wants for a collection parameters, [InstPrmEnt] is the expression the user
wants for an entities parameters. *)

val create_instprmcoll : string -> parameters_instance
(** [create_instprmcoll spec] returns a collection parameters with instance
the species name [spec]. *)

val create_instprment : myexpr -> parameters_instance
(** [create_instprment expr] returns the entity parameters with instance the
expression [expr]. *)

val is_coll_param : parameters_instance -> bool
(** determines if a instance parameter is a collection or a entity one. *)

val string_of_parameters_instance : parameters_instance -> string
(** converts a [parameters_instance] to string *)

val string_of_parameters_instance_verbose : parameters_instance -> string
(** converts a [parameters_instance] to string for verbose purposes. *)

type species_test = string * parameters_instance list
(** This is the type used to record in memory which species the user wants to
test (the species given in command line).
[species ::= spec_name * parameters_instance list]
[spec_name] is the name of the species.
[parameters_instance list] is the list of value that instanciate the parameters.
  *)

type parameters =
    PrmColl of string * (species_name * string list)
  | PrmEnt of string * Own_types.typ * myexpr
(** Type of effective parameters used in a species definition, this type is
created by merging an instance of the two types [parameters_expect] and
[parameters_instance]. *)

val create_prmcoll : string -> species_name * string list -> parameters
(** [create_prmcoll n s_prm] create a collection parameters of type
[parameters] with name [n] and type [s_prm] (species applied to a
list of parameters name. *)

val create_prment : string -> Own_types.typ -> myexpr -> parameters
(** [create_prment n s_prm] create a entity parameters of type
[parameters] with name [n] and type [s_prm] (type name applies to a
list of type, list(int) for example). *)

val get_name_prm : parameters -> string
(** [get_name_prm p] returns the name of parameters [p]. *)

exception Cant_instanciate_param of parameters_expect * parameters_instance
(** Raised when attempt to instanciate a collection parameters with an entity
parameters or conversely. *)

val merge_prm : parameters_expect -> parameters_instance -> parameters
(** Takes a [parameters_expect] value and a [parameters_instance] and returns the
corresponding value in [parameters] type *)                                                                  


(**  {6 Type of species} *)

type species = {
  specname : string;    (** Species name.*)
  specparam : parameters list;  (** Parameters expected by the species. *)
  specinh : (species_name * string list) list; (** List of species inherited. *)
  specdef : methods list;                (** List of method defined within. *)
  specrep : Own_types.typ option;        (** Definition of the rep's type *)
}
(** Definition of a species, a species is has a name [specname], a list of
parameters exptected [specparam], a list of species [specinh] inherited
from, a list list of methods definition [specdef] and a rep's type. *)

exception Param_non_canon of string * string
(** The exception raised when nowhere TODO: delete it *)

val spec_create :
  string ->
  parameters list ->
  (species_name * string list) list ->
  Own_types.typ option -> methods list -> species
(** [spec_create n p inh rep meth] returns a species of name [n] with expected parameters
[p]. The species inherit from [inh] has a rep of type [rep] (eventually
[None]) and a methods [meth] defined. *)


(** {6 Type of collection} *)

type collection = {
  collname : string; (** Collection name. *)
  collimpl : species_name * species_name list;
                        (** Species, applied by it parameters, it implements. *)
}
(** A collection has a name [collname] and implements the species [collimpl] *) 

val coll_create : string -> species_name * species_name list -> collection
(** [coll_create n spec] returns a collection applied with name [n] which
implements the species [spec] ([spec] can by applied with some other
collection name). *)

(** {6 Type of toplevel definition} *)

type toplevel_def =
    ObjCollection of collection (** Collection. *)
  | ObjSpecies of species        (** Species. *)
  | ObjToplet     of string * Own_types.typ option * myexpr (** Variable binding. *)
  | ObjTopcall    of myexpr  (* Evaluation of an expression. *)
  | ObjType       of string * (string * Own_types.typ list) list (** Non-parameterized type *);;
(** This type defined all object that can by defined in the toplevel of a focal
source. *)

val create_toplevel_coll : collection -> toplevel_def
(** [create_toplevel_coll c] returns the toplevel definition of a collection. *)

val create_toplevel_spec : species -> toplevel_def
(** [create_toplevel_spec s] returns the toplevel definition of a species. *)

val create_toplevel_let : string -> Own_types.typ option -> myexpr -> toplevel_def
(** [create_toplevel_let var t expr] returns the variable binding of [var], of
type [t], with expression [expr]. *)

val create_toplevel_call : myexpr -> toplevel_def
(** [create_toplevel_call expr] returns the toplevel definition of a expression
evaluation. *)

val create_toplevel_type : string -> (string * Own_types.typ list) list -> toplevel_def
(** [create_toplevel_type n c] returns the toplevel type definition named [n]
with constructors [c].  *)                                                                        

(** {6 Type of differents file} *)

type fichier_foc = {
  ficopenuse : string list; (** List of module the file open and uses. *)
  ficobjet : toplevel_def list; (** List of all toplevel definition. *)
}

val fic_create : string list -> toplevel_def list -> fichier_foc
(** [fic_create op_us top_def] returns a focal file which used the [op_us]
modules and with toplevel definition [top_def]. *)

type import = string * (string * string) list
(** [import] define the type of a species import.
[import ::= spec_name * (fun_name * caml_code) list].
[spec_name] is the name of the species for which the import is define
([toplevel] for the toplevel).
[fun_name] is the name of the function to give in a caml import statement.
[caml_code] is the caml code imported. *)

val create_import : string -> (string * string) list -> import
(** [create_import spec def_l] returns a import value for species [spec] with
binding def_l. See [import] for details. *)

val add_caml_import : import -> string -> string -> import
(** [add_caml_import imp f_n code] returns [imp] with a new binding. [f_n] with
code [code] is add to [imp]. *)

type fichier_fml = import list
(** A fml file is defined by a list of [import] value *)

val add_import : import -> fichier_fml -> fichier_fml
(** [add_import imp fml] returns [fml] with [imp] as new list of functions imported
for a species *)

