% prolog source file

%:- module(termes,
%          [set_types/3, % for a list of variables
           %set_type/3, % for a variable
           %unifyD/2, % An equality over terms
           %unifyD/3, % reified version
           %unifyD_pattern/2, % unify a pattern and a variable
           %disunifyD/2,
           %antiunifyD/3, % make the domain union of a list of terms.
           %remove_constructor/2,
           %label_algebraics/1
           %]
          % ,hidden(true) % debugging is disable inside this module
%).

%:- use_module(library(atts)).
% On définit avant les attributs qu'on utilise ici :
% :- attribute dom/1, type/1. <- mis dans autres.pl

:- use_module(library(ordsets),
     [ord_intersection/3,
      ord_intersect/2,
      ord_del_element/3,
      list_to_ord_set/2,
      ord_member/2,
      ord_union/3
      ]).

% Dom est le domaine de la variable. Il s'agit d'un ensemble de constante
% (atome prolog)
% type est une ground terme qui est le type de donnée de la variable.
% On utilise celui-ci pour instancier une variable à un constructeur. Le cas se
% produit par exemple quand on retire du domaine une valeur et que le domaine
% se réduit en un singleton.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Les fonctions de manipulation du type qui sont nécessaire pour les variables
% attribuées.
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set_integers(L).
%
% Set the domain of the variable in L to FD. Add an attribute type set to int.

set_integers([V|Vs]) :-
  !,
  %V in -1000000..1000000,
  % V in 0..4,
  % V in -33554432..33554431,
  V in -32767..32768,
  put_atts(V, type(int)),
  (call_rec([get_atts(V, dom(_))]) -> throw('Can t set integers type'); true),
  set_integers(Vs).

set_integers([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set_bools(L).
%
% Set the domain of the variable in L to FD. Domain is [0, 1]. attribute type is
% int.

set_bools([B|Bs]) :-
  !, B in 0..1,
  put_atts(B, type(int)),
  (call_rec([get_atts(B, dom(_))]) -> throw('Can t set bool type'); true),
  set_bools(Bs).
set_bools([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_function(L).    DEPRECATED
%
% Get the function definition of the variable V.
%
%get_function_name_args(V, Name, Expected, Args, FV) :-
%    get_atts(V, type(fun(Name, Expected, Args, FV))), !
%  ;
%    throw(get_function_name_args('Not a functional variable'))
%  .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set_function(L).       DEPRECATED
%
% Set the function definition for the variable V.
%
%
%set_function(V, Name, Expected, L, FV) :-
%  !,
%  put_atts(V, type(fun(Name, Expected, L, FV))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% is_int(V).
%
%  pass if V is an integer (have an attribute type(type) or is a integer
%  constant).
%

is_int(V) :-
  (var(V) ->
    get_atts(V,type(int))
  ; integer(V)
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set_dom_and_type(LV, L, T).
%
% L doit être une liste ordonnée.
% T doit être un terme ground.
%
% Positionne l'attribut Dom (resp. Type) des variables de LV à L (resp. T).

set_dom_and_type([X|R], L, T) :-
  !, 
  ( get_atts(X, dom(DD))  ->
    ( ord_intersection(L,DD,DDD),
      put_atts(X, dom(DDD))
    )
  ; put_atts(X, dom(L))
  ),
  put_atts(X, type(T)),
  set_dom_and_type(R, L, T).
set_dom_and_type([], _, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filtre_variable(+L1, -L2).
%
% Returns in L2 all element of L1 which are variable.

filtre_variable([X|R], L2) :-
  !,
  filtre_variable(R,L3),
  (var(X) -> L2 = [X|L3]; L2 = L3).
filtre_variable([], []) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set_type(X, Type, TEnv).
%
% Sets the domain and the type of the variable X.
%
% If Type is int, call set_integers. 
% If Type is bool, call set_bools.
% Uf Type is fun(...), call set_function
%
% Exemple:
% set_type(X, arbre(list(int))).
% set_type([X], list(int)).
% set_type([], _T, _Env).
% set_type([E,R], T, Env).

set_type(V, T, TEnv) :-
  ( T == int ->
    set_integers([V])
  ;
    (T == bool ->
      set_bools([V])
    ;
      ((T = fun(Arity), integer(Arity)) ->
        true
      ; get_ord_constructors(Cs, T, TEnv),
        ( Cs = [O] ->
          set_constructor(V, T, O)
        ; set_dom_and_type([V], Cs, T)
        )
      )
    )
  ).

set_type(V, T) :-
  type_envi(TEnv),
  var(V) -> set_type(V, T, TEnv); true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set_types(Vs, Type, TEnv).
%
% Same but for a set of variable.
%
% Exemple :
% set_types([X,Y], int).
% set_types([L1,L2], list(int)).

set_types(LE, T, TEnv) :-
  filtre_variable(LE, LV),
  ( T == int ->
    set_integers(LV)
  ;
    ( T == bool ->
      set_bools(LV)
    ; get_ord_constructors(L, T, TEnv),
      set_dom_and_type(LV, L, T)
    )
  ).

set_types(LE, T) :-
  type_envi(TEnv),
  set_types(LE, T, TEnv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set_types_vardef(Vs, Type, TEnv).
%
% Same above but for a list of vardef (var(X,T)).
%
% Example :
% set_types([var(X,int),var(L,list(int))], TEnv).

set_types_vardef([var(X,T)|R], TEnv) :-
  !,
  set_types([X],T,TEnv),
  set_types_vardef(R,TEnv).

set_types_vardef([], _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set_type_deep(Val, T, TEnv).
%
% Take a type T and a non-ground term Val. Set the variable in the term Val to
% their types respecting T. fail if Val is not a possible value for type T.
%
% Example:
% push_type(list(int), cons(X, nil))
%         set the type of X to int.

set_types_vars_deep([], [], _TEnv) :- !.
set_types_vars_deep([Val|Vals], [T|Ts], TEnv) :- 
  set_type_deep(Val, T, TEnv),
  set_types_vars_deep(Vals, Ts, TEnv).

set_type_deep(Val, T, TEnv) :-
  ( (var(Val); T == int) ->
    set_type(Val, T, TEnv)
  ;
    Val =.. [C|Args],
    args_type_from_cons_name(Ts, T, C, TEnv),
    set_types_vars_deep(Args, Ts, TEnv)
  ).

set_type_deep(Val, T) :-
  type_envi(TEnv),
  set_type_deep(Val, T, TEnv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set_types_vars(-Vs, +L, +TEnv).
% set_types_vars(+Vs, +L, +TEnv).
%
% Takes a list L = [T_1; T_2; ...] of types. Returns a list of fresh variables
% [X_1; X_2; ...] each variable X_1 has type T_1 (so a dom and a type
% attribute).

set_types_vars([V|Vs], [T|Ts], TEnv) :-
  !,
  set_type(V, T, TEnv),
  set_types_vars(Vs, Ts, TEnv).

set_types_vars([], [], _TEnv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% args_from_cons_name(-Vs, +Type_Name, +Cons_name, +TEnv).
%
% Where Cons_name of the type Type_name expected arguments of types [T_1; T_2;
% ...]. 
%
% returns a list of fresh attributed variables [X_1; X_2; ...] where each X_i
% has type T_i.

args_from_cons_name(Vs, T, C, TEnv) :-
  args_type_from_cons_name(Ts, T, C, TEnv),
  set_types_vars(Vs, Ts, TEnv).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set_constructor(Var, Type_Name, Cons_name).
%
% Set the value of Var to the constructor Cons_name.
% It does not check if the Cons_name is is the domain.
%
% If the constructor admits arguments, Var is instanciated to a Cons_name
% applied to variables. The variables are fresh and are set to the type
% corresponding to what the constructor expect.

set_constructor(X, T, C) :-
  type_envi(TEnv),
  args_from_cons_name(Vs, T, C, TEnv),
  X =.. [C|Vs].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- op(500,xfx, inT).
%:- op(499,xfx, :).

% The value shows by Sicstus when an attributed variables is printed :
attribute_goal(Var, ((Var in Type) = Dom)) :-
  get_atts(Var, dom(Dom)),
  !,
  get_atts(Var, type(Type)).

attribute_goal(Var, (nodom(Var in Type))) :-
  get_atts(Var, type(Type)), !.

% Shows the attributes of an environment :
attribute_goal(Env, (env(Env) = [N, Awake, Mode, Meth, Nb])) :-
  get_k(Env, N),
  !,
  get_awake(Env, Awake),
  get_mode(Env, Mode),
  get_atts(Env, nb_actif(Nb)),
  get_apply_method(Env, Meth).
  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% verify_attributes(+Var, +Other, -Goals).
% 
% Prédicat appelé quand un action de type : 
%  X = Y avec X contenant peut-être des variables attribuée s'est produite. Ce
%  prédicat vérifie si les attributs de X et de Y sont compatible entre-elle.
%
%  Ainsi, si Y est un terme partiellement instancié, il vérifie si la tête du
%  terme est contenu dans le domaine de X et si les arguments de Y sont
%  compatible avec le constructeur.
%
%  Si Y est un variable non attribuée les attributs de X sont simplement copiés
%  dans Y.
%
%  Si Y est une variable attribuée, l'intersection des deux domaines est créé.
%  Si l'intersection est vide alors le prédicat échoue, s'il n'y a qu'une
%  valeur possible après intersection le terme est partiellement instancié.
%
%  Exemples :
%

/*
verify_attributes(X, Y, _Goal) :-
  get_atts(X, dom(DomX)), !,
  print('verify_attributes\n'),
  print('dom(X) ='),
  print(DomX),
  ( var(Y) ->
    get_atts(Y, dom(DomY)),
    print('\ndom(Y) = '),
    print(DomY)
  ;
    print('\nY = '),
    print(Y)
  ).
*/

/*
verify_attributes(X, Y, Goal) :-
  get_atts(X, type(TypeX)), !, 
  get_atts(X, dom(DomX)), !, 
  ( var(Y) ->
    get_atts(Y, dom(DomY)),
    ord_intersection(DomX,DomY,DomN),
    DomN = [HDomN|TDomN],
    ( TDomN = [] ->
      % Après intersection il n'y a plus qu'un seul élément :
      Goal = [set_constructor(Y, TypeX, HDomN)]
    ; % Il y a plus d'un élément après intersection :
      Goal = [],
      put_atts(Y, dom(DomN))
    )
  ; Y =.. [HY|_],
    Goal = [set_constructor(Z, TypeX, HY), Z = Y],
    ord_member(HY, DomX)
  ).
*/

verify_attributes(V1, Value, Goals) :-
  push_attributes(V1, Value, Goals).

/* check if attribute of V1 are compatible with the attributes of V2.
 V1 should always be a variable.
 fail if not.
 put the merged attributes in V2 and return to Goal the list of action to perform after the call */
push_attributes(V1, V2, Goals) :-
  get_atts(V1, type(TypeV1)) ->
  ( get_atts(V1, dom(DomV1)) ->
    ( var(V2) ->
      ( get_atts(V2, dom(DomV2)) ->
        /* V2 has a domain attribute */
        (call_rec([get_atts(V2, type(TypeV2)), TypeV1 = TypeV2]) -> true; throw('Erreur de type')
        ),
        ( ord_intersection(DomV1,DomV2,DomN),
          DomN = [_El|Els], /* fail if dom is empty */
          ( Els = [] ->
            Goals = [/*set_constructor(V2, TypeV1, El)*/]
          ; put_atts(V2, dom(DomN)),
            put_atts(V1, dom(DomN)),
            Goals = []
          )
        )
      ; /* V2 doesn't have a domain attribute */
        ( var(V2) ->
          put_atts(V2, dom(DomV1)),
          put_atts(V2, type(TypeV1)),
          Goals = []
        ; Goals = []
        )
      )
    ; /* V2 is a nonvar */
      V2 =.. [HV2 | _],
      ord_member(HV2, DomV1),
      %% type_envi(TEnv),
      %% args_from_cons_name(Vs, TypeV1, HV2, TEnv),
      %% Goals = [V1 =.. [HV2 | Vs]]     %%% Debug <- Why did i write this ?
      Goals = []
      %Goals = [set_type_deep(V2, TypeV1)]
    )
  ; /* V1 has no domain */
    (var(V2) -> put_atts(V2,type(TypeV1)); true),
    Goals = []
  )
; /* V1 has no attributes */
  Goals = [].

% merge_attributes(X, Y, Goal).
%
% Merge the attributes of X and Y. X and Y should be variable. Goal is a list
% of goals to execute after merge. This predicate fail if the two variables are
% not compatible over their attributes.
% 
% Put the result of the merge to Y.
%
/*
Exemples : 


Les trois cas d'intersection :

% Quand l'intersection donne un singleton (C2 = a) :
get_default_tenv(TEnv),
put_atts(C1, dom([a,d])),
put_atts(C1, type(constant)),
put_atts(C2, dom([a,c])),
put_atts(C2, type(constant)),
merge_attributes(C1, C2).

% Quand l'intersection donne un singleton (L2 = cons(X,Y)) :
get_default_tenv(TEnv),
put_atts(L1, dom([cons])),
put_atts(L1, type(list(constant))),
put_atts(L2, dom([cons,nil])),
put_atts(L2, type(list(constant))),
merge_attributes(L1, L2).

% Quand l'intersection donne un ensemble vide (fail):
get_default_tenv(TEnv),
put_atts(C1, dom([b,d])),
put_atts(C1, type(constant)),
put_atts(C2, dom([a,c])),
put_atts(C2, type(constant)),
merge_attributes(C1, C2, L).

% Quand l'intersection donne un ensemble de 2 éléments ou plus (dom(C2) = [a d]):
get_default_tenv(TEnv),
put_atts(C1, dom([a,b,d])),
put_atts(C1, type(constant)),
put_atts(C2, dom([a,d])),
put_atts(C2, type(constant)),
merge_attributes(C1, C2).

% Quand les deux sont des entiers et que l'intersection donne un singleton :
get_default_tenv(TEnv),
set_types([X,Y], int, TEnv),
X in -10..10,
Y in  10..20,
merge_attributes(X, Y).

% Quand les deux sont des entiers et que l'intersection donne un ensemble vide :
get_default_tenv(TEnv),
set_types([X,Y], int, TEnv),
X in -10..10,
Y in  11..20,
merge_attributes(X, Y).

*/

/*
merge_attributes(V1, V2) :-
  ( (is_int(V1); is_int(V2)),
    !,
    V1 #= V2
  ;
    ( get_atts(V1, dom(DomV1)),
      get_atts(V1, type(TypeV1)),
      ( get_atts(V2, dom(DomV2))
        %get_atts(V2, type(TypeV2))
        ->
        ( ord_intersection(DomV1,DomV2,DomN),
          DomN = [El|Els],
          %TypeV1 = TypeV2, % peut-etre pas besoin 
          ( Els = [] ->
            set_constructor(V2, TypeV1, El)
          ; put_atts(V2, dom(DomN)),
            put_atts(V1, dom(DomN))
          )
        )
      ; put_atts(V2, dom(DomV1)),
        put_atts(V2, type(TypeV1))
      )
    ; get_atts(V2, dom(DomV2)),
      get_atts(V2, type(TypeV2)),
      put_atts(V1, dom(DomV2)),
      put_atts(V1, type(TypeV2))
    ; /* ignore if no variable is attribued *
      true
    )
  ).
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% domain_disjoint_##(X, Y).
%
%  Verify if the two variables X and Y have disjoint domain.
%  fail if the domains are not disjoint.
%
% Exemples :
/*
Fail :
get_default_tenv(TEnv),
set_types([L1,L2], list(int), TEnv),
domain_disjoint_vv(L1, L2).
Pass : 
get_default_tenv(TEnv),
set_types([C1,C2], constant, TEnv),
put_atts(C1, dom([a,d])),
put_atts(C2, dom([b,c])),
domain_disjoint_vv(C1, C2).
*/

domain_disjoint_vv(X, Y) :-
  get_atts(X, dom(DomX)),
  get_atts(Y, dom(DomY)),
  ord_intersection(DomX, DomY, DomN),
  DomN == [].

/*
Fail :
get_default_tenv(TEnv),
set_types([L1,L2], list(int), TEnv),
domain_disjoint_vp(L1, cons(33,L2)).
Pass :
get_default_tenv(TEnv),
set_types([C1], constant, TEnv),
put_atts(C1, dom([a,d])),
domain_disjoint_vp(C1, b).
*/

domain_disjoint_vp(X, P) :-
  P =.. [HP|_],
  get_atts(X, dom(DomX)),
  \+(ord_member(HP, DomX)).

/*
Pass :
domain_disjoint_pp(
       cons(2, cons(45, nil)),
       cons(2, cons(3, nil)) ).
Fail :
domain_disjoint_pp(
       cons(1, cons(2, cons(3, nil))),
       cons(1, cons(2, cons(3, nil)))
         ).
*/

domain_disjoint_pp(P1, P2) :-
  P1 =.. [HP1|RP1],
  P2 =.. [HP2|RP2],
  ( HP1 == HP2 ->
    one_disjoint(RP1,RP2)
  ; true
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% one_disjoint(L1, L2).
%
% The both list must have same size.
%
% L1 = [X_1; ...] and L2 = [Y_1, ...].
%
% Checks if one couple of variables (X_i, Y_i) is disjoint.
%
% fail if all couple are not disjoint.

one_disjoint([], []) :- fail.
one_disjoint([X|RX], [Y|RY]) :-
  ( are_disjoint(X, Y) ->
    true
  ; one_disjoint(RX, RY)
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% are_disjoint(X, Y).
%
% Verify if the domain of X and Y are disjoint.
%
% Exemples :
/*
Fails :
are_disjoint(a, a).

put_atts(X, type(binary)),
put_atts(X, dom([one, zero])),
are_disjoint(zero(X), zero(one(nil))).

Passes:
are_disjoint(a, b).

put_atts(X, type(binary)),
put_atts(X, dom([one, zero])),
are_disjoint(zero(X), zero(nil)).
*/

are_disjoint(X, Y) :-
  ( var(X) ->
    ( var(Y) ->
      domain_disjoint_vv(X, Y)
    ; domain_disjoint_vp(X, Y)
    )
  ; 
    ( var(Y) ->
      domain_disjoint_vp(Y, X)
    ; domain_disjoint_pp(X, Y)
    )
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% unifyD_##(X, Y).
%
% # = v  : the argument is a variable
% # = p  : the arguement is not a variable
%
% unify two attributed variables or two list of variable.
%
% the first argument should be an attributed variable. The second argument could
% be a partial ground term or a variable attributed or not.
% Exemples :
/*

% Quand l'intersection donne un ensemble de 2 éléments ou plus (dom(C2) = [a
% d]):
get_default_tenv(TEnv),
set_types([I1,I2,I3,I4], int, TEnv),
I1 in -10..30,
I2 in  -3..50,
set_type(L, list(int), TEnv),
L1 = cons(10, cons(I3, cons(I1, cons(34, nil)))),
L2 = cons(10, cons(35, cons(I2, L))),
unifyD(L1, L2).

*/

unifyD_vp(X, P) :-
  P =.. [HP|TP],
  ( get_atts(X, dom(DomX)), !,
  % Inutile si on on suppose la surete de typage :
  % get_atts(X, type(TypeX)),
  % type_envi(TEnv),
  % args_from_cons_name(Vs, TypeX, HP, TEnv),
  % unifies(TP, Vs)
    ord_member(HP, DomX),
    X =.. [HP| TP]
  ; X =.. [HP| TP]
  ).

unifies([], []) :- !.
unifies([X|LX],[Y|LY]) :-
  unifyD(X,Y),
  unifies(LX,LY).

unifyD(X, Y) :-
  X = Y.
/*  ( (is_int(X); is_int(Y)), !, X #= Y) ;
    var(X) ->
    ( var(Y) ->
      merge_attributes(X,Y),
      X = Y
    ;
      unifyD_vp(X,Y)
    )
  ;
    (var(Y) ->
      unifyD_vp(Y,X)
    ; X =.. [HX|RX],
      Y =.. [HY|RY],
      HX = HY,
      unifies(RX, RY)
    ).
*/

% R should be a FD variable in [0, 1].
unifyD(R, X, Y) :-
  ( (is_int(X); is_int(Y)), !, R #<=> X #= Y);
  ( R == 1 -> unifyD(X, Y)
  ; 
    ( R == 0 -> disunifyD(X, Y)
    ; 
      ( are_disjoint(X, Y) ->
        (disunifyD(X, Y), R = 0)
      ;
        ( X == Y ->
          R = 1
        ;
          when(ground(R), unifyD(R, X, Y)),
          when((ground(X), ground(Y)), unifyD(R, X, Y))  % attempt /!\
        )
      )
    )
  ).

unifyD_pattern(X, Pat) :-
  Pat =.. [HPat| _],
  ( var(X) ->
    get_atts(X, dom(DomX)),
    get_atts(X, type(TypeX)),
    ord_member(HPat, DomX),
    set_constructor(X, TypeX, HPat),
    X = Pat
  ; X = Pat
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% not_equal_##(X, Y)
%
% Constraint two variables to be disequal.

not_equal_vv(X, Y) :-
  ( (is_int(X) ; is_int(Y)), !, X #\= Y)
  ; when( 
    (nonvar(X); nonvar(Y)),
      ( var(X) ->
        not_equal_vp(X, Y)
      ;
        ( var(Y) ->
          not_equal_vp(Y, X)
        ; not_equal_pp(X, Y)
        )
      )
    ).

not_equal_vp(V, Partial) :-
  Partial =.. [H| _],
  get_atts(V, type(TypeV)),
  ( constructor_is_constant(H,TypeV) ->
    get_atts(V, dom(DomV)),
    ord_del_element(DomV, H, DomN),
    put_atts(V, dom(DomN))
  ;
    when(nonvar(V), not_equal_pp(V, Partial))
  ).

not_equal_pp(P1, P2) :-
  ( are_disjoint(P1, P2) ->
    true
  ;
    when(
      (ground(P1), ground(P2)), 
        P1 \= P2
      )
  ).

% disunifyD(X, Y).
%
% Disequality of terms
%
%
% Exemple :
/*
get_default_tenv(TEnv),
set_types([L1,L2],list(int), TEnv),
set_type(I, int, TEnv),
disunifyD(L1, cons(I, L2)),
unifyD(L2, nil),
not_a_constructor(L1,nil).
*/

disunifyD(X, Y) :-
  not_equal_vv(X, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% antiunifyD(X, L, T).
%
% make the union of the domains of variables in L and returns the result in X.
% the variables in L must be of type T.
%
/*
antiunifyD(X, [nil, zero(nil), one(nil)], binary).

get_default_tenv(TEnv),
set_types([I1,I2,I3], int, TEnv),
I1 in -50..45,
I2 in 70..95,
I3 in 47..49,
antiunifyD(X, [cons( 1, cons(I1, cons(18,nil))),
               cons(I2, cons(I3, cons(13,nil)))], list(int)).
*/

antiunifyint_ii(R, I1, I2) :-
  R in {I1}\/{I2},
  (var(R) -> put_atts(R, type(int)); true).

antiunifyint_vi(R, X, I) :-
  fd_dom(X, RX),
  %list_to_fdset([I], SY),
  %fdset_union(SX, SY, SN),
  %fdset_to_range(SN, Range),
  R in {I}\/RX,
  (var(R) -> put_atts(R, type(int)); true).

antiunifyint_vv(R, X, Y) :-
  fd_dom(X, RX),
  fd_dom(Y, RY),
  R in RX\/RY,
  (var(R) -> put_atts(R, type(int)); true).

antiunifyint(R, X, Y) :-
  (var(X) ->
    ( var(Y) ->
      antiunifyint_vv(R, X, Y)
    ; antiunifyint_vi(R, X, Y)
    )
  ;
    ( var(Y) ->
      antiunifyint_vi(R, Y, X)
    ; antiunifyint_ii(R, X, Y)
    )
  ).

% antiunifyD(-Res, +L, +T).
%
% Returns in Res the antiunification (union) of all the variables in L. T is the
% type of the elements in T.
%

antiunifyD(_, [], _) :- fail.
antiunifyD(X, [E|R], T) :-
  antiunifyDaux(X, E, R, T).

% antiunifyDaux(-Res, +E, +L, +T).
%
% An auxiliary predicate of antiunifyD.
% antiunifyDaux(Res, E, L, T) is equivalent to antiunifyD(Res, [E|L], T).
%

antiunifyDaux(C, E1, [], _) :- !, C = E1.
antiunifyDaux(C, E1, [E2|L], T) :-
  antiunifyDtwo(R, E1, E2, T),
  antiunifyDaux(C, R, L, T).

% antiunifyDtwo(-R, +X, +Y, +T).
%
% Computes the antiunification of the variables (or partial values) X and Y in
% R. T is the type of X and Y.

antiunifyDtwo(R, X, Y, T) :-
  ( (is_int(X); is_int(Y)), !,
    antiunifyint(R, X, Y)
  )
  ; ( var(X) ->
    ( var(Y) ->
      antiunifyD_vv(R, X, Y, T)
    ; antiunifyD_vp(R, X, Y, T)
    )
  ;
    ( var(Y) ->
      antiunifyD_vp(R, Y, X, T)
    ;
      antiunifyD_pp(R, X, Y, T)
    )
  ).

% antiunifyD_vv(-R, +X, +Y, +T).
%
% Likes antiunifyDtwo but X and Y are variables.

antiunifyD_vv(R, X, Y, T) :-
  get_atts(X, dom(DomX)),
  get_atts(Y, dom(DomY)),
  ord_union(DomX, DomY, DomN),
  put_atts(R, dom(DomN)),
  put_atts(R, type(T)).

% antiunifyD_vv(-R, +X, +Y, +T).
%
% Likes antiunifyDtwo but X is a variable and Y is a partial value.

antiunifyD_vp(R, X, P, T) :-
  P =.. [HP| _],
  get_atts(X, dom(DomX)),
  ord_union([HP], DomX, DomN),
  put_atts(R, dom(DomN)),
  put_atts(R, type(T)).

% antiunifyD_vv(-R, +X, +Y, +T).
%
% Likes antiunifyDtwo but X and Y are both partial values.

antiunifyD_pp(R, P1, P2, T) :-
  P1 =.. [HP1| RP1],
  P2 =.. [HP2| RP2],
  ( HP1 = HP2 ->
    type_envi(TEnv),
    args_type_from_cons_name(Ts, T, HP1, TEnv),
    antiunifiesD(VP, RP1, RP2, Ts),
    R =.. [HP1 | VP]
  ; ord_union([HP1], [HP2], DomN),
    put_atts(R, dom(DomN)),
    put_atts(R, type(T))
  ).

% antiunifiesD(-R, +L1, +L2, +Ts).
%
% L1, L2, Ts are lists of same lengths.
%
% Returns in R the list of variables containing the pairwise antiunification
% of variables in L1 and L2. Ts contains the types of elements in L1, L2.

antiunifiesD([], [], [], _) :- !.

antiunifiesD([R|Rs], [V1|V1s], [V2|V2s], [T|Ts]) :-
  antiunifyDtwo(R, V1, V2, T),
  antiunifiesD(Rs, V1s, V2s, Ts).

% antiunify_vardef_list(+VarDefs, +L1, -L2).
%
% VarDefs is a list of var(X,T) where T is a type.
% L1 is a list of list of variables or partial values.
%
% Performs the antiunification of the elements in each sublist of L1 and
% returns the result in L2.

antiunify_vardef_list([], [],[]) :- !.
%antiunify_vardef_list(_, [],[]) :- !. % Cas ou il n'y a pas de valeurs à merger

antiunify_vardef_list([var(X,_)|VarDef], [],[X|L3]) :-
  !,
  antiunify_vardef_list(VarDef, [], L3).

antiunify_vardef_list([var(_,T)|VarDef], [L1|L2],L3) :-
  !,
  antiunifyD(X,L1,T),
  antiunify_vardef_list(VarDef, L2,L4),
  L3 = [X|L4].

% equal_vardef_list(Vardefs, L).
%
% VarDefs is a list of var(X,T) where T is a type.
% L is a list of variables or partial values.
%
% Unify the pairwise unification of the elements of L with the X variables of Vardefs.

equal_vardef_list([], []) :- !.
equal_vardef_list([var(X,_)|VarDef], [V1|R]) :-
  X = V1,
  equal_vardef_list(VarDef, R).

% remove_constructor(X, C).
%
% Remove the constructor C from the domain of X.
% If the variable is instanciated look if the head is different from the
% constructor.
%
% Afterwards :
% If the domain is empty fail.
% If the domain is a singleton instanciate X.
% Example :
/*
get_default_tenv(TEnv),
set_type(L, list(int), TEnv),
remove_constructor(L,cons(X,Y)).
*/

remove_constructor(X, Y) :-
  Y =.. [F| _],
  ( var(X) ->
    get_atts(X, dom(DomX)),
    ord_del_element(DomX,F, DomN),
    ( DomN = [] -> fail
    ;
      ( DomN = [C] ->
        get_atts(X, type(TypeX)),
        set_constructor(X, TypeX, C)
      ; put_atts(X, dom(DomN))
      )
    )
  ; X =.. [HX | _],
    HX \= F
  ).

% get_default_tenv(TEnv), set_types([L1,L2],list(int), TEnv), set_type(I, int, TEnv), disunifyD(L1, cons(I, L2)), unifyD(L2, nil), remove_constructor(L1,nil), I = 40, L1 = cons(40, nil).

%findalldestroy(Goal) :-
%  call(Goal), fail.

