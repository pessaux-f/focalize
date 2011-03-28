% prolog source file
%:- module(types,
%          [type_envi/1,
%           get_default_tenv/1,
%           get_ord_constructors/3,
%           constructor_is_constant/2,
%           args_type_from_cons_name/4
%          ]
%          % ,hidden(true) % debugging is disable inside this module
%         ).


:- use_module(library(assoc)).

get_default_tenv(TEnv) :-
  Type_L = [def(list(a),
              [ cons(a,list(a)),
                nil
              ]),
            def(constant, [a, b, c, d]),
            def(bin_tree(a),
              [leaf(a),
               node(a,bin_tree(a),bin_tree(a))
              ]),
            def(tree(a),
              [node(list(a))
              ]),
            def(couple(a,b),
              [pair(a,b)]),
            def(binary,
              [nil,
               one(binary),
               zero(binary)
              ]),
            def(triangle,
              [equilateral,
               error,
               isoscele,
               scalene
              ])
           ],
  set_type_env(Type_L, TEnv).

% Un type de donnée FoCal est répresenté en prolog par une liste
% d'association. Il associe à son nom de constructeur la liste des arguments
% du constructeur.
%
% A l'origine, un type est donnée par une liste de :
% constructeur(T1, ..., Tn)
% où T1, ..., Tn sont les noms des types 

% assoc_from_pair_list(+L, -A).
%
% exemple:
% assoc_from_pair_list([(1,4),(5,6),(2,4)], T).

assoc_from_pair_list([], A) :-
  empty_assoc(A).

assoc_from_pair_list([(C, V)|R], A) :-
  assoc_from_pair_list(R, AA),
  put_assoc(C, AA, V, A).



% instanciate_args(+T, +Vargs, -T2) :=
% Get a type T, a list of association Vargs.
% Replace each occurence of L with correspond to a element associated in Vargs
% to the element associated.
%
% Exemple:
% assoc_from_pair_list([(a,int)], Vargs),
% instanciate_args(list(list(a)), Vargs, Res).
%
% assoc_from_pair_list([(a,int),(b,list(int))], Vargs),
% instanciate_args(list(crp(list(a),b)), Vargs, Res).

instanciate_args(Type, Vargs, T2) :-
  Type =.. [E|L],
  (L == [] ->
    ((get_assoc(E,Vargs,New), !,
      T2 = New
    );
    T2 = E)
    ;
    (instanciate_args_list(L, Vargs, L2),
     T2 =.. [E | L2]
    )
  ).
     
% Same, for list :
instanciate_args_list([], _Vargs, L2) :-
  L2 = [].

instanciate_args_list([E|R], Vargs, L2) :-
  instanciate_args_list(R, Vargs, LL),
  instanciate_args(E, Vargs, EE),
  L2 = [EE | LL].
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_type(+L, -T).
%
% Converts a list of constructor to a type representation.
%
% La fonction est découpé en fonction de la liste L
%
% L = []:
% Pas de constructeur alors la liste d'association T est vide.
%
% L = [E|R]:
% Au moins un constructeur, on crée sa liste d'association :
% On le décompose en son nom de constructeur Cons et liste de ses arguments
% Args. On ajoute ensuite dans la liste d'association correspondant au reste le
% couple (Cons,Args) (cela donne la valeur de retour T).
%
% Exemple :
% get_type([nil, cons(a, list(a))], Vargs, T).

get_type([], T) :-
  empty_assoc(T).

get_type([E|R], T) :-
  E =.. [Cons | Args],  
  get_type(R , TT),
  put_assoc(Cons, TT, Args, T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set_type_env(+Type_L,)
%
% Each element of Type_L is a pair of (Type_name(arguments), Constructor_list).
% The constructor list is a liste of ground Herbrand term.
%
% Exemple :
% Type_L = [(list(a),[nil,cons(a,list(a))]),(bin_tree(a),[leaf(a),node(a,bin_tree(a),bin_tree(a))]),(tree(a),[node(list(a))])],
% get_type_env(Type_L, Tenv).

get_type_env([], TEnv) :-
  empty_assoc(TEnv).


get_type_env([def(Name, Cons_L)|R], TEnv) :-
  get_type_env(R, TEnv2),
  get_type(Cons_L, T_rep), 
  Name =.. [T_n|T_a],
  put_assoc(T_n, TEnv2, (T_a, T_rep), TEnv).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set_type_env(TEnv).
%

set_type_env(Type_L, TEnv) :-
  get_type_env(Type_L, TEnv),
  retractall(type_envi(_)),
  assert(type_envi(TEnv)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% get_ord_constructors(-L, +T, +TEnv).
%
% From a type T and a environnement of type TEnv, returns the list of all
% constructor name of T. 
%
% Exemple:
% Type_L = [(list(a),[nil,cons(a,list(a))]),(bin_tree(a),[leaf(a),node(a,bin_tree(a),bin_tree(a))]),(tree(a),[node(list(a))])],
% get_type_env(Type_L, TEnv).
%
% get_ord_constructors(L, list(int), TEnv).
% get_ord_constructors(L, bin_tree(int), TEnv).

get_ord_constructors(L, T, TEnv) :-
  T =.. [T_name | _Rest],
  get_assoc(T_name, TEnv, (_T_a, T_rep)),
  assoc_to_list(T_rep, Lcons),
  retire_tiret(Lcons, L).

retire_tiret([-(E,_U)|R1], [E|R2]) :-
  !, retire_tiret(R1,R2).
retire_tiret([], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% constructor_os_constant(C, T, TEnv).
%
% fail if constructor C of type T is not a constant.
%
% Exemples :
%
% get_default_tenv(TEnv).
%
% Pass : constructor_is_constant(a, constant).
%
% Fail : constructor_is_constant(cons, list(int)).
%
% Pass : constructor_is_constant(nil, list(int)).
%
% Fail : constructor_is_constant(truc, list(int)).


constructor_is_constant(C, T) :-
  type_envi(TEnv),
  T =.. [T_h| _ ],
  get_assoc(T_h, TEnv, (_, Cs)),
  get_assoc(C, Cs, LArgs),
  LArgs == [].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Get a type and the applied argument, a constructor name CN.
% Returns the list containing the type of all arguments of CN instanciated by
% the argument of the type. constructthe constructor instanciate by the
% arguments of the type.
%
% Type_L = [(list(a),[nil,cons(a,list(a))]),(bin_tree(a),[leaf(a),node(a,bin_tree(a),bin_tree(a))]),(tree(a),[node(list(a))])],
% get_type_env(Type_L, TEnv),
% T = list(tree(bin_tree(int))),
% args_type_from_cons_name(Ret, T, cons, TEnv).
% args_type_from_cons_name(Ret, list(int), cons, TEnv). % doit retourner [int, list(int)]

args_type_from_cons_name(Ret, T, Cons_name, TEnv) :-
  T =.. [T_h|T_eff],
  get_assoc( T_h, TEnv, (E_exp, Cons)),
  get_assoc( Cons_name, Cons, Tmp),
  list_combine(E_exp, T_eff, VArgs),
  assoc_from_pair_list(VArgs,VArgs2),
  instanciate_args_list(Tmp, VArgs2, Ret).

list_combine([], [], []).

list_combine([E1|R1], [E2|R2], L3) :-
  list_combine(R1,R2,R3),
  L3 = [(E1, E2)| R3].
