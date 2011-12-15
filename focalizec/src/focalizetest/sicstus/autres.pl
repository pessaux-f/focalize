%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%NOM :   autres.pl
%
%FONCTION :
%        contient toutes les fonctions génériques ainsi que la gestion de
%        l'environnement 
%    
%HISTORIQUE :
%       M. Carlier 22/07/2008 Création
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Un environnement est défini par les attributs suivants :

%:- module(environment,
%          [ init_env/1,
%            init_env/2,
%            init_env/3,
%            init_env/4,
%          ]
%         ).

:- use_module(library(atts)).

:- attribute k/1, functions/1, awake/1, mymode/1, apply_method/1, nb_actif/1,
             dom/1, type/1, exploring/1.

set_exploring(Env, Bool) :-
  put_atts(Env, exploring(Bool)).

set_k(Env, N) :-
  put_atts(Env, k(N)).

set_functions(Env, Funs) :-
  put_atts(Env, functions(Funs)).

set_awake(Env, Awake) :-
  put_atts(Env, awake(Awake)).

set_mode(Env, Mode) :-
  put_atts(Env, mymode(Mode)).

set_apply_method(Env, Meth) :-
  put_atts(Env, apply_method(Meth)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_exploring(Env) :-
  get_atts(Env, exploring(1)).

get_k(Env, N) :-
  get_atts(Env, k(N)).

get_functions(Env, Funs) :-
  get_atts(Env, functions(Funs)).

get_awake(Env, Awake) :-
  get_atts(Env, awake(Awake)).

get_mode(Env, Mode) :-
  get_atts(Env, mymode(Mode)).

get_apply_method(Env, Meth) :-
  get_atts(Env, apply_method(Meth)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% init_env(-ENV)     initialisation de l'environnement (K = 1 par defaut)
% init_env(-ENV,+K)  initialisation de la variable d'environnement avec K :
%                    politique de reveil
% init_env(-ENV,+K, +Opt)
%                    Donne en plus d'indication sur le traitement des ite/match
%
%          Attention1 : K entier, K > 0 sinon K = 1 par defaut
%          Attention2 : 2 env differents "travaillent" sur le meme store de
%          contraintes !
% Opt peut prendre les valeurs suivantes :
% - meta : on utilise les meta-contraintes avec les règles arrières (valeur par
%                                                                        defaut)
% - noback : on utilise les meta-contraintes sans les règles arrières
% - naive : on utilise les version naive de ite et match
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_nb_constraint(Env, Nb) :-
  get_atts(Env, nb_actif(Nb)).

add_a_constraint(Env) :-
  get_atts(Env, nb_actif(N)),
  NpO is N + 1,
  (
%   NpO > 1500 ->
%    raise_exception('Trop de contraintes simultanément')
%  ;
    true
  ),
  put_atts(Env, nb_actif(NpO)).

remove_a_constraint(Env) :-
  get_atts(Env, nb_actif(N)),
  NmO is N - 1,
  put_atts(Env, nb_actif(NmO)).

init_env(Env) :-
  set_exploring(Env, 0),
  set_k(Env, 2),
  empty_assoc(Vide),
  set_functions(Env, Vide),
  Awake in 0..10000,
  set_awake(Env, Awake),
  set_apply_method(Env, assert),
  put_atts(Env, nb_actif(0)),
  set_mode(Env, meta).

init_env(Env, K):-
  set_exploring(Env, 0),
  set_k(Env, K),
  empty_assoc(Vide),
  set_functions(Env, Vide),
  Awake in 0..10000,
  set_awake(Env, Awake),
  set_apply_method(Env, assert),
  put_atts(Env, nb_actif(0)),
  set_mode(Env, meta).

init_env(Env, K, Mode):-
  set_exploring(Env, 0),
  set_k(Env, K),
  empty_assoc(Vide),
  set_functions(Env, Vide),
  Awake in 0..10000,
  set_awake(Env, Awake),
  set_apply_method(Env, assert),
  put_atts(Env, nb_actif(0)),
  set_mode(Env, Mode).

init_env(Env, K, Mode, Apply):-
  set_exploring(Env, 0),
  set_k(Env, K),
  empty_assoc(Vide),
  set_functions(Env, Vide),
  Awake in 0..10000,
  set_awake(Env, Awake),
  set_apply_method(Env, assert),
  put_atts(Env, nb_actif(0)),
  set_mode(Env, Mode).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fin_env(+ENV)   predicat a positionner en fin de systeme de contraintes
%  ex :   ?- init_env(_ENV,10), fin_env(_ENV).
%                _ENV = env('$mutable'(1,0),1,10)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fin_env(Env) :-
   get_awake(Env, Awake),
   clpfd:fd_max(Awake,N),
   NmO is N - 1,
   Awake in 0..NmO.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% decr_k(+ENV) decremente la variable representant la politique de reveil.
% ex : init_env(_ENV,10,3),decr_k(_ENV).
%           _ENV = env('$mutable'(2,0),_A,10)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
decr_k(Env):-
  get_k(Env, K),
  ( K > 0 ->
    KmO is K - 1,
    set_k(Env, KmO),
    set_exploring(Env, 1)
  ; fail
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% env_not_awakable(+ENV). vrai ssi K_FLAG == 0.
% ex : init_env(ENV,5),decr_k(ENV),env_not_awakable(ENV). -- no
% ex : init_env(ENV,1),decr_k(ENV),env_not_awakable(ENV). -- ENV = env('$mutable'(0,0),_A)
% ex : init_env(ENV),env_not_awakable(ENV).               -- no        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
env_not_awakable(Env):-
  get_k(Env, K),
  K == 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% use_meta(+ENV). vrai ssi on utilise les meta-contraintes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
use_meta(Env) :-
  get_mode(Env, Mode),
  Mode \= naive.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% use_back_rule(+ENV). vrai ssi on utilise les regles arriere
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
use_back_rule(Env) :-
  get_mode(Env, Mode),
  Mode \= noback.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_reveil(+ENV,-Reveil). acces a la variable Reveil
% ex : init_env(ENV,10), get_reveil(ENV,Reveil) .      
%           -- ENV = env('$mutable'(1,0),Reveil,10)
% DEPRECATED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% unset_dbg(+Env). Disactivate the CLP(FD) debugger.
% DEPRECATED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%unset_dbg(Env) :-
%  Env = env(_MUT, _Reveil, _Opt, Dbg),
%  Dbg == 0 -> true; (fdbg_off, Dbg = 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set_dbg(+Env). Activate the CLP(FD) debugger.
% DEPRECATED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%set_dbg(Env) :-
%  Env = env(_MUT, _Reveil, _Opt, Dbg),
%  Dbg == 0 -> true; fdbg_on.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_dbg_status(-Res, +Env).
% DEPRECATED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%get_dbg_status(Res, Env) :-
%  Env = env(_MUT, _Reveil, _Opt, Dbg),
%  Dbg == 0 -> Res = 0; 
%  (Dbg == 1 -> Res = 0; Res = 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% add_dom(+Xs,-S) vrai ssi Xs est une liste de variable a domaine finie et,
%                  S une liste de condition de reveil portant sur ces variables.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_dom([],[]).
add_dom([X|Xs],[dom(X)|S]) :-
        var(X),
        !,
        add_dom(Xs,S).
add_dom([_X|Xs],S) :-
        add_dom(Xs,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% call_rec(+L)      pose chaque element de L dans le store
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
call_rec([]).
call_rec([X|S]) :-
        X,
        call_rec(S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% call_rec(+L1, +L2, -L3)      L3 est la concaténation des deux premières listes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

append([E|R], L2, L3) :-
  !,
  append(R,L2, Tmp),
  L3 = [E|Tmp].

append([],L,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_domain_constraint(+L1,-L2).
%
% From a list of fd variable, returns the list of constraint X in min(X)..max(X)
% for all X in L1.
%
% Ex :
% X in 0..10, Y in -10..5, Z in 5..13, get_domain_constraint([X,Y,Z],R).

get_domain_constraint([], []) :- !.

get_domain_constraint([E|R], [EE|RR]) :-
  var(E),
  !,
  clpfd:fd_max(E,Max),
  clpfd:fd_min(E,Min),
  EE = (E in Min..Max),
  get_domain_constraint(R,RR).

get_domain_constraint([_E|R], RR) :-
  get_domain_constraint(R,RR).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The predicates that handle the refresh atoms. the most important predicate
% here is:
% new_atom(-A).
%
% returns a fresh new atom.
%
% Examples:
% ?- new_atom(A) 
% A = cst__a
% ?- new_atom(A)
% A = cst__b

% the new fresh atom represented as a list of char code
:- dynamic get_last_atom/1.
get_last_atom(L) :-
  atom_codes(a, L).

% update thenext fresh atom
save_atom(L) :-
  retractall(get_last_atom(_)),
  assert(get_last_atom(L)).


% Take a list of char code (representing an atom). Return the list of char
% codes representing the next atom
get_next_atom(N,[E|R]) :-
  atom_codes(z, [E]) ->
  get_next_atom(NN,R),
  atom_codes(a,[T]),
  N = [T|NN]
;
  EE is E  + 1,
  N = [EE|R].

get_next_atom(N,[]) :-
  atom_codes(a,N).

% Creates a new atom prefixed by cst__.
new_atom(X) :-
  get_last_atom(L),
  % Create the current atom
  atom_codes(cst__, Prefix),
  append(Prefix, L, Return),
  atom_codes(X, Return),
  % print(X),
  % Save the next atom 
  get_next_atom(N,L),
  save_atom(N),
  !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% new_atom_list(+L1, -L2).
%
% generates many fresh atom in L2. L2 and L1 have the same length.
% Examples:
% ?- new_atom_list([1,2,3,4], L).
% L = [cst__a, cst__b, cst__c, cst__d)
%

new_atom_list([],[]) :- !.
new_atom_list([_|R],L) :-
  !,
  new_atom_list(R,L2),
  new_atom(X),
  L = [X|L2].

% In an ite and a match, a variable is encapsulated in a term of the form :
% var(X, type).
% thus when X is bound the type is recoverable.

% Accessors :
get_var_vardef(var(X, _), X).
get_type_vardef(var(_, T), T).

% Same for lists :
get_var_vardef_list([], []).
get_var_vardef_list([var(X,_)|R1], [X|R2]) :-
  get_var_vardef_list(R1,R2).

% Takes only variable of type int or bool (FD variables). 
get_FD_vardef_list([], []).
get_FD_vardef_list([var(X,T)|R], L) :-
  get_FD_vardef_list(R,L2),
  ((T == int; T == bool) -> L = [X|L2]; L = L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_firsts(+L1, -L2, -L3).
%
% L1 is a list of List.
%
% It returns in L2 the head of each List and in L3 the tail of each List.
%
% Examples:
% ?- get_firsts([[1,2,3],[4,5,6],[7,8,9]], Hs, Ts).
% Hs = [1, 4, 7],
% Ts = [[2,3], [5,6], [8,9]].
%

get_firsts([],[],[]) :- !.
get_firsts([[]|_],[],[]) :- !.
get_firsts([[E|R]|R2],RHs,RTs) :-
  get_firsts(R2,Hs,Ts),
  RHs = [E|Hs],
  RTs = [R|Ts].
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% transposed(+L1, -L2).
%
% L1 is a matrix (coded as a list of list).
%
% returns the transposed of L1 in L2.

transposed([],[]) :- !.
%transposed([[]],[]) :- !.
transposed(L, R) :-
  get_firsts(L, Hs, Rs),
  (Rs == [] ->
    R = Hs
  ;
    transposed(Rs, NL),
    R = [Hs| NL]
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set_atom(+A, +L).
%
% A is an atom and L is a list.
%
% Etablish the dynamic predicate A(L) in the predicate database.
%
% Examples:
% ?- a(_).
% no
% ?- set_atom(a,[1,2,3,4]).
% yes
% ?- a(L).
% L = [1,2,3,4].

set_atom(Atom, Vars) :-
  AA =.. [Atom, Vars],
  AB =.. [Atom, _],
  retractall(AB),
  assert(AA).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% free_atom(+A).
%
% Takes an atom A and free the dynamic predicates A/1.

free_atom(Atom) :-
  % AB =.. [Atom, [_]],
  % retractall(AB),
  abolish(Atom).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% free_atom(+L).
%
% Takes an list of atom L and free the dynamic predicates A/1 for all A in L.

free_atoms([]) :- !.
free_atoms([Atom|R]) :-
  !,
  free_atom(Atom),
  free_atoms(R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_and_free_atoms(+L1, -L2).
%
% L1 is a list of atoms.
%
% returns in L2 the list of arguments of the dynamics predicate in L1. free the
% invoked predicates
%
% Examples:
% a(4). b([t,t,t]).
% ?- get_and_free_atoms([a,b], L).
% L = [4, [t,t,t]).

get_and_free_atoms([], []) :- !.
get_and_free_atoms([A|AL], [Arg|Args]) :-
  !,
  P =.. [A, Arg],
  call(P),
  % FP =.. [A, _],
  % retractall(FP),
  abolish(A),
  get_and_free_atoms(AL, Args).



tuple_of_list([E], E) :- !.

tuple_of_list([E|R], T1) :-
  T1 =.. [',', E, T2],
  tuple_of_list(R, T2).
 
