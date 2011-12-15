% prolog program
:- [library(random)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Labeling avec choix de valeur (pseudo-) aléatoire
% ex d'appel :
% init_env(_ENV), J0 in 0..6, X #<=> (I0 #= 0),ite(X,[J0,J2],[J2#=J0-1],[J2#=J0], _ENV), X = 1, fin_env(_ENV), my_labeling([random], [J0,J2]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
my_labeling(L1,L2) :-
        !,
        filtre_label(L2, LVAR),
%        clpfd:domain(LVAR,-33554432, 33554431), % les bornes maxi des FD_vars
        my_labeling_1(L1,LVAR).

my_labeling_1([random],L2) :-
        !, % Modif Matthieu
 %       ran_rec(L2,L2).
        labeling_random(L2).

my_labeling_1(L1,L2) :-
        clpfd:labeling(L1,L2).

ran_rec([],L2) :-
        integer_list(L2,L_REST),
        ((L_REST == [],!) ; ran_rec(L2,L2) ).
ran_rec([X|S],L2) :-
        number(X),
        !,
        ran_rec(S,L2).
ran_rec([X|S],L2) :-
        clpfd:fd_min(X,A),
        clpfd:fd_max(X,B),
        BB is B + 1, % Modif Matthieu
        random:random(A,BB,RAN),
        (X #= RAN ; X #> RAN ; X #< RAN ),
        ran_rec(S,L2).

labeling_random([]) :- !.
labeling_random([X|L2]) :-
        number(X),
        !,
        labeling_random(L2).
labeling_random([X|L]) :-
  % Generate a random value for X
  clpfd:fd_min(X,A),
  clpfd:fd_max(X,B),
  BB is B + 1,
  random:random(A, BB, Value),
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  (X  #= Value, labeling_random(L);
   X #\= Value, labeling_random([X|L])
  ).
  
  

filtre_label([],[]) :-!.
filtre_label([X|Xs],Ys) :-
        ground(X),
        !,
        filtre_label(Xs,Ys).
filtre_label([X|Xs],[X|Ys]) :-
        filtre_label(Xs,Ys).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% integer_list(+L,--L1)
%
%  vrai ssi L est une liste et L1 est le plus grand reste de L, qui commence
%  par autre chose qu'un entier
%  ex d'appel : ?- integer_list([], L).             L = []
%               ?- integer_list(t([]),J).           no
%               ?- integer_list([3,89,X,67], L1).   L1 = [X,67]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
integer_list(L,_L1) :-
        %\+( listq(L) ),
        \+( lists:is_list(L) ),
        !,
        fail.
integer_list([],[]) :-
        !.
integer_list([X|L],L1) :-
        integer(X),
        !,
        integer_list(L,L1).
integer_list(L,L).


%%%%%%%%%%%%%%%%%%%%
%
%
% Exemple :
% init_env(_Env),
%   match([X,Y,Z],
%         [  ([X = nil],[]),
%            ([X = cons(Y,Z), [])
%         ],
%         _Env
%        )


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% label_only_ints(+L, TEnv, Env).
%
% Take a list of variables (that could be partially intanciated). The variables
% in the lists that are of integer types.
% 
% If the function encounters a variable that is not an integer, fail !

/*
label_only_ints([], _TEnv, Env) :- !, fin_env(Env).

label_only_ints([H|T], TEnv, Env) :-
  label_only_int(H, TEnv, Env),
  label_only_ints(T, TEnv, Env).

label_only_int(X, TEnv, Env) :-
 (is_int(X) ->
    my_labeling([random], [X])
  ; ( var(X) ->
      fail
    ;
    ( X =.. [_|T],
      label_only_ints(T, TEnv, Env)
    )
    )
 ).
*/


% For debugging, calculates the depth of a term :
label_terms_dbg_depth_aux([], 0).
label_terms_dbg_depth_aux([X|R], D) :-
  label_terms_dbg_depth_aux(R, D1),
  label_term_dbg_depth_aux(X, D2),
  D is max(D1, D2).

label_term_dbg_depth_aux(X, D) :-
  var(X) -> D = 1;
  (
    X =.. [_|Xs],
    label_terms_dbg_depth_aux(Xs, D1),
    D is D1 + 1
  ).

label_term_dbg_depth(X) :-
  label_term_dbg_depth_aux(X, D),
  write('term is depth '),
  write(D), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% label_term(+X, LInt, +Max, +TEnv, +Env).
%
% X is the variable to label.
% LInt is the returned variable list.
% Max is the maximum depth of the labeling. 
% TEnv is the environnement of types.
% Env is the ite/match environnement.
%
% Label the term X by a top-down instanciation. The maximum of label depth is
% Max. fail if the variable is not ground when we reach the maximum depth.
% 
% X is assumed to be a term variable.

label_algebraic_aux(_X, _LInt, 0, _TEnv, _Env) :- !, fail.

label_algebraic_aux(X, LInt, Max, TEnv, Env) :-
  var(X) ->
    ( get_atts(X, dom([C|_])),
      ( get_atts(X, type(T)),
        args_from_cons_name(Vs, T, C, TEnv),
        X =.. [C|Vs],
        %fin_env(Env),  fin_env(Env),
        NMax is Max - 1,
        label_algebraics_aux(Vs, LInt, NMax, TEnv, Env)
      ; % if the first constructor is not the good one :
        remove_constructor(X, C),
        NMax is Max - 1,
        label_algebraic_aux(X, LInt, NMax, TEnv, Env)
      )
    );
    ( X =.. [_|T],
      NMax is Max - 1,
      label_algebraics_aux(T, LInt, NMax, TEnv, Env)
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% label_algebraic(Var, TEnv).
%
% Take an algebraic variable (it could be an integer or a term) and instanciate
% it.

label_algebraic(X, Max, TEnv, Env) :-
  is_int(X) ->
    my_labeling([random], [X])
  ;
  ( label_algebraic_aux(X, LInt, Max, TEnv, Env),
    fin_env(Env),
    my_labeling([random], LInt),
    %fin_env(Env),
    true
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% label_algebraics(LVar, TEnv).
%
% Label the list of variables. It instanciated firstly the albegraic variables
% and secondly the finite domain variables.
%

label_algebraics(L, Env) :-
  type_envi(TEnv),
  label_algebraics_aux(L, LInt, 8, TEnv, Env),
  my_labeling([random], LInt),
  fin_env(Env),
  fin_env(Env),
  fin_env(Env),
  fin_env(Env),
  fin_env(Env),
  fin_env(Env),
  fin_env(Env),
  fin_env(Env),
  fin_env(Env),
  get_nb_constraint(Env, Nb),
  write('*'),
  (Nb == 0 -> true; format('Warning: we have labelled all variables but it remains ~w unsolved constraint(s)~n', Nb)),
  flush_output.

% Auxiliary function for label_algebraics :
label_algebraics_aux([X | L], LInt, Max, TEnv, Env) :-
(
  is_int(X) ->
  ( label_algebraics_aux(L, LInt2, Max, TEnv, Env), % postpones the
    LInt = [X|LInt2]                                % labeling of integers
  )
  ;
  ( label_algebraic_aux(X, LInt2, Max, TEnv, Env),
    %fin_env(Env), 
    label_algebraics_aux(L, LInt3, Max, TEnv, Env),
    append(LInt2, LInt3, LInt)
  )
),
  my_labeling([random], LInt).

label_algebraics_aux([], [], _Max, _TEnv, _Env).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% label_algebraics_debug(LVar, FILE, TEnv).
%
% Same as label_algebraics and print in FILE debuging statement.
% (value of the variable before labeling and after labeling).
%

label_algebraics_debug(L, FILE, Env) :-
  type_envi(TEnv),
  label_algebraics_print(L, 4, FILE, TEnv, Env).

label_algebraics_print([X | L], Max, FILE, TEnv, Env) :-
  write(FILE, '\n'),
  write(FILE, var(X)),
  label_algebraic(X, Max, TEnv, Env),
  write(FILE, bind(X)),
  flush_output(FILE),
  label_algebraics_print(L, Max, FILE, TEnv, Env).

label_algebraics_print([], _Max, FILE, _TEnv, _Env) :-
  write(FILE, '\nEnd'),
  flush_output(FILE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% write_statistics(+Filename).
%
% Write the statistics sicstus internal values in file Filename.
% Filename should be either some(X) or none (X is the reel value of the file.
% If Filname is 'none' no statistic are output.

write_statistics(none) :- !.

write_statistics(some(File)) :-
  open(File, append, Stat),
  findall((X,Y), statistics(X,Y),L),
  List =.. [Opt,Prop, L],
  write(Stat, List),
  write(Stat, '.\n'),
  findall((X,Y), fd_statistics(X,Y),L2),
  List2 =.. [Opt, Prop, L2],
  write(Stat, List2),
  write(Stat, '.\n'),
  flush_output(Stat),
  close(Stat).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% label_and_write(Vs, N, File, Env, Halt, Stat).
%
% Labels the variables Vs and compute N solution of the system. Write the
% results in the file File with one line by solution. a line in the file is of
% the form :
% var(X_1, ..., X_n)
%
% Stat should be either none or some('filename').
% If Halt is 1 then quit sicstus after the labeling.

label_and_write(Vs, N, File, Env, _Prop, Halt, Stat) :-
  %statistics,
  %fd_statistics,
  write('Beginning labeling...\n'),
  open(File, write, Stream),
  findallnprint(Vs, label_algebraics(Vs, Env), Stream, N),
  flush_output(Stream),
  close(Stream),
  write('Labeling successfully finished :D\n'),
  %write_statistics(Stat),
  (Halt == 1 -> halt; true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% print_solution(+Stream, +L).
%
% When L = [e_1, ..., e_n], print the line [var(e_1, ..., e_n)] to the stream
% Stream.

print_solution(Stream, E) :-
  To_print =.. [vars|E],
  write(Stream, To_print),
  write(Stream, '\n'),
  flush_output(Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% print_solutions(+Stream, +L).
%
% Same a print_solution for a list of lists.

print_solutions(_Stream, []).

print_solutions(Stream, [E|R]) :-
  To_print =.. [vars|E],
  write(Stream, To_print),
  write(Stream, '\n'),
  print_solutions(Stream, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% findallnprint(L, Goal, Stream, N).
%
% Find N solutions of Goal and print the list of variable L in the stream
% Stream.

findallnprintnb(Keep, Asked) :-
  Keep == Asked ->
    print('Warning : no test cases found\n')
  ;
  ( Nb is Asked - Keep,
    print( Nb ),
    print(' test cases found\n')
  ).

findallnprint(X, Goal, Stream, N) :-
  N == 0 -> true
;
  bb_put(remains,[N, N]),
  (findall_1_print(X, Goal, Stream);
   (bb_get(remains, [NN, N]),
    findallnprintnb(NN, N),
    bb_delete(remains, _)
   )
  ).

findall_1_print(X, Goal, Stream) :-
  call(Goal),
  print_solution(Stream, X),
  bb_get(remains, [N, Total]),
  NmO is N - 1,
  ( NmO == 0 ->
    ( bb_delete(remains, [N, Total]),
      findallnprintnb(NmO, Total),
      !
    )
  ;
    bb_update(remains, [N, Total], [NmO, Total]),
    fail
  ).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% findalln(L1, Goal, L2, N).
%
% Find N solutions of Goal and print the list of variable L1.
% Returns the solutions in L2.

findalln(X,Goal,L,N) :-
  N == 0 -> L = [];
  bb_put(all,[]),
  bb_put(remains,N),
  ( findall_1(X, Goal, L); bb_delete(remains, _), bb_delete(all, L)).

findall_1(X, Goal, L) :-
  call(Goal), update_list(X),
  bb_get(remains,N),
  NN is N - 1,
  ( NN == 0 ->
    bb_delete(remains,_), !, bb_delete(all,L)
  ;
    bb_update(remains, N, NN),
    fail
  ).
  
% ajoute une solution a l'ensemble trouvé :
update_list(New) :-
    bb_get(all, Old),
    bb_update(all, Old, [New | Old]).

%% The next predicates are used for calculating the MC/DC criteria :

put_one([]) :-
   print(')').
put_one([E|R]) :-
   print(', one'),
   E #= 1, put_one(R).

put_a_zero([], Vs, Goal, Stream, N) :- findallnprint(Vs, Goal, Stream, N).
put_a_zero([E|R], Vs, Goal, Stream, N) :-
  ( E #= 0,
    put_one(R),
    findallnprint(Vs, Goal, Stream, N),
    fail
  )
;
  (E #= 1,
   put_a_zero(R, Vs, Goal, Stream, N)
  ).

mcdc(Truth, Vs, Goal, Stream, N) :-
  put_a_zero(Truth, Vs, Goal, Stream, N).
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% label_and_write_mcdc(Vs, N, Truth, File, Env, Stat, Halt, Stat).
%
% Labels the variables Vs and compute N time MC/DC on the precondition (Truth
% contains the variables corresponding to the truth value of each conjunct). Write the
% results in the file File with one line by solution. a line in the file is of
% the form :
% var(X_1, ..., X_n)
%
% Stat should be either none or some('filename').
% If Halt is 1 then quit sicstus after the N coverage of MC/DC.

label_and_write_mcdc(Vs, N, Truth, File, Env, _Prop, Halt, Stat) :-
  statistics,
  open(File, write, Stream),
  mcdc(Truth, Vs, label_algebraics(Vs, Env), Stream, N),
  flush_output(Stream),
  close(Stream),
  write_statistics(Stat),
  (Halt == 1 -> halt; true).


set_debug :-
  add_breakpoint([pred(label_term/4)], _).
