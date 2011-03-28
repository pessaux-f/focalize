% prolog program

% Dans cette version, on utilise l'état de la contrainte pour simuler la
% suppression d'un pattern.
%
% L'état de la contrainte est alors la liste des patterns.

:- multifile clpfd:dispatch_global/4.

% un état et un couple de :
% pattern restant * pattern supprimé

match_meta(VarDef, Matched_Var, PatL, Env) :-
  get_awake(Env, Reveil),
  add_a_constraint(Env),
  % Set the types of all variables :
  type_envi(TEnv),
  set_types_vardef(VarDef,TEnv),
  get_FD_vardef_list(VarDef, LVar),
  get_var_vardef_list(VarDef, Vars),
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  add_dom(LVar,DOM),
  ACTIVE in 0..1,
  Awake in 0..1,
  when(nonvar(Matched_Var), Awake = 1),
  clpfd:fd_global(match_ctr(Matched_Var, VarDef, Vars, Env),
                  etat(PatL, [], ACTIVE),          % constraint state
                  [max(Reveil), dom(Awake) |DOM],  % Awake conditions
                  [idempotent(true)]). % Don't need to wake the constraint many times

% a state of the constraint is a 3-tuple of :
% list of pattern * list of pattern * fd var
% 
% first list of pattern is the activated pattern
% second list of pattern is the list of pattern proved unsat
% the fd var specify is the constraint is active or not (if it returns
% straigthly or not) .
%

% On ajoute un point de choix à dispatch_global de la bibliothèque clpfd :
clpfd:dispatch_global(match_ctr(MVar, VarDef, Vars, Env),
                      etat(Cur, Deleted, Active),
                      PatSor, Actions) :- % The output values are 
                                          % the two last arguments
%  new_atom_list([_], [Ret]),
%  get_dbg_status(T, Env),
%  (T == 1 ->
%   ((
%     unset_dbg(Env),
%     intermediaire(MVar, VarDef, Vars, Env, etat(Cur, Deleted, Active), PatSor, Actions),
%  %   PatSor = etat(Cur, Deleted, Active), Actions = [],
%     set_atom(Ret, [PatSor, Actions]), 
%     fail
%    );
%    get_and_free_atoms([Ret], [[PatSor, Actions]]),
%    set_dbg(Env)
%   );
    intermediaire(MVar, VarDef, Vars, Env, etat(Cur, Deleted, Active), PatSor, Actions)
%  )
  .

intermediaire(_MVar, _VarDef, _Vars, Env, etat(Cur, Deleted, Active), PatSor, Actions) :-
  (nonvar(Active); env_not_awakable(Env)), % If the constaint should not be awaked
  !,
  Actions = [],
  PatSor = etat(Cur,Deleted, Active).

intermediaire(MVar, _VarDef, Vars, Env, etat(Cur, Deleted, Active), PatSor, Actions) :-
  !,
  %% Pour l'union des domaines reduit %%%%%%%%%%%%%
  new_atom_list(Cur,FreshAtom),                   %
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  match_idempotent(MVar, Cur, FreshAtom, Vars, NDeleted, NCur, _AtomActif, NActions, Active, Env),
  append(Deleted,NDeleted,NLDeleted),
  PatSor = etat(NCur, NLDeleted, Active),
  ( NCur == [] ->
    %% Pour l'union des domaines reduit %%%%%%%%%%%%%
    %free_atoms(AtomActif),                      %  AtomActif ?????
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    Actions = [fail] % Il n'y a plus de filtre candidat à la résolution du filtrage
  ;
    ( 
     % %% Pour l'union des domaines reduit %%%%%%%%%%%%%
     % get_and_free_atoms(AtomActif, TmpL),            %
     % transposed(TmpL,NVars),                         % 
     % antiunify_vardef_list(VarDef, NVars,Res),       %
     % BonusAction = call(user:equal_vardef_list(VarDef, Res)),
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      % Sinon on met ça :
       BonusAction = call(true),
      %%%% free_atoms(FreshAtom),
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      ( NActions == [] ->
        ( NCur = [pattern(Pc,Ec)] -> % S'il ne reste qu'un seul pattern :
          remove_a_constraint(Env),
          Actions = [exit,
                     BonusAction,
                     call(user:unifyD_pattern(MVar, Pc)),
                     call(user:call_rec(Ec))
                     ]
        ;
          Actions = [BonusAction|NActions]
        )
      ;
        Actions = [BonusAction|NActions]
      )
     )
  ).



% match_idempotent(+Var, +To_process, -To_del, -To_keep, -Actions, +Active, +Env).
%
% verify a list of pattern.
%
% Active : if this variable is instanciated. The constraint is inactive (when
% called, it returns straightly).
%
% Actions : the list of actions to execute after the constraint.
%
% To_process, To_keep, To_del : three list of pattern, its meaning is the name
% of the variable.

match_idempotent(MVar, [E|A_traiter], [Atom|Atoms], Vars,
                 A_supprimer, A_garder, Atom_actif, Act, Active, Env) :-
  match_solve_one(MVar, E, Atom, Vars, Actions, Result, Active, Env),
  (Result == overflow ->
     (!, A_supprimer = [], A_garder = [E|A_traiter],
         Atom_actif = [], Act = Actions);
  (Result == entailed -> % The pattern is entailed
     (!, A_supprimer = A_traiter, A_garder = [E], 
         Atom_actif = [], Act = Actions);
  (Result == nothing -> 
     (!,
      match_idempotent(MVar, A_traiter, Atoms, Vars, A_supprimer, A_garder2,
                       Atom_actif2, Act, Active, Env),
      A_garder = [E | A_garder2],
      Atom_actif = [Atom| Atom_actif2]
     );
  (Result == backward -> % The pattern should be removed
     (!,
      match_idempotent(MVar, A_traiter, Atoms, Vars, A_supprimer2, A_garder,
                       Atom_actif, Act, Active, Env),
      A_supprimer = [E | A_supprimer2]
     )
  )
  ))).

match_idempotent(_MVar, [], _, _, [], [], [] ,[], _Active, _Env).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% match_solve_one(+X, +PatL, +VarL, -Actions, +Env).
%
% Prend un pattern et vérifie si le pattern est impliqué par le store.
% Vérifie aussi si le pattern n'est pas impliqué (sémantique arrière du
% pattern).
%
% Retourne le cas échéant les actions à faire (suppression du pattern dans le
% match, arrêt de la contrainte). 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

match_solve_one(MVar,
                pattern(Pc,Ec),
                _Atom,
                _Vars,
                Actions,
                Result,
                Active,
                Env) :-
  (\+(user:call_rec([call(Active = 0),
                     user:decr_k(Env),
                     user:remove_constructor(MVar, Pc)
                    ])),
    !, 
    Result = entailed,
    Actions = [entailed,
               call(user:unifyD_pattern(MVar, Pc)),
               call(user:call_rec(Ec))]
  ).

match_solve_one(MVar,
                pattern(Pc,Ec),
                _Atom,
                _Vars,
                Actions,
                Result,
                Active,
                Env) :-
  %%%%%%%%%%%% Teste la non-implication du pattern  %%%%%%%%%%%%%%%
   (use_back_rule(Env),
    \+((user:call_rec([call(Active = 0),
                       user:decr_k(Env),
                       call(user:unifyD_pattern(MVar, Pc)),
                       user:call_rec(Ec)])
        %% Pour l'union des domaines reduit %%%%%%%%%%%%%
        %,set_atom(Atom, Vars)                           %
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      )),
    !,
    Result = backward,
    Actions = [] % On ne regardera plus le pattern à l'avenir
   ).

%%%%%%%%% On a rien déduit des patterns du match %%%%%%%%%%%%%%%
match_solve_one(_MVar, _Pat, _Atom, _Vars, Actions, Result, _Active, _Env) :-
  call_rec([true, true, true]),
  Result = nothing, Actions = []. % Yes indique qu'on a fait un assert sur _Atom

