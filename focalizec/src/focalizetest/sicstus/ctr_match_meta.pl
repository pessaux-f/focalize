% prolog program

% Dans cette version, on utilise l'état de la contrainte pour simuler la
% suppression d'un pattern.
%
% L'état de la contrainte est alors la liste des patterns.

:- multifile clpfd:dispatch_global/4.
:- multifile portray/1.

% Portray for "pretty" printing :

write_pattern_expr([]).
write_pattern_expr([E|R]) :-
  current_prolog_flag(debugger_print_options, Flags),
  write('  '), write_term(E, Flags), nl,
  write_pattern_expr(R).

write_pattern([]).

write_pattern([pattern(P,E)|R]) :-
  % current_prolog_flag(debugger_print_options, Flags),
  write('| '), write(P), write(' -> '),
  write_pattern_expr(E),
  write_pattern(R).

portray(intermediaire(Matched_Var, _VarDef, _Vars, _Env, etat(Cur, _Deleted, _Active), _PatSor, Actions)) :-
  current_prolog_flag(debugger_print_options, Flags),
  nl,
  write('MATCH '),
  write_term(Matched_Var, Flags),
  write(' WITH '), nl,
  write_pattern(Cur),
  (Actions == [exit, call(user:unifyD_pattern(_MVar, Pc)) | _] ->
    write('Actions = imply pattern :'), write_term(Pc, Flags)
  ;
  (Actions == [fail] ->
    write('Actions =  constraint failed')
  ;
    true
  )).

% un état et un couple de :
% pattern restant * pattern supprimé

match_meta(VarDef, Matched_Var, PatL, Env) :-
  get_awake(Env, Reveil),
  add_a_constraint(Env), % for counting meta constraints
  %%%% Set the types of all variables %%%%%
  type_envi(TEnv),
  set_types_vardef(VarDef, TEnv),
  get_FD_vardef_list(VarDef, LVar),
  get_var_vardef_list(VarDef, Vars), % TODO: liste de when pour Vars
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  add_dom(LVar, DOM),
  ACTIVE in 0..1,
  Awake in 0..1, %  val(Awake) in fd_global ???
  when(nonvar(Matched_Var), Awake = 1),
  clpfd:fd_global(match_ctr(Matched_Var, VarDef, Vars, Env),
                  etat(PatL, [], ACTIVE),          % constraint state
                  [max(Reveil), dom(Awake) |DOM],  % Awake conditions
                  [idempotent(true)]).             % Don't need to wake the constraint many times

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

/*
intermediaire(_MVar, _VarDef, _Vars, Env, etat(Cur, Deleted, Active), PatSor, Actions) :-
  (nonvar(Active); env_not_awakable(Env)), % If the constraint should not be awaked
  !,
  Actions = [],
  PatSor = etat(Cur,Deleted, Active).
*/

intermediaire(MVar, _VarDef, Vars, Env, etat(ActPat, DelPat1, Active), etat(KeptPat, DelPat, Active), Actions) :-
  !,
  %% Pour l'union des domaines reduit %%%%%%%%%%%%%
  new_atom_list(ActPat,FreshAtom),                %
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %write_term(ActPat,[cycles(true)]), nl,
  match_activate_all(ActPat, MVar, FreshAtom, Vars, DelPat2, KeptPat, _AtomActif, Active, OneEntailed, Env),
  append(DelPat1, DelPat2, DelPat),
  match_conclude_actions(KeptPat, OneEntailed, MVar, Actions, Env).


% match_conclude_actions(+KeptPat, +MVar, -Actions, +Env).
%
% Take the list of KeptPat of active pattern and the matched variable MVar.
% Returns the list of actions to apply after the resolution of the constraint.

match_conclude_actions([], _, _MVar, [fail], Env) :-
  !,
  remove_a_constraint(Env).

match_conclude_actions([pattern(Pc, Ec)], _, MVar, [exit, call(user:unifyD_pattern(MVar, Pc)), call(user:call_rec(Ec))], Env) :-
  !,
  remove_a_constraint(Env).

match_conclude_actions(_, Pat, MVar, [exit, call(user:unifyD_pattern(MVar, Pc)), call(user:call_rec(Ec))], Env) :-
  nonvar(Pat),
  Pat = [pattern(Pc, Ec)],
  !,
  remove_a_constraint(Env).

match_conclude_actions(_, _, _MVar, [], _Env). % TODO : do something

/*
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
 */

% match_activate_all(+To_be_process, +MVar, +Atoms, +Vars, -To_del, -To_keep, _Atom_actif, +Active, +Env).
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

% When the current pattern is entailed, KeptPat is a singleton. So the caller
% can conclude.

match_activate_all([CurPat|OthPat], MVar, [Atom|Atoms], Vars, DelPat, KeptPat, _Atom_actif, Active, OneEntailed, Env) :-
  match_activate_one(MVar, CurPat, Atom, Vars, Verdict, Active, Env),
  match_conclude_on_verdict(Verdict, CurPat, OthPat, DelPat1, KeptPat1, CallOth, OneEntailed),
  match_activate_all(CallOth, MVar, Atoms, Vars, DelPat2, KeptPat2, _Atom_actif, Active, OneEntailed, Env),
    append(DelPat1, DelPat2, DelPat),
    append(KeptPat1, KeptPat2, KeptPat).

match_activate_all([], _MVar, _Atoms, _Vars, [], [], [],  _Active, _, _Env).

% match_conclude_on_verdict(+Verdict, +CurPat, +OthPat, -DelPat, -KeptPat, -CallOth).
%
% Take the pattern CurPat we just resolved with its Verdict, the list of all
% other patterns OthPat to be resolved.
% According to Verdict, returns :
%
% - the list DelPat of patterns that should not be resolved on a future
% activation of the match constraint;
% - the list of pattern KeptPat, that sould be activated on a future activation of the
% match constraint.
% - the list CallOth of pattern we should now activate. It is instantiatate
% either with [] or with OthPat.

% The results shows we activate match/ite too many times. All patterns are kept.
match_conclude_on_verdict(overflow, CurPat, OthPat, DelPat, KeptPat, CallOth, _OneEntailed) :-
  !,
  KeptPat = [CurPat|OthPat],
  DelPat  = [],
  CallOth = [].

% The results shows CurPat is entailed. Returns it.
match_conclude_on_verdict(entailed, CurPat, OthPat, DelPat, KeptPat, CallOth, OneEntailed) :-
  !,
  KeptPat = [CurPat],
  DelPat = OthPat,
  CallOth = [],
  OneEntailed = [CurPat].

% The results is not conclusive on CurPat.
% We should resolve others patterns.
match_conclude_on_verdict(nothing, CurPat, OthPat, DelPat, KeptPat, CallOth, _OneEntailed) :-
  !,
  KeptPat = [CurPat],
  DelPat = [],
  CallOth = OthPat.

% The backward rule concludes CurPat is inconsistent with constraint store.
% We should resolve others patterns and remove CurPat.
match_conclude_on_verdict(backward, CurPat, OthPat, DelPat, KeptPat, CallOth, _OneEntailed) :-
  !,
  KeptPat = [],
  DelPat = [CurPat],
  CallOth = OthPat.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% match_activate_one(+X, +Pat, +Atom, +VarL, -Verdict, +Active, +Env).
%
% Take a pattern and activate both semantic rules on it.
%
% Returns the verdict \in [overflow, entailed, backward, nothing].
%
% overflow = we activate too many ite/match constraint
% entailed = the pattern is entailed
% backward = the pattern is not possible
% nothing = no conclusion
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

match_activate_one(_MVar, _Pattern, _Atom, _Vars, Verdict, _Active, Env) :-
  \+(decr_k(Env)),  % We already activated the too many constraint and our quota is reached /!\
  !, 
  %write_debug('overflow'),
  Verdict = overflow.

% The forward rule resolution :
match_activate_one(MVar, pattern(Pc, _Ec), _Atom, _Vars, Verdict, Active, Env) :-
  \+(user:call_rec([call(Active = 0),
                    user:decr_k(Env),
                    user:remove_constructor(MVar, Pc)
                   ])),
  !, 
  %write_debug('entailed'),
  Verdict = entailed.

% This clause decides if we want to execute backward rule
match_activate_one(_MVar, _Pattern, _Atom, _Vars, Verdict, _Active, Env) :-
  \+((use_back_rule(Env))), % We don't want backward rules
  !,
  %write_debug('nothing (nobackward)'),
  Verdict = nothing.

% The backward rule resolution :
match_activate_one(MVar, pattern(Pc,Ec), _Atom, _Vars, Verdict, Active, Env) :-
  %write_debug('Activation of backward rule'),
  %%%%%%%%%%%% Teste la non-implication du pattern  %%%%%%%%%%%%%%%
  \+((user:call_rec([call(Active = 0),
                     user:decr_k(Env),
                     call(user:unifyD_pattern(MVar, Pc)),
                     user:call_rec(Ec)]))),
  %% Pour l'union des domaines reduit %%%%%%%%%%%%%
  %,set_atom(Atom, Vars)                           %
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  !,
  %write_debug('backward'),
  Verdict = backward.

% The semantics rules are not conclusive :
match_activate_one(_MVar, _Pat, _Atom, _Vars, Verdict, _Active, _Env) :-
  Verdict = nothing.

