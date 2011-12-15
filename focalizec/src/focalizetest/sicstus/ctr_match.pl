% prolog program

?- load_files(ctr_match_meta).
?- load_files(ctr_match_naif).

:- multifile portray/1.

portray(match(_VarDef, MVar, Patl, Env)) :-
  current_prolog_flag(debugger_print_options, Flags),
  (get_exploring(Env) ->
    write_term(try_match(MVar, Patl), Flags)
  ;
    write_term(match(MVar, Patl), Flags)
  ).
  %format('NEW_MATCH ~@ WITH~n~@', [write_term(MVar, Flags), write_pattern(Patl)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% match(X, VarDef, LC1, LC2, ENV).
%
% The match constraint. It calls the meta-constraint or the naive implementation
% with respect to the variable ENV.
%

match(VarDef, Matched_Var, PatL, Env) :-
  write_debug(match(VarDef, Matched_Var, PatL, Env)),
  get_nb_constraint(Env, Nb),
  format_debug('Nb actif : ~w', [Nb]),
  use_meta(Env) ->
    match_meta(VarDef, Matched_Var, PatL, Env)
  ;
    match_naive(VarDef, Matched_Var, PatL, Env).

