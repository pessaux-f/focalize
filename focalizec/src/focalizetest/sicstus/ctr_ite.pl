% prolog program

?- load_files(ctr_ite_meta).
?- load_files(ctr_ite_naif).

:- multifile portray/1.

portray(ite(X, _VarDef, LC1, LC2, _Env)) :-
  current_prolog_flag(debugger_print_options, Flags),
  (get_exploring(Env) ->
    write_term(try_ite(X, LC1, LC2), Flags)
  ;
    write_term(ite(X, LC1, LC2), Flags)
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ite(X, VarDef, LC1, LC2, ENV).
%
% The ite constraint. It calls the meta-constraint or the naive implementation
% with respect to the variable ENV.
%

ite(X,VarDef,LC1,LC2, ENV) :-
  %write_debug(ite(X, LC1, LC2)),
  use_meta(ENV) ->
    ite_meta(X, VarDef, LC1, LC2, ENV)
  ;
    ite_naive(X, VarDef, LC1, LC2, ENV).

