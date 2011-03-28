% prolog program

?- load_files(ctr_match_meta).
?- load_files(ctr_match_naif).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% match(X, VarDef, LC1, LC2, ENV).
%
% The match constraint. It calls the meta-constraint or the naive implementation
% with respect to the variable ENV.
%

match(VarDef, Matched_Var, PatL, Env) :-
  use_meta(Env) ->
    match_meta(VarDef, Matched_Var, PatL, Env)
  ;
    match_naive(VarDef, Matched_Var, PatL, Env).

