% prolog program

?- load_files(ctr_ite_meta).
?- load_files(ctr_ite_naif).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ite(X, VarDef, LC1, LC2, ENV).
%
% The ite constraint. It calls the meta-constraint or the naive implementation
% with respect to the variable ENV.
%

ite(X,VarDef,LC1,LC2, ENV) :-
  use_meta(ENV) ->
    ite_meta(X, VarDef, LC1, LC2, ENV)
  ;
    ite_naive(X, VarDef, LC1, LC2, ENV).

