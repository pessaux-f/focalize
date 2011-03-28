% prolog program


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ite_naive(+X, _, +T, +E, _).
%
% ite with a naive implementation.
%

ite_naive(X,_VarDef,LC1,LC2, _ENV) :-
  ( X #= 1, 
    call_rec(LC1)
  ) 
;
  ( (X #\= 1),
    call_rec(LC2)
  ).

