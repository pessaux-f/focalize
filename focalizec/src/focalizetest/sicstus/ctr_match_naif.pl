% prolog program

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ite_naive(+X, _, +T, +E, _).
%
% ite with a naive implementation.
%

list_rev(Res, [], Res).

list_rev(Res, [E|R], Y) :-
  list_rev(Res, R , [E|Y]).


match_naive(_VarDef, MVar, [pattern(Pc,Ec)|Pats], _Env) :-
  list_rev(R, [pattern(Pc,Ec)|Pats], []),
  %R = [pattern(Pc, Ec)| Pats],
  match_naive2(_VarDef, MVar, R, _Env).

match_naive2(_VarDef, MVar, [pattern(Pc,Ec)|Pats], _Env) :-
  ( call(user:unifyD_pattern(MVar, Pc)),
    call(user:call_rec(Ec))
  )
;
  match_naive2(_VarDef, MVar, Pats, _Env).
