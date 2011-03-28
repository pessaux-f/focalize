
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% function_add_assert(-FName, +R, +LArgs, +Def, +FV, Env).
%
% put a new definition and returns the name of the new introduced clause.

function_add_atom_assert(FName, R, LArgs, Body, FV, Env) :-
  Head =.. [FName, R, LArgs, FV, Env],
  NewDef =.. [:-, Head, user:call_rec(Body)],
  assert(NewDef).

function_call_assert(FName, R, LArgs, FV, Env) :-
  Call =.. [FName, R, LArgs, FV, Env],
  call(Call).
