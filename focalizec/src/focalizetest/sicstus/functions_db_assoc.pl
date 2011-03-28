
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% function_add(-FName, +R, +LArgs, +Body, +FV, +Env).
%
% put a new function definition in the environment.
%

function_add_atom_assoc(FName, R, LArgs, Body, FV, Env) :-
  get_functions(Env, Funs),
  /* Define the body of the clause */
  Args = (R, LArgs, FV, Env),
  copy_term((Args, Body), Value), % We ensure the variables in Value are
                                  % disjoint from the original.
  /* ***************************** */
  put_assoc(FName, Funs, Value, NFuns),
  set_functions(Env, NFuns).

function_call_assoc(FName, R, Args, FV, Env) :-
  get_functions(Env, Funs),
  (get_assoc(FName, Funs, Value); 
   Msg =.. ['function_unknown', FName],
   raise_exception(Msg)
  ),
  copy_term(Value, (EArgs, Body)),
  EArgs = (R, Args, FV, Env),
  user:call_rec(Body).
  
