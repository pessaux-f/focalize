

function_add_atom(FName, R, LArgs, Body, FV, Env) :-
  get_apply_method(Env, assoc) ->
    function_add_atom_assoc(FName, R, LArgs, Body, FV, Env)
  ;
    function_add_atom_assert(FName, R, LArgs, Body, FV, Env).

function_call(FName, R, Args, FV, Env) :-
  get_apply_method(Env, assoc) ->
  function_call_assoc(FName, R, Args, FV, Env)
  ;
  function_call_assert(FName, R, Args, FV, Env).



% This function is not dependent of the implementation of apply :
function_add(FName, R, LArgs, Body, FV, Env) :-
  /* Define the body of the clause */
  new_atom(FName),
  function_add_atom(FName, R, LArgs, Body, FV, Env).

