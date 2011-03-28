% prolog program

% This file contains the predicate handling high-order features.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fun_eq(F, Lambdas, Def).
%
% fun_eq set the definition of variable F to Def, the arguments are Lambdas.

fun_eq(F, R, Lambdas, Def, FV, Env) :-
  % Create a new clause definition with body Def and args Lambdas :
  function_add(FName, R, Lambdas, Def, FV, Env),
  % Declare F as the function Fname of arity length(Lambdas) :
  length(Lambdas, NbExpect),
  closure_defs(F, FName, NbExpect, [], FV).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% apply(F, L).
%
% The apply constraint. freeze the call F(L) until F is defined.
%

%%%%%%%%%%%%%%%%%%
% supply_new_args(+Nb, -NNb, +Args, -LArgs, -RArgs). (not exported)
%
% Take [Nb] element in front of [Args] and put them in [LArgs]. [RArgs] is
% unified with the remainder of [LArgs] and [NNb] is the number of elements that
% would be taken but [Args] is not long enough.
%
% Example:
% supply_new_args(3, X, [A, B, C, D, E], L1, L2).
% -> X = 0, L1 = [A, B, C], L2 = [D, E].
%
% supply_new_args(4, X, [A, B, C], L1, L2).
% -> X = 1, L1 = [A, B, C], L2 = [].
%
% supply_new_args(5, X, [], L1, L2).
% -> X = 5, L1 = [], L2 = [].

supply_new_args(X, 0, L, [], L) :- X == 0, !.
supply_new_args(Nb, Nb, [], [], []) :- !.
supply_new_args(Nb, NNb, [X|NL], [X|L], Over) :-
  NbMO is Nb - 1,
  supply_new_args(NbMO, NNb, NL, L, Over).


apply(R, F, L, Env) :-
  % The function is awaked when F is known
  freeze(F,
  (
  
      closure_defs(F, FN, NbR, Args, FV),
      supply_new_args(NbR, NNbR, L, BegL, EndL),
      append(Args, BegL, NArgs),
      (NNbR == 0 ->
       (
        function_call(FN, RFN, NArgs, FV, Env),
        (EndL == [] ->
         R = RFN
         ; 
         apply(R, RFN, EndL, Env)
        )
       )
       ; 
       closure_defs(R, FN, NNbR, NArgs, FV)
      )

  ) 

  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% closure_defs(+F, -Name, -Expected, -Args, -FV).
% closure_defs(-F, +Name, +Expected, +Args, +FV).
%
% Obtain from the function variable F, the name of the predicate, the number of
% expected argument (0 means, all arguments are known), the list of currently
% applied argument and the list of free variables.

closure_defs(F, Name, Expected, Args, FV) :-
  F = (Name, Expected, Args, FV).

get_partial_list([], _).
get_partial_list([X|R], [X|Pl]) :-
  get_partial_list(R, Pl).

