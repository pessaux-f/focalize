%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%NOM :   ite.pl
%
%FONCTION :
%        Implantation d'un opérateur déductif pour modéliser la conditionnelle
%        Labeling avec choix de valeur aléatoire
%    
%HISTORIQUE :
%       A. Gotlieb   31/03/08 Création
%
%REF :
%       Arnaud Gotlieb, Bernard Botella, Michel Rueher:
%       "A CLP Framework for Computing Structural Test Data"
%       Computational Logic 2000, Constraints stream, pp 399-413
%       London, UK, July 2000
%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile clpfd:dispatch_global/4.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ite/5
% ite(+X,+LVar,+LC1,+LC2,+Env)
%   - X  est une variable de reification de la decision
%   - LVar est la liste des variables sur lesquels il faut reveiller la contrainte
%       (variables de la condition + variable des phi-fonctions V0 et V2
%   - LC1 et LC2 sont les listes des contraintes des deux parties de la conditionnelle (THEN et ELSE)
%   - Env environnement (cette variable doit être initialisée par init_env)
%
%   exemples d'appels :
%   ?- init_env(_ENV), J0 in 0..6, X #<=> (I0 #= 0),ite(X,[J0,J2],[J2#=J0-1],[J2#=J0], _ENV), X = 1, fin_env(_ENV).
%          I0 = 0,
%          J0 in  0..6,
%          J2 in -1..5
%  
%   ?- init_env(_ENV), J0 in 0..6, X #<=> (I0 #= 0),ite(X,[J0,J2],[J2#=J0-1],[J2#=J0], _ENV), X = 0, fin_env(_ENV).
%          X  = 0,
%          J2 = J0,
%          I0 in (inf.. -1)\/(1..sup),
%          J0 in 0..6
%
%   ?- init_env(Env), X #<=> (I0 #=< 16), ite(X,[J0,J2],[J2 #=J0*I0],[ite(Y,
%   [],[],[], Env),J2#=J0], Env), J2#>8, J0 #= 2, fin_env(Env).
%          X = 1,
%          J0 = 2,
%          I0 in 5..16,
%          J2 in 10..32 ?
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ite_meta(X,VarDef,LC1,LC2, Env) :-
         get_awake(Env,Reveil),
         type_envi(TEnv),
         set_types_vardef(VarDef,TEnv), % Set the types of the variables
         get_FD_vardef_list(VarDef, LVar),
         get_var_vardef_list(VarDef, Vars),
         add_dom([X|LVar],DOM), % awaking condition
         State in 0..1, % bound means the constraint is disable 
         clpfd:fd_global(ite_ctr(X, LC1, LC2, Vars, VarDef, Env),
                         etat(State), % ite/5 devient une contrainte CLPFD
                         [max(Reveil)|DOM]).

clpfd:dispatch_global(ite_ctr(X, LC1, LC2, Vars, VarDef, Env),
                      etat(State), etat(State), Actions) :-
  ite_indirection(X,LC1,LC2, Vars, VarDef, Env, Actions, State).
  
ite_indirection(_X, _LC1, _LC2, _Vars, _VarDef, Env, Actions, State) :-
 (nonvar(State) ; env_not_awakable(Env)),
 !,
 Actions = [].

ite_indirection(X, LC1, LC2, Vars, VarDef, Env, Actions, State) :-
  %% Pour l'union des domaines reduit %%%%%%%%%%%%%
  new_atom_list([_,_],FreshAtom),                 %
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ite_solve(X,LC1,LC2, Vars, VarDef, Env, FreshAtom, Actions, State).

            %%%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%%%
         %%%%% %%%%% Le reste code les contraintes gardées %%%%% %%%%%
            %%%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%%%

ite_solve(X,LC1, _LC2, _Vars, _VarDef, _ENV, _FA, Actions, _State) :-
        (nonvar(X), (X > 0 ; X < 0)),                  % teste si la decision est connue (X != 0 ?)
        !,
        Actions = [exit,                               % alors la partie THEN doit être exécutée
                   call(X #= 1),
                   call(user:call_rec(LC1))].

ite_solve(X, _LC1, LC2, _Vars, _VarDef, _ENV, _FA, Actions, _State) :-      
        (X == 0),                                      % teste si la decision est connue (X == 0 ?)
        !,
        Actions = [exit,                               % alors la partie ELSE doit être exécutée
                   call(X #= 0),
                   call(user:call_rec(LC2))].


            /* Pour les deux autres contraintes gardées il faut */
                     /* utiliser la politique de réveil */

ite_solve(_X,_LC1,_LC2, _Vars, _VarDef, Env, _FA, Actions, _State) :-
  \+(use_back_rule(Env)),                           % pas de règle arrière
  call_rec([true, true, true]),
  !,
  Actions = [].                                     % alors la contrainte est suspendue



ite_solve(X, LC1, LC2, _Vars, _VarDef, Env, [_,_AtomE], Actions, State) :-
        \+((decr_k(Env),
            call_rec([State = 0, X #= 0 ,call_rec(LC2)])
            %% Pour l'union des domaines reduit %%%%%%%%%%%%%
            %,set_atom(_AtomE, _Vars)                         %
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           )),   % Test d'inconsistance de la partie ELSE
        !,
        Actions = [exit,                           % alors la partie THEN doit être executée
                   call(X #= 1),
                   call(user:call_rec(LC1))].

ite_solve(X, LC1, LC2, Vars, _VarDef, Env, [_AtomT,AtomE], Actions, State) :-
        \+((decr_k(Env), 
            call_rec([State = 0, X #= 1,call_rec(LC1)])
            %% Pour l'union des domaines reduit %%%%%%%%%%%%%
            %,set_atom(_AtomT, Vars)                           %
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          )),   % Test d'inconsistance de la partie THEN
        !,
        %% Pour l'union des domaines reduit %%%%%%%%%%%%%%%%%%%%%%%%%%%
        %free_atoms([AtomE]), % Libère les atomes alloués jusqu'alors  %
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        Actions = [exit,                          % alors la partie ELSE doit être exécutée
                   call(X #= 0),
                   call(user:call_rec(LC2))].

ite_solve(_X,_LC1,_LC2, _Vars, _VarDef, _ENV, _Atoms, Actions, _State) :- 
           % sinon, on suspend. Il est possible de faire ici une
        !, % union des domaines calculés dans les tests d'inconsistances
        %% Pour l'union des domaines reduit %%%%%%%%%%%%%
        %get_and_free_atoms(Atoms, RedDoms),             %
        %transposed(RedDoms, NVars),                     %
        %antiunify_vardef_list(VarDef, NVars,Res),       %
        %Actions = [call(user:equal_vardef_list(VarDef, Res))].
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % À faire quand on ne fait pas l'union des domaines :
        %free_atoms(Atoms),
         Actions = [].

