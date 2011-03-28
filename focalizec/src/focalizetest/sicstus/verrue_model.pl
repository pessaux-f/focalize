% prolog program

:- multifile clpfd:dispatch_global/4.

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% one_of/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
one_of(C1,C2, ENV) :-
         get_awake(ENV,Reveil),
         clpfd:fd_global(one_of_ctr(C1, C2, ENV),
                         etat,
                         [max(Reveil)]).

clpfd:dispatch_global(one_of_ctr(C1, C2, ENV),
                      etat(State), etat(State), Actions) :-
     
  
*/

%in_l(E, cons(F, L)) :-
%  (E = F; in_l(E, L)).

not_in_l(_, nil).
not_in_l(E, cons(F, L)) :-
  (E \= F, not_in_l(E, L)).

mac_aux(_Sigma, [],_S, _Env).
mac_aux(Sigma, [O|OL],S, Env) :-
   ( not_in_l(pair(S,pair(O,read)), Sigma);
     (plg_f_o345(R1, Sigma, O, Env),
      plg_f_s501(R2, Sigma, S, Env),
      plg_order_inf311(1,R1, R2, Env)
     )
   ),
   mac_aux(Sigma, OL,S, Env).

mac_aux2(_Sigma, _L1, [], _Env).
mac_aux2(Sigma, L1, [S|LS], Env) :-
  mac_aux(Sigma, L1, S, Env),
  mac_aux2(Sigma, L1, LS, Env).




/* Pour tout sujet s et tout abjet o
   s lit o  dans sigma implique f_o(o) < f_s(s) */
mac_prolog(Sigma, Env) :-
  mac_aux2(Sigma, [o1, o2, o3], [s1, s2], Env).
  
   
   
mac_star_aux(_Sigma, [], _S, _Env).
mac_star_aux(Sigma, [O2|OL],S, O1, Env) :-
   ( not_in_l(pair(S,pair(O1,read)), Sigma);
     not_in_l(pair(S,pair(O2,write)), Sigma);
     (plg_f_o345(R1, Sigma, O1, Env),
      plg_f_o345(R2, Sigma, O2, Env),
      plg_order_inf311(1,R1, R2, Env)
     )
   ),
   mac_aux(Sigma, OL,S, Env).

mac_star_aux2(_Sigma, _L1, [], _O1, _Env).
mac_star_aux2(Sigma, L1, [S|LS], O1, Env) :-
  mac_star_aux(Sigma, L1, S, O1, Env),
  mac_star_aux2(Sigma, L1, LS, O1, Env).

mac_star_aux3(_Sigma, _L1, _LS, [], _Env).
mac_star_aux3(Sigma, L1, LS, [O1|LO1], Env) :-
  mac_star_aux2(Sigma, L1, LS, O1, Env),
  mac_star_aux3(Sigma, L1, LS, LO1, Env).



/* Pour tout sujet s et tout abjet o
   s lit o  dans sigma implique f_o(o) < f_s(s) */
mac_star_prolog(Sigma, Env) :-
  mac_star_aux3(Sigma, [o1, o2, o3], [s1, s2], [o1, o2, o3], Env).
  
