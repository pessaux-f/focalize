
%write_debug(_).

format_debug(_Format, _L) :-
  %format(_Format, _L), nl,
  true.

write_debug(_T) :-
  %write_term(_T, [portrayed(true)]), nl,
  true.
  /*
  write_term(T, [cycles(true)]),nl.
  write_term(T, [cycles(true)]),nl.
  */

debug_on :-
  %add_breakpoint(goal(ran_rec(_,_)), _).
  %add_breakpoint(goal(label_algebraics_aux(_, _, _, _, _)), _).
  %add_breakpoint(goal(unifyD(_, _)), _).
  add_breakpoint(goal(match(_,_,_,_)), _),
  %add_breakpoint(goal(print_solution(_, _)), _).
  true.
