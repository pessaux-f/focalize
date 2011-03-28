

debug_on :-
  %add_breakpoint(goal(ran_rec(_,_)), _).
  add_breakpoint(goal(label_algebraics_aux(_, _, _, _, _)), _).
  %add_breakpoint(goal(print_solution(_, _)), _).
