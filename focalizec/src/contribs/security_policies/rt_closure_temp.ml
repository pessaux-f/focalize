let rec f x l = match l with
  | [] -> ([], [])
  | h::t -> let (l1, l2) = f x t in
	      if x = fst h
	      then (h::l1, l2)
	      else (l1, h::l2);; 

let rec subst_first l x = match l with
  | [] -> []
  | (a, b)::t -> (x, b)::(subst_first t x);;
