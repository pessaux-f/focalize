open Format;;

open Own_prolog;;

(** pretty print a list of elements. *)
let pretty_print_list output_formatter l f sep =
  match l with
  | [] -> ()
  | e::[] -> f output_formatter e
  | e::((_::_) as r )->
      f output_formatter e;
      List.iter (fun e ->
                   sep output_formatter;
                   fprintf output_formatter "%a" f e) r;;

let pretty_print_list_sep output_formatter l f f_sep sep =
  let rec aux l =
    match l with
    | [] -> ()
    | e::r ->
        begin
          if f_sep e then
            fprintf output_formatter "@\n%a" f e
          else
            begin
              sep output_formatter;
              fprintf output_formatter "%a" f e;
            end
        end;
        aux r in
  match l with
  | [] -> ()
  | e::[] ->
      f output_formatter e
  | e::r ->
      f output_formatter e;
      aux r;;

(** pretty print a list of elements separate by commas. *)
let pretty_print_list_comma output_formatter l f =
  pretty_print_list output_formatter l f
                    (fun _e -> fprintf output_formatter ",@ ");;

let pretty_print_list_sep_comma output_formatter l f f_sep =
  pretty_print_list_sep output_formatter l f f_sep
                    (fun _e -> fprintf output_formatter ",@ ");;

(** pretty print a list of elements separate by commas. *)
let pretty_print_list_double_newline output_formatter l f =
  pretty_print_list output_formatter l f
                    (fun _e -> fprintf output_formatter "@\n@\n");;

(** This list contains the priority of all operators. *)
let priority_list = ["-->",  1200; ":-",   1200; "?-",  1200; "if",    1200;
                     "else", 1170; "then", 1150; ";",   1100; "do",    1100;
                     "|",    1100; "->",   1050; "*->", 1050; "except",1050;
                     "from", 1050; ",",    1000; "#<",   700; "#<=",    700;
                     "#<=>", 700;
                     "#=",    700; "#=<",   700; "#>",   700; "#>=",    700;
                     "#\\=",  700; "::",    700; "<",    700; "=",      700;
                     "=..",   700; "=:=",   700; "=<",   700; "==",     700;
                     "=\\=",  700; ">",     700; ">=",   700; "@<",     700;
                     "@=<",   700; "@>",    700; "@>=",  700; "\\=",    700;
                     "\\==",  700; "is",    700; "~=",   700; "@",      650;
                     "with",  650; ":",    600; "..",    600; "+",      500;
                     "-",     500; "/\\",  500; "\\/",   500; "*",      400; 
                     "/",     400; "//",   400; "rem",   400; "div",    400;
                     "mod",   400; "<<",   400; ">>",    400; "^",      200];;

let separator_prior = List.assoc "," priority_list;;

(** A prolog term is an infix operator if and only if it has arity 2 and is in
the [priority_list].
[is_infix f_name args] returns [true] if [f_name] applied to the arguments [args] is
an infix operator. It returns [false] otherwise. *)
let is_infix f =
  List.mem_assoc f priority_list;;

(** A we take the priority of an operator from its name. If the function name
passed in argument is not an operator returns [-1]. *)
let prior_of f =
  try List.assoc f priority_list
  with
  Not_found -> -1;;

(** Print a prolog term. *)
let rec print_prolog_term last_prior singleton output_formatter t =
  match t with
  | Prolog_comment s -> 
      fprintf output_formatter "@[/* %s */ @]\n" s
  | Prolog_fun(s,([arg1;arg2] as t_l)) ->
      if is_infix s then
        (let curr_prior = prior_of s in
         let par = last_prior < curr_prior in
         fprintf output_formatter "@[%s%a@ %s@ %a%s@]"
                 (if par then "(" else "")
                 (print_prolog_term curr_prior singleton) arg1 
                 s
                 (print_prolog_term curr_prior singleton) arg2
                 (if par then ")" else "")
        )
      else
        fprintf output_formatter "@[%s(@[%a@])@]"
                s (print_prolog_terms_comma singleton) t_l
  | Prolog_fun(s,[]) ->
      fprintf output_formatter "%s" s
  | Prolog_fun(s,t_l) -> 
      fprintf output_formatter "@[%s(@[<hv>%a@])@]"
              s (print_prolog_terms_comma singleton) t_l
  | Prolog_var(x) ->
      if List.mem x singleton then
        fprintf output_formatter "%s" ("_" ^ x)
      else
        fprintf output_formatter "%s" x
  | Prolog_conjunction l ->
      let curr_prior = prior_of "," in
      let par = last_prior <= curr_prior in
      fprintf output_formatter "@[%s%a%s@]"
              (if par then "(" else "")
              (print_prolog_terms_comma singleton) l
              (if par then ")" else "")
  | Prolog_int i ->
      fprintf output_formatter "%d" i
  | Prolog_list l ->
      fprintf output_formatter "[@[<hv>%a@]]"
          (print_prolog_terms_comma singleton) l
(** Print a list a prolog terms separate by commas. *)
and print_prolog_terms_comma singleton output_formatter l =
  pretty_print_list_sep_comma output_formatter
                                 l
                                 (print_prolog_term separator_prior singleton)
                                 prolog_is_comment;;

(** Print a prolog clause. *)
let print_prolog_clause output_formatter ((h,b) : prolog_clause) =
  let singleton = get_singleton (h,b) in
  match h with
  | None ->
      fprintf output_formatter "@[<hov 2>:-@ @[%a.@]@]@\n"
                               (print_prolog_terms_comma []) b
  | Some h -> 
      fprintf output_formatter "@[<hov 2>%a@ :-@ @[%a.@]@]@\n"
                   (print_prolog_term (prior_of ":-") singleton) h
                   (print_prolog_terms_comma singleton) b;;

(** Print a list a prolog clause separate by commas. *)
let print_prolog_clause_list output_formatter l =
  pretty_print_list_comma output_formatter l print_prolog_clause;;

(** Print a prolog program. *)
let print_prolog_pgm output_formatter (l : prolog_pgm) =
  pretty_print_list_double_newline output_formatter l print_prolog_clause;;

