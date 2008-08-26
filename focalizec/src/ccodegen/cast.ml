open Format
open Types

type file = string * phrase list

and phrase =
    Include of string
  | Comment of string
  | Typedef of ctype * string
  | Export of phrase
  | Signature of bool * string * ctype
  | Data of string * ctype * primary_expr
  | Function of ctype * string * (string * ctype) list * statement list

and primary_expr =
    Ident of string
  | Concat of primary_expr list
  | Constant of string
  | Ref of primary_expr
  | Cast of ctype * primary_expr
  | Access of primary_expr * string

and statement =
    Expr of expr
  | Return of expr
  | Decl of string * ctype
  | Affect of primary_expr * expr
  | Skip
  | If of expr * statement list * statement list

and expr =
    Prim of primary_expr
  | Call of primary_expr * expr list
  | ECast of ctype * expr
  | Equal of expr * expr

let pp_list b s e em f ppf l =
  let rec aux ppf = function
      [] -> assert false
    | [h] ->
	fprintf ppf "%a%a"
	  f h
	  fprintf (format_of_string e)
    | h::t ->
	fprintf ppf "%a%a%a"
	  f h
	  fprintf (format_of_string s)
	  aux t
  in
  match l with
    [] -> fprintf ppf (format_of_string em)
  | _ ->
      fprintf ppf "%a%a"
	fprintf (format_of_string b)
	aux l

module PP =
  struct
    
    let rec ctype ppf = function
	TypeId str ->
	  fprintf ppf "%s" str
      | Ptr t ->
	  fprintf ppf "%a*" ctype t
      | Annot (t, str) ->
	  fprintf ppf "%a /*%s*/" ctype t str
      | Fun (ret, args) ->
	  fprintf ppf "%a (*) %a"
	    ctype ret
	    (pp_list "(@[" ",@;" "@])" "(void)" ctype) args
      |	Param (t, l) ->
	  fprintf ppf "@[%a /* P%a */@]"
	    ctype t
	    (pp_list "(@[" ",@;" "@])" "" ctype) l
      |	Struct (ido, l) ->
	  fprintf ppf "struct%a%a"
	    (fun ppf x -> 
	      match x with None -> () | Some id -> fprintf ppf " %s" id) ido
	    (pp_list "@;@[<hov 2>{@;@[" ";@\n" ";@]@]@;}" ""
	       (fun ppf (lbl, t) -> 
		 match t with
		   Fun (ret, args) ->
		     fprintf ppf "@[%a@;@[<hov 2>(*%s)@;%a@]@]"
		       ctype ret
		       lbl
		       (pp_list "(@[" ",@;" "@])" "(void)" ctype) args
		 | _ ->
		     fprintf ppf "@[%a@;%s@]" ctype t lbl)) l
      |	Enum (ido, l) ->
	  fprintf ppf "@[<hov 2>enum %a%a@]"
	    (fun ppf x -> 
	      match x with None -> () | Some id -> fprintf ppf "%s" id) ido
	    (pp_list "{@;@[" ",@;" "@]@;}" ""
	       (fun ppf str -> 
		 fprintf ppf "%s" str)) l
      |	Union (ido, l) ->
	  fprintf ppf "union%a%a"
	    (fun ppf x -> 
	      match x with None -> () | Some id -> fprintf ppf " %s" id) ido
	    (pp_list "@;@[<hov 2>{@;@[" ";@;" ";@]@]@;}" ""
	       (fun ppf (lbl, t) ->
		 if lbl = "" then
		   fprintf ppf "%a" ctype t
		 else 
		   fprintf ppf "%a %s" ctype t lbl)) l

    let rec primary_expr ppf = function
	Ident code ->
	  fprintf ppf "@[%s@]" code
      |	Concat l ->
	  fprintf ppf "@[%a@]"
	    (pp_list "{@[" ",@;" "@]}" "{}" primary_expr) l
      |	Constant str ->
	  fprintf ppf "%s" str
      |	Ref p ->
	  fprintf ppf "@[&%a@]"
	    primary_expr p
      |	Cast (t, p) ->
	  fprintf ppf "@[(%a)@;(%a)@]"
	    ctype t
	    primary_expr p
      |	Access (p, str) ->
	  fprintf ppf "@[%a->%s@]"
	    primary_expr p
	    str

    and statement ppf = function
	Expr e ->
	  fprintf ppf "@[%a;@]"
	    expr e
      |	Return e ->
	  fprintf ppf "@[return %a;@]"
	    expr e
      |	Decl (str, t) ->
	  fprintf ppf "@[%a %s;@]"
	    ctype t 
	    str
      |	Affect (p, e) ->
	  fprintf ppf "@[<hov 2>%a =@;%a;@]"
	    primary_expr p
	    expr e
      |	Skip ->
	  fprintf ppf ";"
      |	If (a, l, l') ->
	  fprintf ppf "@[<hov 2>if (%a)@\n@[<hov 2>{@\n%a@]@\n}@]@\n@[<hov 2>else@\n@[<hov 2>{@\n%a@]@\n}@]"
	    expr a
	    (pp_list "@[" "@\n" "@]" "" statement) l
	    (pp_list "@[" "@\n" "@]" "" statement) l'


    and expr ppf = function
	Prim pe -> 
	  primary_expr ppf pe
      |	Call (pe, args) ->
	  fprintf ppf "@[%a%a@]"
	    primary_expr pe
	    (pp_list "(@[" ",@;" "@])" "()" expr) args
      |	ECast (t, e) ->
	  fprintf ppf "@[(%a)(%a)@]"
	    ctype t
	    expr e
      |	Equal (a, b) ->
	  fprintf ppf "@[%a ==@;%a@]"
	    expr a
	    expr b

    let rec phrase ppf = function
	Include str ->
	  fprintf ppf "@[#include \"%s.h\"@]" str
      | Comment str ->
	  fprintf ppf "@[<hov 3>/* %s*/@]" str
      | Typedef (te, str) ->
	  fprintf ppf "@[typedef %a@;%s;@]"
	  ctype te
	    str
      |	Export ph ->
	  phrase ppf ph
      |	Signature (inline, name, Fun (ret, args)) ->
	  fprintf ppf "@[%a%a %s %a;@]"
	    (fun ppf b -> if b then fprintf ppf "inline@ " else ()) inline
	    ctype ret
	    name
	    (pp_list "(@[" ",@;" "@])" "(void)" ctype) args
      |	Signature (_, name, ty) ->
	  fprintf ppf "@[%a %s;@]"
	    ctype ty
	    name
      |	Data (name, ty, pe) ->
	  begin match ty with
	    Fun (ret, args) ->
	      fprintf ppf "@[<hov 2>%a (*%s) %a =@;%a;@]"
		ctype ret
		name
		(pp_list "(@[" ",@;" "@])" "(void)" ctype) args
		primary_expr pe
	  | _ ->
	      fprintf ppf "@[<hov 2>%a %s =@;%a;@]"
		ctype ty
		name
		primary_expr pe
	  end
      |	Function (ret, name, args, body) ->
	  fprintf ppf "@[%a %s %a@\n@[<hov 2>{@\n@[%a@]@]@\n}@]"
	    ctype ret
	    name
	    (pp_list "(@[" ",@;" "@])" "(void)"
	       (fun ppf (id, ty) ->
		 fprintf ppf "%a %s" ctype ty id)) args
	    (pp_list "" "@\n" "" "" statement) body
	  

  
    let file (hdr, src) f =
      let (base, l) = f in
      let intro ppf = 
	fprintf ppf "@[<hov 3>/*@ This@ file@ is@ automaticaly@ generated.@ Modify@ it@ at@ you@ own@ risks.@ */@]@." in
      intro hdr;
      intro src;
      let interface = 
	Include "c_builtins" ::
	(List.flatten (List.map (function Export ph -> [ph] | _ -> []) l)) in
      let body =
	Include base ::
	(List.flatten
	   (List.map (fun x -> match x with
	     Export _ -> []
	   | _ -> [x]) l)) in
      let macro = String.uppercase base in
      fprintf hdr "@\n@[#ifndef __%s_H@]@." macro;
      fprintf hdr "@\n@[#define __%s_H@]@." macro;
      let pp_list = pp_list "@\n" "@.@\n" "@." "" phrase in
      pp_list hdr interface;
      fprintf hdr "@\n@[#endif@]@.";
      pp_list src body

  end


