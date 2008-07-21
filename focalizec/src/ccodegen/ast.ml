type ('a, 'b) ast =
    { mutable ast_desc : 'a;
      ast_type : 'b;
      ast_loc : Location.t option }
;;

let mk_ast ?(loc = None) ty desc =
  { ast_desc = desc;
    ast_type = ty;
    ast_loc = loc }
;;

let mk_uast ?(loc = None) desc =
  { ast_desc = desc;
    ast_type = ();
    ast_loc = loc }
;;
