type ('a, 'b) ast =
    { mutable ast_desc : 'a;
      mutable ast_type : 'b;
      ast_loc : Location.t }
;;

let mk_ast loc ty desc =
  { ast_desc = desc;
    ast_type = ty;
    ast_loc = loc }
;;
