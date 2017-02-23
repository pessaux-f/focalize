open Own_basics ;;
open Own_types ;;
open Own_expr ;;
open Whattodo ;;


module MyFormat :
    sig
      val print_string : string -> unit
      val force_newline : unit -> unit
      val open_box : int -> unit
      val close_box : unit -> unit
      val print_space : unit -> unit
      val print_int : int -> unit
      val set_margin : int -> unit
      val set_formatter_out_channel : out_channel -> unit
    end =
   struct
        let box = ref [];;
        let current = ref 0;;
        let indent = ref 0;;
        let file = ref stdout;;
        let sep = ref 0;;
        let max = ref 120;;

        let set_formatter_out_channel f = file := f;;

        let set_margin s = max := s;;

        let force_newline () =
                output_char !file '\n';
                output_string !file (String.make !indent ' ');
                sep := 0;
                current := !indent;
                flush !file;;


        let print_space () = sep := !sep + 1;;

        let open_box n = box := n :: !box; indent := !indent + n;;

        let close_box () =
          match !box with
            | [] -> ()
            | n::r -> box:=r;indent := !indent - n;;


        let print_string s =
                let l = String.length s in
                if !current + !sep + l > !max then
                 (force_newline ();
                  output_string !file s;
                  current := !current + l
                 )
                else
                 (output_string !file (String.make !sep ' ');
                  sep := 0;
                  output_string !file s;
                  current := !current + !sep + l
                 );;

        let print_int i = print_string (Pervasives.string_of_int i);;

end



let print_uses lib =
  MyFormat.print_string "use ";
  MyFormat.print_string ("\"" ^ lib ^ "\"");
  MyFormat.print_string ";;";
  MyFormat.force_newline ();
  MyFormat.print_string "open ";
  MyFormat.print_string ("\"" ^ lib ^ "\"");
  MyFormat.print_string ";;";
  MyFormat.force_newline ()
;;

let print_species_name ((m,s) : Own_expr.species_name) =
  if m = get_output_module () then
    MyFormat.print_string s
  else
    ( MyFormat.print_string m;
      MyFormat.print_string "#";
      MyFormat.print_string s
    )


(** Pretty print a collection definition *)
let print_coll c =
  MyFormat.print_string "collection";
  MyFormat.print_space ();
  MyFormat.print_string c.collname;
  MyFormat.print_space ();
  MyFormat.print_string "=";
  MyFormat.print_space ();
  MyFormat.print_string "implement";
  MyFormat.print_space ();
  let name,l_p = c.collimpl in
  MyFormat.print_string (snd name);
  (match l_p with
    | [] -> ()
    | e::r -> MyFormat.print_string "(";
              print_species_name e;
              List.iter (fun e ->
                           MyFormat.print_string ", ";
                           print_species_name e
                        ) r;
              MyFormat.print_string ")";
  );
  MyFormat.print_string ";";
  MyFormat.force_newline ();
  MyFormat.print_space ();
  MyFormat.print_string "end;;";
  MyFormat.force_newline ();
  MyFormat.force_newline ();;

let print_indent_symb i =
  match i with
  | Infix(s)
  | Prefix(None, s) -> MyFormat.print_string s
  | Prefix(Some m, s) ->
      if String.length s > 0 then
        if List.mem s.[0] ['!'; '#'; '~'; '?'; '$'; '{'] then
          MyFormat.print_string s
        else
          MyFormat.print_string (m ^ "#" ^ s)
      else
        MyFormat.print_string (m ^ "#" ^ s)



(** Pretty print an expression *)

let rec print_myexpr : (string * string) list -> myexpr -> unit =
  fun l_caml ->
  function
    | MIfte (cond,e1,e2) ->
        MyFormat.open_box 0;
        MyFormat.print_string "if";
        MyFormat.print_space ();
        print_myexpr l_caml cond;
        MyFormat.print_space ();
        MyFormat.print_string "then"; (* TODO : inverts these two lines *)
        MyFormat.open_box 2;          (* *)
        MyFormat.force_newline ();
        print_myexpr l_caml e1;
        MyFormat.close_box ();
        MyFormat.force_newline ();
        MyFormat.print_string "else"; (* TODO : inverts these two lines *)
        MyFormat.open_box 2;          (* *)
        MyFormat.force_newline ();
        print_myexpr l_caml e2;
        MyFormat.close_box ();
        MyFormat.close_box ()
    | MApp  (e,_, []) ->
        print_myexpr l_caml e ;
        (* MyFormat.print_string "()" *)
    | MApp  (MFun (_,_,_) as e,_, e1::r) ->
        MyFormat.print_string "(";
        print_myexpr l_caml e;
        MyFormat.print_string ")";
        MyFormat.print_string "(";
        MyFormat.open_box 0;
        print_myexpr l_caml (fst e1);
        List.iter
          (fun (e, _) ->
            MyFormat.print_string "," ;
            MyFormat.print_space () ;
            print_myexpr l_caml e)
          r ;
        MyFormat.print_string ")" ;
        MyFormat.close_box () ;
    | MApp  (MGlob_id(Infix s), _, e1::e2::[]) ->
        MyFormat.print_string "(";
        print_myexpr l_caml (fst e1);
        MyFormat.print_space ();
        MyFormat.print_string s;
        MyFormat.print_space ();
        print_myexpr l_caml (fst e2);
        MyFormat.print_string ")";
    | MApp  (e, _, e1::r) ->
        print_myexpr l_caml e;
        MyFormat.print_string "(";
        MyFormat.open_box 0;
        print_myexpr l_caml (fst e1);
        List.iter
          (fun (e, _) ->
            MyFormat.print_string "," ;
            MyFormat.print_space () ;
            print_myexpr l_caml e)
          r ;
        MyFormat.print_string ")" ;
        MyFormat.close_box ()
    | MMeth (None, s) ->
        MyFormat.print_string "!";
        MyFormat.print_string s;
    | MMeth (Some e,s) ->
        MyFormat.print_string e;
        MyFormat.print_string "!";
        MyFormat.print_string s;
    | MFun  (var,_, e ) ->
        MyFormat.print_string "function";
        MyFormat.print_space ();
        MyFormat.print_string var;
        MyFormat.print_space ();
        MyFormat.print_string "->";
        MyFormat.print_space ();
        MyFormat.open_box 2;
        MyFormat.force_newline ();
        print_myexpr l_caml e;
        MyFormat.close_box ()
    | MVarloc (b, var,e1,(MVarloc(_,_,_,_) as e2)) ->
        MyFormat.print_string "let";
        MyFormat.print_space ();
        (if b then
          (MyFormat.print_string "rec";
           MyFormat.print_space ()
          )
        );
        MyFormat.print_string (fst var);
        MyFormat.print_space ();
        MyFormat.print_string "=";
        MyFormat.print_space ();
        MyFormat.open_box 2;
        (match e1 with
        | MVarloc(_,_,_,_) -> MyFormat.force_newline() | _ -> ());
        print_myexpr l_caml e1;
        MyFormat.print_space ();
        MyFormat.print_string "in";
        MyFormat.close_box ();
        MyFormat.force_newline ();
        print_myexpr l_caml e2
    | MVarloc (b, var,e1,e2) ->
        MyFormat.print_string "let";
        MyFormat.print_space ();
        (if b then
          (MyFormat.print_string "rec";
           MyFormat.print_space ()
          )
        );
        MyFormat.print_string (fst var);
        MyFormat.print_space ();
        MyFormat.print_string "=";
        MyFormat.print_space ();
        MyFormat.open_box 2;
        print_myexpr l_caml e1;
        MyFormat.print_space ();
        MyFormat.print_string "in";
        MyFormat.force_newline ();
        print_myexpr l_caml e2;
        MyFormat.close_box ()
    | MGlob_id s ->
        print_indent_symb s
    | MCaml_def s ->
        (* Choose a typr variable having fw chances to really appear somewhere
           else. We can't analyze the type of the external definition since ...
           we do not have it. So saying that the type is a variable will
           allways pass even if too laxist. *)
        MyFormat.print_string "internal 'zzz" ;
        MyFormat.force_newline () ;
        MyFormat.print_string "external | caml -> {*" ;
        MyFormat.print_space ();
        let s =
          try List.assoc s l_caml with
          | Not_found -> failwith (s ^ " caml definition unvailable") in
        MyFormat.print_string s ;
        MyFormat.print_string "*}"
    | MVar(id, None) -> (if not (id = focself) then MyFormat.print_string id)
    | MVar (id, Some _t) ->
        if not (id = focself) then MyFormat.print_string id (* TODO ? *)
    | MInt i -> print_int i
    | MString s -> MyFormat.print_string ("\"" ^ (* String.escaped *) s ^ "\"")
    | MMatch ((e, _), c_l) ->
        MyFormat.print_string "(match" ;
        MyFormat.print_space () ;
        print_myexpr l_caml e ;
        MyFormat.print_space () ;
        MyFormat.print_string "with" ;
        MyFormat.open_box 2 ;
        List.iter
          (fun (s,l,e) ->
            MyFormat.force_newline () ;
            MyFormat.print_string "|" ;
            MyFormat.print_space () ;
            (match s, l with
            | Prefix (_, _), _ ->
                print_indent_symb s ;
                if l <> [] then
                  MyFormat.print_string
                    (to_args (function None -> "_" | Some id -> id) l) ;
            | Infix s, [e1; e2] ->
                (match e1 with
                | None -> ()
                | Some s -> MyFormat.print_string s
                );
                MyFormat.print_space () ;
                MyFormat.print_string s ;
                (match e2 with
                | None -> ()
                | Some s -> MyFormat.print_string s
                )
            | _ -> failwith "pattern: infix operator applied to <> 2 args"
            );
            MyFormat.print_space () ;
            MyFormat.print_string "->" ;
            MyFormat.open_box 2 ;
            MyFormat.force_newline () ;
            print_myexpr l_caml e ;
            MyFormat.close_box ())
          c_l ;
        MyFormat.close_box () ;
        MyFormat.force_newline () ;
        MyFormat.print_string ")"
;;


let rec print_list_comma l print_elem =
  match l with
    | [] -> ()
    | [e] -> print_elem e
    | e::(_::_ as r) ->print_elem e;
                       MyFormat.print_string ",";
                       MyFormat.print_space ();
                       print_list_comma r print_elem;;

(** Pretty print a species definition *)
let print_inherits l =
  print_list_comma l (fun (s,l) ->
                        print_species_name s;
                        if not(l = []) then
                         (MyFormat.print_string "(";
                          print_list_comma l MyFormat.print_string;
                          MyFormat.print_string ")"
                         ));;


let rec print_args la =
    match la with
        [] -> ()
      | [n,None] -> MyFormat.print_string n;
      | [n,Some t] ->
(*       | [n, t] -> MyFormat.print_string n; *)
          MyFormat.print_string n;
          MyFormat.print_space ();
          MyFormat.print_string ":";
          MyFormat.print_space ();
          MyFormat.print_string (string_of_typ t)
      | e::(_::_ as r) -> print_args [e];
                          MyFormat.print_string ",";
                          MyFormat.print_space ();
                          print_args r;;


let rec split_arg_def e =
  match e with
    | MFun(v,t,e) -> let lv,e = split_arg_def e in
                         (v,t)::lv,e
    | _ -> [],e;;

let print_a_meth_bind m l =
  MyFormat.print_string m.methname;
  let (args,e) = split_arg_def m.methdef in
  if not (args = []) then
    (MyFormat.print_string "(";
     print_args args;
     MyFormat.print_string ")"
    );
  MyFormat.print_space ();
  MyFormat.print_string ":";
  MyFormat.print_space ();
  MyFormat.print_string (string_of_typ m.methtyp);
  MyFormat.print_space ();
  MyFormat.print_string "=";
  MyFormat.open_box 2;
  MyFormat.force_newline ();
  print_myexpr l e;
  MyFormat.close_box();
  MyFormat.force_newline ()
;;



let print_meth m l =
  match m with
  | Unique m ->
      MyFormat.print_string "let ";
      if m.methrec then
        (MyFormat.print_space ();
        MyFormat.print_string "rec";
        MyFormat.print_space ()
        );
      print_a_meth_bind m l;
      MyFormat.print_string ";";
  | Multiple ll ->
      let rec aux ll =
        match ll with
        | [] -> ()
        | [m] ->
            print_a_meth_bind m l
        | m::r ->
            print_a_meth_bind m l;
(*             MyFormat.print_space (); *)
(*            MyFormat.print_string "and"; *)
            MyFormat.print_string ";";
            MyFormat.force_newline ();
            MyFormat.print_string " let rec ";
            MyFormat.print_space ();
            aux r in
      MyFormat.print_string "let";
      MyFormat.print_space ();
      MyFormat.print_string "rec";
      MyFormat.print_space ();
      MyFormat.print_space ();
      aux ll;
      MyFormat.print_string ";";;

let print_spec spec l =
  MyFormat.print_string "species ";
  MyFormat.print_string spec.specname;
  if not(spec.specparam = []) then
    (MyFormat.print_string "(";
     print_list_comma spec.specparam
                      (function
                        PrmColl(n,(s,l)) ->
                         MyFormat.print_string n;
                         MyFormat.print_string " is ";
                         MyFormat.print_string (string_of_species_name s);
                         if not(l = [] ) then
                           (MyFormat.print_string "(";
                            print_list_comma l MyFormat.print_string;
                             MyFormat.print_string ")"
                           )
                        | PrmEnt(n,t,_) ->
                         MyFormat.print_string n;
                         MyFormat.print_string " : ";
                         MyFormat.print_string (string_of_typ t);
                      );
     MyFormat.print_string ")"
    );
  MyFormat.print_space ();
  MyFormat.print_string "=";
  MyFormat.open_box 2;
    MyFormat.force_newline ();
    if not(spec.specinh = []) then
      (MyFormat.print_space ();
       MyFormat.print_string "inherit";
       MyFormat.print_space ();
       print_inherits spec.specinh;
       MyFormat.print_string ";";
       MyFormat.force_newline ()
      );
    begin
      match spec.specrep with
        | None -> if spec.specinh = [] then MyFormat.print_string "rep;"
        | Some t -> MyFormat.print_string ("representation = " ^ string_of_typ t ^ ";");
                    MyFormat.force_newline ()
    end;
    List.iter (fun e -> MyFormat.force_newline (); print_meth e l) spec.specdef;
  MyFormat.close_box ();
  MyFormat.force_newline ();
  MyFormat.print_string "end;;";
  MyFormat.force_newline ()
;;

let print_tlet n t e l_caml =
  MyFormat.print_string "let" ;
  MyFormat.print_space () ;
  MyFormat.print_string n ;
  MyFormat.print_space () ;
  (match t with
  | None -> ()
  | Some t ->
      MyFormat.print_string ":" ;
      MyFormat.print_space () ;
      MyFormat.print_string (string_of_typ t) ;
      MyFormat.print_space ()) ;
  MyFormat.print_string "=" ;
  MyFormat.print_space () ;
  MyFormat.open_box 2 ;
  (match e with |MVarloc(_,_,_,_) -> MyFormat.force_newline() | _ -> ()) ;
  print_myexpr l_caml e ;
  MyFormat.print_string ";;" ;
  MyFormat.close_box () ;
  MyFormat.force_newline ()
;;


let print_args_cons la =
    match la with
      | [] -> ()
      | [e] ->
          MyFormat.print_string "(";
          MyFormat.print_string (string_of_typ e);
          MyFormat.print_string ")";
      | e::r ->
          MyFormat.print_string "(";
          MyFormat.print_string (string_of_typ e);
          List.iter (fun e -> MyFormat.print_string (", " ^ string_of_typ e)) r;
          MyFormat.print_string ")";;

let print_ttype n c_l =
  MyFormat.print_string "type";
  MyFormat.print_space ();
  MyFormat.print_string n;
  MyFormat.print_space ();
  MyFormat.print_string "=";
  MyFormat.open_box 2;
  List.iter
    (fun (c,t) ->
      MyFormat.force_newline ();
      MyFormat.print_string "|";
      MyFormat.print_space ();
      MyFormat.print_string c;
      print_args_cons t)
    c_l ;
  MyFormat.close_box () ;
  MyFormat.force_newline ();
  MyFormat.print_string ";;";
  MyFormat.force_newline ()
;;

let print_tcall expr l=
  print_myexpr l expr;
  MyFormat.print_string ";;";
  MyFormat.force_newline ()
;;
(**************************************)
(* the test itself  *)

let print_toplevel_def_list ast fml =
  let f s =
    try List.assoc s fml with
    | Not_found -> [] in
  List.iter
    (function
      | ObjCollection c -> print_coll c
      | ObjSpecies s -> print_spec s (f s.specname)
      | ObjToplet (n, t, e) -> print_tlet n t e (f "toplevel")
      | ObjTopcall e -> print_tcall e (f "toplevel")
      | ObjType (n, c_l) -> print_ttype n c_l)
    ast
;;


let print_foc_file f ast (fml : Own_expr.fichier_fml) =
  MyFormat.set_margin 120;
  if f <> "" then (* default is stdout *)
    MyFormat.set_formatter_out_channel (open_out f) ;
  List.iter
    (fun m ->
      MyFormat.print_string "open \"" ;
      MyFormat.print_string m ;
      MyFormat.print_string "\";;" ;
      MyFormat.force_newline ()
    )
    ("basics" :: Whattodo.get_open ()) ;
  List.iter print_uses ast.ficopenuse ;
  MyFormat.force_newline () ;
  print_toplevel_def_list ast.ficobjet fml ;
  MyFormat.force_newline ()
;;
  (* if there no property to test, don't execute test *)
(*  match l_coll_test with
  | [] -> ()
  | e::r -> print_toplevel_def_list (postambule (call_test_prop e r));;
*)


let set_out_channel = MyFormat.set_formatter_out_channel;;
