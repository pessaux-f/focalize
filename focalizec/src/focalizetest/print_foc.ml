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

open MyFormat;;
(* open Format;; *)

let print_uses lib =
  print_string "use ";
  print_string ("\"" ^ lib ^ "\"");
  print_string ";;";
  force_newline ();
  print_string "open ";
  print_string ("\"" ^ lib ^ "\"");
  print_string ";;";
  force_newline ();;

let print_species_name ((m,s) : Own_expr.species_name) =
  if m = get_output_module () then
    print_string s
  else
    ( print_string m;
      print_string "#";
      print_string s
    )


(** Pretty print a collection definition *)
let print_coll c =
  print_string "collection";
  print_space ();
  print_string c.collname;
  print_space ();
  print_string "=";
  print_space ();
  print_string "implement";
  print_space ();
  let name,l_p = c.collimpl in
  print_string (snd name);
  (match l_p with
    | [] -> ()
    | e::r -> print_string "(";
              print_species_name e;
              List.iter (fun e ->
                           print_string ", ";
                           print_species_name e
                        ) r;
              print_string ")";
  );
  print_string ";";
  force_newline ();
  print_space ();
  print_string "end;;";
  force_newline ();
  force_newline ();;

let print_indent_symb i =
  match i with
  | Infix(s)
  | Prefix(None, s) -> print_string s
  | Prefix(Some m, s) ->
      if String.length s > 0 then
        if List.mem s.[0] ['!'; '#'; '~'; '?'; '$'; '{'] then
          print_string s
        else
          print_string (m ^ "#" ^ s)
      else
        print_string (m ^ "#" ^ s)



(** Pretty print an expression *)

let rec print_myexpr : (string * string) list -> myexpr -> unit =
  fun l_caml ->
  function
    | MIfte (cond,e1,e2) ->
        open_box 0;
          print_string "if";
          print_space ();
          print_myexpr l_caml cond;
          print_space ();
          print_string "then"; (* TODO : inverts these two lines *)
          open_box 2;          (* *)
            force_newline ();
            print_myexpr l_caml e1;
          close_box ();
          force_newline ();
          print_string "else"; (* TODO : inverts these two lines *)
          open_box 2;          (* *)
            force_newline ();
            print_myexpr l_caml e2;
          close_box ();
        close_box ()
    | MApp  (e,_, []) ->
            print_myexpr l_caml e;
(*             print_string "()" *)
    | MApp  (MFun (_,_,_) as e,_, e1::r) ->
            print_string "(";
            print_myexpr l_caml e;
            print_string ")";
            print_string "(";
            open_box 0;
            print_myexpr l_caml (fst e1);
            List.iter (fun (e,_) -> print_string ",";print_space ();
            print_myexpr l_caml e) r;
            print_string ")";
            close_box ();
    | MApp  (MGlob_id(Infix s), _, e1::e2::[]) ->
            print_string "(";
            print_myexpr l_caml (fst e1);
            print_space ();
            print_string s;
            print_space ();
            print_myexpr l_caml (fst e2);
            print_string ")";
    | MApp  (e, _, e1::r) ->
            print_myexpr l_caml e;
            print_string "(";
            open_box 0;
            print_myexpr l_caml (fst e1);
            List.iter (fun (e,_) -> print_string ",";print_space ();
            print_myexpr l_caml e) r;
            print_string ")";
            close_box ()
    | MMeth (None, s) ->
            print_string "!";
            print_string s;
    | MMeth (Some e,s) ->
            print_string e;
            print_string "!";
            print_string s;
    | MFun  (var,_, e ) ->
            print_string "function";
            print_space ();
            print_string var;
            print_space ();
            print_string "->";
            print_space ();
            open_box 2;
            force_newline ();
            print_myexpr l_caml e;
            close_box ()
    | MVarloc (b, var,e1,(MVarloc(_,_,_,_) as e2)) ->
            print_string "let";
            print_space ();
            (if b then
              (print_string "rec";
              print_space ()
              )
            );
            print_string (fst var);
            print_space ();
            print_string "=";
            print_space ();
            open_box 2;
            (match e1 with |MVarloc(_,_,_,_) -> force_newline() | _ -> ());
            print_myexpr l_caml e1;
            print_space ();
            print_string "in";
            close_box ();
            force_newline ();
            print_myexpr l_caml e2
    | MVarloc (b, var,e1,e2) ->
            print_string "let";
            print_space ();
            (if b then
              (print_string "rec";
              print_space ()
              )
            );
            print_string (fst var);
            print_space ();
            print_string "=";
            print_space ();
            open_box 2;
            print_myexpr l_caml e1;
            print_space ();
            print_string "in";
            force_newline ();
            print_myexpr l_caml e2;
            close_box ()
    | MGlob_id s ->
        print_indent_symb s
    | MCaml_def s ->
        print_string "internal 'a";
        force_newline ();
        print_string "external | caml -> {*";
        print_space ();
        let s = try List.assoc s l_caml with 
                | Not_found -> failwith (s ^ " caml definition unvailable") in
        print_string s;
        print_string "*}"
    | MVar(id, None) -> (if not (id = focself) then print_string id)
    | MVar(id, Some _t) -> (if not (id = focself) then print_string id) (* TODO ? *)
    | MInt i -> print_int i
    | MString s -> print_string ("\"" ^ (* String.escaped *) s ^ "\"")
    | MMatch((e, _),c_l) -> print_string "(match";
                       print_space ();
                       print_myexpr l_caml e;
                       print_space ();
                       print_string "with";
                       open_box 2;
                       List.iter
                         (fun (s,l,e) ->
                           force_newline ();
                           print_string "|";
                           print_space ();
                           (match s, l with
                           | Prefix(_, _), _ ->
                               print_indent_symb s;
                               if l <> [] then
                                 print_string (to_args (function None -> "_" | Some id -> id) l);
                           | Infix s, [e1; e2] ->
                               (match e1 with 
                               | None -> ()
                               | Some s -> print_string s
                               );
                               print_space ();
                               print_string s;
                               (match e2 with 
                               | None -> ()
                               | Some s -> print_string s
                               )
                           | _ -> failwith "pattern: infix operator applied to <> 2 args"
                           );
                           print_space ();
                           print_string "->";
                           open_box 2;
                           force_newline ();
                           print_myexpr l_caml e;
                           close_box ();
                           )
                                 c_l;
                       close_box ();
                       force_newline ();
                       print_string ")"
                       ;;

let rec print_list_comma l print_elem =
  match l with
    | [] -> ()
    | [e] -> print_elem e
    | e::(_::_ as r) ->print_elem e;
                       print_string ",";
                       print_space ();
                       print_list_comma r print_elem;;

(** Pretty print a species definition *)
let print_inherits l =
  print_list_comma l (fun (s,l) ->
                        print_species_name s;
                        if not(l = []) then
                         (print_string "(";
                          print_list_comma l print_string;
                          print_string ")"
                         ));;


let rec print_args la =
    match la with
        [] -> ()
      | [n,None] -> print_string n;
      | [n,Some t] ->
(*       | [n, t] -> print_string n; *)
          print_string n;
          print_space ();
          print_string ":";
          print_space ();
          print_string (string_of_typ t)
      | e::(_::_ as r) -> print_args [e];
                          print_string ",";
                          print_space ();
                          print_args r;;


let rec split_arg_def e =
  match e with
    | MFun(v,t,e) -> let lv,e = split_arg_def e in
                         (v,t)::lv,e
    | _ -> [],e;;

let print_a_meth_bind m l =
  print_string m.methname;
  let (args,e) = split_arg_def m.methdef in
  if not (args = []) then
    (print_string "(";
     print_args args;
     print_string ")"
    );
  print_space ();
  print_string ":";
  print_space ();
  print_string (string_of_typ m.methtyp);
  print_space ();
  print_string "=";
  open_box 2;
  force_newline ();
  print_myexpr l e;
  close_box();
  force_newline ();;

let print_meth m l =
  match m with
  | Unique m ->
      print_string "let ";
      if m.methrec then
        (print_space ();
        print_string "rec";
        print_space ()
        );
      print_a_meth_bind m l;
      print_string ";";
  | Multiple ll ->
      let rec aux ll =
        match ll with
        | [] -> ()
        | [m] ->
            print_a_meth_bind m l
        | m::r ->
            print_a_meth_bind m l;
(*             print_space (); *)
(*            print_string "and"; *)
            print_string ";";
            force_newline ();
            print_string " let rec ";
            print_space ();
            aux r in
      print_string "let";
      print_space ();
      print_string "rec";
      print_space ();
      print_space ();
      aux ll;
      print_string ";";;

let print_spec spec l =
  print_string "species ";
  print_string spec.specname;
  if not(spec.specparam = []) then
    (print_string "(";
     print_list_comma spec.specparam
                      (function
                        PrmColl(n,(s,l)) ->
                         print_string n; 
                         print_string " is ";
                         print_string (string_of_species_name s);
                         if not(l = [] ) then
                           (print_string "(";
                            print_list_comma l print_string;
                             print_string ")"
                           )
                        | PrmEnt(n,t,_) ->
                         print_string n; 
                         print_string " : ";
                         print_string (string_of_typ t);
                      );
     print_string ")"
    );
  print_space ();
  print_string "=";
  open_box 2;
    force_newline ();
    if not(spec.specinh = []) then
      (print_space ();
       print_string "inherit";
       print_space ();
       print_inherits spec.specinh;
       print_string ";";
       force_newline ()
      );
    begin
      match spec.specrep with
        | None -> if spec.specinh = [] then print_string "rep;"
        | Some t -> print_string ("representation = " ^ string_of_typ t ^ ";");
                    force_newline ()
    end;
    List.iter (fun e -> force_newline (); print_meth e l) spec.specdef;
  close_box ();
  force_newline ();
  print_string "end;;";
  force_newline ();;

let print_tlet n t e l_caml =
  print_string "let";
  print_space ();
  print_string n;
  print_space ();
  begin
  match t with
  | None -> ()
  | Some t -> print_string ":";
              print_space ();
              print_string (string_of_typ t);
              print_space ()
  end;
  print_string "=";
  print_space ();
  open_box 2;
  (match e with |MVarloc(_,_,_,_) -> force_newline() | _ -> ());
  print_myexpr l_caml e;
  print_string ";;";
  close_box ();
  force_newline ();;


let print_args_cons la =
    match la with
        [] -> ()
      | [e] -> 
          print_string "(";
          print_string (string_of_typ e);
          print_string ")";
      | e::r ->
          print_string "(";
          print_string (string_of_typ e);
          List.iter (fun e -> print_string (", " ^ string_of_typ e)) r;
          print_string ")";;

let print_ttype n c_l =
  print_string "type";
  print_space ();
  print_string n;
  print_space ();
  print_string "=";
  open_box 2;
  List.iter (fun (c,t) -> force_newline ();
                          print_string "|";
                          print_space ();
                          print_string c;
                          print_args_cons t 
            ) c_l;
  close_box ();
  force_newline ();
  print_string ";;";
  force_newline ();;

let print_tcall expr l=
  print_myexpr l expr;
  print_string ";;";
  force_newline ();;
(**************************************)
(* the test itself  *)

let print_toplevel_def_list ast fml =
  let f s =
    try List.assoc s fml with 
    | Not_found -> []
        in 
  List.iter
    (function
    | ObjCollection c -> print_coll c
    | ObjSpecies s -> print_spec s (f s.specname)
    | ObjToplet(n,t,e) -> print_tlet n t e (f "toplevel")
    | ObjTopcall e -> print_tcall e (f "toplevel")
    | ObjType(n,c_l) -> print_ttype n c_l
    ) ast;;

let print_foc_file f ast (fml : Own_expr.fichier_fml) =
  set_margin 120;
  if not(f = "") then (* default is stdout *)
    set_formatter_out_channel (open_out f);
  List.iter (fun m ->
              print_string "open \"";
              print_string m;
              print_string "\";;";
              force_newline ()
           ) ("basics" :: Whattodo.get_open ());
  List.iter print_uses ast.ficopenuse;
  force_newline ();
  print_toplevel_def_list ast.ficobjet fml;
  force_newline ();;
  (* if there no property to test, don't execute test *)
(*  match l_coll_test with
  | [] -> ()
  | e::r -> print_toplevel_def_list (postambule (call_test_prop e r));;
*)


let set_out_channel = set_formatter_out_channel;;
