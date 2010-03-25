open Own_expr;;
open Own_basics;;

type minifoc_var =
  | FVInt of string (** An integer variable *)
  | FVFun of string * Own_types.typ (** A functional variable *)
  | FVHer of string * Own_types.typ;;  (** An Herbrand variable *)

type minifoc_arg =
  | FVar of minifoc_var (* A variable name *)
  | FInt of int    (** An integer constant *)
  | FConstruct of string * minifoc_arg list;; (** Appel à un constructeur *)

(* My expressions *)
type minifoc_expr =
  | FIfte of string * minifoc_expr * minifoc_expr
  | FMethVar of string * Own_types.typ * minifoc_arg list
  | FFun      of (string * Own_types.typ) list * minifoc_expr
  | FMeth of string * string * minifoc_arg list
            (** a function is applied to a list of string/integer name/value *)
  | FBasic of string * minifoc_arg list (** same as [FMeth] *)
  | FMatch of string * (string * minifoc_arg list * minifoc_expr) list
                               (** pattern matching is only on variable name *)
  | FVarloc of minifoc_var * minifoc_expr * minifoc_expr (** a [let] expression *)
  | FValue of minifoc_arg;; (** the value of a variable or an integer *)

type minifoc_function = string * string list * minifoc_expr;;

let rec dbg_string_minifoc_var a =
  match a with
  | FVHer(v,t) -> "(" ^ v ^ ", " ^ Own_types.string_of_typ t ^ ")"
  | FVFun(v, t) -> "(" ^ v  ^ ", " ^ Own_types.string_of_typ t ^ ")"
  | FVInt v -> v
;;

let rec dbg_string_minifoc_arg a : string=
  match a with
  | FVar v -> dbg_string_minifoc_var v
  | FInt i -> string_of_int i
  | FConstruct(_,_) -> "Fconstruct(...)" (*failwith "string_of_minifoc_arg";; *)

let rec dbg_string_minifoc_expr e =
  match e with
  |FIfte(s,e1,e2) ->
      "if " ^ s ^ " then " ^ dbg_string_minifoc_expr e1 ^ " else " ^
      dbg_string_minifoc_expr e2
  |FMeth(s1,s2, ml) ->
      s1 ^ "!" ^ s2 ^ "(" ^
            List.fold_right (fun e s -> dbg_string_minifoc_arg e
      ^ "," ^ s)  ml "" ^ ")"
  | FMethVar(var, _, ml) ->
      var ^ "(" ^
            List.fold_right (fun e s -> dbg_string_minifoc_arg e
      ^ "," ^ s)  ml "" ^ ")"
  |FBasic(s2, ml) ->
      "#" ^ s2 ^ "(" ^
            List.fold_right (fun e s -> dbg_string_minifoc_arg e
                     ^ "," ^ s) ml ""  ^ ")"
  | FVarloc(v, e1, e2) ->
      "let " ^ dbg_string_minifoc_var v ^ " = " ^
      dbg_string_minifoc_expr e1 ^ " in " ^
      dbg_string_minifoc_expr e2
  | FValue m -> dbg_string_minifoc_arg m
  | FFun(l,e) ->
      List.fold_right (fun (v,t) s -> "fun (" ^ v ^ " : " ^ Own_types.string_of_typ t ^ ") -> " ^ s) l (dbg_string_minifoc_expr e)
  | FMatch(var, c_args_expr_l) ->
      "match " ^ var  ^ " with " ^
      List.fold_right (fun (c,_args,expr) s -> "| " ^ c ^ " -> " ^
      dbg_string_minifoc_expr expr ^ s) c_args_expr_l ""
(*   | _ -> "azerty" (*  failwith "not string_of_minifoc_expr";; *) *)


exception To_minifoc of string;;

let convert_pattern f (i, s_o_l, e) =
  Own_basics.ident_name i,
  List.map (function None -> FVar(FVInt (Fresh_variable.new_prolog_var ())) | Some s -> FVar(FVInt s)) s_o_l,
  f e;;

(** A function converting a focal expression to a minifoc expression. *)
let rec minifoc_expr_of_myexpr e =
  let rec add_let (l : (Own_expr.myexpr * Own_types.typ) list) e lv f =
    match l with
    | [] -> e lv
    | (MVar(v,_), t)::r ->
        add_let r e (lv @ [FVar(FVHer(v,t))]) f
    | (e', t)::r ->
        let v = Fresh_variable.new_prolog_var () in
        let v_t = FVHer(v, t) in
        FVarloc(v_t,
                f e',
                add_let r e (lv @ [FVar v_t]) f) in
  let rec aux e =
    match e with
    | MIfte(e1, e2, e3) ->
        let v = Fresh_variable.new_prolog_var () in
        FVarloc(FVHer(v, Own_types.TAtom(None, "bool")), aux e1,
                FIfte(v,aux e2, aux e3))
    | MVarloc(b, (x, t_o), e1, e2) ->
        if b then
          raise (To_minifoc "Recursive definition inside function")
        else
          begin
            match t_o with
            | None -> raise (To_minifoc "Lack of type information in AST")
            | Some t ->
                FVarloc(FVHer(x, t), aux e1, aux e2)
          end
    | MInt i -> FValue(FInt i)
    | MVar(x, Some t) -> FValue(FVar(FVHer(x, t)))
    | MVar(_, None) | MMatch((_, None), _)  ->
        raise (To_minifoc "Lack of type information in AST")
    | MCaml_def s ->
        raise (To_minifoc ("Can't convert the caml definition : " ^ s))
    | MString _s ->
        raise (To_minifoc "Can't convert a string into prolog")
    | MMatch((e1, Some t), l) ->
        let v = Fresh_variable.new_prolog_var () in
        let l_pat =
          List.map (convert_pattern aux) l in
        FVarloc(FVHer(v, t), aux e1,
                FMatch(v, l_pat))
    | MFun(_, _, _) ->
        let rec aux_fun e =
          match e with
          | MFun(_, None, _) -> raise (To_minifoc "Lack of type information MFun")
          | MFun(x, Some t, e) ->
              let l, e = aux_fun e in
              (x, t)::l, e
          | _ -> [], e in
        let l_args, e = aux_fun e in
            FFun(l_args, aux e)
        (* function call *)
    | MMeth(Some c, f) ->
        FMeth(c, f, [])
    | MMeth(None, f) ->
        FMeth(focself, f, [])
    | MApp(MMeth(None, f), _, l) ->
        add_let l (fun lv -> FMeth(focself, f, lv)) [] aux
    | MApp(MMeth(Some c, f), _, l) ->
        add_let l (fun lv -> FMeth(c, f, lv)) [] aux
        (* ************* *)
        (*  global call  *)
    | MGlob_id(i) ->
        if Focalize_inter.is_constructor i then
          FValue(FConstruct(String.uncapitalize (ident_name i), []))
        else
          FBasic(ident_name i, [])
    | MApp(MGlob_id(i), _, l) ->
        let func = 
          fun lv ->
            if Focalize_inter.is_constructor i then
              FValue(FConstruct(String.uncapitalize (ident_name i), lv))
            else
              FBasic(ident_name i, lv) in
        add_let l func [] aux
    | MApp(MVar(_, Some _t), None, _) -> raise (To_minifoc "Lack of type information MApp")
    | MApp(MVar(f, Some t), Some _, l) -> 
        add_let l (fun lv -> FMethVar(f, t, lv)) [] aux
    | MApp(_, None, _) -> raise (To_minifoc "Lack of type information MApp")
    | MApp(e, Some t, l) ->
        let v = Fresh_variable.new_prolog_var () in
        FVarloc(FVFun(v, t), aux e,
                add_let l (fun lv -> FMethVar(v, t, lv)) [] aux
                )
        in
    aux e;;


