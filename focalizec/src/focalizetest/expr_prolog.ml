open Own_expr;;
open Own_basics;;

type minifoc_var =
  | FVInt of string (** An integer variable *)
  | FVHer of string * Own_types.typ;;  (** An Herbrand variable *)

type minifoc_arg =
  | FVar of minifoc_var (* A variable name *)
  | FInt of int    (** An integer constant *)
  | FConstruct of string * minifoc_arg list;; (** Appel à un constructeur *)

(* My expressions *)
type minifoc_expr =
  | FIfte of string * minifoc_expr * minifoc_expr
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
  |FBasic(s2, ml) ->
      "#" ^ s2 ^ "(" ^
            List.fold_right (fun e s -> dbg_string_minifoc_arg e
                     ^ "," ^ s) ml ""  ^ ")"
  | FVarloc(v, e1, e2) ->
      "let " ^ dbg_string_minifoc_var v ^ " = " ^
      dbg_string_minifoc_expr e1 ^ " in " ^
      dbg_string_minifoc_expr e2
  | FValue m -> dbg_string_minifoc_arg m
  | _ -> failwith "not string_of_minifoc_expr";;


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
  let rec aux (m : string) (s : string) (a : string) e =
    let aux_ = aux m s a in
    match e with
    | MIfte(e1, e2, e3) ->
        let v = Fresh_variable.new_prolog_var () in
        FVarloc(FVHer(v, Own_types.TAtom(None, "bool")), aux_ e1,
                FIfte(v,aux_ e2, aux_ e3))
    | MVarloc(b, (x, t_o), e1, e2) ->
        if b then
          raise (To_minifoc "Recursive definition inside function")
        else
          begin
            match t_o with
            | None -> raise (To_minifoc "Lack of type information in AST")
            | Some t ->
                FVarloc(FVHer(x, t),aux_ e1, aux_ e2)
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
          List.map (convert_pattern aux_) l in
        FVarloc(FVHer(v, t), aux_ e1,
                FMatch(v, l_pat))
    | MFun(_x, _t, _e) ->
        raise (To_minifoc "Can't convert high order function")
        (* function call *)
    | MMeth(Some c, f) ->
        FMeth(c, f, [])
    | MMeth(None, f) ->
        FMeth(focself, f, [])
    | MApp(MMeth(None, f), l) ->
        add_let l (fun lv -> FMeth(focself, f, lv)) [] aux_
    | MApp(MMeth(Some c, f), l) ->
        add_let l (fun lv -> FMeth(c, f, lv)) [] aux_
        (* ************* *)
        (*  global call  *)
    | MGlob_id(i) ->
        if Focalize_inter.is_constructor i then
          FValue(FConstruct(String.uncapitalize (ident_name i), []))
        else
          FBasic(ident_name i, [])
    | MApp(MGlob_id(i), l) ->
        let func = 
          fun lv ->
            if Focalize_inter.is_constructor i then
              FValue(FConstruct(String.uncapitalize (ident_name i), lv))
            else
              FBasic(ident_name i, lv) in
        add_let l func [] aux_
    | MApp(r, _l) -> raise (To_minifoc ("Application too sophisticated" ^
    dbg_string_myexpr r))
        in
    aux "r" "r" "r" e;;


