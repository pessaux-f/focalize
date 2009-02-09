{
open Lexing ;;



(** {Rem} Not exported outside this module. *)
exception Directive_not_terminated ;;



type directive =
  | D_end
  | D_found of Parsetree.module_name
;;


let initial_string_buffer = String.create 256 ;;
let string_buff = ref initial_string_buffer
and string_index = ref 0
;;



let reset_string_buffer () =
  string_buff := initial_string_buffer ;
  string_index := 0
;;



let store_string_char c =
  if !string_index >= String.length !string_buff then begin
    let new_buff = String.create (String.length !string_buff * 2) in
      String.blit !string_buff 0
                  new_buff 0 (String.length !string_buff);
      string_buff := new_buff
  end;
  String.unsafe_set !string_buff !string_index c;
  incr string_index
;;



let get_stored_string () =
  let s = String.sub !string_buff 0 !string_index in
  string_buff := initial_string_buffer;
  s
;;

}

let whites = [ ' ' '\t' '\n' ]*


rule start = parse
  | "open" whites "\""  {
    reset_string_buffer () ;
    try
      lex_directive lexbuf ;
      let comp_unit = get_stored_string () in
      D_found comp_unit
    with Directive_not_terminated -> D_end }
  | "use" whites "\""   {
    reset_string_buffer () ;
    try
      lex_directive lexbuf ;
      let comp_unit = get_stored_string () in
      D_found comp_unit
    with Directive_not_terminated -> D_end }
  | eof       { D_end }
  | _         { start lexbuf }


and lex_directive = parse
  | eof {
    (* The directive is not terminated. *)
    raise Directive_not_terminated }
  | "\"" whites ";;" { () }
  | _ { store_string_char (Lexing.lexeme_char lexbuf 0) ; 
	lex_directive lexbuf }
