{
open Lexing ;;

type documentation_tag =
  | DT_Title of string
  | DT_Author of string
  | DT_Description of string
  | DT_MathMl of string
  | DT_LaTeX of string
  | DT_None of string
;;

let initial_string_buffer = String.create 256;;
let string_buff = ref initial_string_buffer
and string_index = ref 0
;;

let store_string_char c =
  if !string_index >= String.length (!string_buff) then begin
    let new_buff = String.create (String.length (!string_buff) * 2) in
      String.blit (!string_buff) 0
                  new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
  end;
  String.unsafe_set (!string_buff) (!string_index) c;
  incr string_index
;;

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0
;;
let get_stored_string () =
  let s = String.sub (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer ;
  s
;;

}


(** ASCII 010 is newline or ['\n']. *)
let newline = '\010'
(** ASCII 32 is space, ASCII 9 is tab, ASCII 12 is CTRL-L *)
let blank = [ '\032' '\009' '\012' ]


rule start = parse
  | (blank | newline) +
      { start lexbuf }
  | "@title "
     {
      reset_string_buffer () ;
      tagged_text lexbuf ;
      DT_Title (get_stored_string ())
    }
  | "@author "
     {
      reset_string_buffer () ;
      tagged_text lexbuf ;
      DT_Author (get_stored_string ())
    }
  | "@description "
     {
      reset_string_buffer () ;
      tagged_text lexbuf ;
      DT_Description (get_stored_string ())
    }
  | "@mathml "
      {
       reset_string_buffer () ;
       tagged_text lexbuf ;
       DT_MathMl (get_stored_string ())
     }
  | "@latex "
      {
       reset_string_buffer () ;
       tagged_text lexbuf ;
       DT_LaTeX (get_stored_string ())
     }
  | eof
      { DT_None "" }
  | _
      {
       reset_string_buffer () ;
       store_string_char (Lexing.lexeme_char lexbuf 0) ;
       untagged_text lexbuf ;
       DT_None (get_stored_string ())
     }


and tagged_text = parse
  | newline               { () }
  | eof                   { () }
  | _
      { store_string_char (Lexing.lexeme_char lexbuf 0) ;
	tagged_text lexbuf }


and untagged_text = parse
  | eof                   { () }
  | _
      { store_string_char (Lexing.lexeme_char lexbuf 0) ;
	tagged_text lexbuf }
