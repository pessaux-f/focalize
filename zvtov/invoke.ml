(*  Copyright 2004 INRIA  *)
(*  $Id: invoke.ml,v 1.31 2009-08-24 12:15:00 doligez Exp $  *)

open Misc;;
open Printf;;

let set_tptp_option () = add_opt := ("-itptp" :: !add_opt);;

let translate_progress x =
  match x with
  | 0 | 1 -> x
  | 2 -> 1
  | _ -> assert false
;;

let copy_file name oc =
  let ic = open_in_bin name in
  let buflen = 8192 in
  let buf = String.create buflen in
  let rec loop () =
    let n = input ic buf 0 buflen in
    if n = 0 then () else begin
      output oc buf 0 n;
      loop ();
    end;
  in
  loop ();
  close_in ic;
;;

let file_size name =
  let ic = open_in_bin name in
  let result = in_channel_length ic in
  close_in ic;
  result
;;

let rec print_path () p =
  match p with
  | [] -> assert false
  | [s] -> Printf.sprintf "%d" s
  | h::t -> Printf.sprintf "%d.%a" h print_path t
;;

let print_step () p =
  match p with
  | [] -> ""
  | _ -> ", step " ^ (print_path () p)
;;

type pos = Line of int | Nowhere;;

let get_pos loc =
  try Scanf.sscanf loc "File \"%_[^\"]\", line %d," (fun l -> Line l)
  with Scanf.Scan_failure _ | End_of_file -> Nowhere
;;

let output_placeholder oc data =
  let lexbuf = Lexing.from_string data in
  let name = ref "" in
  let end_head = ref (-1) in
  let theorem_end = ref (-1) in
  let theorem = ref (-1) in
  let tail = ref (-1) in
  let rec loop () =
    match Lexer_coq.coqtoken lexbuf with
    | Parser_coq.BEGINPROOF ->
        end_head := Lexing.lexeme_end lexbuf;
        loop ();
    | Parser_coq.BEGINNAME n ->
        name := n;
        end_head := Lexing.lexeme_end lexbuf;
        loop ();
    | Parser_coq.BEGINHEADER ->
        end_head := Lexing.lexeme_end lexbuf;
        loop ();
    | Parser_coq.PARAMETER ->
        if !theorem_end < 0 then theorem_end := Lexing.lexeme_start lexbuf;
        loop ();
    | Parser_coq.DEFINITION ->
        if !theorem_end < 0 then theorem_end := Lexing.lexeme_start lexbuf;
        loop ();
    | Parser_coq.THEOREM ->
        theorem := Lexing.lexeme_start lexbuf;
        theorem_end := -1;
        loop ();
    | Parser_coq.ENDPROOF ->
        if !theorem_end < 0 then theorem_end := Lexing.lexeme_start lexbuf;
        tail := Lexing.lexeme_start lexbuf;
        loop ();
    | Parser_coq.EOF ->
        ()
    | _ -> loop ();
  in
  loop ();
  let len = String.length data in
  if !tail < 0 then tail := len;
  assert (!theorem_end >= 0);
  if !end_head < 0 then end_head := 0;
  if !theorem >= 0 then
    output_string oc (String.sub data !theorem (!tail - !theorem))
  else begin
    output_string oc "Theorem ";
    output_string oc !name;
    output_string oc " :\n";
    output_string oc (String.sub data !end_head (!theorem_end - !end_head));
    output_string oc ".\n";
  end;
  output_string oc "Proof. TO_BE_DONE_MANUALLY.\n";
;;

let lemma_number = ref 0;;

let zenon_loc file (_: string * string) data loc oc =
  incr lemma_number;
  begin match !progress_level with
  | 0 -> ()
  | 1 ->
      begin match get_pos loc with
      | Line l -> Printf.eprintf "%d " l;
      | Nowhere -> Printf.eprintf "#%d " !lemma_number;
      end;
      flush stderr;
  | 2 ->
      Printf.eprintf "## %s\n" loc;
      flush stderr;
  | _ -> assert false;
  end;
  let tmp_in = (file ^ "-zvtmp.coz") in
  let tmp_out = (file ^ "-zvtmp.v") in
  let tmp_err = (file ^ "-zvtmp.err") in
  let cleanup () =
    try_remove tmp_in;
    try_remove tmp_out;
    try_remove tmp_err;
  in
  if Cache.find data tmp_out tmp_err then begin
    begin try
      Printf.eprintf " (cached)\x0D";
      copy_file tmp_out oc;
      if file_size tmp_err > 0 then
        begin
          Printf.eprintf "%s\n" loc;
          copy_file tmp_err stderr;
        end;
      if !progress_level >= 2 then Printf.eprintf "\n";
    with x -> cleanup (); raise x
    end;
    cleanup ();
  end else begin
    let tmpoc = open_out_bin tmp_in in
    output_string tmpoc data;
    close_out tmpoc;
    let cmd =
      if !use_coqterm then
        Printf.sprintf "%s -p%d -ocoqterm -x %s %s %s -wout %s %s >%s"
                       !zcmd (translate_progress !progress_level)
                       !focal_ext
                       !zopt (String.concat " " (List.rev !add_opt))
                       tmp_err tmp_in tmp_out
      else
        Printf.sprintf "%s -p%d -ocoq -x %s %s %s -wout %s %s >%s"
                       !zcmd (translate_progress !progress_level)
                       !focal_ext
                       !zopt (String.concat " " (List.rev !add_opt))
                       tmp_err tmp_in tmp_out
    in
    if !verbose then Printf.eprintf "%s\n%!" cmd;
    try_remove tmp_err;
    let rc = Sys.command cmd in
    begin match rc with
    | 0 ->   (* OK *)
        copy_file tmp_out oc;

        if Sys.file_exists tmp_err then begin
          Printf.eprintf "%s\n" loc;
          copy_file tmp_err stderr;
        end else begin
          close_out (open_out tmp_err);
        end;
        Cache.add data tmp_out tmp_err;
    | 255 -> (* interrupted *)
        cleanup ();
        raise Sys.Break;
    | _ ->
      Printf.eprintf "%s:\n" loc;
      if Sys.file_exists tmp_err then copy_file tmp_err stderr;
      Printf.eprintf "### proof failed\n";
      flush stderr;
      if !stop_on_failure then begin
        cleanup ();
        raise Zenon_failed;
      end;
      output_placeholder oc data;
    end;
    cleanup ();
  end;
;;

let atp_function = ref zenon_loc

let set_atp f = atp_function:=f

let atp proof loc oc = !atp_function proof loc oc

let zenon_version () =
  let tempfile = Filename.temp_file "zvtov_version" ".txt" in
  let cmd1 = Printf.sprintf "%s -versions >%s" !zcmd tempfile in
  let rc1 = Sys.command cmd1 in
  if rc1 = 0 then begin
    let b = Buffer.create 1000 in
    let ic = open_in tempfile in
    let rec loop () =
      Buffer.add_string b (input_line ic);
      Buffer.add_char b '\n';
      loop ();
    in
    try loop (); with _ -> ();
    close_in ic;
    try_remove tempfile;
    Buffer.contents b
  end else ""
;;

let signature () =
  let aopt = String.concat " " (List.rev !add_opt) in
  Printf.sprintf "%s -x %s %s %s\n%s\n%s\n" !zcmd !focal_ext
                 !zopt aopt (zenon_version ())
                 (if !use_coqterm then "term" else "script")
;;
