(*  Copyright 2004 INRIA  *)
(*  $Id: invoke.ml,v 1.4 2004-06-02 17:08:10 doligez Exp $  *)

let zcmd = ref "zenon";;
let zopt = ref "-x coqbool -ifocal -ocoqterm7 -q -short -max-time 1m";;

let progress_flag = ref false;;

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

let zenon_loc file data loc oc =
  if !progress_flag then begin
    Printf.eprintf "## %s\n" loc;
    flush stderr;
  end;
  let tmp_in = (file ^ "-tmp.coz") in
  let tmp_out = (file ^ "-tmp.v") in
  let tmpoc = open_out_bin tmp_in in
  output_string tmpoc data;
  close_out tmpoc;
  let cmd = Printf.sprintf "%s %s %s >%s" !zcmd !zopt tmp_in tmp_out in
  let rc = Sys.command cmd in
  if rc = 0 then begin
    copy_file tmp_out oc;
  end else begin
    Printf.eprintf "%s:\n  proof failed\n" loc;
    flush stderr;
    output_string oc data;
  end;
  Sys.remove tmp_in;
  Sys.remove tmp_out;
;;

let zenon file species proof step data oc =
  let loc = Printf.sprintf "File %s, species %s\n  proof of %s%a"
                           file species proof print_step step
  in zenon_loc file data loc oc;
;;
