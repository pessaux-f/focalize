(*  Copyright 2004 INRIA  *)
(*  $Id: cache.ml,v 1.1 2004-10-11 16:07:39 doligez Exp $  *)

(* format of a cache file:
file ::= ( "begin\x0A" data "proof\x0A" data "end\x0A" )*
data ::= block* "0\x0A" checksum "\x0A"
block ::= n:int "\x0A" (n bytes) "\x0A"
checksum = hex representation of md5 digest of data
*)

open Printf;;

type reference = int;;

let active = ref true;;

let oldcachefile = ref "";;
let newcachefile = ref "";;
let table = (Hashtbl.create 97 : (string, int) Hashtbl.t);;

let read_const ic s =
  let l = String.length s in
  let buf = String.create l in
  really_input ic buf 0 l;
  if buf <> s then raise Exit;
;;

let rec read_len ic accu =
  let c = input_byte ic in
  if c = 0x0A then accu
  else read_len ic (accu * 10 + c - 0x30)
;;

let read_block ic =
  let n = read_len ic 0 in
  if n > 0 then begin
    let buf = String.create n in
    really_input ic buf 0 n;
    read_const ic "\x0A";
    Some buf
  end else begin
    None
  end
;;

let rec skip_to_check ic =
  let n = read_len ic 0 in
  if n > 0 then begin
    seek_in ic (pos_in ic + n);
    read_const ic "\x0A";
    skip_to_check ic;
  end
;;

let rec read_check ic =
  let buf = String.create 32 in
  really_input ic buf 0 32;
  read_const ic "\x0A";
  buf
;;

let read_item ic =
  read_const ic "begin\x0A";
  skip_to_check ic;
  let key = read_check ic in
  read_const ic "proof\x0A";
  let offset = pos_in ic in
  skip_to_check ic;
  let _ = read_check ic in
  read_const ic "end\x0A";
  Hashtbl.add table key offset;
;;

let init base =
  if !active then begin
    oldcachefile := base ^ ".pfc";
    newcachefile := base ^ ".pfctmp";
    (try Sys.remove !newcachefile with Sys_error _ -> ());
    begin try
      let ic = open_in_bin !oldcachefile in
      while true do read_item ic done;
    with Exit | End_of_file | Sys_error _ -> ()
    end
  end
;;

let close () =
  if !active then begin
    (try Sys.remove !oldcachefile with Sys_error _ -> ());
    (try Sys.rename !newcachefile !oldcachefile with Sys_error _ -> ());
  end
;;

let write_block oc data len =
  assert (String.length data >= len);
  if !active then begin
    fprintf oc "%d\x0A" len;
    output oc data 0 len;
    output_string oc "\x0A";
  end
;;

let write_checksum oc chk = fprintf oc "0\x0A%s\x0A" (Digest.to_hex chk);;

let write_file oc file =
  let ic = open_in_bin file in
  let buflen = 4096 in
  let buf = String.create buflen in
  let rec loop () =
    let n = input ic buf 0 buflen in
    if n = 0 then () else begin
      write_block oc buf n;
      loop ();
    end;
  in
  loop ();
  close_in ic;
;;

let add key file =
  if !active then begin
    let oc = open_out_gen [Open_wronly; Open_append; Open_creat; Open_binary]
                          0o666 !newcachefile
    in
    output_string oc "begin\x0A";
    write_block oc key (String.length key);
    write_checksum oc (Digest.string key);
    output_string oc "proof\x0A";
    write_file oc file;
    write_checksum oc (Digest.file file);
    output_string oc "end\x0A";
    close_out oc;
  end
;;

let rec copy_data ic oc =
  match read_block ic with
  | None -> ()
  | Some s ->
      output_string oc s;
      copy_data ic oc;
;;

let find key destfile =
  if !active then begin
    try
      let hashkey = Digest.to_hex (Digest.string key) in
      let offset = Hashtbl.find table hashkey in
      let ic = open_in_bin !oldcachefile in
      let oc = open_out_bin destfile in
      seek_in ic offset;
      copy_data ic oc;
      close_out oc;
      let chk = read_check ic in
      close_in ic;
      if chk <> (Digest.to_hex (Digest.file destfile))
      then false
      else (add key destfile; true)
    with Not_found | End_of_file | Sys_error _ -> false
  end else
    false
;;
