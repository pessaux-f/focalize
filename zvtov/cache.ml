(*  Copyright 2004 INRIA  *)
(*  $Id: cache.ml,v 1.9 2009-04-03 15:50:36 doligez Exp $  *)

(* format of a cache file:
file ::= block block block
         ( "begin\x0A" data "proof\x0A" data "err\x0A" data "end\x0A" )*
data ::= block* "0\x0A" checksum "\x0A"
block ::= n:int "\x0A" (n bytes) "\x0A"
checksum = hex representation of md5 digest of data
*)

open Misc;;
open Printf;;

type reference = int;;

let header = "This is a proof cache file generated by zvtov."

let oldcachefile = ref "";;
let newcachefile = ref "";;
let table = (Hashtbl.create 97 : (string, (int * int)) Hashtbl.t);;

let read_const ic s =
  let l = String.length s in
  let buf = Bytes.create l in
  really_input ic buf 0 l;
  if (Bytes.to_string buf) <> s then raise Exit;
;;

let rec read_len ic accu =
  let c = input_byte ic in
  if c = 0x0A then accu
  else read_len ic (accu * 10 + c - 0x30)
;;

let read_block ic =
  let n = read_len ic 0 in
  if n > 0 then begin
    let buf = Bytes.create n in
    really_input ic buf 0 n;
    read_const ic "\x0A";
    Some (Bytes.to_string buf)
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
  let buf = Bytes.create 32 in
  really_input ic buf 0 32;
  read_const ic "\x0A";
  Bytes.to_string buf
;;

let read_item ic =
  read_const ic "begin\x0A";
  skip_to_check ic;
  let key = read_check ic in
  read_const ic "proof\x0A";
  let offset1 = pos_in ic in
  skip_to_check ic;
  let _ = read_check ic in
  read_const ic "err\x0A";
  let offset2 = pos_in ic in
  skip_to_check ic;
  let _ = read_check ic in
  read_const ic "end\x0A";
  Hashtbl.add table key (offset1, offset2);
;;

let write_block oc data len =
  assert (String.length data >= len);
  if !with_cache then begin
    fprintf oc "%d\x0A" len;
    output oc (Bytes.of_string data) 0 len;
    output_string oc "\x0A";
  end
;;

let write_checksum oc chk = fprintf oc "0\x0A%s\x0A" (Digest.to_hex chk);;

let write_file oc file =
  let ic = open_in_bin file in
  let buflen = 4096 in
  let buf = Bytes.create buflen in
  let rec loop () =
    let n = input ic buf 0 buflen in
    if n = 0 then () else begin
      write_block oc (Bytes.to_string buf) n;
      loop ();
    end;
  in
  loop ();
  close_in ic;
;;

let init base version1 version2 =
  if !with_cache then begin
    oldcachefile := base ^ ".pfc";
    newcachefile := base ^ "-zvtmp.pfc";
    let oc = open_out_bin !newcachefile in
    write_block oc header (String.length header);
    write_block oc version1 (String.length version1);
    write_block oc version2 (String.length version2);
    close_out oc;
    begin try
      let ic = open_in_bin !oldcachefile in
      let hd = read_block ic in
      if hd <> Some header then raise Exit;
      let file_version1 = read_block ic in
      if file_version1 <> Some version1 then raise Exit;
      let file_version2 = read_block ic in
      if file_version2 <> Some version2 then raise Exit;
      while true do read_item ic done;
    with Exit | End_of_file | Sys_error _ -> ()
    end;
  end
;;

let close () =
  if !with_cache then begin
    (try Sys.remove !oldcachefile with Sys_error _ -> ());
    (try Sys.rename !newcachefile !oldcachefile with Sys_error _ -> ());
  end
;;

let add key file1 file2 =
  if !with_cache then begin
    let oc = open_out_gen [Open_wronly; Open_append; Open_creat; Open_binary]
                          0o666 !newcachefile
    in
    output_string oc "begin\x0A";
    write_block oc key (String.length key);
    write_checksum oc (Digest.string key);
    output_string oc "proof\x0A";
    write_file oc file1;
    write_checksum oc (Digest.file file1);
    output_string oc "err\x0A";
    write_file oc file2;
    write_checksum oc (Digest.file file2);
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

let find key destfile1 destfile2 =
  if !with_cache then begin
    try
      let hashkey = Digest.to_hex (Digest.string key) in
      let (offset1, offset2) = Hashtbl.find table hashkey in
      let ic = open_in_bin !oldcachefile in
      let oc1 = open_out_bin destfile1 in
      let oc2 = open_out_bin destfile2 in
      seek_in ic offset1;
      copy_data ic oc1;
      close_out oc1;
      let chk1 = read_check ic in
      seek_in ic offset2;
      copy_data ic oc2;
      close_out oc2;
      let chk2 = read_check ic in
      close_in ic;
      if chk1 <> (Digest.to_hex (Digest.file destfile1))
         || chk2 <> (Digest.to_hex (Digest.file destfile2))
      then false
      else (add key destfile1 destfile2; true)
    with Not_found | End_of_file | Sys_error _ -> false
  end else
    false
;;
