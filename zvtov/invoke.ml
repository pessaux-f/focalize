(*  Copyright 2004 INRIA  *)
(*  $Id: invoke.ml,v 1.15 2005-07-01 12:26:22 prevosto Exp $  *)

let zcmd = ref "zenon";;
let zopt = ref "-x coqbool -q -short -max-time 1m";;
let izopt = ref "-ifocal"
let set_tptp_option () = izopt:="-itptp"

let use_coqterm = ref false;;

let progress_level = ref 1;;
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
  let tmp_in = (file ^ "-tmp.coz") in
  let tmp_out = (file ^ "-tmp.v") in
  if Cache.find data tmp_out then begin
    Printf.eprintf "\x0D";
    copy_file tmp_out oc;
    if !progress_level >= 2 then Printf.eprintf "(cached)\n";
  end else begin
    let tmpoc = open_out_bin tmp_in in
    output_string tmpoc data;
    close_out tmpoc;
    let cmd =
      if !use_coqterm then
        Printf.sprintf "%s -p%d -ocoqterm %s %s %s >%s"
                       !zcmd (translate_progress !progress_level)
                       !izopt !zopt tmp_in tmp_out
      else
        Printf.sprintf "%s -p%d -ocoq %s %s %s >%s"
                       !zcmd (translate_progress !progress_level)
                       !izopt !zopt tmp_in tmp_out
    in
    let rc = Sys.command cmd in
    begin match rc with
    | 0 ->   (* OK *)
        copy_file tmp_out oc;
        Cache.add data tmp_out;
    | 255 -> (* interrupted *)
        Printf.eprintf "interrupt\n";
        Sys.remove tmp_in;
        Sys.remove tmp_out;
        exit (-1);
    | _ ->
      Printf.eprintf "%s:\n  proof failed\n" loc;
      flush stderr;
      output_string oc data;
    end;
    Sys.remove tmp_in;
    Sys.remove tmp_out;
  end;
;;

let atp_function = ref zenon_loc

let set_atp f = atp_function:=f

let atp proof loc oc = !atp_function proof loc oc

let zenon file species proof step data oc =
  let loc = Printf.sprintf "File %s, species %s\n  proof of %s%a"
                           file species proof print_step step
  in zenon_loc file ("lemma", "True") data loc oc;
;;

let zenon_version () =
  let tempfile = Filename.temp_file "zenon_version" ".txt" in
  let cmd = Printf.sprintf "%s -v >%s" !zcmd tempfile in
  let rc = Sys.command cmd in
  if rc = 0 then begin
    let ic = open_in tempfile in
    let result = try input_line ic with _ -> "" in
    close_in ic;
    (try Sys.remove tempfile with _ -> ());
    result
  end else ""
;;

let signature () =
  Printf.sprintf "%s %s %s\n%s\n%s\n" !zcmd !izopt !zopt (zenon_version ())
                 (if !use_coqterm then "term" else "script")
;;
