type 'a env =
    { scoping : 'a }

type 'a data =
    { d_unit : string;
      d_dir : string;
      d_unit_md5 : Digest.t;
      d_comp_md5 : Digest.t;
      d_env : 'a env }
      
type opt =
    Wall
  | Redef
  | Auto_use
  | Verbose
      
type 'a context =
    { cc_md5 : Digest.t;
      cc_paths : string list;
      cc_options : opt list;
      cc_fun : 'a context -> string -> 'a data;
	cc_env : 'a env } 
      
let save d =
  let out = open_out_bin (Filename.concat d.d_dir (d.d_unit^".co")) in
  output_value out d;
  close_out out
    
exception UnknowUnit of string
exception UnitConflict of string list
    
let load ctx unit =
  (* Checking all possibles files matching the given one. *)
  let rec search = function
      [] -> []
    | h::t -> 
	if Sys.file_exists (Filename.concat h (unit^".foc")) then
	  h :: (search t)
	else
	  (search t)
  in
  (* Check if there is exactly one candidate. *)
  begin match search ctx.cc_paths with
    [] -> raise (UnknowUnit unit)
  | [dir] ->
      (* Nominal case. *)
      let src = Filename.concat dir (unit^".foc") in
      let data_file = Filename.concat dir (unit^".co") in
      if Sys.file_exists data_file then
	let input = open_in_bin data_file in
	let data = input_value input in
	close_in input;
	(* Verifying the authenticity of informations (according to the *)
	(* source file and compiler binaries. *)
	let src_md5 = Digest.file src in
	(* If the source or the compiler changed, we recompile it. *)
	if ((src_md5 <> data.d_unit_md5)
	  || (ctx.cc_md5 <> data.d_comp_md5)) then
	  begin
	    let data = ctx.cc_fun ctx src in
	    save data
	  end;
	data
      else
	let data = ctx.cc_fun ctx src in
	save data;
	data
  | l -> 
      let candidates = List.map 
	  (fun x -> 
	    Filename.concat x (unit^".foc")) l in
      raise (UnitConflict candidates)
  end
    
