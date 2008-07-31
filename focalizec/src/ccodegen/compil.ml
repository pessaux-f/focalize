open Saving;;


module Focalize =
  struct
    
    let compile _ unit = raise (UnknowUnit unit)
	
    let mk_context paths options =
      { cc_md5 = Digest.file Sys.executable_name;
	cc_paths = (Installation.install_lib_dir :: 
		    Filename.current_dir_name :: 
		    paths);
	cc_options = options;
	cc_fun = compile;
	cc_env = { scoping = Cscoping.empty } }
 	
    let translate ctx ast file =
      let dir = Filename.dirname file in
      let name = Filename.chop_extension (Filename.basename file) in
	  Cscoping.file ctx ast;
	save { d_unit = name;
	       d_dir = dir;
	       d_unit_md5 = Digest.file file;
	       d_comp_md5 = ctx.cc_md5;
	       d_env = ctx.cc_env }
	  
  end
    
    
