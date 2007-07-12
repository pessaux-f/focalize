exception Cant_access_file of Parsetree.fname
exception Corrupted_fo of Parsetree.fname

val lib_paths : string list ref
val open_in_from_lib_paths : Parsetree.fname -> in_channel
val fo_filename_from_module_name : string -> string
type magic
val fo_magic : magic
val check_magic : in_channel -> magic -> bool
val write_magic : out_channel -> magic -> unit
