open Parsetree
open Infer
open Format

type context {
    c_file : string
  } 


let compile (file, l) =
  let dir = Filename.dirname file in
  let base = Filename.chop_extension (Filename.basename file) in
  let header = Filename.concat dir (base^".h") in
  let source = Filename.concat dir (base^".c") in
  let header_out = open_out_bin header in
  let source_out = open_out_bin source in
  try
    let header_ppf = formatter_of_out_channel header_out in
    let source_ppf = formatter_of_out_channel source_out in
    let write_intro ppf =
      fprintf ppf "@[<hov 3>/*@ This@ file@ is@ automaticaly@ generated.@ Modify@ it@ at@ you@ own@ risks.@ */@]@." in
    write_intro header_ppf;
    write_intro source_ppf;
    let ctx = { c_file = file } in
    compile_phrase ctx 
    close_out header_out;
    close_out source_out
  with
    anything ->
      begin
	close_out header_out;
	close_out source_out;
	raise anything
      end
