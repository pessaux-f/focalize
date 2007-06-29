let focal_version_number = 0.1 ;;

let focal_full_version = Printf.sprintf "%.2f %s" focal_version_number "alpha";;

let (get_verbose,
     set_verbose) =
  let verbose = ref false in
  ((fun () -> !verbose),
   (fun b -> verbose := b))
;;


let (get_downgrade,
     set_downgrade) =
  let downgrade = ref false in
  ((fun () -> !downgrade),
   (fun () -> downgrade := true))
;;


let (get_input_file_name,
     set_input_file_name) =
  let input_file_name = ref "-" in
  ((fun () -> !input_file_name),
   (fun fname ->
     if !input_file_name = "-" then input_file_name := fname
     else failwith "Input file name is already set."))
;;



let (get_output_file_name, set_output_file_name) =
  let output_file_name = ref "" in
  (fun () -> !output_file_name),
  (fun fname ->
    if !output_file_name = "" then output_file_name := fname
    else failwith "Input file name is already set.")
;;


let print_focal_version v =
  prerr_endline (Printf.sprintf "The Focal compiler, version %s" v) ;
  exit 0
;;

let print_focal_short_version () =
  print_focal_version (Printf.sprintf "%.2f" focal_version_number)
;;

let print_focal_full_version () = print_focal_version focal_full_version ;;
