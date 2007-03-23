let focal_version_number = 0.1;;
let focal_full_version = Printf.sprintf "%.2f %s" focal_version_number "alpha";;
let get_verbose, set_verbose =
  let verbose = ref false in
  (fun () -> !verbose),
  (fun b -> verbose := b)
;;
