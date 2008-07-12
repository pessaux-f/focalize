let pcm_phrase_location = function _ -> None;;

let pcm_phrase_is_open = function
    Infer.PCM_open _ -> true
  | _ -> false
;;

let pcm_phrase_open_name = function
    Infer.PCM_open (_, s) -> s
  | _ -> assert false
;;


let pcm_phrase_is_expr = function
    Infer.PCM_expr _ -> true
  | _ -> false
;;

let pcm_phrase_expr_value = function
    Infer.PCM_expr e -> e
  | _ -> assert false
;;
