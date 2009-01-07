(* *********************************************************************** *)
(* look_for:string -> in_str: string -> (int * int) option                 *)
(** {Descr}: Search from left to right, the indices where the string
    [look_for] was found in the string [in_str]. If the searched string is
    the empty string, then we consider that the search always fails.
    This method is pure brute force.
    If the string is found, then we return the start and stop positions in
    the string [in_str] where [look_for] was found.

    {b Rem}: Not exported outside this module.                             *)
(* *********************************************************************** *)
let str_search ~look_for ~in_str =
  if look_for = "" then None
  else
    (begin
    let m = String.length look_for in
    let n = String.length in_str in
    let found_index = ref None in
    let j = ref 0 in
    while !j <= n - m && !found_index = None do
      let i = ref 0 in
      while !i < m && look_for.[!i] = in_str.[!i + !j] do incr i done ;
      if !i >= m then found_index := Some (!j, (!j + m)) ;
      incr j
    done ;
    !found_index
    end)
;;



(** {b Descr}: If the string [look_for] was found in the string [in_str],
    returns the characters of [in_str] remaining after the position where
    [look_for] was found. *)
let get_text_after_matched_string ~look_for ~in_str =
  match str_search ~look_for ~in_str with
   | None -> None
   | Some (_, match_end) ->
       let rem_len = (String.length in_str) - match_end in
       let sub_str = String.sub in_str match_end rem_len in
       Some sub_str
;;
