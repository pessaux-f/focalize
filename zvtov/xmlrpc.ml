(*  Copyright 2005 MPI  *)
(*  $Id: xmlrpc.ml,v 1.4 2008-12-27 19:05:03 weis Exp $  *)

(* call external provers through the MathServe server. works only if
   the focalize file has been compiled with the -tptp option.
*)
open Pxp_document
open Pxp_core_types
open Http_client

(* default server is the local machine. *)
let mathserve_server = ref "http://localhost:12345"

let _ = Options.register_option "-server" (Arg.Set_string mathserve_server)
          "sets the MathServe server that will be used"

(* default request. *)
let request = ref "proveProblemOpt"

let _ = Options.register_option  "-request" (Arg.Set_string request)
          "sets the request name for MathServe"

(* default time given to the server to handle request. *)
let time = ref 60

let _ = Options.register_option "-time" (Arg.Set_int time)
          "time (seconds) given to the server to complete each request"

let set_http_client_opt () =
  let http_client_opt = XmlRPCNet.handler#get_options in
  let my_settings =
    { http_client_opt with
        synchronization = Sync; (* do not allow multiple simultaneous requests.
                                   Otherwise, may receive proofs for the wrong
                                   lemma. *)
        connection_timeout = float_of_int (!time + 5);
        (* 5 secs for communication is completely arbitrary... *)
        verbose_status = !Misc.progress_level >= 2;
        verbose_request_contents = !Misc.progress_level >=2;
        verbose_response_contents = !Misc.progress_level >=2
    }
  in XmlRPCNet.handler#set_options my_settings

type rpc_answer =
  | CoqProof of string
  | ProofFound of string
  | NoProof of string

let get_prover_name res_node =
  try
    (match res_node#attribute "rdf:ID" with
         Value s ->
           let i = String.index s '_' in
             String.sub s 0 i
       | _ -> "unknown prover")
  with Not_found -> "unknown prover"

let find_proof prover_name proof_node =
  let oracle = ProofFound ("proof given by " ^ prover_name)
  in
    try
      let formal_proof = proof_node#nth_node 0 in
      let proof_text =
        ((formal_proof#nth_node 0)#nth_node 0)#data
      in
        (match formal_proof#node_type with
             T_element "mw:CoqProofScript" ->
               CoqProof proof_text
           | T_element "mw:CoqProofTerm" ->
               CoqProof proof_text
           | _ -> oracle
               (* Could also extract the tstp proof as a comment,
                  but there's already enough things in the Coq file.
               *)
        )
    with Failure _ -> oracle

let status = ["Unsatisfiable", "Theorem";
              "Theorem", "Theorem";
              "Satisfiable", "Sat";
              "Tautology", "Theorem";
              "TautologousConclusion", "Theorem";
              "Equivalent", "Theorem";
              "ContradictoryAxioms", "Theorem";
              "NoConsequence", "Sat";
              "CounterSatisfiable", "Sat";
              "CounterTheorem", "Sat";
              "Timeout" , "Timeout" ]

let strip s =
  let i = String.index s '#' in
  let l = String.length s in
    String.sub s (i+1) (l - i - 1)

let examine_status prover_name status_node =
  try
    (match status_node#attribute "rdf:resource" with
         Value s ->
           let s = strip s in
             (match List.assoc s status with
                | "Theorem" ->
                    (try
                       find_proof prover_name (status_node#next_node)
                     with Failure s -> ProofFound prover_name)
                | "Timeout" ->
                    NoProof (prover_name ^ ": Time out")
                | "Sat" ->
                    NoProof (prover_name ^ ": STATEMENT IS FALSE")
                | "Failed" ->
                    NoProof (prover_name ^ ": failed")
                | _ -> assert false)
       | _ -> NoProof "unable to understand prover status")
  with Not_found -> NoProof "unable to find prover status"


let parse_answer answer =
  try
    let source = Pxp_yacc.from_string answer in
    let doc = Pxp_yacc.parse_wfdocument_entity Pxp_yacc.default_config
                source Pxp_yacc.default_spec
    in
    let root = doc#root in
    let _ = Pxp_document.iter_tree
              ~pre:(fun x -> Pxp_document.strip_whitespace
                      ~left:`Strip_seq ~right:`Strip_seq x)
              root
    in
    let prover_ans = root#nth_node 0 in
      match prover_ans#node_type with
          T_element "mw:Failure" ->
            let msg = (prover_ans#nth_node 0)#data in
              NoProof ("prover failed: " ^ msg)
        | T_element "mw:FoAtpResult"
        | T_element "mw:FOScriptResult"
          ->
            let prover_name = get_prover_name prover_ans in
            let status_node = prover_ans#nth_node 1 in
              examine_status prover_name status_node
        | _ ->
            NoProof "unable to understand prover answer"
  with
      Pxp_types.At(_,exn) ->
        NoProof ("unable to parse the answer from MathServe: " ^
                 (Printexc.to_string exn))
    | Not_found ->
        NoProof ("unable to parse the answer from MathServe: " ^
                 "got unexpected xml document.")

let handle_fault oc fault =
  Printf.fprintf oc "Error %d: %s"
    fault.XmlRPCTypes.code fault.XmlRPCTypes.desc

(* invoke a new remote call for the given problem. *)
let remote_call file (statement, name) problem loc oc =
  set_http_client_opt ();
  if !Misc.progress_level >= 2 then
    Printf.eprintf "%s: tptp problem is:\n%s\n%!" loc name;
  let request = new XmlRPCClient.remote !mathserve_server !request in
    try
      let answer = request#call
                     [ `String problem; XmlRPCTypes.xr_of_int !time] in
        (match answer with
             `String answer ->
               (match parse_answer answer with
                    CoqProof s ->
                      Printf.eprintf "%s: got a coq proof\n%s\n%!" loc
                      (String.sub s 0 (min 80 (String.length s)))
                      ;
                      Printf.fprintf oc "%s\n" s
                      (* WARNING: implicitely the proof contains also the
                         statement of the lemma. *)
                  | ProofFound info ->
                      Printf.eprintf "%s: got a proof\n%!" loc;
                      Printf.fprintf oc
                      "(* %s *)\nLemma %s:\n  %s.\n  \
                       eapply dontwanttoproveit.\n  Save.\n"
                      info name statement
                  | NoProof info ->
                      Printf.eprintf "%s:\n  proof failed:\n%s\n%!" loc info;
                      output_string oc problem
               )
           | _ ->
               Printf.eprintf "%s: Answer from MathServe not understood\n%!" loc;
               output_string oc problem)
    with
        XmlRPCClient.Request_failed s ->
          Printf.eprintf "%s: rpc request failed\n%s\n%!" loc s;
          output_string oc problem
      | XmlRPCClient.Request_fault fault ->
          Printf.eprintf "%s: proof failed\n%a%!" loc handle_fault fault;
          output_string oc problem
      | XmlRPCTypes.Bad_type ->
          Format.eprintf
          "%s: rpc request failed\nBad XML-RPC answer from server\n%!" loc;
          output_string oc problem

let _ = Options.register_option "-mathserve"
          (Arg.Unit (fun () -> Invoke.set_atp remote_call))
          "use a MathServe server to do the proofs"
