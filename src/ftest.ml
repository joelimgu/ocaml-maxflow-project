open Gfile
open Tools
open Graph
open Fulkerson
open Parsedeliver

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in


(* TEST WITH DELIVERY SYSTEM *)
(*let valReturnDeliver = fulkerson_deliver infile in
Printf.printf "Le flot maximum de pièces dans cette configuration est de %d\n" valReturnDeliver ; *)
(* ------------- *)

(* TEST WITH DEFAULT GRAPH *)
let graph = from_file infile in
let graph2 = gmap graph (int_of_string) in
let return_test_fulkerson = fulkerson graph2 _source _sink in

Printf.printf "Le flot maximum est de %d\n" return_test_fulkerson ;
(* ----------------------- *)

  ()