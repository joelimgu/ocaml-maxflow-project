open Gfile
open Tools
open Graph
open Fulkerson

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

  (* Open file *)
  let graph = from_file infile in

  (* Rewrite the graph that has been read. *)

  let graph2 = gmap graph (int_of_string) in
  let graph3 = add_arc graph2 1 2 22 in

  let graphintint = gmap graph2 (function | value -> (0,value)) in

  (* TODO remap string to int *)
  let () = write_file outfile (gmap graph3 string_of_int) in

  let () = match (dfs graphintint 0 5) with
  | None -> Printf.printf "Unknown line:\n%s\n%!" "None (path not found)"
  | Some(p) -> List.map (fun n -> Printf.printf "path->%s" (string_of_int n)) p; ()
  in

  let return_test_fulkerson = fulkerson graph2 0 5 in
  let () = Printf.printf "DFS test: %i" return_test_fulkerson in

(*  let () = Printf.printf "%i" return_test_dfs in*)

(*   let o: int out_arcs = [] in*)
  let () = export graph "test" in

  ()

