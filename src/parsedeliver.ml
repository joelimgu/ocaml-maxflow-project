open Graph
open Printf
open Gfile
open Text



(* Ensure that the given node exists in the graph. If not, create it. 
 * (Necessary because the website we use to create online graphs does not generate correct files when some nodes have been deleted.) *)
let ensure graph id = if node_exists graph id then graph else new_node graph id

(* Reads a line with an arc. *)


(* Reads a comment or fail. *)
let read_comment graph line =
  try Scanf.sscanf line " %%" graph
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"

let from_file_deliver path =

  let infile = open_in path in

  (* Read all lines until end of file. 
   * n is the current node counter. *)
  let rec loop n graph =
    try


      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in
      
      let (n2, graph2) =
      


        (* Ignore empty lines *)
        if line = "" then (n, graph)

        (* The first character of a line determines its content : n or e. *)
        else 

          match line with
          | line when (Text.contains line "fabriquer") -> (n+1, 
                                                           new_node graph (int_of_string (List.nth (Text.split ~sep:" " line) 1)))
          | line when (Text.contains line "besoin") -> (n+1, 
                                                           new_node graph (int_of_string (List.nth (Text.split ~sep:" " line) 2)))
          | line when (Text.contains line "entre") -> (n, 
                                                           graph)
 
                                                           (* It should be a comment, otherwise we complain. *)
          | _ -> (n, read_comment graph line) 

      in



      let (n3, graph3) = 
      (* Ignore empty lines *)
      if line = "" then (n2, graph2)

       (* The first character of a line determines its content : n or e. *)
       else 
         match line with
         | line when (Text.contains line "fabriquer") -> (n2, 
                                                          new_arc graph2 0 (int_of_string (List.nth (Text.split ~sep:" " line) 1)) (int_of_string (List.nth (Text.split ~sep:" " line) 4)) )
         | line when (Text.contains line "besoin") -> (n2, 
                                                          new_arc graph2 (int_of_string (List.nth (Text.split ~sep:" " line) 2)) 99 (int_of_string (List.nth (Text.split ~sep:" " line) 6)))
         | line when (Text.contains line "entre") -> (n2, 
                                                          new_arc graph2 (int_of_string (List.nth (Text.split ~sep:" " line) 6)) (int_of_string (List.nth (Text.split ~sep:" " line) 10)) (int_of_string (List.nth (Text.split ~sep:" " line) 14)))

                                                          (* It should be a comment, otherwise we complain. *)
         | _ -> (n, read_comment graph line) 
       in
    
      
      loop n3 graph3
      

    with End_of_file -> graph (* Done *)
  in

  let graph_begin = empty_graph in

  let graph_begin_2 = new_node graph_begin 0 in
  let graph_begin_3 = new_node graph_begin_2 99 in

  let final_graph = loop 0 graph_begin_3 in

  close_in infile ;
  final_graph