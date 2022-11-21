open Graph

let clone_nodes gr = n_fold gr (fun gr2 n -> new_node gr2 n) empty_graph
;;


let gmap gr f = assert false

(* TODO : TRY TO FIND A WAY TO GET THE WEIGHT OF THE ARC IN ORDER TO REPLACE IT IN CASE IF ARC EXISTS *)
let add_arc gr n1 n2 weight =
  match (find_arc gr n1 n2) with
  | Some(x) -> new_arc gr n1 n2 (weight + x)
  | None -> new_arc gr n1 n2 weight
