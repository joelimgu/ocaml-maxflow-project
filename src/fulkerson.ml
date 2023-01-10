open Gfile
open Tools
open Graph

(* get the diff between poids and cap of an arc *)
let get_diff (label: ('a*'a)) = match label with
| (cap, poids) -> poids - cap
;;

(* returns true if the arc is at its maximum capacity i.e if the diff of the label is zero *)
let diff_equals_zero = (function | (_,label) -> get_diff label != 0);;

let dfs filter graph (node_src: id) (node_dest: id) =
    (*create an empty graph that will be used as an accumulator of visited nodes in DFS *)
    let new_viewed_nodes = empty_graph in
    (* internal recursive function to avoid the user having to pass the accumulator as an argument *)
    let rec in_dfs viewed_nodes n =
        (* return the node if we are at the destination *)
        if n = node_dest then
            Some([n])
        else
        (* if we arent at the destination node add the actual node to the graph of viewed nodes *)
        let g = new_node viewed_nodes n in
        (* get the arcs exiting the node n*)
        let (out_arcs:(id * ('b*'b)) list) = out_arcs graph n in
        (* filter out the arcs that are unusable using the filter function passed bby the user *)
        let (available_nodes: ((id * ('b*'b)) list)) = List.filter filter out_arcs in
        (* get the nodes at the other end of the arcs *)
        let out_nodes = List.map (function | (id,_) -> id) available_nodes in
        (* filter out the nodes we've already visited to avoid loops *)
        let not_visited_nodes = List.filter (fun o_n -> not(node_exists viewed_nodes o_n)) out_nodes in
        (* continue the DFS for every remaining node *)
        match List.find_map (fun p -> (in_dfs g p)) not_visited_nodes with
        | None -> None
        | Some(l) -> Some(n::l)
    in
    (* call the inner function that does the actual DFS *)
    in_dfs new_viewed_nodes node_src
;;


let fulkerson graph start dest =
    (* generate a graph with 0 as the used capacity *)
    let (cap_graph: (int*int) graph) =
        gmap graph (function | poids -> (0,poids)) in
    (* find arcs in a path *)
    let rec get_arcs g path acc =
        match path with
        | a::b::rest -> (function | Some(x) ->  x::acc | None -> acc) (find_arc g a b)
        | _ -> acc in
    (* replace arcs in the path with the new arcs in the list *)
    let rec map_path (g: ('a*'a) graph) (path: id list) (new_arc_list: ('a*'a) list) =
        match (path,new_arc_list) with
        | (a::b::rest_path,l::rest_arc) -> new_arc (map_path g rest_path rest_arc) a b l
        | _ -> g in
    (* get min between a number and the arc diff *)
    let min_arc (min_acc: int) (label:(int*int)) =
        if (get_diff label) < min_acc
        then get_diff label
        else min_acc in
    (* inner function to avoid the user having to pass the accumulator as an argument *)
    let rec in_fulkerson (n:int) (acc: (int*int) graph) =
        (* find a path to the destination using DFS, if the path doesnt exist return an empty path *)
        let path =
        match (dfs diff_equals_zero acc start dest) with
           | None -> []
           | Some(path) -> path in
        (* if the path is empty the algorithm has finished so we return the max flow (n) *)
        match path with
        | [] -> n
        | path ->
            (* find the arcs associated with the path *)
            let (arcs_in_path: (int*int) list) =
                get_arcs acc path [] in
            (* value to add to every arc *)
            let (min: 'int) =
                List.fold_left (min_arc) (get_diff (List.hd arcs_in_path)) arcs_in_path in
            (* create new arcs to reflect the new used capacity *)
            let new_arcs =
                List.map (function | (cap, poids) -> (cap+min,poids)) arcs_in_path in
            (* create a graph with the new arcs *)
            let updated_graph = map_path acc path new_arcs in
            (* call the function with the new graph *)
            in_fulkerson (n+min) updated_graph
    in
    (* call the inner function with the accumulators *)
    in_fulkerson 0 cap_graph
;;
