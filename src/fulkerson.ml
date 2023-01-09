open Gfile
open Tools
open Graph

(* get the diff between poids and cap of an arc *)
let get_diff (label: ('a*'a)) = match label with
| (cap, poids) -> poids - cap
;;

let dfs graph (node_src: id) (node_dest: id) =
    let new_viewed_nodes = empty_graph in
    let rec in_dfs viewed_nodes n =
        if n=node_dest then Some([n]) else
        let g = new_node viewed_nodes n in
        let (out_arcs:(id * ('a*'a)) list) = out_arcs graph n in
        let (available_nodes: ((id * ('a*'a)) list)) = List.filter (function | (_,label) -> get_diff label != 0) out_arcs in
        let out_nodes = List.map (function | (id,_) -> id) available_nodes in
        let not_visited_nodes = List.filter (fun o_n -> not(node_exists viewed_nodes o_n)) out_nodes in
        match List.find_map (fun p -> (in_dfs g p)) not_visited_nodes with
        | None -> None
        | Some(l) -> Some(n::l)
    in
    in_dfs new_viewed_nodes node_src
;;



let fulkerson graph start dest =
    (* create a graph with a capacity *)
(*    let (cap_graph: (int*int) graph) = gmap graph (function | (poids,_) -> (0,poids)) in*)
    let (cap_graph: (int*int) graph) = gmap graph (function | poids -> (0,poids)) in
    (* find arcs in a path *)
    let rec get_arcs g path acc = match path with
    | a::b::rest -> (function | Some(x) ->  x::acc | None -> acc) (find_arc g a b)
    | _ -> acc
    in
    (* replace arcs in the path with the new arcs in the list*)
    let rec map_path (g: ('a*'a) graph) (path: id list) (new_arc_list: ('a*'a) list) = match (path,new_arc_list) with
    | (a::b::rest_path,l::rest_arc) -> new_arc (map_path g rest_path rest_arc) a b l
    | _ -> g
    in
    (*get min between a number and the arc diff *)
    let min_arc (min_acc: int) (label:(int*int)) = if (get_diff label) < min_acc then get_diff label else min_acc
    in
    let rec in_fulkerson (n:int) (acc: (int*int) graph) =
        (*TODO filter graph before DFS *)
        let path = match (dfs acc start dest) with
           | None -> []
           | Some(path) -> path
        in
        let _ = List.map (fun n -> Printf.printf "path->%s" (string_of_int n)) path in
        match path with
        | [] -> n
        | path ->
            let (arcs_in_path: (int*int) list) = get_arcs acc path []
            in
            (* value to add to every arc *)
            let (min: 'int) = List.fold_left (min_arc) (get_diff (List.hd arcs_in_path)) arcs_in_path
            in
            let new_arcs = List.map (function | (cap, poids) -> (cap+min,poids)) arcs_in_path
            in
            let updated_graph = map_path acc path new_arcs
            in
            let () = Printf.printf "%i\n" n in
            in_fulkerson (n+min) updated_graph
    in
    in_fulkerson 0 cap_graph
;;
