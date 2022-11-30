open Gfile
open Tools
open Graph

let dfs graph node_src node_dest =
    let new_viewed_nodes = empty_graph in
    let rec in_dfs viewed_nodes n =
        if n=node_dest then Some([n]) else
        let g = new_node viewed_nodes n in
        let out_arcs = out_arcs graph n in
        let out_nodes = List.map (function | (id,y) -> id) out_arcs in
        let not_visited_nodes = List.filter (fun o_n -> not(node_exists viewed_nodes o_n)) out_nodes in
        match List.find_map (fun p -> (in_dfs g p)) not_visited_nodes with
        | None -> None
        | Some(l) -> Some(n::l)
    in
    in_dfs new_viewed_nodes node_src
;;



let fulkerson graph start dest =
    let cap_graph = gmap graph (function | (poids,id) -> ((0,poids), id)) in
    (* find arcs in a path *)
    let rec get_arcs g path acc = match path with
    | a::b::rest -> function | Some(x) -> (find_arc g a b)::acc | None -> acc
    | _ -> acc
    in
    (* replace arcs in the path with the new arcs in the list*)
    let rec map_path g path arc_list = match (path,arc_list) with
    | (a::b::rest_path,(l,id)::rest_arc) -> new_arc (map_path g rest_path rest_arc) a b l
    | _ -> g
    in
    (* get the diff between poids and cap of an arc *)
    let get_diff arc = match arc with
    | ((cap, poids),_) -> poids-cap
    in
    (*get min betwwen a number and the arc diff *)
    let min_arc min_acc arc = if (get_diff arc) < min_acc then get_diff arc else min_acc
    in
    let in_fulkerson n acc =
        let path = match dfs graph start dest with
           | None -> acc
           | Some(path) -> path
        in
        let arcs_in_path = get_arcs graph path []
        in
        let min = List.fold_left (min_arc) (List.hd arcs_in_path) arcs_in_path
        in
        let new_arcs = List.map (function | ((cap, poids),id) -> ((cap+min,poids), id)) arcs_in_path
        in
        ()
    in
    ()
;;

(*(int*(int*id)) <=> (int*int*id)*)
