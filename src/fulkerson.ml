open Gfile
open Tools
open Graph

let dfs graph node_src node_dest =
    let new_viewed_nodes = empty_graph in
    let rec in_dfs viewed_nodes n =
        let g = new_node viewed_nodes n in
        let out_arcs = out_arcs graph n in
        let out_nodes = List.map (function | (id,y) -> id) out_arcs in
        let not_visited_nodes = List.filter (fun o_n -> not(node_exists viewed_nodes o_n)) out_nodes in
        match (n,not_visited_nodes) with
        | (n,_) when n=node_dest -> Some([n])
        | (_,[]) -> None
        | _ -> let path = match List.find_map (fun p -> (in_dfs g p)) not_visited_nodes with
            | None -> None
            | Some(l) -> Some(n::l)
            in
            path
    in
    in_dfs new_viewed_nodes node_src
;;



(*;;*)
(*let find_path graph start dest =*)
(*  (* Transformer le graph avec des arcs tuple *)*)
(*  let graphTuple = gmap graph (fun arc -> match arc with*)
(*    | (id, p) -> (id, 0, p)*)
(*    ) in*)
(*(*parcours en porfondeur*)*)
(*    ()*)

    ()


let fulkerson graph start dest = assert false;;
