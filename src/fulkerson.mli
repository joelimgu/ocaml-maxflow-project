open Gfile
open Tools
open Graph

(* DFS algorithm, returns a list of nodes for where the path goes though *)
val dfs: (Graph.id *('b*'b) -> bool) -> ('b*'b) graph -> id -> id -> id list option

(* returns the max capacity that is able to go thought the graph using the ford-fulkerson algorithm *)
val fulkerson: int graph -> id -> id -> int
