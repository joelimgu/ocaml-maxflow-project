open Gfile
open Tools
open Graph

val dfs: (int*int) graph -> id -> id -> id list option

val fulkerson: int graph -> id -> id -> int
(*val find_path: 'a graph -> 'a -> 'a -> 'a graph*)

(*val fulkerson: int graph -> id -> id -> int * int graph*)
