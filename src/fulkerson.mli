open Gfile
open Tools
open Graph

val find_path: 'a graph -> 'a -> 'a -> 'a graph

val fulkerson: int graph -> id -> id -> int * int graph