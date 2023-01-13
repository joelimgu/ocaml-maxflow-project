# Project

This project realized by Joel Imbergamo Guasch and Killian Gonet implements the Ford-Fulkerson algorithm in OCaml and apply it in a basic application of delivery system between factories and cities. 

# Installation

- Install the module "ocamlfind" : ```opam install ocamlfind```
- Install the module "Text" : ```opam install text```

# Using

- To launch the test, launch ```make demo``` in a terminal. By default, the file graphs/graph2 is used in the test, but you can make your own by using the `delivery_template.example` (in French, sorry :)).
- If you want to change the path of the executed file, you need to go to the Makefile and modify the first argument of ./ftestnative to your path. 

# Warnings

- The name of a city or of a factory **cannot** be 0 or 99 (reserved to calculate the max flow of the graph).