
build:
	@echo "\n==== COMPILING ====\n"
	ocamlbuild ftest.native -use-ocamlfind

format:
	ocp-indent --inplace src/*

edit:
	code . -n

demo1: build
	@echo "\n==== EXECUTING ====\n"
	./ftest.native graphs/graph1 0 5 outfile
	@echo "\n==== RESULT ==== (content of outfile) \n"
	@cat outfile

demo2: build
	@echo "\n==== EXECUTING ====\n"
	./ftest.native graphs/graph2 1 2 outfile
	@echo "\n==== RESULT ==== (content of outfile) \n"
	@cat outfile

clean:
	-rm -rf _build/
	-rm ftest.native
