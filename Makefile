# Nome de ficheiros
AST = ast
NATIVE = hashcons_ppx
FILE = Tests/test_term
DUMP = AST
RESULT = Results/result

# Comandos
clean = rm -r -f _build *.native $(RESULT)*
native = ocamlbuild -package compiler-libs.common  $(NATIVE).native 
build = ocamlfind ppx_tools/rewriter ./$(NATIVE).native  $(FILE).ml > $(RESULT).ml
dump = ocamlfind ppx_tools/dumpast -loc_discard $(AST).ml > $(AST)$(DUMP).ml
exec = ocamlc unix.cma -o $(RESULT) $(RESULT).ml  && ./$(RESULT)
show = cat $(RESULT).ml

all:
	$(clean) 
	$(native)  	
	$(build)
	$(exec)

native: 
	$(native)
build: 
	$(build)
dump: 
	$(dump)
exec: 
	$(exec)
show: 
	$(show)
clean:
	$(clean)


