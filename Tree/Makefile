# Nome de ficheiros
FILE = before
RESULT = Results/result

# Comandos
clean = rm -r -f _build *.native $(RESULT)* $(DUMP).ml
native = ocamlbuild -package compiler-libs.common  $(NATIVE).native 
build = ocamlfind ppx_tools/rewriter ./$(NATIVE).native  $(FILE).ml > $(RESULT).ml
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
exec: 
	$(exec)
show: 
	$(show)
clean:
	$(clean)


