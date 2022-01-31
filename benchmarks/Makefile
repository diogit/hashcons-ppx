# Nome de ficheiros
BEFORE = Before/tree
AFTER = After/hctree

# Comandos
clean = rm -r -f *.native $(RESULT)*
before = ocamlopt -o $(BEFORE) unix.cmxa $(BEFORE).ml  && ./$(BEFORE)
after = ocamlopt -o $(AFTER) unix.cmxa $(AFTER).ml  && ./$(AFTER)

all:
	$(before)
	$(after)

tree: 
	$(before)
hctree:
	$(after)


