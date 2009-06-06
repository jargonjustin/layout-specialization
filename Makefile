BUILDFLAGS=-cflag -g -lflag -g -libs unix,graphics,threads/threads

all: main.native

clean:
	ocamlbuild -clean
	rm -f deps.gv flow.gv deps.png flow.png

main.native: src/*
	ocamlbuild ${BUILDFLAGS} src/main.native

test: main.native
	OCAMLRUNPARAM='b' ./main.native -t -v samples/minimal.lag samples/minimal.layout

deps: main.native
	OCAMLRUNPARAM='b' ./main.native -r -deps deps.gv samples/minimal.lag samples/minimal.layout
	dot -Tpng deps.gv > deps.png
	open deps.png

flow: main.native
	OCAMLRUNPARAM='b' ./main.native -r -flow flow.gv samples/minimal.lag samples/minimal.layout
	dot -Tpng flow.gv > flow.png
	open flow.png
