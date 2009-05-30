BUILDFLAGS=-cflag -g -lflag -g

all: main.native

clean:
	rm -rf main.native _build

main.native: src/*
	ocamlbuild ${BUILDFLAGS} src/main.native

test: main.native
	OCAMLRUNPARAM='b' ./main.native -v -p samples/minimal.lag samples/minimal.layout