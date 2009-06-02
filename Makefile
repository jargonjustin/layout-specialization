BUILDFLAGS=-cflag -g -lflag -g -lib graphics

all: main.native

clean:
	ocamlbuild -clean

main.native: src/*
	ocamlbuild ${BUILDFLAGS} src/main.native

test: main.native
	OCAMLRUNPARAM='b' ./main.native -t -v samples/minimal.lag samples/minimal.layout
