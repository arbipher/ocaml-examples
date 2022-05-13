default: build

build:
	dune build

test:
	dune runtest -f

utop:
	dune utop . -- -implicit-bindings

promote:
	dune runtest -f --auto-promote

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

ben:
	dune exec bin/speed.exe

auto:
	dune test --auto-promote

.PHONY: default build install uninstall test clean ben auto