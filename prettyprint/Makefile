default: build

d:
	dune exec ./bin/past1.exe | tee result1.txt
	dune exec ./bin/past2.exe | tee result2.txt
	diff result1.txt result2.txt

d2: 
	dune exec ./bin/past2.exe | tee result2.txt

p:
	dune test --auto-promote

run:
	dune exec bin/main.exe

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

.PHONY: default build install uninstall test clean