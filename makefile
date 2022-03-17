.PHONY: test
test:
	dune runtest


.PHONY: parse
parse:
	dune runtest ./src/parse


.PHONY: doc
doc:
	dune build @doc


.PHONY: alba.bc
alba.bc:
	dune build src/exe/alba.bc


.PHONY: alba.exe
alba.exe:
	dune build src/exe/alba.exe


.PHONY: test_alba
test_alba:
	dune exec src/exe/alba.bc -- --work-dir=src/alba_src
