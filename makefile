.PHONY: test
test:
	dune runtest


.PHONY: parse
parse:
	dune runtest ./src/parse


.PHONY: doc
doc:
	dune build @doc
