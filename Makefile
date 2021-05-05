.PHONY: default test deps clean report-missing-deps
BUILD=dune build

native:
	$(BUILD) _build/install/default/bin/bard

bytecode:
	$(BUILD) _build/install/default/bin/bard.bc

all:
	$(BUILD)

compile-test-tool:
	$(BUILD) _build/install/default/bin/runtests 

test: all
	_build/install/default/bin/runtests 

deps:
	opam install --deps-only .

clean:
	dune clean

report-missing-deps:
	dune external-lib-deps @install --missing

utop:
	dune utop . -- -init=./.ocamlinit
	
zip-compiler:
	(cd src; zip -FSr ../compiler.zip compiler)
