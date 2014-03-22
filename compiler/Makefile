BIN=/usr/local/bin
CXXFLAGS=-I/usr/lib/ocaml
CFLAGS=-I/usr/lib/ocaml
LIBS=llvm.cmxa llvm_bitreader.cmxa unix.cmxa str.cmxa

smpcc: oli.cmx state.cmx options.cmx gobe.cmx main.cmx llvmfixup.o bindings.o
	ocamlopt -cc 'g++ -Wno-write-strings' -cclib -rdynamic $(LIBS) $^ -o $@

clean:
	$(RM) *.cmx *.cmi *.o

install: smpcc
	mv $< $(BIN)/$<

uninstall:
	$(RM) $(BIN)/$<

%.cmx: %.ml
	ocamlopt -c $< -o $@

include .depend
.depend:
	ocamldep -native *.ml *.mli > $@
