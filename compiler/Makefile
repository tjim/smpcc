BIN=/usr/local/bin
CXXFLAGS=-I/usr/lib/ocaml
CFLAGS=-I/usr/lib/ocaml
LIBS=llvm.cmxa llvm_bitreader.cmxa unix.cmxa str.cmxa

smpcc: oli.cmx state.cmx options.cmx gobe.cmx main.cmx llvmfixup.o bindings.o
	ocamlopt -cc 'g++ -Wno-write-strings' -cclib -rdynamic $(LIBS) $^ -o $@

parse: util.cmx llparse.cmx lllex.cmx
	ocamlopt nums.cmxa $^ -o $@

clean:
	$(RM) *.cmx *.cmi *.o

install: smpcc
	mv $< $(BIN)/$<

uninstall:
	$(RM) $(BIN)/$<

%.cmi: %.mli
	ocamlc -c $<

%.cmx: %.ml
	ocamlopt -c $< -o $@

%.ml: %.mll
	ocamllex $< -o $@

%.ml: %.mly
	ocamlyacc $<

include .depend
.depend: lllex.ml llparse.ml llparse.mli
	ocamldep -native *.ml *.mli > $@
