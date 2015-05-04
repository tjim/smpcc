BIN=/usr/local/bin
CXXFLAGS=-I/usr/lib/ocaml
CFLAGS=-I/usr/lib/ocaml

smpcc: llabs.cmx state.cmx llparse.cmx lllex.cmx options.cmx garbled.cmx gmw.cmx front.cmx main.cmx
	ocamlfind ocamlopt -package findlib,cil callgraph.cmxa oneret.cmxa $^ -linkpkg -o $@

parse: llabs.cmx state.cmx llparse.cmx lllex.cmx echo.cmx
	ocamlfind ocamlopt str.cmxa nums.cmxa $^ -linkpkg -o $@

extractir: extractir.cmx
	ocamlopt unix.cmxa $^ -o $@

clean:
	$(RM) *.cmx *.cmi *.o llparse.mli llparse.mli lllex.ml

install: smpcc
	mv $< $(BIN)/$<

uninstall:
	$(RM) $(BIN)/smpcc

front.cmx: front.ml
	ocamlfind ocamlopt -c -package findlib,cil $< -o $@

%.cmi: %.mli
	ocamlc -c $<

%.cmx: %.ml
	ocamlopt -c $< -o $@

%.ml: %.mll
	ocamllex $< -o $@

%.ml: %.mly
	ocamlyacc $<

%.mli: %.mly
	ocamlyacc $<

include .depend
.depend: lllex.ml llparse.ml llparse.mli
	ocamldep -native *.ml *.mli > $@
