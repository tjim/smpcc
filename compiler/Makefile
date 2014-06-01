BIN=/usr/local/bin
CXXFLAGS=-I/usr/lib/ocaml
CFLAGS=-I/usr/lib/ocaml
LIBS=nums.cmxa unix.cmxa str.cmxa

smpcc: util.cmx state.cmx llparse.cmx lllex.cmx options.cmx gobe.cmx gmw.cmx main.cmx
	ocamlopt $(LIBS) $^ -o $@

parse: util.cmx state.cmx llparse.cmx lllex.cmx echo.cmx
	ocamlopt $(LIBS) $^ -o $@

clean:
	$(RM) *.cmx *.cmi *.o llparse.mli llparse.mli lllex.ml

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

%.mli: %.mly
	ocamlyacc $<

include .depend
.depend: lllex.ml llparse.ml llparse.mli
	ocamldep -native *.ml *.mli > $@
