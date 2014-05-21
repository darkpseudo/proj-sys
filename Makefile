OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread,graphics \
		-libs unix
TARGET=native
TARGET=byte


example:
	$(OCAMLBUILD) example.$(TARGET)



clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~

cleanall: realclean
