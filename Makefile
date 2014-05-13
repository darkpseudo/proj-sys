OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread,graphics \
		-libs unix,graphics
TARGET=native
TARGET=byte

agent:
	$(OCAMLBUILD) agent.$(TARGET)

example:
	$(OCAMLBUILD) example.$(TARGET)



clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~

cleanall: realclean
