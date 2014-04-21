OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix
<<<<<<< HEAD
TARGET=byte
=======
TARGET=native
>>>>>>> d6f6229afa683834ca298c9738ca7b05f8ca2d0c

example:
	$(OCAMLBUILD) example.$(TARGET)


clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~

cleanall: realclean
