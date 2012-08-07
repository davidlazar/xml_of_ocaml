OCAML_INCS = -I +camlp4 -I +camlp4/Camlp4Parsers
OCAML_LIBS = dynlink.cma camlp4fulllib.cma str.cma

xml_of_ocaml: ASTToXML.ml main.ml
	ocamlc $(OCAML_INCS) $(OCAML_LIBS) $^ -o $@

clean:
	$(RM) *.cmi *.cmo xml_of_ocaml
