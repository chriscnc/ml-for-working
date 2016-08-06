.SUFFIXES: .mli .mll .mly .ml .cmo .cmi .cmx

.ml.cmo :
	ocamlc -c $(OFLAGS) $(INCLUDES) $<
.mli.cmi :
	ocamlc -c $(OFLAGS) $(INCLUDES) $<

utils.cmo: utils.cmi


