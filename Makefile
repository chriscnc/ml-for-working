OCB_FLAGS = -tag bin_annot
OCB = ocamlbuild $(OCB_FLAGS)

current: 
	$(OCB) sequence.native

test: current
	$(OCB) sequence_test.native
	./sequence_test.native

utils:
	$(OCB) utils.native
	$(OCB) utils_test.native
	./utils_test.native

clean:
	$(OCB) -clean


.PHONY: current clean byte native profile debug test

