OCB_FLAGS = -tag bin_annot
OCB = ocamlbuild $(OCB_FLAGS)

current: 
	$(OCB) ch5.native

test: current
	$(OCB) ch5_test.native
	./ch5_test.native

utils:
	$(OCB) utils.native
	$(OCB) utils_test.native
	./utils_test.native

clean:
	$(OCB) -clean


.PHONY: current clean byte native profile debug test

