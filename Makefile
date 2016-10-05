OCB_FLAGS = -tag bin_annot
OCB = ocamlbuild $(OCB_FLAGS)

all: ch5

clean:
	$(OCB) -clean

utils:
	$(OCB) utils_test.native
	$(OCB) utils_test.byte

tree: 
	$(OCB) tree_test.native
	$(OCB) tree_test.byte

dict: 
	$(OCB) dict_test.native
	$(OCB) dict_test.byte

dict2: 
	$(OCB) dict2_test.native
	$(OCB) dict2_test.byte

tautology: 
	$(OCB) tautology_test.native
	$(OCB) tautology_test.byte

ch5:
	$(OCB) ch5.native
	$(OCB) ch5_test.native


test: test_ch5

test_utils: utils
	./utils_test.native 

test_tree: tree
	./tree_test.native

test_dict: dict
	./dict_test.native

test_dict2: dict
	./dict2_test.native

test_tautology: tautology
	./tautology_test.native

test_ch5: ch5
	./ch5_test.native

.PHONY: all clean byte native profile debug test

