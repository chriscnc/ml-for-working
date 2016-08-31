OCB_FLAGS = -tag bin_annot
OCB = ocamlbuild $(OCB_FLAGS)

all: utils tree dict dict2

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

test: test_utils test_tree test_dict test_dict2

test_utils: utils
	./utils_test.native 

test_tree: tree
	./tree_test.native

test_dict: dict
	./dict_test.native

test_dict2: dict
	./dict2_test.native

.PHONY: all clean byte native profile debug test

