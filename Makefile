OCB_FLAGS = -tag bin_annot
OCB = ocamlbuild $(OCB_FLAGS)

all: native byte # profile debug

clean:
	$(OCB) -clean

native: 
	$(OCB) utils_test.native

byte:
	$(OCB) utils_test.byte

profile:
	$(OCB) -tag profile utils_test.native

debug:
	$(OCB) -tag debug utils_test.byte

test: native
	./utils_test.native 

.PHONY: all clean byte native profile debug test

