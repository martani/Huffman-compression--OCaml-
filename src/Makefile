# see: http://www.ocaml.info/home/ocaml_sources.html#toc16

# put here the names of your source files (in the right order)
SOURCES = huffman_static.ml dynamicTree.ml huffman_dynamic.ml main.ml

LIBS = unix

# the name of the resulting executable
RESULT = huffman

# generate type information (.annot files)
ANNOTATE = yes

# make target (see manual) : byte-code, debug-code, native-code
all: native-code

test:huffman
	@echo "Testing Static Huffman ..."
	@echo "[[ Compress ]]"
	@time ./huffman stat c $(file) "output_oc_hu__"
	@echo ""
	@echo "[[ Uncompress ]]"
	@time ./huffman stat d "output_oc_hu__" "output2_oc_hu__"
	@echo "<<< Compressed ?? Uncompress >>> If diff shows any messages, a bug exists. No messages, all good."
	@diff $(file) "output2_oc_hu__"
	@echo ""
	@echo "Testing Dynamic Huffman ..."
	@echo "[[ Compress ]]"
	@time ./huffman dyn c $(file) "output_d_oc_hu__"
	@echo ""
	@echo "[[Uncompress ]]"
	@time ./huffman dyn d "output_d_oc_hu__" "output_d2_oc_hu__"
	@echo "<<< Compressed ?? Uncompress >>> If diff shows any messages, a bug exists. No messages, all good."
	@diff $(file) "output_d2_oc_hu__"
	@rm output_oc_hu__ output2_oc_hu__ output_d_oc_hu__ output_d2_oc_hu__


include OCamlMakefile
