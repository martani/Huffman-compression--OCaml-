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
	@echo "Testing... static huffman"
	@echo "Compress:"
	@time ./huffman stat c "input" "output"
	@echo "\nUncompress"
	@time ./huffman stat d "output" "output2"
	@echo "\n<<<<<<<<<<<<<<<<Compressed = Uncompress ??>>>>>>>>>>>>>>>>"
	@diff "input" "output2"
	@echo "\nTesting... dynamic huffman"
	@echo "Compress"
	@time ./huffman dyn c "input" "output_d"
	@echo "\nUncompress"
	@time ./huffman dyn d "output_d" "output_d2"
	@echo "\n<<<<<<<<<<<<<<<Compressed = Uncompress ??>>>>>>>>>>>>>>>>>"
	@diff "input" "output_d2"


include OCamlMakefile
