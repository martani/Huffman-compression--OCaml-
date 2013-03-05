Static / Dynamic (adaptive) Huffman compression in OCaml
========================================================

This program illustrates how to use the Huffman Static and dynamic methods to compress / uncompress files using OCaml.

( **Note:** *the code is barely purely functional. If you intend to learn functional programming, look for better example on the web*).

Building and testing the code
-------------------
To build the program:
`````
$ make
`````
To test the program:
`````
$ make test file=/path/to/file/on/file/system
`````
This directive tests the program by compressing the file `file` using static and dynamic compression and decompression.
It shows the time spent on each operation and if any errors occur between the compressed/uncompressed files, the `diff` command
is used to check for such anomalies.

Distributed under the BSD license. (See LICENSE for more information.)
