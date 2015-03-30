Disclamer
=========

I made this stuff in 2009, it was my first project in OCaml,
first project in any functional language and a first compiler.

I wrote all this stuff before I've read a book about compiler
construction, garbage collection, memory management, etc.

Hopefully, it explains all weird things you may meet inside.

The good news is that this stuff works.

Build
-----

  1.  Install OCaml and OCaml build tools
  1.  Install ExtLib
  1.  make
 

Examples
--------


    ./beepc.native ./helloworld.beep
	ls -1
	helloworld.bin
	helloworld_stubs.c
	helloworld_stubs.h

The output:

  - helloworld.bin --- bytecodes file
  - helloworld\_stubs.c --- stubs
  - helloworld\_stubs.h --- stubs

