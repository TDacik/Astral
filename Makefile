# This Makefile provides a workaround for https://github.com/janestreet/pythonlib/issues/1.
# When the issue is fixed, the Makefile can be removed.
#
# Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022

.PHONY: build install clean make

build:
	./make.sh

install:
	dune install

clena:
	dune clean

make:
	build
	install
