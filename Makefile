# This Makefile provides a workaround for https://github.com/janestreet/pythonlib/issues/1.
# When the issue is fixed, the Makefile can be removed.
#
# Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022

.PHONY: build install clean make

build:
	./make.sh


install-astral:
	dune install astral

install-tools:
	dune install astral-convertor
	dune install astral-generator

install-binding:
	echo "TODO"

install:
	install-astral
	install-tools
	install-binding

clean:
	dune clean

make:
	build
	install
