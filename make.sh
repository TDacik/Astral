# This scripts provides a workaround for https://github.com/janestreet/pythonlib/issues/1
#
# Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022

LIBS="
  base base/base_internalhash_types
  ppx_expect/collector
  time_now
  bin_prot
  pyml
  stdcompat
  z3
  zarith"

OPAM_LIBS="$OPAM_SWITCH_PREFIX/lib"

for lib in $LIBS
do
  export LIBRARY_PATH="$OPAM_LIBS/$lib:$LIBRARY_PATH"
done

dune build
