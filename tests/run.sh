#!/bin/sh

for test_case in $(find . -name '*.exe')
do
    ./$test_case --compact
done
