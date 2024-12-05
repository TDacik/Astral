#!/bin/sh

for test_case in $(find . -name '*_tests.exe')
do
    ./$test_case #--compact
done
