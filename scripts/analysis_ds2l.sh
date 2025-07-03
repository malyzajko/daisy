#!/bin/bash --posix
#
# This script runs Daisy's datastructure-based analysis (DS2L) on the
# available benchmarks, including code generation (generated code goes to output/).
# Prints the results to standard output as well as to a CSV file in output/;
# if the file already exists, the results are appended.

# make sure the code is compiled
sbt compile

# generate daisy script, if it doesn't exist
if [ ! -e daisy ]; then
  sbt script
fi

if [ "$1" = "" ]; then
  prec="Float64"
else
  prec="$1"
fi

# run Daisy on each testfile
for file in testcases/ds2l/AllDiffSmall/*.scala; do
  echo "*******"
  echo ${file}
  time ./daisy --precision="${prec}" --results-csv=ds2l_results.csv \
    --ds --codegen --lang=Scala ${file}
done