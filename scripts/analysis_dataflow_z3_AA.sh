#!/bin/bash --posix
#
# This script runs Daisy on all FPBench benchmarks computing absolute rounding
# errors using Z3 to compute tighter ranges, and affine arithmetic for errors.
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
for file in testcases/fpbench/*.scala; do
  echo "*******"
  echo ${file}
  time ./daisy --silent --precision="${prec}" --results-csv=dataflow_z3_AA_results.csv \
    --analysis=dataflow --rangeMethod=smt --solver=z3 --errorMethod=affine ${file}
done