#!/bin/bash --posix
#
# This script runs Daisy on all FPBench benchmarks computing absolute rounding
# errors using the default analysis options after the rewriting optimization.
# Prints the results to standard output as well as to a CSV file in output/;
# if the file already exists, the results are appended.
# If you want to fix the random seed for determinism, use --rewrite-seed=1490
# with a non-zero number.

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
  time ./daisy --precision="${prec}" --results-csv=rewriting_IA_AA_results.csv \
    --rewrite --rewrite-seed=0 \
    --analysis=dataflow --rangeMethod=interval --errorMethod=affine ${file}
done