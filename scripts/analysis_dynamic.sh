#!/bin/bash --posix
#
# This script runs Daisy's dynamic analysis to compute an under-approximation
# of rounding errors on all FPBench benchmarks.
# Prints the results to standard output as well as to a CSV file in output/;
# if the file already exists, the results are appended.
# Use --dynamic-method=bgrt to try our re-implementation for maximizing errors
# from "Efficient Search for Inputs Causing High Floating-point Errors", PPoPP'14.
#
# Setting the dynamic-seed to 0 (default setting) will produce a random seed based
# on current time. If you want to fix the seed (e.g. for reproducibility), set
# the option to a non-zero number.

# make sure the code is compiled
sbt compile

# generate daisy script, if it doesn't exist
if [ ! -e daisy ]; then
  sbt script
fi


# run Daisy on each testfile
for file in testcases/fpbench/*.scala; do
  echo "*******"
  echo ${file}
  time ./daisy --dynamic --sampleSize=100000 --dynamic-seed=0 --dynamic-method=uniformMPFR \
    --silent --results-csv=dynamic_results.csv ${file}

  # time ./daisy --dynamic --sampleSize=10000 --dynamic-seed=0 --dynamic-method=bgrt \
  #   --silent --results-csv=dynamic_results.csv ${file}
done