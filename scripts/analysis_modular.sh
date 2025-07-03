#!/bin/bash --posix
#
# This script runs Daisy on all modular benchmarks computing absolute rounding
# errors using the default analysis options (made explicit here).
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

# run Daisy on each testfile from unittests
for file in testcases/modular/unittests/*.scala; do
  echo "*******"
  echo ${file}

  # --silent
  # TODO: check if precision is taken into account
  time ./daisy --precision="${prec}" --results-csv=modular_results.csv \
    --modularRoundOffEval ${file}
    # bb/gelpia
    #--checkInterval
    #--shorterInterval
done

# run Daisy on each testfile from case studies
for file in testcases/modular/case_studies/*.scala; do
  echo "*******"
  echo ${file}

  # --silent
  # TODO: check if precision is taken into account
  time ./daisy --precision="${prec}" --results-csv=modular_results.csv \
    --modularRoundOffEval ${file}
    # bb/gelpia
    #--checkInterval
    #--shorterInterval
done