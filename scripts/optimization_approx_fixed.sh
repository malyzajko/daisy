#!/bin/bash --posix
#
# This script shows how to run Daisy with approximation of transcendental
# functions with Metalibm for fixed-point arithmetic.
# example usage: ./scripts/optimization_approx_fixed.sh testcases/approx/Benchmarks_large.scala sinxx10

# Input file
file=$1

# Function to consider
fnc=$2


# Make sure the code is compiled and daisy script generated
sbt compile
if [ ! -e daisy ]; then sbt script; fi


# Run daisy
./daisy --approx --apfixed --lang=C --precision=Fixed64 --cost=combined \
  --choosePrecision=fixed --functions=${fnc} ${file}