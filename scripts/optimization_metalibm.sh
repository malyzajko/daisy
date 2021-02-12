#!/bin/bash --posix
#
# This script shows how to run Daisy with approximation of transcendental
# functions with Metalibm
# example usage: ./scripts/optimization_metalibm.sh testcases/transcendentals/TransBenchsErrorBoundsLarge.scala TranscendentalBenchmarks sinxx10

# Input file
file=$1

# Name of object in input file
objectName=$2

# Function to consider
fnc=$3


# Make sure the code is compiled and daisy script generated
sbt compile
if [ ! -e daisy ]; then sbt script; fi


# Run daisy
./daisy --metalibm --codegen --lang=C --benchmarking --bound=10000000 --functions=${fnc} ${file} --RDTSC


# compile generated code
./output/compileScript.sh ${objectName}

# run benchmarking
./output/${objectName}_exe rdtsc_metalibm.txt