#!/bin/bash --posix

# Script to run Daisy to generate uniform 128 bit baseline
bench_folder=$1
output_folder=$2
input=$bench_folder/*

#rm -rf $output_folder
mkdir $output_folder

touch "${output_folder}/runtime_log.txt"

for file in $input
do

  filename=${file##*/}
  benchname="${filename%.scala}"
  echo "baseline ${benchname}" 2>&1 | tee -a "${output_folder}/runtime_log.txt"

  /usr/bin/time -f "%e" timeout 30m ./daisy --subdiv --errorMethod=affineMPFR --precision=Float128 \
    --codegen --lang=C --benchmarking --bound=1000000 \
    --silent $file 3>&1 1>&2 2>&3 | tee -a "${output_folder}/runtime_log.txt"

  # move to the right place
  mv "output/${benchname}.cpp" "${output_folder}/"
  mv "output/${benchname}_benchmark.cpp" "${output_folder}/"
done

