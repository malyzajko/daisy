#!/bin/bash --posix

# Script to generate mixed-precision tuning benchmarks for all files in a given folder,
# and copies them into another specified folder
orig_folder=$1
bench_folder=$2
input=$orig_folder/*

#rm -rf $bench_folder
mkdir $bench_folder

for file in $input
do
  echo "generating benchmark for ${file}"
  ./daisy --mixed-exp-gen --subdiv --errorMethod=affineMPFR \
    --bench-precisions=Float64 --lang=DaisyInput \
    --outputFolder="${bench_folder}" "${file}"

  # move to the right place
  #benchname=${file##*/}
  #mv "output/${benchname}" "${bench_folder}/"
done




