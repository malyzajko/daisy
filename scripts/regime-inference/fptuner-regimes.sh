#!/bin/bash --posix

# Script to run mixed-precision tuning without regime inference
bench_folder=$1
output_folder=$2
additional_options=$3
input=$bench_folder/*

#rm -rf $output_folder
mkdir $output_folder

touch "${output_folder}/runtime_log.txt"

for file in $input
do

  filename=${file##*/}
  benchname="${filename%.scala}"
  echo "tuning ${benchname}" 2>&1 | tee -a "${output_folder}/runtime_log.txt"

  /usr/bin/time -f "%e" timeout 30m ./daisy --mixed-tuning --regime-inf --fptuner \
    --mixed-cost-function=simple \
    "${additional_options}" \
    --codegen --lang=C --benchmarking --bound=1000000 \
    --silent $file 3>&1 1>&2 2>&3 | tee -a "${output_folder}/runtime_log.txt"

  # move to the right place
  mv "output/${benchname}.cpp" "${output_folder}/"
  mv "output/${benchname}_benchmark.cpp" "${output_folder}/"
done

# move the log with abstract cost
mv "output/regime-tuning.csv" "${output_folder}/"
