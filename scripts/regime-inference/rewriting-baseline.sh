#!/bin/bash --posix

# Script to run rewriting without regime inference
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

  /usr/bin/time -f "%e" timeout 30m ./daisy --rewrite --codegen --lang=C --precision=Float64 \
    --rewrite-fitness-fnc=subdiv --rewrite-population=10 --rewrite-generations=20 \
    "${additional_options}" \
    $file 3>&1 1>&2 2>&3 | tee -a "${output_folder}/runtime_log.txt"

  # move to the right place
  mv "output/${benchname}.cpp" "${output_folder}/"
done

# move the log with abstract cost
mv "output/regime-rewriting-baseline.csv" "${output_folder}/"

# --rewrite-seed=123456 \
#--rewrite-seed=654321