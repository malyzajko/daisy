#!/bin/bash --posix
#
# This script should be run from the Daisy home directory
#
# This script will
# - run Daisy to generate a fixed-point program
# - move the generated file to a folder in $resdir folder

# parameters are: 1)resultdir 2)exp_name 3)file_name 4)options
resdir=$1
exp_name=$2
file=$3
options=$4
targeterr=$5

# 'Standard' benchmark set
declare -a functions=(
  "axisRotationX" "axisRotationY" \
  "forwardk2jX" 
  "forwardk2jY" \
  "xu1" "xu2"  \
  "rodriguesRotation" \
  "sinxx10" \
  "pendulum1" "pendulum2" \
  "predictGaussianNB" "predictSVC" \
  "predictMLPLogistic"
  )

cd ../..
approx_folder="${resdir}/source_${exp_name}"
echo $approx_folder
mkdir $approx_folder


#Run daisy on each testfile
for fnc in "${functions[@]}"
do
  echo "******* running Daisy on ${fnc}"
  #echo
  time ./daisy --silent --rangeMethod=interval --errorMethod=affine\
    --approx --apfixed --lang=C --precision=Fixed64 ${options}\
    --functions="${fnc}" ${file}

  new_file_name="${approx_folder}/${fnc}.cpp"
  # Move file to folder and rename appropriately
  mv "output/ApproxBenchmarks${targeterr}.cpp" "${new_file_name}"
  rm -rf output/impl.gappa*
  rm -rf impl.gappa*
  rm -rf impl.c-*
done

cd scripts/approx
echo "Finished experiment ${exp_name} for ${resdir}"
