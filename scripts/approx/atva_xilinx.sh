#!/bin/bash --posix
#
# This script compiles using Xilinx and copies resutls
# Parameters are 1)target error


targeterr=$1 # can be "large" or "small"
resultdir="${targeterr}"

# if [ ne -d $resultdir]; then
  # if the folder does not exist, create
  mkdir $resultdir
  mkdir "${resultdir}/uniform"
  mkdir "${resultdir}/uni-mixed"
  mkdir "${resultdir}/mixed-uni"
  mkdir "${resultdir}/mixed"
  mkdir "${resultdir}/logs"

# Series of experiments
# Choose the experiments that you have source cpp-files for
declare -a experiments=(
	"ml2_norefinement" "area210_norefinement" "combined210_norefinement" \
	"area210" "ml2" "combined210"
	)

# Run Vivado
for fnc in "${experiments[@]}"
do
	echo "=== running uniform-precision experiments: ${exp_group} ==="
	(time ./runxilinx.sh "${resultdir}/uniform" ${exp_group}) 2>&1 | tee -a -i "${resultdir}/logs/xilinx_${exp_group}_uni_uni.log"

	echo "=== running uniform-mixed experiments: ${exp_group} ==="
	(time ./runxilinx.sh "${resultdir}/uni-mixed" ${exp_group}) 2>&1 |  tee -a -i "${resultdir}/logs/xilinx_${exp_group}_uni_mixed.log"

	echo "=== running mixed-uniform experiments: ${exp_group} ==="
	(time ./runxilinx.sh "${resultdir}/mixed-uni" ${exp_group}) 2>&1 |  tee -a -i "${resultdir}/logs/xilinx_${exp_group}_mixed_uni.log"

	echo "=== running mixed-precision experiments: ${exp_group} ==="
	(time ./runxilinx.sh "${resultdir}/mixed" ${exp_group}) 2>&1 | tee -a -i "${resultdir}/logs/xilinx_${exp_group}_mixed_mixed.log"
done