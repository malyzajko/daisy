#!/bin/bash --posix
# Parameters are 1)path 2)exp_name
src_path=$1
exp_name=$2

# 'Standard' benchmark set
declare -a functions=("axisRotationX"  "axisRotationY" \
  "forwardk2jX" "forwardk2jY" \
  "xu1" "xu2"  \
  "rodriguesRotation" \
  "sinxx10" \
  "pendulum1" "pendulum2" \
  "predictGaussianNB" "predictSVC" \
  "predictMLPLogistic"
  )

res_dir="${src_path}/results_${exp_name}"
mkdir $res_dir

# Run Vivado
for fnc in "${functions[@]}"
do
  vivado_hls vivado_run.tcl $fnc "${src_path}/source_${exp_name}"

  # Collect the synthesis reports
  cp "accel_${fnc}/solution/syn/report/${fnc}_csynth.rpt" "${res_dir}/"
done