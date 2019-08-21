#!/bin/bash --posix
# This script will run Daisy (and Metalibm) to synthesize approximate programs
# for benchmarks with large target errors, and then compile them usign Xilinx Vivado HLS.
#
# Run the script form the current directory scripts/approx

base="output"
file="testcases/approx/Benchmarks_large.scala"

cd ../..
approx_folder="${base}/source_large"
mkdir $approx_folder
xilinx_folder="${base}/results_large"
mkdir $xilinx_folder
logdir="${base}/logs"
mkdir $logdir
if [ -e  "${logdir}/daisy_large.log" ]; then
  rm -rf  "${logdir}/daisy_large.log"
fi

script="daisy"
if [[ $OSTYPE == "linux-gnu" ]]; then
  script="daisy.sh"
fi

if [ ! -f $script ] ; then
  sbtV=$(sbt sbtVersion | grep "[info]")
  if [[ -z sbtV ]]; then
    echo "The script for Daisy does not exist and build system is not installed."
    echo "Install sbt and run `sbt script` in Daisy home directory."
    echo "brew install sbt"
    echo "For more details see https://www.scala-sbt.org/release/docs/Setup.html"
    exit 1
  else
    sbt compile
    sbt script
  fi
fi

declare -a functionsUni=(
  "axisRotationX" "axisRotationY" \
  "forwardk2jX" "forwardk2jY" \
  "xu1" "xu2"  \
  "rodriguesRotation" \
  "sinxx10" \
  "pendulum1" "pendulum2" \
  "predictSVC"
  )

# Run daisy on each testfile with uniform precision assignment
for fnc in "${functionsUni[@]}"
do
  echo "******* running Daisy on ${fnc}" | tee -a -i "${logdir}/daisy_large.log"

  (time $script --silent --rangeMethod=interval --errorMethod=affine\
    --approx --apfixed --lang=C --precision=Fixed64 --cost=combined --choosePrecision=fixed \
    --functions="${fnc}" ${file}) 2>&1 | tee -a -i "${logdir}/daisy_large.log"

  new_file_name="${approx_folder}/${fnc}.cpp"
  # Move file to folder and rename appropriately
  mv "output/ApproxBenchmarksLarge.cpp" "${new_file_name}"
  rm -rf output/impl.gappa*
  rm -rf impl.gappa*
  rm -rf impl.c-*
done

# predictGaussianNB: mixed precision for global error budget, uniform for polynomials
fnc="predictGaussianNB"
echo "******* running Daisy on ${fnc}" | tee -a -i "${logdir}/daisy_large.log"
(time $script --silent --rangeMethod=interval --errorMethod=affine\
    --approx --apfixed --lang=C --precision=Fixed64\
    --cost=combined --mixed-tuning --choosePrecision=fixed --polyUniform \
    --functions="${fnc}" ${file}) 2>&1 | tee -a -i "${logdir}/daisy_large.log"

new_file_name="${approx_folder}/${fnc}.cpp"
mv "output/ApproxBenchmarksLarge.cpp" "${new_file_name}"

# predictMLPLogistic uniform precision for global error budget, mixed for polynomials
fnc="predictMLPLogistic"
echo "******* running Daisy on ${fnc}" | tee -a -i "${logdir}/daisy_large.log"
(time $script --silent --rangeMethod=interval --errorMethod=affine\
    --approx --apfixed --lang=C --precision=Fixed64\
    --cost=combined --choosePrecision=fixed --polyMixed \
    --functions="${fnc}" ${file}) 2>&1 | tee -a -i "${logdir}/daisy_large.log"

new_file_name="${approx_folder}/${fnc}.cpp"
mv "output/ApproxBenchmarksLarge.cpp" "${new_file_name}"

rm -rf output/impl.gappa*
rm -rf impl.gappa*
rm -rf impl.c-*
rm -rf output/problemdef.sollya

if [[ $OSTYPE == "darwin"* ]]; then
    # Skip Vivado HLS compilation
    # Parse Daisy log files and reports for a readable output
    printf "======== Large target errors ============\n"
    python3 scripts/approx/ParseLogs.py "large" "macos"
    exit 0
fi

declare -a functions=( "${functionsUni[@]}" "predictGaussianNB" "predictMLPLogistic" )
echo ${functions[*]}

# Run Vivado Xilinx HLS
for fnc in "${functions[@]}"
do
  vivado_hls vivado_run.tcl $fnc "${approx_folder}"
  # Collect the synthesis reports
  cp "accel_${fnc}/solution/syn/report/${fnc}_csynth.rpt" "${xilinx_folder}/"
done
rm -rf accel*

# Parse Daisy log files and reports for a readable output
printf "\n===================== Large target errors ==========================\n"
python3 scripts/approx/ParseLogs.py "large"