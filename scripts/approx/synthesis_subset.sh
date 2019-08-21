#!/bin/bash --posix
# This script will run Daisy (and Metalibm) to synthesize approximate programs
# for a subset of benchmarks with both large and small target errors, and then compile them usign Xilinx Vivado HLS.
#
# Run the script form the current directory scripts/approx

base="output"
filelarge="testcases/approx/Benchmarks_large.scala"
filesmall="testcases/approx/Benchmarks_small.scala"

cd ../..
approx_folder="${base}/source_large_subset"
mkdir $approx_folder
xilinx_folder="${base}/results_large_subset"
mkdir $xilinx_folder
logdir="${base}/logs"
mkdir $logdir
if [ -e  "${logdir}/daisy_large_subset.log" ]; then
  rm -rf  "${logdir}/daisy_large_subset.log"
fi
if [ -e  "${logdir}/daisy_small_subset.log" ]; then
  rm -rf  "${logdir}/daisy_small_subset.log"
fi

declare -a functions=(
  "axisRotationX" "axisRotationY" \
  "forwardk2jX" "pendulum2"
  )

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

# # Run daisy on a subset of testfiles with large target errors
for fnc in "${functions[@]}"
do
  echo "******* running Daisy on ${fnc}" | tee -a -i "${logdir}/daisy_large_subset.log"

  (time $script --silent --rangeMethod=interval --errorMethod=affine\
    --approx --apfixed --lang=C --precision=Fixed64 --cost=combined --choosePrecision=fixed \
    --functions="${fnc}" ${filelarge}) 2>&1 | tee -a -i "${logdir}/daisy_large_subset.log"

  new_file_name="${approx_folder}/${fnc}.cpp"
  # Move file to folder and rename appropriately
  mv "output/ApproxBenchmarksLarge.cpp" "${new_file_name}"
  rm -rf output/impl.gappa*
  rm -rf impl.gappa*
  rm -rf impl.c-*
done

# Run Vivado Xilinx HLS
for fnc in "${functions[@]}"
do
  vivado_hls vivado_run.tcl $fnc "${approx_folder}"
  # Collect the synthesis reports
  cp "accel_${fnc}/solution/syn/report/${fnc}_csynth.rpt" "${xilinx_folder}/"
done
rm -rf accel*

######## Small target errors ###########

approx_folder="${base}/source_small_subset"
mkdir $approx_folder
xilinx_folder="${base}/results_small_subset"
mkdir $xilinx_folder

# Run daisy on a subset of testfiles with small target errors
for fnc in "${functions[@]}"
do
  echo "******* running Daisy on ${fnc}" | tee -a -i "${logdir}/daisy_small_subset.log"

  (time $script --silent --rangeMethod=interval --errorMethod=affine\
    --approx --apfixed --lang=C --precision=Fixed64 --cost=combined --choosePrecision=fixed \
    --functions="${fnc}" ${filesmall}) 2>&1 | tee -a -i "${logdir}/daisy_small_subset.log"

  new_file_name="${approx_folder}/${fnc}.cpp"
  # Move file to folder and rename appropriately
  mv "output/ApproxBenchmarksSmall.cpp" "${new_file_name}"
  rm -rf output/impl.gappa*
  rm -rf impl.gappa*
  rm -rf impl.c-*
done

rm -rf output/problemdef.sollya

if [[ $OSTYPE == "darwin"* ]]; then
    # Skip Vivado HLS compilation
    # Parse Daisy log files and reports for a readable output
    printf "======== Large target errors ============\n"
    python3 scripts/approx/ParseLogs.py "large_subset" "macos"
    printf "\n ======== Small target errors ============\n"
    python3 scripts/approx/ParseLogs.py "small_subset" "macos"
    exit 0
fi

# Run Vivado Xilinx HLS
for fnc in "${functions[@]}"
do
  vivado_hls vivado_run.tcl $fnc "${approx_folder}"
  # Collect the synthesis reports
  cp "accel_${fnc}/solution/syn/report/${fnc}_csynth.rpt" "${xilinx_folder}/"
done
rm -rf accel*

# Parse Daisy log files and reports for a readable output
printf "======== Large target errors ============\n"
python3 scripts/approx/ParseLogs.py "large_subset"
printf "\n ======== Small target errors ============\n"
python3 scripts/approx/ParseLogs.py "small_subset"