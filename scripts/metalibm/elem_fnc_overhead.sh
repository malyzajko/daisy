#!/bin/bash --posix


prec=$1
echo "Running for precision: $prec"

inputFile=TransBenchsArith.scala

outputFolder=overhead

mkdir "output/${outputFolder}/"

resultsFile="output/${outputFolder}/results_${prec}_${inputFile}.txt"
rm -f "${resultsFile}"
touch -f "${resultsFile}"

echo "# Stats: mean cycles, std. deviation, min cycles, max cycles" >> "${resultsFile}"
echo ""

# # run on the following functions
declare -a functions=("sinxx10" "xu1" "xu2" "integrate18257" \
  "integrateStoutemyer2007" "axisRotationX" \
  "axisRotationY" "rodriguesRotation" "pendulum1" "pendulum2" \
  "forwardk2jX" "forwardk2jY" \
  "ex2_1" "ex2_2" "ex2_3" "ex2_4" "ex2_5" \
  "ex2_9" "ex2_10" "ex2_11" "ex3_d")


for fnc in "${functions[@]}"
do
  echo "${fnc}" >> "${resultsFile}"


  time ./daisy testcases/transcendentals/$inputFile --functions=$fnc --codegen \
  --lang=C --benchmarking --bound=10000000 --precision=$prec --comp-opts=[cse] \

  ./output/compileScript.sh TranscendentalBenchmarks
  mv ./output/TranscendentalBenchmarks_exe ./output/${outputFolder}/exe_mathh_$fnc


  time ./daisy testcases/transcendentals/$inputFile --functions="${fnc}Arith" --codegen \
  --lang=C --benchmarking --bound=10000000 --precision=$prec --comp-opts=[cse] \

  ./output/compileScript.sh TranscendentalBenchmarks
  mv ./output/TranscendentalBenchmarks_exe ./output/${outputFolder}/exe_arith_$fnc

  # --------------------------
  #      benchmarking
  # --------------------------

  TIMEDATA_FILENAME_MATHH="${fnc}_timedata_mathh.txt"

  TIMEDATA_FILENAME_ARITH="${fnc}_timedata_arith.txt"

  ./output/${outputFolder}/exe_mathh_$fnc "${TIMEDATA_FILENAME_MATHH}"

  ./output/${outputFolder}/exe_arith_$fnc "${TIMEDATA_FILENAME_ARITH}"

  STATS="$(python scripts/metalibm/timedata_processing.py ${TIMEDATA_FILENAME_MATHH} ${TIMEDATA_FILENAME_ARITH} )"

  echo "${STATS}"

  echo "${STATS}" >> "${resultsFile}"

  rm $TIMEDATA_FILENAME_MATHH
  rm $TIMEDATA_FILENAME_ARITH

  echo "" >> "${resultsFile}"
  echo "" >> "${resultsFile}"

done