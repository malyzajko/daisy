#!/bin/bash --posix

# This script runs all executables in the output/$outputFolderName, and analyzes
# the recorded running times.

# to be run from the main directory

outputFolderName=$1
echo "output folder: $outputFolderName"

outputFolder=output/$outputFolderName
echo "going to time executables in folder: ${outputFolder}"

resultsFile="${outputFolder}/timing_results.txt"
rm -f "${resultsFile}"
touch "${resultsFile}"

declare -a functions=("sinxx10" "xu1" "xu2" "integrate18257" \
  "integrateStoutemyer2007" "axisRotationX" \
  "axisRotationY" "rodriguesRotation" "pendulum1" "pendulum2" \
  "forwardk2jX" "forwardk2jY" \
  "ex2_1" "ex2_2" "ex2_3" "ex2_4" "ex2_5" \
  "ex2_9" "ex2_10" "ex2_11" "ex3_d")

echo "# Stats: mean cycles, std. deviation, min cycles, max cycles" >> "${resultsFile}"


for fnc in "${functions[@]}"
do

  if [ -f ${outputFolder}/exe_mathh_$fnc ]; then

    echo "measuring: ${fnc}"
    echo "${fnc}" >> "${resultsFile}"

    TIMEDATA_FILENAME_MATHH="${fnc}_timedata_mathh.txt"

    TIMEDATA_FILENAME_METALIBM="${fnc}_timedata.txt"

    ./${outputFolder}/exe_mathh_$fnc "${TIMEDATA_FILENAME_MATHH}"

    ./${outputFolder}/exe_arith_$fnc "${TIMEDATA_FILENAME_METALIBM}"

    STATS="$(python scripts/metalibm/timedata_processing.py ${TIMEDATA_FILENAME_MATHH} ${TIMEDATA_FILENAME_METALIBM} )"

    echo "${STATS}"

    echo "${STATS}" >> "${resultsFile}"

    rm $TIMEDATA_FILENAME_MATHH
    rm $TIMEDATA_FILENAME_METALIBM

    echo "" >> "${resultsFile}"
    echo "" >> "${resultsFile}"
    echo ""

  fi

done