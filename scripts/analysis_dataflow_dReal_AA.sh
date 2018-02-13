#!/bin/bash --posix
#
# This script

# 'Standard' benchmark set
declare -a files=("testcases/rosa/Bsplines.scala" \
  "testcases/rosa/Doppler.scala" \
  "testcases/real2float/Himmilbeau.scala" \
  "testcases/control/InvertedPendulum.scala" \
  "testcases/real2float/Kepler.scala" \
  "testcases/rosa/RigidBody.scala" \
  "testcases/misc/Sine.scala" \
  "testcases/misc/Sqrt.scala" \
  "testcases/control/Traincar4.scala" \
  "testcases/rosa/Turbine.scala" \
  "testcases/rosa/JetEngine.scala" \
  "testcases/misc/Transcendentals.scala")

# Make sure the code is compiled
sbt compile

# generate daisy script
if [ ! -e daisy ]
then
  sbt script
fi


# Run daisy on each testfile
for file in "${files[@]}"
do
  echo "*******"
  echo ${file}
  time ./daisy --silent --results-csv=dataflow_dReal_AA_results.csv \
    --analysis=dataflow --rangeMethod=smt --solver=dReal --errorMethod=affine ${file}
done