#!/bin/bash --posix
#
# This script

# 'Standard' benchmark set
declare -a files=("testcases/rosa/Bsplines.scala" \
  "testcases/rosa/DopplerInlined.scala" \
  "testcases/real2float/Himmilbeau.scala" \
  "testcases/control/InvertedPendulum.scala" \
  "testcases/real2float/Kepler.scala" \
  "testcases/rosa/RigidBody.scala" \
  "testcases/misc/Sine.scala" \
  "testcases/misc/Sqrt.scala" \
  "testcases/control/Traincar4.scala" \
  "testcases/rosa/Turbine.scala" \
  "testcases/rosa/JetEngineInlined.scala" \
  "testcases/misc/TranscendentalsInlined.scala")

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
  time ./daisy --silent --analysis=opt --rangeMethod=smt --solver=z3 ${file}
done