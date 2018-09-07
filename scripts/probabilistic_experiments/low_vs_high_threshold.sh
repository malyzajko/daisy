#!/bin/bash --posix

# 'Standard' benchmark set
declare -a files=("testcases/probabilistic/Doppler.scala" \
  "testcases/probabilistic/Sine.scala" \
  "testcases/probabilistic/Sqrt.scala" \
  "testcases/probabilistic/Bsplines.scala" \
  "testcases/probabilistic/RigidBody.scala" \
  "testcases/probabilistic/Turbine.scala" \
  "testcases/probabilistic/Traincars.scala")

# file with uniform high threshold
declare -a thres1=("testcases/probabilistic/thresholdLowUniform")
declare -a thres2=("testcases/probabilistic/thresholdHighUniform")

declare -i DEFAULT_TIMEOUT=1200


# Make sure the code is compiled
sbt compile

# generate daisy script
if [ ! -e daisy ]
then
  sbt script
fi


# Run probabilistic phase of daisy on each testfile
# Runs the probabilistic analysis with 4 DSI and 8000 outer subdivision for float32 uniform inputs
echo "******************** Probabilisitic Analysis for uniform low threshold ********************"
for file in "${files[@]}"
do
  echo ${file}
  time ./daisy --probabilistic $file --thresholdFile=${thres1} --precision=Float32
done

# Runs the probabilistic analysis with 4 DSI and 8000 outer subdivision for float32 normal inputs
echo "******************** Probabilisitic Analysis for uniform high threshold ********************"
for file in "${files[@]}"
do
  echo ${file}
  time ./daisy --probabilistic $file --thresholdFile=${thres2} --precision=Float32
done