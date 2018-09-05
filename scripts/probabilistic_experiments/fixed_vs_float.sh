#!/bin/bash --posix

# Case Studies from the paper
declare -a files=("testcases/probabilistic/Bspline2.scala" \
  "testcases/probabilistic/RigidBody.scala" \
  "testcases/probabilistic/Traincars.scala" \
  "testcases/probabilistic/Filters.scala" \
  "testcases/probabilistic/SolveCubic.scala" \
  "testcases/probabilistic/ClassIDs.scala" \
  "testcases/probabilistic/Neuron.scala")

# file with uniform high threshold
declare -a thres=("testcases/probabilistic/thresholdLowUniform")
declare -i DEFAULT_TIMEOUT=1200


# Make sure the code is compiled
sbt compile

# generate daisy script
if [ ! -e daisy ]
then
  sbt script
fi


# Run probabilistic phase of daisy on each testfile

# Runs the probabilistic analysis with 4 DSI and 8000 outer subdivision for fixed16
echo "******************** Probabilisitic Analysis for uniform independent fixed16 inputs ********************"
for file in "${files[@]}"
do
  echo ${file}
  time ./daisy --probabilistic $file --thresholdFile=${thres} --precision=Fixed16
done

# Runs the probabilistic analysis with 4 DSI and 8000 outer subdivision for float32
echo "******************** Probabilisitic Analysis for uniform independent float32 inputs ********************"
for file in "${files[@]}"
do
  echo ${file}
  time ./daisy --probabilistic $file --thresholdFile=${thres} --precision=Float32
done
