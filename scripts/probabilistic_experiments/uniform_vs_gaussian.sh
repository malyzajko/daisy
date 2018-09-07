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
declare -a thres1=("testcases/probabilistic/thresholdLowUniform")
declare -a thres2=("testcases/probabilistic/thresholdLowGaussian")

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
echo "******************** Probabilisitic Analysis for uniform distribution of inputs ********************"
for file in "${files[@]}"
do
  echo ${file}
  time ./daisy --probabilistic $file --thresholdFile=${thres1} --precision=Float32
done

# Runs the probabilistic analysis with 4 DSI and 8000 outer subdivision for float32 normal inputs
echo "******************** Probabilisitic Analysis for normal distribution of inputs ********************"
for file in "${files[@]}"
do
  echo ${file}
  time ./daisy --probabilistic $file --thresholdFile=${thres2} --precision=Float32 --gaussian
done