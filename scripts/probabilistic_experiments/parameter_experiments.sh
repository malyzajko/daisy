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

# Runs only the probabilistic analysis with 25 focal elements with no outer subdivision
echo "*********************************** Probabilistic Analysis only ***********************************"
for file in "${files[@]}"
do
  echo ${file}
  time ./daisy --probabilistic $file --thresholdFile=${thres} --precision=Float32 --dsSubDiv=25 --outerDiv=1
done

# Runs only the Z3 reachability based non-probabilistic analysis with 32000 outer subdivision
echo "*********************************** Non-Probabilistic Analysis ***********************************"
for file in "${files[@]}"
do
  echo ${file}
  time ./daisy --probabilistic $file --thresholdFile=${thres} --precision=Float32 --dsSubDiv=1 --outerDiv=32000
done

# Runs the probabilistic analysis with 2 DSI and 16000 outer subdivision
echo "************************* Outer Subdivision and Probabilisitc Analysis with Coarse Discretization *************************"
for file in "${files[@]}"
do
  echo ${file}
  time ./daisy --probabilistic $file --thresholdFile=${thres} --precision=Float32 --dsSubDiv=2 --outerDiv=16000
done

# Runs the probabilistic analysis with 4 DSI and 8000 outer subdivision. Provides the best result.
echo "************************* Outer Subdivision and Probabilisitc Analysis with Larger Discretization *************************"
for file in "${files[@]}"
do
  echo ${file}
  time ./daisy --probabilistic $file --thresholdFile=${thres} --precision=Float32
done

# Runs the probabilistic analysis with derivative guided outer subdivision. Works till 9 input variables
echo "************************* Derivative guided Outer Subdivision with Probabilisitc Analysis *************************"
for file in "${files[@]}"
do
  echo ${file}
  time ./daisy --probabilistic $file --thresholdFile=${thres} --precision=Float32 --derivativeGuided
done

