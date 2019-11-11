#!/bin/bash --posix

# Case Studies from the paper
# declare -a files=("testcases/probabilistic/Benchmarks.scala")
declare -a files=("testcases/probabilistic/Bspline2.scala")

# Make sure the code is compiled
sbt compile

# generate daisy script
if [ ! -e daisy ]
then
  sbt script
fi


# Run probabilistic error phase of daisy on each testfile

# Runs the non-probabilistic worst case error analysis with setting A: 0 DSI, 100 outer subdivision and uniform float32 inputs
echo "******************** Non-probabilisitic error analysis for uniform float32 inputs ********************"
for file in "${files[@]}"
do
  echo ${file}
  ./daisy --probabilisticError $file --precision=Float32 --divLimit=100 --worstCase --thresProb=0.15
done

# Runs the non-probabilistic worst case error analysis with setting B: 0 DSI, 100 outer subdivision and gaussian float32 inputs
echo "******************** Non-probabilisitic error analysis for gaussian float32 inputs ********************"
for file in "${files[@]}"
do
  echo ${file}
  ./daisy --probabilisticError $file --precision=Float32 --divLimit=100 --worstCase --gaussian --thresProb=0.15
done

# Runs the probabilistic error analysis with setting C: 2 DSI, 50 outer subdivision and uniform float32 inputs
echo "******************** Probabilisitic error analysis for uniform float32 inputs ********************"
for file in "${files[@]}"
do
  echo ${file}
  ./daisy --probabilisticError $file --precision=Float32 --dsSubDiv=2 --divLimit=50 --thresProb=0.15
done

# Runs the probabilistic error analysis with setting D: 2 DSI, 50 outer subdivision and gaussian float32 inputs
echo "******************** Probabilisitic error analysis for gaussian float32 inputs ********************"
for file in "${files[@]}"
do
  echo ${file}
  ./daisy --probabilisticError $file --precision=Float32 --dsSubDiv=2 --divLimit=50 --gaussian --thresProb=0.15
done

# Runs the probabilistic error analysis with approximate error specification: 2 DSI, 50 outer subdivision and uniform float32 inputs
echo "******************** Probabilisitic error analysis for uniform float32 inputs with approximate error specification ********************"
for file in "${files[@]}"
do
  echo ${file}
  ./daisy --probabilisticError $file --precision=Float32 --dsSubDiv=2 --divLimit=50 --thresProb=0.15 --approximate --bigErrorMultiplier=2 --bigErrorProb=0.1
done

# Runs the probabilistic error analysis with approximate error specification: 2 DSI, 50 outer subdivision and gaussian float32 inputs
echo "******************** Probabilisitic error analysis for gaussian float32 inputs with approximate error specification ********************"
for file in "${files[@]}"
do
  echo ${file}
  ./daisy --probabilisticError $file --precision=Float32 --dsSubDiv=2 --divLimit=50 --thresProb=0.15 --gaussian --approximate --bigErrorMultiplier=2 --bigErrorProb=0.1
done
