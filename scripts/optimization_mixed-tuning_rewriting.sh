
#!/bin/bash --posix
#
# Runs mixed-precision tuning on a set of benchmarks
# Usage: this script needs to be run from the /daisy home directory. If you
# get an error that daisy is not found, check this.
#
# Parameters are non-optional:
#
# 1: whether to use rewriting
# 2: whether to run benchmarking
#
# ./scripts/mixed-precision/run_delta_batch.sh [rewriting|foo] [bench|foo] [directory of generated files] [bench-id]
#
# seeds used: 1469010147126

declare -a arr=("Bsplines0" \
  "Bsplines1" \
  "Bsplines2" \
  "Doppler" \
  "Himmilbeau" \
  "InvertedPendulum" \
  "Kepler0" \
  "Kepler1" \
  "Kepler2" \
  "RigidBody1" \
  "RigidBody2" \
  "Sine" \
  "Sqrt" \
  "Traincar4_State8" \
  "Traincar4_State9" \
  "Turbine1" \
  "Turbine2" \
  "Turbine3")


echo "Running mixed-precision optimization with delta debugging"
# rm mixed-opt-numValid.txt    # just in case there is old data there
# rm mixed-opt-minCost.txt
# rm mixed-opt-opCount.txt
for file in "${arr[@]}"
do
  echo "---> $file"   # --rangeMethod=smt #--rewrite-seed-system-millis \
  if [ "$1" = "rewriting" ]; then
    ./daisy --mixed-tuning --rewrite --rangeMethod=smt \
      --rewrite-custom-seed=1490794789615 "testcases/mixed-precision/${file}.scala"
      #--rewrite-seed-system-millis "testcases/mixed-precision/${file}.scala"

  else
    ./daisy --mixed-tuning --rangeMethod=smt "testcases/mixed-precision/${file}.scala"
    #./daisy --mixed-fixedpoint --mixed-tuning --rangeMethod=smt "testcases/mixed-precision/fixedpoint/${file}.scala"
  fi
done
