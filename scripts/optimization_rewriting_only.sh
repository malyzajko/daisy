
#!/bin/bash --posix
#
# deterministic genetic search !
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


for file in "${arr[@]}"
do
  echo "---> $file"
  ./daisy --rangeMethod=smt --rewrite --choosePrecision --rewrite-custom-seed=1490794789615 \
    "testcases/mixed-precision/${file}.scala"
done

