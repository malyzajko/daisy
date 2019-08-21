#!/bin/bash --posix
#
# This script should be run from the {daisy_home}/script/approx directory
# Uncomment the group of experiments you want to run

base="scripts/approx/"

targeterr="large"
targeterrCapital="Large"
filename="testcases/approx/Benchmarks_large.scala"

resultdir="${base}${targeterr}"

# if [ ne -d $resultdir]; then
  # if the folder does not exist, create
  mkdir $targeterr
  mkdir "${targeterr}/uniform"
  mkdir "${targeterr}/uni-mixed"
  mkdir "${targeterr}/mixed-uni"
  mkdir "${targeterr}/mixed"
  mkdir "${targeterr}/logs"
# fi

# Make sure the code is compiled
sbt compile

# generate daisy script, if needed
if [ ! -e daisy ]
then
  sbt -J-Xmx2G -J-Xss2M compile 
fi

# Series of experiments without refinement loop
# exp_group="ml2_norefinement"
# echo "=== running uniform-precision experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/uniform" ${exp_group} ${filename} "--cost=ml --choosePrecision=fixed --no-refinement ") 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_uni.log"

# echo "=== running uniform-mixed experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/uni-mixed" ${exp_group} ${filename} "--cost=ml --choosePrecision=fixed --polyMixed --no-refinement ") 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_mixed.log"

# echo "=== running mixed-uniform experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/mixed-uni" ${exp_group} ${filename} "--cost=ml --mixed-tuning --choosePrecision=fixed --polyUniform --no-refinement ") 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_uni.log"

# echo "=== running mixed-precision experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/mixed" ${exp_group} ${filename} "--cost=ml --mixed-tuning --no-refinement ") 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_mixed.log"

# exp_group="area210_norefinement"
# # echo "=== running uniform-precision experiments: ${exp_group} ==="
# # (time ./rundaisy.sh "${resultdir}/uniform" ${exp_group} ${filename} "--cost=area --choosePrecision=fixed --no-refinement " ${targeterrCapital}) 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_uni.log"

# echo "=== running uniform-mixed experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/uni-mixed" ${exp_group} ${filename} "--cost=area --choosePrecision=fixed --polyMixed --no-refinement " ${targeterrCapital}) 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_mixed.log"

# echo "=== running mixed-uniform experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/mixed-uni" ${exp_group} ${filename} "--cost=area --mixed-tuning --choosePrecision=fixed --polyUniform --no-refinement " ${targeterrCapital}) 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_uni.log"

# # echo "=== running mixed-precision experiments: ${exp_group} ==="
# # (time ./rundaisy.sh "${resultdir}/mixed" ${exp_group} ${filename} "--cost=area --mixed-tuning --no-refinement " ${targeterrCapital}) 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_mixed.log"

exp_group="combined210_norefinement"
echo "=== running uniform-precision experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/uniform" ${exp_group} ${filename} "--cost=combined --choosePrecision=fixed --no-refinement " ${targeterrCapital}) 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_uni.log"

# echo "=== running uniform-mixed experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/uni-mixed" ${exp_group} ${filename} "--cost=combined --choosePrecision=fixed --polyMixed --no-refinement " ${targeterrCapital}) 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_mixed.log"

# echo "=== running mixed-uniform experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/mixed-uni" ${exp_group} ${filename} "--cost=combined --mixed-tuning --choosePrecision=fixed --polyUniform --no-refinement " ${targeterrCapital}) 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_uni.log"

# echo "=== running mixed-precision experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/mixed" ${exp_group} ${filename} "--cost=combined --mixed-tuning --no-refinement " ${targeterrCapital}) 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_mixed.log"


# The rest is with refinement loop
# # area cost
# exp_group="area210"
# echo "=== running uniform-precision experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/uniform" ${exp_group} ${filename} "--cost=area --choosePrecision=fixed " ${targeterrCapital}) 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_uni.log"

# echo "=== running uniform-mixed experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/uni-mixed" ${exp_group} ${filename} "--cost=area --choosePrecision=fixed --polyMixed " ${targeterrCapital}) 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_mixed.log"

# echo "=== running mixed-uniform experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/mixed-uni" ${exp_group} ${filename} "--cost=area --mixed-tuning --choosePrecision=fixed --polyUniform " ${targeterrCapital}) 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_uni.log"

# echo "=== running mixed-precision experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/mixed" ${exp_group} ${filename} "--cost=area --mixed-tuning " ${targeterrCapital}) 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_mixed.log"


# # ML cost
# exp_group="ml2"
# echo "=== running uniform-precision experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/uniform" ${exp_group} ${filename} "--cost=ml --choosePrecision=fixed ") 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_uni.log"

# echo "=== running uniform-mixed experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/uni-mixed" ${exp_group} ${filename} "--cost=ml --choosePrecision=fixed --polyMixed ") 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_mixed.log"

# echo "=== running mixed-uniform experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/mixed-uni" ${exp_group} ${filename} "--cost=ml --mixed-tuning --choosePrecision=fixed --polyUniform ") 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_uni.log"

# echo "=== running mixed-precision experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/mixed" ${exp_group} ${filename} "--cost=ml --mixed-tuning ") 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_mixed.log"


# # combined cost
# exp_group="combined210"
# echo "=== running uniform-precision experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/uniform" ${exp_group} ${filename} "--cost=combined --choosePrecision=fixed " ${targeterrCapital}) 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_uni.log"

# echo "=== running uniform-mixed experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/uni-mixed" ${exp_group} ${filename} "--cost=combined --choosePrecision=fixed --polyMixed " ${targeterrCapital}) 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_mixed.log"

# echo "=== running mixed-uniform experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/mixed-uni" ${exp_group} ${filename} "--cost=combined --mixed-tuning --choosePrecision=fixed --polyUniform " ${targeterrCapital}) 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_uni.log"

# echo "=== running mixed-precision experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/mixed" ${exp_group} ${filename} "--cost=combined --mixed-tuning " ${targeterrCapital}) 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_mixed.log"



# perform experiments for small target error as well
targeterr="small"
filename="testcases/approx/Benchmarks_small.scala"

resultdir="${base}${targeterr}"

targeterrCapital="Small"
mkdir $targeterr
mkdir "${targeterr}/uniform"
mkdir "${targeterr}/uni-mixed"
mkdir "${targeterr}/mixed-uni"
mkdir "${targeterr}/mixed"
mkdir "${targeterr}/logs"

exp_group="area210_norefinement"
echo "=== running uniform-precision experiments: ${exp_group} ==="
(time ./rundaisy.sh "${resultdir}/uniform" ${exp_group} ${filename} "--cost=area --choosePrecision=fixed --no-refinement " ${targeterrCapital}) 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_uni.log"

# echo "=== running uniform-mixed experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/uni-mixed" ${exp_group} ${filename} "--cost=area --choosePrecision=fixed --polyMixed --no-refinement " ${targeterrCapital}) 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_mixed.log"

# echo "=== running mixed-uniform experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/mixed-uni" ${exp_group} ${filename} "--cost=area --mixed-tuning --choosePrecision=fixed --polyUniform --no-refinement " ${targeterrCapital}) 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_uni.log"

# echo "=== running mixed-precision experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/mixed" ${exp_group} ${filename} "--cost=area --mixed-tuning --no-refinement " ${targeterrCapital}) 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_mixed.log"


# exp_group="ml2_norefinement"
# echo "=== running uniform-precision experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/uniform" ${exp_group} ${filename} "--cost=ml --choosePrecision=fixed --no-refinement " ${targeterrCapital}) 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_uni.log"

# echo "=== running uniform-mixed experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/uni-mixed" ${exp_group} ${filename} "--cost=ml --choosePrecision=fixed --polyMixed --no-refinement " ${targeterrCapital}) 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_mixed.log"

# echo "=== running mixed-uniform experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/mixed-uni" ${exp_group} ${filename} "--cost=ml --mixed-tuning --choosePrecision=fixed --polyUniform --no-refinement " ${targeterrCapital}) 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_uni.log"

# echo "=== running mixed-precision experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/mixed" ${exp_group} ${filename} "--cost=ml --mixed-tuning --no-refinement " ${targeterrCapital}) 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_mixed.log"


# exp_group="combined210_norefinement"
# echo "=== running uniform-precision experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/uniform" ${exp_group} ${filename} "--cost=combined --choosePrecision=fixed --no-refinement " ${targeterrCapital}) 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_uni.log"

# echo "=== running uniform-mixed experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/uni-mixed" ${exp_group} ${filename} "--cost=combined --choosePrecision=fixed --polyMixed --no-refinement " ${targeterrCapital}) 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_mixed.log"

# echo "=== running mixed-uniform experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/mixed-uni" ${exp_group} ${filename} "--cost=combined --mixed-tuning --choosePrecision=fixed --polyUniform --no-refinement " ${targeterrCapital}) 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_uni.log"

# echo "=== running mixed-precision experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/mixed" ${exp_group} ${filename} "--cost=combined --mixed-tuning --no-refinement " ${targeterrCapital}) 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_mixed.log"


# The rest is with refinement loop
# area cost
# exp_group="area210"
# # echo "=== running uniform-precision experiments: ${exp_group} ==="
# # (time ./rundaisy.sh "${resultdir}/uniform" ${exp_group} ${filename} "--cost=area --choosePrecision=fixed " ${targeterrCapital}) 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_uni.log"

# # echo "=== running uniform-mixed experiments: ${exp_group} ==="
# # (time ./rundaisy.sh "${resultdir}/uni-mixed" ${exp_group} ${filename} "--cost=area --choosePrecision=fixed --polyMixed " ${targeterrCapital}) 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_mixed.log"

# echo "=== running mixed-uniform experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/mixed-uni" ${exp_group} ${filename} "--cost=area --mixed-tuning --choosePrecision=fixed --polyUniform " ${targeterrCapital}) 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_uni.log"

# echo "=== running mixed-precision experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/mixed" ${exp_group} ${filename} "--cost=area --mixed-tuning " ${targeterrCapital}) 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_mixed.log"

# combined cost
# exp_group="combined210"
# echo "=== running uniform-precision experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/uniform" ${exp_group} ${filename} "--cost=combined --choosePrecision=fixed " ${targeterrCapital}) 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_uni.log"

# echo "=== running uniform-mixed experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/uni-mixed" ${exp_group} ${filename} "--cost=combined --choosePrecision=fixed --polyMixed " ${targeterrCapital}) 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_mixed.log"

# echo "=== running mixed-uniform experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/mixed-uni" ${exp_group} ${filename} "--cost=combined --mixed-tuning --choosePrecision=fixed --polyUniform " ${targeterrCapital}) 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_uni.log"

# echo "=== running mixed-precision experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/mixed" ${exp_group} ${filename} "--cost=combined --mixed-tuning " ${targeterrCapital}) 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_mixed.log"

# ML cost
# exp_group="ml2"
# echo "=== running uniform-precision experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/uniform" ${exp_group} ${filename} "--cost=ml --choosePrecision=fixed " ${targeterrCapital}) 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_uni.log"

# echo "=== running uniform-mixed experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/uni-mixed" ${exp_group} ${filename} "--cost=ml --choosePrecision=fixed --polyMixed " ${targeterrCapital}) 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_uni_mixed.log"

# echo "=== running mixed-uniform experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/mixed-uni" ${exp_group} ${filename} "--cost=ml --mixed-tuning --choosePrecision=fixed --polyUniform " ${targeterrCapital}) 2>&1 |  tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_uni.log"

# echo "=== running mixed-precision experiments: ${exp_group} ==="
# (time ./rundaisy.sh "${resultdir}/mixed" ${exp_group} ${filename} "--cost=ml --mixed-tuning " ${targeterrCapital}) 2>&1 | tee -a -i "${targeterr}/logs/daisy_${exp_group}_mixed_mixed.log"
