#!/bin/bash --posix
# set -e
# declare -a subset=(
# "NMSESection3_11_1.scala" \
# "NewtonRaphsonsMethod_1.scala" \
# "Nonlin2_2.scala" \
# "PID_5.scala" \
# "Verhulst_1.scala" 
# )

# FPBench config (or add to the PATH)
fpbenchdir=/Users/anastasiia/Documents/apps/FPBench

src=testcases/fpbench
# herbiesrc32=testcases/herbie_fpcore/src32
herbiesrc64=testcases/herbie_fpcore/src64
# herbieout_fpcore32=testcases/herbie_fpcore/out32/fpcore
herbieout_fpcore64=testcases/herbie_fpcore/out64-4/fpcore
# herbieout_scala=testcases/herbie_fpcore/out/scala

# mkdir -p $herbiesrc32
mkdir -p $herbiesrc64
# mkdir -p $herbieout_fpcore32
mkdir -p $herbieout_fpcore64

# for file in $src/*
# # for file in "${subset[@]}"
# do
#   echo $file
#   ./daisy --lang=FPCore --codegen --pow-unroll --precision=Float32 $file
#   # ./daisy --lang=FPCore --codegen $src/$file
  
#   filename=$(basename -- "$file")
#   benchmark="${filename%_*}"
#   mv output/$benchmark.fpcore $herbiesrc32/$benchmark.fpcore 
#   # echo output/$benchmark.fpcore
#   # echo $herbiesrc/$benchmark.fpcore 
# done

echo "Hint: Comment out the failing benchmarks in $src/*"
for file in $src/*
do
  echo $file
  ./daisy --lang=FPCore --codegen --pow-unroll --precision=Float64 $file
  
  filename=$(basename -- "$file")
  benchmark="${filename%.*}"
  mv output/$benchmark.fpcore $herbiesrc64/$benchmark.fpcore 
done

# echo "Running Herbie with 32-bit precision"
# for file in $herbiesrc32/*
# do
# 	filename=$(basename -- "$file")
# 	benchmark="${filename%.*}"

# 	herbie improve $file $herbieout_fpcore32/$benchmark.fpcore
# done

echo "Running Herbie with 64-bit precision"

for file in $herbiesrc64/*
do
	filename=$(basename -- "$file")
	benchmark="${filename%.*}"

	herbie improve $file $herbieout_fpcore64/$benchmark.fpcore 2>&1| tee -a -i scripts/regime_inference_experiments/herbie-upd-benchmarks.log
done

# echo "Transform Herbie output (FPCore format) back into Daisy-readable format (Scala)"
# for file in $herbieout_fpcore/*
# do
#   filename=$(basename -- "$file")
#   benchmark="${filename%.*}"
#   # use FPBench to translate FPCore to Scala
#   racket $fpbenchdir/export.rkt $file $herbieout_scala/$benchmark.scala
#   # echo $fpbenchdir/export.rkt 
#   # echo $file 
#   # echo $herbieout_scala/$benchmark.scala
#   # echo " "
# done

# echo "Confirm the rounding error on Herbie-generated programs"

