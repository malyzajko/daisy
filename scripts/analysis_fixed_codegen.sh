#!/bin/bash --posix
#
# This script runs Daisy on all FPBench benchmarks using dataflow analysis
# (default options made explicit here) assuming a uniform fixed-point bitlength,
# and generates fixed-point code using either bitshift operations in Scala or C,
# or generates C++ code using Xilinx apfixed library (to compile to FPGAs).
# Generated code is saved in the output/ folder.
#
# Note: not all of the FPBench benchmarks are supported for fixed-point code
# generation; for simplicity they are included here, but will not produce code.

# Make sure the code is compiled
sbt compile

# generate daisy script
if [ ! -e daisy ]
then
  sbt script
fi

# run Daisy on each testfile
for file in testcases/fpbench/*.scala; do
  echo "*******"
  echo ${file}
  ./daisy --analysis=dataflow --rangeMethod=interval --errorMethod=affine \
    --codegen --lang=C --precision=Fixed32 ${file}
  #./daisy --analysis=dataflow --rangeMethod=interval --errorMethod=affine \
  #  --codegen --apfixed --precision=Fixed32 ${file}
done