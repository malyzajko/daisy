#!/bin/bash --posix
#
# This script runs Daisy's mixed-precision tuning on the set of benchmarks
# used in the ICCPS'18 paper. The automatically generated code will be saved
# in the output/ folder.
# Using the non-default --rangeMethod=smt is optional; --errorMethod=affine is
# the default option, made explicit here.
#
# The second command below runs rewriting before mixed-precision tuning. The
# range method is set to the default for performance reasons. If you want to fix
# the random seed for determinism, use --rewrite-seed with a non-zero number.


# make sure the code is compiled
sbt compile

# generate daisy script, if it doesn't exist
if [ ! -e daisy ]; then
  sbt script
fi

# run Daisy on each testfile
for file in testcases/mixed-precision/*.scala; do
  echo "*******"
  echo ${file}
  time ./daisy --silent --mixed-tuning --rangeMethod=smt --errorMethod=affine ${file}

  # also use rewriting
  # time ./daisy --silent --mixed-tuning --rewrite --rewrite-seed=0 \
  #   --rangeMethod=interval --errorMethod=affine ${file}
done



# for file in "${arr[@]}"
# do
#   echo "---> $file"   # --rangeMethod=smt #--rewrite-seed-system-millis \
#   if [ "$1" = "rewriting" ]; then
#     ./daisy --mixed-tuning --rewrite --rangeMethod=smt \
#       --rewrite-seed=1490794789615 "testcases/mixed-precision/${file}.scala"
#       #--rewrite-seed-system-millis "testcases/mixed-precision/${file}.scala"

#   else
#     ./daisy --mixed-tuning --rangeMethod=smt "testcases/mixed-precision/${file}.scala"
#     #./daisy --mixed-fixedpoint --mixed-tuning --rangeMethod=smt "testcases/mixed-precision/fixedpoint/${file}.scala"
#   fi
# done
