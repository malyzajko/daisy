#!/bin/bash --posix
#
# Runs all regression tests, prints results to standard output
# Run this file from Daisy's top level directory

# make sure the code is compiled
#sbt compile

# generate daisy script, if it doesn't exist
if [ ! -e daisy ]; then
  sbt script
fi

# format of time command:
TIMEFORMAT=%3lR


echo "TEST 1 (quick): dataflow analysis, interval ranges, affine errors"
if [ -e output/test_dataflow_IA_AA_results.csv ]; then
  rm output/test_dataflow_IA_AA_results.csv
fi
time ./daisy --silent --no-stdout-print --results-csv="test_dataflow_IA_AA_results.csv" \
  --analysis=dataflow --rangeMethod=interval --errorMethod=affine \
  regression/testcases/allTests_dataflow.scala

python3 regression/scripts/check_consistent_csv.py \
  regression/reference/dataflow_IA_AA_results.csv output/test_dataflow_IA_AA_results.csv


echo ""
echo "TEST 2 (quick): dataflow analysis, interval ranges, interval errors"
if [ -e output/test_dataflow_IA_IA_results.csv ]; then
  rm output/test_dataflow_IA_IA_results.csv
fi
time ./daisy --silent --no-stdout-print --results-csv="test_dataflow_IA_IA_results.csv" \
  --analysis=dataflow --rangeMethod=interval --errorMethod=interval \
  regression/testcases/allTests_dataflow.scala

python3 regression/scripts/check_consistent_csv.py \
  regression/reference/dataflow_IA_IA_results.csv output/test_dataflow_IA_IA_results.csv


echo ""
echo "TEST 3 (quick): dataflow analysis, affine ranges, affine errors"
if [ -e output/test_dataflow_AA_AA_results.csv ]; then
  rm output/test_dataflow_AA_AA_results.csv
fi
time ./daisy --silent --no-stdout-print --results-csv="test_dataflow_AA_AA_results.csv" \
  --analysis=dataflow --rangeMethod=affine --errorMethod=affine \
  regression/testcases/allTests_dataflow_AA_AA.scala

python3 regression/scripts/check_consistent_csv.py \
  regression/reference/dataflow_AA_AA_results.csv output/test_dataflow_AA_AA_results.csv


echo ""
echo "TEST 4 (longer): dataflow subdivision, interval ranges, affine errors"
if [ -e output/test_subdiv_IA_AA_results.csv ]; then
  rm output/test_subdiv_IA_AA_results.csv
fi
time ./daisy --silent --no-stdout-print --results-csv="test_subdiv_IA_AA_results.csv" \
  --analysis=dataflow --rangeMethod=interval --errorMethod=affine \
  --subdiv --divLimit=3 --totalOpt=32 \
  regression/testcases/allTests_subdiv.scala

python3 regression/scripts/check_consistent_csv.py \
  regression/reference/dataflow_subdiv_IA_AA_results.csv output/test_subdiv_IA_AA_results.csv


echo ""
echo "TEST 5 (longer): taylor analysis with intervals"
if [ -e output/test_taylor_IA_results.csv ]; then
  rm output/test_taylor_IA_results.csv
fi
time ./daisy --silent --no-stdout-print --results-csv="test_taylor_IA_results.csv" \
  --FPTaylor regression/testcases/allTests_taylor.scala

python3 regression/scripts/check_consistent_csv.py \
  regression/reference/taylor_IA_results.csv output/test_taylor_IA_results.csv


echo ""
echo "TEST 6 (longer): dataflow analysis, Z3 ranges, affine errors"
if [ -e output/test_dataflow_z3_AA_results.csv ]; then
  rm output/test_dataflow_z3_AA_results.csv
fi
time ./daisy --silent --no-stdout-print --results-csv="test_dataflow_z3_AA_results.csv" \
  --analysis=dataflow --rangeMethod=smt --errorMethod=affine \
  regression/testcases/allTests_dataflow_z3.scala

python3 regression/scripts/check_consistent_csv.py \
  regression/reference/dataflow_z3_AA_results.csv output/test_dataflow_z3_AA_results.csv


# echo ""
# echo "TEST 7 (longer): modular analysis"
# if [ -e output/test_modular_results.csv ]; then
#   rm output/test_modular_results.csv
# fi
# time ./daisy --silent --no-stdout-print --results-csv="test_modular_results.csv" \
#   --modularRoundOffEval regression/testcases/allTests_modular.scala

# python3 regression/scripts/check_consistent_csv.py \
#   regression/reference/modular_results.csv output/test_modular_results.csv


echo ""
echo "TEST 8 (quick): DS2L analysis (data structures)"
if [ -e output/test_ds2l_results.csv ]; then
  rm output/test_ds2l_results.csv
fi
time ./daisy --silent --no-stdout-print --results-csv="test_ds2l_results.csv" \
  --ds regression/testcases/allTests_ds2l.scala

python3 regression/scripts/check_consistent_csv.py \
  regression/reference/ds2l_results.csv output/test_ds2l_results.csv


echo ""
echo "TEST 9 (longer): rewriting optimization"
if [ -e output/test_rewriting_results.csv ]; then
  rm output/test_rewriting_results.csv
fi
time ./daisy --silent --no-stdout-print --results-csv="test_rewriting_results.csv" \
  --rewrite --rewrite-seed=42 --codegen --lang=Scala \
  --analysis=dataflow --rangeMethod=interval --errorMethod=affine \
  regression/testcases/allTests_rewriting.scala

python3 regression/scripts/check_consistent_csv.py \
  regression/reference/rewriting_results.csv output/test_rewriting_results.csv

# also need to check the generated output
diff output/allTests_rewriting.scala regression/reference/allTests_rewriting.scala


echo ""
echo "TEST 10 (quick): mixed-tuning optimization"
if [ -e output/test_tuning_results.csv ]; then
  rm output/test_tuning_results.csv
fi
time ./daisy --silent --no-stdout-print --results-csv="test_tuning_results.csv" \
  --mixed-tuning --rangeMethod=interval --errorMethod=affine \
  regression/testcases/allTests_tuning.scala

python3 regression/scripts/check_consistent_csv.py \
  regression/reference/tuning_results.csv output/test_tuning_results.csv

# also need to check the generated output
diff output/allTests_tuning.scala regression/reference/allTests_tuning.scala

