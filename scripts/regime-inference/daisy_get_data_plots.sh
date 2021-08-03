#!/bin/bash --posix

# Here is how to generate all of the plots and data for tables using scripts

# Cactus plot, also prints # benchmarks improved and average improvement
echo "HALF-ERROR"
python3 scripts/regime-inference/mixed_tuning_data_analysis.py half_error/logs/daisy_log.txt \
  half_error/logs/seed77/bottomGenetic_log.txt half_error/logs/seed77/bottomTop_log.txt \
  half_error/logs/seed77/bottomUp_log.txt half_error/logs/seed77/topDown_log.txt half_error/logs/seed77/genetic_log.txt

echo "ORDER-ERROR"
python3 scripts/regime-inference/mixed_tuning_data_analysis.py order_error/logs/daisy_log.txt \
  order_error/logs/seed77/bottomGenetic_log.txt order_error/logs/seed77/bottomTop_log.txt \
  order_error/logs/seed77/bottomUp_log.txt order_error/logs/seed77/topDown_log.txt order_error/logs/seed77/genetic_log.txt


# Get info about # regimes
echo "HALF-ERROR"
python3 scripts/regime-inference/parse_regime_inference_info.py half_error/logs/seed77/bottomGenetic_regime-tuning.csv \
  half_error/logs/seed77/bottomTop_regime-tuning.csv half_error/logs/seed77/bottomUp_regime-tuning.csv \
  half_error/logs/seed77/topDown_regime-tuning.csv half_error/logs/seed77/genetic_regime-tuning.csv

echo "ORDER-ERROR"
python3 scripts/regime-inference/parse_regime_inference_info.py order_error/logs/seed77/bottomGenetic_regime-tuning.csv \
  order_error/logs/seed77/bottomTop_regime-tuning.csv order_error/logs/seed77/bottomUp_regime-tuning.csv \
  order_error/logs/seed77/topDown_regime-tuning.csv order_error/logs/seed77/genetic_regime-tuning.csv



# Average running times of regime inference:

echo "average running time for half error - bottomGenetic"
python3 scripts/regime-inference/average_runtime_from_log.py half_error/bottomGenetic_77/runtime_log.txt \
  half_error/bottomGenetic_1234/runtime_log.txt half_error/bottomGenetic_4242/runtime_log.txt

echo "average running time for half error - bottomTop"
python3 scripts/regime-inference/average_runtime_from_log.py half_error/bottomTop_77/runtime_log.txt \
  half_error/bottomTop_1234/runtime_log.txt half_error/bottomTop_4242/runtime_log.txt

echo "average running time for half error - topDown"
python3 scripts/regime-inference/average_runtime_from_log.py half_error/topDown_77/runtime_log.txt \
  half_error/topDown_1234/runtime_log.txt half_error/topDown_4242/runtime_log.txt

echo "average running time for half error - genetic"
python3 scripts/regime-inference/average_runtime_from_log.py half_error/genetic_77/runtime_log.txt \
  half_error/genetic_1234/runtime_log.txt half_error/genetic_4242/runtime_log.txt

# echo "average running time for half error - naive"
# python3 scripts/regime-inference/average_runtime_from_log.py half_error/naive_77/runtime_log.txt \
#   half_error/naive_1234/runtime_log.txt half_error/naive_4242/runtime_log.txt

echo " "

echo "average running time for order error - bottomGenetic"
python3 scripts/regime-inference/average_runtime_from_log.py order_error/bottomGenetic_77/runtime_log.txt \
  order_error/bottomGenetic_1234/runtime_log.txt order_error/bottomGenetic_4242/runtime_log.txt

echo "average running time for order error - bottomTop"
python3 scripts/regime-inference/average_runtime_from_log.py order_error/bottomTop_77/runtime_log.txt \
  order_error/bottomTop_1234/runtime_log.txt order_error/bottomTop_4242/runtime_log.txt

echo "average running time for order error - topDown"
python3 scripts/regime-inference/average_runtime_from_log.py order_error/topDown_77/runtime_log.txt \
  order_error/topDown_1234/runtime_log.txt order_error/topDown_4242/runtime_log.txt

echo "average running time for order error - genetic"
python3 scripts/regime-inference/average_runtime_from_log.py order_error/genetic_77/runtime_log.txt \
  order_error/genetic_1234/runtime_log.txt order_error/genetic_4242/runtime_log.txt

# echo "average running time for order error - naive"
# python3 scripts/regime-inference/average_runtime_from_log.py order_error/naive_77/runtime_log.txt \
#   order_error/naive_1234/runtime_log.txt order_error/naive_4242/runtime_log.txt
