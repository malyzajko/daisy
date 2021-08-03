#!/bin/bash --posix

top_folder=$1
seed=$2

## TODO: generate bench_fptuner benchmarks

echo "Running FPTuner regime inference in folder ${top_folder}"

./scripts/regime-inference/compile_daisy.sh
echo "Compiled Daisy" >> "${top_folder}/fptuner_tuning_all_log.txt"


# FPTuner baseline
# if [ ! -d "${top_folder}/fptuner_baseline" ]; then
#   ./scripts/regime-inference/fptuner-baseline.sh ${top_folder}/fptuner_bench ${top_folder}/fptuner_baseline
#   echo "Finished FPTuner baseline" >> "${top_folder}/fptuner_tuning_all_log.txt"
# else
#   echo "FPTuner baseline already exists - SKIPPING" >> "${top_folder}/fptuner_tuning_all_log.txt"
# fi

# Evaluate performance
#./scripts/regime-inference/compile_and_run_C.sh ${top_folder}/fptuner_baseline ${top_folder}/fptuner_baseline_exe


# regime inference with FPTuner
# if [ ! -d "${top_folder}/fptuner_naive" ]; then
#   ./scripts/regime-inference/fptuner-regimes.sh ${top_folder}/fptuner_bench_filtered "${top_folder}/fptuner_naive" "--regime-strategy=naive"
#   echo "Finished FPTuner regime inference with naive" >> "${top_folder}/fptuner_tuning_all_log.txt"
# else
#   echo "FPTuner regime-inf with naive ${seed} already exists - SKIPPING" >> "${top_folder}/fptuner_tuning_all_log.txt"
# fi

# Evaluate performance
#./scripts/regime-inference/compile_and_run_C.sh "${top_folder}/fptuner_naive" "${top_folder}/fptuner_naive_exe"


# regime inference with FPTuner
if [ ! -d "${top_folder}/fptuner_topDown_${seed}" ]; then
  ./scripts/regime-inference/fptuner-regimes.sh ${top_folder}/fptuner_bench_filtered "${top_folder}/fptuner_topDown_${seed}" "--regime-strategy=topDown"
  echo "Finished FPTuner regime inference with topDown" >> "${top_folder}/fptuner_tuning_all_log.txt"
else
  echo "FPTuner regime-inf with topDown ${seed} already exists - SKIPPING" >> "${top_folder}/fptuner_tuning_all_log.txt"
fi

# # Evaluate performance
./scripts/regime-inference/compile_and_run_C.sh "${top_folder}/fptuner_topDown_${seed}" "${top_folder}/fptuner_topDown_exe"


#if [ ! -d "${top_folder}/fptuner_bottomTop_${seed}" ]; then
#  ./scripts/regime-inference/fptuner-regimes.sh ${top_folder}/fptuner_bench_filtered "${top_folder}/fptuner_bottomTop_${seed}" "--regime-strategy=bottomTop"
#  echo "Finished FPTuner regime inference with bottomTop" >> "${top_folder}/fptuner_tuning_all_log.txt"
#else
#  echo "FPTuner regime-inf with bottomTop ${seed} already exists - SKIPPING" >> "${top_folder}/fptuner_tuning_all_log.txt"
#fi

# Evaluate performance
#./scripts/regime-inference/compile_and_run_C.sh "${top_folder}/fptuner_bottomTop_${seed}" "${top_folder}/fptuner_bottomTop_exe_${seed}"


#FPTuner using Daisy's regime inference
#if [ ! -d "${top_folder}/fptuner_daisy_bottomGenetic_${seed}" ]; then
#  ./scripts/regime-inference/fptuner-daisy-regimes.sh ${top_folder}/fptuner_bench_filtered "${top_folder}/fptuner_daisy_bottomGenetic_${seed}" "--regime-strategy=bottomGenetic --regime-seed=${seed} --rewrite-seed=${seed}"
#  echo "Finished FPTuner on Daisy's regimes with bottomGenetic" >> "${top_folder}/fptuner_tuning_all_log.txt"
#else
#  echo "FPTuner with Daisy regimes bottomGenetic ${seed} already exists - SKIPPING" >> "${top_folder}/fptuner_tuning_all_log.txt"
#fi

#Evaluate performance
#./scripts/regime-inference/compile_and_run_C.sh "${top_folder}/fptuner_daisy_bottomGenetic_${seed}" "${top_folder}/fptuner_daisy_bottomGenetic_exe_${seed}"

#if [ ! -d "${top_folder}/fptuner_daisy_bottomTop_${seed}" ]; then
#  ./scripts/regime-inference/fptuner-daisy-regimes.sh ${top_folder}/fptuner_bench_filtered "${top_folder}/fptuner_daisy_bottomTop_${seed}" "--regime-strategy=bottomTop"
#  echo "Finished FPTuner on Daisy's regimes with bottomGenetic" >> "${top_folder}/fptuner_tuning_all_log.txt"
#else
#  echo "FPTuner with Daisy regimes bottomGenetic ${seed} already exists - SKIPPING" >> "${top_folder}/fptuner_tuning_all_log.txt"
#fi

# Evaluate performance
#./scripts/regime-inference/compile_and_run_C.sh "${top_folder}/fptuner_daisy_bottomTop_${seed}" "${top_folder}/fptuner_daisy_bottomTop_exe_${seed}"



