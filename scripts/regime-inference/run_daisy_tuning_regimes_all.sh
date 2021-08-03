#!/bin/bash --posix

top_folder=$1
seed=$2

echo "Running Daisy regime inference in folder ${top_folder}"

./scripts/regime-inference/compile_daisy.sh
echo "Compiled Daisy" >> "${top_folder}/daisy_tuning_all_log.txt"


# uniform 128 bit baseline
# if [ ! -d "${top_folder}/float128_baseline" ]; then
#   ./scripts/regime-inference/float128-baseline.sh ${top_folder}/bench ${top_folder}/float128_baseline
#   ./scripts/regime-inference/compile_and_run_C.sh ${top_folder}/float128_baseline ${top_folder}/float128_baseline_exe
#   echo "Finished float 128 baseline" >> "${top_folder}/daisy_tuning_all_log.txt"
# else
#   echo "Float 128 baseline already exists - SKIPPING" >> "${top_folder}/daisy_tuning_all_log.txt"
# fi


# # mixed-tuning without regime inference baseline
# if [ ! -d "${top_folder}/daisy_baseline" ]; then
#   ./scripts/regime-inference/daisy-baseline.sh ${top_folder}/bench ${top_folder}/daisy_baseline
#   ./scripts/regime-inference/compile_and_run_C.sh ${top_folder}/daisy_baseline ${top_folder}/daisy_baseline_exe
#   echo "Finished float 128 baseline" >> "${top_folder}/daisy_tuning_all_log.txt"
# else
#   echo "Daisy baseline already exists - SKIPPING" >> "${top_folder}/daisy_tuning_all_log.txt"
# fi


# regime inference
if [ ! -d "${top_folder}/bottomTop_${seed}" ]; then
  ./scripts/regime-inference/regime_inference.sh ${top_folder}/bench "${top_folder}/bottomTop_${seed}" "--regime-strategy=bottomTop"
  ./scripts/regime-inference/compile_and_run_C.sh "${top_folder}/bottomTop_${seed}" "${top_folder}/bottomTop_exe_${seed}"
  echo "Finished regime inference bottom-top" >> "${top_folder}/daisy_tuning_all_log.txt"
else
  echo "Daisy baseline already exists - SKIPPING" >> "${top_folder}/daisy_tuning_all_log.txt"
fi

if [ ! -d "${top_folder}/bottomGenetic_${seed}" ]; then
  ./scripts/regime-inference/regime_inference.sh ${top_folder}/bench "${top_folder}/bottomGenetic_${seed}" "--regime-strategy=bottomGenetic --regime-seed=${seed}"
  ./scripts/regime-inference/compile_and_run_C.sh "${top_folder}/bottomGenetic_${seed}" "${top_folder}/bottomGenetic_exe_${seed}"
  echo "Finished regime inference bottom-genetic with seed ${seed}" >> "${top_folder}/daisy_tuning_all_log.txt"
else
  echo "Bottom genetic ${seed} already exists - SKIPPING" >> "${top_folder}/daisy_tuning_all_log.txt"
fi

if [ ! -d "${top_folder}/naive_${seed}" ]; then
  ./scripts/regime-inference/regime_inference.sh ${top_folder}/bench "${top_folder}/naive_${seed}" "--regime-strategy=naive"
  ./scripts/regime-inference/compile_and_run_C.sh "${top_folder}/naive_${seed}" "${top_folder}/naive_exe_${seed}"
  echo "Finished regime inference bottom-up naive" >> "${top_folder}/daisy_tuning_all_log.txt"
else
  echo "Naive ${seed} already exists - SKIPPING" >> "${top_folder}/daisy_tuning_all_log.txt"
fi

if [ ! -d "${top_folder}/topDown_${seed}" ]; then
  ./scripts/regime-inference/regime_inference.sh ${top_folder}/bench "${top_folder}/topDown_${seed}" "--regime-strategy=topDown --topDownDepth=100"
  ./scripts/regime-inference/compile_and_run_C.sh "${top_folder}/topDown_${seed}" "${top_folder}/topDown_exe_${seed}"
  echo "Finished regime inference top-down" >> "${top_folder}/daisy_tuning_all_log.txt"
else
  echo "TopDown ${seed} already exists - SKIPPING" >> "${top_folder}/daisy_tuning_all_log.txt"
fi

if [ ! -d "${top_folder}/genetic_${seed}" ]; then
  ./scripts/regime-inference/regime_inference.sh ${top_folder}/bench "${top_folder}/genetic_${seed}" "--regime-strategy=genetic --regime-seed=${seed}"
  ./scripts/regime-inference/compile_and_run_C.sh "${top_folder}/genetic_${seed}" "${top_folder}/genetic_exe_${seed}"
  echo "Finished regime inference genetic with seed ${seed}" >> "${top_folder}/daisy_tuning_all_log.txt"
else
  echo "Genetic ${seed} already exists - SKIPPING" >> "${top_folder}/daisy_tuning_all_log.txt"
fi

# if [ ! -d "${top_folder}/bottomNoMerge_${seed}" ]; then
#   ./scripts/regime-inference/regime_inference.sh ${top_folder}/bench "${top_folder}/bottomNoMerge_${seed}" "--regime-strategy=naiveWithoutMerging"
#   ./scripts/regime-inference/compile_and_run_C.sh "${top_folder}/bottomNoMerge_${seed}" "${top_folder}/bottomNoMerge_exe_${seed}"
#   echo "Finished regime inference bottom-up without merging" >> "${top_folder}/daisy_tuning_all_log.txt"
# else
#   echo "BottomNoMerge ${seed} already exists - SKIPPING" >> "${top_folder}/daisy_tuning_all_log.txt"
# fi


# if [ ! -d "${top_folder}/bottomUp_${seed}" ]; then
#   ./scripts/regime-inference/regime_inference.sh ${top_folder}/bench "${top_folder}/bottomUp_${seed}" "--regime-strategy=bottomUp"
#   ./scripts/regime-inference/compile_and_run_C.sh "${top_folder}/bottomUp_${seed}" "${top_folder}/bottomUp_exe_${seed}"
#   echo "Finished regime inference bottom-up" >> "${top_folder}/daisy_tuning_all_log.txt"
# else
#   echo "BottomUp ${seed} already exists - SKIPPING" >> "${top_folder}/daisy_tuning_all_log.txt"
# fi


