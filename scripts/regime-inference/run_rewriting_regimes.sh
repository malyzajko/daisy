#!/bin/bash --posix

top_folder=$1
seed=$2

echo "Running Daisy regime inference with rewriting in folder ${top_folder}"

./scripts/regime-inference/compile_daisy.sh
echo "Compiled Daisy" >> "${top_folder}/daisy_rewriting_all_log.txt"


# baseline
if [ ! -d "${top_folder}/rw_baseline_${seed}" ]; then
  ./scripts/regime-inference/rewriting-baseline.sh ${top_folder}/bench "${top_folder}/rw_baseline_${seed}" "--rewrite-seed=${seed}"
  echo "Finished rewriting baseline ${seed}" >> "${top_folder}/daisy_rewriting_all_log.txt"
else
  echo "Rewrite baseline ${seed} already exists - SKIPPING" >> "${top_folder}/daisy_rewriting_all_log.txt"
fi

if [ ! -d "${top_folder}/rw_reinf_naive_${seed}" ]; then
  ./scripts/regime-inference/regime_inference_rewriting.sh ${top_folder}/bench "${top_folder}/rw_reinf_naive_${seed}" "--regime-strategy=naive --rewrite-seed=${seed}"
  echo "Finished rewriting regime inference naive ${seed}" >> "${top_folder}/daisy_rewriting_all_log.txt"
else
  echo "Rewrite regime-inf ${seed} for naive already exists - SKIPPING" >> "${top_folder}/daisy_rewriting_all_log.txt"
fi

# regime inference with cheaper, best strategy
if [ ! -d "${top_folder}/rw_reinf_bottomTop_${seed}" ]; then
  ./scripts/regime-inference/regime_inference_rewriting.sh ${top_folder}/bench "${top_folder}/rw_reinf_bottomTop_${seed}" "--regime-strategy=bottomTop --rewrite-seed=${seed}"
  echo "Finished rewriting regime inference bottomTop ${seed}" >> "${top_folder}/daisy_rewriting_all_log.txt"
else
  echo "Rewrite regime-inf ${seed} for bottomTop already exists - SKIPPING" >> "${top_folder}/daisy_rewriting_all_log.txt"
fi

# if [ ! -d "${top_folder}/rw_reinf_bottomUp_${seed}" ]; then
#   ./scripts/regime-inference/regime_inference_rewriting.sh ${top_folder}/bench "${top_folder}/rw_reinf_bottomUp_${seed}" "--regime-strategy=bottomUp --rewrite-seed=${seed}"
#   echo "Finished rewriting regime inference bottomUp ${seed}" >> "${top_folder}/daisy_rewriting_all_log.txt"
# else
#   echo "Rewrite regime-inf ${seed} for bottomUp already exists - SKIPPING" >> "${top_folder}/daisy_rewriting_all_log.txt"
# fi


if [ ! -d "${top_folder}/rw_reinf_topDown_${seed}" ]; then
  ./scripts/regime-inference/regime_inference_rewriting.sh ${top_folder}/bench "${top_folder}/rw_reinf_topDown_${seed}" "--regime-strategy=topDown --topDownDepth=100 --rewrite-seed=${seed}"
  echo "Finished rewriting regime inference topDown ${seed}" >> "${top_folder}/daisy_rewriting_all_log.txt"
else
  echo "Rewrite regime-inf ${seed} for topDown already exists - SKIPPING" >> "${top_folder}/daisy_rewriting_all_log.txt"
fi

if [ ! -d "${top_folder}/rw_reinf_genetic_${seed}" ]; then
  ./scripts/regime-inference/regime_inference_rewriting.sh ${top_folder}/bench "${top_folder}/rw_reinf_genetic_${seed}" "--regime-strategy=genetic --regime-seed=${seed} --rewrite-seed=${seed}"
  echo "Finished rewriting regime inference genetic ${seed}" >> "${top_folder}/daisy_rewriting_all_log.txt"
else
  echo "Rewrite regime-inf ${seed} for genetic already exists - SKIPPING" >> "${top_folder}/daisy_rewriting_all_log.txt"
fi

# regime inference with best strategy
if [ ! -d "${top_folder}/rw_reinf_bottomGenetic_${seed}" ]; then
  ./scripts/regime-inference/regime_inference_rewriting.sh ${top_folder}/bench "${top_folder}/rw_reinf_bottomGenetic_${seed}" "--regime-strategy=bottomGenetic --regime-seed=${seed} --rewrite-seed=${seed}"
  echo "Finished rewriting regime inference bottomGenetic ${seed}" >> "${top_folder}/daisy_rewriting_all_log.txt"
else
  echo "Rewrite regime-inf ${seed} for bottomGenetic already exists - SKIPPING" >> "${top_folder}/daisy_rewriting_all_log.txt"
fi