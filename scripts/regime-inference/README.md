# Instructions for Running Regime Inference Evaluation

## System Requirements

* GCC (not Clang): needed for quadmath
* python package 'csv': needed for computing stats
* MPFR (for transcendentals in Daisy)
* z3 (for additional constraints in Daisy)

## Compile Daisy and generate script to run

  `./scripts/regime-inference/compile_daisy.sh`

## Benchmark Generation

  `./scripts/regime-inference/generate_tuning_benchmarks.sh testcases/fpbench ${benchmarks}`


## Generate Baseline Mixed-Tuned Programs

  uniform 128 bit baseline:

  `./scripts/float128-baseline.sh ${benchmarks} output_dir`

  with Daisy:

  `./scripts/regime-inference/daisy-baseline.sh ${benchmarks} output_dir`

  with FPTuner:

  `./scripts/regime-inference/fptuner-baseline.sh ${benchmarks} ${output_dir}/fptuner-baseline`


## Generate Regime-Inference for Mixed-Precision Tuning with Daisy

  `./scripts/regime-inference/regime_inference.sh ${benchmarks} output_dir ${options}`

  where options depend on the method used:

  * naive method: --regime-strategy=naive

  * bottom-up method: --regime-strategy=bottomUp

  * top-down method: "--regime-strategy=topDown --topDownDepth=100"

  * genetic method: --regime-strategy=genetic

  * bottom-top method: --regime-strategy=bottomTop
  
  * bottom-genetic method: --regime-strategy=bottomGenetic

## Generate Regime-Inference for Mixed-Precision Tuning with FPTuner

  combination of Daisy and FPTuner:

  `./scripts/regime-inference/fptuner-daisy-regimes.sh ${benchmarks} output_dir --regime-strategy=bottomUp`

  only FPTuner:

  `./scripts/regime-inference/fptuner-regimes.sh ${benchmarks} output_dir --regime-strategy=bottomUp`

## Compile and Run (mixed-tuned programs)

  `./scripts/regime-inference/compile_and_run_C.sh ${output_dir} ${output_dir_exe}`


## Generate Baseline Rewritten Programs

  `./scripts/regime-inference/rewriting-baseline.sh ${benchmarks} output_dir`


## Generate Regime-Inference for Rewriting

  `./scripts/regime-inference/regime_inference_rewriting.sh ${benchmarks} output_dir ${options}`

  where options are as for mixed-tuning rewriting


## Creating Plots

  Cactus plots (log files need to be named 'METHOD_log.txt', where the METHOD is used in the legend of the plot):

  `python3 ./scripts/regime-inference/mixed_tuning_data_analysis.py baseline_log.txt method1_log.txt method2_log.txt`

  Rewriting plot:

  `python ./scripts/regime-inference/plot_rewriting.py regime-rewriting-baseline.csv regime-rewriting.csv`

  Create CSV file with regime inference running times from several runs:

  `python ./scripts/regime-inference/average_runtime_from_log.py log1.txt log2.txt log3.txt`

