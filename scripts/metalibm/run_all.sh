

# generates approximate implementations
# executables and generated sources are saved in the output/ folder

# setting: double precision, small target error, output folder 'float64_small', no compound functions, equal error distribution, table width: 8 bits
./scripts/metalibm/metalibm_script.sh Float64 TransBenchsErrorBoundsSmall.scala float64_small 0 equal 8 2>&1 | tee analysis_log_small.txt

# setting: double precision, middle target error, output folder 'float64_middle', no compound functions, equal error distribution, table width: 8 bits
./scripts/metalibm/metalibm_script.sh Float64 TransBenchsErrorBoundsMiddle.scala float64_middle 0 equal 8 2>&1 | tee analysis_log_middle.txt

# setting: double precision, large target error, output folder 'float64_large', no compound functions, equal error distribution, table width: 8 bits
./scripts/metalibm/metalibm_script.sh Float64 TransBenchsErrorBoundsLarge.scala float64_large 0 equal 8 2>&1 | tee analysis_log_large.txt

# setting: double precision, large target error, output folder 'float64_large_deriv', no compound functions, derivative-based error distribution, table width: 8 bits
./scripts/metalibm/metalibm_script.sh Float64 TransBenchsErrorBoundsLarge.scala float64_large_deriv 0 deriv 8 2>&1 | tee analysis_log_large_deriv.txt

# setting: double precision, large target error, output folder 'tableWidth_large', no compound functions, equal error distribution, no table
./scripts/metalibm/metalibm_script.sh Float64 TransBenchsErrorBoundsLarge.scala tableWidth_large 0 equal 0 2>&1 | tee analysis_log_tablewidth.txt

# setting: double precision, large target error, output folder 'compound_mid_large', compound functions with depth 1, equal error distribution, table width: 8 bits
./scripts/metalibm/metalibm_script.sh Float64 TransBenchsErrorBoundsLarge.scala compound_mid_large 1 equal 8 2>&1 | tee analysis_log_compound_mid.txt

# setting: double precision, large target error, output folder 'compound_large_large', compound functions (approximate everything), equal error distribution, table width: 8 bits
./scripts/metalibm/metalibm_script.sh Float64 TransBenchsErrorBoundsLarge.scala compound_large_large 1000 equal 8 2>&1 | tee analysis_log_compound_large.txt

# setting: single precision, middle target error, output folder 'float32_middle', no compound functions, equal error distribution, table width: 8 bits
./scripts/metalibm/metalibm_script.sh Float32 TransBenchsErrorBoundsMiddleFloat32.scala float32_middle 0 equal 8 2>&1 | tee analysis_log_float32_middle.txt
