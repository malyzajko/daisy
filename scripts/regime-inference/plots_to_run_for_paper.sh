

# Assuming Daisy experiments have been run
# ./scripts/regime-inference/run_daisy_tuning_regimes_all.sh half_error 77
# ./scripts/regime-inference/run_daisy_tuning_regimes_all.sh half_error 1234
# ./scripts/regime-inference/run_daisy_tuning_regimes_all.sh half_error 4242

# copy over the log files and rename (name of the file is used to distinguish the series)
# cd half_error/
cp float128_baseline_exe/log.txt logs/seed77/baseline_log.txt
cp daisy_baseline_exe/log.txt logs/daisy_log.txt

# mkdir logs/seed77
cp bottomTop_exe_77/log.txt logs/seed77/bottomTop_log.txt
cp bottomGenetic_exe_77/log.txt logs/seed77/bottomGenetic_log.txt
cp bottomUp_exe_77/log.txt logs/seed77/bottomUp_log.txt
cp topDown_exe_77/log.txt logs/seed77/topDown_log.txt
cp genetic_exe_77/log.txt logs/seed77/genetic_log.txt

# Finally, generate the cactus plot
python3 scripts/regime-inference/cactus_plot.py half_error/logs/baseline_log.txt half_error/logs/daisy_log.txt half_error/logs/seed77/bottomUp_log.txt half_error/logs/seed77/bottomTop_log.txt half_error/logs/seed77/bottomGenetic_log.txt half_error/logs/seed77/topDown_log.txt half_error/logs/seed77/genetic_log.txt

# We also need some other statistics, like average/min/max/timeouts....


# for seed 1234
# mkdir logs/seed1234
cp bottomTop_exe_1234/log.txt logs/seed1234/bottomTop_log.txt;cp bottomGenetic_exe_1234/log.txt logs/seed1234/bottomGenetic_log.txt;cp bottomUp_exe_1234/log.txt logs/seed1234/bottomUp_log.txt;cp topDown_exe_1234/log.txt logs/seed1234/topDown_log.txt;cp genetic_exe_1234/log.txt logs/seed1234/genetic_log.txt

# run from top-directory
python3 scripts/regime-inference/cactus_plot.py half_error/logs/baseline_log.txt half_error/logs/daisy_log.txt half_error/logs/seed1234/bottomUp_log.txt half_error/logs/seed1234/bottomTop_log.txt half_error/logs/seed1234/bottomGenetic_log.txt half_error/logs/seed1234/topDown_log.txt half_error/logs/seed1234/genetic_log.txt


# for seed 4242
# mkdir logs/seed4242
cp bottomTop_exe_4242/log.txt logs/seed4242/bottomTop_log.txt;cp bottomGenetic_exe_4242/log.txt logs/seed4242/bottomGenetic_log.txt;cp bottomUp_exe_4242/log.txt logs/seed4242/bottomUp_log.txt;cp topDown_exe_4242/log.txt logs/seed4242/topDown_log.txt;cp genetic_exe_4242/log.txt logs/seed4242/genetic_log.txt

python3 scripts/regime-inference/cactus_plot.py half_error/logs/baseline_log.txt half_error/logs/daisy_log.txt half_error/logs/seed4242/bottomUp_log.txt half_error/logs/seed4242/bottomTop_log.txt half_error/logs/seed4242/bottomGenetic_log.txt half_error/logs/seed4242/topDown_log.txt half_error/logs/seed4242/genetic_log.txt


# compare different genetic ones:
python3 scripts/regime-inference/cactus_plot.py half_error/logs/baseline_log.txt half_error/logs/seed77/bottomGenetic77_log.txt half_error/logs/seed1234/bottomGenetic1234_log.txt half_error/logs/seed4242/bottomGenetic4242_log.txt




