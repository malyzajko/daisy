
# for this to work, compile the code with:
# ./daisy --codegen --lang=C --apfixed --precision=Fixed??? file

# script for compiling with vivado
# parameters:
# $1: benchmark function name
# $2: precision name

# NOTE: the source file name has to have form: benchName-precision.cpp

set bench [lindex $argv 0]
set src_dir [lindex $argv 1]
set project accel_$bench

puts stdout ">>>>>> benchmark:"
puts stdout $bench
puts stdout $project

# Commands to compile code to FPGA
# run with vivado_hls -f hls.tcl
#set src_dir "apfixed_Fixed32"
open_project -reset $project
set_top $bench
add_files $src_dir/$bench.cpp
#add_files -tb $src_dir/mmult_test.cpp   #for simulation
open_solution -reset "solution"
set_part {xc7z020clg484-1}
create_clock -period 10 -name default
#csim_design -clean
csynth_design
close_project
exit