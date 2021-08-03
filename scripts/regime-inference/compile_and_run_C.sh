#!/bin/bash --posix

# Script to compile and run mixed-precision tuning programs
bench_folder=$1
output_folder=$2
input=$bench_folder/*

#rm -rf $output_folder
mkdir $output_folder

touch "${output_folder}/log.txt"

for file in $input
do
  #echo $file
  filename=${file##*/}
  benchname="${filename%.cpp}"

  if ! [[ $benchname =~ "_benchmark" || $benchname =~ "log.txt" ]]
  then
    echo "compiling ${benchname}"

    #g++-10 /usr/local/opt/gcc/lib/gcc/10/libquadmath.dylib -Winline -O2 -fPIC -DNDEBUG "${bench_folder}/${benchname}_benchmark.cpp" \
    #  "${bench_folder}/${benchname}.cpp" -o "${benchname}.exe"
    
    g++ -Winline -O2 -fPIC -DNDEBUG "${bench_folder}/${benchname}_benchmark.cpp" \
      "${bench_folder}/${benchname}.cpp" -o "${benchname}.exe" -lquadmath

    echo "executing ${benchname}"

    eval "./${benchname}.exe" 2>&1 | tee -a "${output_folder}/log.txt"
    eval "./${benchname}.exe" 2>&1 | tee -a "${output_folder}/log.txt"
    eval "./${benchname}.exe" 2>&1 | tee -a "${output_folder}/log.txt"

    # move to the right place
    mv "${benchname}.exe" "${output_folder}/"
  fi

done
