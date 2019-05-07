#!/bin/bash --posix

# name of generated file (without suffix)
name=$1

cd output/
g++ -Winline -finline-limit=1200 -O2 -fPIC -c ${name}.c ${name}_benchmark.c
g++ -o ${name}_exe ${name}.o ${name}_benchmark.o -lm
