
sbt compile

if [ ! -e daisy ]; then
  sbt script
fi

# run Daisy on each testfile
for file in testcases/ints/*.scala; do
  echo "*******"
  echo ${file}
  time ./daisy --silent --results-csv=ints_real_analysis.csv ${file}

done