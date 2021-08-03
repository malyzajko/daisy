
import csv
import sys
import statistics as stats
import os

# usage: python3 parse_regime_inference_info.sh


# dictionary: benchmark -> list of regimes for different methods
header = []
all_data = {}

# returns a dictionary: benchmark -> # regimes
def parseCSV(filename):
    regimes = []
    header.append(filename)
    with open(filename) as csvDataFile:
        csvReader = csv.reader(csvDataFile)
        for row in csvReader:
            if len(row) == 5:
                #print(row)
                bench = row[0].strip()
                num_reg = row[1].strip()
                regimes.append(int(num_reg))

                if bench in all_data:
                    all_data[bench].append(num_reg)
                else:
                    all_data[bench] = [num_reg]
            else:
                print(f"malformed row: {row}")

    count_more_than_1_regime = sum(i > 1 for i in regimes)
    avrg_regimes = stats.mean(regimes)
    print(f"{filename}: num bench with >1 regime: {count_more_than_1_regime}, avrg num regimes: {avrg_regimes}")


for i in range(1, len(sys.argv)):
    filename = sys.argv[i]
    parseCSV(filename)

csv = open("regime_info.csv", 'w')

# header
csv.write(f"benchmark, {', '.join(header)}\n")

for bench in list(all_data.keys()):
    csv.write(f"{bench}, {', '.join(all_data[bench])}\n")