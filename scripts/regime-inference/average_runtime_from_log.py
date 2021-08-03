import sys
import csv
import os

import statistics as stats

# usage: python average_runtime_from_log.py log1.txt log2.txt log3.txt

# dictionary: benchmark -> list-of-runtimes
data = {}

def getRuntimeFromLog(filename):
    file = open(filename, 'r')
    Lines = file.readlines()

    # Strips the newline character
    for (i, line) in enumerate(Lines):
        if 'tuning' in line:
            bench = line.replace('tuning', '').strip()

            if Lines[i + 1].strip() == "Command exited with non-zero status 124": #timeout
                time = Lines[i + 2].strip().split(' ')[0]
            else:
                time = Lines[i + 1].strip().split(' ')[0]

            if bench in data:
                data[bench].append(float(time))
            else:
                data[bench] = [float(time)]
    return data



for i in range(1, len(sys.argv)):
    filename = sys.argv[i]
    #print(f"collecting data from {filename}")
    getRuntimeFromLog(filename)

#print(data)

benchs = data.keys()

# compute average for each benchmark, and also average overall
# dictionary: benchmark -> average runtime
averages = {}
for bench in benchs:
    avrg = stats.mean(data[bench])
    averages[bench] =  avrg
    data[bench].append(avrg)   # append the average, so that we can print it to the CSV file

# take all the values and compute average of those
overall_average = stats.mean(list(averages.values()))
print(f"overall average: {overall_average}")


# write data into CSV file
csv = open(f"average_runtimes.csv", 'w')
# write header
csv.write(f"benchmark, {', '.join(sys.argv[1:])}, average\n")
# write data
for bench in benchs:
    #print(bench)
    #print(raw_data[bench])
    csv.write(f"{bench}, {', '.join(map(str, data[bench]))}\n")
csv.close