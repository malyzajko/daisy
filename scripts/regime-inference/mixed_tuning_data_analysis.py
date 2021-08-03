import csv
import sys
import statistics as stats
import os
import matplotlib.pyplot as plt

# usage: python3 mixed_tuning_data_analysis.py baseline_128float_log.txt method1_log.txt method2_log.txt ...

# output: cactus plot, csv file with average running times, csv file with improvements

# returns a dictionary, mapping benchmark names to average running times
def computeAveragesFromLogFile(filename):
    raw = {}
    with open(filename) as csvDataFile:
        csvReader = csv.reader(csvDataFile)
        for row in csvReader:
            if len(row) == 3:
                benchmark = row[1].strip()
                time = row[2].strip()
                if benchmark in raw:
                  raw[benchmark].append(float(time))
                else:
                  raw[benchmark] = [float(time)]

    average = {}
    for bench in raw.keys():
        runtimes = raw[bench]
        avrg = stats.mean(runtimes)
        average[bench] = avrg

    return average

# input: benchmarks: list of benchmarks
def writeDictionaryToCSV(benchmarks, avrg_runtimes, filename):
    csv = open(f"{filename}.csv", 'w')

    # write header
    labels = avrg_runtimes.keys()
    csv.write(f"benchmark, {', '.join(labels)}\n")

    # write averages for each benchmark
    for bench in benchmarks:
        #print(bench)
        runtimes = []
        #print(runtimes)
        for l in labels:
            #print(average_running_times[l][bench])
            if bench in avrg_runtimes[l]:
                runtimes.append(str(avrg_runtimes[l][bench]))
            else:
                runtimes.append("-")
        #print(runtimes)
        csv.write(f"{bench}, {', '.join(runtimes)}\n")
    csv.close

float128_baseline_file = sys.argv[1] # first file needs to be the baseline
print(f"baseline file: {float128_baseline_file}")

baseline_average = computeAveragesFromLogFile(float128_baseline_file)
#print(baseline_average)

benchmarks = list(baseline_average.keys())

# dictionary to keep all the data, indexed by filename (- log.txt)
improvements = {}    # improvement in percent over baseline
average_running_times = {}

# counts the number of benchmarks for which improvement < 0.98
count_bench_improved = {}
average_improvements = {}

for i in range(2, len(sys.argv)):
    print(f"index {i}, file: {sys.argv[i]}")
    filename = sys.argv[i]
    #print(f"filename: {filename}")
    avrg_runtimes = computeAveragesFromLogFile(filename)
    #print(avrg_runtimes)
    label = os.path.basename(filename).replace("_log.txt", "")
    #print(label)
    average_running_times[label] = avrg_runtimes

    improv = {}
    count_improved = 0
    num_finished = 0
    total_improv = 0
    for bench in benchmarks:
        if bench in avrg_runtimes:
            # compute improvement
            impr = (baseline_average[bench] - avrg_runtimes[bench]) / baseline_average[bench]
            improv[bench] = impr

            # for calculating the number of benchmarks that were improved
            if impr > 0.02:
                count_improved = count_improved + 1

            # for calculating the average
            num_finished = num_finished + 1
            total_improv = total_improv + impr
        else:
            improv[bench] = "-"

    improvements[label] = improv
    count_bench_improved[label] = count_improved
    average_improvements[label] = total_improv / num_finished


all_running_times = average_running_times.copy()
all_running_times["baseline"] = baseline_average
writeDictionaryToCSV(benchmarks, all_running_times, "average_running_times")
writeDictionaryToCSV(benchmarks, improvements, "running_time_improvements")

# ---------------------
# Data analysis to produce table in paper
# ---------------------
print("Number of benchmarks improved (improvement > 0.02%)")
print(count_bench_improved)
print("Average improvements:")
print(average_improvements)


# unique best:


# ---------------------
# create a cactus plot
# ---------------------
x_axis = range(0, len(benchmarks))

# 'o', '.', ',', 'x', '+', 'v', '^', '<', '>', 's', 'd'
marker = ['o', 'v', 'd', 'o', 'x', 'd', '.', '.', '.', '.', '.']
colors = ['blue', 'green', 'red', 'purple', 'black', 'black']

#f, ax = plt.subplots()
f, ax = plt.subplots(figsize=(5.0, 5.0))

for (i, seriesName) in enumerate(improvements.keys()):
    series = list(improvements[seriesName].values())

    # replace "-" and outliers with default -0.1
    seriesNumeric = list(map(lambda x: -0.1 if ((x == "-") or (x < 0.0)) else x , series))

    # sort by size
    seriesNumeric.sort(reverse=True)

    plt.plot(x_axis, seriesNumeric, marker[i], color=colors[i], markersize=3, label=seriesName)

plt.legend(loc='upper right')
# add a horizontal line at 1
x_limit = len(benchmarks) + 2
plt.plot([0.0, x_limit], [1.0, 1.0], '--', color='grey')
plt.xlim(-2, x_limit)
ax.set_ylabel('improvement over baseline')
ax.set_xlabel('benchmarks')
#plt.ylim(0.0, 1.5) test_str.rsplit(spl_char, 1)[0]
name = sys.argv[-1].split("/")[0]
print(name)
f.savefig(f"cactus_{name}.pdf", dpi=300, bbox_inches='tight') #,rasterized=True
plt.show()


