import csv
import sys
import statistics as stats
import os
import matplotlib.pyplot as plt

# usage: python plot_rewriting.py regime-rewriting-baseline.csv regime-rewriting.csv

# output: scatter plot, csv file with max errors

filenameBaseline = sys.argv[1]
filenameRegime = sys.argv[2]

# holds: benchmark -> [baseline max error, regime min error, regime max error, rel. max error improvement]
raw_data = {}
reg_improv = []  # regime inference improvements
base_improv = []  # baseline improvements
diff_improv = [] # difference in improvement (reginf - baseline)
num_bench_improved = 0

# read in baseline file (bench, oldError, newError)
with open(filenameBaseline) as csvDataFile:
    csvReader = csv.reader(csvDataFile)
    for row in csvReader:
        assert (len(row) == 3)
        benchmark = row[0].strip()
        oldError = row[1].strip()
        newError = row[2].strip()
        raw_data[benchmark] = [oldError, newError]


# read in regime inference file (benchmark, # regimes, minError, maxError, weightedAvrgError)
with open(filenameRegime) as csvDataFile:
    csvReader = csv.reader(csvDataFile)
    for row in csvReader:
        assert(len(row) == 5)
        benchmark = row[0].strip()
        numRegimes = row[1].strip()
        minError = row[2].strip()
        maxError = row[3].strip()
        avrgError = row[4].strip()
        if benchmark in raw_data: # baseline can time out too
            raw_data[benchmark] = raw_data[benchmark] + [numRegimes, minError, maxError, avrgError]

            # compute and append improvements
            data = raw_data[benchmark]
            origError = float(data[0])
            rwBaseError = float(data[1])

            improvBaseline = (origError - rwBaseError) / origError
            improvRegimesMax = (origError - float(maxError)) / origError
            raw_data[benchmark] = raw_data[benchmark] + [str(improvBaseline), str(improvRegimesMax)]
            if (improvRegimesMax > improvBaseline):
                num_bench_improved = num_bench_improved + 1

            # filter, only take those values with any improvement, in one or the other method
            if (improvRegimesMax != 0.0 or improvBaseline != 0.0):
                reg_improv.append(improvRegimesMax)
                base_improv.append(improvBaseline)
                diff_improv.append(improvRegimesMax - improvBaseline)

print(f"# benchmarks improved: {num_bench_improved}")
print(len(reg_improv))
print(len(base_improv))

# write data into CSV file
csv = open(f"rewriting_data.csv", 'w')
# write header
csv.write(f"benchmark, original error, rewriting baseline, num regimes, min error, max error, weighted avrg error, improv. baseline, improv. rewriting\n")
# write data
benchs = raw_data.keys()
for bench in benchs:
    #print(bench)
    #print(raw_data[bench])
    csv.write(f"{bench}, {', '.join(raw_data[bench])}\n")
csv.write(f"# benchmarks improved: {num_bench_improved}")
csv.close

# create a scatter plot
# f, ax = plt.subplots()
# plt.scatter(reg_improv, base_improv, marker='o')
# plt.xlabel("regime inference max error improvements")
# plt.ylabel("baseline rewriting improvement")
# # add the 45 degree line
# plt.plot([0.0, 1.0], [0.0, 1.0], '--', color='grey')
# plt.xlim(0.0, 1.0)
# plt.ylim(0.0, 1.0)

# f.savefig(f"scatter_rewriting.pdf", dpi=300, bbox_inches='tight') #,rasterized=True
# plt.show()

# create bar chart
#x = range(1, len(reg_improv))

#width = 0.35

x_pos = [i + 1 for i, _ in enumerate(diff_improv)]
#y_pos = [i + width for i, _ in enumerate(base_improv)]


#plt.figure(figsize=(15,3))
f, ax = plt.subplots(figsize=(12, 3))

# add horizontal lines
x_limit = len(diff_improv) + 2

#plt.plot([-2.0, x_limit], [0.2, 0.2], '--', color='grey', linewidth=1)

#ax.grid(zorder=0)
plt.bar(x_pos, diff_improv, color='steelblue', label='Baseline')
ax.set_axisbelow(True)
ax.grid(linestyle='--', linewidth='0.5', color='grey', axis='y')

#plt.bar(y_pos, reg_improv, width, color='green', label='Reg.Inf.')
plt.xlabel("Benchmarks")
plt.ylabel("Difference in Error Improvement")
plt.xlim(-2, x_limit)
#plt.title("Energy output from various fuel sources")

plt.xticks(x_pos, x_pos)
# too many labels
for label in ax.xaxis.get_ticklabels()[::2]:
    label.set_visible(False)

f.savefig(f"rewriting_bar.pdf", dpi=300, bbox_inches='tight') #,rasterized=True
plt.show()



