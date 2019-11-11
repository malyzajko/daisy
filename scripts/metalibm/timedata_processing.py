import numpy as np
from scipy import stats
#import matplotlib.pyplot as plt
import sys

with open(sys.argv[1]) as filestream:
	for line in filestream:
		values1 = line.split(",")

values1.pop()
values1 = [int(e) for e in values1]

values1.sort()

# remove the upper 10%
newList1 = values1[0 : int(len(values1) * 0.90)]

with open(sys.argv[2]) as filestream:
  for line in filestream:
    values2 = line.split(",")

values2.pop()
values2 = [int(e) for e in values2]

values2.sort()

# remove the upper 10%
newList2 = values2[0 : int(len(values2) * 0.90)]

mean1, stdDev1, min1, max1 = (np.mean(newList1), np.std(newList1), np.min(newList1), np.max(newList1))

mean2, stdDev2, min2, max2 = (np.mean(newList2), np.std(newList2), np.min(newList2), np.max(newList2))

print ("mathh   : %f, %f, %f, %f") % (mean1, stdDev1, min1, max1)

print ("metalibm: %f, %f, %f, %f") % (mean2, stdDev2, min2, max2)

# plt.hist(newList1, bins=50)
# plt.show()

# plt.hist(newList2, bins=50)
# plt.show()


