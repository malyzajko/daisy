import pandas as pd
import re

import os, sys

targeterr = sys.argv[1] # "large" or "small" or "subset_*""
baseline = "baseline" in sys.argv
macos = "macos" in sys.argv # means there will be no Xilinx reports

base = "output/baseline/" if baseline else "output/"

benchmarks = ["axisRotationX", "axisRotationY", "forwardk2jX", "pendulum2"] if targeterr.endswith("subset") else ["axisRotationX", "axisRotationY",
              "forwardk2jX", "forwardk2jY",
              "xu1", "xu2",
              "rodriguesRotation",
              "sinxx10",
              "pendulum1", "pendulum2",
              "predictGaussianNB", "predictSVC",
              "predictMLPLogistic"] 

# Number of elementary fnc calls
fncs = {"axisRotationX": 2, "axisRotationY": 2,
        "forwardk2jX": 2, "forwardk2jY": 2,
        "xu1": 3, "xu2": 3,
        "rodriguesRotation": 2,
        "sinxx10": 1,
        "pendulum1": 1, "pendulum2": 1,
        "predictGaussianNB": 5, "predictSVC": 1,
        "predictMLPLogistic": 1}
target_large = {"axisRotationX": '1.49e-6',
        "axisRotationY": '1.49e-6',
        "forwardk2jX": '8.39e-7', 
        "forwardk2jY": '4.89e-7',
        "xu1": '1.89e-6', "xu2": '1.88e-6',
        "rodriguesRotation": '1.70e-4',
        "sinxx10": '2.51e-5',
        "pendulum1": '4.79e-7', "pendulum2": '1.07e-6',
        "predictGaussianNB": '4.15e-3',
        "predictSVC": '1.46e-2',
        "predictMLPLogistic": '2.15e-2'}
target_small = {"axisRotationX": '1.49e-10',
        "axisRotationY": '1.49e-10',
        "forwardk2jX": '8.39e-11', 
        "forwardk2jY": '4.89e-11',
        "xu1": '1.89e-10', "xu2": '1.88e-10',
        "rodriguesRotation": '1.70e-8',
        "sinxx10": '2.51e-9',
        "pendulum1": '4.79e-11', "pendulum2": '1.07e-10',
        "predictGaussianNB": '4.15e-7',
        "predictSVC": '1.46e-6',
        "predictMLPLogistic": '2.15e-6'}

target = target_large if targeterr == "large" else target_small

if macos and baseline:
    print("No baseline results to present. Xilinx Vivado HLS is not installed on MacOS.")
    exit(0)

# Parse Xilinx reports
if not macos:
    mcycles={}
    for bench in benchmarks:
        filename = base + 'results_' + targeterr + '/' + bench + '_csynth.rpt'
        if os.path.isfile(filename):
            df = pd.read_csv(filename, skiprows=30, nrows=1)
            line = re.sub('[\s+]', '', df.iat[0, 0])
            values = line.split('|')

            mint = values[1]
            maxt = values[2]

            latency = mint if (mint == maxt) else mint + '-' + maxt
            mcycles[bench] = latency
        else:
            print('Report ' + filename + ' does not exist.\n')
            mcycles[bench] = '-'

if baseline:
    # Print results
    print('----------------------------------------------')
    print("|                    | Machine |   Target    |")
    print("|     Benchmark      | cycles  |   error     |")
    print('----------------------------------------------')

    for b in benchmarks:
        print('| ' + "{:<18}".format(b) + ' | ' + "{:>7}".format(str(mcycles[b])) + ' | ' + "{:>11}".format(target[b]) + ' |')

    print('----------------------------------------------')

else:
    # Parse Daisy log
    log = open(base + 'logs/daisy_' + targeterr +'.log')

    # strings to look for in the log:
    log_benchmark = '******* running Daisy on '
    log_error = 'Absolute error:'
    log_arithmops = 'Number of arithmetic operations in generated code:'
    log_time = 'real'

    accuracy = {}
    poly_size = {}
    times = {}

    bench = ""
    for line in log:
        if line.startswith(log_benchmark):
            bench = line[len(log_benchmark):].strip()
            # print(bench)
        if line.find(log_error) != -1:
            start = line.index(log_error)
            err = line[(start + len(log_error)):].strip()
            accuracy[bench] = err
            # print(err)
        if line.find(log_arithmops) != -1:
            start = line.index(log_arithmops)
            arithmops = line[(start + len(log_arithmops)):].strip()
            if bench in poly_size:
                tmp = int(poly_size[bench])
                poly_size[bench] = tmp + int(arithmops)
            else:
                poly_size[bench] = arithmops
        # print(arithmops)
        if line.find(log_time) != -1:
            start = line.index(log_time)
            time = line[(start + len(log_time)):].strip()

            # remove 0m
            if time.startswith('0m'):
                time = time[2:]
            elif time.find('m') != -1:
                between = time.index('m')+1
                tmp = time[between:]
                time = time[:between] + ' ' + tmp

            if not bench in times:
                times[bench] = time

    if macos:
        # Print results
        print('-------------------------------------------------------------------------------------------------------------')
        print("|                    |   Target    |                        || # elem. | # arithm. |   Synthesis   |")
        print("|     Benchmark      |   error     |         Accuracy       ||  fnc.   |     ops   |     time      |")
        print('-------------------------------------------------------------------------------------------------------------')

        for b in benchmarks:
            print('| ' + "{:<18}".format(b) + ' | ' + "{:>11}".format(target[b]) + ' | '
                  + "{:<22}".format(str(accuracy[b])) + ' || ' + "{:>7}".format(str(fncs[b]))+ ' | '+ "{:>9}".format(str(poly_size[b]))+ ' | ' + "{:>13}".format(str(times[b])) + ' |')

        print('-------------------------------------------------------------------------------------------------------------')
    else:
        # Print results
        print('-------------------------------------------------------------------------------------------------------------')
        print("|                    | Machine |   Target    |                        || # elem. | # arithm. |   Synthesis   |")
        print("|     Benchmark      | cycles  |   error     |         Accuracy       ||  fnc.   |     ops   |     time      |")
        print('-------------------------------------------------------------------------------------------------------------')

        for b in benchmarks:
            print('| ' + "{:<18}".format(b) + ' | ' + "{:>7}".format(str(mcycles[b])) + ' | ' + "{:>11}".format(target[b]) + ' | '
                 + "{:<22}".format(str(accuracy[b])) + ' || ' + "{:>7}".format(str(fncs[b]))+ ' | '+ "{:>9}".format(str(poly_size[b]))+ ' | ' + "{:>13}".format(str(times[b])) + ' |')

        print('-------------------------------------------------------------------------------------------------------------')
