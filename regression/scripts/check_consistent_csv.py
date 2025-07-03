# Checks two CSV files from Daisy for consistency
# Based on scripts developed by Oscar Wångmar
# Assumes that the CSV files have the following columns, separated by semicolons:
# function name, absolute error, relative error, real range, time
#
# Only processes the first full benchmark block before the first function repeats.

# Usage: python3 check_consistent_csv.py <reference.csv> <new.csv>

import sys
import csv
from pathlib import Path
from typing import Dict, List

def read_benchmark_csv(path: Path) -> Dict[str, str]:
    """Reads a benchmark CSV file and returns a dict of function name to all fields except the time.
    Only processes the first full benchmark block before the first function repeats.
    """
    data = {}
    seen_funcs = set()
    with path.open("r", encoding="utf-8") as f:
        reader = csv.reader(f, delimiter=';')
        next(reader)  # skip header
        for row in reader:
            if not row or all(cell.strip() == "" for cell in row):
                continue
            if len(row) < 3:
                continue
            func = row[0].strip()
            if func in seen_funcs:
                break  # stop processing on repeat
            seen_funcs.add(func)
            values = row[1:-1]  # skip time column
            data[func] = ";".join(values)
    return data

def compare_benchmarks(ref: Dict[str, str], new: Dict[str, str]) -> str:
    all_keys = sorted(set(ref) | set(new))
    if len(all_keys) == 0:
        print("ERROR: No keys/benchmarks found!")
        sys.exit(1)

    output: List[str] = ["# Benchmark Comparison Report\n"]
    num_faults = 0

    for key in all_keys:
        if key not in ref:
            output.append(f"- ❌ `{key}` is new in the second file.")
            num_faults = num_faults + 1
        elif key not in new:
            output.append(f"- ❌ `{key}` is missing in the second file.")
            num_faults = num_faults + 1
        elif ref[key] != new[key]:
            output.append(f"- ❌ `{key}` differs:")
            output.append(f"    - Old: `{ref[key]}`")
            output.append(f"    - New: `{new[key]}`")
            num_faults = num_faults + 1
        else:
            output.append(f"- ✅ `{key}` is unchanged.")

    return ("\n".join(output), num_faults)


if __name__ == "__main__":

    if len(sys.argv) != 3:
        print("Usage: python check_consistent_csv.py <reference.csv> <new.csv>")
        sys.exit(1)

    ref_file = Path(sys.argv[1])
    new_file = Path(sys.argv[2])

    ref_dict = read_benchmark_csv(ref_file)
    new_dict = read_benchmark_csv(new_file)

    report, num_faults = compare_benchmarks(ref_dict, new_dict)

    if num_faults == 0:
        print("All results consistent")
    else:
        report_path = Path("inconsistency_report_" + ref_file.stem + ".md")
        report_path.write_text(report, encoding="utf-8")
        print("Results NOT consistent")
        print(f"Report written to {report_path.resolve()}")
