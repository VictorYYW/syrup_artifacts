#!/usr/bin/env python3
import os
import csv
from collections import defaultdict
from common import grouper

LOG_DIR = "Expert"
colors = ['tab:blue', 'tab:orange', 'tab:green']
linestyles = ['-', '--', '-.']
tools = [r'$\textsc{SyRup}$', r'$\textsc{Burst}$', r'$\textsc{SMyth}$']
BAR_WIDTH = 0.2  # used to be 0.3 for three column
FIG_WIDTH = BAR_WIDTH * 10 * 6

MAX_CARD = 20  # will be read from result.csv
cards = range(0, MAX_CARD+1)

cutoffs = [i / 10 for i in range(1, 11)]
learnability = defaultdict(list)

with open('learnability.csv', 'w') as f_out, \
     open(os.path.join(LOG_DIR, 'result.csv'), 'r') as f_in:
    rows_out = csv.writer(f_out)
    rows_out.writerow(['task'] + tools + tools)

    rows_in = csv.reader(f_in, delimiter=',')
    header = next(rows_in)
    MAX_CARD = int(header[-1])

    for i, (name, count, *data) in enumerate(grouper(18, rows_in)):
        corrects = data[:4]
        del corrects[1]
        row_out = [name[0]]

        for tool, correct in zip(tools, corrects):
            correct = list(map(
                lambda x, y: int(x)/int(y) if int(y) != 0 else 0.0,
                correct[1:], count[1:]))
            row_out.append(
                next((i for i, v in enumerate(correct) if v >= 0.5), 99)
            )

        for tool, correct in zip(tools, corrects):
            correct = list(map(
                lambda x, y: int(x)/int(y) if int(y) != 0 else 0.0,
                correct[1:], count[1:]))
            row_out.append(
                next((i for i, v in enumerate(correct) if v >= 0.9), 99)
            )

        rows_out.writerow(row_out)
