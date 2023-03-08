#!/usr/bin/env python3
import os
import argparse
import csv
from collections import defaultdict
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.ticker as mtick
from common import grouper, mean_of_matrix, benchmarks_nonrec

matplotlib.use('agg')
matplotlib.rc('font', size=15)
plt.rcParams.update({
    "text.usetex": True,
    "font.family": "Helvetica"
})
# plt.style.use('_mpl-gallery')

LOG_DIR = "Expert"

parser = argparse.ArgumentParser()
parser.add_argument("--rec",
                    help="only consider recursive tasks",
                    action="store_true")
parser.add_argument("--ablation",
                    help="include SyRup* for abalation study",
                    action="store_true")
args = parser.parse_args()


def summary_learnability(tool1, tool2, learnability1, learnability2):
    print(f'{tool1} needs more examples than {tool2} to achieve 50% on \
    {sum(x[4] > y[4] for x, y in zip(learnability1, learnability2))} \
    tasks')
    print(f'{tool1} needs more examples than {tool2} to achieve 90% on \
    {sum(x[8] > y[8] for x, y in zip(learnability1, learnability2))} \
    tasks')
    print(f'{tool1} needs less examples than {tool2} to achieve 50% on \
    {sum(x[4] < y[4] for x, y in zip(learnability1, learnability2))} \
    tasks')
    print(f'{tool1} needs less examples than {tool2} to achieve 90% on \
    {sum(x[8] < y[8] for x, y in zip(learnability1, learnability2))} \
    tasks')


if args.ablation:
    colors = ['tab:blue', 'tab:red', 'tab:orange', 'tab:green']
    linestyles = ['-', '-', '--', '-.']
    tools = [r'$\textsc{SyRup}$', r'$\textsc{SyRup}^*$',
             r'$\textsc{Burst}$', r'$\textsc{SMyth}$']
else:
    colors = ['tab:blue', 'tab:orange', 'tab:green']
    linestyles = ['-', '--', '-.']
    tools = [r'$\textsc{SyRup}$',
             r'$\textsc{Burst}$', r'$\textsc{SMyth}$']
FIG_NAME = "learnability{}{}.pdf".format(
    "-abalation" if args.ablation else "",
    "-rec" if args.rec else "")

BAR_WIDTH = 0.2  # used to be 0.3 for three column
FIG_WIDTH = BAR_WIDTH * 10 * 6

MAX_CARD = 20  # will be read from result.csv
cards = range(0, MAX_CARD+1)

cutoffs = [i / 10 for i in range(1, 11)]
learnability = defaultdict(list)

with open(os.path.join(LOG_DIR, 'result.csv'), 'r') as f:
    rows = csv.reader(f, delimiter=',')
    header = next(rows)
    MAX_CARD = int(header[-1])

    for i, (name, count, *data) in enumerate(grouper(18, rows)):
        if args.rec and name[0] in benchmarks_nonrec:
            continue
        corrects = data[:4]
        if not args.ablation:
            del corrects[1]

        # get number of trace-complete examples
        n = next((i for i, v in enumerate(count[2:]) if int(v) == 0), MAX_CARD)

        for tool, correct in zip(tools, corrects):
            correct = list(map(
                lambda x, y: int(x)/int(y) if int(y) != 0 else 0.0,
                correct[1:], count[1:]))
            line = []
            for cutoff in cutoffs:
                line.append(
                    next((i/n for i, v in enumerate(correct) if v >= cutoff), 1))

            learnability[tool].append(line)

# print summary
if args.ablation:
    summary_learnability(
        'SyRup', 'SyRup*', learnability[tools[0]], learnability[tools[1]])
else:
    learnability_baseline = [
        min(l1, l2) for l1, l2 in
        zip(learnability[tools[1]], learnability[tools[2]])]
    summary_learnability(
        'SyRup', 'Best(Burst, SMyth)',
        learnability[tools[0]], learnability_baseline)


# visualize
fig, ax = plt.subplots()
for c, ls, tool in zip(colors, linestyles, tools):
    ax.xaxis.set_major_formatter(mtick.PercentFormatter(1.0))
    ax.set_ylim([0, 1])
    ax.yaxis.set_major_formatter(mtick.PercentFormatter(1.0))
    ax.set_ylabel('percentage of trace-complete examples')
    ax.set_xlabel('success rate')
    ax.plot(cutoffs, mean_of_matrix(
        learnability[tool]), label=tool, linestyle=ls, color=c)
ax.legend(loc='lower right')

fig.savefig(FIG_NAME, bbox_inches="tight")
