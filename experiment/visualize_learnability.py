#!/usr/bin/env python3
import os
import argparse
import csv
from collections import defaultdict
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.ticker as mtick
import matplotlib.lines as mlines
from common import benchmarks, benchmarks_nonrec, \
    draw_hlines, grouper, mean_of_matrix

matplotlib.use('agg')
matplotlib.rc('font', size=10)
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


def summary_learnability(benchmark_names, tool1, tool2, learnability1, learnability2):
    print(f'{tool1} needs more examples than {tool2} to achieve 50% on \
    {sum(x[4] > y[4] for x, y in zip(learnability1, learnability2))} \
    tasks')
    print(', '.join([benchmark_names[i]
                     for i, (x, y) in
                     enumerate(zip(learnability1, learnability2))
                     if x[4] > y[4]]))
    print(f'{tool1} needs more examples than {tool2} to achieve 90% on \
    {sum(x[8] > y[8] for x, y in zip(learnability1, learnability2))} \
    tasks')
    print(f'{tool1} needs less examples than {tool2} to achieve 50% on \
    {sum(x[4] < y[4] for x, y in zip(learnability1, learnability2))} \
    tasks')
    print(', '.join([benchmark_names[i]
                     for i, (x, y) in
                     enumerate(zip(learnability1, learnability2))
                     if x[4] < y[4]]))
    print(f'{tool1} needs less examples than {tool2} to achieve 90% on \
    {sum(x[8] < y[8] for x, y in zip(learnability1, learnability2))} \
    tasks')


with open(os.path.join(LOG_DIR, 'result.csv'), 'r') as f:
    for linecount, _line in enumerate(f):
        pass

if args.ablation and linecount % 18 == 0:
    colors = ['tab:blue', 'tab:red', 'tab:orange', 'tab:green']
    linestyles = ['-', '-', '--', '-.']
    tools = [r'$\textsc{SyRup}$', r'$\textsc{SyRup}^*$',
             r'$\textsc{Burst}$', r'$\textsc{SMyth}$']
elif not args.ablation:
    colors = ['tab:blue', 'tab:orange', 'tab:green']
    linestyles = ['-', '--', '-.']
    tools = [r'$\textsc{SyRup}$',
             r'$\textsc{Burst}$', r'$\textsc{SMyth}$']
else:
    raise ValueError('Wrong number of lines in result.csv')
FIG_NAME = "learnability{}{}.pdf".format(
    "-ablation" if args.ablation else "",
    "-rec" if args.rec else "")

MAX_CARD = 20  # will be read from result.csv
cards = range(0, MAX_CARD+1)

cutoffs = [i / 10 for i in range(1, 11)]
learnability = defaultdict(list)

NUM_OF_COLS = 5
NUM_OF_ROWS = -(-len(benchmarks) // NUM_OF_COLS)
fig, axs2d = plt.subplots(NUM_OF_ROWS, NUM_OF_COLS, figsize=(8.5, 9), sharex=True, sharey=True)
axs = axs2d.flatten()
# gs = fig.add_gridspec(len(benchmarks) + 1, hspace=0)
# fig = plt.figure(figsize=(FIG_WIDTH, len(benchmarks)))
# gs = fig.add_gridspec(len(benchmarks) + 1, hspace=0)
# axs = gs.subplots(sharex=True)

with open(os.path.join(LOG_DIR, 'result.csv'), 'r') as f:
    rows = csv.reader(f, delimiter=',')
    header = next(rows)
    MAX_CARD = int(header[-1])

    prefix_last = ""
    i = 0
    rows_to_be_separated = []
    for (name, count, *data) in grouper(
            15 if linecount % 15 == 0 else 18, rows):
        if args.rec and name[0] in benchmarks_nonrec:
            continue
        if linecount % 15 == 0:
            corrects = data[:3]
        else:
            corrects = data[:4]
            if not args.ablation:
                del corrects[1]

        # get prefix that ends with underscore in name
        prefix = name[0][:name[0].find('_')]
        if prefix != prefix_last:
            prefix_last = prefix
            for j in range(i, (i + 4) // 5 * 5):
                axs[j].axis('off')
            i = (i + 4) // 5 * 5
            if i != 0:
                rows_to_be_separated.append(i//5-1)

        # get number of trace-complete examples
        n = next((i for i, v in enumerate(count[2:]) if int(v) == 0), MAX_CARD)

        axs[i].xaxis.set_major_formatter(mtick.PercentFormatter(1.0))
        axs[i].yaxis.set_major_formatter(mtick.PercentFormatter(1.0))
        # axs[i].set_yticks([0, n])
        axs[i].set_yticks([0, 1])
        axs[i].set_ylim([0, 1.1])
        axs[i].set_title(name[0])
        axs[i].text(1.02, .05, f'100\% = {n} IOs',
                    ha='right', va='bottom', fontsize=7,
                    bbox={'facecolor': 'white', 'alpha': 0.2,
                          'pad': 0.2, 'boxstyle': 'round'})
        for c, ls, tool, correct in zip(colors, linestyles, tools, corrects):
            correct = list(map(
                lambda x, y: int(x)/int(y) if int(y) != 0 else 0.0,
                correct[1:], count[1:]))

            # get percentage of trace-complete examples
            line = []
            for cutoff in cutoffs:
                line.append(
                    next((i/n for i, v in enumerate(correct) if v >= cutoff), 1))
            learnability[tool].append(line)
            axs[i].plot(cutoffs, line, label=tool, linestyle=ls, color=c)

        i += 1
    rows_to_be_separated.append(i//5)

axs[i].legend(
    [mlines.Line2D([], [], color=c, linestyle=ls)
     for c, ls in zip(colors, linestyles)], tools,
    loc='lower right', fontsize=10)

for j in range(i, len(axs)):
    axs[j].axis('off')

fig.supxlabel('success rate')
fig.supylabel('percentage of trace-complete examples')
fig.tight_layout()
draw_hlines(fig, axs2d, rows_to_be_separated)
fig.savefig("learnability-individual{}{}.pdf".format(
    "-ablation" if args.ablation else "",
    "-rec" if args.rec else ""), bbox_inches='tight')
# print summary
benchmark_names = [name for name in benchmarks
                   if not args.rec or name not in benchmarks_nonrec]
if args.ablation:
    summary_learnability(
        benchmark_names,
        'SyRup', 'SyRup*', learnability[tools[0]], learnability[tools[1]])
else:
    learnability_baseline = [
        min(l1, l2) for l1, l2 in
        zip(learnability[tools[1]], learnability[tools[2]])]
    summary_learnability(
        benchmark_names,
        'SyRup', 'Best(Burst, SMyth)',
        learnability[tools[0]], learnability_baseline)
    learnability_baseline = [
        max(l1, l2) for l1, l2 in
        zip(learnability[tools[1]], learnability[tools[2]])]
    summary_learnability(
        benchmark_names,
        'SyRup', 'Worst(Burst, SMyth)',
        learnability[tools[0]], learnability_baseline)


# visualize
fig, ax = plt.subplots(figsize=(4, 3))
for c, ls, tool in zip(colors, linestyles, tools):
    ax.xaxis.set_major_formatter(mtick.PercentFormatter(1.0))
    ax.set_ylim([0, 1])
    ax.yaxis.set_major_formatter(mtick.PercentFormatter(1.0))
    ax.set_ylabel('percentage of trace-complete examples')
    ax.set_xlabel('success rate')
    avg_learnability = mean_of_matrix(learnability[tool])
    print(tool)
    print(avg_learnability)
    ax.plot(cutoffs, avg_learnability, label=tool, linestyle=ls, color=c)
ax.legend(loc='lower right')
fig.tight_layout()
fig.savefig(FIG_NAME, bbox_inches='tight')
