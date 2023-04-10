#!/usr/bin/env python3
import os
import csv
import argparse
from collections import defaultdict
import numpy as np
import matplotlib
from matplotlib import ticker
import matplotlib.pyplot as plt
import matplotlib.lines as mlines
from common import LOG_DIRS, benchmarks, \
    draw_hlines, grouper

matplotlib.use('tkagg')
matplotlib.rc('font', size=10)
plt.rcParams.update({
    "text.usetex": True,
    "font.family": "Helvetica"
})
# plt.style.use('_mpl-gallery')

parser = argparse.ArgumentParser()
parser.add_argument('--card', type=int, default=3)
args = parser.parse_args()

colors = ['tab:blue', 'tab:orange', 'tab:green']
linestyles = ['-', '--', '-.']
tools = [r'$\textsc{SyRup}$', r'$\textsc{Burst}$', r'$\textsc{SMyth}$']

MAX_CARD = 20  # will be read from result.csv
cards = range(0, MAX_CARD+1)

cutoffs = [i / 10 for i in range(1, 11)]
learnability = defaultdict(list)

NUM_OF_COLS = 5
NUM_OF_ROWS = -(-len(benchmarks) // NUM_OF_COLS)

for LOG_DIR in LOG_DIRS:
    print(LOG_DIR)
    with open(os.path.join(LOG_DIR, 'result.csv'), 'r') as f:
        for linecount, _line in enumerate(f):
            pass
    timeout_rate_matrix = []
    failure_rate_matrix = []
    fig, axs2d = plt.subplots(NUM_OF_ROWS, NUM_OF_COLS, figsize=(8.5, 9),
                              sharex=False, sharey=False)
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
            if linecount % 15 == 0:
                burst_sof = data[4]
                smyth_sof = data[5]
                smyth_nosol = data[6]
                runtimes = data[-6:-3]
                timeouts = data[-3:]
            else:
                burst_sof = data[5]
                smyth_sof = data[6]
                smyth_nosol = data[7]
                runtimes = data[-8:-4]
                del runtimes[1]
                timeouts = data[-4:]
                del timeouts[1]

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
            print(n)

            count = [int(c) for c in count[2:n+2]]
            runtimes = [[np.float64(t) for t in runtime[2:n+2]] for runtime in runtimes]
            timeouts = [[int(t) for t in timeout[2:n+2]] for timeout in timeouts]
            print(timeouts)
            failures = []
            failures.append([int(c) for c in timeouts[0]])
            failures.append([int(x) + int(y) for x, y in zip(
                burst_sof[2:n+2], timeouts[1])])
            failures.append([int(x) + int(y) + int(z) for x, y, z in zip(
                smyth_sof[2:n+2], smyth_nosol[2:n+2], timeouts[2])])

            if len(count) > args.card - 1:
                timeout_rate_matrix.append([timeouts[s][args.card - 1]
                                            / count[args.card - 1] for
                                            s in range(3)])
            print([runtime[args.card - 1]/(count[args.card - 1] - failure[args.card - 1]) for runtime, failure in zip(runtimes, failures) if len(runtime) > args.card - 1])
            print([failure[args.card - 1] for failure in failures if len(failure) > args.card - 1])

            axs[i].xaxis.set_major_formatter(ticker.FormatStrFormatter("%d"))
            # axs[i].set_yticks([0, 1])
            # axs[i].set_ylim([0, 1.1])
            axs[i].set_title(name[0])
            xs = range(1, n + 1)
            for c, ls, tool, runtime, failure in zip(
                    colors, linestyles, tools, runtimes, failures):
                ys = [(total_runtime + 1200.0 * fail_count) / n
                      for n, total_runtime, fail_count in
                      zip(count, runtime, failure)]
                axs[i].plot(xs, ys, label=tool, linestyle=ls, color=c)

            i += 1
        # print(np.mean(timeout_rate_matrix, axis=0))
        rows_to_be_separated.append(i//5)

    axs[i].legend(
        [mlines.Line2D([], [], color=c, linestyle=ls)
         for c, ls in zip(colors, linestyles)], tools,
        loc='lower right', fontsize=10)

    for j in range(i, len(axs)):
        axs[j].axis('off')

    fig.supxlabel(f'number of {LOG_DIR} examples')
    fig.supylabel('average runtime (s)')
    fig.tight_layout()
    draw_hlines(fig, axs2d, rows_to_be_separated)
    fig.savefig(f'runtime-individual-{LOG_DIR}.pdf', bbox_inches='tight')
