#!/usr/bin/env python3
import csv
import itertools
from collections import defaultdict
import os
import argparse
import numpy as np
from statistics import mean
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.ticker as mtick
from common import parse_result_csv, LOG_DIRS

matplotlib.use('agg')
matplotlib.rc('font', size=15)
plt.rcParams.update({
    "text.usetex": True,
    "font.family": "Helvetica"
})
# plt.style.use('_mpl-gallery')

parser = argparse.ArgumentParser()
parser.add_argument('--card', type=int, default=3)
args = parser.parse_args()

FIG_NAME = f'sensitivity{args.card}.pdf'
BAR_WIDTH = 0.2  # used to be 0.3 for three column
FIG_WIDTH = BAR_WIDTH * 10 * 6


# xs = [(x_label, x_success), (y_label, y_success), ... ]
def plot_bar_with_violin(ax, xs):
    pos = np.arange(len(xs))  # number of synthesizers
    ax.set_xticks(pos, map(lambda x: x[0], xs))
    colors = ['tab:cyan', 'tab:olive', 'tab:gray', 'tab:pink']
    for i, (_, matrixes) in enumerate(xs):
        for j, (color, log_dir) in enumerate(zip(colors, LOG_DIRS)):
            # if len(y[log_dir]) <= args.card:
            # continue
            ys = [row[args.card]
                  for row in matrixes[log_dir] if len(row) > args.card]
            ax.bar(i + (j - 1.5) * BAR_WIDTH, mean(ys),
                   BAR_WIDTH, color=color, label=log_dir)
            # ax.boxplot(ys, positions=[i + (j - 1.5) * BAR_WIDTH], widths=0.1)
            vp = ax.violinplot(
                ys, [i + (j - 2) * BAR_WIDTH], widths=0.3,
                showmeans=False, showmedians=False, showextrema=False)
            for body in vp['bodies']:
                # get the center
                m = np.mean(body.get_paths()[0].vertices[:, 0])
                # modify the paths to not go further right than the center
                body.get_paths()[0].vertices[:, 0] = np.clip(
                    body.get_paths()[0].vertices[:, 0], m, np.inf)
                body.set_alpha(0.3)
                body.set_edgecolor('black')
                body.set_facecolor('blue')


syrup_success_matrix = defaultdict(list)
smyth_success_matrix = defaultdict(list)
burst_success_matrix = defaultdict(list)

for benchmark in parse_result_csv():
    syrup_success = {}
    burst_success = {}
    smyth_success = {}
    for log_dir in LOG_DIRS:
        syrup_success[log_dir] = benchmark[log_dir]['syrup_success']
        burst_success[log_dir] = benchmark[log_dir]['burst_success']
        smyth_success[log_dir] = benchmark[log_dir]['smyth_success']
        syrup_success_matrix[log_dir].append(syrup_success[log_dir])
        burst_success_matrix[log_dir].append(burst_success[log_dir])
        smyth_success_matrix[log_dir].append(smyth_success[log_dir])

fig, ax = plt.subplots(figsize=(FIG_WIDTH, 10))
# fig.suptitle('Number of input-output examples is %d' % args.card)
ax.set_ylabel('average success rate given {} I/O examples'.format(args.card))
ax.yaxis.set_major_formatter(mtick.PercentFormatter(1.0))
ax.set_ylim([0, 1])
plot_bar_with_violin(ax, [
    (r'$\textsc{SyRup}$', syrup_success_matrix),
    (r'$\textsc{SMyth}$', smyth_success_matrix),
    (r'$\textsc{Burst}$', burst_success_matrix),
])
handles, labels = plt.gca().get_legend_handles_labels()
by_label = dict(zip(labels, handles))
plt.legend(by_label.values(), by_label.keys(), loc='upper right')
fig.savefig(FIG_NAME)
