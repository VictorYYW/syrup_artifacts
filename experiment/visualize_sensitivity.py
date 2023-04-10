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
import matplotlib.patches as mpatches
import matplotlib.colors as mcolors
from common import parse_result_csv, LOG_DIRS

matplotlib.use('agg')
matplotlib.rc('font', size=24)
plt.rcParams.update({
    "text.usetex": True,
    "font.family": "Helvetica"
})
# plt.style.use('_mpl-gallery')

parser = argparse.ArgumentParser()
parser.add_argument('--max-card', type=int, default=6)
args = parser.parse_args()

FIG_NAME = 'sensitivity.pdf'
BAR_WIDTH = 0.2  # used to be 0.3 for three column
FIG_WIDTH = BAR_WIDTH * 10 * 6

colors = ['tab:blue', 'tab:orange', 'tab:green']
hatches = ['//', 'o', '', '*']

# xs = [(x_label, x_success), (y_label, y_success), ... ]
def plot_bar_with_violin(ax, xs, card):
    pos = np.arange(len(xs))  # number of synthesizers
    ax.set_xticks(np.subtract(pos, 0.1), map(lambda x: x[0], xs))
    ax.tick_params(axis='both', which='both', length=0)
    # colors = ['tab:cyan', 'tab:olive', 'tab:gray', 'tab:pink']
    for i, (color, (_syn_name, matrixes)) in enumerate(zip(colors, xs)):
        for j, (hatch, log_dir) in enumerate(zip(hatches, LOG_DIRS)):
            # if len(y[log_dir]) <= args.card:
            # continue
            ys = [row[card]
                  for row in matrixes[log_dir] if len(row) > card]
            # ax.bar(i + (j - 1.5) * BAR_WIDTH, mean(ys),
            #        BAR_WIDTH, color=color, label=log_dir)
            # ax.boxplot(ys, positions=[i + (j - 1.5) * BAR_WIDTH], widths=0.1)
            vp = ax.violinplot(
                ys, [i + (j - 2) * BAR_WIDTH], widths=0.3,
                showmeans=True, showmedians=False, showextrema=False)
            vp['cmeans'].set_color('black')
            for body in vp['bodies']:
                # keep the left half of the violin
                m = np.mean(body.get_paths()[0].vertices[:, 0])
                body.get_paths()[0].vertices[:, 0] = np.clip(
                    body.get_paths()[0].vertices[:, 0], m, np.inf)
                body.set_offsets((-12, 0))
                # violin styling
                body.set_alpha(None)
                body.set_hatch(hatch)
                body.set_edgecolor(mcolors.to_rgba('black', 1))
                body.set_facecolor(mcolors.to_rgba(color, 0.8))


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

fig, axs = plt.subplots(-(-args.max_card // 3), 3,
                        figsize=(FIG_WIDTH * 2, 10), sharey=True, sharex=True)
fig.supylabel('(average) success rate')
# fig.suptitle('Number of input-output examples is %d' % args.card)

# iterate over subplots
for i, ax in enumerate(axs.flat):
    if i == 0:
        ax.legend(
            [mpatches.Patch(edgecolor='black', alpha=1.0, facecolor='none', hatch=hatch)
             for hatch in hatches], LOG_DIRS, loc='upper left')
    # ax.set_ylabel('average success rate given {} I/O examples'.format(args.card))
    ax.set_title(f'{i + 1} I/O example{"s" if i > 0 else ""}')
    ax.yaxis.set_major_formatter(mtick.PercentFormatter(1.0))
    ax.set_ylim([0, 1])

    plot_bar_with_violin(ax, [
        (r'$\textsc{SyRup}$', syrup_success_matrix),
        (r'$\textsc{SMyth}$', smyth_success_matrix),
        (r'$\textsc{Burst}$', burst_success_matrix),
    ], i + 1)

fig.tight_layout()
fig.savefig(FIG_NAME)
