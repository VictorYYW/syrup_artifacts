"""configuration and common functions for experiments."""
from subprocess import run, PIPE, STDOUT
import os
import csv
import itertools
import pandas as pd
import random

TIMEOUT = 120
LOG_DIRS = ["Random", "Random+BC", "Expert", "Expert+BC"]

benchmarks = [
    "bool_band",
    "bool_bor",
    "bool_impl",
    "bool_neg",
    "bool_xor",
    "nat_add",
    "nat_iseven",
    "nat_max",
    "nat_pred",
    "list_hd",
    "list_tl",
    "list_last",
    "list_take",
    "list_drop",
    "list_nth",
    "list_sum",
    "list_length",
    "list_even_parity",
    "list_inc",
    "list_stutter",
    "list_snoc",
    "list_append",
    "list_compress",
    "list_concat",
    "list_rev_append",
    "list_rev_fold",
    "list_rev_snoc",
    "list_rev_tailcall",
    "list_pairwise_swap",
    "list_sort_sorted_insert",
    "list_sorted_insert",
    "list_map",
    "list_filter",
    "list_fold",
    "tree_count_leaves",
    "tree_count_nodes",
    "tree_inorder",
    "tree_inorder_bool",
    "tree_postorder",
    "tree_preorder",
    "tree_nodes_at_level",
    "tree_map",
    "tree_binsert",
]

benchmarks_nonrec = ["bool_band", "bool_bor", "bool_impl", "bool_neg", "bool_xor",
                     "nat_pred", "list_hd", "list_inc", "list_rev_fold", "list_sum", "list_tl"]

SYRUP_DIR = os.path.relpath("../syrup")
SMYTH_DIR = os.path.relpath("../smyth")
BURST_DIR = os.path.relpath("../burst")


def run_syrup(mode, name, io_str):
    return run(
        [os.path.join(SYRUP_DIR, "syrup"), mode, name, io_str],
        check=False,
        stdout=PIPE,
        stderr=STDOUT,
        timeout=TIMEOUT,
        universal_newlines=True,
    )


def run_smyth(name, io_str):
    return run(
        [os.path.join(SMYTH_DIR, "smyth"), "forge-exs", name, io_str],
        check=False,
        stdout=PIPE,
        stderr=STDOUT,
        timeout=TIMEOUT,
        universal_newlines=True,
        cwd=SMYTH_DIR,
    )


def run_burst(name, io_str):
    return run(
        [os.path.join(BURST_DIR, "BurstCmdLine.exe"), name, "-exs", io_str],
        check=False,
        stdout=PIPE,
        stderr=STDOUT,
        timeout=TIMEOUT,
        universal_newlines=True,
        cwd=BURST_DIR,
    )


def R(it, k):
    '''https://en.wikipedia.org/wiki/Reservoir_sampling#Algorithm_R'''
    it = iter(it)
    result = []
    for i, datum in enumerate(it):
        if i < k:
            result.append(datum)
        else:
            j = random.randint(0, i-1)
            if j < k:
                result[j] = datum
    return result


def grouped_powerset(iterable):
    "([1,2,3]) --> [[(1,), (2,), (3,)], [(1, 2), (1, 3), (2, 3)], [(1, 2, 3)]]"
    s = list(iterable)
    return [itertools.combinations(s, r) for r in range(1, len(s)+1)]


def grouper(n, iterable):
    it = iter(iterable)
    while True:
        chunk = tuple(itertools.islice(it, n))
        if not chunk:
            return
        yield chunk

# https://stackoverflow.com/questions/40318013/numpy-mean-on-varying-row-size


def mean_of_matrix(xss):
    df = pd.DataFrame(xss)
    return df.mean().values.tolist()


def parse_result_csv():
    # read csv and preprocess collected data
    results = [{} for _ in benchmarks]

    # log directory name infer what examples were used
    for exs in LOG_DIRS:
        csv_path = os.path.join(exs, 'result.csv')
        with open(csv_path, 'r') as f:
            for linecount, _line in enumerate(f):
                pass
        with open(csv_path, 'r') as f:
            rows = csv.reader(f, delimiter=',')

            header = next(rows)
            MAX_CARD = int(header[-1])
            for i, (name, count, *data) in enumerate(
                    grouper(15 if linecount % 15 == 0 else 18, rows)):
                if linecount % 15 == 0:
                    syrup_correct, burst_correct, smyth_correct, _, \
                        burst_sof, smyth_sof, smyth_nosol, \
                        syrup_time, burst_time, smyth_time, \
                        syrup_timeout, burst_timeout, smyth_timeout \
                        = data
                else:
                    syrup_correct, _, burst_correct, smyth_correct, _, \
                        burst_sof, smyth_sof, smyth_nosol, \
                        syrup_time, _, burst_time, smyth_time, \
                        syrup_timeout, _, burst_timeout, smyth_timeout \
                        = data
                # convert string to integers
                count = [int(x) for x in count[1:]]
                syrup_correct = [int(x) for x in syrup_correct[1:]]
                burst_correct = [int(x) for x in burst_correct[1:]]
                smyth_correct = [int(x) for x in smyth_correct[1:]]
                syrup_time = [float(x) for x in syrup_time[1:]]
                burst_time = [float(x) for x in burst_time[1:]]
                smyth_time = [float(x) for x in smyth_time[1:]]
                syrup_timeout = [int(x) for x in syrup_timeout[1:]]
                burst_timeout = [int(x) for x in burst_timeout[1:]]
                smyth_timeout = [int(x) for x in smyth_timeout[1:]]

                # remove unnecessary columns
                tmp = list(filter(
                    lambda x: x[0] == 0 or x[1] > 0,
                    zip(range(0, MAX_CARD+1), count,
                        syrup_correct, burst_correct, smyth_correct,
                        syrup_time, burst_time, smyth_time,
                        syrup_timeout, burst_timeout, smyth_timeout)
                ))
                [cards, count,
                 syrup_correct, burst_correct, smyth_correct,
                 syrup_time, burst_time, smyth_time,
                 syrup_timeout, burst_timeout, smyth_timeout] = zip(*tmp)

                benchmark = dict([
                    ("name", name[0]),
                    ("cards", cards),
                    ("count", count),
                    ("syrup_success", list(
                        map(lambda x, y: x/y if y else 0, syrup_correct, count))),
                    ("burst_success", list(
                        map(lambda x, y: x/y if y else 0, burst_correct, count))),
                    ("smyth_success", list(
                        map(lambda x, y: x/y if y else 0, smyth_correct, count))),
                    ("syrup_timeout", list(
                        map(lambda x, y: x/y if y else 0, syrup_timeout, count))),
                    ("burst_timeout", list(
                        map(lambda x, y: x/y if y else 0, burst_timeout, count))),
                    ("smyth_timeout", list(
                        map(lambda x, y: x/y if y else 0, smyth_timeout, count))),
                    ("syrup_time", list(
                        map(lambda x, y: x/y if y else 0, syrup_time, count))),
                    ("burst_time", list(
                        map(lambda x, y: x/y if y else 0, burst_time, count))),
                    ("smyth_time", list(
                        map(lambda x, y: x/y if y else 0, smyth_time, count))),
                ])

                results[i][exs] = benchmark
    return results
