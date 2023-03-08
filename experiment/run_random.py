#!/usr/bin/env python3
import os
import argparse
import csv
import bisect
import re
from contextlib import ExitStack
from subprocess import TimeoutExpired
import common
from common import benchmarks, R, \
    run_syrup, run_smyth, run_burst

MAX_NUM_OF_EXS = 20

parser = argparse.ArgumentParser()
parser.add_argument('-n', '--number-of-instances',
                    help='Number of I/O sets tested on \
                    each synthesizer for each task',
                    type=int, default=10)
parser.add_argument('-t', '--timeout',
                    help='Timeout for each synthesizer',
                    type=int, default=120)
parser.add_argument('-bc', '--include-base-case',
                    help="Enforce inclusion of the base case \
                    in the I/O examples", action="store_true")
parser.add_argument("--ablation",
                    help="Also evaluate a SyRup's variant for ablation study",
                    action="store_true")
args = parser.parse_args()
common.TIMEOUT = args.timeout
MAX_GROUP_SIZE = args.number_of_instances
INCLUDE_BASE_CASE = args.include_base_case
if not args.ablation:
    SYRUP_ALT_MODES = ["syrup"]
else:
    SYRUP_ALT_MODES = ["syrup", "naive"]

EXPERT_DIR = os.path.relpath('io-smyth')
# we will add base case later, do this to avoid duplicated base case
EXS_DIR = os.path.relpath(
    'io-random-nobase' if INCLUDE_BASE_CASE else 'io-random')
filenames = next(os.walk(EXS_DIR))[2]
LOG_DIR = os.path.relpath('Random' +
                          ('+BC' if INCLUDE_BASE_CASE else ''))


os.makedirs(LOG_DIR, exist_ok=True)
with open(os.path.join(LOG_DIR, "result.csv"), "w", newline="") as f:
    csv_writer = csv.writer(f)
    csv_writer.writerow(["cardinality of set"] +
                        list(range(MAX_NUM_OF_EXS + 1)))
    for name in benchmarks:
        with ExitStack() as stack:
            f_alts = [
                stack.enter_context(
                    open(os.path.join(LOG_DIR, name + "_" + mode), 'w')
                ) for mode in SYRUP_ALT_MODES
            ]
            f_smyth = stack.enter_context(
                open(os.path.join(LOG_DIR, name + "_smyth"), 'w'))
            f_burst = stack.enter_context(
                open(os.path.join(LOG_DIR, name + "_burst"), 'w'))

            print(name, flush=True)
            res_timeout = [[0] * (MAX_NUM_OF_EXS + 1)
                           for _i in range(4)]
            res_correct = [[0] * (MAX_NUM_OF_EXS + 1)
                           for _i in range(5)]
            res_time = [[0] * (MAX_NUM_OF_EXS + 1)
                        for _i in range(4)]
            res_count = [0] * (MAX_NUM_OF_EXS + 1)
            res_nosol = [0] * (MAX_NUM_OF_EXS + 1)
            res_sof = [[0] * (MAX_NUM_OF_EXS + 1)
                       for _i in range(4)]

            cards = []
            for filename in filenames:
                if re.search(name + '[0-9]+', filename):
                    bisect.insort(cards, int(
                        re.search('[0-9]+', filename).group()))

            cards = cards[0:MAX_NUM_OF_EXS]

            groups = []
            for card in cards:
                with open(os.path.join(EXS_DIR, name+str(card)), 'r') as f_exs:
                    ls = f_exs.readlines()
                    # split into chunks of size `card`
                    group = [ls[i:i + card]
                             for i in range(0, len(ls), card)]
                    groups.append(group)

            with open(os.path.join(EXPERT_DIR, name), 'r') as f_io:
                lines = f_io.readlines()
            example_base = lines[0]

            for count, group in enumerate(
                    groups,
                    start=(2 if INCLUDE_BASE_CASE else 1)
            ):
                group = R(group, MAX_GROUP_SIZE)
                for set in group:
                    if INCLUDE_BASE_CASE:
                        set += (example_base, )
                    exs = ''.join(set)
                    res_count[count] += 1

                    for i, (f_alt, syrup_mode) in enumerate(zip(f_alts, SYRUP_ALT_MODES)):
                        print(exs, file=f_alt)
                        print('=================================', file=f_alt)
                        try:
                            p = run_syrup(syrup_mode, name, exs)
                            print(p.stdout, file=f_alt, flush=True)
                            result = p.stdout.splitlines()[-1]
                            if "TIMEOUT" in result:
                                res_timeout[i][count] += 1
                            else:
                                [time, is_correct] = result.split(' ')
                                res_time[i][count] += float(time)
                                res_correct[i][count] += int(
                                    is_correct == "true")
                        except TimeoutExpired as e:
                            print(e.stdout, file=f_alt, flush=True)
                            res_timeout[i][count] += 1

                    print(exs, file=f_burst)
                    print('=================================', file=f_burst)
                    try:
                        p = run_burst(name, exs)
                        print(p.stdout, file=f_burst, flush=True)
                        # had to do this to catch (Failure "bad typechecking")
                        if "Failure" not in p.stdout:
                            result = p.stdout.splitlines()[-1]
                            if "TIMEOUT" in result:
                                res_timeout[len(SYRUP_ALT_MODES)][count] += 1
                            elif "STACKOVERFLOW" in result:
                                res_sof[len(SYRUP_ALT_MODES)][count] += 1
                            else:
                                [time, is_correct] = result.split(' ')
                                res_time[len(SYRUP_ALT_MODES)
                                         ][count] += float(time)
                                res_correct[len(SYRUP_ALT_MODES)][count] += int(
                                    is_correct == "true")
                    except TimeoutExpired as e:
                        print(e.stdout, file=f_burst, flush=True)
                        res_timeout[len(SYRUP_ALT_MODES)][count] += 1

                    print(exs, file=f_smyth)
                    print('=================================', file=f_smyth)
                    try:
                        p = run_smyth(name, exs)
                        print(p.stdout, file=f_smyth, flush=True)
                        result = p.stdout.splitlines()[-1]
                        if "TIMEOUT" in result:
                            res_timeout[len(SYRUP_ALT_MODES)+1][count] += 1
                        elif "STACKOVERFLOW" in result:
                            res_sof[len(SYRUP_ALT_MODES)+1][count] += 1
                        elif "NOSOL" in result:
                            res_nosol[count] += 1
                        else:
                            [time, is_correct1,
                                is_correct2] = result.split(' ')
                            res_time[len(SYRUP_ALT_MODES) +
                                     1][count] += float(time)
                            res_correct[len(SYRUP_ALT_MODES)+1][count] += int(
                                is_correct1 == "true")
                            res_correct[len(SYRUP_ALT_MODES)+2][count] += int(
                                is_correct2 == "true")
                    except TimeoutExpired as e:
                        print(e.stdout, file=f_smyth, flush=True)
                        res_timeout[len(SYRUP_ALT_MODES)+1][count] += 1

            csv_writer.writerow(
                [name] + [""] * (MAX_NUM_OF_EXS+1))
            csv_writer.writerow(["count"] + res_count)
            for i, mode in enumerate(SYRUP_ALT_MODES):
                csv_writer.writerow([mode+"-correct"] + res_correct[i])
            csv_writer.writerow(["burst-correct"] +
                                res_correct[len(SYRUP_ALT_MODES)])
            csv_writer.writerow(["smyth-correct"] +
                                res_correct[len(SYRUP_ALT_MODES)+1])
            csv_writer.writerow(["smyth-rec-correct"] +
                                res_correct[len(SYRUP_ALT_MODES)+2])
            csv_writer.writerow(["burst-sof"] + res_sof[len(SYRUP_ALT_MODES)])
            csv_writer.writerow(
                ["smyth-sof"] + res_sof[len(SYRUP_ALT_MODES)+1])
            csv_writer.writerow(["smyth-nosol"] + res_nosol)
            for i, mode in enumerate(SYRUP_ALT_MODES):
                csv_writer.writerow([mode+"time"] + res_time[i])
            csv_writer.writerow(
                ["burst-time"] + res_time[len(SYRUP_ALT_MODES)])
            csv_writer.writerow(
                ["smyth-time"] + res_time[len(SYRUP_ALT_MODES)+1])
            for i, mode in enumerate(SYRUP_ALT_MODES):
                csv_writer.writerow([mode+"timeout"] + res_timeout[i])
            csv_writer.writerow(["burst-timeout"] +
                                res_timeout[len(SYRUP_ALT_MODES)])
            csv_writer.writerow(["smyth-timeout"] +
                                res_timeout[len(SYRUP_ALT_MODES)+1])
            f.flush()
