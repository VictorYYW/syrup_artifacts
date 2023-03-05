# Artifacts for "Trace-Guided Inductive Synthesis of Recursive Functional Programs"

The repo include the synthesis tool described in the paper, `SyRup`,
and two other synthesizers from prior work, `SMyth` and `Burst`.  For
ease of benchmarking, we have hard coded in the synthesizers, 43
programming tasks used in the paper as well as their accompanying
input-output examples that are extensive enough to verify synthesized
programs' correctness. And thus, by taking a known task name and I/O
examples, each synthesizer not only generate a program but also report
synthesis time and the correctness of the program.

## Begin Here

The following sections describe the experiment pipeline step by step
with the later steps depending on the previous steps. Since the
intermediate result are already in the repo, you may start from an
arbitrary step and skip its previous steps. To produce the exact
figures as in the paper, please skip to [the last
step](#reproduce-figures).

## Build

Assuming you have `opam` installed, you may build `SyRup`, `SMyth`, and `Burst` via

```
./build.sh
```

Alternatively, you may use [Docker](https://www.docker.com/get-started/).

```
docker build -t syrup_artifacts .
docker run -it syrup_artifacts
```

## I/O example generation

We evaluate synthesizers on multiple example sets for each programming
tasks.

- Nothing to do here. `./run_expert.py` generates "expert" examples on
the fly during experiment, by sampling subsets of trace-complete
examples used by Myth (stored in `experiment/io-myth`), and

- `gen_random_example` generates "random" examples of different cardinalities
prior to experiment, by sampling inputs and obtaining corresponding
outputs by evaluating our reference implementation on the inputs, and
stores them in `experiment/random-io-nobase` and
`experiment/random-io`.

## Run Experiments

While the repo includes intermediate experiment result in
`experiment/{Expert,Expert+BC,Random,Random+BC}`, you may rerun all
the experiments to overwrite the existing result in the folders as
follows.

```shell
cd experiment
./run_expert.py --ablation > expert.out 2>&1 & # --ablation flag is necessary to generate Fig. 9
./run_expert.py -bc > expert+bc.out 2>&1 &
./run_random.py > random.out 2>&1 &
./run_random.py -bc > random+bc.out 2>&1 &
```

Please bear in mind that, it may take a few days to finish all the
experiments (the one on expert examples took us over a day, the one on
randomly generated I/O examples took us over a week).  This is because
our evaluation invokes dozens of instances of synthesizers on each
programming tasks. And some tasks may take longer than a few seconds,
and even time out (120 seconds).

You may lower the number of I/O example sets tested on each
synthesizer for each tasks by passing `-n 10`, and timeout by passing
`-t 120`.

## Reproduce Figures

To generate the exact visualizations in the paper,

```shell
cd experiment
./visualize_learnability.py # Fig. 7, learnability.pdf
./gen_table.py #Table 1, learnability.csv
./visualize_sensitivity.py # Fig. 8, sensitivity3.pdf
./visualize_learnability.py --ablation --rec # Fig. 9, learnability-ablation-rec.pdf
```

## Run SyRup on Individual Tasks

As described in the paper, necessary definitions of algebraic data
types and background functions's Z3 encoding are built in the
implementation of `SyRup`.  As a result, we currently only support
synthesis for known tasks (given in `syrup/lib/references.ml`).

```
syrup/syrup syrup nat_add "(8, 5) -> 13"
```
