# Artifacts for "Trace-Guided Inductive Synthesis of Recursive Functional Programs"

The repo include the synthesis tool described in the paper, `SyRup`,
and two other synthesizers from prior work, `SMyth` and `Burst`.  For
ease of benchmarking, we have hard coded in the synthesizers, 43
programming tasks used in the paper as well as their accompanying
input-output examples that are extensive enough to verify synthesized
programs' correctness. And thus, by taking a known task name and I/O
examples, each synthesizer not only generate a program but also report
synthesis time and the correctness of the program.

## Build

To run the python scripts for reproduction of the visualizations in
the paper, make sure `python3` (as well as `pandas`, `numpy`, and
`matplotlib`) and `latex` is installed, and properly set up. If you
are on a Debian-based Linux distribution, the following commands
should do the job.

```
sudo apt install -y python3 python3-pip texlive-full
pip install pandas numpy matplotlib
```

You may also need to install `libgmp-dev` for `z3` before moving on
building the tools.

```
sudo apt instlal -y libgmp-dev
```

Assuming you have `opam` installed, you may build the tools (i.e.,
`SyRup`, `SMyth`, and `Burst`) via

```
./build.sh
```


### Use Docker

Alternatively, you may use the pre-built [Docker](https://www.docker.com/get-started/) image.

```
docker pull victoryuan/syrup_artifacts:pldi23v1
```

Or build the the Docker image on your own.

```
docker build . -t victoryuan/syrup_artifacts:pldi23v1
```

And run a Docker container in background.

```
docker run -itd --name syrup victoryuan/syrup_artifacts:pldi23v1
```

To execute command in the docker container,

```
docker exec -t syrup bash -c 'cd experiment && ./visualize_learnability.py'
docker exec -t syrup bash -c 'syrup/syrup syrup nat_add "(8, 5) -> 13"'
```

Tip: to open a pdf in the docker container, first copy it to the host machine.

```
docker cp syrup:/home/opam/experiment/learnability.pdf .
```

When done with the docker,

```
docker stop syrup && docker rm syrup
```

## Experiments

The following sections describe the experiment pipeline step by step
with the later steps depending on the previous steps. Since the
intermediate result are already in the repo, you may start from an
arbitrary step and skip its previous steps. To reproduce the *exact*
figures as in the paper, please skip to [the last step](#reproduce-figures).

### I/O example generation

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

### Run Experiments

Our evaluation invokes dozens of instances of synthesizers on each
programming tasks, and it may take a few days to finish all the
experiments (the one on expert examples took us over a day, the one on
randomly generated I/O examples took us over a week). And some tasks
may take longer than a few seconds, and even time out (120 seconds).
You may lower the number of I/O example sets tested on each
synthesizer for each tasks by passing `-n 10`, and timeout by passing
`-t 120`.

Therefore, it is recommended to run the experiments in background in
parallel with `nohup`, on a server or a spare desktop (preferrably
equiped with at least 32GB RAM, especially for `./run_random.py`), and
inspect `*.out` files to check the progress of the experiment as it
prints out benchmark names (43 in total). The experiments were
performed on a department-wide shared Linux server equipped with two
2.90GHz Intel Xeon E5-2690 2.90GHz 8-core processors and 192GB of RAM.

```shell
cd experiment
nohup ./run_expert.py --ablation > expert.out 2>&1 &
nohup ./run_expert.py -bc > expert+bc.out 2>&1 &
nohup ./run_random.py > random.out 2>&1 &
nohup ./run_random.py -bc > random+bc.out 2>&1 &
```

To run experiments inside a Docker container,

```shell
docker exec -td syrup bash -c 'cd experiment && ./run_expert.py --ablation > expert.out 2>&1'
docker exec -td syrup bash -c 'cd experiment && ./run_expert.py -bc > expert+bc.out 2>&1'
docker exec -td syrup bash -c 'cd experiment && ./run_random.py > random.out 2>&1'
docker exec -td syrup bash -c 'cd experiment && ./run_random.py -bc > random+bc.out 2>&1'
docker attach syrup # Launch shell to check if the experiments are finished, press "CTRL-p CTRL-q" to detach again
```

### Reproduce Figures

The experiment results from the previous section are already included
in the repo under
`experiment/{Expert,Expert+BC,Random,Random+BC}`. You can directly
generate the exact visualizations in the paper.

```shell
cd experiment
./visualize_learnability.py # Fig. 7, learnability.pdf
./gen_table.py # Table 1, learnability.csv
./visualize_sensitivity.py # Fig. 8, sensitivity3.pdf
./visualize_learnability.py --ablation --rec # Fig. 9, learnability-ablation-rec.pdf
```

## Run SyRup on Individual Tasks

As described in the paper, necessary definitions of algebraic data
types and background functions's Z3 encoding are built in the
implementation of `SyRup`.  As a result, we currently only support
synthesis for known tasks (given in `syrup/lib/references.ml`, one may
supply additional task as well).

```
syrup/syrup syrup nat_add "(8, 5) -> 13"
```
