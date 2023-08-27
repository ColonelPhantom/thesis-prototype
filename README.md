# Thesis prototype
This repository contains the relevant artefacts for my Bachelor's thesis "Efficiently calculating reachability probabilities in Markov chains".

## Archived results
Logs for the results as found in the thesis are found in the `results` directory.

## PRISM files
Prism model files were generated with the Haskell scripts found in the `prism` directory.

## C++ prototype
A prototype implementation of the matrix-free approach described, based on the `eigen3` library, can be found in the `eigen-matrixfree` directory. This prototype covers both the linear system approach and the forward iteration approach.

## Benchmark scripts
The `bench` directory contains scripts used to run benchmarks of the prototype, Prism, Storm and Rubicon. Before running any, make sure to run `bench/generate.sh` and ensure Podman, GCC and Hyperfine are installed. (It is also possible to use Docker, replace any Podman references if necessary and remove the `,z` from the mount options in the scripts).

Example invocation: `bench/storm-docker.sh si 2 snakes_ladders.txt sparse`. Read the scripts for more details. The other main entrypoints are `rubicon-docker.sh`, `prism.sh`, and `eigen.sh`.

Note: the `rubicon-ci` container referenced in `rubicon-docker.sh` represents a custom build of Rubicon against a more recent version of Storm. The Dockerfile for this can be found at https://github.com/ColonelPhantom/rubicon.
