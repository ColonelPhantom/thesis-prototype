#!/bin/bash

PLAYERS=(2 3 4)
TRANS=(snakes_ladders.txt snakes_snakes.txt)
HORIZON=(10 20 50 100 200)

OUTPUT=results/eigen-$(date +%F+%T)
mkdir -p results tmp
mkdir $OUTPUT
lscpu > $OUTPUT/lscpu

for p in ${PLAYERS[@]}
    do
    time g++ -O3 -g -std=c++20 -Wall -Wextra -o tmp/solver-$p \
        -DSNAKES_LADDERS_PLAYERS=$p \
        eigen-matrixfree/main.cpp
done

join_arr() {
    local IFS="$1"
    shift
    echo "$*"
}

case $1 in
sf)
hyperfine \
    --min-runs 3 \
    --export-csv $OUTPUT/results-free.csv \
    -L p "$2" \
    -L t "$3" \
    "timeout 30m /bin/time tmp/solver-{p} snakes-free 100 {t} &> $OUTPUT/log-free-{t}-{p}"
    ;;
si)
hyperfine \
    --min-runs 3 \
    --export-csv $OUTPUT/results-free.csv \
    -L p "$2" \
    -L t "$3" \
    "timeout 30m /bin/time tmp/solver-{p} snakes-iter 100 {t} &> $OUTPUT/log-free-{t}-{p}"
    ;;
sh)
hyperfine \
    --min-runs 3 \
    --export-csv $OUTPUT/results-fixed.csv \
    -L p "$2" \
    -L t "$3" \
    -L h "$4" \
    "timeout 30m /bin/time tmp/solver-{p} snakes-fixed 100 {t} {h} &> $OUTPUT/log-fixed-{t}-{p}-{h}"
    ;;
factory)
hyperfine \
    --min-runs 3 \
    --export-csv $OUTPUT/results-factory.csv \
    -L f "$2" \
    -L h "$3" \
    "timeout 15m /bin/time tmp/solver-$PLAYERS factories-fixed {f} {h} &> $OUTPUT/log-factory-{f}-{h}"
    ;;
esac
