#!/bin/bash

PLAYERS=(2 3)
TRANS=(snakes_ladders.txt snakes_snakes.txt)
HORIZON=(10 20 50 100 200)

OUTPUT=results/prism-$(date +%F+%T)
mkdir -p results
mkdir $OUTPUT
lscpu > $OUTPUT/lscpu

join_arr() {
  local IFS="$1"
  shift
  echo "$*"
}

case $1 in
si)
hyperfine \
    --min-runs 3 \
    --export-csv $OUTPUT/results-free.csv \
    -L p "$2" \
    -L t "$3" \
    "timeout 30m /bin/time -o $OUTPUT/time-free-{t}-{p} prism tmp/{t}-{p}.prism -pf 'P=? [ F \"win0\" ]' -gs > $OUTPUT/log-free-{t}-{p}"
;;
sh)
hyperfine \
    --min-runs 3 \
    --export-csv $OUTPUT/results-fixed.csv \
    -L p "$2" \
    -L t "$3" \
    -L h "$4" \
    "timeout 30m /bin/time -o $OUTPUT/time-fixed-{t}-{p}-{h} prism tmp/{t}-{p}.prism -pf 'P=? [ F<={h} finished ]' -gs > $OUTPUT/log-fixed-{t}-{p}-{h}"
;;
factory)
hyperfine \
    --min-runs 3 \
    --export-csv $OUTPUT/results-factory.csv \
    -L f "$2" \
    -L h "$3" \
    "timeout 15m /bin/time -o $OUTPUT/time-factory-{f}-{h}-$4 prism tmp/factories-{f}.prism -pf 'P=? [ F <= {h} allStrike]' $4 > $OUTPUT/log-factory-{f}-{h}-$4"
;;
esac
