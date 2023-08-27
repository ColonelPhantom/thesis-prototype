#!/bin/bash

PLAYERS=(2 3)
TRANS=(snakes_ladders.txt snakes_snakes.txt)
HORIZON=(10 20 50 100 200)

OUTPUT=/data/results/storm-$(date +%F+%T)
mkdir -p /data/results
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
    "timeout 30m /bin/time storm --prism /data/tmp/{t}-{p}.prism --prop 'P=? [ F \"win0\" ]' --timemem -e $4 -v > $OUTPUT/log-free-{t}-{p}"
;;
sh)
hyperfine \
    --min-runs 3 \
    --export-csv $OUTPUT/results-fixed.csv \
    -L p "$2" \
    -L t "$3" \
    -L h "$4" \
    "timeout 30m /bin/time storm --prism /data/tmp/{t}-{p}.prism --prop 'P=? [ F<={h} finished ]' --timemem -e $5 -v > $OUTPUT/log-fixed-{t}-{p}-{h}"
;;
factory)
echo "Testing factories with parameters $@"
hyperfine \
    --min-runs 3 \
    --export-csv $OUTPUT/results-factory.csv \
    -L f "$2" \
    -L h "$3" \
    "timeout 15m /bin/time storm --prism /data/tmp/factories-{f}.prism --prop 'P=? [ F <= {h} allStrike]' --timemem -e $4 > $OUTPUT/log-factory-{f}-$4-{h}"
;;

*)
echo "Error: unknown command $1"
;;
esac
