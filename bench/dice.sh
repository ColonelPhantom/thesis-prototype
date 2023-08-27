#!/bin/bash

OUTPUT=/data/results/rubicon-$(date +%F+%T)
mkdir -p /data/results
sudo mkdir $OUTPUT
sudo chown opam:opam $OUTPUT
sudo apt install wget time
wget "https://github.com/sharkdp/hyperfine/releases/download/v1.17.0/hyperfine-musl_1.17.0_amd64.deb"
sudo dpkg -i hyperfine-musl_1.17.0_amd64.deb
lscpu > $OUTPUT/lscpu

case $1 in

factory)
hyperfine \
    --min-runs 3 \
    --export-csv $OUTPUT/results-factory.csv \
    -L f "$2" \
    -L h "$3" \
    "timeout 15m /usr/bin/time -o $OUTPUT/time-factory-{f}-{h} dice /data/tmp/rubicon-factory-{f}-{h}.dice > $OUTPUT/log-factory-{f}-{h}"
    ;;

snakes)
hyperfine \
    --min-runs 3 \
    --export-csv $OUTPUT/results-snakes.csv \
    -L f "$2" \
    -L t "$3" \
    -L h "$4" \
    "timeout 15m /usr/bin/time -o $OUTPUT/time-{t}-{f}-{h} dice /data/tmp/rubicon-{t}-{f}-{h}.dice > $OUTPUT/log-{t}-{f}-{h}"
    ;;

esac
