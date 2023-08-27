for i in $(seq 2 4);
do
    for h in 10 20 40 80 160;
    do
        for t in snakes_ladders.txt snakes_snakes.txt; do
            time python3 rubicon/rubicon.py --prism /data/tmp/$t-$i.prism --prop "P=? [ F <= $h finished]" --output "/data/tmp/rubicon-$t-$i-$h.dice"
        done
    done
done
