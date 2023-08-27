for i in $(seq 8 20);
do
    for h in 10 20 40 80 160;
    do
        time python3 rubicon/rubicon.py --prism /data/tmp/factories-$i.prism --prop "P=? [ F <= $h allStrike]" --output "/data/tmp/rubicon-factory-$i-$h.dice"
    done
done
