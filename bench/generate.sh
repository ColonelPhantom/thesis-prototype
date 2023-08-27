PLAYERS=(2 3 4)
TRANS=(snakes_ladders.txt snakes_snakes.txt)


rm -rf tmp
mkdir -p tmp

for p in ${PLAYERS[@]}
    do
    for t in ${TRANS[@]}
        do
        echo "Generating $t $p"
        runghc prism/HsSnakes.hs $p 100 < $t > tmp/$t-$p.prism
    done
done


for i in $(seq 8 20);
    do
    echo "Generating factories $i"
    runghc prism/HsFactories.hs $i < factory-weights.txt > tmp/factories-$i.prism
done
