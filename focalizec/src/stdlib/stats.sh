#! /bin/bash

function percent {
    if [ $2 = '0' ]
    then
        echo 'NaN'
    else
        val=$(echo "scale = 2;  100 * $1 / $2" | bc -l)
        echo "$val%"
    fi
}

all_lines=$(cat *.fcl | wc -l)
ok=

# Test if the dk files is accepted by Dedukti
for f in *.fcl
do
    s=${f%.*}.sk
    d=${f%.*}.dk

    echo
    echo ${f%.*}
    skindent "$s" > $d && dkcheck -e $d |& head -n 1
    if [ -e ${f%.*}.dko ]
    then
        ok="$ok $f"
        echo 'OK'
    else
        echo 'KO'
    fi
    size=$(cat $f | wc -l)
    echo "size: $size"
    echo -n "Ignored: "
    percent $(grep -c 'ASSUMED' $f) $size

done

ok_lines=$(cat $ok | grep -v 'ASSUMED' | wc -l)

echo
echo

echo -n "OK: "
percent $ok_lines $all_lines

echo -n "ASSUMED: "
percent $(cat *.fcl | grep -c 'ASSUMED') $all_lines

echo -n "ASSUMED in OK: "
percent $(cat $ok | grep -c 'ASSUMED') $all_lines
