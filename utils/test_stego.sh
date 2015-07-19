#!/usr/bin/env bash

HELP="Usage: $0 msg img1 img2"
die() {
    echo $HELP; exit 1
}
[[ $3 ]] || die
MSG=$1
IMG1=$2
IMG2=$3
LINES=`cat $MSG | wc -l`
X=$((RANDOM%10))
Y=$((RANDOM%10))
PASS="test"
ENCD1=$IMG1.enc.png
ENCD2=$IMG2.enc.png
MARKED="$ENCD1.mark.png"
UNMARKED="$MARKED.unmark.png"

cat $MSG | ./stego -x$X -y$Y -f $IMG1 -s $IMG2 -e &&
           ./schlock -i $ENCD1  -m -p$PASS        &&
           ./schlock -i $MARKED -u -p$PASS        &&
           ./stego -x$X -y$Y -f$UNMARKED -s$ENCD2 -d
         | head -n$LINES
