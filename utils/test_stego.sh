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
PLEN1=$((20+RANDOM%8))
PLEN2=$((20+RANDOM%8))
N1=$((10+RANDOM%8))
N2=$((10+RANDOM%8))
PASS1=`head -c$PLEN1 /dev/urandom`
PASS2=`head -c$PLEN2 /dev/urandom`
ENCD1=$IMG1.enc.png
ENCD2=$IMG2.enc.png
MARKED1="$ENCD1.mark.png"
MARKED2="$ENCD2.mark.png"
UNMARKED1="$MARKED1.unmark.png"
UNMARKED2="$MARKED2.unmark.png"

printf "Using X, Y = $X, $Y\n"            1>&2
printf "Using N1, N2 = $N2, $N2\n"        1>&2
printf "Using  PASS1 = "                  1>&2
printf $PASS1 | hexdump -e '/1 "0x%02X "' 1>&2
printf "\n"                               1>&2
printf "Using  PASS2 = "                  1>&2
printf $PASS2 | hexdump -e '/1 "0x%02X "' 1>&2
printf "\n"                               1>&2

cat $MSG | ./stego -x$X -y$Y -f$IMG1 -s$IMG2 -e    &&
           ./schlock -i $ENCD1   -m -p$PASS1 -n$N1 &&
           ./schlock -i $MARKED1 -u -p$PASS1 -n$N1 &&
           ./schlock -i $ENCD2   -m -p$PASS2 -n$N2 &&
           ./schlock -i $MARKED2 -u -p$PASS2 -n$N2 &&
           ./stego -x$X -y$Y -f$UNMARKED1 -s$UNMARKED2 -d | head -n$LINES
