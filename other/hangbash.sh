#!/usr/bin/env bash

[[ $1 ]] || { echo "$0 dict [guesses]"; exit 1; }
L=`cat $1| wc -l`; W=`awk "NR==$((RANDOM%(L+1))){print tolower(\\$0)}" $1`
N=${2-6}; G=""; T=""; V=""
while [[ $N -gt 0 ]]; do
    echo -n "Guesses: $N, W: "; echo $W | tr -c "$G\n" "-"
    echo -ne "Tried: $T\tCorrect: $G\nEnter a char: "
    read -n1 i; echo; i=${i,,}
    case $W in *$i*) case $G in *$i*);;*) G="$G$i";;esac;;
                  *) case $T in *$i*);;*) ((--N)) ;;esac;;
    esac
    case $T in *$i*);;*) T="$T$i";;esac
    V="y"
    for ((j=0;j<${#W};++j)); do case $G in *${W:$j:1}*);; *) V="";;esac; done
    [[ $V ]] && { echo Congrats, it was: $W; exit 0; }
done
echo Sorry, it was: $W
