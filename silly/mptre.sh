#!/usr/bin/env bash
# trash mp3 player in bash; useage: source mptre.sh; add */*.mp3; play; stop
declare -A P; bin=mpv; tramp=mpv
alias map=m play=p pause=pa resume=re add=a stop=s shownum=sn del=d next=n
alias list=lf k=s random=rn swap=sw restart=rs shuffle=rn take=t
t() { for i in "${@:$1+3}"; do $2 "$i"; done; }; m() { t 0 "$@"; }
d() { unset P; [[ $pc ]] && kill $pc_PID; }; lf() { m echo "${P[@]}"; };
a() { for ((i=0;i<$#;++i)); do P[${#P[@]}]=`readlink -f "${@:i+1:1}"`; done; }
sn() { for i in ${!P[@]}; do echo $i - ${P[$i]}; done; }
sw() { t="${P[$1]}"; P[$1]="${P[$2]}"; P[$2]=$t; }
ap() { t ${1-0} $tramp "${P[@]}" &>/dev/null; }
p() { [[ $pc ]] && re || coproc pc { ap; }; }; rs() { s; p; }
s() { [[ $pc ]] && { kill $pc_PID; killall $bin; }; }
sc() { [[ `pidof $bin` ]] && killall $1 $bin; };
n() { sc -INT; }; pa() { sc -STOP; }; re() { sc -CONT; }
rn() { for ((i=0;i<${#P[@]};++i)); do sw $i $((RANDOM%${#P[@]})); done; rs; }
