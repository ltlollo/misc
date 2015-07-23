#!/usr/bin/env bash
# trash mp3 player in bash; useage: source mptre.sh; add */*.mp3; play; stop
declare -A P; bin="mpv"; dr="/tmp/mps"; [[ -d $dr ]] || mkdir $dr || echo "Err"
alias map=m play=p pause=pa resume=re add=a stop=s shownum=sn del=d next=n
alias list=lf k=s random=rn swap=sw restart=rs shuffle=rn take=t
t() { for i in "${@:$1+3}"; do $2 "$i"&touch $dr/$!;wait `j`;rm $dr/`j`;done ;}
d() { unset P; [[ $pc ]] && kill $pc_PID ;}; lf() { m echo "${P[@]}" ;};
a() { for ((i=0;i<$#;++i)); do P[${#P[@]}]=`readlink -f "${@:i+1:1}"`; done ;}
sn() { for i in ${!P[@]}; do echo $i - ${P[$i]}; done ;}; j() { ls $dr ;}
sw() { t="${P[$1]}"; P[$1]="${P[$2]}"; P[$2]=$t ;}; m() { t 0 "$@" ;}
p() { [[ $pc ]] && re || coproc pc { ap ;} &>/dev/null ;}; rs() { s; p ;}
s() { [[ $pc ]] && { kill $pc_PID; sc; [[ `j` ]] && rm $dr/`j` ;} ;}
sc() { [[ `j` ]] && kill $1 `j` ;}; ap() { t ${1-0} $bin "${P[@]}" ;}
n() { sc -INT ;}; pa() { sc -STOP ;}; re() { sc -CONT ;}
rn() { for ((i=0;i<${#P[@]};++i)); do sw $i $((RANDOM%${#P[@]})); done; rs ;}
