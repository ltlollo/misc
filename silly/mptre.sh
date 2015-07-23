#!/usr/bin/env bash
# trash mp3 player in bash; useage: source mptre.sh; add */*.mp3; play; stop
declare -A P; bin="mpv"; bd="/tmp/mps"; dr="$bd/fd"; ns="$bd/n"
alias play=p pause=pa resume=r add=a stop=s shownum=sn del=d next=n back=b
alias list=sn k=s random=rn swap=sw restart=rs shuffle=rn md=mkdir curr=c job=j
[[ -d $bd ]]||md $bd&&[[ -d $dr ]]||md $dr&&[[ -d $ns ]]||md $ns||echo "Err"
pn() { $bin "${P[$1]}"&touch $dr/$! "$ns/$1";wait `j`; cl ;}; j() { ls $dr ;}
ap() { for ((i=${1-0};i<${#P[@]};++i)); do pn $i; done;}; c() { ls $ns ;}
d() { unset P; [[ $pc ]] && kill $pc_PID ;};
b() { l=`c`; s; p $((l?l-1:${#P[@]}-1));};
a() { for ((i=0;i<$#;++i)); do P[${#P[@]}]=`readlink -f "${@:i+1:1}"`; done ;}
sn() { for ((i=0;i<${#P[@]};++i)); do echo $i - ${P[$i]}; done ;}
sw() { t="${P[$1]}"; P[$1]="${P[$2]}"; P[$2]=$t ;}; r() { sc -CONT ;}
p() { [[ $pc ]] && r || coproc pc { ap $1;} &>/dev/null ;}; pa() { sc -STOP ;};
s() { [[ $pc ]] && { kill $pc_PID; sc; [[ `j` ]] && cl ;} ;}; n() { sc -INT ;};
sc() { [[ `j` ]] && kill $1 `j` ;}; cl(){ rm $dr/`j` $ns/`c` ;}; rs() { s; p ;}
rn() { for ((i=0;i<${#P[@]};++i)); do sw $i $((RANDOM%${#P[@]})); done; rs ;}
