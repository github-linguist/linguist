mwv=: 25 0.25
prods=: <;. _1 ' panacea: ichor: gold:'
hdrs=: <;. _1 ' weight: volume: value:'
vls=: 3000 1800 2500
ws=: 0.3 0.2 2.0
vs=: 0.025 0.015 0.002

ip=: +/ .*
prtscr=: (1!:2)&2

KS=: 3 : 0
 os=. (#:i.@(*/)) mwv >:@<.@<./@:% ws,:vs
 bo=.os#~(ws,:vs) mwv&(*./@:>)@ip"_ 1 os
 mo=.bo{~{.\: vls ip"1 bo
 prtscr &.> prods ([,' ',":@])&.>mo
 prtscr &.> hdrs ('total '&,@[,' ',":@])&.> mo ip"1 ws,vs,:vls
 LF
)
