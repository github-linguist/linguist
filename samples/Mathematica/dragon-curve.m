FoldOutLine[{a_,b_}]:={{a,#},{b,#}}&[a+0.5(b-a)+{{0.,0.5},{-0.5,0.}}.(b-a)]
NextStep[in_]:=Flatten[FoldOutLine/@in,1]
lines={{{0.,0.},{1.,0.}}};
Graphics[Line/@Nest[NextStep,lines,11]]
