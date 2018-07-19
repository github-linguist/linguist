Options[fn]={Add->False,Offset-> 0};
fn[x_,y_,OptionsPattern[]]:=If[OptionValue[Add]==True,x+y+OptionValue[Offset],{x,y,OptionValue[Offset]}]

fn[3,4,{Add->True,Offset->2}]
->9
fn[3,4,{Offset->2,Add->True}]
->9
