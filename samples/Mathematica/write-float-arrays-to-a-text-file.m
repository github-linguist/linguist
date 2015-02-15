exportPrec[path_, data1_, data2_, prec1_, prec2_]:=Export[path,Transpose[{Map[ToString[NumberForm[#, prec2]] &, data2],Map[ToString[NumberForm[#, prec1]] &, data1]}], "Table"]
