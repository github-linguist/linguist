x := RandomInteger[10];
Print["<table>", "\n","<tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>"]
Scan[Print["<tr><td>", #, "</td><td>", x, "</td><td>", x, "</td><td>","</td></tr>"] & , Range[3]]
Print["</table>"]
