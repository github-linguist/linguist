data _null_;
input x y;
file "output.txt";
put x 12.3 " " y 12.5;
cards;
1      1
2      1.4142135623730951
3      1.7320508075688772
1e11   316227.76601683791
;
run;
