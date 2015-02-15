/* Showing operators and their fortran-like equivalents. Note that ~= and ^= both mean "different" */
data _null_;
input a b;
put a= b=;
if a = b then put "a = b";
if a ^= b then put "a ^= b";
if a ~= b then put "a ~= b";
if a < b then put "a < b";
if a > b then put "a > b";
if a <= b then put "a <= b";
if a >= b then put "a >= b";
if a eq b then put "a eq b";
if a ne b then put "a ne b";
if a lt b then put "a lt b";
if a gt b then put "a gt b";
if a le b then put "a le b";
if a ge b then put "a ge b";
cards;
1 2
2 1
1 1
;
run;
