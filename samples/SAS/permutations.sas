/* Store permutations in a SAS dataset. Translation of Fortran 77 */
data perm;
n=6;
array a{6} p1-p6;
do i=1 to n;
a(i)=i;
end;
L1:
output;
link L2;
if next then goto L1;
stop;
L2:
next=0;
i=n-1;
L10:
if a(i)<a(i+1) then goto L20;
i=i-1;
if i=0 then goto L20;
goto L10;
L20:
j=i+1;
k=n;
L30:
t=a(j);
a(j)=a(k);
a(k)=t;
j=j+1;
k=k-1;
if j<k then goto L30;
j=i;
if j=0 then return;
L40:
j=j+1;
if a(j)<a(i) then goto L40;
t=a(i);
a(i)=a(j);
a(j)=t;
next=1;
return;
keep p1-p6;
run;
