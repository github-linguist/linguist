/* Store all 92 permutations in a SAS dataset. Translation of Fortran 77 */
data queens;
array a{8} p1-p8;
array s{8};
array u{30};
n=8;
do i=1 to n;
a(i)=i;
end;
do i=1 to 4*n-2;
u(i)=0;
end;
m=0;
i=1;
r=2*n-1;
goto L40;
L30:
s(i)=j;
u(p)=1;
u(q+r)=1;
i=i+1;
L40:
if i>n then goto L80;
j=i;
L50:
z=a(i);
y=a(j);
p=i-y+n;
q=i+y-1;
a(i)=y;
a(j)=z;
if u(p)=0 and u(q+r)=0 then goto L30;
L60:
j=j+1;
if j<=n then goto L50;
L70:
j=j-1;
if j=i then goto L90;
z=a(i);
a(i)=a(j);
a(j)=z;
goto L70;
L80:
m=m+1;
output;
L90:
i=i-1;
if i=0 then goto L100;
p=i-a(i)+n;
q=i+a(i)-1;
j=s(i);
u(p)=0;
u(q+r)=0;
goto L60;
L100:
put n m;
keep p1-p8;
run;
