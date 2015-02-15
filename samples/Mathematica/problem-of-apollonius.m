Apolonius[a1_,b1_,c1_,a2_,b2_,c2_,a3_,b3_,c3_,S1_,S2_ ,S3_ ]:=
Module[{x1=a1,y1=b1,r1=c1,x2=a2,y2=b2,r2=c2,x3=a3,y3=b3,r3=c3,s1=S1,s2=S2,s3=S3},
v11 = 2*x2 - 2*x1; v12 = 2*y2 - 2*y1;
v13 = x1^2 - x2^2 + y1^2 - y2^2 - r1^2 + r2^2;
v14 = 2*s2*r2 - 2*s1*r1;

v21 = 2*x3-2*x2 ; v22 = 2*y3 - 2*y2;
v23 = x2^2 - x3^2 + y2^2 - y3^2 - r2^2 + r3^2;
v24 = 2*s3*r3 - 2*s2*r2;

w12 = v12/v11; w13 = v13/v11; w14 = v14/v11;

w22 = v22/v21 - w12;
w23 = v23/v21 - w13;
w24 = v24/v21 - w14;

p = -w23/w22; q=w24/w22;
m = -w12*p - w13; n=w14 - w12*q;

a = n^2 + q^2-1;
b = 2*m*n - 2*n*x1 + 2*p*q - 2*q*y1 + 2*s1*r1;
c = x1^2+m^2 - 2*m*x1 + p^2+y1^2 - 2*p*y1 - r1^2;

d= b^2 - 4*a*c;
rs = (-b -Sqrt[d])/(2*a);
xs = m + n*rs; ys = p + q*rs;
Map[N,{xs, ys, rs} ]]
