program queens;

const l=16;

var i,j,k,m,n,p,q,r,y,z: integer;
    a,s: array[1..l] of integer;
    u: array[1..4*l-2] of integer;

label L3,L4,L5,L6,L7,L8,L9,L10;

begin
   for i:=1 to l do a[i]:=i;
   for i:=1 to 4*l-2 do u[i]:=0;
   for n:=1 to l do
   begin
      m:=0;
      i:=1;
      r:=2*n-1;
      goto L4;
L3:
      s[i]:=j;
      u[p]:=1;
      u[q+r]:=1;
      i:=i+1;
L4:
      if i>n then goto L8;
      j:=i;
L5:
      z:=a[i];
      y:=a[j];
      p:=i-y+n;
      q:=i+y-1;
      a[i]:=y;
      a[j]:=z;
      if (u[p]=0) and (u[q+r]=0) then goto L3;
L6:
      j:=j+1;
      if j<=n then goto L5;
L7:
      j:=j-1;
      if j=i then goto L9;
      z:=a[i];
      a[i]:=a[j];
      a[j]:=z;
      goto L7;
L8:
      m:=m+1;
      { uncomment the following to print solutions }
      { write(n,' ',m,':');
      for k:=1 to n do write(' ',a[k]);
      writeln; }
L9:
      i:=i-1;
      if i=0 then goto L10;
      p:=i-a[i]+n;
      q:=i+a[i]-1;
      j:=s[i];
      u[p]:=0;
      u[q+r]:=0;
      goto L6;
L10:
      writeln(n,' ',m);
   end;
end.

{ 1 1
  2 0
  3 0
  4 2
  5 10
  6 4
  7 40
  8 92
  9 352
 10 724
 11 2680
 12 14200
 13 73712
 14 365596
 15 2279184
 16 14772512 }
