i := 0;
j := 0;
for i st i < 10 step i:=i+1 do (
  write('i = ');
  write(i);
  writeln;
  j := 0.0;
  for j st j < 1.0 step j:=j+0.1 do (
    write('   j = ');
    write(j);
    writeln
  )
)
