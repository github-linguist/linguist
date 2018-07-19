run := function()
  local a, b, f;
  f := InputTextUser();
  Print("a =\n");
  a := Int(Chomp(ReadLine(f)));
  Print("b =\n");
  b := Int(Chomp(ReadLine(f)));
  Display(Concatenation(String(a), " + ", String(b), " = ", String(a + b)));
  Display(Concatenation(String(a), " - ", String(b), " = ", String(a - b)));
  Display(Concatenation(String(a), " * ", String(b), " = ", String(a * b)));
  Display(Concatenation(String(a), " / ", String(b), " = ", String(QuoInt(a, b)))); # toward 0
  Display(Concatenation(String(a), " mod ", String(b), " = ", String(RemInt(a, b)))); # nonnegative
  Display(Concatenation(String(a), " ^ ", String(b), " = ", String(a ^ b)));
  CloseStream(f);
end;
