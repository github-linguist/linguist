Program StringConcat;
  Var
     s, s1   : String;

Begin
    s := 'hello';
    writeln(s + ' literal');
    s1 := concat(s, ' literal');
    { s1 := s + ' literal'; works too, with FreePascal }
    writeln(s1);
End.
