Program FloydDemo (input, output);

function digits(number: integer): integer;
  begin
    digits := trunc(ln(number) / ln(10)) + 1;
  end;

procedure floyd1 (numberOfLines: integer);
{ variant with repeat .. until loop }
  var
    i, j, numbersInLine, startOfLastlLine: integer;

  begin
    startOfLastlLine := (numberOfLines - 1) * numberOfLines div 2 + 1;
    i := 1;
    j := 1;
    numbersInLine := 1;
    repeat
      repeat
        write(i: digits(startOfLastlLine - 1 + j), ' ');
        inc(i);
	inc(j);
      until (j > numbersInLine);
      writeln;
      j := 1;
      inc(numbersInLine);
    until (numbersInLine > numberOfLines);
  end;

procedure floyd2 (numberOfLines: integer);
{ Variant with for .. do loop }
  var
    i, j, numbersInLine, startOfLastlLine: integer;

  begin
    startOfLastlLine := (numberOfLines - 1) * numberOfLines div 2 + 1;
    i := 1;
    for numbersInLine := 1 to numberOfLines do
    begin
      for j := 1 to numbersInLine do
      begin
        write(i: digits(startOfLastlLine - 1 + j), ' ');
        inc(i);
      end;
      writeln;
    end;
  end;

begin
  writeln ('*** Floyd 5 ***');
  floyd1(5);
  writeln;
  writeln ('*** Floyd 14 ***');
  floyd2(14);
end.
