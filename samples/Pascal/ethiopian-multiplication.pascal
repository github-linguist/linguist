program EthiopianMultiplication;

  function Double(Number: Integer): Integer;
  begin
    Double := Number * 2
  end;

  function Halve(Number: Integer): Integer;
  begin
    Halve := Number div 2
  end;

  function Even(Number: Integer): Boolean;
  begin
    Even := Number mod 2 = 0
  end;

  function Ethiopian(NumberA, NumberB: Integer): Integer;
  begin
    Ethiopian := 0;
    while NumberA >= 1 do
	begin
	  if not Even(NumberA) then
	    Ethiopian := Ethiopian + NumberB;
	  NumberA := Halve(NumberA);
	  NumberB := Double(NumberB)
	end
  end;

begin
  Write(Ethiopian(17, 34))
end.
