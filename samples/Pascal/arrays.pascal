Program ArrayDemo;
uses
  SysUtils;
var
  StaticArray: array[0..9] of Integer;
  DynamicArray: array of Integer;
  StaticArrayText,
  DynamicArrayText: string;
  lcv: Integer;
begin
  // Setting the length of the dynamic array the same as the static one
  SetLength(DynamicArray, Length(StaticArray));
  // Asking random numbers storing into the static array
  for lcv := 0 to Pred(Length(StaticArray)) do
  begin
    write('Enter a integer random number for position ', Succ(lcv), ': ');
    readln(StaticArray[lcv]);
  end;
  // Storing entered numbers of the static array in reverse order into the dynamic
  for lcv := 0 to Pred(Length(StaticArray)) do
    DynamicArray[Pred(Length(DynamicArray)) - lcv] := StaticArray[lcv];
  // Concatenating the static and dynamic array into a single string variable
  StaticArrayText := '';
  DynamicArrayText := '';
  for lcv := 0 to Pred(Length(StaticArray)) do
  begin
    StaticArrayText := StaticArrayText + IntToStr(StaticArray[lcv]) + ' ';
    DynamicArrayText := DynamicArrayText + IntToStr(DynamicArray[lcv]) + ' ';
  end;
  // Displaying both arrays
  writeln(StaticArrayText);
  writeln(DynamicArrayText);
end.
