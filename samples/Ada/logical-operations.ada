procedure Print_Logic(A : Boolean; B : Boolean) is
begin
   Put_Line("A and B is " & Boolean'Image(A and B));
   Put_Line("A or B  is " & Boolean'Image(A or B));
   Put_Line("A xor B is " & Boolean'Image(A xor B));
   Put_Line("not A   is " & Boolean'Image(not A));
end Print_Logic;
