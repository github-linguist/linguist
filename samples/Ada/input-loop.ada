with Ada.Text_Io; use Ada.Text_Io;

procedure Read_Stream is
   Line : String(1..10);
   Length : Natural;
begin
   while not End_Of_File loop
      Get_Line(Line, Length); -- read up to 10 characters at a time
      Put(Line(1..Length));
      -- The current line of input data may be longer than the string receiving the data.
      -- If so, the current input file column number will be greater than 0
      -- and the extra data will be unread until the next iteration.
      -- If not, we have read past an end of line marker and col will be 1
      if Col(Current_Input) = 1 then
         New_Line;
      end if;
   end loop;
end Read_Stream;
