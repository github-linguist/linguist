with Ada.Text_IO;

procedure Middle_Three_Digits is

   Impossible: exception;

   function Middle_String(I: Integer; Middle_Size: Positive) return String is
      S: constant String := Integer'Image(I);
      First: Natural := S'First;
      Full_Size, Border: Natural;
   begin
      while S(First) not in '0' .. '9' loop -- skip leading blanks and minus
         First := First + 1;
      end loop;
      Full_Size := S'Last-First+1;
      if (Full_Size < Middle_Size) or (Full_Size mod 2 = 0) then
         raise Impossible;
      else
         Border := (Full_Size - Middle_Size)/2;
         return S(First+Border .. First+Border+Middle_Size-1);
      end if;
   end Middle_String;

   Inputs: array(Positive range <>) of Integer :=
     (123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345,
      1, 2, -1, -10, 2002, -2002, 0);
   Error_Message: constant String := "number of digits must be >= 3 and odd";

   package IIO is new Ada.Text_IO.Integer_IO(Integer);

begin
   for I in Inputs'Range loop
      IIO.Put(Inputs(I), Width => 9);
      Ada.Text_IO.Put(": ");
      begin
         Ada.Text_IO.Put(Middle_String(Inputs(I), 3));
      exception
         when Impossible => Ada.Text_IO.Put("****" & Error_Message & "****");
      end;
      Ada.Text_IO.New_Line;
   end loop;

end Middle_Three_Digits;
