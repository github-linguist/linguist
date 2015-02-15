with Ada.Text_Io; use Ada.Text_Io;

procedure Exceptions_From_Nested_Calls is
   U0 : exception;
   U1 : exception;
   Baz_Count : Natural := 0;
   procedure Baz is
   begin
      Baz_Count := Baz_Count + 1;
      if Baz_Count = 1 then
         raise U0;
      else
         raise U1;
      end if;
   end Baz;
   procedure Bar is
   begin
      Baz;
   end Bar;
   procedure Foo is
   begin
      Bar;
   exception
      when U0 =>
         Put_Line("Procedure Foo caught exception U0");
   end Foo;
begin
   for I in 1..2 loop
      Foo;
   end loop;
end Exceptions_From_Nested_Calls;
