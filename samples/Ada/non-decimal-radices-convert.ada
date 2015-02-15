with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Fixed;
With Ada.Strings.Unbounded;

procedure Number_Base_Conversion is
   Max_Base : constant := 36;
   subtype Base_Type is Integer range 2..Max_Base;
   Num_Digits : constant String := "0123456789abcdefghijklmnopqrstuvwxyz";
   Invalid_Digit : exception;

   function To_Decimal(Value : String; Base : Base_Type) return Integer is
      use Ada.Strings.Fixed;
      Result : Integer := 0;
      Decimal_Value : Integer;
      Radix_Offset : Natural := 0;
   begin
      for I in reverse Value'range loop
         Decimal_Value := Index(Num_Digits, Value(I..I)) - 1;
         if Decimal_Value < 0 then
            raise Invalid_Digit;
         end if;
         Result := Result + (Base**Radix_Offset * Decimal_Value);
         Radix_Offset := Radix_Offset + 1;
      end loop;
      return Result;
   end To_Decimal;

   function To_Base(Value : Natural; Base : Base_Type) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := Null_Unbounded_String;
      Temp : Natural := Value;
      Base_Digit : String(1..1);
   begin
      if Temp = 0 then
         return "0";
      end if;
      while Temp > 0 loop
         Base_Digit(1) := Num_Digits((Temp mod Base) + 1);
         if Result = Null_Unbounded_String then
            Append(Result, Base_Digit);
         else
            Insert(Source => Result,
               Before => 1,
               New_Item => Base_Digit);
         end if;
         Temp := Temp / Base;
      end loop;
      return To_String(Result);
   end To_Base;

begin
   Put_Line("26 converted to base 16 is " & To_Base(26, 16));
   Put_line("1a (base 16) is decimal" & Integer'image(To_Decimal("1a", 16)));
end Number_Base_Conversion;
