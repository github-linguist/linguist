with Ada.Text_Io.Editing; use Ada.Text_Io.Editing;
with Ada.Text_Io; use Ada.Text_Io;

procedure Zero_Fill is
   Pic_String: String := "<999999.99>";
   Pic : Picture := To_Picture(Pic_String);
   type Money is delta 0.01 digits 8;
   package Money_Output is new Decimal_Output(Money);
   use Money_Output;

   Value : Money := 37.25;
begin
   Put(Item => Value, Pic => Pic);
end Zero_Fill;
