with Ada.Text_IO; use Ada.Text_IO;
with GNATCOLL.GMP; use GNATCOLL.GMP;
with GNATCOLL.GMP.Integers; use GNATCOLL.GMP.Integers;
procedure ArbitraryInt is
   type stracc is access String;
   BigInt : Big_Integer;
   len : Natural;
   str : stracc;
begin
   Set (BigInt, 5);
   Raise_To_N (BigInt, Unsigned_Long (4**(3**2)));
   str := new String'(Image (BigInt));
   len := str'Length;
   Put_Line ("Size is:"& Natural'Image (len));
   Put_Line (str (1 .. 20) & "....." & str (len - 19 .. len));
end ArbitraryInt;
