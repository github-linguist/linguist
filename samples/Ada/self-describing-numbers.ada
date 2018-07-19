with Ada.Text_IO; use Ada.Text_IO;
procedure SelfDesc is
   subtype Desc_Int is Long_Integer range 0 .. 10**10-1;

   function isDesc (innum : Desc_Int) return Boolean is
      subtype S_Int is Natural range 0 .. 10;
      type S_Int_Arr is array (0 .. 9) of S_Int;
      ref, cnt : S_Int_Arr := (others => 0);
      n, digit : S_Int := 0;  num : Desc_Int := innum;
   begin
      loop
         digit := S_Int (num mod 10);
         ref (9 - n) := digit;  cnt (digit) := cnt (digit) + 1;
         num := num / 10; exit when num = 0; n := n + 1;
      end loop;
      return ref (9 - n .. 9) = cnt (0 .. n);
   end isDesc;

begin
   for i in Desc_Int range 1 .. 100_000_000 loop
      if isDesc (i) then
         Put_Line (Desc_Int'Image (i));
      end if;
   end loop;
end SelfDesc;
