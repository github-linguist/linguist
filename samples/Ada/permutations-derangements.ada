with Ada.Text_IO; use Ada.Text_IO;
procedure DePermute is
   type U64 is mod 2**64;
   type Num is range 0 .. 20;
   type NumList is array (Natural range <>) of Num;
   type PtNumList is access all NumList;
   package IO is new Ada.Text_IO.Integer_IO (Num);
   package UIO is new Ada.Text_IO.Modular_IO (U64);

   function deranged (depth : Natural; list : PtNumList;
      show : Boolean) return U64 is
      tmp : Num;  count : U64 := 0;
   begin
      if depth = list'Length then
         if show then
            for i in list'Range loop IO.Put (list (i), 2); end loop;
            New_Line;
         end if;  return 1;
      end if;
      for i in reverse depth .. list'Last loop
         if Num (i + 1) /= list (depth) then
            tmp := list (i); list (i) := list (depth); list (depth) := tmp;
            count := count + deranged (depth + 1, list, show);
            tmp := list (i); list (i) := list (depth); list (depth) := tmp;
         end if;
      end loop;
      return count;
   end deranged;

   function gen_n (len : Natural; show : Boolean) return U64 is
      list : PtNumList;
   begin
      list := new NumList (0 .. len - 1);
      for i in list'Range loop list (i) := Num (i + 1); end loop;
      return deranged (0, list, show);
   end gen_n;

   function sub_fact (n : Natural) return U64 is begin
      if n < 2 then return U64 (1 - n);
      else return (sub_fact (n - 1) + sub_fact (n - 2)) * U64 (n - 1);
      end if;
   end sub_fact;

   count : U64;
begin
   Put_Line ("Deranged 4:");
   count := gen_n (4, True);
   Put_Line ("List vs. calc:");
   for i in Natural range 0 .. 9 loop
      IO.Put (Num (i), 1);  UIO.Put (gen_n (i, False), 7);
      UIO.Put (sub_fact (i), 7);  New_Line;
   end loop;
   Put_Line ("!20 = " & U64'Image (sub_fact (20)));
end DePermute;
